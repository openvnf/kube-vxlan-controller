-module(kube_vxlan_controller_run).

-behavior(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-include_lib("kernel/include/logger.hrl").

-define(Db, kube_vxlan_controller_db).
-define(K8s, kube_vxlan_controller_k8s).
-define(Net, kube_vxlan_controller_net).
-define(Pod, kube_vxlan_controller_pod).
-define(Agent, kube_vxlan_controller_agent).
-define(State, kube_vxlan_controller_state).
-define(Tools, kube_vxlan_controller_tools).
-define(Format, kube_vxlan_controller_format).

-define(ShortNodeName, 'kube-vxlan-controller').
-define(Cookie, 'kube-vxlan-controller').

-define(TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

-define(DEBUG_OPTS, [{install, {fun logger_sys_debug:logger_gen_statem_trace/3, ?MODULE}}]).
-define(JsonDecodeOptions, [return_maps, {labels, atom}]).

start_link(Config) ->
    %%Opts = [{debug, ?DEBUG_OPTS}],
    Opts = [],
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [Config], Opts).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> handle_event_function.

init([#{server := Server,
	ca_cert_file := CaCertFile,
	selector := Selector} = Config]) ->
    process_flag(trap_exit, true),

    ResourceVersion = ?Db:load_resource_version(Selector, Config),
    Data0 = ?State:set_resource_version(ResourceVersion, Config),

    #{host := Host,
      port := Port} = uri_string:parse(Server),

    Opts = #{connect_timeout => ?TIMEOUT,
	     protocols => [http2],
	     transport => tls,
	     tls_opts => [{cacertfile, CaCertFile}]
	    },
    {ok, ConnPid} = gun:open(Host, Port, Opts),
    ?LOG(info, "~p: connecting to ~s", [ConnPid, Server]),
    MRef = monitor(process, ConnPid),

    Data = Data0#{config => Config, conn => ConnPid, mref => MRef, pending => <<"">>},
    {ok, init, Data}.

handle_event(info, {'DOWN', MRef, process, ConnPid, Reason}, _,
	     #{mref := MRef, conn := ConnPid}) ->
    ?LOG(info, "~p: terminated with ~p", [ConnPid, Reason]),
    {stop, normal};

handle_event(info, {gun_up, ConnPid, _Protocol}, init,
	     #{conn := ConnPid,
	       selector := Selector,
	       token := Token} = Data0) ->

    ResourceVersion = ?State:resource_version(Data0),
    Data =
	case ?State:is_resource_version_shown(Data0) of
	    true -> Data0;
	    false ->
		?LOG(info, "Watching pods (selector: ~s) from version: ~w",
		     [Selector, ResourceVersion]),
		?State:set_resource_version_shown(Data0)
	end,

    Resource = "/api/v1/pods",
    Query =
	uri_string:compose_query(
	  [{"labelSelector", Selector},
	   {"resourceVersion", integer_to_list(ResourceVersion)},
	   {"watch", "true"}
	   %%{"allowWatchBookmarks", "true"}
	   %%,{"timeoutSeconds", "10"}
	  ]),
    Path = uri_string:recompose(#{path => Resource, query => Query}),
    ?LOG(debug, "Path: ~p", [Path]),
    Headers = headers(Token),

    StreamRef = gun:get(ConnPid, Path, Headers),
    {next_state, requested, Data#{stream => StreamRef}};

handle_event(info, {gun_response, ConnPid, _StreamRef, fin, Status, _Headers}, _,
	     #{conn := ConnPid}) ->
    ?LOG(debug, "~p: terminated with status ~p", [ConnPid, Status]),
    {stop, normal};

handle_event(info, {gun_response, ConnPid, _StreamRef, nofin, 200, _Headers}, requested,
	     #{conn := ConnPid} = Data) ->
    {next_state, watching, Data};

handle_event(info, {gun_data, ConnPid, _StreamRef, nofin, Bin}, watching,
	     #{conn := ConnPid} = Data0) ->
    Data = handle_api_data(Bin, Data0),
    {keep_state, Data};

%% handle_event(info, {gun_response, ConnPid, _StreamRef, fin, Status, _Headers} = _Msg, _State,
%%	     #{conn := ConnPid} = _Data) ->
%%     ?LOG(debug, "Info: ~p, State: ~p", [_Msg, _State]),
%%     ?LOG(info, "~p: got response ~p", [ConnPid, Reason]),
%%     keep_state_and_data;

handle_event(_Ev, _Msg, _State, _Data) ->
    ?LOG(debug, "Ev: ~p, Msg: ~p, State: ~p", [_Ev, _Msg, _State]),
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%=========================================================================
%%%  internal functions
%%%=========================================================================

headers(Token) ->
    [
     {<<"authorization">>, iolist_to_binary(["Bearer ", Token])},
     {<<"accept">>, <<"application/json">>}
    ].

handle_api_data(Bin, #{pending := In} = Data) ->
    handle_api_data(Data#{pending := <<In/binary, Bin/binary>>}).

handle_api_data(#{pending := In} = Data0) ->
    case binary:split(In, <<$\n>>) of
	[In] -> %% no newline,
	    Data0;
	[Head, Tail] ->
	    Data = process_api_data(Head, Data0),
	    handle_api_data(Data#{pending => Tail})
    end.

process_api_data(Bin, #{selector := Selector} = Data0) ->
    case jsx:decode(Bin, ?JsonDecodeOptions) of
	#{type := Type, object := Object} ->
	    {Kind, Resource} = read_event(Object, Data0),
	    %%?LOG(debug, #{event => EventType, resource => Resource}),

	    Data = ?State:set_resource_version(Resource, Data0),
	    ResourceVersion = ?State:resource_version(Data),

	    ?Db:save_resource_version(Selector, ResourceVersion, Data),

	    %% process_event(Kind, atom(Type), Resource, Data),
	    process_event_(Kind, atom(Type), Resource, Data),
	    Data;
	Ev ->
	    ?LOG(info, "unexpected message from k8s: ~p", [Ev]),
	    Data0
    end.

process_event_(pod, Type, #{pod_uid := UId} = Pod, #{config := Config}) ->
    ?LOG(info, "POD UID: ~p -> ~p", [UId, Type]),
    ?Pod:process_event(Type, Pod, Config),
    ok;
process_event_(_Kind, _Type, _Resource, _Data) ->
    ok.

%% process_event(pod, added, Pod, Data) ->
%%     ?State:set(pod_added, Pod, Data);

%% process_event(pod, deleted, Pod, Data) ->
%%     ?State:unset(pod_added, Pod,
%%     ?State:unset(agent_terminated, Pod, Data));

%% process_event(pod, modified, Pod = #{init_agent := running}, #{config := Config} = Data) ->
%%     UseInitAgentConfig =
%% 	Config#{agent_container_name => maps:get(agent_init_container_name, Data)},
%%     pod_setup(Pod, UseInitAgentConfig, Data);

%% process_event(pod, modified, Pod = #{agent := running}, Data0) ->
%%     case ?State:is(pod_added, Pod, Data0) of
%% 	true ->
%% 	    Data = ?State:unset(pod_added, Pod, Data0),
%% 	    pod_join(Pod, Data);
%% 	false ->
%% 	    Data0
%%     end;

%% process_event(pod, modified, Pod = #{agent := terminated}, Data0) ->
%%     case ?State:is(agent_terminated, Pod, Data0) of
%% 	true -> Data0;
%% 	false ->
%% 	    Data = ?State:set(agent_terminated, Pod, Data0),
%% 	    pod_leave(Pod, Data)
%%     end;

%% process_event(_Kind, _Type, _Resource, Data) ->
%%     Data.

atom(Bin) when is_binary(Bin) ->
    binary_to_atom(string:lowercase(Bin), latin1).

read_event(#{kind := Kind,
	     metadata :=
		 #{namespace := Namespace,
		   uid := PodUid,
		   name := PodName,
		   resourceVersion := ResourceVersion
		  } = Metadata,
	     status :=
		 #{phase := Phase} = Status}, Data) ->
    Ev =
	#{resource_version => binary_to_integer(ResourceVersion),
	  namespace => binary_to_list(Namespace),
	  pod_uid => binary_to_list(PodUid),
	  pod_name => binary_to_list(PodName),
	  pod_ip => binary_to_list(maps:get(podIP, Status, <<>>)),
	  nets_data => ?Tools:pod_nets_data(
			  maps:get(annotations, Metadata, #{}),
			  Data
			 ),
	  phase => binary_to_list(Phase),
	  agent => ?Tools:pod_container_state(
		      maps:get(agent_container_name, Data),
		      maps:get(containerStatuses, Status, [])
		     ),
	  init_agent => ?Tools:pod_container_state(
			   maps:get(agent_init_container_name, Data),
			   maps:get(initContainerStatuses, Status, [])
			  )
	 },
    %%?LOG(info, "ICS: ~p", [maps:get(initContainerStatuses, Status, [])]),
    {atom(Kind), Ev};

read_event(#{code := Code,
	     kind := Kind,
	     message := Message = <<"too old resource version:", Versions/binary>>,
	     reason := Reason,
	     status := Status
	    }, _Data) ->
    Ev =
	#{code => Code,
	  reason => binary_to_list(Reason),
	  status => binary_to_list(Status),
	  message => binary_to_list(Message),
	  resource_version =>
	      begin
		  [_DesiredVersion, OldestVersion] = string:lexemes(Versions, "( )"),
		  binary_to_integer(OldestVersion) - 1
	      end
	 },
    {atom(Kind), Ev}.

%% pod_setup(PodResource, Config, State) ->
%%     Pod = pod(PodResource, ?Db:nets_options(Config)),
%%     ?LOG(info, "Pod setup:~n~s", [pods_format([Pod])]),
%%     ?Net:pod_setup(Pod, Config),
%%     ?Agent:terminate(Pod, Config),
%%     State.

%% pod_join(PodResource, #{config := Config} = Data) ->
%%     {Pod, NetPods} = pods(PodResource, Config),
%%     ?LOG(info, "Pod joining:~n~s", [pods_format([Pod])]),
%%     ?LOG(info, "Pods to join:~n~s", [pods_format(NetPods)]),
%%     ?Net:pod_join(Pod, NetPods, Config),
%%     Data.

%% pod_leave(PodResource,  #{config := Config} = Data) ->
%%     {Pod, NetPods} = pods(PodResource, Config),
%%     ?LOG(info, "Pod leaving:~n~s", [pods_format([Pod])]),
%%     ?LOG(info, "Pods to leave:~n~s", [pods_format(NetPods)]),
%%     ?Net:pod_leave(Pod, NetPods, Config),
%%     Data.

%% pods(PodResource,  Config) ->
%%     GlobalNetsOptions = ?Db:nets_options(Config),
%%     Pod = pod(PodResource, GlobalNetsOptions),

%%     {ok, PodResources} = ?Pod:get({label, maps:get(selector, Config)}, Config),
%%     Filters = [
%% 	{with_nets, ?Net:pod_net_names(Pod)},
%% 	{without_pods, [maps:get(name, Pod)]}
%%     ],
%%     {Pod, ?Tools:pods(PodResources, GlobalNetsOptions, Filters, Config)}.

%% pod(#{namespace := Namespace,
%%       pod_name := PodName,
%%       pod_ip := PodIp,
%%       nets_data := NetsData},
%%     GlobalNetsOptions) ->
%%     #{namespace => Namespace,
%%       name => PodName,
%%       ip => PodIp,
%%       nets => ?Tools:pod_nets(NetsData, GlobalNetsOptions)
%%      }.

%% pods_format(Pods) ->
%%     Indent = 2,
%%     lists:flatten([
%% 	?Format:pod(Pod) ++ "\n" ++
%% 	?Format:pod_nets(maps:get(nets, Pod), Indent) ++ "\n" ||
%% 	Pod <- Pods
%%     ]).
