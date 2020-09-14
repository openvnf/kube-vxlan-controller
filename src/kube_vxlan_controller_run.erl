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
-define(PodReg, kube_vxlan_controller_pod_reg).
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

callback_mode() -> [handle_event_function, state_enter].

init([#{server := Server,
	ca_cert_file := CaCertFile,
	selector := Selector} = Config]) ->
    process_flag(trap_exit, true),

    ?PodReg:clear(),
    ResourceVersion = ?Db:load_resource_version(Selector, Config),
    Data0 = ?State:set_resource_version(ResourceVersion, Config),

    {Host, Port} = ?K8s:uri_parse(Server),
    Opts = #{connect_timeout => ?TIMEOUT,
	     protocols => [http2],
	     transport => tls,
	     tls_opts => [{cacertfile, CaCertFile}]
	    },
    {ok, ConnPid} = gun:open(Host, Port, Opts),
    ?LOG(info, "~p: connecting to ~s", [ConnPid, Server]),
    MRef = monitor(process, ConnPid),

    Data = Data0#{cycle => make_ref(),
		  config => Config,
		  conn => ConnPid,
		  mref => MRef,
		  pending => <<"">>
		 },
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
	  [{"labelSelector", Selector}
	   %%,{"timeoutSeconds", "10"}
	  ]),
    Path = uri_string:recompose(#{path => Resource, query => Query}),
    ?LOG(debug, "Path: ~p", [Path]),
    Headers = headers(Token),

    StreamRef = gun:get(ConnPid, Path, Headers),
    {next_state, {loading, init}, Data#{stream => StreamRef}};

handle_event(enter, _, {watch, init}, #{pending := Pending})
  when Pending =/= <<>> ->
    ?LOG(error, "initial load incomplete: ~p", [Pending]),
    {stop, {error, incomplete}};

handle_event(enter, _, {watch, init},
	     #{conn := ConnPid, selector := Selector, token := Token} = Data0) ->

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
    {keep_state, Data#{stream => StreamRef}};

handle_event(enter, _, _, _) ->
    keep_state_and_data;

handle_event(info, {gun_response, ConnPid, _StreamRef, nofin, 200, _Headers}, {loading, init},
	     #{conn := ConnPid} = Data) ->
    {next_state, {loading, data}, Data};

handle_event(info, {gun_response, ConnPid, _StreamRef, fin, 200, _Headers}, {loading, _} = State,
	     #{conn := ConnPid} = Data0) ->
    Data = handle_api_data(State, <<>>, Data0),
    {next_state, {watch, init}, Data};

handle_event(info, {gun_data, ConnPid, StreamRef, fin, Bin}, {loading, data} = State,
	     #{conn := ConnPid, stream := StreamRef} = Data0) ->
    Data = handle_api_data(State, Bin, Data0),
    {next_state, {watch, init}, Data};

handle_event(info, {gun_response, ConnPid, StreamRef, nofin, 200, _Headers}, {watch, init},
	     #{conn := ConnPid, stream := StreamRef} = Data) ->
    {next_state, {watch, data}, Data};

handle_event(info, {gun_response, ConnPid, StreamRef, fin, Status, _Headers}, _,
	     #{conn := ConnPid, stream := StreamRef}) ->
    ?LOG(debug, "~p: stream closed with status ~p", [ConnPid, Status]),
    {stop, normal};

handle_event(info, {gun_data, ConnPid, StreamRef, nofin, Bin}, State,
	     #{conn := ConnPid, stream := StreamRef} = Data0) ->
    Data = handle_api_data(State, Bin, Data0),
    {keep_state, Data};

handle_event(info, {gun_down, ConnPid, _Protocol, State, _Streams}, _, #{conn := ConnPid}) ->
    ?LOG(debug, "~p: connection closed with ~p", [ConnPid, State]),
    {stop, normal};

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

handle_api_data(State, Bin, #{pending := In} = Data) ->
    handle_api_data(State, Data#{pending := <<In/binary, Bin/binary>>}).

handle_api_data(State, #{pending := In} = Data0) ->
    case binary:split(In, <<$\n>>) of
	[In] -> %% no newline,
	    Data0;
	[Head, Tail] ->
	    Data = process_api_data(State, Head, Data0),
	    handle_api_data(State, Data#{pending => Tail})
    end.

process_api_data(State, Bin, #{selector := Selector} = Data0) ->
    case jsx:decode(Bin, ?JsonDecodeOptions) of
	Object when is_map(Object) ->
	    Data = process_api_object(State, Object, Data0),

	    ResourceVersion = ?State:resource_version(Data),
	    ?Db:save_resource_version(Selector, ResourceVersion, Data),
	    Data;

	Ev ->
	    ?LOG(info, "unexpected message from k8s: ~p", [Ev]),
	    Data0
    end.

process_api_object({watch, _}, #{type := Type, object := Object}, Data) ->
    {Kind, Resource} = read_event(Object, Data),
    process_event(Kind, atom(Type), Resource, Data),
    ?State:set_resource_version(Resource, Data);

process_api_object({loading, _}, #{items := Pods}, Data) ->
    lists:foldl(
      fun(Pod, Acc) ->
	      {Kind, Resource} = read_event(Pod#{kind => pod}, Acc),
	      %%?LOG(debug, #{event => EventType, resource => Resource}),

	      process_event(Kind, init, Resource, Acc),
	      ?State:set_resource_version(Resource, Acc)
      end, Data, Pods).

process_event(pod, Type, #{pod_name := Name} = Pod, #{cycle := Cycle, config := Config}) ->
    ?LOG(info, "POD: ~p -> ~p", [Name, Type]),
    ?Pod:process_event(Cycle, Type, Pod, Config),
    ok;
process_event(_Kind, _Type, _Resource, _Data) ->
    ok.

atom(Bin) when is_binary(Bin) ->
    binary_to_atom(string:lowercase(Bin), latin1);
atom(Atom) when is_atom(Atom) ->
    Atom.

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
