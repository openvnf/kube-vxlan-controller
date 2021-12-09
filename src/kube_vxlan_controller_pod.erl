-module(kube_vxlan_controller_pod).

-behavior(gen_statem).

%% API
-export([start_link/3, stop/1, init_state/1, process_event/4, bridge_cmd/5]).
-export([get/1, get/2, get/3, exec/5]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

-include_lib("kernel/include/logger.hrl").
-include("include/kube_vxlan_controller.hrl").

-define(Db, kube_vxlan_controller_db).
-define(K8s, kube_vxlan_controller_k8s).
-define(Net, kube_vxlan_controller_net).
-define(Pod, kube_vxlan_controller_pod).
-define(Agent, kube_vxlan_controller_agent).
-define(Tools, kube_vxlan_controller_tools).
-define(Format, kube_vxlan_controller_format).
-define(PodReg, kube_vxlan_controller_pod_reg).

-define(STDOUT, 1).
-define(STDERR, 2).

-define(ExecQuery, [
    {"stdout", "true"},
    {"stderr", "true"}
]).

-record(data, {uid, pod, bridges, config}).

%%%===================================================================
%%% API
%%%===================================================================

%%-define(DEBUG_OPTS, [{install, {fun logger_sys_debug:logger_gen_statem_trace/3, ?MODULE}}]).
-define(DEBUG_OPTS, []).
-define(JsonDecodeOptions, [return_maps, {labels, atom}]).

start_link(Id, Event, Config) ->
    Opts = [{debug, ?DEBUG_OPTS}],
    %%Opts = [],
    gen_statem:start_link(?MODULE, [Id, Event, Config], Opts).

stop(Server) ->
    gen_statem:cast(Server, stop).

init_state(Server) ->
    gen_statem:cast(Server, init).

process_event(Cycle, Type, #{pod_name := Name, pod_uid := PodUid} = Pod, Config) ->
    ?PodReg:process_event(#uid{id = Name, uid = PodUid, cycle = Cycle}, {Type, Pod}, Config).

bridge_cmd(Action, Version, #{owner := Pid} = Pod, NetNames, IP) ->
    gen_statem:cast(Pid, {bridge_cmd, Action, Version, Pod, NetNames, IP}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> [handle_event_function, state_enter].

init([Id, Event, Config]) ->
    ?LOG(info, "Pod ~p started", [Id]),
    Data = #data{uid = Id, pod = #{}, bridges = #{}, config = Config},
    self() ! Event,
    {ok, pending, Data}.

handle_event(enter, _, pending, _Data) ->
    keep_state_and_data;

handle_event(enter, _, setup, #data{uid = Id, config = Config,
				    pod = #{pod_name := Name} = PodResource}) ->
    ?LOG(debug, "====== Pod ~s (~p): Setup", [Name, Id#uid.id]),

    UseInitAgentConfig =
	Config#{agent_container_name => maps:get(agent_init_container_name, Config)},

    Pod = pod(PodResource, ?Db:nets_options()),
    ?LOG(info, "Pod setup:~n~s", [pods_format([Pod])]),
    ?Net:pod_setup(Pod, UseInitAgentConfig),
    ?Agent:terminate(Pod, UseInitAgentConfig),

    keep_state_and_data;

handle_event(enter, _, join, #data{uid = Id, pod = #{pod_name := Name} = PodResource}) ->
    ?LOG(debug, "====== Pod ~s (~p): Join", [Name, Id#uid.id]),

    {Pod, NetPods} = pods(Id, PodResource),
    ?LOG(info, "Pod joining (~p):~n~s", [Id#uid.id, pods_format([Pod])]),
    ?LOG(info, "Pods to join (~p):~n~s", [Id#uid.id, pods_format(NetPods)]),

    ?Net:pod_join(Pod, NetPods),

    keep_state_and_data;

handle_event(enter, _, leave, #data{uid = Id, pod = #{pod_name := Name} = PodResource}) ->
    ?LOG(debug, "====== Pod ~s (~p): Leave", [Name, Id#uid.id]),
    ?PodReg:unset(Id),

    {Pod, NetPods} = pods(Id, PodResource),
    ?LOG(info, "Pod leaving (~p):~n~s", [Id#uid.id, pods_format([Pod])]),
    ?LOG(info, "Pods to leave (~p):~n~s", [Id#uid.id, pods_format(NetPods)]),
    ?Net:pod_leave(Pod, NetPods),

    keep_state_and_data;

handle_event(enter, _, State, #data{uid = Id, pod = #{pod_name := Name}}) ->
    ?LOG(debug, "====== Pod ~s (~p): Enter ~p", [Name, Id#uid.id, State]),
    keep_state_and_data;

handle_event(info, {deleted, _Pod}, join = State, #data{uid = Id} = Data) ->
    ?LOG(debug, "====== Pod ~p: deleted in state ~p", [Id#uid.id, State]),
    {next_state, leave, Data, [postpone]};

handle_event(info, {deleted, _Pod}, State, #data{uid = Id}) ->
    ?LOG(debug, "====== Pod ~p: deleted in state ~p", [Id#uid.id, State]),
    {stop, normal};

handle_event(info, {init, Pod}, State, #data{uid = Id} = Data) ->
    case next_state(Pod, State) of
	leave ->
	    ok;
	_ ->
	    true = ?PodReg:set(Id, pod(Pod, ?Db:nets_options()))
    end,
    ?LOG(debug, "====== Pod ~p: init in ~p", [Id#uid.id, State]),
    {keep_state, Data#data{pod = Pod}};

handle_event(info, {_, Pod}, State, #data{uid = Id} = Data) ->
    NextState = next_state(Pod, State),
    true = ?PodReg:set(Id, pod(Pod, ?Db:nets_options())),
    ?LOG(debug, "====== Pod ~p: ~p -> ~p", [Id#uid.id, State, NextState]),
    {next_state, NextState, Data#data{pod = Pod}};

handle_event(cast, init, pending = State, #data{uid = Id, pod = Pod} = Data) ->
    NextState = next_state(Pod, State),
    ?LOG(debug, "====== Pod ~p: ~p -> ~p", [Id#uid.id, State, NextState]),
    {next_state, NextState, Data};

handle_event(cast, stop, State, #data{uid = Id} = _Data) ->
    ?LOG(debug, "====== Pod ~p: In ~w, CleanUp Stop", [Id#uid.id, State]),
    {stop, normal};

handle_event(cast, {bridge_cmd, Action, Version, Pod, NetNames, IP}, join, Data0) ->
    Data = bridges(Action, Version, Pod, NetNames, IP, Data0),
    {keep_state, Data};
handle_event(cast, {bridge_cmd, _, _, _, _}, leave, #data{uid = Id}) ->
    ?LOG(debug, "====== Pod ~p: skipping bridge_cmd in state leave", [Id]),
    keet_state_and_data;
handle_event(cast, {bridge_cmd, _, _, _, _}, State, #data{uid = Id}) ->
    ?LOG(debug, "====== Pod ~p: postpone bridge_cmd in state ~p", [Id, State]),
    {keep_state_and_data, postpone};

handle_event(_, {gun_down, _,ws ,normal, _}, _State, _Data) ->
    %% normal gun Ws termination
    keep_state_and_data;

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

next_state(#{init_agent := running}, _) ->
    setup;
next_state(#{agent := running}, _) ->
    join;
next_state(#{agent := terminated}, _) ->
    leave;
next_state(_, State) ->
    State.

bridges(Action, Version, Pod, NetNames, IP, Data) ->
    lists:foldl(
      fun(NetName, D) -> bridge(Action, Version, Pod, NetName, IP, D) end, Data, NetNames).

bridge_action(delete, Key, _ActionVersion, #data{bridges = Bridges} = Data) ->
    Data#data{bridges = maps:remove(Key, Bridges)};
bridge_action(Action, Key, ActionVersion, #data{bridges = Bridges} = Data) ->
    Data#data{bridges = maps:put(Key, ActionVersion, Bridges)}.

bridge(Action, ActionVersion, Pod, NetName, IP,
       #data{uid = Id, bridges = Bridges, config = Config} = Data) ->
    Key = {NetName, IP},
    case maps:get(Key, Bridges, 0) of
	Version when Version < ActionVersion ->
	    ?LOG(debug, "====== Pod ~p: executing bridge_cmd ~0p for ~0p, ~0p < ~0p",
		 [Id#uid.id, Action, Key, Version, ActionVersion]),
	    ?Net:bridge(Action, Pod, NetName, IP, Config),
	    bridge_action(Action, Key, ActionVersion, Data);
	Version ->
	    ?LOG(debug, "====== Pod ~p: skipping bridge_cmd ~0p for ~0p, ~0p > ~0p",
		 [Id#uid.id, Action, Key, Version, ActionVersion]),
	    Data
    end.

pods(Id, PodResource) ->
    GlobalNetsOptions = ?Db:nets_options(),
    Pod = pod(PodResource, GlobalNetsOptions),

    Filters =
	[{with_nets, ?Net:pod_net_names(Pod)},
	 {without_pods, [maps:get(name, Pod)]}],
    NetPods = ?Tools:pods(?PodReg:all(Id#uid.cycle), Filters),
    {Pod, NetPods}.

pod(#{namespace := Namespace, pod_name := PodName, pod_ip := PodIp,
	  nets_data := NetsData, resource_version := Version},
    GlobalNetsOptions) ->
    #{owner => self(),
      version => Version,
      namespace => Namespace,
      name => PodName,
      ip => PodIp,
      nets => ?Tools:pod_nets(NetsData, GlobalNetsOptions)
     }.

pods_format(Pods) ->
    Indent = 2,
    lists:flatten([
	?Format:pod(Pod) ++ "\n" ++
	?Format:pod_nets(maps:get(nets, Pod), Indent) ++ "\n" ||
	Pod <- Pods
    ]).

get(Config) -> get(all, {label, false}, Config).
get(Filter = {label, _Selector}, Config) -> get(all, Filter, Config).

get(Namespace, Filter, Config) ->
    Resource = case {Namespace, Filter} of
	{all, Filter} ->
	    "/api/v1/pods";
	{Namespace, {label, _Selector}} -> fmt(
	    "/api/v1/namespaces/~s/pods", [Namespace]
	);
	{Namespace, {pod, Name}} -> fmt(
	    "/api/v1/namespaces/~s/pods/~s", [Namespace, Name]
	)
    end,
    Query =
	case Filter of
	    {pod, _Name}      -> [];
	    {label, false}    -> [];
	    {label, Selector} -> [{"labelSelector", Selector}]
    end,
    case ?K8s:http_request(Resource, Query, Config) of
	{ok, Result} -> {ok, maps:get(items, Result, Result)};
	{error, Reason} -> {error, Reason}
    end.

exec(Namespace, PodName, ContainerName, Command, Config) ->
    Silent = maps:get(silent, Config, false),

    Silent orelse
	?LOG(info, "~s/~s/~s: ~s", [Namespace, PodName, ContainerName, Command]),

    Resource = fmt("/api/v1/namespaces/~s/pods/~s/exec", [Namespace, PodName]),

    Query = lists:foldl(
	fun(Arg, ExecQuery) -> [{"command", Arg}|ExecQuery] end,
	[{"container", ContainerName}|?ExecQuery],
	lists:reverse(string:split(Command, " ", all))
    ),

    case ?K8s:ws_connect(Resource, Query, Config) of
	{ok, ConnPid, StreamRef} ->
	    case ?K8s:ws_recv(ConnPid, StreamRef) of
		{ok, Result} ->
		    ?K8s:ws_close(ConnPid),
		    Silent orelse ?LOG(info, "~p", [Result]),
		    maps:get(?STDOUT, Result, <<>>);
		{error, Reason} ->
		    ?K8s:ws_close(ConnPid),
		    ?LOG(error, #{reason => Reason}),
		    <<>>
	    end;
	{error, Reason} ->
	    ?LOG(error, #{reason => Reason}),
	    <<>>
    end.

fmt(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
