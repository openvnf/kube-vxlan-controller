-module(kube_vxlan_controller_pod).

-behavior(gen_statem).

%% API
-export([start_link/3, stop/1, init_state/1, process_event/4]).
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

-record(data, {uid, pod, config}).

%%%===================================================================
%%% API
%%%===================================================================

-define(DEBUG_OPTS, [{install, {fun logger_sys_debug:logger_gen_statem_trace/3, ?MODULE}}]).
-define(JsonDecodeOptions, [return_maps, {labels, atom}]).

start_link(Id, Event, Config) ->
    Opts = [{debug, ?DEBUG_OPTS}],
    %%Opts = [],
    gen_statem:start_link(?MODULE, [Id, Event, Config], Opts).

stop(Server) ->
    gen_statem:cast(Server, stop).

init_state(Server) ->
    gen_statem:cast(Server, init).

process_event(Cycle, Type, #{pod_name := Name} = Pod, Config) ->
    ?PodReg:process_event(#uid{id = Name, cycle = Cycle}, {Type, Pod}, Config).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> [handle_event_function, state_enter].

init([Id, Event, Config]) ->
    ?LOG(info, "Pod ~p started", [Id]),
    Data = #data{uid = Id, pod = #{}, config = Config},
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

handle_event(enter, _, join, #data{uid = Id, config = Config,
				   pod = #{pod_name := Name} = PodResource}) ->
    ?LOG(debug, "====== Pod ~s (~p): Join", [Name, Id#uid.id]),

    {Pod, NetPods} = pods(Id, PodResource, Config),
    ?LOG(info, "Pod joining:~n~s", [pods_format([Pod])]),
    ?LOG(info, "Pods to join:~n~s", [pods_format(NetPods)]),

    ?Net:pod_join(Pod, NetPods, Config),

    keep_state_and_data;

handle_event(enter, _, leave, #data{uid = Id, config = Config,
				   pod = #{pod_name := Name} = PodResource}) ->
    ?LOG(debug, "====== Pod ~s (~p): Leave", [Name, Id#uid.id]),
    ?PodReg:unset(Id),

    {Pod, NetPods} = pods(Id, PodResource, Config),
    ?LOG(info, "Pod leaving:~n~s", [pods_format([Pod])]),
    ?LOG(info, "Pods to leave:~n~s", [pods_format(NetPods)]),
    ?Net:pod_leave(Pod, NetPods, Config),

    keep_state_and_data;

handle_event(enter, _, State, #data{uid = Id, pod = #{pod_name := Name}}) ->
    ?LOG(debug, "====== Pod ~s (~p): Enter ~p", [Name, Id#uid.id, State]),
    keep_state_and_data;

handle_event(info, {deleted, _Pod}, State, #data{uid = Id}) ->
    ?LOG(debug, "====== Pod ~p: Terminated in state ~p", [Id#uid.id, State]),
    {stop, normal};

handle_event(info, {init, Pod}, State, #data{uid = Id} = Data) ->
    case next_state(Pod, State) of
	leave ->
	    ok;
	_ ->
	    true = ?PodReg:set(Id, pod(Pod, ?Db:nets_options()))
    end,
    ?LOG(debug, "====== Pod ~p: ~p", [Id#uid.id, State]),
    {keep_state, Data#data{pod = Pod}};

handle_event(info, {_, Pod}, State, #data{uid = Id} = Data) ->
    NextState = next_state(Pod, State),
    true = ?PodReg:set(Id, pod(Pod, ?Db:nets_options())),
    ?LOG(debug, "====== Pod ~p: ~p", [Id#uid.id, NextState]),
    {next_state, NextState, Data#data{pod = Pod}};

handle_event(cast, init, pending = State, #data{uid = Id, pod = Pod} = Data) ->
    NextState = next_state(Pod, State),
    ?LOG(debug, "====== Pod ~p: ~p", [Id#uid.id, NextState]),
    {next_state, NextState, Data};

handle_event(cast, stop, State, #data{uid = Id} = _Data) ->
    ?LOG(debug, "====== Pod ~p: In ~w, CleanUp Stop", [Id#uid.id, State]),
    {stop, normal};

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

pods(Id, PodResource, Config) ->

    GlobalNetsOptions = ?Db:nets_options(),
    Pod = pod(PodResource, GlobalNetsOptions),

    {ok, PodResources} = ?Pod:get({label, maps:get(selector, Config)}, Config),
    Filters = [
	{with_nets, ?Net:pod_net_names(Pod)},
	{without_pods, [maps:get(name, Pod)]}
    ],

    NetPods = ?Tools:pods(?PodReg:all(Id#uid.cycle), Filters),
    {Pod, NetPods}.

pod(#{namespace := Namespace,
      pod_name := PodName,
      pod_ip := PodIp,
      nets_data := NetsData},
    GlobalNetsOptions) ->
    #{namespace => Namespace,
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
