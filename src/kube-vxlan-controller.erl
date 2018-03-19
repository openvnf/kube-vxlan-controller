-module('kube-vxlan-controller').

-export([main/1]).

-define(Db, kube_vxlan_controller_db).
-define(K8s, kube_vxlan_controller_k8s).
-define(Cli, kube_vxlan_controller_cli).
-define(Net, kube_vxlan_controller_net).
-define(Pod, kube_vxlan_controller_pod).
-define(Log, kube_vxlan_controller_log).
-define(Agent, kube_vxlan_controller_agent).
-define(State, kube_vxlan_controller_state).
-define(Config, kube_vxlan_controller_config).
-define(Inspect, kube_vxlan_controller_inspect).

-define(NetworkType, "vxlan").
-define(NetworkDev, "eth0").

main(CliArgs) ->
    application:ensure_all_started(?MODULE),
    case ?Cli:args(CliArgs) of
        {run, Args} -> do(run, Args);
        {inspect, Subject, Args} -> do({inspect, Subject}, Args);
        {config, Args} -> do(config, Args);
        version -> io:format(?Cli:version(?Config:version()));
        usage -> io:format(?Cli:usage())
    end.

do(config, {NamedArgs, _OrderedArgs}) ->
    io:format("~p~n", [?Config:init()]),
    io:format("~p~n", [?Config:load(NamedArgs)]);

do(Action, {NamedArgs, OrderedArgs}) ->
    ?Config:init(),
    case load_config(NamedArgs) of
        {ok, Config} -> do(Action, OrderedArgs, Config);
        {error, Reason} -> io:format("~p~n", [Reason])
    end.

do(run, _Args, Config) -> run(Config, #{});
do({inspect, Subject}, Args, Config) -> ?Inspect:Subject(Args, Config).

load_config(Args) -> cpf_funs:apply_while([
    {load, fun ?Config:load/1, [Args]},
    {build, fun ?Config:build/1, [{load}]},
    {validate, fun ?Config:validate/1, [{build}]}
]).

run(Config, State) ->
    ResourceVersion = ?State:resource_version(State),
    Selector = maps:get(selector, Config),

    NewState = case ?State:is_resource_version_shown(State) of
        true -> State;
        false ->
            ?Log:info("Watching pods (selector: ~s) from version: ~s",
                      [Selector, ResourceVersion]),
            ?State:set_resource_version_shown(State)
    end,

    Resource = "/api/v1/watch/pods",
    Query = [
        {"labelSelector", Selector},
        {"resourceVersion", ResourceVersion},
        {"timeoutSeconds", "10"}
    ],
    case ?K8s:http_stream_request(Resource, Query, Config) of
        {ok, Stream} -> run(Stream, Config, NewState);
        {error, Reason} -> ?Log:error(Reason)
    end.

run(Stream, Config, State) ->
    case ?K8s:http_stream_read(Stream) of
        {ok, done} -> run(Config, State);
        {ok, Events} ->
            NewState = process_events(Events, Config, State),
            run(Stream, Config, NewState);
        {error, timeout} -> run(Config, State);
        {error, Reason} -> ?Log:error(Reason)
    end.

process_events(Events, Config, State) ->
    lists:foldl(process_event_fun(Config), State, Events).

process_event_fun(Config) -> fun(Event, State) ->
    {EventType, Resource} = read_event(Event, Config),
    ?Log:info("~s~n~p", [EventType, Resource]),

    NewState = ?State:set_resource_version(Resource, State),
    process_event(EventType, Resource, Config, NewState)
end.

process_event(pod_added, Pod = #{phase := "Pending"}, Config, State) ->
    NewState = ?State:set_pod_pending(add, Pod, State),
    case maps:get(init_agent_ready, Pod) of
        true -> handle_pod_initialisation(Pod, Config, NewState);
        false -> NewState
    end;

process_event(
    pod_added,
    Pod = #{phase := "Running", agent_ready := true},
    Config, State
) ->
    handle_pod_added(Pod, Config, State);

process_event(pod_modified, Pod = #{phase := "Pending"}, Config, State) ->
    case maps:get(init_agent_ready, Pod) of
        true -> handle_pod_initialisation(Pod, Config, State);
        false -> State
    end;

process_event(
    pod_modified,
    Pod = #{phase := "Running", agent_ready := true},
    Config, State
) ->
    case ?State:pod_pending_action(Pod, State) of
        {ok, add} ->
            NewState = ?State:unset_pod_pending(Pod, State),
            handle_pod_added(Pod, Config, NewState);
        {ok, modify} ->
            State;
        error ->
            ?State:set_pod_pending(modify, Pod, State)
    end;

process_event(pod_deleted, Pod, Config, State) ->
    case ?State:pod_pending_action(Pod, State) of
        {ok, add} -> State;
        {ok, modify} ->
            NewPod = ?State:merge_pod_pending_info(Pod, State),
            NewState = ?State:unset_pod_pending(NewPod, State),
            handle_pod_deleted(NewPod, Config, NewState);
        error -> State
    end;

process_event(_Event, _Pod, _Config, State) -> State.

read_event(#{
  type := Type,
    object := #{
      kind := Kind,
      metadata := #{
        namespace := Namespace,
        uid := PodUid,
        name := PodName,
        annotations := Annotations,
        resourceVersion := ResourceVersion
      },
      status := Status = #{
        phase := Phase
      }
    }
}, Config) -> {
    list_to_atom(
        string:lowercase(binary_to_list(Kind)) ++
        [$_|string:lowercase(binary_to_list(Type))]
    ),
    #{resource_version => binary_to_list(ResourceVersion),
      namespace => binary_to_list(Namespace),
      pod_uid => binary_to_list(PodUid),
      pod_name => binary_to_list(PodName),
      pod_ip => binary_to_list(maps:get(podIP, Status, <<>>)),
      networks => networks(Annotations, Config),
      phase => binary_to_list(Phase),
      agent_ready => lists:any(
          is_container_ready_fun(maps:get(agent_container_name, Config)),
          maps:get(containerStatuses, Status, [])
      ),
      init_agent_ready => lists:any(
          is_container_ready_fun(maps:get(agent_init_container_name, Config)),
          maps:get(initContainerStatuses, Status, [])
      )
    }
}.

is_container_ready_fun(ContainerName) -> fun(#{name := Name, state := State}) ->
    list_to_binary(ContainerName) == Name andalso maps:is_key(running, State)
end.

handle_pod_initialisation(#{
    namespace := Namespace,
    pod_name := PodName,
    networks := Networks
}, Config, State) ->
    ?Log:info("Pod initialisation ~p:", [{Namespace, PodName, Networks}]),

    NetNames = [Name || {Name, _Network} <- Networks],
    {NameIdMap, NotFoundNames} = ?Db:network_name_id_map(NetNames, Config),

    NotFoundNames == [] orelse
        ?Log:warning("VXLAN Id for these networks not found: ~p",
                     [NotFoundNames]),

    AgentConfig = maps:put(agent_container_name,
                  maps:get(agent_init_container_name, Config), Config),

    lists:foreach(fun({_NetName, Net}) ->
        ?Net:pod_init(Namespace, PodName, Net, AgentConfig)
    end, networks_set_ids(NameIdMap, Networks)),

    ?Agent:terminate(Namespace, PodName, AgentConfig),

    State.

handle_pod_added(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    networks := Networks
}, Config, State) ->
    ?Log:info("Pod added: ~p", [{Namespace, PodName, PodIp, Networks}]),

    Pods = ?Pod:get(maps:get(selector, Config), Config),
    NetNames = [Name || {Name, _Network} <- Networks],

    lists:foreach(fun(NetName) ->
        NetPods = network_members(NetName, PodName, Pods, Config),
        ?Log:info("Pods within \"~s\" to join:~n~s",
                  [NetName, network_members_format(NetPods)]),
        ?Net:pod_add(Namespace, PodName, PodIp, NetName, NetPods, Config)
    end, NetNames),

    State.

handle_pod_deleted(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    networks := Networks
}, Config, State) ->
    ?Log:info("Pod deleted: ~p", [{Namespace, PodName, PodIp, Networks}]),

    Pods = ?Pod:get(maps:get(selector, Config), Config),
    NetNames = [Name || {Name, _Network} <- Networks],

    lists:foreach(fun(NetName) ->
        NetPods = network_members(NetName, PodName, Pods, Config),
        ?Log:info("Pods within \"~s\" to join:~n~s",
                  [NetName, network_members_format(NetPods)]),
        ?Net:pod_delete(Namespace, PodName, PodIp, NetName, NetPods, Config)
    end, NetNames),

    State.

network_new(NetworkName) -> #{
    name => NetworkName,
    type => ?NetworkType,
    dev => ?NetworkDev
}.

networks_set_ids(NameIdMap, Networks) ->
    [{Name, case maps:is_key(id, Network) of
        true -> Network;
        false -> maps:put(id, maps:get(Name, NameIdMap), Network)
    end} || {Name, Network} <- Networks].

networks(Annotations, #{annotation := AnnotationName}) ->
    AnnotationValue = maps:get(AnnotationName, Annotations, <<>>),
    TokensBinary = re:replace(AnnotationValue, "\\h*,\\h*", ",", [global]),
    Tokens = string:lexemes(TokensBinary, ",\n"),
    lists:reverse(lists:foldl(fun networks_build/2, [], Tokens)).

networks_build(Token = <<" ", _/binary>>, Networks) ->
    networks_add_params(string:lexemes(Token, " "), Networks);

networks_build(Token, Networks) ->
    [NetworkNameBinary|NetworkParams] = string:lexemes(Token, " "),
    NetworkName = binary_to_list(NetworkNameBinary),
    Network = {NetworkName, network_new(NetworkName)},
    networks_add_params(NetworkParams, [Network|Networks]).

networks_add_params(Params, Networks) ->
    lists:foldl(fun networks_add_param/2, Networks, Params).

networks_add_param(Option, [{NetworkName, Network}|RestNetworks]) ->
    [KeyBinary|ValueIolist] = string:split(Option, "="),
    Key = binary_to_atom(KeyBinary, latin1),
    Value = binary_to_list(iolist_to_binary(ValueIolist)),
    [{NetworkName, maps:put(Key, Value, Network)}|RestNetworks].

%vxlan_names(Annotations, #{annotation := Annotation}) ->
%    A8nVxlanName = binary_to_list(maps:get(Annotation, Annotations, <<>>)),
%    string:lexemes(A8nVxlanName, ", \n").

network_members(NetworkName, ExcludePodName, Pods, Config) ->
    [{binary_to_list(Namespace),
      binary_to_list(PodName),
      binary_to_list(PodIp)} ||
     #{metadata := #{
         namespace := Namespace,
         name := PodName,
         annotations := Annotations
      },
       status := #{
         podIP := PodIp,
         phase := <<"Running">>
       }
     } <- Pods,
     lists:keymember(NetworkName, 1, networks(Annotations, Config)) andalso
     binary_to_list(PodName) /= ExcludePodName].

network_members_format(Pods) ->
    lists:flatten(io_lib:format(
        lists:flatten(lists:duplicate(length(Pods), "~p~n")), Pods)).
