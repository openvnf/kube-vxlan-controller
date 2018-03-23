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
-define(Tools, kube_vxlan_controller_tools).
-define(Config, kube_vxlan_controller_config).
-define(Inspect, kube_vxlan_controller_inspect).

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
    io:format("~p~n", [?Config:read(NamedArgs)]);

do(Action, {NamedArgs, OrderedArgs}) ->
    case ?Config:load(NamedArgs) of
        {ok, Config} -> do(Action, OrderedArgs, Config);
        {error, Reason} -> io:format("~p~n", [Reason])
    end.

do(run, _Args, Config) ->
    Selector = maps:get(selector, Config),
    ResourceVersion = ?Db:load_resource_version(Selector, Config),
    State = ?State:set_resource_version(ResourceVersion, #{}),

    run(Config, State);

do({inspect, Subject}, Args, Config) ->
    ?Inspect:Subject(Args, Config).

run(Config, State) ->
    ResourceVersion = ?State:resource_version(State),
    Selector = maps:get(selector, Config),

    NewState = case ?State:is_resource_version_shown(State) of
        true -> State;
        false ->
            ?Log:info("Watching pods (selector: ~s) from version: ~s",
                      [Selector, ResourceVersion]),
            ?Db:save_resource_version(Selector, ResourceVersion, Config),
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

process_event(_Event, _Resource, _Config, State) -> State.

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
    event_type(Kind, Type),
    #{resource_version => binary_to_list(ResourceVersion),
      namespace => binary_to_list(Namespace),
      pod_uid => binary_to_list(PodUid),
      pod_name => binary_to_list(PodName),
      pod_ip => binary_to_list(maps:get(podIP, Status, <<>>)),
      networks_data => ?Tools:networks_data(Annotations, Config),
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
};

read_event(#{
    type := Type,
    object := #{
      code := Code,
      kind := Kind,
      message := Message = <<"too old resource version:", Versions/binary>>,
      reason := Reason,
      status := Status
    }
}, _Config) -> {
    event_type(Kind, Type),
    #{code => Code,
      reason => binary_to_list(Reason),
      status => binary_to_list(Status),
      message => binary_to_list(Message),
      resource_version => begin
          [_DesiredVersion, OldestVersion] = string:lexemes(Versions, "( )"),
          integer_to_list(binary_to_integer(OldestVersion) - 1)
      end
    }
}.

event_type(Kind, Type) ->
    EventType = <<(string:lowercase(Kind))/binary, "_",
                  (string:lowercase(Type))/binary>>,
    binary_to_atom(EventType, latin1).

is_container_ready_fun(ContainerName) -> fun(#{name := Name, state := State}) ->
    list_to_binary(ContainerName) == Name andalso maps:is_key(running, State)
end.

handle_pod_initialisation(#{
    namespace := Namespace,
    pod_name := PodName,
    networks_data := NetworksData
}, Config, State) ->
    NameIdMap = ?Db:networks_name_id_map(Config),
    Networks = ?Tools:networks(NetworksData, NameIdMap),

    ?Log:info("Pod initialisation ~p:", [{Namespace, PodName, Networks}]),

    AgentConfig = maps:put(agent_container_name,
                  maps:get(agent_init_container_name, Config), Config),

    lists:foreach(fun({_NetName, Net}) ->
        ?Net:pod_init(Namespace, PodName, Net, AgentConfig)
    end, Networks),

    ?Agent:terminate(Namespace, PodName, AgentConfig),

    State.

handle_pod_added(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    networks_data := NetworksData
}, Config, State) ->
    NameIdMap = ?Db:networks_name_id_map(Config),
    Networks = ?Tools:networks(NetworksData, NameIdMap),

    ?Log:info("Pod added: ~p", [{Namespace, PodName, PodIp, Networks}]),

    {ok, Pods} = ?Pod:get({label, maps:get(selector, Config)}, Config),

    lists:foreach(fun({NetName, Net}) ->
        NetPods = ?Tools:network_members(
            NetName, PodName, Pods, NameIdMap, Config
        ),
        ?Log:info("Pods within \"~s\" to join:~n~s",
                  [NetName, pods_format(NetPods)]),
        ?Net:pod_add(Namespace, PodName, PodIp, Net, NetPods, Config)
    end, Networks),

    State.

handle_pod_deleted(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    networks_data := NetworksData
}, Config, State) ->
    NameIdMap = ?Db:networks_name_id_map(Config),
    Networks = ?Tools:networks(NetworksData, NameIdMap),

    ?Log:info("Pod deleted: ~p", [{Namespace, PodName, PodIp, Networks}]),

    {ok, Pods} = ?Pod:get({label, maps:get(selector, Config)}, Config),

    lists:foreach(fun({NetName, Net}) ->
        NetPods = ?Tools:network_members(
            NetName, PodName, Pods, NameIdMap, Config
        ),
        ?Log:info("Pods within \"~s\" to leave:~n~s",
                  [NetName, pods_format(NetPods)]),
        ?Net:pod_delete(Namespace, PodName, PodIp, Net, NetPods, Config)
    end, Networks),

    State.

pods_format(Pods) ->
    Format = lists:flatten(lists:duplicate(length(Pods), "~p~n")),
    lists:flatten(io_lib:format(Format, Pods)).
