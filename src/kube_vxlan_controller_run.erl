-module(kube_vxlan_controller_run).

-export([
    loop/1
]).

-define(Db, kube_vxlan_controller_db).
-define(K8s, kube_vxlan_controller_k8s).
-define(Net, kube_vxlan_controller_net).
-define(Pod, kube_vxlan_controller_pod).
-define(Log, kube_vxlan_controller_log).
-define(Agent, kube_vxlan_controller_agent).
-define(State, kube_vxlan_controller_state).
-define(Tools, kube_vxlan_controller_tools).

loop(Config) ->
    Selector = maps:get(selector, Config),
    ResourceVersion = ?Db:load_resource_version(Selector, Config),
    State = ?State:set_resource_version(ResourceVersion, #{}),

    loop(Config, State).

loop(Config, State) ->
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
        {ok, Stream} -> loop(Stream, Config, NewState);
        {error, Reason} -> ?Log:error(Reason)
    end.

loop(Stream, Config, State) ->
    case ?K8s:http_stream_read(Stream) of
        {ok, done} -> loop(Config, State);
        {ok, Events} ->
            NewState = process_events(Events, Config, State),
            loop(Stream, Config, NewState);
        {error, timeout} -> loop(Config, State);
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

process_event(pod_added, Pod, _Config, State) ->
    ?State:set(pod_added, Pod, State);

process_event(pod_deleted, Pod, _Config, State) ->
    ?State:unset(agent_terminated, Pod, State);

process_event(pod_modified, Pod = #{init_agent := running}, Config, State) ->
    pod_init(Pod, Config, State);

process_event(pod_modified, Pod = #{agent := running}, Config, State) ->
    case ?State:is(pod_added, Pod, State) of
        true ->
            NewState = ?State:unset(pod_added, Pod, State),
            pod_setup(Pod, Config, NewState);
        false ->
            State
    end;

process_event(pod_modified, Pod = #{agent := terminated}, Config, State) ->
    case ?State:is(agent_terminated, Pod, State) of
        true -> State;
        false ->
            NewState = ?State:set(agent_terminated, Pod, State),
            pod_cleanup(Pod, Config, NewState)
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
      agent => container_state(
          maps:get(agent_container_name, Config),
          maps:get(containerStatuses, Status, [])
      ),
      init_agent => container_state(
          maps:get(agent_init_container_name, Config),
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

container_state(_ContainerName, []) -> unknown;
container_state(Name, Statuses) ->
    [[State]] = [maps:keys(maps:get(state, Status)) || Status <- Statuses,
                 maps:get(name, Status) == list_to_binary(Name)],
    State.

pod_init(#{
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

pod_setup(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    networks_data := NetworksData
}, Config, State) ->
    NameIdMap = ?Db:networks_name_id_map(Config),
    Networks = ?Tools:networks(NetworksData, NameIdMap),

    ?Log:info("Pod setup: ~p", [{Namespace, PodName, PodIp, Networks}]),

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

pod_cleanup(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    networks_data := NetworksData
}, Config, State) ->
    NameIdMap = ?Db:networks_name_id_map(Config),
    Networks = ?Tools:networks(NetworksData, NameIdMap),

    ?Log:info("Pod cleanup: ~p", [{Namespace, PodName, PodIp, Networks}]),

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
