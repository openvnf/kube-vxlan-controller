-module('kube-vxlan-controller').

-export([main/1, run/2, config/0]).

-define(K8s, kube_vxlan_controller_k8s_client).
-define(Net, kube_vxlan_controller_net).
-define(Pod, kube_vxlan_controller_pod).
-define(Log, kube_vxlan_controller_log).

-define(Server, "https://api.k8s.nce-01.fra-01.eu.cennso.net").
-define(NamespaceFile, "pki/namespace").
-define(CaCertFile, "pki/ca.pem").
-define(TokenFile, "pki/token").

-define(VxlanConfigName, "kube-vxlan-controller").
-define(AgentContainerName, "vxlan-controller-agent").

main(_) ->
    application:ensure_all_started(?MODULE),
    run(config(), #{}).

config() -> #{
    server => ?Server,
    namespace => binary_to_list(element(2, file:read_file(?NamespaceFile))),
    ca_cert_file => ?CaCertFile,
    token => binary_to_list(element(2, file:read_file(?TokenFile))),
    vxlan_config_name => ?VxlanConfigName,
    agent_container_name => ?AgentContainerName
}.

run(Config, State) ->
    ?Log:info("Watching pods from version: ~s", [resource_version(State)]),

    Resource = "/api/v1/watch/pods",
    Query = [
        {"labelSelector", "vxlan=true"},
        {"resourceVersion", resource_version(State)},
        {"timeoutSeconds", "10"}
    ],
    case ?K8s:http_stream_request(Resource, Query, Config) of
        {ok, Stream} -> run(Stream, Config, State);
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
    {EventType, Resource} = read_event(Event),
    ?Log:info("~s~n~p", [EventType, Resource]),

    NewState = set_resource_version(Resource, State),
    process_event(EventType, Resource, Config, NewState)
end.

process_event(pod_added, Pod = #{phase := "Pending"}, _Config, State) ->
    set_pod_pending(add, Pod, State);

process_event(pod_added, Pod = #{phase := "Running"}, Config, State) ->
    handle_pod_added(Pod, Config, State);

process_event(pod_modified, #{phase := "Pending"}, _Config, State) ->
    State;

process_event(pod_modified, Pod = #{phase := "Running"}, Config, State) ->
    case pod_pending_action(Pod, State) of
        {ok, add} ->
            NewState = unset_pod_pending(Pod, State),
            handle_pod_added(Pod, Config, NewState);
        {ok, modify} ->
            State;
        error ->
            set_pod_pending(modify, Pod, State)
    end;

process_event(pod_deleted, Pod, Config, State) ->
    case pod_pending_action(Pod, State) of
        {ok, add} -> State;
        {ok, modify} ->
            NewPod = merge_pod_pending_info(Pod, State),
            NewState = unset_pod_pending(NewPod, State),
            handle_pod_deleted(NewPod, Config, NewState);
        error -> State
    end.

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
}) -> {
    list_to_atom(
        string:lowercase(binary_to_list(Kind)) ++
        [$_|string:lowercase(binary_to_list(Type))]
    ),
    #{resource_version => binary_to_list(ResourceVersion),
      namespace => binary_to_list(Namespace),
      pod_uid => binary_to_list(PodUid),
      pod_name => binary_to_list(PodName),
      pod_ip => binary_to_list(maps:get(podIP, Status, <<>>)),
      vxlan_names => ?Pod:vxlan_names(Annotations),
      phase => binary_to_list(Phase)}
}.

handle_pod_added(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    vxlan_names := VxlanNames
}, Config, State) ->
    ?Log:info("Pod added ~p:", [{Namespace, PodName, PodIp, VxlanNames}]),

    VxlanIds = ?Net:vxlan_ids(Config),
    Pods = ?Pod:list(Config),

    lists:foreach(fun(VxlanName) ->
        case maps:find(VxlanName, VxlanIds) of
            {ok, VxlanId} -> add_pod_to_vxlan(
                Namespace, PodName, PodIp,
                VxlanName, VxlanId, Pods, Config
            );
            error ->
                ?Log:error("Vxlan Id for \"~s\" not found", [VxlanName])
        end
    end, VxlanNames),

    State.

handle_pod_deleted(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    vxlan_names := VxlanNames
}, Config, State) ->
    ?Log:info("Pod deleted ~p:", [{Namespace, PodName, PodIp, VxlanNames}]),

    Pods = ?Pod:list(Config),

    lists:foreach(fun(VxlanName) ->
        delete_pod_from_vxlan(Namespace, PodName, PodIp, VxlanName, Pods, Config)
    end, VxlanNames),

    State.

add_pod_to_vxlan(Namespace, PodName, PodIp, VxlanName, VxlanId, Pods, Config) ->
    ?Net:vxlan_add(Namespace, PodName, VxlanName, VxlanId, Config),
    ?Net:vxlan_up(Namespace, PodName, VxlanName, Config),
    VxlanPods = ?Pod:filter(vxlan, VxlanName, PodName, Pods),
    lists:foreach(fun({VxlanPodName, VxlanPodIp}) ->
        ?Net:bridge_append(Namespace, VxlanPodName, VxlanName, PodIp, Config),
        ?Net:bridge_append(Namespace, PodName, VxlanName, VxlanPodIp, Config)
    end, VxlanPods).

delete_pod_from_vxlan(Namespace, PodName, PodIp, VxlanName, Pods, Config) ->
    VxlanPods = ?Pod:filter(vxlan, VxlanName, PodName, Pods),
    lists:foreach(fun({VxlanPodName, _VxlanPodIp}) ->
        ?Net:bridge_delete(Namespace, VxlanPodName, VxlanName, PodIp, Config)
    end, VxlanPods).

merge_pod_pending_info(Pod = #{pod_uid := PodUid}, State) ->
    #{pending_info := Info} = maps:get(PodUid, State),
    maps:merge(Pod, Info).

pod_pending_action(#{pod_uid := PodUid}, State) ->
    case maps:find(PodUid, State) of
        {ok, #{pending_action := Action}} -> {ok, Action};
        error -> error
    end.

set_pod_pending(Action, #{pod_uid := PodUid, pod_ip := PodIp}, State) ->
    maps:put(PodUid, #{pending_action => Action,
                       pending_info => #{pod_ip => PodIp}}, State).

unset_pod_pending(#{pod_uid := PodUid}, State) ->
    maps:remove(PodUid, State).

resource_version(State) ->
    maps:get(resource_version, State, "0").

set_resource_version(#{resource_version := Value}, State) ->
    OldValue = list_to_integer(resource_version(State)),
    ProposedValue = list_to_integer(Value),

    case ProposedValue > OldValue of
        true ->
            NewValue = integer_to_list(ProposedValue + 1),
            maps:put(resource_version, NewValue, State);
        false -> State
    end.
