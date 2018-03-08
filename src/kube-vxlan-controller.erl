-module('kube-vxlan-controller').

-export([main/1, run/2, config/0]).

-define(K8s, kube_vxlan_controller_k8s).
-define(Cli, kube_vxlan_controller_cli).
-define(Net, kube_vxlan_controller_net).
-define(Pod, kube_vxlan_controller_pod).
-define(Log, kube_vxlan_controller_log).
-define(State, kube_vxlan_controller_state).

-define(Server, "https://api.k8s.nce-01.fra-01.eu.cennso.net").
-define(NamespaceFile, "pki/namespace").
-define(CaCertFile, "pki/ca.pem").
-define(TokenFile, "pki/token").

-define(VxlanConfigName, "kube-vxlan-controller").
-define(AgentContainerName, "vxlan-controller-agent").

-define(LabelSelector, "vxlan=true").

-define(MandatoryConfigParams, [
    server,
    namespace,
    ca_cert_file,
    token,
    vxlan_config_name,
    agent_container_name
]).

main(Args) ->
    application:ensure_all_started(?MODULE),
    case ?Cli:read_args(Args) of
        {ok, {run, Config}} -> run(Config);
        {ok, {version, Version}} -> show_version(Version)
    end.

show_version(Version) -> io:format(?Cli:version(Version)).
show_usage() -> io:format(?Cli:usage()).

config() -> #{
    server => ?Server,
    namespace => binary_to_list(element(2, file:read_file(?NamespaceFile))),
    ca_cert_file => ?CaCertFile,
    token => binary_to_list(element(2, file:read_file(?TokenFile))),
    vxlan_config_name => ?VxlanConfigName,
    agent_container_name => ?AgentContainerName
}.

is_config_valid(Config) ->
    maps:size(maps:with(?MandatoryConfigParams, Config)) ==
    length(?MandatoryConfigParams).

run(Config) ->
    case is_config_valid(Config) of
        true -> run(Config, #{});
        false -> show_usage()
    end.

run(Config, State) ->
    ResourceVersion = ?State:resource_version(State),

    NewState = case ?State:is_resource_version_shown(State) of
        true -> State;
        false ->
            ?Log:info("Watching pods from version: ~s", [ResourceVersion]),
            ?State:set_resource_version_shown(State)
    end,

    Resource = "/api/v1/watch/pods",
    Query = [
        {"labelSelector", ?LabelSelector},
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
    {EventType, Resource} = read_event(Event),
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

process_event(pod_added, Pod = #{phase := "Running"}, Config, State) ->
    handle_pod_added(Pod, Config, State);

process_event(pod_modified, Pod = #{phase := "Pending"}, Config, State) ->
    case maps:get(init_agent_ready, Pod) of
        true -> handle_pod_initialisation(Pod, Config, State);
        false -> State
    end;

process_event(pod_modified, Pod = #{phase := "Running"}, Config, State) ->
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
      phase => binary_to_list(Phase),
      init_agent_ready => lists:any(
          fun is_init_agent_ready/1,
          maps:get(initContainerStatuses, Status, [])
      )
    }
}.

is_init_agent_ready(#{name := Name, state := State}) ->
    Name == <<"vxlan-controller-agent-init">> andalso
    maps:is_key(running, State).

handle_pod_initialisation(#{
    namespace := Namespace,
    pod_name := PodName,
    vxlan_names := VxlanNames
}, Config, State) ->
    ?Log:info("Pod initialisation ~p:", [{Namespace, PodName, VxlanNames}]),

    VxlanIds = ?Net:vxlan_ids(Config),

    lists:foreach(fun(VxlanName) ->
        case maps:find(VxlanName, VxlanIds) of
            {ok, VxlanId} ->
                ?Net:vxlan_init_pod(
                    Namespace, PodName, VxlanName, VxlanId,
                    maps:put(agent_container_name,
                             "vxlan-controller-agent-init", Config)
                );
            error ->
                ?Log:error("Vxlan Id for \"~s\" not found", [VxlanName])
        end
    end, VxlanNames),

    ?Pod:exec(
        Namespace, PodName, "vxlan-controller-agent-init",
        "kill -TERM 1", Config
    ),
    ?Pod:exec(
        Namespace, PodName, "vxlan-controller-agent-init",
        "touch /run/terminate", Config
    ),

    State.

handle_pod_added(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    vxlan_names := VxlanNames
}, Config, State) ->
    ?Log:info("Pod added ~p:", [{Namespace, PodName, PodIp, VxlanNames}]),

    Pods = ?Pod:get(?LabelSelector, Config),

    lists:foreach(fun(VxlanName) ->
        VxlanPods = ?Pod:filter(vxlan, VxlanName, PodName, Pods),
        ?Net:vxlan_add_pod(
            Namespace, PodName, PodIp,
            VxlanName, VxlanPods, Config
        )
    end, VxlanNames),

    State.

handle_pod_deleted(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    vxlan_names := VxlanNames
}, Config, State) ->
    ?Log:info("Pod deleted ~p:", [{Namespace, PodName, PodIp, VxlanNames}]),

    Pods = ?Pod:get(?LabelSelector, Config),

    lists:foreach(fun(VxlanName) ->
        VxlanPods = ?Pod:filter(vxlan, VxlanName, PodName, Pods),
        ?Net:vxlan_delete_pod(
            Namespace, PodName, PodIp, VxlanName, VxlanPods, Config
        )
    end, VxlanNames),

    State.
