-module('kube-vxlan-controller').

-export([main/1, run/2, config/0]).

-define(K8s, kube_vxlan_controller_k8s_client).
-define(Pod, kube_vxlan_controller_pod).
-define(Log, kube_vxlan_controller_log).

-define(Server, "https://api.k8s.nce-01.fra-01.eu.cennso.net").
-define(CaCertFile, "pki/ca.pem").
-define(TokenFile, "pki/token").

-define(AgentContainerName, "vxlan-controller-agent").

main(_) ->
    application:ensure_all_started(?MODULE),
    run(config(), #{}).

config() -> #{
    server => ?Server,
    token => binary_to_list(element(2, file:read_file(?TokenFile))),
    ca_cert_file => ?CaCertFile,
    agent_container_name => ?AgentContainerName
}.

run(Config, State) ->
    ?Log:info("Resource version: ~s", [resource_version(State)]),
    Resource = "/api/v1/watch/pods",
    Query = [
        {"labelSelector", "vxlan=true"},
        {"resourceVersion", resource_version(State)},
        {"timeoutSeconds", "30"}
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
    NormalizedEvent = normalize_event(Event),
    NewState = set_resource_version(NormalizedEvent, State),
    process_event(NormalizedEvent, Config, NewState)
end.

process_event(Event, _Config, State) ->
    ?Log:info(Event),
    set_resource_version(Event, State).

normalize_event(#{
  type := Type,
    object := #{
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
}) -> #{
    event => list_to_atom(string:lowercase(binary_to_list(Type))),
    resource_version => binary_to_list(ResourceVersion),
    namespace => binary_to_list(Namespace),
    pod_uid => binary_to_list(PodUid),
    pod_name => binary_to_list(PodName),
    pod_ip => binary_to_list(maps:get(podIP, Status, <<>>)),
    vxlan_names => ?Pod:vxlan_names(Annotations),
    phase => binary_to_list(Phase)
}.

handle_pod_added(Namespace, PodName, PodIp, VxlanNames, _Config) ->
    ?Log:info("Pod added ~p:", [{Namespace, PodName, PodIp, VxlanNames}]).

handle_pod_deleted(Namespace, PodName, PodIp, VxlanNames, _Config) ->
    ?Log:info("Pod deleted ~p:", [{Namespace, PodName, PodIp, VxlanNames}]).

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
