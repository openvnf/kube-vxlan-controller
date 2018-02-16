-module('kube-vxlan-controller').

-export([main/1, run/1]).

-define(K8s, kube_vxlan_controller_k8s_client).
-define(Log, kube_vxlan_controller_log).

-define(Server, "https://api.k8s.nce-01.fra-01.eu.cennso.net").
-define(CaCertFile, "pki/ca.pem").
-define(TokenFile, "pki/token").

-define(WatchApi, "/api/v1/watch/pods?labelSelector=vxlan%3Dtrue").

%{ok, S} = rtmtb_api_usr_core:connect("https://api.k8s.nce-01.fra-01.eu.cennso.net/api/v1/namespaces/aalferov/pods/alpine1-594d4c669f-d27t4/exec?command=ls&command=-l&container=alpine1&container=alpine1&stderr=true&stdout=true", Headers), rtmtb_api_usr_core:recv(S).

-define(A8nVxlanNames, 'vxlan.travelping.com/names').
-define(A8nVxlanNamesSep, ", \n").

-define(AgentSpec, #{
  spec => #{
    template => #{
      spec => #{
        containers => [
          #{name => "kube-vxlan-controller-agent",
            image => "aialferov/kube-vxlan-controller-agent"}
        ]
      }
    }
  }
}).

%curl -X PATCH --cacert pki/ca.pem https://api.k8s.nce-01.fra-01.eu.cennso.net/apis/apps/v1beta2/namespaces/aalferov/deployments/alpine1 -H "Content-Type: application/strategic-merge-patch+json" --header "Authorization: Bearer $(cat pki/token)" -d '{"spec":{"template":{"spec":{"containers":[{"name":"vxlan-controller-agent","image":"alpine"}]}}}}'

main(_) ->
    Config = #{
        server => ?Server,
        token => binary_to_list(element(2, file:read_file(?TokenFile))),
        ca_cert_file => ?CaCertFile
    },
    application:ensure_all_started(?MODULE),
    run(Config).

run(Config) ->
    case ?K8s:http_stream_request(
        "/api/v1/watch/pods",
        [{"labelSelector", "vxlan=true"}],
        Config
    ) of
        {ok, Stream} -> run(Stream, Config);
        {error, Reason} -> ?Log:error(Reason)
    end.

run(Stream, Config) ->
    case ?K8s:http_stream_read(Stream) of
        {ok, done} -> run(Config);
        {ok, Events} ->
            process_events(Events, Config),
            run(Stream, Config);
        {error, Reason} -> ?Log:error(Reason)
    end.

process_events(Events, Config) ->
    lists:foreach(process_event_fun(Config), Events).

process_event_fun(Config) -> fun(Event = #{
    type := Type,
    object := #{
        metadata := #{
            namespace := Namespace,
            uid := Uid,
            name := Name,
            annotations := Annotations
        },
        status := Status
    }
}) ->
    VxlanNames = btl(maps:get(?A8nVxlanNames, Annotations, <<>>)),
    %?Log:info(Event)
    process_event(#{
        event => list_to_atom(string:lowercase(btl(Type))),
        namespace => btl(Namespace),
        uid => btl(Uid),
        name => btl(Name),
        pod_ip => btl(maps:get(podIP, Status, <<>>)),
        vxlan_names => string:lexemes(VxlanNames, ?A8nVxlanNamesSep),
    }, Config)
end.

process_event(Ev = #{namespace := Namespace, name := Name}, Config) ->
    ?Log:info(patch_pod(Namespace, Name, Config)).

patch_pod(Namespace, PodName, Config) ->
    ResourcePod = "/api/v1/namespaces/" ++ Namespace ++
                  "/pods/" ++ PodName,
    {ok, [#{
      metadata := #{
        ownerReferences := [#{
          kind := <<"ReplicaSet">>,
          name := ReplicaSetName
        }|_]
      }
    }]} = ?K8s:http_request(ResourcePod, [], Config),

    ResourceReplicaSet = "/apis/extensions/v1/namespaces/" ++ Namespace ++
                         "/replicasets/" ++ btl(ReplicaSetName),
    {ok, [#{
      metadata := #{
        ownerReferences := [#{
          kind := <<"Deployment">>,
          name := DeploymentName
        }|_]
      }
    }]} = ?K8s:http_request(ResourceReplicaSet, [], Config),
    ?Log:info(DeploymentName).

btl(B) -> binary_to_list(B).
