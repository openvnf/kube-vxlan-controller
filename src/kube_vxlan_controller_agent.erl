-module(kube_vxlan_controller_agent).
-export([embed/3]).

-define(K8s, kube_vxlan_controller_k8s_client).
-define(Log, kube_vxlan_controller_log).

-define(AgentContainerName, <<"vxlan-controller-agent">>).
-define(AgentImage, <<"aialferov/kube-vxlan-controller-agent">>).

-define(AgentSpec, #{
  spec => #{
    template => #{
      spec => #{
        containers => [
          #{name => ?AgentContainerName,
            image => ?AgentImage}
        ]}}}
}).

embed(Namespace, DeploymentName, Config) ->
    ?Log:info("Embedding agent into \"~s\" deployment", [DeploymentName]),

    Resource = "/apis/apps/v1beta2/namespaces/" ++ Namespace ++
               "/deployments/" ++ binary_to_list(DeploymentName),
    Headers = [
        {"Content-Type", <<"application/strategic-merge-patch+json">>}
    ],
    Body = jsx:encode(?AgentSpec),

    {ok, Data} = ?K8s:http_request(patch, Resource, [], Headers, Body, Config),
    ?Log:info(Data),
    ok.

%get_deployment_name(Namespace, ReplicaSetName) ->
%   ResourceReplicaSet = "/apis/extensions/v1beta1/namespaces/" ++ Namespace ++
%                        "/replicasets/" ++ binary_to_list(ReplicaSetName),
%   {ok, [#{
%     metadata := #{
%       ownerReferences := [#{
%         kind := <<"Deployment">>,
%         name := DeploymentName
%       }|_]
%     }
%   }]} = ?K8s:http_request(ResourceReplicaSet, [], Config),
%   DeploymentName.
