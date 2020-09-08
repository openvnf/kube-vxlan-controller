-module(kube_vxlan_controller_agent).

-export([terminate/2, exec/3]).
-export([start_link/1, init/2]).

-define(Sup, kube_vxlan_controller_agent_sup).
-define(Pod, kube_vxlan_controller_pod).

terminate(Pod, Config) ->
    ?Sup:exec({terminate, Pod, Config}).

exec(Pod, Command, Config) ->
    ?Sup:exec({exec, Pod, Command, Config}).

%%%=======================================================

start_link(Action) ->
    proc_lib:start_link(?MODULE, init, [self(), Action]).

init(Parent, {terminate, Pod, Config}) ->
    proc_lib:init_ack(Parent, {ok, self()}),

    %%% TODO: provided for BC, remove once not needed
    run(Pod, "touch /run/terminate", Config),
    %%%
    run(Pod, "kill -TERM 1", Config);

init(Parent, {exec, Pod, Command, Config}) ->
    proc_lib:init_ack(Parent, {ok, self()}),

    run(Pod, Command, Config).

run(#{namespace := Namespace, name := PodName}, Command, Config) ->
    ContainerName = maps:get(agent_container_name, Config),
    ?Pod:exec(Namespace, PodName, ContainerName, Command, Config).

%-define(AgentContainerName, <<"vxlan-controller-agent">>).
%-define(AgentImage, <<"aialferov/kube-vxlan-controller-agent">>).

%-define(AgentSpec, #{
%  spec => #{
%    template => #{
%      spec => #{
%        containers => [
%          #{name => ?AgentContainerName,
%            image => ?AgentImage}
%        ]}}}
%}).
%
%embed(Namespace, DeploymentName, Config) ->
%    ?LOG(info, "Embedding agent into \"~s\" deployment", [DeploymentName]),
%
%    Resource = "/apis/apps/v1beta2/namespaces/" ++ Namespace ++
%               "/deployments/" ++ binary_to_list(DeploymentName),
%    Headers = [
%        {"Content-Type", <<"application/strategic-merge-patch+json">>}
%    ],
%    Body = jsx:encode(?AgentSpec),
%
%    {ok, Data} = ?K8s:http_request(patch, Resource, [], Headers, Body, Config),
%    ?LOG(info, Data),
%    ok.
%
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
