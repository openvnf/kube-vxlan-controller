-module(kube_vxlan_controller_net).

-export([
    vxlan_init_pod/5,
    vxlan_add_pod/6,
    vxlan_delete_pod/6,

    vxlan_add/5,
    vxlan_delete/4,

    bridge_append/5,
    bridge_delete/5,

    bridge_macs/5,

    vxlan_ids/1,
    vxlan_id/4,

    vxlan_up/4,
    vxlan_down/4,
    vxlan_restart/4
]).

-define(K8s, kube_vxlan_controller_k8s).
-define(Pod, kube_vxlan_controller_pod).
-define(Utils, kube_vxlan_controller_utils).
-define(Log, kube_vxlan_controller_log).

vxlan_init_pod(Namespace, PodName, VxlanName, VxlanId, Config) ->
    vxlan_add(Namespace, PodName, VxlanName, VxlanId, Config),
    vxlan_up(Namespace, PodName, VxlanName, Config).

vxlan_add_pod(Namespace, PodName, PodIp, VxlanName, VxlanPods, Config) ->
    lists:foreach(fun({VxlanPodName, VxlanPodIp}) ->
        bridge_append(Namespace, VxlanPodName, VxlanName, PodIp, Config),
        bridge_append(Namespace, PodName, VxlanName, VxlanPodIp, Config)
    end, VxlanPods).

vxlan_delete_pod(Namespace, _PodName, PodIp, VxlanName, VxlanPods, Config) ->
    lists:foreach(fun({VxlanPodName, _VxlanPodIp}) ->
        bridge_delete(Namespace, VxlanPodName, VxlanName, PodIp, Config)
    end, VxlanPods).

vxlan_add(Namespace, PodName, VxlanName, VxlanId, Config) ->
    Command = "ip link add " ++ VxlanName ++ " " ++
              "type vxlan id " ++ VxlanId ++ " dev eth0 dstport 0",
    pod_exec(Namespace, PodName, Command, Config).

vxlan_delete(Namespace, PodName, VxlanName, Config) ->
    Command = "ip link delete " ++ VxlanName,
    pod_exec(Namespace, PodName, Command, Config).

bridge_append(Namespace, PodName, VxlanName, TargetIp, Config) ->
    BridgeExists =
        bridge_macs(Namespace, PodName, VxlanName, TargetIp, Config) /= [],
    BridgeExists orelse begin
        Command = "bridge fdb append to 00:00:00:00:00:00 " ++
                  "dst " ++ TargetIp ++ " dev " ++ VxlanName,
        pod_exec(Namespace, PodName, Command, Config)
    end.

bridge_delete(Namespace, PodName, VxlanName, TargetIp, Config) ->
    lists:foreach(fun(Mac) ->
        Command = "bridge fdb delete " ++ Mac ++ " " ++
                  "dst " ++ TargetIp ++ " dev " ++ VxlanName,
        pod_exec(Namespace, PodName, Command, Config)
    end, bridge_macs(Namespace, PodName, VxlanName, TargetIp, Config)).

bridge_macs(Namespace, PodName, VxlanName, TargetIp, Config) ->
    Command = "bridge fdb show dev " ++ VxlanName,
    Result = pod_exec(Namespace, PodName, Command, Config),
    [Mac || FdbRecord <- string:lexemes(Result, "\n"),
            [Mac, "dst", Ip|_ ] <- [string:lexemes(FdbRecord, " ")],
            Ip == TargetIp].

vxlan_ids(Config = #{namespace := Namespace, vxlan_config_name := Name}) ->
    Resource = "/api/v1/namespaces/" ++ Namespace ++ "/configmaps/" ++ Name,
    {ok, [#{data := Data}]} = ?K8s:http_request(Resource, [], Config),

    maps:fold(fun(K, V, Map) ->
        maps:put(atom_to_list(K), binary_to_list(V), Map)
    end, #{}, Data).

vxlan_id(Namespace, PodName, VxlanName, Config) ->
    Command = "ip -d link show " ++ VxlanName,
    Result = pod_exec(Namespace, PodName, Command, Config),

    case string:lexemes(hd(lists:reverse(string:lexemes(Result, "\n"))), " ") of
        ["vxlan", "id", Id|_] -> {ok, Id};
        _Other -> {error, not_found}
    end.

vxlan_up(Namespace, PodName, VxlanName, Config) ->
    Command = "ip link set " ++ VxlanName ++ " up",
    pod_exec(Namespace, PodName, Command, Config).

vxlan_down(Namespace, PodName, VxlanName, Config) ->
    Command = "ip link set " ++ VxlanName ++ " down",
    pod_exec(Namespace, PodName, Command, Config).

vxlan_restart(Namespace, PodName, VxlanName, Config) ->
    vxlan_down(Namespace, PodName, VxlanName, Config),
    vxlan_up(Namespace, PodName, VxlanName, Config).

pod_exec(Namespace, PodName, Command, Config) ->
    AgentContainerName = maps:get(agent_container_name, Config),
    ?Pod:exec(Namespace, PodName, AgentContainerName, Command, Config).
