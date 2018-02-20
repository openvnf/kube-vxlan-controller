-module(kube_vxlan_controller_net).

-export([
    config/1,

    vxlan_add/5,
    vxlan_delete/4,

    bridge_append/5,
    bridge_delete/5,

    bridge_mac/5,
    vxlan_id/4,

    vxlan_up/4,
    vxlan_down/4,
    vxlan_restart/4
]).

-define(K8s, kube_vxlan_controller_k8s_client).
-define(Pod, kube_vxlan_controller_pod).
-define(Utils, kube_vxlan_controller_utils).
-define(Log, kube_vxlan_controller_log).

config(Config = #{namespace := Namespace, vxlan_config_name := Name}) ->
    Resource = "/api/v1/namespaces/" ++ Namespace ++ "/configmaps/" ++ Name,
    {ok, [#{data := Data}]} = ?K8s:http_request(Resource, [], Config),
    maps:fold(fun(K, V, Map) ->
        maps:put(atom_to_list(K), binary_to_list(V), Map)
    end, #{}, Data).

vxlan_add(Namespace, PodName, VxlanName, VxlanId, Config) ->
    Command = "ip link add " ++ VxlanName ++ " " ++
              "type vxlan id " ++ VxlanId ++ " dev eth0 dstport 0",
    pod_exec(Namespace, PodName, Command, Config).

vxlan_delete(Namespace, PodName, VxlanName, Config) ->
    Command = "ip link delete " ++ VxlanName,
    pod_exec(Namespace, PodName, Command, Config).

bridge_append(Namespace, PodName, VxlanName, BridgeToIp, Config) ->
    case bridge_mac(Namespace, PodName, VxlanName, BridgeToIp, Config) of
        {ok, _Mac} -> ok;
        false ->
            Command = "bridge fdb append to 00:00:00:00:00:00 " ++
                      "dst " ++ BridgeToIp ++ " dev " ++ VxlanName,
            pod_exec(Namespace, PodName, Command, Config)
    end.

bridge_delete(Namespace, PodName, VxlanName, BridgeToIp, Config) ->
    case bridge_mac(Namespace, PodName, VxlanName, BridgeToIp, Config) of
        {ok, Mac} ->
            Command = "bridge fdb delete " ++ Mac ++ " " ++
                      "dst " ++ BridgeToIp ++ " dev " ++ VxlanName,
            pod_exec(Namespace, PodName, Command, Config);
        false -> ok
    end.

bridge_mac(Namespace, PodName, VxlanName, BridgeToIp, Config) ->
    Command = "bridge fdb show dev " ++ VxlanName,
    Result = pod_exec(Namespace, PodName, Command, Config),
    ?Utils:foldl_while(bridge_mac_fun(BridgeToIp), string:lexemes(Result, "\n")).

bridge_mac_fun(BridgeToIp) -> fun(FdbItem) ->
    case string:lexemes(FdbItem, " ") of
        [Mac, "dst", BridgeToIp|_] -> {ok, Mac};
        _Other -> false
    end
end.

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
