-module(kube_vxlan_controller_net).

-export([
    vxlan_add/5,
    vxlan_delete/4,

    bridge_append/5,
    bridge_delete/5,

    bridge_mac/5,
    vxlan_id/4
]).

-define(K8s, kube_vxlan_controller_k8s_client).
-define(Pod, kube_vxlan_controller_pod).
-define(Utils, kube_vxlan_controller_utils).
-define(Log, kube_vxlan_controller_log).

-define(AgentContainerName, "vxlan-controller-agent").

vxlan_add(Namespace, PodName, VxlanName, VxlanId, Config) ->
    Command = "ip link add " ++ VxlanName ++ " " ++
              "type vxlan id " ++ VxlanId ++ " dev eth0 dstport 0",
    ?Pod:exec(Namespace, PodName, ?AgentContainerName, Command, Config).

vxlan_delete(Namespace, PodName, VxlanName, Config) ->
    Command = "ip link delete " ++ VxlanName,
    ?Pod:exec(Namespace, PodName, ?AgentContainerName, Command, Config).

bridge_append(Namespace, PodName, VxlanName, BridgeToIp, Config) ->
    case bridge_mac(Namespace, PodName, VxlanName, BridgeToIp, Config) of
        {ok, _Mac} -> ok;
        false ->
            Command = "bridge fdb append to 00:00:00:00:00:00 " ++
                      "dst " ++ BridgeToIp ++ " dev " ++ VxlanName,
            ?Pod:exec(Namespace, PodName, ?AgentContainerName, Command, Config)
    end.

bridge_delete(Namespace, PodName, VxlanName, BridgeToIp, Config) ->
    case bridge_mac(Namespace, PodName, VxlanName, BridgeToIp, Config) of
        {ok, Mac} ->
            Command = "bridge fdb delete " ++ Mac ++ " " ++
                      "dst " ++ BridgeToIp ++ " dev " ++ VxlanName,
            ?Pod:exec(Namespace, PodName, ?AgentContainerName, Command, Config);
        false -> ok
    end.

bridge_mac(Namespace, PodName, VxlanName, BridgeToIp, Config) ->
    Command = "bridge fdb show dev " ++ VxlanName,
    Result = ?Pod:exec(Namespace, PodName, ?AgentContainerName, Command, Config),
    ?Utils:foldl_while(bridge_mac_fun(BridgeToIp), string:lexemes(Result, "\n")).

bridge_mac_fun(BridgeToIp) -> fun(FdbItem) ->
    case string:lexemes(FdbItem, " ") of
        [Mac, "dst", BridgeToIp|_] -> {ok, Mac};
        _Other -> false
    end
end.

vxlan_id(Namespace, PodName, VxlanName, Config) ->
    Command = "ip -d link show " ++ VxlanName,
    Result = ?Pod:exec(Namespace, PodName, ?AgentContainerName, Command, Config),

    case string:lexemes(hd(lists:reverse(string:lexemes(Result, "\n"))), " ") of
        ["vxlan", "id", Id|_] -> {ok, Id};
        _Other -> {error, not_found}
    end.
