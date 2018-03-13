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

    vxlan_id/4,

    vxlan_up/4,
    vxlan_down/4
]).

-define(Agent, kube_vxlan_controller_agent).

vxlan_init_pod(Namespace, PodName, VxlanName, VxlanId, Config) ->
    vxlan_add(Namespace, PodName, VxlanName, VxlanId, Config),
    vxlan_up(Namespace, PodName, VxlanName, Config).

vxlan_add_pod(Namespace, PodName, PodIp, VxlanName, VxlanPods, Config) ->
    lists:foreach(fun({VxlanPodNamespace, VxlanPodName, VxlanPodIp}) ->
        bridge_append(VxlanPodNamespace, VxlanPodName, VxlanName, PodIp, Config),
        bridge_append(Namespace, PodName, VxlanName, VxlanPodIp, Config)
    end, VxlanPods).

vxlan_delete_pod(_Namespace, _PodName, PodIp, VxlanName, VxlanPods, Config) ->
    lists:foreach(fun({VxlanPodNamespace, VxlanPodName, _VxlanPodIp}) ->
        bridge_delete(VxlanPodNamespace, VxlanPodName, VxlanName, PodIp, Config)
    end, VxlanPods).

vxlan_add(Namespace, PodName, VxlanName, VxlanId, Config) ->
    Command = "ip link add " ++ VxlanName ++ " " ++
              "type vxlan id " ++ VxlanId ++ " dev eth0 dstport 0",
    ?Agent:exec(Namespace, PodName, Command, Config).

vxlan_delete(Namespace, PodName, VxlanName, Config) ->
    Command = "ip link delete " ++ VxlanName,
    ?Agent:exec(Namespace, PodName, Command, Config).

bridge_append(Namespace, PodName, VxlanName, TargetIp, Config) ->
    BridgeExists =
        bridge_macs(Namespace, PodName, VxlanName, TargetIp, Config) /= [],
    BridgeExists orelse begin
        Command = "bridge fdb append to 00:00:00:00:00:00 " ++
                  "dst " ++ TargetIp ++ " dev " ++ VxlanName,
        ?Agent:exec(Namespace, PodName, Command, Config)
    end.

bridge_delete(Namespace, PodName, VxlanName, TargetIp, Config) ->
    lists:foreach(fun(Mac) ->
        Command = "bridge fdb delete " ++ Mac ++ " " ++
                  "dst " ++ TargetIp ++ " dev " ++ VxlanName,
        ?Agent:exec(Namespace, PodName, Command, Config)
    end, bridge_macs(Namespace, PodName, VxlanName, TargetIp, Config)).

bridge_macs(Namespace, PodName, VxlanName, TargetIp, Config) ->
    Command = "bridge fdb show dev " ++ VxlanName,
    Result = ?Agent:exec(Namespace, PodName, Command, Config),
    [Mac || FdbRecord <- string:lexemes(Result, "\n"),
            [Mac, "dst", Ip|_ ] <- [string:lexemes(FdbRecord, " ")],
            Ip == TargetIp].

vxlan_id(Namespace, PodName, VxlanName, Config) ->
    Command = "ip -d link show " ++ VxlanName,
    Result = ?Agent:exec(Namespace, PodName, Command, Config),

    case string:lexemes(hd(lists:reverse(string:lexemes(Result, "\n"))), " ") of
        ["vxlan", "id", Id|_] -> {ok, Id};
        _Other -> {error, not_found}
    end.

vxlan_up(Namespace, PodName, VxlanName, Config) ->
    Command = "ip link set " ++ VxlanName ++ " up",
    ?Agent:exec(Namespace, PodName, Command, Config).

vxlan_down(Namespace, PodName, VxlanName, Config) ->
    Command = "ip link set " ++ VxlanName ++ " down",
    ?Agent:exec(Namespace, PodName, Command, Config).
