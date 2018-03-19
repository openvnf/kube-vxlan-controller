-module(kube_vxlan_controller_net).

-export([
    pod_init/4,
    pod_add/6,
    pod_delete/6,

    link_add/4,
    link_delete/4,
    link_up/4,
    link_down/4,

    bridge_append/5,
    bridge_delete/5,

    bridge_macs/5,

    vxlan_id/4
]).

-define(Agent, kube_vxlan_controller_agent).

pod_init(Namespace, PodName, Net, Config) ->
    link_add(Namespace, PodName, Net, Config),
    link_up(Namespace, PodName, maps:get(name, Net), Config).

pod_add(Namespace, PodName, PodIp, NetName, NetPods, Config) ->
    lists:foreach(fun({NetPodNamespace, NetPodName, NetPodIp}) ->
        bridge_append(NetPodNamespace, NetPodName, NetName, PodIp, Config),
        bridge_append(Namespace, PodName, NetName, NetPodIp, Config)
    end, NetPods).

pod_delete(Namespace, PodName, PodIp, NetName, NetPods, Config) ->
    lists:foreach(fun({NetPodNamespace, NetPodName, NetPodIp}) ->
        bridge_delete(Namespace, PodName, NetName, NetPodIp, Config),
        bridge_delete(NetPodNamespace, NetPodName, NetName, PodIp, Config)
    end, NetPods).

link_add(Namespace, PodName, Net, Config) ->
    Command = "ip link add " ++ maps:get(name, Net) ++
              " type " ++ maps:get(type, Net) ++
              " id " ++ maps:get(id, Net) ++
              " dev " ++ maps:get(dev, Net) ++
              " dstport 0",

    ?Agent:exec(Namespace, PodName, Command, Config).

link_delete(Namespace, PodName, NetName, Config) ->
    Command = "ip link delete " ++ NetName,
    ?Agent:exec(Namespace, PodName, Command, Config).

link_up(Namespace, PodName, NetName, Config) ->
    Command = "ip link set " ++ NetName ++ " up",
    ?Agent:exec(Namespace, PodName, Command, Config).

link_down(Namespace, PodName, NetName, Config) ->
    Command = "ip link set " ++ NetName ++ " down",
    ?Agent:exec(Namespace, PodName, Command, Config).

bridge_append(Namespace, PodName, NetName, TargetIp, Config) ->
    BridgeExists =
        bridge_macs(Namespace, PodName, NetName, TargetIp, Config) /= [],
    BridgeExists orelse begin
        Command = "bridge fdb append to 00:00:00:00:00:00 " ++
                  "dst " ++ TargetIp ++ " dev " ++ NetName,
        ?Agent:exec(Namespace, PodName, Command, Config)
    end.

bridge_delete(Namespace, PodName, NetName, TargetIp, Config) ->
    lists:foreach(fun(Mac) ->
        Command = "bridge fdb delete " ++ Mac ++ " " ++
                  "dst " ++ TargetIp ++ " dev " ++ NetName,
        ?Agent:exec(Namespace, PodName, Command, Config)
    end, bridge_macs(Namespace, PodName, NetName, TargetIp, Config)).

bridge_macs(Namespace, PodName, NetName, TargetIp, Config) ->
    Command = "bridge fdb show dev " ++ NetName,
    Result = ?Agent:exec(Namespace, PodName, Command, Config),
    [Mac || FdbRecord <- string:lexemes(Result, "\n"),
            [Mac, "dst", Ip|_ ] <- [string:lexemes(FdbRecord, " ")],
            Ip == TargetIp].

vxlan_id(Namespace, PodName, NetName, Config) ->
    Command = "ip -d link show " ++ NetName,
    Result = ?Agent:exec(Namespace, PodName, Command, Config),

    case string:lexemes(hd(lists:reverse(string:lexemes(Result, "\n"))), " ") of
        ["vxlan", "id", Id|_] -> {ok, Id};
        _Other -> {error, not_found}
    end.
