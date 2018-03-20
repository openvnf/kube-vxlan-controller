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
    link_up(Namespace, PodName, Net, Config).

pod_add(Namespace, PodName, PodIp, Net, NetPods, Config) ->
    lists:foreach(fun({NetPodNamespace, NetPodName, NetPodIp, NetPodNet}) ->
        bridge_append(NetPodNamespace, NetPodName, NetPodNet, PodIp, Config),
        bridge_append(Namespace, PodName, Net, NetPodIp, Config)
    end, NetPods).

pod_delete(Namespace, PodName, PodIp, Net, NetPods, Config) ->
    lists:foreach(fun({NetPodNamespace, NetPodName, NetPodIp, NetPodNet}) ->
        bridge_delete(Namespace, PodName, Net, NetPodIp, Config),
        bridge_delete(NetPodNamespace, NetPodName, NetPodNet, PodIp, Config)
    end, NetPods).

link_add(Namespace, PodName, Net, Config) ->
    Command = "ip link add " ++ maps:get(name, Net) ++
              " type " ++ maps:get(type, Net) ++
              " id " ++ maps:get(id, Net) ++
              " dev " ++ maps:get(dev, Net) ++
              " dstport 0",

    ?Agent:exec(Namespace, PodName, Command, Config).

link_delete(Namespace, PodName, Net, Config) ->
    Command = "ip link delete " ++ maps:get(name, Net),
    ?Agent:exec(Namespace, PodName, Command, Config).

link_up(Namespace, PodName, Net, Config) ->
    Command = "ip link set " ++ maps:get(name, Net) ++ " up",
    ?Agent:exec(Namespace, PodName, Command, Config).

link_down(Namespace, PodName, Net, Config) ->
    Command = "ip link set " ++ maps:get(name, Net) ++ " down",
    ?Agent:exec(Namespace, PodName, Command, Config).

bridge_append(Namespace, PodName, Net, TargetIp, Config) ->
    BridgeExists = bridge_macs(Namespace, PodName, Net, TargetIp, Config) /= [],
    BridgeExists orelse begin
        Command = "bridge fdb append to 00:00:00:00:00:00" ++
                  " dst " ++ TargetIp ++
                  " dev " ++ maps:get(name, Net),
        ?Agent:exec(Namespace, PodName, Command, Config)
    end.

bridge_delete(Namespace, PodName, Net, TargetIp, Config) ->
    lists:foreach(fun(Mac) ->
        Command = "bridge fdb delete " ++ Mac ++
                  " dst " ++ TargetIp ++
                  " dev " ++ maps:get(name, Net),
        ?Agent:exec(Namespace, PodName, Command, Config)
    end, bridge_macs(Namespace, PodName, Net, TargetIp, Config)).

bridge_macs(Namespace, PodName, Net, TargetIp, Config) ->
    Command = "bridge fdb show dev " ++ maps:get(name, Net),
    Result = ?Agent:exec(Namespace, PodName, Command, Config),
    [Mac || FdbRecord <- string:lexemes(Result, "\n"),
            [Mac, "dst", Ip|_ ] <- [string:lexemes(FdbRecord, " ")],
            Ip == TargetIp].

vxlan_id(Namespace, PodName, Net, Config) ->
    Command = "ip -d link show " ++ maps:get(name, Net),
    Result = ?Agent:exec(Namespace, PodName, Command, Config),

    case string:lexemes(hd(lists:reverse(string:lexemes(Result, "\n"))), " ") of
        ["vxlan", "id", Id|_] -> {ok, Id};
        _Other -> {error, not_found}
    end.
