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
-define(Utils, kube_vxlan_controller_utils).
-define(Log, kube_vxlan_controller_log).

-define(AgentContainerName, "vxlan-controller-agent").

-define(AgentExecQuery, [
    {"container", ?AgentContainerName},
    {"stdout", "true"},
    {"stderr", "true"}
]).

vxlan_add(Namespace, PodName, VxlanName, VxlanId, Config) ->
    Command = "ip link add " ++ VxlanName ++ " " ++
              "type vxlan id " ++ VxlanId ++ " dev eth0 dstport 0",
    pod_exec(Namespace, PodName, Command, Config).

vxlan_delete(Namespace, PodName, VxlanName, Config) ->
    Command = "ip link delete " ++ VxlanName,
    pod_exec(Namespace, PodName, Command, Config).

bridge_append(Namespace, PodName, VxlanName, BridgeToIp, Config) ->
    Command = "bridge fdb append to 00:00:00:00:00:00 " ++
              "dst " ++ BridgeToIp ++ " dev " ++ VxlanName,
    pod_exec(Namespace, PodName, Command, Config).

bridge_delete(Namespace, PodName, VxlanName, BridgeToIp, Config) ->
    case bridge_mac(Namespace, PodName, BridgeToIp, VxlanName, Config) of
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
        _Other -> {error, not_found}
    end
end.

vxlan_id(Namespace, PodName, VxlanName, Config) ->
    Command = "ip -d link show " ++ VxlanName,
    Result = pod_exec(Namespace, PodName, Command, Config),

    case string:lexemes(hd(lists:reverse(string:lexemes(Result, "\n"))), " ") of
        ["vxlan", "id", Id|_] -> {ok, Id};
        _Other -> {error, not_found}
    end.
    
pod_exec(Namespace, PodName, Command, Config) ->
    ?Log:info("~s/~s: ~s", [Namespace, PodName, Command]),

    Resource = "/api/v1/namespaces/" ++ Namespace ++
               "/pods/" ++ PodName ++ "/exec",

    Query = [{"command", CommandItem} || CommandItem <-
             string:split(Command, " ", all)] ++ ?AgentExecQuery,

    {ok, Socket} = ?K8s:ws_connect(Resource, Query, Config),
    {ok, Result} = ?K8s:ws_recv(Socket),
    ?K8s:ws_disconnect(Socket),

    ?Log:info(Result),
    Result.
