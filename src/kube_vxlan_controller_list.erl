-module(kube_vxlan_controller_list).

-export([
    pods/1, pods/2
]).

-define(Db, kube_vxlan_controller_db).
-define(Net, kube_vxlan_controller_net).
-define(Pod, kube_vxlan_controller_pod).
-define(Tools, kube_vxlan_controller_tools).

pods(Config) -> pods([], Config).

pods(PodNamePrefixes, Config) ->
    SilentConfig = maps:put(silent, true, Config),

    {ok, PodResources} = ?Pod:get({label, maps:get(selector, Config)}, Config),
    Pods = ?Tools:pods(PodResources, ?Db:nets_options(Config), [], Config),

    [io:format("~s/~s ~s ~s~n", [Namespace, PodName, NetName, Ip]) ||
     Pod = #{namespace := Namespace, name := PodName} <- Pods,
     PodNamePrefixes == [] orelse lists:any(
        fun(Prefix) -> lists:prefix(Prefix, PodName) end,
        PodNamePrefixes
     ),
     {NetName, _NetOptions} <- maps:get(nets, Pod),
     Ip <- ips(Pod, NetName, SilentConfig)].

ips(Pod, NetName, Config) ->
    case ?Net:ips(Pod, NetName, Config) of
        [] -> [""];
        Ips -> Ips
    end.
