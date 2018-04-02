-module(kube_vxlan_controller_inspect).

-export([
    networks/2,
    nets/2
]).

-define(Db, kube_vxlan_controller_db).
-define(Net, kube_vxlan_controller_net).
-define(Pod, kube_vxlan_controller_pod).
-define(Agent, kube_vxlan_controller_agent).
-define(Tools, kube_vxlan_controller_tools).
-define(Format, kube_vxlan_controller_format).

networks(NetNames, Config) -> nets(NetNames, Config).

nets(NetNames, Config) ->
    SilentConfig = maps:put(silent, true, Config),

    {ok, PodResources} = ?Pod:get({label, maps:get(selector, Config)}, Config),
    Pods = ?Tools:pods(PodResources, ?Db:nets_options(Config),
                       [{with_nets, NetNames}], Config),
    lists:foreach(fun(NetName) ->
        io:format("[~s]~n", [NetName]),
        lists:foreach(fun(Pod) ->
            show_pod(Pod, NetName, SilentConfig)
        end, Pods)
    end, NetNames).

show_pod(Pod = #{nets := Nets}, NetName, Config) ->
    PodNetOptions = proplists:get_value(NetName, Nets, false),
    is_map(PodNetOptions) andalso begin
        CommandFdb = ?Net:cmd("bridge fdb show dev ~s", [name], Pod, NetName),
        CommandDev = ?Net:cmd("ip -d addr show dev ~s", [name], Pod, NetName),

        Fdb = ?Agent:exec(Pod, CommandFdb, Config),
        Dev = ?Agent:exec(Pod, CommandDev, Config),

        Ident = 6,
        io:format(
            " pod: ~s~n"
            " net: ~s~n"
            " fdb: ~s~n"
            " dev: ~s~n"
            "~n", [
            ?Format:pod(Pod),
            ?Format:pod_net_options(PodNetOptions),
            string:trim(?Format:fdb(Fdb, Ident)),
            string:trim(?Format:dev(Dev, Ident))
        ])
    end.
