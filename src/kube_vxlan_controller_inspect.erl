-module(kube_vxlan_controller_inspect).

-export([
    networks/2,
    nets/2
]).

-define(Db, kube_vxlan_controller_db).
-define(Pod, kube_vxlan_controller_pod).
-define(Agent, kube_vxlan_controller_agent).
-define(Tools, kube_vxlan_controller_tools).

networks(NetNames, Config) -> nets(NetNames, Config).

nets(NetNames, Config) ->
    {ok, Pods} = ?Pod:get({label, maps:get(selector, Config)}, Config),
    GlobalNetsOptions = ?Db:nets_options(Config),

    SilentConfig = maps:put(silent, true, Config),

    lists:foreach(fun(NetName) ->
        io:format("[~s]~n", [NetName]),
        lists:foreach(
            show_net_member_fun(SilentConfig),
            ?Tools:net_members(NetName, "", Pods, GlobalNetsOptions, Config)
        )
    end, NetNames).

show_net_member_fun(Config) -> fun({Namespace, PodName, PodIp, PodNet}) ->
    io:format(" pod: ~s/~s ~s~n", [Namespace, PodName, PodIp]),
    io:format(" net: ~s~n", [net_options_format(PodNet)]),

    CommandDev = "ip -d addr show dev " ++ net_option(name, PodNet),
    Dev = ?Agent:exec(Namespace, PodName, CommandDev, Config),
    io:format(" dev: ~s~n", [dev_format(Dev)]),

    CommandFdb = "bridge fdb show dev " ++ net_option(name, PodNet),
    Fdb = ?Agent:exec(Namespace, PodName, CommandFdb, Config),
    io:format(" fdb: ~s~n", [fdb_format(Fdb)]),

    io:format("~n")
end.

net_option(OptionName, {_NetName, NetOptions}) ->
    maps:get(OptionName, NetOptions).

net_options_format({_NetName, NetOptions}) ->
    string:trim(maps:fold(fun(OptionName, OptionValue, Acc) ->
        Acc ++ format("~s:~s ", [OptionName, OptionValue])
    end, "", NetOptions)).

dev_format(Fdb) ->
    lists:join("\n      ", [L || [_,_,_|L] <- string:lexemes(Fdb, "\n")]).

fdb_format(Fdb) ->
    lists:join("\n      ", string:lexemes(Fdb, "\n")).

format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
