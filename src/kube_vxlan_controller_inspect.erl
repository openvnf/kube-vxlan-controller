-module(kube_vxlan_controller_inspect).

-export([
    networks/2
]).

-define(Db, kube_vxlan_controller_db).
-define(Pod, kube_vxlan_controller_pod).
-define(Agent, kube_vxlan_controller_agent).
-define(Tools, kube_vxlan_controller_tools).

networks(NetNames, Config) ->
    {ok, Pods} = ?Pod:get({label, maps:get(selector, Config)}, Config),
    NameIdMap = ?Db:networks_name_id_map(Config),

    SilentConfig = maps:put(silent, true, Config),

    lists:foreach(fun(NetName) ->
        io:format("[~s]~n", [NetName]),
        lists:foreach(fun({Namespace, PodName, PodIp, PodNet}) ->
            io:format(" pod: ~s/~s ~s~n", [Namespace, PodName, PodIp]),
            io:format(" net: ~s~n", [network_format(PodNet)]),

            CommandDev = "ip -d addr show dev " ++ maps:get(name, PodNet),
            Dev = ?Agent:exec(Namespace, PodName, CommandDev, SilentConfig),
            io:format(" dev: ~s~n", [dev_format(Dev)]),

            CommandFdb = "bridge fdb show dev " ++ maps:get(name, PodNet),
            Fdb = ?Agent:exec(Namespace, PodName, CommandFdb, SilentConfig),
            io:format(" fdb: ~s~n", [fdb_format(Fdb)]),

            io:format("~n")
        end, ?Tools:network_members(NetName, "", Pods, NameIdMap, Config))
    end, NetNames).

network_format(Net) ->
    string:trim(maps:fold(fun(Key, Value, Acc) ->
        Acc ++ fmt("~s:~s ", [Key, Value])
    end, "", Net)).

dev_format(Fdb) ->
    lists:join("\n      ", [L || [_,_,_|L] <- string:lexemes(Fdb, "\n")]).

fdb_format(Fdb) ->
    lists:join("\n      ", string:lexemes(Fdb, "\n")).

fmt(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
