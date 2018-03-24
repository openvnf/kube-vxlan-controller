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
            Args = [Namespace, PodName, PodIp, network_format(PodNet)],
            io:format(" pod: ~s/~s ~s~n net: ~s~n fdb: ", Args),
            Command = "bridge fdb show dev " ++ maps:get(name, PodNet),
            Fdb = ?Agent:exec(Namespace, PodName, Command, SilentConfig),
            io:format("~s~n~n", [fdb_format(Fdb)])
        end, ?Tools:network_members(NetName, "", Pods, NameIdMap, Config))
    end, NetNames).

network_format(Net) ->
    string:trim(maps:fold(fun(Key, Value, Acc) ->
        Acc ++ fmt("~s:~s ", [Key, Value])
    end, "", Net)).

fdb_format(Fdb) ->
    lists:join("\n      ", string:lexemes(Fdb, "\n")).

fmt(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
