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

-define(Fields, ["pod", "net", "fdb", "dev", "route"]).

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
        CommandRoute = ?Net:cmd("ip route show dev ~s", [name], Pod, NetName),

        FormatFun = fun(Field) ->
            [$ |Field] ++ ": ~s~n"
        end,
        ArgFun = fun
            ("pod") -> ?Format:pod(Pod);
            ("net") -> ?Format:pod_net_options(PodNetOptions);
            ("fdb") ->
                Fdb = ?Agent:exec(Pod, CommandFdb, Config),
                string:trim(?Format:fdb(Fdb, 6));
            ("dev") ->
                Dev = ?Agent:exec(Pod, CommandDev, Config),
                string:trim(?Format:dev(Dev, 6));
            ("route") ->
                Route = ?Agent:exec(Pod, CommandRoute, Config),
                string:trim(?Format:route(Route, 8))
        end,

        Fields = read_fields(maps:get(fields, Config)),
        Format = lists:flatten(lists:map(FormatFun, Fields)) ++ "~n",
        Args = lists:map(ArgFun, Fields),

        io:format(Format, Args)
    end.

read_fields("all") -> ?Fields;
read_fields(Fields) ->
    [Field || Field <- string:lexemes(Fields, ","),
     lists:member(Field, ?Fields)].
