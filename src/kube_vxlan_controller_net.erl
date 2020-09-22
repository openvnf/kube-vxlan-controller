-module(kube_vxlan_controller_net).

-export([
    pod_setup/2,
    pod_join/2,
    pod_leave/2,

    links/3, links/4, link/4,
    bridge/5,

    bridge_macs/4,
    vxlan_id/3,
    ips/3,

    common_pod_net_names/2,
    pod_net_names/1,

    cmd/4
]).

-include_lib("kernel/include/logger.hrl").

-define(Pod, kube_vxlan_controller_pod).
-define(Agent, kube_vxlan_controller_agent).

pod_setup(Pod, Config) ->
    links(add, Pod, Config),
    links(ip, Pod, Config),
    links(up, Pod, Config),
    links(route, Pod, Config).

pod_join(#{version := Version} = Pod, NetPods) ->
    lists:foreach(
      fun(NetPod) ->
	      NetNames = common_pod_net_names(Pod, NetPod),
	      ?Pod:bridge_cmd(append, Version, NetPod, NetNames, maps:get(ip, Pod)),
	      ?Pod:bridge_cmd(append, Version, Pod, NetNames, maps:get(ip, NetPod))
      end, NetPods).

pod_leave(#{version := Version} = Pod, NetPods) ->
    lists:foreach(fun(NetPod) ->
	NetNames = common_pod_net_names(Pod, NetPod),
	?Pod:bridge_cmd(delete, Version, Pod, NetNames, maps:get(ip, NetPod)),
	?Pod:bridge_cmd(delete, Version, NetPod, NetNames, maps:get(ip, Pod))
    end, NetPods).

links(Action, Pod, Config) ->
    links(Action, Pod, pod_net_names(Pod), Config).

links(Action, Pod, NetNames, Config) ->
    lists:foreach(fun(NetName) ->
	link(Action, Pod, NetName, Config)
    end, NetNames).

link(add, Pod, NetName, Config) ->
    Command = cmd("ip link add ~s type ~s id ~s dev ~s dstport 0",
		  [name, type, id, dev], Pod, NetName),
    ?Agent:exec(Pod, Command, Config);

link(delete, Pod, NetName, Config) ->
    Command = cmd("ip link delete ~s", [name], Pod, NetName),
    ?Agent:exec(Pod, Command, Config);

link(up, Pod, NetName, Config) ->
    NeedUp = pod_net_option(up, NetName, Pod),
    NeedUp andalso begin
	Command = cmd("ip link set ~s up", [name], Pod, NetName),
	?Agent:exec(Pod, Command, Config)
    end;

link(down, Pod, NetName, Config) ->
    Command = cmd("ip link set ~s down", [name], Pod, NetName),
    ?Agent:exec(Pod, Command, Config);

link(ip, Pod, NetName, Config) ->
    Ip = pod_net_option(ip, NetName, Pod),
    Ip == false orelse begin
	Command = cmd("ip addr add ~s dev ~s", [ip, name], Pod, NetName),
	?Agent:exec(Pod, Command, Config)
    end;

link(route, Pod, NetName, Config) ->
    Route = pod_net_option(route, NetName, Pod),
    Route == false orelse begin
	[Subnet, Gw] = string:split(Route, ":"),
	Command = cmd("ip route add ~s via ~s", [Subnet, Gw], Pod, NetName),
	?Agent:exec(Pod, Command, Config)
    end.

bridge(append, Pod, NetName, TargetIp, Config) ->
    Command = cmd("bridge fdb append to 00:00:00:00:00:00 dst ~s dev ~s",
		  [TargetIp, name], Pod, NetName),
    ?Agent:exec(Pod, Command, Config);

%% we can sunconditional try to add the FDB, attempting to
%% add an existing FDB doesn't hurt
%%
%% bridge(append, Pod, NetName, TargetIp, Config) ->
%%     BridgeMacs = bridge_macs(Pod, NetName, TargetIp, Config),
%%     BridgeExists = lists:member("00:00:00:00:00:00", BridgeMacs),
%%     BridgeExists orelse begin
%% 	Command = cmd("bridge fdb append to 00:00:00:00:00:00 dst ~s dev ~s",
%% 		      [TargetIp, name], Pod, NetName),
%% 	?Agent:exec(Pod, Command, Config)
%%     end;

bridge(delete, Pod, NetName, TargetIp, Config) ->
    lists:foreach(fun(Mac) ->
	Command = cmd("bridge fdb delete ~s dst ~s dev ~s",
		      [Mac, TargetIp, name], Pod, NetName),
	?Agent:exec(Pod, Command, Config)
    end, bridge_macs(Pod, NetName, TargetIp, Config)).

bridge_macs(Pod, NetName, TargetIp, Config) ->
    Command = cmd("bridge fdb show dev ~s", [name], Pod, NetName),
    Result = ?Agent:exec(Pod, Command, Config),
    ?LOG(debug, "Exec Result: ~p", [Result]),
    [Mac || FdbRecord <- string:lexemes(binary_to_list(Result), "\n"),
	    [Mac, "dst", Ip|_ ] <- [string:lexemes(FdbRecord, " ")],
	    Ip == TargetIp].

vxlan_id(Pod, NetName, Config) ->
    Command = cmd("ip -d link show ~s", [name], Pod, NetName),
    Result = ?Agent:exec(Pod, Command, Config),

    List = [Id || Line <- string:lexemes(binary_to_list(Result), "\n"),
	    ["vxlan", "id", Id|_] <- [string:lexemes(Line, " ")]],
    List /= [] andalso {true, hd(List)}.

ips(Pod, NetName, Config) ->
    Command = cmd("ip addr show dev ~s", [name], Pod, NetName),
    Result = ?Agent:exec(Pod, Command, Config),

    [Ip || Line <- string:lexemes(binary_to_list(Result), "\n"),
     ["inet", Ip|_] <- [string:lexemes(Line, " ")]].

common_pod_net_names(Pod1, Pod2) ->
    Pod2Nets = maps:get(nets, Pod2),
    [Name || {Name, _Options} <- maps:get(nets, Pod1),
     proplists:is_defined(Name, Pod2Nets)].

pod_net_names(Pod) ->
    [Name || {Name, _Options} <- maps:get(nets, Pod)].

pod_net_options(NetName, Pod) ->
    proplists:get_value(NetName, maps:get(nets, Pod)).

pod_net_option(OptionName, NetName, Pod) ->
    maps:get(OptionName, pod_net_options(NetName, Pod), false).

cmd(Format, Args, Pod, NetName) ->
    NetOptions = pod_net_options(NetName, Pod),
    CmdArgs = [cmd_arg(Arg, NetOptions) || Arg <- Args],
    lists:flatten(io_lib:format(Format, CmdArgs)).

cmd_arg(Arg, NetOptions) when is_atom(Arg) -> maps:get(Arg, NetOptions);
cmd_arg(Arg, _NetOptions) -> Arg.
