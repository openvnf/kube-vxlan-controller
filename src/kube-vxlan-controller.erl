-module('kube-vxlan-controller').

-export([main/1]).

-define(Cli, kube_vxlan_controller_cli).
-define(Run, kube_vxlan_controller_run).
-define(List, kube_vxlan_controller_list).
-define(Config, kube_vxlan_controller_config).
-define(Inspect, kube_vxlan_controller_inspect).

main(CliArgs) ->
    application:ensure_all_started(?MODULE),
    case ?Cli:args(CliArgs) of
        {run, Args} -> do(run, Args);
        {list, Subject, Args} -> do({list, Subject}, Args);
        {inspect, Subject, Args} -> do({inspect, Subject}, Args);
        {config, Args} -> do(config, Args);
        {help, Command} -> io:format(?Cli:help(Command));
        version -> io:format(?Cli:version(?Config:version()));
        usage -> io:format(?Cli:usage())
    end.

do(config, {NamedArgs, _OrderedArgs}) ->
    io:format("~p~n", [?Config:init()]),
    io:format("~p~n", [?Config:read(NamedArgs)]);

do(Action, {NamedArgs, OrderedArgs}) ->
    case ?Config:load(NamedArgs) of
        {ok, Config} -> do(Action, OrderedArgs, Config);
        {error, Reason} -> io:format("~p~n", [Reason])
    end.

do(run, _Args, Config) -> ?Run:loop(Config);
do({list, Subject}, _Args, Config) -> ?List:Subject(Config);
do({inspect, Subject}, Args, Config) -> ?Inspect:Subject(Args, Config).
