-module(kube_vxlan_controller_cli).

-export([
    args/1,

    version/1,
    help/1,
    usage/0
]).

-define(Usage,
    "Usage: kube-vxlan-controller <Command> [Options]~n"
    "       kube-vxlan-controller help <Command>~n"
    "~n"
    "Commands~n"
    "       run         Run the controller~n"
    "       inspect     Print information about a desired entity~n"
    "       version     Print the controller version~n"
    "~n" ++
    ?UsageOptions
).
-define(UsageOptions,
    "Options~n"
    "       --server=<Kubernetes API server>~n"
    "       --ca-cert-file=<filepath>~n"
    "       --token=<token>~n"
    "       --token-file=<filepath>~n"
    "       --namespace=<namespace>~n"
    "       --namespace-file=<filepath>~n"
    "       --selector=<label selector>~n"
    "       --annotation=<network list annotation>~n"
    "       --configmap-name=<network data configmap name>~n"
    "       --agent-container-name=<name>~n"
    "       --agent-init-container-name=<name>~n"
    "       --db-file=<db file path>~n"
).

-define(HelpRun,
    "kube-vxlan-controller run [Options]~n"
    "~n" ++
    ?UsageOptions
).

-define(HelpInspect,
    "kube-vxlan-controller inspect [Options] [Inspect Options]~n"
    "~n" ++
    ?UsageOptions ++
    "~n" ++
    "Inspect Options~n"
    "       --fields=<Fields>~n"
    "~n"
    "Fields~n"
    "       Any combination of 'pod,net,fdb,dev,route' comma separated~n"
    "       or 'all' to include all of them. If not specified 'pod,net,fdb'~n"
    "       will be used.~n"
).

-define(Version, "Version ~s (git-~s)~n").

args(AllArgs) -> case AllArgs of
    ["run"|Args] -> {run, read_args(Args)};
    ["inspect", Subject|Args] ->
        {inspect, list_to_atom(Subject), read_args(Args)};
    ["config"|Args] -> {config, read_args(Args)};
    ["version"] -> version;
    ["help", Command|_Args] -> {help, list_to_atom(Command)};
    [Command, "help"|_Args] -> {help, list_to_atom(Command)};
    _Other -> usage
end.

version({Vsn, GitSha}) -> lists:flatten(io_lib:format(?Version, [Vsn, GitSha])).

help(run) -> ?HelpRun;
help(inspect) -> ?HelpInspect;
help(_Command) -> ?Usage.

usage() -> ?Usage.

read_args(Args) ->
    {Named, Ordered} = lists:foldl(fun read_arg/2, {#{}, []}, Args),
    {Named, lists:reverse(Ordered)}.

read_arg("--" ++ NamedArg, {Named, Ordered}) ->
    case string:split(NamedArg, "=") of
        [Name] -> {add_arg(arg_name(Name), "", Named), Ordered};
        [Name, Value] -> {add_arg(arg_name(Name), Value, Named), Ordered}
    end;

read_arg([$-|Switches], {Named, Ordered}) ->
    {lists:foldl(fun add_switch/2, Named, Switches), Ordered};

read_arg(Arg, {Named, Ordered}) ->
    {Named, [Arg|Ordered]}.

add_arg(Name, NewValue, Named) ->
    case maps:find(Name, Named) of
        {ok, Value = [H|_]} when is_number(H) ->
            maps:put(Name, [NewValue, Value], Named);
        {ok, Values = [H|_]} when is_list(H) ->
            maps:put(Name, [NewValue|Values], Named);
        error -> maps:put(Name, NewValue, Named)
    end.

add_switch(Switch, Named) ->
    Name = switch_name(Switch),
    maps:put(Name, maps:get(Name, Named, 0) + 1, Named).

arg_name(Key) ->
    list_to_atom(lists:flatten(string:replace(Key, "-", "_", all))).

switch_name(Switch) ->
    list_to_atom([Switch]).
