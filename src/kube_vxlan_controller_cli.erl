-module(kube_vxlan_controller_cli).

-export([
    args/1,

    usage/0,
    version/1
]).

-define(Usage,
    "Usage: kube-vxlan-controller run [Options]~n"
    "       kube-vxlan-controller inspect <Subject> [Options]~n"
    "       kube-vxlan-controller config [Options]~n"
    "       kube-vxlan-controller version~n"
    "~n"
    "Subject~n"
    "       networks~n"
    "~n"
    "Options~n"
    "       --server=<Kubernetes API server>~n"
    "       --ca-cert-file=<filepath>~n"
    "       --token=<token>~n"
    "       --token-file=<filepath>~n"
    "       --namespace=<namespace>~n"
    "       --namespace-file=<filepath>~n"
    "       --selector=<label selector>~n"
    "       --annotation=<network list annotation>~n"
    "       --configmap-name=<data configmap name>~n"
    "       --agent-container-name=<name>~n"
    "       --agent-init-container-name=<name>~n"
).

-define(Version, "Version ~s (git-~s)~n").

args(AllArgs) -> case AllArgs of
    ["run"|Args] -> {run, read_args(Args)};
    ["inspect", Subject|Args] ->
        {inspect, list_to_atom(Subject), read_args(Args)};
    ["config"|Args] -> {config, read_args(Args)};
    ["version"] -> version;
    _Other -> usage
end.

usage() -> ?Usage.
version({Vsn, GitSha}) -> lists:flatten(io_lib:format(?Version, [Vsn, GitSha])).

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
