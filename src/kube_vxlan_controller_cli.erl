-module(kube_vxlan_controller_cli).

-export([
    usage/0,
    version/1,
    read_args/1
]).

-define(Usage,
    "Usage: kube-vxlan-controller run~n"
    "           --server=<Kubernetes API server>~n"
    "           --namespace-file=<filepath>~n"
    "           --ca-cert-file=<filepath>~n"
    "           --token-file=<filepath>~n"
    "           --selector=<label selector>~n"
    "           --annotation=<network list annotation>~n"
    "           --vxlan-config-name=<vxlan config map name>~n"
    "           --agent-container-name=<name>~n"
    "~n"
    "       kube-vxlan-controller version~n"
).

-define(Version, "Version ~s (git-~s)~n").

usage() -> ?Usage.
version({Vsn, GitSha}) -> lists:flatten(io_lib:format(?Version, [Vsn, GitSha])).

read_args(["run"|Args]) -> {ok, {run, lists:foldl(fun read_arg/2, #{}, Args)}};
read_args(["version"]) -> {ok, version};
read_args(_Other) -> {ok, usage}.

read_arg("--" ++ Arg, Config) ->
    case string:split(Arg, "=") of
        [Key, Value] -> read_arg(Key, Value, Config);
        _Other -> Config
    end.

read_arg(RawKey, Value, Config) when is_list(RawKey) ->
    Key = list_to_atom(lists:flatten(string:replace(RawKey, "-", "_", all))),
    read_arg(Key, Value, Config);

read_arg(namespace_file, Value, Config) ->
    read_arg(namespace, read_file(Value), Config);

read_arg(token_file, Value, Config) ->
    read_arg(token, read_file(Value), Config);

read_arg(Key = annotation, Value, Config) ->
    maps:put(Key, list_to_atom(Value), Config);

read_arg(Key, Value, Config) -> maps:put(Key, Value, Config).

read_file(FileName) ->
    binary_to_list(element(2, file:read_file(FileName))).
