-module(kube_vxlan_controller_cli).

-export([
    usage/0,
    read_args/1
]).

-define(Usage,
    "Usage: kube-vxlan-controller~n"
    "           --server=<Kubernetes API server>~n"
    "           --namespace-file=<filepath>~n"
    "           --ca-cert-file=<filepath>~n"
    "           --token-file=<filepath>~n"
    "           --vxlan-config-name=<vxlan config map name>~n"
    "           --agent-container-name=<name>~n"
).

usage() -> ?Usage.

read_args(Args) ->
    lists:foldl(fun read_arg/2, #{}, Args).

read_arg("--" ++ Arg, Config) ->
    case string:lexemes(Arg, "=") of
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

read_arg(Key, Value, Config) -> maps:put(Key, Value, Config).

read_file(FileName) ->
    binary_to_list(element(2, file:read_file(FileName))).
