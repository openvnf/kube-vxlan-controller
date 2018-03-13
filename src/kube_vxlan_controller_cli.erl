-module(kube_vxlan_controller_cli).

-export([
    usage/0,
    version/1,
    read_args/1
]).

-define(Usage,
    "Usage: kube-vxlan-controller run <Options>~n"
    "       kube-vxlan-controller inspect <Subject> <Options>~n"
    "       kube-vxlan-controller version~n"
    "~n"
    "Subject~n"
    "       networks~n"
    "~n"
    "Options~n"
    "       --server=<Kubernetes API server>~n"
    "       --namespace-file=<filepath>~n"
    "       --ca-cert-file=<filepath>~n"
    "       --token-file=<filepath>~n"
    "       --selector=<label selector>~n"
    "       --annotation=<network list annotation>~n"
    "       --vxlan-config-name=<vxlan config map name>~n"
    "       --agent-container-name=<name>~n"
    "       --agent-init-container-name=<name>~n"
).

-define(Version, "Version ~s (git-~s)~n").

usage() -> ?Usage.
version({Vsn, GitSha}) -> lists:flatten(io_lib:format(?Version, [Vsn, GitSha])).

read_args(["run"|Args]) ->
    {ok, {run, read_cmd_args(Args)}};

read_args(["inspect", Subject|Args]) ->
    {ok, {inspect, list_to_atom(Subject), read_cmd_args(Args)}};

read_args(["version"]) -> {ok, version};
read_args(_Other) -> {ok, usage}.

read_cmd_args(Args) ->
    {CmdArgs, Config} = lists:foldl(fun read_arg/2, {[], #{}}, Args),
    {lists:reverse(CmdArgs), Config}.

read_arg("--" ++ Arg, {Args, Config}) ->
    case string:split(Arg, "=") of
        [Key, Value] -> {Args, read_arg(Key, Value, Config)};
        _Other -> {Args, Config}
    end;

read_arg(Arg, {Args, Config}) -> {[Arg|Args], Config}.

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
