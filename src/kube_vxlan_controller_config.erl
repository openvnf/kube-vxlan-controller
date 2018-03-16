-module(kube_vxlan_controller_config).

-export([
    init/0,

    load/1,
    build/1,
    validate/1,

    version/0
]).

-define(App, 'kube-vxlan-controller').

-define(OsEnvPrefix, "KVC_").

-define(ConfigKeys, [
    server,
    ca_cert_file,
    token,
    token_file,
    namespace,
    namespace_file,
    selector,
    annotation,
    configmap_name,
    agent_container_name,
    agent_init_container_name
]).

init() -> cpf_env:load(application:get_env(?App, config_files, [])).

load(Args) -> lists:foldl(fun load_args/2, Args, ?ConfigKeys).

load_args(Key, Config) ->
    case maps:find(Key, Config) of
        {ok, Value} -> maps:put(Key, {Value, arg}, Config);
        error -> load_env(Key, Config)
    end.

load_env(Key, Config) ->
    case os:getenv(?OsEnvPrefix ++ string:uppercase(atom_to_list(Key))) of
        Value when is_list(Value) -> maps:put(Key, {Value, env}, Config);
        false -> load_cfg(Key, Config)
    end.

load_cfg(Key, Config) ->
    case application:get_env(?App, Key) of
        {ok, Value} -> maps:put(Key, {Value, cfg}, Config);
        undefined -> Config
    end.

build(Config) -> cpf_funs:apply_while([
    {annotation, fun build/2, [annotation, unsource(Config)]},
    {token, fun build/2, [token, {annotation}]},
    {namespace, fun build/2, [namespace, {token}]}
]).

build(annotation, Config) ->
    maps:put(annotation, list_to_atom(maps:get(annotation, Config)), Config);

build(token, Config) -> build_from_file(token, Config);
build(namespace, Config) -> build_from_file(namespace, Config).

validate(Config) -> {ok, Config}.

unsource(Config) -> maps:map(fun
    (_Key, {Value, _Source}) -> Value;
    (_Key, Value) -> Value
end, Config).

build_from_file(Key, Config) ->
    FileKey = list_to_atom(atom_to_list(Key) ++ "_file"),
    case maps:is_key(Key, Config) of
        true -> {ok, maps:remove(FileKey, Config)};
        false -> case maps:find(FileKey, Config) of
            {ok, FileName} ->
                NewConfig = maps:remove(FileKey, Config),
                maps_put_file(Key, FileName, NewConfig);
            error -> {ok, Config}
        end
    end.

version() ->
    {ok, Vsn} = application:get_key(?App, vsn),
    {ok, GitSha} = application:get_env(?App, git_sha),
    {Vsn, GitSha}.

maps_put_file(Key, FileName, Map) ->
    case file:read_file(FileName) of
        {ok, Binary} -> {ok, maps:put(Key, binary_to_list(Binary), Map)};
        {error, Reason} -> {error, {Reason, FileName}}
    end.
