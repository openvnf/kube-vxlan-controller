-module(kube_vxlan_controller_db).

-export([
    load_resource_version/2,
    save_resource_version/3,

    network_name_id_map/2,
    network_name_id_map/1
]).

-define(K8s, kube_vxlan_controller_k8s).

load_resource_version(Selector, Config) ->
    maps_get_sub(resource_versions, Selector, load_config(Config), "0").

save_resource_version(Selector, Version, Config) ->
    Data = load_config(Config),
    NewData = maps_put_sub(resource_versions, Selector, Version, Data),
    save_config(NewData, Config).

network_name_id_map(Names, Config) ->
    NameIdMap = maps:with(Names, network_name_id_map(Config)),
    {NameIdMap, [Name || Name <- Names, not maps:is_key(Name, NameIdMap)]}.

network_name_id_map(
    Config = #{namespace := Namespace,
               configmap_name := ConfigMapName}
) ->
    Resource = "/api/v1/namespaces/" ++ Namespace ++
               "/configmaps/" ++ ConfigMapName,
    {ok, [#{data := Data}]} = ?K8s:http_request(Resource, [], Config),

    maps:fold(fun(K, V, Map) ->
        maps:put(atom_to_list(K), binary_to_list(V), Map)
    end, #{}, Data).

load_config(Config) ->
    case file:consult(maps:get(db_file, Config)) of
        {ok, [Data]} when is_map(Data) -> Data;
        {error, _Reason} -> #{}
    end.

save_config(Data, Config) ->
    FilePath = maps:get(db_file, Config),
    filelib:ensure_dir(FilePath),
    file:write_file(FilePath, lists:flatten(io_lib:format("~p.~n", [Data]))).

maps_get_sub(Key, SubKey, Map, Default) ->
    maps:get(SubKey, maps:get(Key, Map, #{}), Default).

maps_put_sub(Key, SubKey, Value, Map) ->
    SubMap = maps:get(Key, Map, #{}),
    NewSubMap = maps:put(SubKey, Value, SubMap),
    maps:put(Key, NewSubMap, Map).
