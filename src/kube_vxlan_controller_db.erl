-module(kube_vxlan_controller_db).

-export([
    load_resource_version/2,
    save_resource_version/3,

    nets_options/1
]).

-define(K8s, kube_vxlan_controller_k8s).

load_resource_version(Selector, Config) ->
    maps_get_sub(resource_versions, Selector, load_config(Config), "0").

save_resource_version(Selector, Version, Config) ->
    Data = load_config(Config),
    NewData = maps_put_sub(resource_versions, Selector, Version, Data),
    save_config(NewData, Config).

nets_options(
    Config = #{namespace := Namespace,
               configmap_name := ConfigMapName}
) ->
    Resource = "/api/v1/namespaces/" ++ Namespace ++
               "/configmaps/" ++ ConfigMapName,
    {ok, [#{data := Data}]} = ?K8s:http_request(Resource, [], Config),

    maps:fold(fun(NetName, NetOptions, Map) ->
        maps:put(atom_to_list(NetName), net_options(NetOptions), Map)
    end, #{}, Data).

net_options(Options) ->
    maps:from_list(net_id_bc([
        {list_to_atom(OptionName), lists:flatten(OptionValue)} ||
        Option <- string:lexemes(binary_to_list(Options), " "),
        [OptionName|OptionValue] <- [string:split(Option, "=")]
    ])).

%%% provided for BC, removed when not needed
net_id_bc(Options) ->
    lists:map(fun({Name, Value}) ->
        NameString = atom_to_list(Name),
        try list_to_integer(NameString) of
            Id -> {id, integer_to_list(Id)}
        catch
            _:_ -> {Name, Value}
        end
    end, Options).
%%%

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
