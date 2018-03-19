-module(kube_vxlan_controller_db).

-export([
    network_name_id_map/2,
    network_name_id_map/1
]).

-define(K8s, kube_vxlan_controller_k8s).

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
