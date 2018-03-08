-module(kube_vxlan_controller_db).

-export([
    vxlan_ids/1, vxlan_ids/2
]).

-define(K8s, kube_vxlan_controller_k8s).

vxlan_ids(Config = #{namespace := Namespace, vxlan_config_name := Name}) ->
    Resource = "/api/v1/namespaces/" ++ Namespace ++ "/configmaps/" ++ Name,
    {ok, [#{data := Data}]} = ?K8s:http_request(Resource, [], Config),

    maps:fold(fun(K, V, Map) ->
        maps:put(atom_to_list(K), binary_to_list(V), Map)
    end, #{}, Data).

vxlan_ids(VxlanNames, Config) ->
    lists:foldl(find_vxlan_id_fun(vxlan_ids(Config)), {#{}, []}, VxlanNames).

find_vxlan_id_fun(VxlanIds) -> fun(VxlanName, {Found, NotFound}) ->
    case maps:find(VxlanName, VxlanIds) of
        {ok, VxlanId} -> {maps:put(VxlanName, VxlanId, Found), NotFound};
        error -> {Found, [VxlanName|NotFound]}
    end
end.
