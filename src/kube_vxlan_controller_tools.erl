-module(kube_vxlan_controller_tools).

-export([
    networks_data/2,
    networks/2,

    network_members/5
]).

-define(NetworkType, "vxlan").
-define(NetworkDev, "eth0").

networks_data(Annotations, Config) ->
    binary_to_list(maps:get(maps:get(annotation, Config), Annotations, <<>>)).

networks(NetworksData, NetworksNameIdMap) ->
    TokensString = re:replace(NetworksData, "\\h*,\\h*", ",",
                              [global, {return, list}]),
    Tokens = string:lexemes(TokensString, ",\n"),
    Networks = lists:reverse(lists:foldl(fun networks_build/2, [], Tokens)),
    networks_set_ids(NetworksNameIdMap, Networks).

networks_build(Token = [$ |_], Networks) ->
    networks_add_params(string:lexemes(Token, " "), Networks);

networks_build(Token, Networks) ->
    [NetworkName|NetworkParams] = string:lexemes(Token, " "),
    Network = {NetworkName, network_new(NetworkName)},
    networks_add_params(NetworkParams, [Network|Networks]).

network_new(NetworkName) -> #{
    name => NetworkName,
    type => ?NetworkType,
    dev => ?NetworkDev
}.

networks_add_params(Params, Networks) ->
    lists:foldl(fun networks_add_param/2, Networks, Params).

networks_add_param(Option, [{NetworkName, Network}|RestNetworks]) ->
    [KeyString|ValueList] = string:split(Option, "="),
    Key = list_to_atom(KeyString),
    Value = lists:flatten(ValueList),
    [{NetworkName, maps:put(Key, Value, Network)}|RestNetworks].

networks_set_ids(NameIdMap, Networks) ->
    [{Name, case maps:is_key(id, Network) of
        true -> Network;
        false -> maps:put(id, maps:get(Name, NameIdMap), Network)
      end} || {Name, Network} <- Networks,
     maps:is_key(id, Network) orelse maps:is_key(Name, NameIdMap)].

network_members(NetworkName, ExcludePodName, Pods, NameIdMap, Config) ->
    [{binary_to_list(Namespace),
      binary_to_list(PodName),
      binary_to_list(PodIp),
      proplists:get_value(NetworkName, Networks)} ||
     #{metadata := #{
         namespace := Namespace,
         name := PodName,
         annotations := Annotations
      },
       status := #{
         podIP := PodIp,
         phase := <<"Running">>
       }
     } <- Pods,
     Networks <- [networks(networks_data(Annotations, Config), NameIdMap)],
     binary_to_list(PodName) /= ExcludePodName andalso
     lists:keymember(NetworkName, 1, Networks)].
