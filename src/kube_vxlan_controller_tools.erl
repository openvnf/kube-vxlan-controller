-module(kube_vxlan_controller_tools).

-export([
    nets_data/2,
    nets/2,

    net_members/5
]).

-define(NetType, "vxlan").
-define(NetDev, "eth0").

nets_data(Annotations, Config) ->
    binary_to_list(maps:get(maps:get(annotation, Config), Annotations, <<>>)).

nets(NetsData, GlobalNetsOptions) ->
    TokensString = re:replace(NetsData, "\\h*,\\h*", ",",
                              [global, {return, list}]),
    Tokens = string:lexemes(TokensString, ",\n"),
    Nets = lists:reverse(lists:foldl(fun nets_build/2, [], Tokens)),
    nets_set_global_options(GlobalNetsOptions, Nets).

nets_build(Token = [$ |_], [Net|Nets]) ->
    [net_add_options(string:lexemes(Token, " "), Net)|Nets];

nets_build(Token, Nets) ->
    [NetName|NetOptions] = string:lexemes(Token, " "),
    [net_add_options(NetOptions, net_new(NetName))|Nets].

net_new(NetName) ->
    {NetName, #{
        name => NetName,
        type => ?NetType,
        dev => ?NetDev
    }}.

net_add_options(Options, Net) ->
    lists:foldl(fun net_add_option/2, Net, Options).

net_add_option(Option, {NetName, NetOptions}) ->
    [OptionName|OptionValue] = string:split(Option, "="),
    {NetName, maps:put(list_to_atom(OptionName),
                       lists:flatten(OptionValue), NetOptions)}.

nets_set_global_options(GlobalNetsOptions, Nets) ->
    lists:filtermap(fun({NetName, NetOptions}) ->
        GlobalNetOptions = maps:get(NetName, GlobalNetsOptions, #{}),
        MergedNetOptions = maps:merge(GlobalNetOptions, NetOptions),
        maps:is_key(id, MergedNetOptions) andalso
            {true, {NetName, MergedNetOptions}}
    end, Nets).

net_members(NetName, ExcludePodName, Pods, GlobalNetsOptions, Config) ->
    [{binary_to_list(Namespace),
      binary_to_list(PodName),
      binary_to_list(PodIp),
      lists:keyfind(NetName, 1, Nets)} ||
     #{metadata := #{
         namespace := Namespace,
         name := PodName,
         annotations := Annotations
      },
       status := #{
         podIP := PodIp,
         containerStatuses := ContainerStatuses
       }
     } <- Pods,
     #{name := ContainerName,
       state := ContainerState
     } <- ContainerStatuses,
     binary_to_list(ContainerName) == maps:get(agent_container_name, Config),
     maps:is_key(running, ContainerState),
     Nets <- [nets(nets_data(Annotations, Config), GlobalNetsOptions)],
     binary_to_list(PodName) /= ExcludePodName andalso
     lists:keymember(NetName, 1, Nets)].
