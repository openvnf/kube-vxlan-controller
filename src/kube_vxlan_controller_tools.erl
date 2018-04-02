-module(kube_vxlan_controller_tools).

-export([
    pods/4,
    pod/4,

    pod_nets_data/2,
    pod_nets/2,

    pod_read_net_option/1,
    pod_container_state/2
]).

-define(NetType, "vxlan").
-define(NetDev, "eth0").

pods(PodResources, GlobalNetsOptions, Filters, Config) ->
    lists:filtermap(fun(PodResource) ->
        pod(PodResource, GlobalNetsOptions, Filters, Config)
    end, PodResources).

pod(#{
    metadata := #{
      namespace := Namespace,
      name := PodName,
      annotations := Annotations
    },
    status := #{
      containerStatuses := ContainerStatuses
    } = Status
}, GlobalNetsOptions, Filters, Config) ->
    IsAgentRunning = is_agent_running(ContainerStatuses, Config),
    IsAgentRunning andalso begin
        NetsData = pod_nets_data(Annotations, Config),
        Pod = #{
            namespace => binary_to_list(Namespace),
            name => binary_to_list(PodName),
            ip => binary_to_list(maps:get(podIP, Status, <<>>)),
            nets => pod_nets(NetsData, GlobalNetsOptions)
        },
        pod_apply_filters(Pod, Filters)
    end.

pod_apply_filters(Pod, Filters) ->
    lists:foldl(fun pod_apply_filter/2, {true, Pod}, Filters).

pod_apply_filter({with_pods, PodNames}, {true, Pod}) ->
    IsMember = lists:member(maps:get(name, Pod), PodNames),
    IsMember andalso {true, Pod};

pod_apply_filter({without_pods, PodNames}, {true, Pod}) ->
    IsNotMember = not lists:member(maps:get(name, Pod), PodNames),
    IsNotMember andalso {true, Pod};

pod_apply_filter({with_nets, NetNames}, {true, Pod}) ->
    PodNets = [Net || Net = {NetName, _NetOptions} <- maps:get(nets, Pod),
               lists:member(NetName, NetNames)],
    PodNets /= [] andalso {true, maps:put(nets, PodNets, Pod)};

pod_apply_filter(_Filter, false) -> false.

pod_nets_data(Annotations, Config) ->
    binary_to_list(maps:get(maps:get(annotation, Config), Annotations, <<>>)).

pod_nets(NetsData, GlobalNetsOptions) ->
    TokensString = re:replace(NetsData, "\\h*,\\h*", ",",
                              [global, {return, list}]),
    Tokens = string:lexemes(TokensString, ",\n"),
    Nets = lists:reverse(lists:foldl(fun pod_nets_build/2, [], Tokens)),
    pod_nets_apply_global_options(GlobalNetsOptions, Nets).

pod_nets_build(Token = [$ |_], [Net|Nets]) ->
    [pod_net_add_options(string:lexemes(Token, " "), Net)|Nets];

pod_nets_build(Token, Nets) ->
    [NetName|NetOptions] = string:lexemes(Token, " "),
    [pod_net_add_options(NetOptions, pod_net_new(NetName))|Nets].

pod_net_new(NetName) ->
    {NetName, #{
        name => NetName,
        type => ?NetType,
        dev => ?NetDev
    }}.

pod_net_add_options(Options, Net) ->
    lists:foldl(fun pod_net_add_option/2, Net, Options).

pod_net_add_option(Option, {NetName, NetOptions}) ->
    {OptionName, OptionValue} = pod_read_net_option(Option),
    {NetName, maps:put(OptionName, OptionValue, NetOptions)}.

pod_read_net_option(Option) ->
    [Name|Value] = string:split(Option, "="),
    {list_to_atom(Name), case Value of
        [] -> true;
        ["true"] -> true;
        ["false"] -> false;
        Value -> lists:flatten(Value)
     end}.

pod_nets_apply_global_options(GlobalNetsOptions, Nets) ->
    lists:filtermap(fun({NetName, NetOptions}) ->
        GlobalNetOptions = maps:get(NetName, GlobalNetsOptions, #{}),
        MergedNetOptions = maps:merge(GlobalNetOptions, NetOptions),
        maps:is_key(id, MergedNetOptions) andalso
            {true, {NetName, MergedNetOptions}}
    end, Nets).

is_agent_running(ContainerStatuses, Config) ->
    ContainerName = maps:get(agent_container_name, Config),
    pod_container_state(ContainerName, ContainerStatuses) == running.

pod_container_state(_ContainerName, []) -> unknown;
pod_container_state(Name, Statuses) ->
    case [maps:keys(maps:get(state, Status)) || Status <- Statuses,
          maps:get(name, Status) == list_to_binary(Name)]
    of
        [[State]] -> State;
        [] -> unknown
    end.
