-module(kube_vxlan_controller_state).

-export([
    merge_pod_pending_info/2,
    pod_pending_action/2,

    set_pod_pending/3,
    unset_pod_pending/2,

    resource_version/1,
    set_resource_version/2,
    is_resource_version_shown/1,
    set_resource_version_shown/1,
    set_resource_version_unshown/1
]).

merge_pod_pending_info(Pod = #{pod_uid := PodUid}, State) ->
    #{pending_info := Info} = maps:get(PodUid, State),
    maps:merge(Pod, Info).

pod_pending_action(#{pod_uid := PodUid}, State) ->
    case maps:find(PodUid, State) of
        {ok, #{pending_action := Action}} -> {ok, Action};
        error -> error
    end.

set_pod_pending(Action, #{pod_uid := PodUid, pod_ip := PodIp}, State) ->
    maps:put(PodUid, #{pending_action => Action,
                       pending_info => #{pod_ip => PodIp}}, State).

unset_pod_pending(#{pod_uid := PodUid}, State) ->
    maps:remove(PodUid, State).

resource_version(State) ->
    maps:get(resource_version, State, "0").

set_resource_version(#{resource_version := Value}, State) ->
    OldValue = list_to_integer(resource_version(State)),
    ProposedValue = list_to_integer(Value),

    case ProposedValue >= OldValue of
        true ->
            NewValue = integer_to_list(ProposedValue + 1),
            NewState = set_resource_version_unshown(State),
            maps:put(resource_version, NewValue, NewState);
        false -> State
    end.

is_resource_version_shown(State) ->
    maps:get(resource_version_shown, State, false).

set_resource_version_shown(State) ->
    maps:put(resource_version_shown, true, State).

set_resource_version_unshown(State) ->
    maps:put(resource_version_shown, false, State).
