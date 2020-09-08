-module(kube_vxlan_controller_state).

-export([
    is/3,
    set/3,
    unset/3,

    resource_version/1,
    set_resource_version/2,
    is_resource_version_shown/1,
    set_resource_version_shown/1,
    set_resource_version_unshown/1
]).

is(Event, #{pod_uid := Uid}, State) ->
    sets:is_element(Uid, event(Event, State)).

set(Event, #{pod_uid := Uid}, State) ->
    maps:put(Event, sets:add_element(Uid, event(Event, State)), State).

unset(Event, #{pod_uid := Uid}, State) ->
    maps:put(Event, sets:del_element(Uid, event(Event, State)), State).

event(Event, State) -> maps:get(Event, State, sets:new()).

resource_version(State) ->
    maps:get(resource_version, State, "0").

set_resource_version(#{resource_version := Value}, State) ->
    OldValue = resource_version(State),
    ProposedValue = Value,

    case ProposedValue >= OldValue of
        true ->
            NewValue = ProposedValue + 1,
            NewState = set_resource_version_unshown(State),
            set_resource_version(NewValue, NewState);
        false -> State
    end;

set_resource_version(Version, State) ->
    maps:put(resource_version, Version, State).

is_resource_version_shown(State) ->
    maps:get(resource_version_shown, State, false).

set_resource_version_shown(State) ->
    maps:put(resource_version_shown, true, State).

set_resource_version_unshown(State) ->
    maps:put(resource_version_shown, false, State).
