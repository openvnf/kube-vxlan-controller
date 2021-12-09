-module(kube_vxlan_controller_pod_reg).

-behaviour(regine_server).

%% API
-export([start_link/0]).
-export([register/2, unregister/1, lookup/1, set/2, unset/1]).
-export([process_event/3, clear/0, all/0, all/1]).

%% regine_server callbacks
-export([init/1, handle_register/4, handle_unregister/3, handle_pid_remove/3,
	 handle_death/3, handle_call/3, terminate/2]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("kernel/include/logger.hrl").
-include("include/kube_vxlan_controller.hrl").

-define(SERVER, ?MODULE).
-define(Pod, kube_vxlan_controller_pod).
-define(PodSup, kube_vxlan_controller_pod_sup).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    regine_server:start_link({local, ?SERVER}, ?MODULE, []).

register(Key, Pid) when is_pid(Pid) ->
    regine_server:register(?SERVER, Pid, Key, undefined).
unregister(Key) ->
    regine_server:unregister(?SERVER, Key, undefined).

set(Key, Value) ->
    regine_server:call(?SERVER, {set, Key, Value}).

unset(Key) ->
    regine_server:call(?SERVER, {set, Key, undefined}).

lookup(Key) ->
    case ets:lookup(?SERVER, Key) of
	[{Key, Pid, _}] ->
	    {ok, Pid};
	_ ->
	    {error, not_found}
    end.

clear() ->
    regine_server:call(?SERVER, clear).

all(Cycle) ->
    Ms = ets:fun2ms(fun({#uid{cycle = C}, _, Pod}) when C =:= Cycle, is_map(Pod) -> Pod end),
    ets:select(?SERVER, Ms).

all() ->
    ets:tab2list(?SERVER).

process_event(Id, Event, Config) ->
    case lookup(Id) of
	{ok, Pid} ->
	    Pid ! Event;
	_ ->
	    regine_server:call(?SERVER, {process_event, Id, Event, Config})
    end.


%%%===================================================================
%%% regine callbacks
%%%===================================================================

init([]) ->
    ets:new(?SERVER, [ordered_set, named_table, public, {keypos, 1}]),
    {ok, #{}}.

handle_register(Pid, Id, Value, State) ->
    case ets:insert_new(?SERVER, {Id, Pid, Value}) of
	true ->  {ok, [Id], State};
	false -> {error, duplicate}
    end.

handle_unregister(Key, _Value, State) ->
    unregister(Key, State).

handle_pid_remove(_Pid, Keys, State) ->
    lists:foreach(fun(Key) -> ets:delete(?SERVER, Key) end, Keys),
    maps:without(Keys, State).

handle_death(_Pid, _Reason, State) ->
    State.

handle_call({set, Key, Value}, _From, State) ->
    Result = ets:update_element(?SERVER, Key, {3, Value}),
    {reply, Result, State};

handle_call(clear, _From, State) ->
    [?Pod:stop(Pid) || {_, Pid, _} <- ets:tab2list(?SERVER)],
    ets:delete_all_objects(?SERVER),
    {reply, ok, State};

handle_call({process_event, Id, Event, Config}, _From, State) ->
    case lookup(Id) of
	{ok, Pid} ->
            Pid ! Event,
            {reply, ok, State};
	_Other ->
            start_pod(Id, Event, Config, State)
    end.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_pod(Id, Event, Config, State) ->
    case ?PodSup:start_pod(Id, Event, Config) of
	{ok, Pid} ->
            {reply, ok, State, [{register, Pid, Id, undefined}]};
	Other ->
	    ?LOG(debug, "failed to start pod for ~p with ~p", [Id, Other]),
            {reply, Other, State}
    end.

unregister(Key, State) ->
    Pids = [Pid || {_, Pid, _} <- ets:take(?SERVER, Key)],
    {Pids, maps:remove(Key, State)}.
