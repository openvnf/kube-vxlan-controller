-module(kube_vxlan_controller_agent_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, exec/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(Agent, kube_vxlan_controller_agent).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

exec(Action) ->
    supervisor:start_child(?SERVER, [Action]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 5, 10},
	  [{?Agent, {?Agent, start_link, []}, temporary, 1000, worker, [?Agent]}]}}.
