-module(kube_vxlan_controller_pod_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_pod/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(Pod, kube_vxlan_controller_pod).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_pod(Id, Event, Config) ->
    supervisor:start_child(?SERVER, [Id, Event, Config]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 5, 10},
	  [{?Pod, {?Pod, start_link, []}, temporary, 1000, worker, [?Pod]}]}}.
