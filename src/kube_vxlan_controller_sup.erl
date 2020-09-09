-module(kube_vxlan_controller_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-define(Run, kube_vxlan_controller_run).
-define(K8s, kube_vxlan_controller_k8s).
-define(AgentSup, kube_vxlan_controller_agent_sup).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Config) ->
    ?LOG(info, "starting ~p with ~p", [?MODULE, Config]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Config]) ->
    {ok, {{one_for_one, 5, 10},
	  [
	   ?CHILD(?AgentSup, supervisor, []),
	   ?CHILD(?K8s, worker, [Config]),
	   ?CHILD(?Run, worker, [Config])
	  ]}}.
