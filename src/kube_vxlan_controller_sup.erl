-module(kube_vxlan_controller_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-define(Run, kube_vxlan_controller_run).
-define(K8s, kube_vxlan_controller_k8s).
-define(PodReg, kube_vxlan_controller_pod_reg).
-define(PodSup, kube_vxlan_controller_pod_sup).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), #{id       => I,
				start    => {I, start_link, Args},
				restart  => permanent,
				shutdown => 5000,
				type     => Type,
				modules  => [I]}).

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
    SupFlags =
	#{strategy  => one_for_one,
	  intensity => 5,
	  period    => 10},
    {ok, {SupFlags,
	  [
	   ?CHILD(?PodSup, supervisor, []),
	   ?CHILD(?PodReg, worker, []),
	   ?CHILD(?K8s, worker, [Config]),
	   ?CHILD(?Run, worker, [Config])
	  ]}}.
