-module('kube-vxlan-controller').

-behaviour(application).

-define(AppSup, kube_vxlan_controller_sup).
-define(Config, kube_vxlan_controller_config).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    case ?Config:load(#{}) of
	{ok, Config} ->
	    ?AppSup:start_link(Config);
	{error, Reason} = Error ->
	    io:format("~p~n", [Reason]),
	    Error
    end.

stop(_State) ->
    ok.
