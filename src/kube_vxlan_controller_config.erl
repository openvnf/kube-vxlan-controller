-module(kube_vxlan_controller_config).

-export([
    version/0
]).

-define(App, 'kube-vxlan-controller').

version() ->
    {ok, Vsn} = application:get_key(?App, vsn),
    {ok, GitSha} = application:get_env(?App, git_sha),
    {Vsn, GitSha}.
