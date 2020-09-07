-module(kube_vxlan_controller_pod).

-export([
    get/1, get/2, get/3,
    exec/5
]).

-include_lib("kernel/include/logger.hrl").

-define(K8s, kube_vxlan_controller_k8s).

-define(ExecQuery, [
    {"stdout", "true"},
    {"stderr", "true"}
]).

get(Config) -> get(all, {label, false}, Config).
get(Filter = {label, _Selector}, Config) -> get(all, Filter, Config).

get(Namespace, Filter, Config) ->
    Resource = case {Namespace, Filter} of
        {all, Filter} ->
            "/api/v1/pods";
        {Namespace, {label, _Selector}} -> fmt(
            "/api/v1/namespaces/~s/pods", [Namespace]
        );
        {Namespace, {pod, Name}} -> fmt(
            "/api/v1/namespaces/~s/pods/~s", [Namespace, Name]
        )
    end,
    Query = case Filter of
        {pod, _Name} -> [];
        {label, false} -> [];
        {label, Selector} -> [{"labelSelector", Selector}]
    end,
    case ?K8s:http_request(Resource, Query, Config) of
        {ok, [Result]} -> {ok, maps:get(items, Result, Result)};
        {error, Reason} -> {error, Reason}
    end.

exec(Namespace, PodName, ContainerName, Command, Config) ->
    Silent = maps:get(silent, Config, false),

    Silent orelse
        ?LOG(info, "~s/~s/~s: ~s", [Namespace, PodName, ContainerName, Command]),

    Resource = fmt("/api/v1/namespaces/~s/pods/~s/exec", [Namespace, PodName]),

    Query = lists:foldl(
        fun(Arg, ExecQuery) -> [{"command", Arg}|ExecQuery] end,
        [{"container", ContainerName}|?ExecQuery],
        lists:reverse(string:split(Command, " ", all))
    ),

    case ?K8s:ws_connect(Resource, Query, Config) of
        {ok, Socket} ->
            case ?K8s:ws_recv(Socket) of
                {ok, Result} ->
                    ?K8s:ws_close(Socket),
                    Silent orelse ?LOG(info, "~s", [Result]),
                    Result;
                {error, Reason} ->
                    ?K8s:ws_close(Socket),
                    ?LOG(error, Reason),
                    ""
            end;
        {error, Reason} ->
            ?LOG(error, Reason),
            ""
    end.

fmt(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
