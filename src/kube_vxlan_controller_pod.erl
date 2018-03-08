-module(kube_vxlan_controller_pod).

-export([
    get/1, get/2, get/3,
    exec/5
]).
    
-define(K8s, kube_vxlan_controller_k8s).
-define(Log, kube_vxlan_controller_log).

-define(ExecQuery, [
    {"stdout", "true"},
    {"stderr", "true"}
]).

get(Config) -> get(false, "", Config).
get(Selector, Config) -> get(false, Selector, Config).
get(Namespace, Selector, Config) ->
    Resource = case Namespace of
        false -> "/api/v1/pods/";
        Namespace -> "/api/v1/namespaces/" ++ Namespace ++ "/pods/"
    end,
    Query = [{"labelSelector", Selector}],
    {ok, [#{items := Items}]} = ?K8s:http_request(Resource, Query, Config),
    Items.

exec(Namespace, PodName, ContainerName, Command, Config) ->
    ?Log:info("~s/~s/~s: ~s", [Namespace, PodName, ContainerName, Command]),

    Resource = "/api/v1/namespaces/" ++ Namespace ++
               "/pods/" ++ PodName ++ "/exec",

    Query = lists:foldl(
        fun(Arg, ExecQuery) -> [{"command", Arg}|ExecQuery] end,
        [{"container", ContainerName}|?ExecQuery],
        lists:reverse(string:split(Command, " ", all))
    ),

    case ?K8s:ws_connect(Resource, Query, Config) of
        {ok, Socket} ->
            {ok, Result} = ?K8s:ws_recv(Socket),
            ?K8s:ws_disconnect(Socket),
            ?Log:info("~s", [Result]),
            Result;
        {error, Reason} ->
            ?Log:error(Reason),
            ""
    end.
