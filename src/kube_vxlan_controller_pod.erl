-module(kube_vxlan_controller_pod).
-export([exec/5]).
    
-define(K8s, kube_vxlan_controller_k8s_client).
-define(Log, kube_vxlan_controller_log).

-define(PodExecQuery, [
    {"stdout", "true"},
    {"stderr", "true"}
]).

exec(Namespace, PodName, ContainerName, Command, Config) ->
    ?Log:info("~s/~s/~s: ~s", [Namespace, PodName, ContainerName, Command]),

    Resource = "/api/v1/namespaces/" ++ Namespace ++
               "/pods/" ++ PodName ++ "/exec",

    Query = lists:foldl(
        fun(Arg, ExecQuery) -> [{"command", Arg}|ExecQuery] end,
        [{"container", ContainerName}|?PodExecQuery],
        lists:reverse(string:split(Command, " ", all))
    ),

    {ok, Socket} = ?K8s:ws_connect(Resource, Query, Config),
    {ok, Result} = ?K8s:ws_recv(Socket),
    ?K8s:ws_disconnect(Socket),

    ?Log:info(Result),
    Result.
