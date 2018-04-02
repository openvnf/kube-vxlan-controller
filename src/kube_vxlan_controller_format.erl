-module(kube_vxlan_controller_format).

-export([
    pod/1,
    pod_nets/2,
    pod_net/1,
    pod_net_options/1,

    dev/2,
    fdb/2
]).

pod(#{namespace := Namespace, name := Name, ip := Ip}) ->
    format("~s/~s ~s", [Namespace, Name, Ip]).

pod_nets(Nets, Ident) ->
    format_list([pod_net(Net) || Net <- Nets], Ident).

pod_net({Name, Options}) ->
    format("~s: ~s", [Name, pod_net_options(Options)]).

pod_net_options(Options) ->
    string:trim(maps:fold(fun(Name, Value, Acc) ->
        Acc ++ format("~s:~s ", [Name, Value])
    end, "", Options)).

dev(Dev, Ident) ->
    format_list([L || [_,_,_|L] <- string:lexemes(Dev, "\n")], Ident).

fdb(Fdb, Ident) ->
    format_list(string:lexemes(Fdb, "\n"), Ident).

format_list(List, Ident) ->
    IdentString = lists:duplicate(Ident, $ ),
    IdentString ++ lists:join([$\n|IdentString], List).

format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
