-module(kube_vxlan_controller_utils).

-export([
    do_while/1,
    foldl_while/2
]).

do_while(FunSpecs) ->
    {_Id, Result} = hd(lists:foldl(fun do_while_fun/2, [], FunSpecs)),
    Result.

do_while_fun(_FunSpec, Results = [{_Id, {error, _Reason}}|_]) -> Results;
do_while_fun({Id, Fun, ArgSpecs}, Results) ->
    case apply(Fun, do_while_args(ArgSpecs, Results)) of
        ok -> [{Id, ok}|Results];
        {ok, Result} -> [{Id, {ok, Result}}|Results];
        {error, Reason} -> [{Id, {error, Reason}}|Results]
    end.

do_while_args(ArgSpecs, Results) ->
    [do_while_arg(ArgSpec, Results) || ArgSpec <- ArgSpecs].

do_while_arg({Id}, Results) when is_atom(Id) ->
    {Id, {ok, Result}} = lists:keyfind(Id, 1, Results),
    Result;

do_while_arg(Arg, _Results) -> Arg.

foldl_while(Fun, [H|T]) -> foldl_while(Fun(H), Fun, T);
foldl_while(_Fun, []) -> false.
foldl_while({ok, Value}, _Fun, _T) -> {ok, Value};
foldl_while(false, Fun, T) -> foldl_while(Fun, T).
