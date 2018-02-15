-module(kube_vxlan_controller_log).

-export([
    info/1,
    warning/1,
    error/1
]).

-define(LogFormat, "~s~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~3..0bZ ~p~n").

info(Term) -> log(info, Term).
warning(Term) -> log(warning, Term).
error(Term) -> log(error, Term).

log(Type, Term) ->
    Now = {_MegaSecs, _Secs, Mcs} = erlang:timestamp(),
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_universal_time(Now),
    Args = [log_type(Type), Y, Mo, D, H, Mi, S, Mcs div 1000, Term],
    io:format(?LogFormat, Args).

log_type(Type) -> case Type of
    info -> "I";
    warning -> "W";
    error -> "E"
end.
