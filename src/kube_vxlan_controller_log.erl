-module(kube_vxlan_controller_log).
-compile({no_auto_import,[error/2]}).

-export([
    info/1, info/2,
    warning/1, warning/2,
    error/1, error/2
]).

-define(LogFormat, "~s~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~3..0bZ ~s~n").
-define(TermFormat, "~p").

info(Term) -> info(?TermFormat, [Term]).
info(Format, Args) -> log(info, Format, Args).

warning(Term) -> warning(?TermFormat, [Term]).
warning(Format, Args) -> log(warning, Format, Args).

error(Term) -> error(?TermFormat, [Term]).
error(Format, Args) -> log(error, Format, Args).

log(Type, Format, Args) ->
    Now = {_MegaSecs, _Secs, Mcs} = erlang:timestamp(),
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_universal_time(Now),
    LogArgs = [log_type(Type), Y, Mo, D, H, Mi, S, Mcs div 1000,
               lists:flatten(io_lib:format(Format, Args))],
    io:format(?LogFormat, LogArgs).

log_type(Type) -> case Type of
    info -> "I";
    warning -> "W";
    error -> "E"
end.
