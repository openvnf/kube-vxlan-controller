-module('kube-vxlan-controller').

-export([main/1, run/0]).

-define(Host, "api.k8s.nce-01.fra-01.eu.cennso.net").
-define(WatchApi, "/api/v1/watch/pods?labelSelector=vxlan%3Dtrue").

%{ok, S} = rtmtb_api_usr_core:connect("https://api.k8s.nce-01.fra-01.eu.cennso.net/api/v1/namespaces/aalferov/pods/alpine1-594d4c669f-d27t4/exec?command=ls&command=-l&container=alpine1&container=alpine1&stderr=true&stdout=true", Headers), rtmtb_api_usr_core:recv(S).

-define(CaCertFile, "pki/ca.pem").
-define(TokenFile, "pki/token").

-define(A8nVxlanNames, 'vxlan.travelping.com/names').
-define(A8nVxlanNamesSep, ", \n").

-define(AgentSpec, #{
  spec => #{
    template => #{
      spec => #{
        containers => [
          #{name => "kube-vxlan-controller-agent",
            image => "aialferov/kube-vxlan-controller-agent"}
        ]
      }
    }
  }
}).

-define(LogFormat, "~s~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~3..0bZ ~p~n").

%curl -X PATCH --cacert pki/ca.pem https://api.k8s.nce-01.fra-01.eu.cennso.net/apis/apps/v1beta2/namespaces/aalferov/deployments/alpine1 -H "Content-Type: application/strategic-merge-patch+json" --header "Authorization: Bearer $(cat pki/token)" -d '{"spec":{"template":{"spec":{"containers":[{"name":"vxlan-controller-agent","image":"alpine"}]}}}}'

main(_) ->
    application:ensure_all_started(?MODULE),
    run().

run() -> run(?Host, ?TokenFile, ?CaCertFile).

run(Host, TokenFile, CaCertFile) ->
    Url = "https://" ++ Host ++ ?WatchApi,
    {ok, Token} = file:read_file(TokenFile),

    Headers = [{"Authorization", "Bearer " ++ binary_to_list(Token)}],
    Options = [
        {ssl_options, [{cacertfile, CaCertFile}]},
        {recv_timeout, infinity}
    ],
    
    case hackney:request(get, Url, Headers, <<>>, Options) of
        {ok, 200, _, Ref} -> run_loop(Ref, <<>>);
        Other -> log(error, Other)
    end.

run_loop(Ref, IncompleteData) ->
    case hackney:stream_body(Ref) of
        done -> ok;
        {ok, NewData} ->
            Data = <<IncompleteData/binary, NewData/binary>>,
            run_loop(Ref, process_stream(binary:split(Data, <<"\n">>)));
        {error, Reason} ->
            log(error, Reason)
    end.

process_stream([Item|T]) when T /= [] ->
    process_item(Item),
    process_stream(T);

process_stream([<<>>]) -> <<>>;
process_stream([Item]) ->
    case binary:last(Item) == $\n of
        true -> process_item(Item), <<>>;
        false -> Item
    end.

process_item(Item) ->
    Json = <<"[", Item/binary, "]">>,
    Event = hd(jsx:decode(Json, [return_maps, {labels, atom}])),
    log(info, event_read(Event)).

event_read(Event = #{
    type := Type,
    object := #{
        metadata := #{
            uid := Uid,
            annotations := Annotations
        },
        status := Status
    }
}) ->
    VxlanNames = btl(maps:get(?A8nVxlanNames, Annotations, <<>>)),
    Event.
%   {list_to_atom(string:lowercase(btl(Type))),
%    btl(Uid),
%    btl(maps:get(podIP, Status, <<>>)),
%    string:lexemes(VxlanNames, ?A8nVxlanNamesSep)}.

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

btl(B) -> binary_to_list(B).
