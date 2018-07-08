-module(kube_vxlan_controller_k8s).

-export([
    http_request/3,
    http_request/6,

    http_stream_request/3,
    http_stream_read/1,

    ws_connect/3,
    ws_close/1,
    ws_recv/1
]).

-define(JsonDecodeOptions, [return_maps, {labels, atom}]).

-define(HttpStreamRecvTimeout, 60 * 1000). % milliseconds

http_request(Resource, Query, Config) ->
    http_request(get, Resource, Query, [], <<>>, Config).

http_request(Method, Resource, Query, RequestHeaders, RequestBody, _Config = #{
    server := Server,
    ca_cert_file := CaCertFile,
    token := Token
}) ->
    Url = url(Server, Resource, Query),
    Options = http_options(CaCertFile),

    case hackney:request(
        Method, Url, RequestHeaders ++ headers(Token),
        RequestBody, Options
    ) of
        {ok, 200, _Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {ok, jsx:decode(<<"[", Body/binary, "]">>, ?JsonDecodeOptions)};
        {ok, Code, Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {error, {Code, Headers, Body}};
        {error, Reason} -> {error, Reason}
    end.

http_stream_request(Resource, Query, _Config = #{
    server := Server,
    ca_cert_file := CaCertFile,
    token := Token
}) ->
    Url = url(Server, Resource, Query),
    Options = http_options(CaCertFile),

    case hackney:request(get, Url, headers(Token), <<>>, Options) of
        {ok, 200, _Headers, Ref} -> {ok, Ref};
        {ok, Code, Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {error, {Code, Headers, Body}};
        {error, Reason} -> {error, Reason}
    end.

http_stream_read(Stream) -> http_stream_read(Stream, false).

http_stream_read(Stream, DecodeFun) ->
    case hackney:stream_body(Stream) of
        done -> {ok, done};
        {ok, Data} -> http_stream_to_json(Stream, Data, DecodeFun);
        {error, Reason} -> {error, Reason}
    end.

http_stream_to_json(_Stream, <<>>, false) -> {ok, []};
http_stream_to_json(Stream, Data, false) ->
    {incomplete, DecodeFun} = jsx:decode(<<"[">>, [stream|?JsonDecodeOptions]),
    http_stream_to_json(Stream, Data, DecodeFun);

http_stream_to_json(Stream, <<>>, DecodeFun) ->
    http_stream_read(Stream, DecodeFun);

http_stream_to_json(Stream, Data, DecodeFun) ->
    IsComplete = binary:last(Data) == $\n,
    DecodableData = binary:replace(Data, <<"\n">>, <<",">>, [global]),
    {incomplete, NewDecodeFun} = DecodeFun(DecodableData),

    case IsComplete of
        true ->
            {incomplete, F} = NewDecodeFun(<<"]">>),
            {ok, F(end_stream)};
        false ->
            http_stream_read(Stream, NewDecodeFun)
    end.

ws_connect(Resource, Query, _Config = #{
    server := Server,
    ca_cert_file := CaCertFile,
    token := Token
}) ->
    Url = url(Server, Resource, Query),
    ewsc:connect(Url, headers(Token), ws_options(CaCertFile)).

ws_close(Socket) -> ewsc:close(Socket).

ws_recv(Socket) -> ws_recv(Socket, <<"">>).

ws_recv(Socket, Acc) ->
    case ewsc:recv(Socket, 5000) of
        {ok, [close|Messages]} ->
            {ok, binary_to_list(ws_append_messages(Acc, Messages))};
        {ok, Messages} ->
            ws_recv(Socket, ws_append_messages(Acc, Messages));
        {error, Reason} ->
            {error, Reason}
    end.

ws_append_messages(Messages, []) -> Messages;
ws_append_messages(Messages, NewMessages) ->
    NewMessagesStripped = [M || <<_, M/binary>> <- NewMessages],
    <<Messages/binary, (iolist_to_binary(NewMessagesStripped))/binary>>.

url(Server, Resource, Query) ->
    Server ++ Resource ++ url_query(Query).

url_query([]) -> "";
url_query(Query) ->
    [$?|lists:flatten(lists:join($&, lists:map(fun url_query_param/1, Query)))].

url_query_param({Key, Value}) ->
    [http_uri:encode(Key)] ++ [$=|http_uri:encode(Value)].

headers(Token) -> [
    {"Authorization", "Bearer " ++ Token}
].

http_options(CaCertFile) -> [
    {ssl_options, [{cacertfile, CaCertFile}]},
    {recv_timeout, ?HttpStreamRecvTimeout}
].

ws_options(CaCertFile) -> [
    {cacertfile, CaCertFile}
].
