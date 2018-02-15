-module(kube_vxlan_controller_ws).

-export([
    connect/1, connect/2, connect/3, disconnect/1,
    send/2, recv/1
]).

-define(Utils, kube_vxlan_controller_utils).

-include_lib("wsock/include/wsock.hrl").

-define(SendTimeout, 30 * 1000). % milliseconds
-define(RecvTimeout, 30 * 1000). % milliseconds

-define(ConnectOptions, [
    binary,
    {active, false},
    {keepalive, true},
    {reuseaddr, true},
    {send_timeout, ?SendTimeout},
    {send_timeout_close, true}
]).

connect(Url) -> connect(Url, [], []).
connect(Url, Headers) -> connect(Url, Headers, []).

connect(Url, Headers, Options) when is_list(Url) ->
    case http_uri:parse(Url) of
        {ok, Result} -> connect(Result, Headers, Options);
        {error, Reason} -> {error, Reason}
    end;

connect({Scheme, _UserInfo, Host, Port, Path, Query}, Headers, Options) ->
    connect({tcp_module(Scheme), Host, Port, Path, Query}, Headers, Options);

connect({Tcp, Host, Port, Path, Query}, Headers, Options) ->
    ConnectOptions = ?ConnectOptions ++ Options,
    ?Utils:do_while([
        {connect, fun Tcp:connect/3, [Host, Port, ConnectOptions]},
        {open, fun wsock_handshake:open/3, [Path ++ Query, Host, Port]},
        {encode, fun wsock_http_encode/2, [{open}, Headers]},
        {send, fun Tcp:send/2, [{connect}, {encode}]},
        {recv, fun Tcp:recv/2, [{connect}, 0]},
        {decode, fun wsock_http:decode/2, [{recv}, response]},
        {handle, fun wsock_handshake:handle_response/2, [{decode}, {open}]},
        {socket, fun(Socket) -> {ok, Socket} end, [{connect}]}
    ]).

disconnect(Socket) -> (tcp_module(Socket)):close(Socket).

send(Socket, Packet) ->
    case (tcp_module(Socket)):send(Socket, encode(Packet)) of
        ok -> ok;
        {error, Reason} -> {error, {send, Reason}}
    end.

recv(Socket) -> recv(Socket, [], false).

recv(Socket, RecvedPackets, FragmentedMessage) ->
    case (tcp_module(Socket)):recv(Socket, 0, ?RecvTimeout) of
        {ok, Binary} -> recv(Socket, Binary, RecvedPackets, FragmentedMessage);
        {error, Reason} -> {error, {recv, Reason}}
    end.

recv(Socket, Binary, RecvedPackets, FragmentedMessage) ->
    case decode(Binary, RecvedPackets, FragmentedMessage) of
        {ok, Packets} -> {ok, Packets};
        {ok, Packets, NextFragmentedMessage} ->
            recv(Socket, Packets, NextFragmentedMessage);
        {error, closed} -> {error, {recv, ws_closed}}
    end.

encode(Packet) ->
    wsock_message:encode(Packet, [mask, text]).

decode(Binary, IncompletePackets, FragmentedMessage) ->
    Messages = decode_binary(Binary, FragmentedMessage),
    decode_messages(Messages, lists:reverse(IncompletePackets)).

decode_binary(Binary, false) -> wsock_message:decode(Binary, [mask, text]);
decode_binary(Binary, FragmentedMessage) ->
    wsock_message:decode(Binary, FragmentedMessage, [mask, text]).

decode_messages([#message{payload = Payload, type = Type}|Messages], Packets)
    when Type == binary; Type == text
->
    decode_messages(Messages, [Payload|Packets]);

decode_messages([Message = #message{type = fragmented}], Packets) ->
    {ok, lists:reverse(Packets), Message};

decode_messages([#message{type = close}], Packets) ->
    {ok, {close, lists:reverse(Packets)}};

decode_messages([], Packets) -> {ok, lists:reverse(Packets)}.

wsock_http_encode(HandshakeRequest, Headers) ->
    Message = HandshakeRequest#handshake.message,
    {ok, wsock_http:encode(Message#http_message{
        headers = Message#http_message.headers ++ Headers
    })}.

tcp_module(Scheme) when is_atom(Scheme) ->
    case Scheme of
        Scheme when Scheme == https; Scheme == wss -> ssl;
        Scheme -> gen_tcp
    end;

tcp_module(_Socket = {sslsocket, _, _}) -> ssl;
tcp_module(Socket) when is_port(Socket) -> gen_tcp.
