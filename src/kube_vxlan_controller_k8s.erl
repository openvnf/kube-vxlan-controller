-module(kube_vxlan_controller_k8s).

-export([http_request/3, http_request/6,
	 ws_connect/3, ws_close/1, ws_recv/2]).

-include_lib("kernel/include/logger.hrl").

-define(JsonDecodeOptions, [return_maps, {labels, atom}]).

-define(HttpStreamRecvTimeout, 60 * 1000). % milliseconds

http_request(Resource, Query, Config) ->
    http_request(get, Resource, Query, [], [], Config).

http_request(Method, Resource, Query, RequestHeaders, RequestBody,
	     #{server := Server, ca_cert_file := CaCertFile, token := Token}) ->

    #{host := Host,
      port := Port} = uri_string:parse(Server),

    Opts = #{connect_timeout => ?HttpStreamRecvTimeout,
	     protocols => [http2],
	     transport => tls,
	     tls_opts => [{cacertfile, CaCertFile}]
	    },
    {ok, ConnPid} = gun:open(Host, Port, Opts),
    {ok, _Protocol} = gun:await_up(ConnPid),

    QueryStr = uri_string:compose_query(Query),
    Path = uri_string:recompose(#{path => Resource, query => QueryStr}),

    StreamRef = gun:request(ConnPid, method(Method), Path,
			    headers(Token, RequestHeaders), RequestBody),
    Response =
	case gun:await(ConnPid, StreamRef) of
	    {response, fin, Status, _Headers} ->
		{error, Status};
	    {response, nofin, 200, _Headers} ->
		{ok, Body} = gun:await_body(ConnPid, StreamRef),
		Doc = jsx:decode(Body, ?JsonDecodeOptions),
		{ok, Doc};
	    {response, nofin, Status, Headers} ->
		{ok, Body} = gun:await_body(ConnPid, StreamRef),
		{error, {Status, Headers, Body}}
	end,
    gun:close(ConnPid),
    Response.

ws_connect(Resource, Query,
	   #{server := Server, ca_cert_file := CaCertFile, token := Token}) ->
    #{host := Host,
      port := Port} = uri_string:parse(Server),

    Opts = #{connect_timeout => ?HttpStreamRecvTimeout,
	     protocols => [http],
	     transport => tls,
	     tls_opts => [{cacertfile, CaCertFile}]
	    },
    {ok, ConnPid} = gun:open(Host, Port, Opts),
    {ok, _Protocol} = gun:await_up(ConnPid),

    QueryStr = uri_string:compose_query(Query),
    Path = uri_string:recompose(#{path => Resource, query => QueryStr}),

    WsOpts =
	#{protocols =>
	      [{<<"v4.channel.k8s.io">>, gun_ws_h},
	       {<<"v3.channel.k8s.io">>, gun_ws_h},
	       {<<"v2.channel.k8s.io">>, gun_ws_h},
	       {<<"channel.k8s.io">>,    gun_ws_h}]},
    StreamRef = gun:ws_upgrade(ConnPid, Path, headers(Token, []), WsOpts),
    case gun:await(ConnPid, StreamRef, 5000) of
	{upgrade, [<<"websocket">>], _Headers} ->
	    {ok, ConnPid, StreamRef};
	{response, nofin, Status, _Headers} ->
	    {ok, Body} = gun:await_body(ConnPid, StreamRef),
	    ?LOG(debug, "WS upgrade failed with ~w: ~p", [Status, Body]),
	    gun:close(ConnPid),
	    {error, {Status, Body}};
	{response, fin, Status, _Headers} ->
	    ?LOG(debug, "WS upgrade failed with ~w", [Status]),
	    gun:close(ConnPid),
	    {error, {Status, <<>>}};
	_Other ->
	    ?LOG(debug, "WS upgrade failed with: ~p", [_Other]),
	    gun:close(ConnPid),
	    {error, failed}
    end.

ws_recv(ConnPid, StreamRef) ->
    ws_recv(ConnPid, StreamRef, []).

ws_recv(ConnPid, StreamRef, Acc) ->
    case gun:await(ConnPid, StreamRef, 5000) of
	{ws, close} ->
	    {ok, Acc};
	{ws, {binary, Bin}} ->
	    ws_recv(ConnPid, StreamRef, ws_append(Bin, Acc));
	{ws, {close, _, Bin}} when is_binary(Bin) ->
	    {ok, ws_append(Bin, Acc)};
	{ws, Frame} ->
	    ?LOG(warning, "unknown frame format: ~p", [Frame]),
	    {error, format};
	{error, _} = Error ->
	    Error
    end.

ws_close(ConnPid) ->
    gun:close(ConnPid).

ws_append(<<>>, Acc) ->
    Acc;
ws_append(<<Id:8, Msg/binary>>, Acc) ->
    Acc ++ [{Id, Msg}].

headers(Token, Headers) ->
    [{<<"authorization">>, iolist_to_binary(["Bearer ", Token])}|Headers].

method(Method) when is_atom(Method) ->
    list_to_binary(string:uppercase(atom_to_list(Method))).
