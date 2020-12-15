-module(kube_vxlan_controller_k8s).

-export([uri_parse/1,
	 http_request/3, http_request/6,
	 ws_connect/3, ws_close/1, ws_recv/2]).

%% API
-export([start_link/1, open/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-define(DEBUG_OPTS, [{install, {fun logger_sys_debug:logger_gen_statem_trace/3, ?MODULE}}]).
-define(JsonDecodeOptions, [return_maps, {labels, atom}]).
-define(HttpStreamRecvTimeout, 60 * 1000). % milliseconds
-define(TIMEOUT, 5000).

start_link(Config) ->
    %%Opts = [{debug, ?DEBUG_OPTS}],
    Opts = [],
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [Config], Opts).

open() ->
    gen_statem:call(?SERVER, get).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> handle_event_function.

init([#{server := Server, ca_cert_file := CaCertFile}]) ->
    process_flag(trap_exit, true),

    {Host, Port} = uri_parse(Server),
    Opts = #{connect_timeout => ?TIMEOUT,
	     transport => tls,
	     tls_opts => [{cacertfile, CaCertFile}]
	    },
    {ok, ConnPid} = gun:open(Host, Port, Opts),
    ?LOG(info, "~p: connecting to ~s", [ConnPid, Server]),
    MRef = monitor(process, ConnPid),

    Data = #{conn => ConnPid, mref => MRef},
    {ok, init, Data}.

handle_event(info, {'DOWN', MRef, process, ConnPid, Reason}, _,
	     #{mref := MRef, conn := ConnPid}) ->
    ?LOG(info, "~p: terminated with ~p", [ConnPid, Reason]),
    gun:close(ConnPid),
    {stop, normal};

handle_event(info, {gun_up, ConnPid, _Protocol}, init, #{conn := ConnPid} = Data) ->
    ?LOG(info, "Command Channel UP Protocol: ~p", [_Protocol]),
    {next_state, up, Data};
handle_event(info, {gun_error, ConnPid, Reason}, _, #{conn := ConnPid}) ->
    ?LOG(error, "Connection Error: ~p", [Reason]),
    gun:close(ConnPid),
    {stop, normal};

handle_event({call, From}, get, up, #{conn := ConnPid}) ->
    {keep_state_and_data, [{reply, From, {ok, ConnPid}}]};
handle_event({call, _From}, get, _, _Data) ->
    {keep_state_and_data, [postpone]};

handle_event(_Ev, _Msg, _State, _Data) ->
    ?LOG(debug, "Ev: ~p, Msg: ~p, State: ~p", [_Ev, _Msg, _State]),
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%=========================================================================
%%%  internal functions
%%%=========================================================================

http_request(Resource, Query, Config) ->
    http_request(get, Resource, Query, [], [], Config).

http_request(Method, Resource, Query, RequestHeaders, RequestBody,
	     #{token := Token}) ->

    {ok, ConnPid} = open(),

    QueryStr = uri_string:compose_query(Query),
    Path = uri_string:recompose(#{path => Resource, query => QueryStr}),
    RequestOpts = #{reply_to => self()},

    StreamRef = gun:request(ConnPid, method(Method), Path,
			    headers(Token, RequestHeaders),
			    RequestBody, RequestOpts),
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
    Response.

ws_connect(Resource, Query,
	   #{server := Server, ca_cert_file := CaCertFile, token := Token}) ->
    {Host, Port} = uri_parse(Server),
    Opts = #{connect_timeout => ?HttpStreamRecvTimeout,
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
    ws_recv(ConnPid, StreamRef, #{}).

ws_recv(ConnPid, StreamRef, Output) ->
    case gun:await(ConnPid, StreamRef, 5000) of
	{ws, close} ->
	    {ok, Output};
	{ws, {binary, Bin}} ->
	    ws_recv(ConnPid, StreamRef, ws_append(Bin, Output));
	{ws, {close, _, Bin}} when is_binary(Bin) ->
	    {ok, ws_append(Bin, Output)};
	{ws, Frame} ->
	    ?LOG(warning, "unknown frame format: ~p", [Frame]),
	    {error, format};
	{error, _} = Error ->
	    Error
    end.

ws_close(ConnPid) ->
    gun:close(ConnPid).

ws_append(<<>>, Output) ->
    Output;
ws_append(<<Id:8, Msg/binary>>, Output) ->
    maps:update_with(Id, fun(X) -> <<X/binary, Msg/binary>> end, Msg, Output).

headers(Token, Headers) ->
    [{<<"authorization">>, iolist_to_binary(["Bearer ", Token])}|Headers].

method(Method) when is_atom(Method) ->
    list_to_binary(string:uppercase(atom_to_list(Method))).

uri_parse(Server) ->
    #{host := Host} = URI = uri_string:parse(Server),
    Port = uri_port(URI),
    {Host, Port}.

uri_port(#{port := Port}) ->
    Port;
uri_port(_) -> 443.
