%%
%% ebosh (Bosh Connection Manager in Erlang)
%%
%% Copyright (c) 2011-2012, Abhinav Singh <me@abhinavsingh.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%

%% Author: abhinavsingh
%% Created: May 12, 2012
%% Description: TODO: Add description to ebosh_web_srvr
-module(ebosh_web).

%%
%% Include files
%%
-include_lib("ebosh_session.hrl").

%%
%% Exported Functions
%%
-export([start/0, stop/0, loop/1]).

%%
%% API Functions
%%
start() ->
	HttpPort = ebosh:get_env(http_port, 9696),
	HttpConf = [{ip, ebosh:get_env(http_ip, "127.0.0.1")}, {port, HttpPort}],
	SSLOpts = [{certfile, ebosh:get_env(https_certfile, "priv/cert/server_cert.pem")}, {keyfile, ebosh:get_env(https_keyfile, "priv/cert/server_key.pem")}],
	HttpsPort = ebosh:get_env(https_port, 9697),
	HttpsConf = [{ip, ebosh:get_env(https_ip, "127.0.0.1")}, {port, HttpsPort}, {ssl, true}, {ssl_opts, SSLOpts}],
	Loop = fun(Req) -> ?MODULE:loop(Req) end,
	{ok, _Http} = mochiweb_http:start([{name, get_srvr_name(HttpPort)}, {loop, Loop} | HttpConf]),
	{ok, _Https} = mochiweb_http:start([{name, get_srvr_name(HttpsPort)}, {loop, Loop} | HttpsConf]).

stop() ->
	HttpPort = ebosh:get_env(http_port, 9696),
	HttpsPort = ebosh:get_env(https_port, 9697),
	mochiweb_http:stop(get_srvr_name(HttpPort)),
	mochiweb_http:stop(get_srvr_name(HttpsPort)).

get_srvr_name(Port) ->
	list_to_atom(atom_to_list(?MODULE) ++ "_" ++ integer_to_list(Port)).

loop(Req) ->
	HttpBindPath = ebosh:get_env(http_bind_path, "/http-bind"),
	case Req:get(method) of
		'POST' ->
			case Req:get(path) of
				HttpBindPath ->
					Body = Req:recv_body(),
					ResFun = fun(BoshPid) -> wait_for_bosh_response(Req, BoshPid) end,
					ErrFun = fun(Res) -> Req:respond(Res) end,
					ebosh_session:stream(Body, ResFun, ErrFun);
				_Any ->
					lager:debug("got ~p", [Req]),
					Req:respond({501, [], []})
			end;
		'GET' ->
			lager:debug("got ~p", [Req]),
			Req:respond({501, [], []});
		'HEAD' ->
			lager:debug("got ~p", [Req]),
			Req:respond({501, [], []});
		'OPTIONS' ->
			lager:debug("got ~p", [Req]),
			Req:respond({501, [], []});
		_ ->
			lager:debug("got ~p", [Req]),
			Req:respond({501, [], []})
	end.

%%
%% Local Functions
%%

wait_for_bosh_response(Req, BoshPid) ->
	Socket = Req:get(socket),
	ok = mochiweb_socket:setopts(Socket, [{active, once}]),
	
	receive
		{?EBOSH_RESPONSE_MSG, Header, Body} ->
			lager:debug("got response body ~p", [Body]),
			Req:respond({200, Header, Body});
		{tcp_closed, Socket} ->
			lager:debug("client closed connection"),
			mochiweb_socket:close(Socket),
			exit(normal);
		Any ->
			lager:debug("rcvd unhandled ~p", [Any]),
			wait_for_bosh_response(Req, BoshPid)
	end.

