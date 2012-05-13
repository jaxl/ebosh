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
%% Description: TODO: Add description to ebosh_cowboy
-module(ebosh_cowboy).
-behaviour(cowboy_http_handler).

%%
%% Include files
%%
-include_lib("ebosh_session.hrl").

%%
%% Exported Functions
%%
-export([init/3, handle/2, terminate/2]).
-export([start/0, stop/0]).

%%
%% API Functions
%%
start() ->
	Dispatch = [
    	%% {Host, list({Path, Handler, Opts})}
    	{'_', [{'_', ebosh_cowboy, []}]}
	],
	
	%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
	HttpPort = ebosh:get_env(http_port, 9696),
	cowboy:start_listener(get_srvr_name(HttpPort), 4,
    	cowboy_tcp_transport, [{port, HttpPort}],
    	cowboy_http_protocol, [{dispatch, Dispatch}]
	).

stop() ->
	HttpPort = ebosh:get_env(http_port, 9696),
	Pid = erlang:whereis(get_srvr_name(HttpPort)),
	cowboy:stop_listener(Pid),
	ok.

%%
%% Behaviour methods
%%
init({_Any, http}, Req, []) ->
	lager:debug("req inited"),
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(200, [], <<"Hello world!">>, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	lager:debug("req terminated"),
	ok.

%%
%% Local Functions
%%
get_srvr_name(Port) ->
	list_to_atom(atom_to_list(?MODULE) ++ "_" ++ integer_to_list(Port)).