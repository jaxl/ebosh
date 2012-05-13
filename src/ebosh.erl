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
%% Created: May 11, 2012
%% Description: TODO: Add description to ebosh
-module(ebosh).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, stop/0]).
-export([get_env/2, ping/1]).

%%
%% API Functions
%%
start() ->
	application:start(lager),
	application:start(crypto),
	application:start(ssl),
	application:start(inets),
	application:start(cowboy),
	application:start(exmpp),
	application:start(ebosh).

stop() ->
	application:stop(ebosh),
	application:stop(exmpp),
	application:stop(cowboy),
	application:stop(inets),
	application:stop(ssl),
	application:stop(crypto),
	application:stop(lager).

get_env(Key, Default) ->
	case application:get_env(Key) of
		undefined -> Default;
		{ok, Any} -> Any
	end.

ping(Node) ->
	case net_adm:ping(Node) of
		pong ->
			lager:debug("node ~p is up", [Node]),
			pong;
		pang ->
			lager:debug("node ~p is currently unreachable", [Node]),
			pang
	end.

%%
%% Local Functions
%%

