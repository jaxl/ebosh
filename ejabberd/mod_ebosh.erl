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

%%%----------------------------------------------------------------------
%%% File    : mod_ebosh.erl
%%% Author  : Abhinav Singh <me [at] abhinavsingh [dot] com>
%%% Purpose : ejabberd adapter for ebosh
%%% Created :
%%% Id      :
%%%----------------------------------------------------------------------

-module(mod_ebosh).

-define(ejabberd_debug, true).

-behaviour(gen_mod).

-export([
    start/2,
    stop/1,
    process/2
    ]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

-define(EBOSH_NODE, 'ebosh@localhost').

%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

process(LocalPath, Req) ->
	?DEBUG("got req ~p on local path ~p", [Req, LocalPath]),
	case Req#request.method of
		'POST' ->
			stream(Req);
		'GET' ->
			?DEBUG("got ~p", [Req]),
			{501, [], []};
		'HEAD' ->
			?DEBUG("got ~p", [Req]),
			{501, [], []};
		'OPTIONS' ->
			?DEBUG("got ~p", [Req]),
			{501, [], []};
		_ ->
			?DEBUG("got ~p", [Req]),
			{501, [], []}
	end.

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------

start(Host, _Opts) ->
	?DEBUG("mod ebosh starting for host ~p", [Host]),
	ok.

stop(Host) ->
	?DEBUG("mod ebosh stopping for host ~p", [Host]),
	ok.

%%%----------------------------------------------------------------------
%%% INTERNAL METHODS
%%%----------------------------------------------------------------------

stream(Req) ->
	Body = Req#request.data,
	
	ResFun = 
		fun({RCode, RHeader, RBody}) ->
				{RCode, RHeader, RBody}
		end,
	
	SetOptFun =
		fun(_Opts) ->
				ok
		end,
	
	case catch rpc:call(?EBOSH_NODE, ebosh_http, stream, [Body, ResFun, SetOptFun], infinity) of
		{badrpc, Reason} ->
			?DEBUG("rpc failed reason ~p", [Reason]),
			{500, [], []};
		{C, H, B} ->
			?DEBUG("rpc call succeeded with ret ~p", [B]),
			{C, H, B}
	end.
