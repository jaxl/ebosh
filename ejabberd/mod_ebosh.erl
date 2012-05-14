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

-include_lib("ebosh_session.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

process(["http-ebosh-bind"], Req) ->
	?DEBUG("got req ~p", [Req]),
	case Req#request.method of
		"POST" ->
			Body = Req#request.data,
			?DEBUG("got req body ~p", [Body]),
			ResFun = fun(BoshPid) -> wait_for_bosh_response(Req, BoshPid) end,
			ErrFun = fun(Res) -> Res end,
			ebosh_session:stream(Body, ResFun, ErrFun);
		"GET" ->
			?DEBUG("got ~p", [Req]),
			{501, [], []};
		"HEAD" ->
			?DEBUG("got ~p", [Req]),
			{501, [], []};
		"OPTIONS" ->
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
    ebosh:start().

stop(Host) ->
	?DEBUG("mod ebosh stopping for host ~p", [Host]),
    ebosh:stop().

%%%----------------------------------------------------------------------
%%% INTERNAL METHODS
%%%----------------------------------------------------------------------

wait_for_bosh_response(Req, BoshPid) ->
	receive
		{?EBOSH_RESPONSE_MSG, Header, Body} ->
			?DEBUG("got response body ~p", [Body]),
			{200, Header, Body};
		{tcp_closed, _Socket} ->
			?DEBUG("client closed connection", []),
			exit(normal);
		Any ->
			?DEBUG("rcvd unhandled ~p", [Any]),
			wait_for_bosh_response(Req, BoshPid)
	end.