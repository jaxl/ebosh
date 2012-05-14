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
%% Description: TODO: Add description to ebosh_http
-module(ebosh_http).

-include_lib("ebosh_session.hrl").

-export([stream/3]).

%% @doc stream raw Body string
%%		This must be called directly from web process adapter modules
%%		Body is rcvd raw bosh <body/> string
%%		ResFun/1 accepts underlying ebosh_session BoshPid as argument
%%		ErrFun/1 if Body string is an invalid bosh body pkt, 
%%		ErrFun({Code, Header, Body}) will be called
stream(Body, ResFun, SetOptFun) when is_function(ResFun) andalso is_function(SetOptFun) ->
	BodySize = iolist_size(Body),
	case ebosh_session:parse_body(Body) of
		error ->
			lager:debug("invalid xml pkt ~p", [Body]),
			ResFun({400, [], []});
		XmlEl ->
			lager:debug("rcvd ~p", [Body]),
			case ebosh_session:is_valid_session_start_pkt(XmlEl) of
				true ->
					%% valid session start pkt
					Sid = ebosh_session:gen_sid(),
					{ok, BoshPid} = ebosh_session:start_super(Sid),
					ebosh_session:stream(Sid, XmlEl, BodySize),
					wait_for_bosh_response(BoshPid, ResFun, SetOptFun);
				false ->
					case ebosh_session:get_attr(XmlEl, "sid") of
						undefined ->
							lager:debug("invalid session start pkt and sid not found in pkt ~p", [Body]),
							ResFun({400, [], []});
						Sid ->
							%% not a session start pkt and sid found
							case ebosh_session:is_alive(Sid) of
								false ->
									lager:debug("sent sid ~p session not found", [Sid]),
									ResFun({400, [], []});
								BoshPid ->
									ebosh_session:stream(Sid, XmlEl, BodySize),
									wait_for_bosh_response(BoshPid, ResFun, SetOptFun)
							end
					end
			end
	end.

wait_for_bosh_response(BoshPid, ResFun, SetOptFun) ->
	SetOptFun([{active, once}]),
	receive
		{?EBOSH_RESPONSE_MSG, BoshPid, Header, Body} ->
			lager:debug("got response body ~p", [Body]),
			ResFun({200, Header, Body});
		{tcp_closed, _Socket} ->
			lager:debug("client closed connection"),
			exit(normal);
		Any ->
			lager:debug("rcvd unhandled ~p", [Any]),
			wait_for_bosh_response(BoshPid, ResFun, SetOptFun)
	end.
