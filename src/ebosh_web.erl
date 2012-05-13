%%
%% ebosh (Bosh Connection Manager in Erlang)
%%
%% Copyright (c) 2011-2012, Abhinav Singh <me@abhinavsingh.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% * Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% * Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in
%% the documentation and/or other materials provided with the
%% distribution.
%%
%% * Neither the name of Abhinav Singh nor the names of his
%% contributors may be used to endorse or promote products derived
%% from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRIC
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
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
	BoshPath = ebosh:get_env(bosh_path, "/http-bind"),
	case Req:get(method) of
		'POST' ->
			case Req:get(path) of
				BoshPath ->
					Body = Req:recv_body(),
					BodySize = iolist_size(Body),
					case ebosh_session:parse_body(Body) of
						error ->
							lager:debug("invalid xml pkt ~p", [Body]),
							Req:respond({400, [], []});
						XmlEl ->
							lager:debug("rcvd ~p", [Body]),
							case ebosh_session:is_valid_session_start_pkt(XmlEl) of
								true ->
									%% valid session start pkt
									Sid = ebosh_session:gen_sid(),
									{ok, BoshPid} = ebosh_session:start_link(Sid),
									ebosh_session:stream(Sid, XmlEl, BodySize),
									wait_for_bosh_response(Req, BoshPid);
								false ->
									case ebosh_session:get_attr(XmlEl, "sid") of
										undefined ->
											lager:debug("invalid session start pkt and sid not found in pkt ~p", [Body]),
											Req:respond({400, [], []});
										Sid ->
											%% not a session start pkt and sid found
											case ebosh_session:is_alive(Sid) of
												false ->
													lager:debug("sent sid ~p session not found", [Sid]),
													Req:respond({400, [], []});
												BoshPid ->
													ebosh_session:stream(Sid, XmlEl, BodySize),
													wait_for_bosh_response(Req, BoshPid)
											end
									end
							end
					end;
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

