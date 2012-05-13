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

%%% -------------------------------------------------------------------
%%% Author  : abhinavsingh
%%% Description :
%%%
%%% Created : May 12, 2012
%%% -------------------------------------------------------------------
-module(ebosh_stream).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-define(CONNECT_TIMEOUT, 10000).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, connect/1]).
-export([send/2, reset_parser/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, 
{
	id			= undefined :: undefined | string(),
	full_jid 	= undefined :: undefined | string(),
	domain 		= undefined,
	host 		= undefined,
	port 		= 5222		:: undefined | integer(),
	cb 			= undefined :: undefined | pid(),
	session		= undefined :: undefined | pid(),
	socket		= undefined
}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Domain) ->
	start_link(Domain, undefined).
start_link(Domain, Route) ->
	gen_server:start_link(?MODULE, [Domain, Route, self()], []).

stop(Pid) ->
	Pid ! stop.

connect(Pid) ->
	gen_server:call(Pid, connect).

send(Pid, Xml) ->
	gen_server:call(Pid, {send, Xml}).

reset_parser(Pid) ->
	Pid ! reset_parser.

%% ====================================================================
%% Server functions
%% ====================================================================
init([Domain, _Route, Cb]) ->
	process_flag(trap_exit, true),
	Session = exmpp_session:start_link({1,0}),
	[{Host, Port}|_] = exmpp_dns:get_c2s(Domain),
	lager:debug("initialized exmpp session ~p", [Session]),
    {ok, #state{domain=Domain,cb=Cb,host=Host,port=Port,session=Session}}.

handle_call(connect, _From, State=#state{session=Session,host=Host,port=Port,domain=Domain}) ->
	Opts = [{domain,Domain},{starttls,enabled},{compression,enabled},{timeout,?CONNECT_TIMEOUT}],
	try exmpp_session:connect_TCP(Session, Host, Port, Opts) of
		{ok, Id, Features} ->
			Socket = exmpp_session:get_property(Session, connection_ref),
			lager:debug("exmpp session ~p connected with conn ref ~p", [Session, Socket]),
			{reply, {ok, Id, Features}, State#state{id=Id,socket=Socket}}
	catch
		_:_ ->
			exmpp_session:stop(Session),
			{stop, 'connect_error', 'connect_error', State}
	end;

handle_call({send, XmlEl}, _From, State=#state{socket=Socket}) when is_record(XmlEl, xmlElement) ->
	[_, Stanza] = xmerl:export_simple([XmlEl], xmerl_xml),
	Bin = lists:flatten(Stanza),
	exmpp_internals:gen_send(Socket, Bin),
	lager:debug("relaying ~p to underlying stream", [Bin]),
	{reply, ok, State};
handle_call({send, XmlEl}, _From, State=#state{session=Session}) when is_record(XmlEl, xmlel) ->
	exmpp_session:send_packet(Session, XmlEl),
	lager:debug("relaying ~p to underlying stream", [XmlEl]),
	{reply, ok, State};

handle_call(Request, _From, State) ->
	lager:debug("unhandled call ~p", [Request]),
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	lager:debug("unhandled cast msg ~p", [Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(stop, State=#state{session=Session}) ->
	exmpp_session:stop(Session),
	{stop, stopped, State};

handle_info(reset_parser, State=#state{session=Session}) ->
	exmpp_session:reset_parser(Session),
	{noreply, State};

%% handle stream restart response, not relayed to callback process
handle_info(_Pkt = #received_packet{packet_type=stream}, State) ->
	lager:debug("got stream start response"),
	{noreply, State};

handle_info(#received_packet{} = Pkt, State=#state{cb=Cb,id=Id}) ->
	lager:debug("got pkt ~p", [Pkt]),
	Cb ! {Id, Pkt},
	{noreply, State};

handle_info(Info, State) ->
	lager:debug("unhandled info ~p", [Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
	lager:debug("unhandled terminate with reason ~p", [Reason]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

