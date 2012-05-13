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

