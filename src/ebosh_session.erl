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
%%%		Web process will start a new bosh session by calling:
%%%		ebosh_session:start_link(Sid)
%%%
%%%		Once started, web process can send bosh packets for processing by calling:
%%%		ebosh_session:stream(Sid, Body), 
%%%		where Body is raw bosh pkt received via http post or other medium
%%%
%%%		Also ebosh_session:stream(Sid, XmlEl) can be called where
%%%		XmlEl = #xmlElement{} obtained by calling ebosh_session:parse_body(Body)
%%%
%%%		Before calling ebosh_session:start_link(Sid), Web process will have to 
%%%		detect new bosh session start requests and generate a new session id by
%%%		calling ebosh_session:gen_sid()
%%%
%%%		New bosh session start requests can be detected in following fashion:
%%%		a) parse incoming bosh body inside web process by calling
%%%		   ebosh_session:parse_body(Body)
%%%		b) test if parsed XmlEl is a valid session start request by calling:
%%%		   ebosh_session:is_valid_session_start_pkt(XmlEl)
%%%		c) if pkt is a valid session start request, generate a new session id
%%%		   by calling ebosh_session:gen_sid() and finally start a new bosh
%%%		   session by calling ebosh_session:start_link(Sid)
%%%		d) Immediately after starting bosh session, send session start pkt for
%%%		   processing by bosh process by calling:
%%%		   ebosh_session:stream(Sid, XmlEl)
%%%		
%%% Created : May 11, 2012
%%% -------------------------------------------------------------------
-module(ebosh_session).

-behaviour(gen_fsm).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("ebosh_session.hrl").

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------
-export([start_link/1, stream/2, stream/3]).
-export([parse_body/1, gen_sid/0, is_valid_session_start_pkt/1, get_attr/2, gen_attrs/1]).
-export([is_alive/1, get_proc_name/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------
-type state() :: atom.
-type sid() :: string().
-type rid() :: integer().
-type stream_id() :: string().
-type xml_pkt() :: #xmlElement{} | #received_packet{}.

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
%% in bytes/seconds
-define(MAX_IN_RATE, 2000).
-define(MAX_OUT_RATE, 20000).

%% CORS headers
-define(CT_XML, {'Content-Type', "text/xml; charset=utf-8"}).
-define(CT_PLAIN, {'Content-Type', "text/plain"}).
-define(AC_ALLOW_ORIGIN, {'Access-Control-Allow-Origin', "*"}).
-define(AC_ALLOW_METHODS, {'Access-Control-Allow-Methods', "GET, POST, OPTIONS"}).
-define(AC_ALLOW_HEADERS, {'Access-Control-Allow-Headers', "Content-Type"}).
-define(AC_MAX_AGE, {'Access-Control-Max-Age', "86400"}).
-define(HEADER, [?CT_XML, ?AC_ALLOW_ORIGIN, ?AC_ALLOW_HEADERS]).

%% timeout messages
-define(MAX_INACTIVITY_TIMEOUT_MSG, max_inactivity_timeout).

%% exmpp parser default options
-define(PARSER_OPTS, 
[
	{names_as_atom,true}, 
	{check_nss,xmpp}, 
	{check_elems,xmpp}, 
	emit_endtag, 
	{root_depth,0}, 
	{max_size,infinity}
]).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(stream , 
{
	to			= undefined :: undefined | string(),
	lang		= "en"		:: string(),
	route		= undefined :: undefined | string(),
	from		= undefined :: undefined | string(),
	version		= undefined :: undefined | float(),
	pid			= undefined :: undefined | pid()
}).

-record(state, 
{
	sid					= undefined :: undefined | sid(),
	rid					= undefined :: undefined | rid(),
	ver					= undefined :: undefined | float(),
	content_type		= ?CT_XML	:: string(),
	max_wait 			= 120		:: integer(),
	wait_tref			= undefined	:: undefined,
	max_inactivity 		= 120		:: integer(),
	inactivity_tref		= undefined	:: undefined,
	max_hold			= 1			:: integer(),
	min_polling 		= 2			:: integer(),
	max_pause 			= 120		:: integer(),
	ack					= false		:: boolean(),
	last_key			= undefined	:: undefined | string(),
	rcvr_pids 			= []		:: [] | [pid(), ...],
	streams 			= []		:: [] | [{stream_id(), #stream{}}, ...],
	buffer 				= []		:: [] | [{stream_id(), xml_pkt()}, ...],
	in_rate				= undefined	:: undefined,
	out_rate			= undefined	:: undefined,
	in_bytes			= 0			:: integer(),
	out_bytes			= 0			:: integer()
}).

%% ====================================================================
%% External functions
%% ====================================================================
-spec start_link(sid()) -> {ok, pid()}.
start_link(Sid) ->
	ProcName = get_proc_name(Sid),
	gen_fsm:start_link({global, ProcName}, ?MODULE, [Sid], []).

-spec stream(sid(), string()) -> ok | error.
stream(Sid, Body) ->
	case parse_body(Body) of
		error ->
			error;
		XmlEl ->
			stream(Sid, XmlEl, iolist_size(Body))
	end.

-spec stream(sid(), #xmlElement{}, integer()) -> ok.
stream(Sid, XmlEl=#xmlElement{}, BodySize) ->
	ProcName = get_proc_name(Sid),
	gen_fsm:send_all_state_event({global, ProcName}, {stream, XmlEl, BodySize, self()}).

-spec parse_body(binary() | string()) -> error | #xmlElement{}.
parse_body(Body) when is_binary(Body) ->
	parse_body(binary_to_list(Body));
parse_body(Body) ->
	try xmerl_scan:string(Body) of
		{XmlEl, []} ->
			XmlEl;
		{_XmlEl, _Rest} ->
			error
	catch
		_Class:_Exception ->
			error
	end.

-spec is_valid_session_start_pkt(#xmlElement{}) -> boolean().
is_valid_session_start_pkt(XmlEl) ->
	Sid = get_attr(XmlEl, "sid"),
	To = get_attr(XmlEl, "to"),
	Lang = get_attr(XmlEl, "xml:lang"),
	Ver = get_attr(XmlEl, "ver"),
	Wait = get_attr(XmlEl, "wait"),
	Hold = get_attr(XmlEl, "hold"),
	
	if Sid =:= undefined 
		 andalso To =/= undefined
		 andalso Lang =/= undefined
		 andalso Ver =/= undefined
		 andalso Wait =/= undefined
		 andalso Hold =/= undefined ->
		   true;
	   true ->
		   false
	end.

-spec gen_sid() -> sid().
gen_sid() ->
	{Mega, Secs, Micro} = now(),
	TimeString = io_lib:format("~p:~p:~p", [Mega, Secs, Micro]),
	<<Digest:160/big-unsigned-integer>> = crypto:sha(TimeString),
	lists:flatten(io_lib:format("~40.16.0b", [Digest])).

-spec get_proc_name(sid()) -> atom.
get_proc_name(Sid) ->
	list_to_atom(atom_to_list(?MODULE) ++ "_" ++ Sid).

-spec is_alive(sid()) -> false | pid().
is_alive(Sid) ->
	case global:whereis_name(get_proc_name(Sid)) of
		undefined -> false;
		Pid -> Pid
	end.

-spec get_attr(#xmlElement{}, string()) -> undefined | string().
get_attr(XmlEl, Attr) ->
	case xmerl_xpath:string("//@"++Attr, XmlEl) of
		[XmlAttr] -> XmlAttr#xmlAttribute.value;
		_ -> undefined
	end.

gen_attrs(Attrs) ->
	[begin 
		 case KV of
			 {K,V} ->
				 K1 = to_binary(K),
				 V1 = to_binary(V),
				 #xmlattr{name = K1, value = V1};
			 {K, NS, V} ->
				 K1 = to_binary(K),
				 V1 = to_binary(V),
				 #xmlattr{name = K1, ns = NS, value = V1}
		 end 
	 end || KV <- Attrs].

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> to_binary(atom_to_list(A));
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I)).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec init([]) -> {ok, state(), #state{}} | 
					  {ok, state(), #state{}, integer()} | 
					  ignore | 
					  {stop, term()}.
init([Sid]) ->
	process_flag(trap_exit, true),
	InRate = shaper:new(?MAX_IN_RATE),
	OutRate = shaper:new(?MAX_OUT_RATE),
    {ok, streaming, #state{sid=Sid,in_rate=InRate,out_rate=OutRate}}.

-spec handle_event({stream, #xmlElement{}}, state(), #state{}) -> {next_state, state(), #state{}} | 
																	  {next_state, state(), #state{}, integer()} | 
																	  {stop, term(), #state{}}.
handle_event({stream, XmlEl, BodySize, RcvrPid}, 
			 StateName=streaming, 
			 StateData=#state{rid=undefined,in_rate=InRate,in_bytes=InBytes}) ->
	{InRate1, _Pause} = shaper:update(InRate, BodySize),
	InBytes1 = InBytes + BodySize,
	
	State = StateData#state{rcvr_pids=[RcvrPid], in_rate=InRate1, in_bytes=InBytes1},
	State1 = start_first_stream(XmlEl, State),
	{next_state, StateName, State1};

handle_event({stream, XmlEl, BodySize, RcvrPid}, 
			 StateName=streaming, 
			 StateData=#state{rid=Rid,
							  in_rate=InRate,
							  in_bytes=InBytes,
							  max_hold=MaxHold,
							  rcvr_pids=RcvrPids}) ->
	{InRate1, _Pause} = shaper:update(InRate, BodySize),
	InBytes1 = InBytes + BodySize,
	
	%% rid must be within the window
	RcvrRid = list_to_integer(get_attr(XmlEl, "rid")),
	MaxRid = Rid + MaxHold + 1,
	lager:debug("got rid ~p, prev rid ~p", [RcvrRid, Rid]),
	
	if 
		%% next rid in the sequence received
		RcvrRid =< MaxRid andalso RcvrRid =:= Rid + 1 ->
			State = StateData#state{rid=RcvrRid, rcvr_pids=RcvrPids ++ [RcvrPid], in_rate=InRate1, in_bytes=InBytes1},
			State1 = stream_data(XmlEl, State),
			{next_state, StateName, State1};
		
		%% out of sequence rid received, wait till in sequence requests are rcvd
		RcvrRid =< MaxRid andalso RcvrRid > Rid + 1 ->
			ok;
		
		%% previously processed rid received
		RcvrPid =< MaxRid andalso RcvrRid =:= Rid - 1 ->
			ok;
		
		true ->
			%% send item not found terminate body
			lager:debug("rcvd rcvr rid outside of window ~p", [RcvrRid]),
			{stop, item_not_found, StateData}
	end;

handle_event(Event, StateName, StateData) ->
	lager:debug("unhandled event ~p", [Event]),
    {next_state, StateName, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
	lager:debug("unhandled sync event ~p", [Event]),
    {reply, ok, StateName, StateData}.

handle_info({StreamId, #received_packet{} = Pkt}, 
			StateName, 
			StateData=#state{rcvr_pids=[], buffer=Buffer}) ->
	%% no rcvr in waiting, append to pending buffer for this stream
	NewBuffer =
	case proplists:get_value(StreamId, Buffer, undefined) of
		undefined -> Buffer ++ [{StreamId, [Pkt]}];
		StreamBuf -> lists:keyreplace(StreamId, 1, Buffer, {StreamId, StreamBuf ++ [Pkt]})
	end,
	{next_state, StateName, StateData#state{buffer=NewBuffer}};

handle_info({StreamId, #received_packet{raw_packet=XmlEl} = Pkt}, 
			StateName, 
			StateData=#state{rcvr_pids=[RcvrPid | RcvrPids], buffer=Buffer,
							 out_rate=OutRate, out_bytes=OutBytes}) ->
	%% prepend any pending buffer for this stream id
	AllXmlEl =
	case proplists:get_value(StreamId, Buffer, undefined) of
		undefined -> [XmlEl];
		StreamBuf ->
			lists:foldl(fun(#received_packet{raw_packet=RXmlEl}, Acc) -> Acc ++ [RXmlEl] end, [], StreamBuf) ++ [XmlEl]
	end,
	
	%% delete stream id key from buffer
	NewBuffer = lists:keydelete(StreamId, 1, Buffer),
	
	%% send to rcvr
	Attrs = gen_attrs([{'xmlns:stream', ?NS_XMPP_b}, {'stream', StreamId}]),
	SentBodySize = send_to_rcvr(RcvrPid, Attrs, AllXmlEl),
	{OutRate1, _Pause} = shaper:update(OutRate, SentBodySize),
	OutBytes1 = OutBytes + SentBodySize,
	
	{next_state, StateName, StateData#state{rcvr_pids=RcvrPids, buffer=NewBuffer, out_rate=OutRate1, out_bytes=OutBytes1}};

handle_info(Info, StateName, StateData) ->
	lager:debug("unhandled info ~p", [Info]),
    {next_state, StateName, StateData}.

terminate(Reason, _StateName, _StateData) ->
	lager:debug("unhandled terminate with reason ~p", [Reason]),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

start_first_stream(XmlEl, State=#state{sid=Sid,
									   max_inactivity=MaxInactivity,
									   max_pause=MaxPause,
									   min_polling=MinPolling,
									   rcvr_pids=RcvrPids,
									   out_rate=OutRate,
									   out_bytes=OutBytes}) ->
	%% mandatory attrs
	To = get_attr(XmlEl, "to"),
	Lang = get_attr(XmlEl, "lang"),
	Ver = get_attr(XmlEl, "ver"),
	Wait = get_attr(XmlEl, "wait"),
	Hold = get_attr(XmlEl, "hold"),
	
	%% optional attrs
	Route = get_attr(XmlEl, "route"),
	From = get_attr(XmlEl, "from"),
	Ack = get_attr(XmlEl, "ack"),
	ContentType = get_attr(XmlEl, "content"),
	
	%% other attrs
	Version = get_attr(XmlEl, "version"),
	Rid = get_attr(XmlEl, "rid"),
	
	%% start stream
	{ok, StreamPid} = ebosh_stream:start_link(To),
	case ebosh_stream:connect(StreamPid) of
		{ok, StreamId, StreamFeatures} ->
			Stream = #stream{to=To,lang=Lang,route=Route,from=From,version=Version,pid=StreamPid},
			
			%% prepare session start response attrs
			KV = [{sid, Sid},{wait,Wait},{requests,list_to_integer(Hold)+1},
				  {inactivity,MaxInactivity},{maxpause,MaxPause},{polling,MinPolling},
				  {ver,Ver},{from,To},{authid,StreamId},{secure,true},{'xmlns:stream',?NS_XMPP_b},
				  {version,?NS_BOSH_s,Version},{stream,StreamId},{restartlogic,?NS_BOSH_s,true}],
			Attrs = gen_attrs(KV),
			
			%% send to rcvr
			[RcvrPid|RcvrPids1] = RcvrPids,
			SentBodySize = send_to_rcvr(RcvrPid, Attrs, StreamFeatures, [{?NS_XBOSH_s, ?NS_XBOSH_pfx}]),
			{OutRate1, _Pause} = shaper:update(OutRate, SentBodySize),
			OutBytes1 = OutBytes + SentBodySize,
			
			%% no rcvr in waiting, set max inactivity timer
			{ok, InactivityTRef} = timer:send_after(MaxInactivity * 1000, ?MAX_INACTIVITY_TIMEOUT_MSG),
			
			State#state{max_wait = list_to_integer(Wait),
						max_hold = list_to_integer(Hold),
						inactivity_tref = InactivityTRef,
						rid = list_to_integer(Rid),
						ver = Ver,
						ack = Ack,
						content_type = ContentType,
						streams = [{StreamId, Stream}],
						out_rate = OutRate1,
						out_bytes = OutBytes1,
						rcvr_pids = RcvrPids1};
		'connect_error' ->
			%% TODO: send appropriate response to rcvr pid
			lager:debug("first stream connect error"),
			State
	end.

stream_data(XmlEl, State=#state{streams=Streams, rcvr_pids=RcvrPids, inactivity_tref=InactivityTRef, buffer=Buffer}) ->
	%% got request, clear previous max inactivity time
	if InactivityTRef =/= undefined -> timer:cancel(InactivityTRef);
	   true -> ok
	end,
	
	%% case deciding attrs
	To = get_attr(XmlEl, "to"),
	Restart = get_attr(XmlEl, "xmpp:restart"),
	StreamId = get_attr(XmlEl, "stream"),
	Type = get_attr(XmlEl, "type"),
	
	%% stream add attrs
	Lang = get_attr(XmlEl, "xml:lang"),
	Route = get_attr(XmlEl, "route"),
	From = get_attr(XmlEl, "from"),
	Version = get_attr(XmlEl, "version"),
	
	%% get body childs
	Childrens = XmlEl#xmlElement.content,
	
	%% total stream cnt active right now
	StreamCnt = erlang:length(Streams),
	RcvrCnt = erlang:length(RcvrPids),
	
	case StreamId of
		%% session terminate request
		undefined when Type =:= "terminate" ->
			%% flush any pending buffer
			%% send terminated body
			%% terminate all underlying streams
			%% shotdown session
			lager:debug("terminate request rcvd"),
			ok;
		
		%% stream id is allowed missing on restart stream request only if 1 stream is active
		undefined when Restart =:= "true" andalso StreamCnt =:= 1 ->
			lager:debug("restart stream request"),
			[{_, Stream}] = Streams,
			ebosh_stream:reset_parser(Stream#stream.pid),
			ebosh_stream:send(Stream#stream.pid, exmpp_stream:opening(Stream#stream.to, ?NS_JABBER_CLIENT, {1,0})),
			
			%% in single stream case, nothing must be in buffer to be sent out
			%% so simply hold onto this request
			State;
		
		%% a pure ping request
		undefined when Childrens =:= [] ->
			%% release any previous request with pending buffer
			%% if no previous request is on hold and no pending buffer to send
			%% hold onto this request
			lager:debug("pure ping request"),
			ok;
		
		%% if only 1 stream is active, pkt belongs to the only stream
		undefined when Childrens =/= [] andalso StreamCnt =:= 1 ->
			%% relay pkt to underlying stream
			[{_, Stream}] = Streams,
			lager:debug("relaying to the only active stream ~p", [Stream#stream.pid]),
			[Children|[]] = Childrens,
			ok = ebosh_stream:send(Stream#stream.pid, Children),
			
			%% release any previous request with pending buffer
			%% if no previous request is on hold and no pending buffer to send
			%% hold onto this request
			if 
				Buffer =:= [] andalso RcvrCnt =:= 1 ->
					lager:debug("buffer is empty and 1 rcvr in queue, hold to it"),
					State;
				Buffer =:= [] andalso RcvrCnt =/= 1 ->
					lager:debug("buffer is empty and more than 1 rcvr in queue, releasing old rcvr"),
					[OldRcvrPid|NewRcvrPids] = RcvrPids,
					send_empty_body(OldRcvrPid),
					State#state{rcvr_pids=NewRcvrPids};
				Buffer =/= [] andalso RcvrCnt =:= 1 ->
					lager:debug("pending buffer found and current rcvr is the only rcvr, relay data to it"),
					ok;
				Buffer =/= [] andalso RcvrCnt =/= 1 ->
					lager:debug("pending buffer found and more than 1 rcvr in waiting, release buffer to previous rcvr"),
					ok
			end;
		
		%% if more than 1 stream is active, this is a broadcast pkt request in multi stream case
		undefined when Childrens =/= [] ->
			%% broadcast pkt to all underlying streams
			%% release any previous request with pending buffer
			%% if no previous request is on hold and no pending buffer to send
			%% hold onto this request
			lager:debug("broadcast pkt rcvd"),
			ok;
		
		StreamId when Type =:= "terminate" ->
			lager:debug("restart request in multi stream case found"),
			ok;
		
		StreamId when Restart =:= "true" ->
			lager:debug("restart request in multi stream case found"),
			ok;
		
		%% stream id found i.e. client do support multi stream
		StreamId ->
			%% flush any pending buffer
			%% relay data to underlying associated stream
			lager:debug("client support multistream, pkt for stream ~p rcvd", [StreamId]),
			ok
	end.

send_empty_body(RcvrPid) ->
	send_to_rcvr(RcvrPid, []).

send_to_rcvr(RcvrPid, Attrs) ->
	send_to_rcvr(RcvrPid, Attrs, []).

send_to_rcvr(RcvrPid, Attrs, Children) ->
	send_to_rcvr(RcvrPid, Attrs, Children, []).

send_to_rcvr(RcvrPid, Attrs, Children, DecNS) ->
	BodyEl = #xmlel{name='body', ns=?NS_HTTP_BIND_s, attrs=Attrs, children=Children, declared_ns=DecNS},
	Body = exmpp_xml:document_to_binary(BodyEl),
	RcvrPid ! {?EBOSH_RESPONSE_MSG, ?HEADER, Body},
	iolist_size(Body).

-ifdef(TEST).

basic_test() ->
	ebosh:start(),
	Body = "<body content='text/xml; charset=utf-8' from='root@dev.jaxl.com' hold='1' rid='1573741820' to='dev.jaxl.com' route='xmpp:dev.jaxl.com:5222' ver='1.6' wait='60' ack='1' xml:lang='en' xmlns='http://jabber.org/protocol/httpbind'/>",
	{ok, BoshPid} = ebosh_session:start_link("some_sid"),
	ok = ebosh_session:stream("some_sid", Body),
	wait_for_resp(BoshPid).

wait_for_resp(BoshPid) ->
	receive
		{?RESPONSE_MSG, ?HEADER, Body} ->
			lager:debug("got ~p~n", [Body])
	end,
	ebosh:stop().

-endif.
