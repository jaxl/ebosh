

#Module ebosh_session#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


__Behaviours:__ [`gen_fsm`](gen_fsm.md).
<a name="types"></a>

##Data Types##




###<a name="type-sid">sid()</a>##



<pre>sid() = string()</pre>



###<a name="type-state">state()</a>##



<pre>state() = atom</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-4">code_change/4</a></td><td>hot code update.</td></tr><tr><td valign="top"><a href="#gen_attrs-1">gen_attrs/1</a></td><td>generate [#xmlattr{}, ...] for passed key-value pair of Attrs.</td></tr><tr><td valign="top"><a href="#gen_sid-0">gen_sid/0</a></td><td>generate new session id.</td></tr><tr><td valign="top"><a href="#get_attr-2">get_attr/2</a></td><td>extract bosh <body></body> #xmlElement{} attribute value by key.</td></tr><tr><td valign="top"><a href="#get_proc_name-1">get_proc_name/1</a></td><td>get ebosh_session registered name for Sid.</td></tr><tr><td valign="top"><a href="#handle_event-3">handle_event/3</a></td><td>this method will ONLY receive the very first session start bosh body packet.</td></tr><tr><td valign="top"><a href="#handle_info-3">handle_info/3</a></td><td>handle xmpp Pkt from underlying StreamId ebosh_stream process
this case handles when we have no rcvrs in waiting.</td></tr><tr><td valign="top"><a href="#handle_sync_event-4">handle_sync_event/4</a></td><td>unhandled sync events.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_alive-1">is_alive/1</a></td><td>check if underlying ebosh_session for Sid is still running.</td></tr><tr><td valign="top"><a href="#is_valid_session_start_pkt-1">is_valid_session_start_pkt/1</a></td><td>check if an #xmlElement{} is a valid session start bosh body.</td></tr><tr><td valign="top"><a href="#parse_body-1">parse_body/1</a></td><td>parse raw Body string as #xmlElement{}.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>start new ebosh_session with passed Sid as a linked process.</td></tr><tr><td valign="top"><a href="#start_super-1">start_super/1</a></td><td>start new ebosh_session with passed Sid as a supervised process.</td></tr><tr><td valign="top"><a href="#stream-2">stream/2</a></td><td>stream raw Body string to ebosh_session for Sid.</td></tr><tr><td valign="top"><a href="#stream-3">stream/3</a></td><td>stream an #xmlElement{} pkt to underlying ebosh_session for Sid
BodySize is required for rate limiting and is required.</td></tr><tr><td valign="top"><a href="#terminate-3">terminate/3</a></td><td>terminate.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="code_change-4"></a>

###code_change/4##


`code_change(OldVsn, StateName, StateData, Extra) -> any()`

hot code update<a name="gen_attrs-1"></a>

###gen_attrs/1##


`gen_attrs(Attrs) -> any()`

generate [#xmlattr{}, ...] for passed key-value pair of Attrs<a name="gen_sid-0"></a>

###gen_sid/0##


<pre>gen_sid() -> <a href="#type-sid">sid()</a></pre>
<br></br>


generate new session id<a name="get_attr-2"></a>

###get_attr/2##


<pre>get_attr(XmlElement::#xmlElement{}, Attr::string()) -&gt; undefined | string()</pre>
<br></br>


extract bosh  #xmlElement{} attribute value by key<a name="get_proc_name-1"></a>

###get_proc_name/1##


<pre>get_proc_name(Sid::<a href="#type-sid">sid()</a>) -> atom</pre>
<br></br>


get ebosh_session registered name for Sid<a name="handle_event-3"></a>

###handle_event/3##


<pre>handle_event(Event::{stream, #xmlElement{}}, StateName::<a href="#type-state">state()</a>, State::#state{}) -> {next_state, <a href="#type-state">state()</a>, #state{}} | {next_state, <a href="#type-state">state()</a>, #state{}, integer()} | {stop, term(), #state{}}</pre>
<br></br>


this method will ONLY receive the very first session start bosh body packet<a name="handle_info-3"></a>

###handle_info/3##


`handle_info(Info, StateName, StateData) -> any()`

handle xmpp Pkt from underlying StreamId ebosh_stream process
this case handles when we have no rcvrs in waiting<a name="handle_sync_event-4"></a>

###handle_sync_event/4##


`handle_sync_event(Event, From, StateName, StateData) -> any()`

unhandled sync events<a name="init-1"></a>

###init/1##


<pre>init(X1::[]) -> {ok, <a href="#type-state">state()</a>, #state{}} | {ok, <a href="#type-state">state()</a>, #state{}, integer()} | ignore | {stop, term()}</pre>
<br></br>


<a name="is_alive-1"></a>

###is_alive/1##


<pre>is_alive(Sid::<a href="#type-sid">sid()</a>) -> false | pid()</pre>
<br></br>


check if underlying ebosh_session for Sid is still running<a name="is_valid_session_start_pkt-1"></a>

###is_valid_session_start_pkt/1##


<pre>is_valid_session_start_pkt(XmlElement::#xmlElement{}) -&gt; boolean()</pre>
<br></br>


check if an #xmlElement{} is a valid session start bosh body<a name="parse_body-1"></a>

###parse_body/1##


<pre>parse_body(Body::binary() | string()) -&gt; error | #xmlElement{}</pre>
<br></br>


parse raw Body string as #xmlElement{}<a name="start_link-1"></a>

###start_link/1##


<pre>start_link(Sid::<a href="#type-sid">sid()</a>) -> {ok, pid()}</pre>
<br></br>


start new ebosh_session with passed Sid as a linked process<a name="start_super-1"></a>

###start_super/1##


<pre>start_super(Sid::<a href="#type-sid">sid()</a>) -> {ok, pid()}</pre>
<br></br>


start new ebosh_session with passed Sid as a supervised process<a name="stream-2"></a>

###stream/2##


<pre>stream(Sid::<a href="#type-sid">sid()</a>, Body::string()) -> ok | error</pre>
<br></br>


stream raw Body string to ebosh_session for Sid<a name="stream-3"></a>

###stream/3##


<pre>stream(Sid::<a href="#type-sid">sid()</a>, XmlElement::#xmlElement{}, BodySize::integer()) -> ok</pre>
<br></br>


stream an #xmlElement{} pkt to underlying ebosh_session for Sid
BodySize is required for rate limiting and is required<a name="terminate-3"></a>

###terminate/3##


`terminate(Reason, StateName, StateData) -> any()`

terminate