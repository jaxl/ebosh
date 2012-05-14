

#Module ebosh_sup#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


__Behaviours:__ [`supervisor`](supervisor.md).
<a name="types"></a>

##Data Types##




###<a name="type-childspec">childspec()</a>##



<pre>childspec() = {atom, {module() | function() | list()}, permanent, integer(), worker, [atom]}</pre>



###<a name="type-supflags">supflags()</a>##



<pre>supflags() = {one_for_all, 0, 1}</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="init-1"></a>

###init/1##


<pre>init(X1::[]) -> {ok, {<a href="#type-supflags">supflags()</a>, [<a href="#type-childspec">childspec()</a>]}} | ignore | {error, term()}</pre>
<br></br>


<a name="start_link-0"></a>

###start_link/0##


`start_link() -> any()`

