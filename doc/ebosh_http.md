

#Module ebosh_http#
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#stream-3">stream/3</a></td><td>stream raw Body string
This must be called directly from web process adapter modules
Body is rcvd raw bosh <body></body> string
ResFun/1 accepts underlying ebosh_session BoshPid as argument
ErrFun/1 if Body string is an invalid bosh body pkt,
ErrFun({Code, Header, Body}) will be called.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="stream-3"></a>

###stream/3##


`stream(Body, ResFun, SetOptFun) -> any()`

stream raw Body string
This must be called directly from web process adapter modules
Body is rcvd raw bosh  string
ResFun/1 accepts underlying ebosh_session BoshPid as argument
ErrFun/1 if Body string is an invalid bosh body pkt,
ErrFun({Code, Header, Body}) will be called