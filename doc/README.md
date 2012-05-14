

#`ebosh' - a BOSH connection manager written on top of erlang otp principles#


Copyright (c) 2011-2012 Abhinav Singh

__Version:__ 0.0.1

__Authors:__ Abhinav Singh ([`me@abhinavsingh.com`](mailto:me@abhinavsingh.com)).

ebosh
======
a work in progress, not yet ready for production use

ebosh is a [BOSH](http://xmpp.org/extensions/xep-0124.md) connection manager written on top of erlang otp principles.

Features:
----------
* stream restarts
* multiple streams
* in-order message forwarding
* starttls, stream compression support
* request and response acknowledgements
* support for https and key sequence protection against insecure sessions
* support for long polling, chunked encoding, jsonp and websocket bosh requests

Quick Start Guide
------------------

### get the source and compile<pre>
	PROMPT> git clone git://github.com/abhinavsingh/ebosh.git
	PROMPT> cd ebosh
	PRMOPT> chmod +x ebosh
	PROMPT> ./ebosh rebar
	PROMPT> ./ebosh deps
	PROMPT> ./ebosh compile
	PROMPT></pre>

### start<pre>
	PROMPT> ./ebosh start
	starting ebosh ...
	PROMPT></pre>

### test

Ping ebosh to test if it's running<pre>
	PROMPT> ./ebosh ping
	pinging ebosh@127.0.0.1 ...
	running
	PROMPT></pre>

Point your bosh clients to http://127.0.0.1:9696/http-bind

### debug<pre>
	PROMPT> ./ebosh ctl
	Erlang R14B04 (erts-5.8.5) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:5] [hipe] [kernel-poll:true]
	Eshell V5.8.5  (abort with ^G)
	(ebosh@127.0.0.1)1>
	BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
	       (v)ersion (k)ill (D)b-tables (d)istribution
	^C
	PROMPT> toolbar:start().
	PROMPT></pre>

### stop<pre>
	PROMPT> ./ebosh stop
	PROMPT> ./ebosh ping
	pinging ebosh@127.0.0.1 ...
	not running</pre>

Configuration Parameters
-------------------------
Edit `ebosh.config` for your custom port and path configuration. Following options are available:
* `http_lib`          "mochiweb" | "cowboy"
* `http_port`         9696
* `https_port`        9697
* `http_bind_path`    "/http-bind"
* `https_certfile`    "priv/cert/server.crt"
* `https_keyfile`     "priv/cert/server.key"

Dependencies
-------------
ebosh require following external libraries.

* [exmpp](https://github.com/abhinavsingh/exmpp/) for xmpp client management
* [mochiweb](https://github.com/abhinavsingh/mochiweb/) or [cowboy](https://github.com/abhinavsingh/cowboy/) for http web server
* [lager](https://github/abhinavsingh/lager/) for logging facility

You can run ebosh either with mochiweb, cowboy or even your own custom erlang server.ebosh requires a patched version of exmpp to work with. You can obtain the 
patched version in two ways:
* `git clone git://github.com/abhinavsingh/exmpp.git`
* patch `src/network/exmpp_session.erl` of exmpp source with this [patch](https://github.com/abhinavsingh/exmpp/commit/580d736ad9c6c776ee1cc83bdcf2f63ca9096b2c)


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="ebosh.md" class="module">ebosh</a></td></tr>
<tr><td><a href="ebosh_app.md" class="module">ebosh_app</a></td></tr>
<tr><td><a href="ebosh_cowboy.md" class="module">ebosh_cowboy</a></td></tr>
<tr><td><a href="ebosh_ejabberd.md" class="module">ebosh_ejabberd</a></td></tr>
<tr><td><a href="ebosh_mochiweb.md" class="module">ebosh_mochiweb</a></td></tr>
<tr><td><a href="ebosh_session.md" class="module">ebosh_session</a></td></tr>
<tr><td><a href="ebosh_stream.md" class="module">ebosh_stream</a></td></tr>
<tr><td><a href="ebosh_sup.md" class="module">ebosh_sup</a></td></tr>
<tr><td><a href="shaper.md" class="module">shaper</a></td></tr></table>

