ebosh
======
A work in progress, not yet ready for production use

ebosh is a BOSH connection manager written on top of erlang otp principles.
Specification about BOSH can be found [here](http://xmpp.org/extensions/xep-0124.html)

Dependencies
-------------
ebosh require following external libraries.

* [exmpp](https://github.com/abhinavsingh/exmpp/) for xmpp client management
* [mochiweb](https://github.com/abhinavsingh/mochiweb/) or [cowboy](https://github.com/abhinavsingh/cowboy/) for http web server
* [lager](https://github/abhinavsingh/lager/) for logging facility

You can run ebosh either with mochiweb, cowboy or even your own custom erlang server.

ebosh requires a patched version of exmpp to work with. You can obtain the 
patched version in two ways:
* git clone git://github.com/abhinavsingh/exmpp.git
* patch src/network/exmpp_session.erl of exmpp source with this [patch](https://github.com/abhinavsingh/exmpp/commit/580d736ad9c6c776ee1cc83bdcf2f63ca9096b2c)

Usage
------

### get the source and compile

PROMPT> git clone git://github.com/abhinavsingh/ebosh.git
PROMPT> cd ebosh && chmod +x ebosh
PROMPT> ./ebosh rebar && ./ebosh deps
PROMPT> ./ebosh compile
PROMPT>

### start ebosh

PROMPT> ./ebosh start
starting ebosh ...
PROMPT> 

### test ebosh

Point your bosh clients to http://127.0.0.1:9696/http-bind
Edit ebosh.config for your custom port and path configuration.
Ping ebosh to test if it's running

PROMPT> ./ebosh ping
pinging ebosh@127.0.0.1 ...
running
PROMPT> 

### debug ebosh

PROMPT> ./ebosh ctl
Erlang R14B04 (erts-5.8.5) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:5] [hipe] [kernel-poll:true]

Eshell V5.8.5  (abort with ^G)
(ebosh@127.0.0.1)1>
BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
^C
PROMPT>

### stop ebosh

PROMPT> ./ebosh stop
PROMPT> ./ebosh ping
pinging ebosh@127.0.0.1 ...
not running
