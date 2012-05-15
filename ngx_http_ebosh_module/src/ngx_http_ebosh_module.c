/*%%
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
%%*/

/*
 * ngx_http_ebosh_module.c
 *
 *  Created on: May 5, 2012
 *      Author: abhinavsingh
 */

/**
 * Nginx configuration file have 4 main contexts:
 * 	1) main
 * 	2) server
 * 	3) upstream
 * 	4) location
 *
 * Nginx module have 3 roles:
 * 	1) handlers i.e. process a request and produce output e.g. file serving, proxy
 * 	2) filters i.e. manipulate output produces by a handler e.g. gzip, server side includes
 * 	3) load-balancers i.e. choose a backend to send request to
 *
 * On startup each handler gets a chance to attach itself to a particular location defined in config
 * Handlers can return in 3 ways:
 * 	1) all is good
 * 	2) there was an error
 * 	3) decline to process request and defer to default handler
 *
 * If handler succeeds filters are called. Multiple filters can hook into same location
 *
 * In short:
 * 	Client sends HTTP request ->
 * 	Nginx chooses the appropriate handler based on the location config ->
 * 	(if applicable) load-balancer picks a backend server ->
 * 	Handler does its thing and passes each output buffer to the first filter ->
 * 	First filter passes the output to the second filter ->
 * 	second to third ->
 * 	third to fourth ->
 * 	Final response sent to client
 */

/**
 * Module Structs:
 * ----------------
 * module can define upto 3 structs, one each for following context:
 * 1) main
 * 2) server
 * 3) location
 *
 * naming convention of structs is ngx_http_<module name>_(main|srv|loc)_conf_t
 *
 * Module Directives:
 * -------------------
 * module directives are defined as static array of ngx_command_t, where:
 * struct ngx_command_t {
 * 		ngx_str_t             name;
 * 		ngx_uint_t            type;
 * 		char               *(*set)(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);
 * 		ngx_uint_t            conf;
 * 		ngx_uint_t            offset;
 * 		void                 *post;
 * };
 *
 * name is directive string with no spaces. data type is ngx_str_t which is a struct having data and len elements
 * type is set of flags that indicate where this directive is legal and how many argument directive takes
 * set struct element is a pointer to a function for setting up parts of module configuration
 * 		this setup function takes 3 arguments:
 * 		1) pointer to ngx_conf_t struct, which contains the arguments passed to the directive
 * 		2) pointer to current ngx_command_t struct
 * 		3) pointer to module custom conf struct
 *
 * Example of command array:
 * static ngx_command_t  ngx_http_circle_gif_commands[] = {
 * 	{
 * 		ngx_string("circle_gif"),			// directive name
 * 		NGX_HTTP_LOC_CONF|NGX_CONF_NOARGS,	// type of directive i.e. where this directive is legal and how many args it take
 * 		ngx_http_circle_gif,				// pointer to function for setting up parts of module conf
 * 		NGX_HTTP_LOC_CONF_OFFSET,			// tell whether this get saved to module main conf, server conf or loc conf
 * 		0,									// offset indicating which part of this conf struct to write to
 * 		NULL								// post data in case module need this at startup, usually NULL
 * 	},
 * 	ngx_null_command						// ngx_command_t array is ngx_null_command terminated
 * }
 *
 * The Module Context:
 * --------------------
 * This is a static ngx_http_module_t struct with reference to functions. e.g.
 * typedef struct {
 * 		ngx_int_t	(*preconfiguration)(ngx_conf_t *cf);
 * 		ngx_int_t	(*postconfiguration)(ngx_conf_t *cf);
 * 		void		*(*create_main_conf)(ngx_conf_t *cf);						// do a malloc and set defaults
 * 		char		*(*init_main_conf)(ngx_conf_t *cf, void *conf);				// override the defaults with what's in nginx.conf
 * 		void		*(*create_srv_conf)(ngx_conf_t *cf);
 * 		char		*(*merge_srv_conf)(ngx_conf_t *cf, void *prev, void *conf);	// merging created server conf with the main conf
 * 		void		*(*create_loc_conf)(ngx_conf_t *cf);
 * 		char		*(*merge_loc_conf)(ngx_conf_t *cf, void *prev, void *conf);	// merging location conf with the server conf
 * }
 *
 * Module Definition:
 * -------------------
 * ngx_module_t
 */

// nginx headers
#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>

// erlang requirements
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

// erlang headers
#include "erl_interface.h"
#include "ei.h"

#define BUFSIZE 1000

extern const char *erl_thisnodename(void);
extern short erl_thiscreation(void);
#define SELF(fd) erl_mk_pid(erl_thisnodename(),fd,0,erl_thiscreation())

/**
 * forward declaration of function for setting up module conf
 */
static char *
ngx_http_ebosh(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);

/**
 * module command directives
 */
static ngx_command_t ngx_http_ebosh_commands[] = {
	{
		ngx_string("ebosh"),					// directive name
		NGX_HTTP_LOC_CONF | NGX_CONF_NOARGS,	// type of directive
		ngx_http_ebosh,							// function pointer
		0,										// at what offset to save conf returned by function pointer
		0,										// offset indicating what offset of conf to write to
		NULL									// startup extra data
	},
	ngx_null_command
};

/**
 * Module context
 */
static ngx_http_module_t ngx_http_ebosh_module_ctx = {
	NULL, // preconf
	NULL, // postconf
	NULL, // create main conf
	NULL, // init main conf
	NULL, // create server conf
	NULL, // merge server conf
	NULL, // create loc conf
	NULL  // merge loc conf
};

/**
 * Module Definition
 */
ngx_module_t ngx_http_ebosh_module = {
	NGX_MODULE_V1,
	&ngx_http_ebosh_module_ctx,		// module ctx
	ngx_http_ebosh_commands,		// module directives
	NGX_HTTP_MODULE, 				// module type
	NULL, // init master
	NULL, // init module
	NULL, // init proc
	NULL, // init thread
	NULL, // exit thread
	NULL, // exit proc
	NULL, // exit master
	NGX_MODULE_V1_PADDING
};

static ngx_int_t
ngx_http_ebosh_handler(ngx_http_request_t *r)
{

	// initialize memory handler
	erl_init(NULL, 0);

	ngx_int_t rc;
	ngx_buf_t *b;
	ngx_chain_t out;

	// TODO: Set this during the erlang message recieve loop.
	r->headers_out.content_type.len = sizeof("text/plain") - 1;
	r->headers_out.content_type.data = (u_char *) "text/plain";

	// TODO: Add secret to config
	if(erl_connect_init(1, "secret", 0) == -1) {
		erl_err_quit("erl_connect_init");
		return NGX_HTTP_INTERNAL_SERVER_ERROR;
	}

	// file descriptor to connected erlang node
	int fd;
	// TODO: Add node name to config
	// TODO: Add node host to config
	if((fd = erl_connect("ebosh@127.0.0.1")) < 0) {
		erl_err_quit("erl_connect");
		return NGX_HTTP_INTERNAL_SERVER_ERROR;
	}
	fprintf(stderr, "Connected to ebosh@127.0.0.1\n\r");

	// prepare message to be sent to erlang node
	ETERM *arr[3], *emsg2;

	// our pid
	arr[0] = SELF(fd);

	// request method
	arr[1] = erl_mk_int(r->method);

	// request uri
	arr[2] = erl_mk_string((const char *) r->uri.data);

	// TODO: prepare header KV list of 2-tuples
	//arr[3] = headers;

	// TODO: prepare raw body
	//arr[4] = body;

	// make final tuple msg to be sent
	emsg2 = erl_mk_tuple(arr, 3);

	// TODO: registered process receiving the msg
	erl_reg_send(fd, "ebosh_http", emsg2);

	// loop vars
	int got;
	unsigned char buf[BUFSIZE];
	ErlMessage emsg;
	ETERM *status, *retbody;

	// TODO: Find a way to fail gracefully if a message isn't recieved within a specific amount of time.
	// TODO: Set the timeout duration in config.
	while(1) {
		// send msg and wait for response
		got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);

		if(got == ERL_TICK) {
			continue;
		}
		// An internal error has occured
		else if(got == ERL_ERROR) {
			r->headers_out.status = NGX_HTTP_INTERNAL_SERVER_ERROR;
			return NGX_HTTP_INTERNAL_SERVER_ERROR;
			break;
		}
		else {
			if(emsg.type == ERL_SEND) {
				// TODO: Handle response tuple: {StatusCode, Headers, Body}
				status = erl_element(1, emsg.msg);
				retbody = erl_element(3, emsg.msg);
				int status_code = ERL_INT_VALUE(status);
				char *body = (char *) ERL_BIN_PTR(retbody);

				// TODO: Set status code from response tuple
				if(status_code == 200) {
					r->headers_out.status = NGX_HTTP_OK;
				}
				else {
					r->headers_out.status = NGX_HTTP_INTERNAL_SERVER_ERROR;
				}

				// alloc pool for output response
				b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
				if(b == NULL) {
					return NGX_HTTP_INTERNAL_SERVER_ERROR;
				}

				// link buffer to the chain of buffers
				out.buf = b;
				out.next = NULL;
				b->pos = (u_char *) body;
				b->last = (u_char *) body + sizeof(body);
				b->memory = 1;
				b->last_buf = 1;

				r->headers_out.status = NGX_HTTP_OK;
				r->headers_out.content_length_n = sizeof(body);
				r->headers_out.last_modified_time = 23349600;

				rc = ngx_http_send_header(r);

				// free ETERM
				erl_free_term(emsg.msg);
				erl_free_term(status);
				erl_free_term(retbody);

				// If there is a body component, run it through the next filter.
				if (rc == NGX_ERROR || rc > NGX_OK || r->header_only) {
					return rc;
				}

				break;
			}
		}
	}

	return ngx_http_output_filter(r, &out);
}

static char *
ngx_http_ebosh(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_core_loc_conf_t *clcf;

    clcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_core_module);
    clcf->handler = ngx_http_ebosh_handler; /* handler to process the 'ebosh' directive */

    return NGX_CONF_OK;
}
