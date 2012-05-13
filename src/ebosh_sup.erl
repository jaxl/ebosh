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
%%% Created : May 11, 2012
%%% -------------------------------------------------------------------
-module(ebosh_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------
-type supflags() :: {one_for_all,0,1}.
-type childspec() :: {atom, {module() | function() | list()}, permanent, integer(), worker, [atom]}.

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec init([]) -> {ok, {supflags(), [childspec()]}} | ignore | {error, term()}.
init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 10,
	MaxSecBetweenRestarts = 10,
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecBetweenRestarts},
	
	HttpMod = list_to_atom("ebosh_" ++ ebosh:get_env(http_lib, "mochiweb")),
	Web = {HttpMod, {HttpMod, start, []}, permanent, 5000, worker, dynamic},
	Procs = [Web],
	
    {ok,{SupFlags, Procs}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

