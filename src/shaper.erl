%%%----------------------------------------------------------------------
%%% File    : shaper.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Functions to control connections traffic
%%% Created :  9 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

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
%%%		Taken directly from ejabberd project
%%%		Modified for compatibility with ebosh
%%% Created : May 12, 2012
%%% -------------------------------------------------------------------
-module(shaper).
-author('alexey@process-one.net').

-export([new/1, update/2]).

-record(maxrate, {maxrate, lastrate, lasttime}).

new(MaxRate) ->
    #maxrate{maxrate = MaxRate,
	     lastrate = 0,
	     lasttime = now_to_usec(now())}.

update(#maxrate{} = State, Size) ->
    MinInterv = 1000 * Size /
	(2 * State#maxrate.maxrate - State#maxrate.lastrate),
    Interv = (now_to_usec(now()) - State#maxrate.lasttime) / 1000,
    lager:debug("State: ~p, Size=~p~nM=~p, I=~p~n", [State, Size, MinInterv, Interv]),
    Pause = if
		MinInterv > Interv ->
		    1 + trunc(MinInterv - Interv);
		true ->
		    0
	    end,
    NextNow = now_to_usec(now()) + Pause * 1000,
    {State#maxrate{
       lastrate = (State#maxrate.lastrate +
		   1000000 * Size / (NextNow - State#maxrate.lasttime))/2,
       lasttime = NextNow},
     Pause}.

now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.