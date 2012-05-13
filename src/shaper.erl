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