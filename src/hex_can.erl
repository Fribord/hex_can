%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Hex CAN plugin 
%%% @end
%%% Created :  24 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_can).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 init_event/2,
	 add_event/3, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(Flags, Signal, Cb) ->
    hex_can_server:add_event(Flags, Signal, Cb).

%%
%%  del_event(Ref::reference()) ->
%%     ok.
del_event(Ref) ->
    hex_can_server:del_event(Ref).
%%
%% output(Flags::[{atom(),term()}], Env::[{atom(),term()}]) ->
%%    ok.
%%
output(Flags, _Env) ->
    ID  = proplists:get_value(id, Flags),      %% mandatory
    Len0 = proplists:get_value(len, Flags, -1), %% -1 = derive from data
    Ext = proplists:get_bool(ext, Flags),      %% extended mode
    Rtr = proplists:get_bool(rtr, Flags),      %% request for transmission
    Data0 = proplists:get_value(data, Flags,<<>>),  %% binary data
    Data  = erlang:iolist_to_binary(Data0),
    Len = if Len0 < 0 -> byte_size(Data);
	     true -> Len0
	  end,
    can:send(ID,Len,Ext,Rtr,Data).

%%
%% init_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,Reason}
%% validate_event is assumed to have been run before init !
init_event(_Dir,_Flags) ->
    ok.

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(_Dir, _Flags) ->
    %% FIXME!!!
    ok.

