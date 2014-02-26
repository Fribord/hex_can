%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Hex CAN plugin 
%%% @end
%%% Created :  24 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_can).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 init_event/2,
	 add_event/2, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(Flags, Signal) ->
    hex_can_server:add_event(Flags, Signal).

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
    Len = proplists:get_value(len, Flags, -1), %% -1 = derive from data
    Ext = proplists:get_bool(ext, Flags),      %% extended mode
    Rtr = proplists:get_bool(rtr, Flags),      %% request for transmission
    Data = proplists:get_value(data, Flags,<<>>),  %% binary data
    can:send(ID,Len,Ext,Rtr,Data).

%%
%% init_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,Reason}
%% validate_event is assumed to have been run before init !
init_event(_Dir,_Flags) ->
    ok.

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(Dir, Flags) ->
    ok.

