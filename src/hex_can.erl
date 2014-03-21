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
-export([transmit/2]).

-include_lib("can/include/can.hrl").
-include_lib("hex/include/hex.hrl").
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
    Intf = proplists:get_value(intf,Flags,0),
    Data  = erlang:iolist_to_binary(Data0),
    Len = if Len0 < 0 -> byte_size(Data);
	     true -> Len0
	  end,
    try can:create(ID,Len,Ext,Rtr,Intf,Data) of
	Frame ->
	    case whereis(hex_can_server) of
		undefined ->
		    can:send(Frame);
		Pid ->
		    can:send_from(Pid, Frame)
	    end
    catch
	error:Reason ->
	    lager:error("can parameter error ~p", [Reason])
    end.

%%
%% init_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,Reason}
%% validate_event is assumed to have been run before init !
init_event(_Dir,_Flags) ->
    ok.

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(out, Flags) ->
    hex:validate_flags(Flags, output_spec());
validate_event(in, Flags) ->
    hex:validate_flags(Flags, input_spec()).

%% select input from interface intf (0 == all)
%%   (not invert) &&     ( (frame.id & mask) == (id & mask) )
%%   (invert) &&     not ( (frame.id & mask) == (id & mask) )
%% default: intf=0, id=0, mask=0, invert=false
%% => (not false) && (frame.id & 0) == (id & 0) == true && (0 == 0)
%% 
input_spec() ->
    [
     {id, optional, unsigned32, 0},
     {mask, optional, unsigned32, 0},
     {invert, optional, boolean, false},
     {intf, optional, unsigned, 0}
    ].

%% fixme! id is mandatory for output, but not for transmit
output_spec() ->
    [
     {id,  optional, unsigned32, 0}, 
     {len, optional, {integer,-1,8}, -1},
     {ext, optional, boolean, false},
     {rtr, optional, boolean, false},
     {data, optional, iolist, <<>>},
     {intf, optional, unsigned, 0}
    ].

%%
%% Transmit is like output but for hex_signal
%%
%% from canopen.hrl (not imported yet)
-define(COBID_ENTRY_EXTENDED,       16#20000000).

transmit(Signal, _Flags) ->
    lager:debug("transmit: ~p", [Signal]),
    COBID = Signal#hex_signal.id,
    SubInd = Signal#hex_signal.chan,
    Index = Signal#hex_signal.type,
    Value = Signal#hex_signal.value,
    {ID,Ext} = if COBID band ?COBID_ENTRY_EXTENDED =/= 0 ->
		       {COBID band ?CAN_EFF_MASK, true};
		  true ->
		       {COBID band ?CAN_SFF_MASK, false}
	       end,
    Data = <<16#80,Index:16/little,SubInd:8,Value:32/little>>,
    try can:create(ID,8,Ext,false,0,Data) of
	Frame ->
	    case whereis(hex_can_server) of
		undefined ->
		    can:send(Frame);
		Pid ->
		    can:send_from(Pid, Frame)
	    end
    catch
	error:Reason ->
	    lager:error("can transmit error ~p", [Reason])
    end.
