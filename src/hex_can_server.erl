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
%%%
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_can_server).

-behaviour(gen_server).

-include_lib("can/include/can.hrl").

%% API
-export([start_link/0, stop/0]).
-export([add_event/3, del_event/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(filter,
	{
	  intf = 0,
	  id   = 0,
	  mask = 0,
	  invert = false
	}).

-record(sub,
	{
	  ref     :: reference(),
	  filter  :: #filter{},
	  signal   :: term(),
	  callback :: atom() | function()
	}).

-record(state, {
	  joined :: boolean(),   %% is joined to hex server?
	  subs = [] :: [#sub{}]
	 }).

%% from canopen.hrl (not imported yet)
-define(COBID_ENTRY_EXTENDED,       16#20000000).
-define(CANID_TO_COBID(ID),
	if ?is_can_id_eff((ID)) ->
		((ID) band ?CAN_EFF_MASK) bor ?COBID_ENTRY_EXTENDED;
	   true ->
		((ID) band ?CAN_SFF_MASK)
	end).


%%%===================================================================
%%% API
%%%===================================================================
add_event(Flags, Signal, Cb) when is_atom(Cb); is_function(Cb,2) ->
    gen_server:call(?MODULE, {add_event, self(), Flags, Signal, Cb}).

del_event(Ref) ->
    gen_server:call(?MODULE, {del_event, Ref}).

stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Joined = hex:auto_join(hex_can),
    case can_router:attach() of
	ok -> 
	    {ok, #state{joined=Joined}};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_event,Pid,Flags,Signal,Cb}, _From, State) ->
    Ref = erlang:monitor(process,Pid),
    lager:debug("add_event: ref=~w, ~p ~p", [Ref, Flags, Signal]),
    %% Interface = proplists:get_value(interface,Flags,-1),
    F = #filter { id = proplists:get_value(id, Flags, 0),
		  mask = proplists:get_value(mask, Flags, 0),
		  invert = proplists:get_bool(invert, Flags),
		  intf   = proplists:get_value(intf, Flags, 0)
		},
    Sub = #sub { ref=Ref, filter=F, signal=Signal, callback=Cb},
    Subs = [Sub | State#state.subs],
    {reply, {ok,Ref}, State#state { subs = Subs }};
handle_call({del_event,Ref}, _From, State) ->
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	false ->
	    {reply, ok, State};
	{value, Sub, Subs} ->
	    erlang:demonitor(Sub#sub.ref),
	    {reply, ok, State#state { subs = Subs}}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Frame=#can_frame{}, State) ->
    CobId = ?CANID_TO_COBID(Frame#can_frame.id),
    Env = case Frame#can_frame.data of
	      <<16#80,Index:16/little,SubInd:8,Value:32/little>> ->
		  [{id,CobId},
		   {chan,SubInd},
		   {type,Index},
		   {value,Value},
		   {data,Frame#can_frame.data},
		   {source, {can,Frame#can_frame.intf}}];
	      _BinData ->
		  [{id,CobId},
		   {data,Frame#can_frame.data},
		   {source, {can,Frame#can_frame.intf}}]
	  end,
    lager:debug("can signal env = ~p\n", [Env]),
    lists:foreach(
      fun(#sub{signal=Sig,callback=Cb,filter=F}) ->
	      case match_filter(Frame, F) of
		  true ->
		      callback(Cb,Sig,Env);
		  false -> ignore
	      end
      end, State#state.subs),
    {noreply, State};
handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    lager:debug("monitor DOWN ~p ~p", [_Pid,_Reason]),
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	false ->
	    {noreply, State};
	{value, _Sub, Subs} ->
	    {noreply, State#state { subs = Subs}}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled info ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

callback(Cb,Signal,Env) when is_atom(Cb) ->
    Cb:event(Signal, Env);
callback(Cb,Signal,Env) when is_function(Cb, 2) ->
    Cb(Signal,Env).

%% the expression below says:
%% the interface must match unless set to 0 (ignore)
%% otherwise the frameid must match the id and mask expression
%% or must not match if invert logic (not for interface!) is used.
%%
match_filter(Frame, #filter { intf=Intf, id=ID, mask=Mask, invert=Invert }) ->
    ((Intf =:= 0) orelse (Intf =:= Frame#can_frame.intf))
	andalso
	  ( ((Frame#can_frame.id band Mask) =:= (ID band Mask))  =/= Invert ).
