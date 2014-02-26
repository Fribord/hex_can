%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_can_server).

-behaviour(gen_server).

-include_lib("lager/include/log.hrl").
-include_lib("can/include/can.hrl").
-include_lib("hex/include/hex.hrl").

%% API
-export([start_link/0, stop/0]).
-export([add_event/2, del_event/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  subs = [] :: [{Ref::reference(),Signal::term()}]
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
add_event(Flags, Signal) ->
    gen_server:call(?MODULE, {add_event, Flags, Signal}).

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
    case can_router:attach() of
	ok -> 
	    {ok, #state{}};
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
handle_call({add_event,_Flags,Signal}, _From, State) ->
    Ref = make_ref(),
    lager:debug("add_event: ref=~w, ~p ~p", [Ref, _Flags, Signal]),
    %% Interface = proplists:get_value(interface,Flags,-1),
    Subs = [{Ref, Signal} | State#state.subs],
    {reply, {ok,Ref}, State#state { subs = Subs }};
handle_call({del_event,Ref}, _From, State) ->
    case lists:keytake(Ref, State#state.subs) of
	false -> 
	    {reply, ok, State};
	{value,_, Subs} ->
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
    Sig =
	case Frame#can_frame.data of
	    <<16#80,Index:16/little,SubInd:8,Value:32/little>> ->
		#hex_signal{id=CobId,
			    chan=SubInd,
			    type=Index,
			    value=Value,
			    source={can,Frame#can_frame.intf}
			   };
	    BinData ->
		#hex_signal {id=CobId,
			     chan=0,
			     type=0,
			     value=BinData,
			     source={can,Frame#can_frame.intf}
			     }
	end,
    lager:debug("signal input ~p\n", [Sig]),
    lists:foreach(
      fun({_Ref,Pat}) ->
	      case hex_server:match_pattern(Sig,Pat) of
		  {true,_} -> hex_server ! Sig;
		  false -> ignore
	      end
      end, State#state.subs),
    {noreply, State};
handle_info(_Info, State) ->
    lager:debug("got info ~p", [_Info]),
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


