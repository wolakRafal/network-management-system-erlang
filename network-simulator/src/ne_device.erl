%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo
%%% @doc
%%%
%%% @end
%%% Created : 12. sty 2016 20:26
%%%-------------------------------------------------------------------
-module(ne_device).
-author("Rafal Wolak").

-behaviour(gen_server).

-include("network.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {attr          = #{} :: map(),     %% Attributes of device, KV store
                plugs         = []  :: list(),    %% list of equipped plugs (contain record #plug)
                controlPorts  = []  :: pid(),     %% List of control ports (PIDs), e.g where to send events from event log
                routingTable  = #{} :: map(),     %% routing table
                eventLog      = []  :: list(),    %% Log with all events on device, limited list
                eventLogId    = 0   :: integer()  %% Last Event Log Id
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []). % this process has no name thus  is not registered

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
-spec(init(InitState :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(InitState) ->
  io:format("Ne Device Started: ~p~n", [InitState]),
  {ok, #state{attr = maps:from_list(InitState), plugs = default_plugs()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
%% adds or modify(overwrite) existing attributes
handle_call({update_attributes, Attr}, _From, S) ->
  {reply, ok, S#state{attr = maps:merge(S#state.attr, Attr)}};
%% replace whole attributes map
handle_call({replace_attributes, Attr}, _From, S) ->
  {reply, ok, S#state{attr = Attr}};

%% gets values for all keys . Any key in Keys that does not exist in Attributes Map are ignored.
handle_call({get_attributes, Keys}, _From, S) ->
  {reply, maps:with(Keys,S), S};


handle_call({add_plug, _Plug}, _From, State) ->
  {reply, ok, State};

handle_call({remove_plug}, _From, State) ->
  {reply, ok, State};

handle_call({update_plug}, _From, State) ->
  {reply, ok, State};

handle_call({register_for_events}, _From, State) ->
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({heart_beat}, State) ->
  io:format("Handle Info: ~p~n", [State]),
  {noreply, State};

handle_info({packet, Packet}, State) ->
  io:format("Handle Info: Receive packet ~p~n~p~n", [Packet, State]),
  {noreply, State};

handle_info(_Info, State) ->
  io:format("Handle Info: ~p~n", [State]),
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% default list of plugs A, B, C, D
default_plugs() ->
  PlugA = #plug{id = a},
  PlugB = #plug{id = b},
  PlugC = #plug{id = c},
  PlugD = #plug{id = d},
  [PlugA, PlugB, PlugC, PlugD].