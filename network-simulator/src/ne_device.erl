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
-import(equipment,[]).

%% API
-export([start_link/1, get_all_plugs/1, get_plug/2, update_plug/2, remove_plug/2, add_plug/2, remove_all_plugs/1,
  add_attr/2, get_attr/2, update_attr/2]).
-export([subscribe/2, get_events/2, flush_log_event/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_EVENT_LOG_SIZE, 100).

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
init(InitState) -> %% when is_record(InitState, state)
  io:format("Ne Device Started with initial state: ~p~n", [InitState]),
  {ok, InitState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
%% adds or modify(overwrite) existing attributes
handle_call({update_attributes, Attrs}, _From, S) ->
  {reply, ok, S#state{attr = maps:merge(S#state.attr, Attrs)}};
%% replace whole attributes map
handle_call({replace_attributes, Attrs}, _From, S) ->
  {reply, ok, S#state{attr = Attrs}};

%% gets values for all keys . Any key in Keys that does not exist in Attributes Map are ignored.
handle_call({get_attributes, []}, _From, S) ->
  {reply, S#state.attr, S};
handle_call({get_attributes, Keys}, _From, S) ->
  {reply, maps:with(Keys,S#state.attr), S};

handle_call({add_plug, P}, _From, S) ->
  Event = {S#state.eventLogId, plug_added}, %% Create Event

  lists:foreach(fun(Pid) -> Pid ! {self(), Event} end, S#state.controlPorts), %% Send Event to all subscribers

  {reply, ok, S#state{  plugs = maps:put(P#plug.id, P, S#state.plugs),
                        eventLog = [Event | S#state.eventLog], %% Update Event Log
                        eventLogId = (S#state.eventLogId + 1) rem ?MAX_EVENT_LOG_SIZE
  }};

handle_call({remove_plugs, []}, _From, S) ->
  {reply, ok, S#state{plugs = #{}}};
handle_call({remove_plugs, Keys}, _From, S) ->
  {reply, ok, S#state{plugs = maps:without(Keys, S#state.plugs)}};


handle_call({get_plugs, []}, _From, S) ->
  {reply, S#state.plugs, S};
handle_call({get_plugs, Keys}, _From, S) ->
  {reply, maps:with(Keys, S#state.plugs), S};

%%% Events
handle_call({get_events, _StartFrom}, _From, S) ->
  {reply, S#state.eventLog, S};

handle_call({add_subscriber, SubscriberPid}, _From, S) ->
  {reply, ok, S#state{controlPorts = [SubscriberPid | S#state.controlPorts]}};

handle_call(flush_log_event, _From, S) ->
  {reply, ok, S#state{eventLog = []}};

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
%%% API
%%%===================================================================

add_attr(Pid, {Key, Value}) ->
  gen_server:call(Pid, {update_attributes, #{Key => Value}});
add_attr(Pid, Attrs) when is_map(Attrs) ->
  gen_server:call(Pid, {update_attributes, Attrs}).
get_attr(Pid, Keys) ->
  gen_server:call(Pid, {get_attributes, Keys}).
update_attr(Pid, AttrMap) ->
  gen_server:call(Pid, {replace_attributes, AttrMap}).

add_plug(Pid, P) when is_record(P, plug) ->
  gen_server:call(Pid, {add_plug, P}).
remove_plug(Pid, P) when is_record(P, plug) ->
  gen_server:call(Pid, {remove_plugs, [P#plug.id]}).
remove_all_plugs(Pid) ->
  gen_server:call(Pid, {remove_plugs, []}).
update_plug(Pid, P) when is_record(P, plug) ->
  gen_server:call(Pid, {remove_plugs, [P#plug.id]}),
  gen_server:call(Pid, {add_plug, P}).
get_plug(Pid, Id) ->
  hd(maps:values(gen_server:call(Pid, {get_plugs, [Id]}))).
get_all_plugs(Pid) ->
  maps:values(gen_server:call(Pid, {get_plugs, []})).

%%% Events
get_events(Pid, StartFrom) when is_integer(StartFrom) ->
  gen_server:call(Pid, {get_events, StartFrom});
get_events(Pid, {Date, Time}) ->
  gen_server:call(Pid, {get_events, {Date, Time}}).

flush_log_event(Pid) ->
  gen_server:call(Pid, flush_log_event).


subscribe(Pid, SubscriberPid) ->
  gen_server:call(Pid, {add_subscriber, SubscriberPid}),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% default list of plugs A, B, C, D
%%default_plugs() ->
%%  PlugA = #plug{id = a},
%%  PlugB = #plug{id = b},
%%  PlugC = #plug{id = c},
%%  PlugD = #plug{id = d},
%%  #{a => PlugA, b => PlugB, c => PlugC, d =>PlugD}.

%% TODO: add a decorator - a function that changes NE state and generates Events on each Change
%% TODO: Update README.md
%% TODO: scripts: automation tests for Mac , and unix
%% TODO Model for Event