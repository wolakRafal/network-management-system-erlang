%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo
%%% @doc
%%%
%%% @end
%%% Created : 12. sty 2016 20:25
%%%-------------------------------------------------------------------
-module(network_sup).
-author("Rafal Wolak").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(StartArgs :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(StartArgs) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([{ne_list, [DefaultNeDevice, EmptyNeDevice]}]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 0, %% We do not restarts of ne device
  MaxSecondsBetweenRestarts = 1,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  %% io:format("Ne Device Started: ~p~n", [DefaultNeDevice]),

  io:format("DefaultNeDevice~p~n",[DefaultNeDevice]),
  io:format("EmptyNeDevice~p~n",[EmptyNeDevice]),

  DefaultNeChild = {ne_device, {ne_device, start_link, [DefaultNeDevice]},
    Restart, Shutdown, Type},

  EmptyNeChild = {ne_device_2, {ne_device, start_link, [EmptyNeDevice]},
    Restart, Shutdown, Type},

  {ok, {SupFlags, [DefaultNeChild, EmptyNeChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
