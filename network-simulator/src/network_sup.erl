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

-define(SUP_NAME, ?MODULE).

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
  supervisor:start_link({local, ?SUP_NAME}, ?MODULE, StartArgs).

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
init([{ne_list, NeList}]) ->
  RestartStrategy = one_for_one, %%  a simplified one_for_one supervisor, where all child processes
                                        %% are dynamically added instances of the same process type,
                                        %%  i.e. running the same code.
  MaxRestarts = 3,
  MaxSecondsBetweenRestarts = 1,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent, %% a transient child process will be restarted only if it terminates abnormally,
                       %% i.e. with another exit reason than normal, shutdown or {shutdown,Term}
                       %% We do not restart of ne device
                       %% it should be handled by operators/administrators manually
  Shutdown = 2000,     % waits 2 sec for children exit signal with reason 'shutdown'
  Type = worker,

  ChildSpecs = lists:map( fun(NEAttr) ->
                            {list_to_atom(maps:get(ne_name,NEAttr)), {ne_device, start_link, [NEAttr]}, Restart, Shutdown, Type, [ne_device]}
                          end,
                          NeList),

  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
