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

-include("network.hrl").
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
init([{ne_list, _NeList}]) ->
        %%  a simplified one_for_one supervisor, where all child processes
        %% are dynamically added instances of the same process type,
        %%  i.e. running the same code.
  MaxRestarts = 3,
  MaxSecondsBetweenRestarts = 1,

  SupFlags = #{ strategy => simple_one_for_one,
                intensity => MaxRestarts,
                period => MaxSecondsBetweenRestarts},

%%    When started, the supervisor does not start any child processes. Instead, all child processes are added dynamically by calling:
%%      1> supervisor:start_child(Sup, List)

  %% We do not restart ne device
  %% it should be handled by operators/administrators manually or by Network Management System (NMS)
  ChildSpecs = [#{  id => ne_device,
                    start => {ne_device, start_link, []},
                    restart => transient,  %% child process is restarted only if it terminates abnormally,
                                          %%      that is, with another exit reason than normal, shutdown, or {shutdown,Term}.
                    shutdown => brutal_kill   %% child process is unconditionally terminated using exit(Child, kill).
    }],

  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
