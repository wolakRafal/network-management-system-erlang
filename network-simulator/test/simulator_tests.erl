%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 04. Mar 2016 17:12
%%%-------------------------------------------------------------------
-module(simulator_tests).
-author("Rafal Wolak").

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(APP_NAME, optical_network).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test() ->
  {
    "The simulated optical network can be started, stopped and has a registered name: optical_network ",
    ?setup(fun is_registered/1)
  }.

start_stop_preset_test() ->
  {
    "The simulated optical network can be started with preset configuration of Network Elements (in network.app)",
    fun is_configured/0
  }.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
  ok = application:start(?APP_NAME),
  whereis(?APP_NAME).

stop(_) ->
  application:stop(?APP_NAME).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
is_registered(Pid)  ->
  [ ?_assert(erlang:is_process_alive(Pid)),
    ?_assertEqual(Pid, whereis(?APP_NAME))].

is_configured() ->
  [].


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%