%%%-------------------------------------------------------------------
%%% @author RafalW
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2016 14:21
%%%-------------------------------------------------------------------
-module(network_test).
-author("RafalW").


%% API
-export([all_tests/0]).


all_tests() ->
  try
    io:format("#########################################~n", []),
    io:format("#### OPTICAL NETWORK SIMULATOR TESTS ####~n"),
    io:format("#########################################~n"),
    all_tests0(),
    io:format("TESTS FINISHED~n"),
    passed
  catch
    Type:Error ->
      io:format(
        "Tests failed~nError: {~p, ~p}~nStack trace:~n~p~n",
        [Type, Error, erlang:get_stacktrace()])
  end.

all_tests0() ->
    ok = setup_network(),
    ok = check_network(),
%%    passed = test_create_ne(),
    passed = ne_device_test:all_tests(),
    passed.

%%%%% Setup empty network
setup_network() ->
  io:format("~~ SETUP EMPTY NETWORK ~~~n"),
  network:start(normal, [{ne_list, []}]),
  ok.

check_network() ->
  [] = network:list_all(),
  ok.