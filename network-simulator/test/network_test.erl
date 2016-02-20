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

-define(TEST_NE_NAME, "test-ne").
-define(NE_NAME, ne_name).
-define(TEST_NE_ID, list_to_atom(?TEST_NE_NAME)).

all_tests() ->
  try
    io:format("#########################################~n", []),
    io:format("#### OPTICAL NETWORK SIMULATOR TESTS ####~n"),
    io:format("#########################################~n"),
    all_tests0(),
    io:format("ALL TESTS FINISHED~n"),
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
    ok = test_create_ne(),
    ok = test_remove_ne(),
    passed = ne_device_test:all_tests(),
    passed.

test_create_ne() ->
  {ok, NePid} = network:add_ne([{?NE_NAME, ?TEST_NE_NAME}, {ne_type, test_ne_type}]),
  io:format("NE Created. ChildSpec = ~p~n", [NePid]),
  ok.

test_remove_ne() ->
  ok = network:stop_ne(?TEST_NE_ID),
  ok = network:remove_ne(?TEST_NE_ID),
  io:format("NE ~w stoped and removed from Network. ~n", [?TEST_NE_ID]),
  ok.

%%%%% Setup
setup_network() ->
  io:format("~~ SETUP NETWORK [with 2 elements]~~~n"),
  network:start(normal, [{ne_list, [
    [{ne_name , "default-ne"} , {ne_type, default}],
    [{ne_name , "empty-ne"}   , {ne_type, empty}]
  ]}]),
  ok.

check_network() ->
  2 = network:ne_count(),
  2 = length(network:list_all()),
  ok.