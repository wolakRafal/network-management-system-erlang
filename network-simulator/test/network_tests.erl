%%%-------------------------------------------------------------------
%%% @author RafalW
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2016 14:21
%%%-------------------------------------------------------------------
-module(network_tests).
-author("Rafal Wolak").

-include_lib("eunit/include/eunit.hrl").

-define(TEST_NE_NAME, "test-ne").
-define(TEST_NE_ID, list_to_atom(?TEST_NE_NAME)).

-export([setup_network/0]).

-undef(NODEBUG).

all_test_() ->
  { setup,
    fun setup_network/0,
    fun cleanup/1,
    [
      fun test_check_network/0,
      fun test_create_ne/0,
      fun test_remove_ne/0
    ]
  }.

%%%%% Setup & cleanup
setup_network() ->
  ?debugMsg(" SETUP NETWORK [with 2 elements] "),
  network:start(normal, [{ne_list, [
    #{ne_name => "default-ne" , ne_type => default},
    #{ne_name => "empty-ne"   , ne_type => empty}
  ]}]),
  ok.

cleanup(_) ->
  network:shutdown().

test_check_network() ->
  ?assertEqual(2 , network:ne_count()),
  ?assertEqual(2 , length(network:list_all())),
  ok.

test_create_ne() ->
  {ok, NePid} = network:add_ne(#{ne_name => ?TEST_NE_NAME, ne_type => test_ne_type}),
  ?assert(is_pid(NePid)),
  ok.

test_remove_ne() ->
  ?assertEqual(ok, network:stop_ne(?TEST_NE_ID)),
  ?assertEqual(ok, network:remove_ne(?TEST_NE_ID)),
  io:fwrite(user, "NE ~w stoped and removed from Network. ~n", [?TEST_NE_ID]),
  ok.

