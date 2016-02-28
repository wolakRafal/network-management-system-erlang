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

%%%%% Setup
setup_network_test() ->
  io:format("~~ SETUP NETWORK [with 2 elements]~~~n"),
  network:start(normal, [{ne_list, [
    #{ne_name => "default-ne" , ne_type => default},
    #{ne_name => "empty-ne"   , ne_type => empty}
  ]}]),
  ok.

check_network_test() ->
  ?assertEqual(2 , network:ne_count()),
  ?assertEqual(2 , length(network:list_all())),
  ok.

create_ne_test() ->
  {ok, NePid} = network:add_ne(#{ne_name => ?TEST_NE_NAME, ne_type => test_ne_type}),
  ?assert(is_pid(NePid)),
  ok.

test_remove_ne_test() ->
  ?assertEqual(ok, network:stop_ne(?TEST_NE_ID)),
  ?assertEqual(ok, network:remove_ne(?TEST_NE_ID)),
  io:format("NE ~w stoped and removed from Network. ~n", [?TEST_NE_ID]),
  ok.

