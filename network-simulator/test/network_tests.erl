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
-include("../include/network.hrl").

-define(TEST_NE_NAME, "test-ne").
-define(TEST_NE_ID, list_to_atom(?TEST_NE_NAME)).

-export([setup_network/0]).

-undef(NODEBUG).


%%%%%%%%%%
%% Deprecated Soon be deleted %%
%%%%%%%%%%


all_test_() ->
  {setup,
    fun setup_network/0,
    fun cleanup/1,
    fun test_check_network/0
  }.

%%%%% Setup & cleanup
setup_network() ->
  ?debugMsg(" SETUP NETWORK [with 2 elements] "),
  network:start(normal, [{ne_list, [ ]}]),
  ok.

cleanup(_) ->
  network:shutdown().

test_check_network() ->
  ?assertEqual(0 , network:ne_count()),
  ?assertEqual(0 , length(network:list_all())),
  ok.

test_send_message_2_nodes() ->
  % 1. create 2 NEs: NE_A & NE_B
  NE_A = network:add_ne(#state{attr =  #{ne_name => "NE A", ne_type => neType}}),
  NE_B = network:add_ne(#state{attr =  #{ne_name => "NE B", ne_type => neType}}),
  Probe = self(), %% TODO finish probe implementation
%%  Probe = spawn(fun () ->
%%                  receive
%%                    get_message ->
%%                  end,
  % 2. create 2 Plugs in each NE: PL_A_1, PL_A_2, PL_B_1, PL_B_2
  % 3. Link PL_A_1 and PL_B_1
  PL_A_1 = #plug{id = 'PL_A_1', out = NE_A},
  PL_A_2 = #plug{id = 'PL_A_2'},
  PL_B_1 = #plug{id = 'PL_B_1', out = NE_B},
  % 4. Set Plug PL_B_2 to send messages to Test Probe Actor
  PL_B_2 = #plug{id = 'PL_B_2', out = Probe},

  % 5. Send Packet Message to PL_A_2
  % 6. Assert that message came to Test Probe Actor
  ok.

test_send_message_via_3_nodes() ->
  ok.

test_connection_alive_between_NE() ->
  % use links
  ok.