%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2016 14:23
%%%-------------------------------------------------------------------
-module(ne_device_tests).
-author("Rafal Wolak").

-include_lib("eunit/include/eunit.hrl").
-include("../src/network.hrl").
-import(network_tests, [setup_network/0]).

-export([event_counter/1]).

-define(Attrs, #{attr1 => "val 1", attr2 => "val 2"}).
-define(TestAttr, #{attr3 => "val 3"}).


all_test_() ->
%%  io:fwrite(user, "  NE DEVICE UNIT TESTS /n", []),
  {foreach,
    fun setup_network_env/0,  %% setup function
    fun cleanup/1,    %% teardown function
    [  fun test_attributes/1,
       fun test_plugs/1 ,
       fun test_event_log/1
    ]
  }.


setup_network_env() ->
  network_tests:setup_network(),
  io:fwrite(user, "[Setup] Create NE process (gen_serv) \n", []),
  {ok, NePid} = ne_device:start_link(?Attrs),
  io:fwrite(user, " Test NE process created: ~p \n", [NePid]),
  NePid.

cleanup(NePid) ->
  exit(NePid, normal).

test_attributes(NePid) ->
  io:fwrite(user, " Test NE Attributes manipulation \n",[]),
  ?assertEqual(ok, ne_device:add_attr(NePid, ?TestAttr)),
  AllAttrs = maps:merge(?Attrs, ?TestAttr),
  [
    ?_assertEqual(AllAttrs , ne_device:get_attr(NePid, [])),
    ?_assertEqual(#{attr3 => "val 3"}, ne_device:get_attr(NePid, [attr3])),
    ?_assertEqual(ok , ne_device:update_attr(NePid, ?TestAttr)),
    ?_assertEqual(#{attr3 => "val 3"} , ne_device:get_attr(NePid, [attr3])),
    ?_assertEqual(ok , ne_device:update_attr(NePid, #{})),
    ?_assertEqual(#{} , ne_device:get_attr(NePid, []))
  ].

test_plugs(NePid) ->
  io:fwrite(user, " Test NE Plugs manipulation n",[]),
  ok = ne_device:remove_all_plugs(NePid),

  Plg1 = #plug{id = xpf, in = "fake PID", out = ""},
  Plg2 = #plug{id = xcc},

  ok = ne_device:add_plug(NePid, Plg1),
  ?assertEqual(Plg1 , ne_device:get_plug(NePid, xpf)),

  ok = ne_device:add_plug(NePid, Plg2),
  ?assertEqual(Plg2 , ne_device:get_plug(NePid, xcc)),
  ?assertEqual([Plg2, Plg1] , ne_device:get_all_plugs(NePid)),

  ok = ne_device:remove_plug(NePid, Plg2),
  ok = ne_device:remove_plug(NePid, Plg1),
  ?_assertEqual([] , ne_device:get_all_plugs(NePid)).

test_event_log(NePid) ->
  io:fwrite(user, " Test Event Log \n", []),
%%  ne_device:flush_log_event(NePid),

  EventCounterPid = spawn(ne_device_tests, event_counter, [0]),
  ok = ne_device:subscribe(NePid, EventCounterPid),
  Plg2 = #plug{id = xcc},
  ok = ne_device:add_plug(NePid, Plg2), %% This generates Event
  EventCounterPid ! {self(), how_many_events},
  receive
    EventCount ->
      io:fwrite(user, " Number of Events ~p \n", [EventCount]),
      EventCount = 1 %% One Event Expected
  end,
  ?_assertEqual(1 , length(ne_device:get_events(NePid, 0))).

%% test subscriber
event_counter(EventCount) ->
  receive
    {From, how_many_events} ->
      From ! EventCount;
    {From, Event} ->
      io:fwrite(user, " Receive Event No#~p From ~p: ~p \n", [EventCount + 1, From, Event]),
      event_counter(EventCount + 1)
  end.