%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2016 14:23
%%%-------------------------------------------------------------------
-module(ne_device_test).
-author("Rafal Wolak").

-include("../src/network.hrl").

%% API
-export([all_tests/0]).

-define(Attrs, #{attr1 => "val 1", attr2 => "val 2"}).
-define(TestAttr, #{attr3 => "val 3"}).


all_tests() ->
  io:format("~~ NE DEVICE UNIT TESTS ~n"),
  {ok, NePid} = test_create_ne(),
  ok = test_attributes(NePid),
  ok = test_plugs(NePid),
  io:format("~~ NE DEVICE UNIT TESTS FINISHED ~n"),
  passed.

test_create_ne() ->
  io:format("#1 Create NE process (gen_serv) ~n"),
  {ok, Pid} = ne_device:start_link(?Attrs),
  io:format(" Test NE process created: ~p ~n", [Pid]),
  {ok, Pid}.

test_attributes(NePid) ->
  io:format("#2 Test NE Attributes manipulation ~n"),
  ok = gen_server:call(NePid, {update_attributes, ?TestAttr}),
  AllAttrs = maps:merge(?Attrs, ?TestAttr),
  AllAttrs = gen_server:call(NePid, {get_attributes, []}),
  #{attr3 := "val 3"} = gen_server:call(NePid, {get_attributes, [attr3]}),
  ok = gen_server:call(NePid, {replace_attributes, ?TestAttr}),
  #{attr3 := "val 3"} = gen_server:call(NePid, {get_attributes, [attr3]}),
  ok = gen_server:call(NePid, {replace_attributes, #{}}),
  #{} = gen_server:call(NePid, {get_attributes, []}),
  ok.

test_plugs(NePid) ->
  io:format("#3 Test NE Plugs manipulation ~n"),
  ok = ne_device:remove_all_plugs(NePid),
  Plg1 = #plug{id = xpf, in = "fake PID", out = ""},
  Plg2 = #plug{id = xcc},
  ok = ne_device:add_plug(NePid, Plg1),
  Plg1 = ne_device:get_plug(NePid, xpf),
  ok = ne_device:add_plug(NePid, Plg2),
  Plg2 = ne_device:get_plug(NePid, xcc),
  [Plg2, Plg1] = ne_device:get_all_plugs(NePid),
  ok = ne_device:remove_plug(NePid, Plg2),
  ok = ne_device:remove_plug(NePid, Plg1),
  [] = ne_device:get_all_plugs(NePid),
  ok.
