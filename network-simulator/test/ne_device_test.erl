%%%-------------------------------------------------------------------
%%% @author RafalW
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2016 14:23
%%%-------------------------------------------------------------------
-module(ne_device_test).
-author("RafalW").

%% API
-export([all_tests/0]).

-define(Attrs, #{attr1 => "val 1", attr2 => "val 2"}).
-define(TestAttr, #{attr3 => "val 3"}).


all_tests() ->
  io:format("~~ NE DEVICE UNIT TESTS ~n"),
  {ok, NePid} = test_create_ne(),
  ok = test_attributes(NePid),
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
