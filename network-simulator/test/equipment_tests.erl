%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2016 14:11
%%%-------------------------------------------------------------------
-module(equipment_tests).
-author("Rafal Wolak").

%% API
-export([]).


%%-import(jiffy, [decode/1, decode/2, encode/1, encode/2]).

-include_lib("eunit/include/eunit.hrl").
-include("network.hrl").

-define(NE_NAME, "test_ne_name").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(APP_NAME, network).
-define(REG_NAME, network_sup).
-define(BULK_SIZE, 100).

%% Equipment test data
-define(EQUIPMENT_EMPTY, dict:from_list(
  [ {"/mit/me", #eqp{id = "/mit/me", parent = "/mit", children = ["/mit/me/1", "/mit/me/2"]}},
    {"/mit/me/1", #eqp{id = "/mit/me/1", parent = "/mit/me", children = []}},
    {"/mit/me/2", #eqp{id = "/mit/me/2", parent = "/mit/me", children = []}}
  ])).

-define(EQUIPMENT, dict:from_list(
  [ {"/mit/me",   #eqp{id = "/mit/me", parent = "/mit", children = ["/mit/me/1", "/mit/me/2"]}},
    {"/mit/me/1", #eqp{id = "/mit/me/1", parent = "/mit/me", children = []}}
  ])).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

equipment_configuration_test() ->
  {
    "NE can be created with any (possibly empty) Equipment configuration. Eqp should have directory structure (tree)",
    ?setup([fun create_ne_empty_equipment/0,
      fun create_ne_with_equipment/0])
  }.

add_ne_from_JSON_test_() ->
  {
    "NE with Equipment can be created from JSON",
    ?setup([fun basic_JSON_test/0])
  }.

get_ne_by_uri_test() ->
  {"NE can be retreived by uri. (Basic Attributes)"}.

get_all_ne_attributes_by_uri_test() ->
  {"It should be possible to get All NE configuration by uri. (Full Audit Sync)"}.

get_specific_attr_of_ne_by_uri_test() ->
  {"It should be possible to retreive specific attributes of NE configuration by uri. (Partial Audit Sync)"}.

get_subtree_of_ne_test() ->
  {"It should be possible to retreive subtree of attributes. (Partial Audit Sync)"}.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
  ok = application:start(?APP_NAME),
  whereis(?REG_NAME).

stop(_) ->
  application:stop(?APP_NAME).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

create_ne_empty_equipment() ->
  {ok, Pid} = network:add_ne(create_ne_with_eq("Test_NE_Empty_Eqp", ?EQUIPMENT_EMPTY)),
  NeState = network:get_ne(Pid),
  [
    ?_assert(is_record(NeState, state)),
    ?_assertEqual(?EQUIPMENT_EMPTY, NeState#state.equipment)
  ].

create_ne_with_equipment() ->
  {ok, Pid} = network:add_ne(create_ne_with_eq("Test_NE_with_eqp", ?EQUIPMENT)),
  NeState = network:get_ne(Pid),
  [
    ?_assert(is_record(NeState, state)),
    ?_assertEqual(?EQUIPMENT, NeState#state.equipment)
  ].

basic_JSON_test() ->
  io:fwrite(user, "MY LOGG ~p~n", [load_json_file("test/resources/test_1.json")]),
  io:fwrite(user, "MY LOGG2 ~tp~n", [jsx:encode(#{"dog" => "winston", "fish" => "mrs.blub"})]),
  [?_assertEqual(load_json_file("test/resources/test_1.json"), aaaaa)].

equipment_JSON(NePid) ->
  network:add_ne({file, "mit_me_1.json"}),
  ?_assertEqual(load_json_file("mit_me_1.json"), network:get_ne_json(NePid)).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Creates The simplest NE instance
create_NE(NeName)->
  #state{attr = #{ne_name => NeName}}.

create_ne_with_eq(NeName, Equipment) ->
  NE = create_NE(NeName),
  NE#state{equipment = Equipment}.


load_json_file(Path) ->
  {ok, Binary} = file:read_file(Path),
  jsx:decode(Binary).