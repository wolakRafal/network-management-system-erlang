%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo
%%% @doc
%%% Simulator Application. Simulates many network devices.
%%% Can simulate cluster of devices for one single Management Domain (MD)
%%% @end
%%% Created : 12. sty 2016 19:50
%%%-------------------------------------------------------------------
-module(network).
-author("Rafal Wolak").

-behaviour(application).
-include("network.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% admin operations
-export([count_ne/0, shutdown/0]).

%% Application API
-export([list_all/0, get/1, add_ne/1, remove_ne/1, stop_ne/1, ne_count/0, get_ne/1, get_ne_json/1]).

-define(NET_SUP, network_sup).


%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% -------
%% Starts single management domain with set of devices configured in
%% 'simulator.app'.
%% New devices can be added by CLI client or REST.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(normal, StartArgs) ->
  network_sup:start_link(StartArgs).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% API for tests and CLI
%%%===================================================================

%% List all NEs (children) under network supervisor
%%
%% Returns a newly created list with information about all child specifications
%% and child processes belonging to the supervisor SupRef.
list_all() ->
  supervisor:which_children(?NET_SUP).

%% counts all active NE devices
ne_count() ->
  {workers, NeProcCount} = lists:keyfind(workers, 1, supervisor:count_children(?NET_SUP)),
  NeProcCount.

%% Gets Child from network supervisor
get(NeId) ->
  {ok, lists:keyfind(NeId, 1, supervisor:which_children(?NET_SUP))}.


%% Gets NE data, whole #state record
get_ne(NePid) ->
  gen_server:call(NePid, get_data).

%% Gets NE Equipment as a JSON.
get_ne_json(NePid) ->
  erlang:error(not_implemented),
  gen_server:call(NePid, get_data_json).

%% Add Network Element as a child to This optical network
%% Path - path to root JSON file with configuration of ne. It can be multiple files - all hierarchy of eqp
%% return {ok, ChildPid :: child()}
%%
add_ne({file, Path}) ->
  {ok, Binary} = file:read_file(Path),
  Json = jsx:decode(Binary),
  add_ne({json, Json});

%% Add Network Element as a child to This optical network
%% _JsonTerm - Equipment in JSON representation as a Erlang Map
%% return {ok, ChildPid :: child()}
%%
add_ne({json, _JsonTerm} = InitState) ->
  supervisor:start_child(?NET_SUP, [InitState]);

%% Add Network Element as a child to This optical network
%% Takes tuple {NeName:string, NeType:atom()}
%% return {ok, ChildPid :: child()}
%%
add_ne(InitStateList) when is_list(InitStateList) ->
  [supervisor:start_child(?NET_SUP, [InitState]) || InitState <- InitStateList];

add_ne(InitState) ->
  supervisor:start_child(?NET_SUP, [InitState]).

%% stop NE device, TODO this terminates child
stop_ne(NePid) ->
  supervisor:terminate_child(?NET_SUP, NePid).

%% Permanently removes NE device form network
%% return ok.
%% this is called if 'simple_one_for_one' strategy is used
remove_ne(Pid) when is_pid(Pid) ->
  supervisor:terminate_child(?NET_SUP, Pid).

%%%===================================================================
%%% Admin functions, for diagnosis and maintenance
%%%===================================================================

count_ne() ->
  supervisor:count_children(?NET_SUP).

%% Gracefully shuts down top level network supervisor - For tests
shutdown() ->
  exit(whereis(network_sup), normal).

%%%===================================================================
%%% Internal functions
%%%===================================================================
