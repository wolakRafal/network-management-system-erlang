%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 07. Mar 2016 22:58
%%%-------------------------------------------------------------------
-module(equipment).
-author("Rafal Wolak").

%% Data structure for Equipment Tree representation.
%% It is a Directory structure like LDAP
%% ================
%%

%% API
-export([]).

-define(node, {
                parent,
                id,
                type,
                attributes,
                children
              }).

