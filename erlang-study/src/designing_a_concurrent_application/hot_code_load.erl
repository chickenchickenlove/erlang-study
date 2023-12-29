%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 12월 2023 오후 3:30
%%%-------------------------------------------------------------------
-module(hot_code_load).
-author("ojt90").

%% API
-export([start/1, start_external/1, init/1, external_init/1, external_loop/1]).

start(Name) ->
  spawn(?MODULE, init, [Name]).

start_external(Name) ->
  spawn(?MODULE, external_init, [Name]).

init(Name) ->
  loop(Name).

external_init(Name) ->
  external_loop(Name).

loop(Name) ->
  io:format("MyName is ~p. local call. Version is 200.0 ~n", [Name]),
  timer:sleep(5000),
  loop(Name).


external_loop(Name) ->
  io:format("MyName is ~p. Externa call. Version is 200.0~n ", [Name]),
  timer:sleep(5000),
  ?MODULE:external_loop(Name).

