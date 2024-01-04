%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 12월 2023 오후 12:00
%%%-------------------------------------------------------------------
-module(test2).
-compile(export_all).

%% API
-export([]).

test_erlang_import() ->
  T = test1:get_test2(),
  test1:get_name(T).


