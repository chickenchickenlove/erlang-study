%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 12월 2023 오후 12:00
%%%-------------------------------------------------------------------
-module(test1).
-compile(export_all).

%% API
-export([]).


-record(test2, {name}).

get_test2() ->
  #test2{name="test2"}.

get_name(T = #test2{}) ->
  T#test2.name.
