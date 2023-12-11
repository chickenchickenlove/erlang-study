%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 12월 2023 오후 6:24
%%%-------------------------------------------------------------------
-module(e_monitor).
-compile(export_all).
%% API
-export([]).

create_monitor() ->
  erlang:monitor(process, spawn(fun() -> timer:sleep(500) end)).

create_dummy_process() ->
  spawn(fun() -> receive X -> X end end).