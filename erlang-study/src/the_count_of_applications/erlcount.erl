%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 12월 2023 오전 11:22
%%%-------------------------------------------------------------------
-module(erlcount).
-author("ojt90").
-behaviour(application).

%% API
-export([start/2, stop/1]).

start(normal, _Args) ->
  erlcount_sup:start_link().

stop(_State) ->
  ok.