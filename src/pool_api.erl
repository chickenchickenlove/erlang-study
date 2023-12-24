%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 12월 2023 오후 11:30
%%%-------------------------------------------------------------------
-module(pool_api).
-behaviour(application).

%% API
%%-export([start_link/0, stop/0, start_pool/3, stop_pool/1, run/2, async_queue/2, sync_queue/2]).
-export([start/2, stop/1, start_pool/3, stop_pool/1, run/2, async_queue/2, sync_queue/2]).

start(normal, _Args) ->
  sup_of_pool_sup:start_link().

stop(_State) ->
  ok.

%%start_link()->
%%  sup_of_pool_sup:start_link().

%%stop() ->
%%  sup_of_pool_sup:stop().

start_pool(Name, Limit, {M, F, A}) ->
  sup_of_pool_sup:start_pool(Name, Limit, {M, F, A}).

stop_pool(Name) ->
  sup_of_pool_sup:stop_pool(Name).

run(Name, Args) ->
  pool_server:run(Name, Args).

sync_queue(Name, Args) ->
  pool_server:sync_queue(Name, Args).

async_queue(Name, Args) ->
  pool_server:async_queue(Name, Args).