%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 12월 2023 오후 5:02
%%%-------------------------------------------------------------------
-module(timeout_case).

%% API
-export([start/1, store/2, take/2, fridge/1]).

start(FoodList) ->
  spawn(?MODULE, fridge, [FoodList]).

store(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  after 5000 ->
    timeout
  end.

take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  after 5000 ->
    timeout_here
  end.

fridge(FoodList) ->

  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge([Food|FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
        true ->
          From ! {self(), {ok, Food}},
          fridge(lists:delete(Food, FoodList));
        false ->
          From ! {self(), not_found},
          fridge(FoodList)
      end;
    terminate ->
      ok
  end.