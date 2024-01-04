%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 12월 2023 오후 4:54
%%%-------------------------------------------------------------------
-module(dog_fsm).
-author("ojt90").

%% API
-export([start/0, squirrel/1, pet/1]).

start() ->
  spawn(fun() -> bark() end).


% Request API
squirrel(Pid) -> Pid ! squirrel.

pet(Pid) -> Pid ! pet.

% State Local Method.
bark() ->
  io:format("Dog says: BARK! BARK!~n"),
  receive
    pet -> wag_tail();
    _   ->
      io:format("Dog is confused~n"),
      bark()
  after 2000 ->
    bark()
  end.

wag_tail() ->
  io:format("Dog wags its tail~n"),
  receive
    pet -> sit();
    _ ->
      io:format("Dog is confused~n"),
      wag_tail()
  after 30000 ->
    bark()
  end.

sit() ->
  io:format("Dog is sitting. Gooooooood boy! ~n"),
  receive
    squirrel -> bark();
    _ ->
      io:format("Dog is confused~n"),
      sit()
  end.