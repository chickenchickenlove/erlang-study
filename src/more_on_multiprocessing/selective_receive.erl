%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 12월 2023 오후 5:27
%%%-------------------------------------------------------------------
-module(selective_receive).

%% API
-export([important/0]).

% Input : {15, normal}
% self() ! {15, high}, self() ! {7, low}, self() ! {1, low}, self() ! {17, high}.
% selective_receive:important().
% [high,high,low,low]

important() ->
  receive
    {Priority, Message} when Priority > 10 ->
      [Message | important()]
  after 0 ->
    normal()
  end.

normal() ->
  receive
    {_, Message} ->
      [Message|normal()]
  after 0 ->
    []
  end.