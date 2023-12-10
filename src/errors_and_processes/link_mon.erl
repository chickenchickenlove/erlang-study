%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 12월 2023 오후 5:46
%%%-------------------------------------------------------------------
-module(link_mon).

%% API
-export([my_proc/0, chain/1, a1/0, b1/1, c1/1]).

my_proc() ->
  timer:sleep(5000),
  exit(reason).



chain(0) ->
  receive
    _ -> ok
  after 2000 ->
    exit("chain dies here")
  end;
chain(N) ->
  Pid = spawn(?MODULE, chain, [N-1]),
  link(Pid),
  receive
    _ -> ok
  end.


a1() ->
  P1 = spawn(?MODULE, b1, [self()]),
  P2 = spawn(?MODULE, c1, [self()]),
  io:format("a1: ~p, b1: ~p, c1: ~p ~n", [self(), P1, P2]).
b1(Pid) ->
  link(Pid),
  timer:sleep(5000),
  exit(no_answer).
c1(Pid) ->
  link(Pid),
  timer:sleep(2000),
  io:format("c1 alive~n"),
  c1(Pid).