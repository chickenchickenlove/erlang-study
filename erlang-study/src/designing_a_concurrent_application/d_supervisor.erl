%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 12월 2023 오후 4:26
%%%-------------------------------------------------------------------
-module(d_supervisor).
-author("ojt90").

%% API
-export([start/2, start_link/2, init/1]).


start(Mod, Args) ->
  spawn(?MODULE, init, [Mod, Args]).

start_link(Mod, Args) ->
  spawn_link(?MODULE, init, [Mod, Args]).

init({Mod, Args}) ->
  process_flag(trap_exit, true),
  loop({Mod, start_link, Args}).

loop({M, F, A}) ->
  Pid = apply(M, F, A),
  receive
    {'EXIT', _From, shutdown} ->
      exit(shutdown);
    {'EXIT', Pid, Reason} ->
      io:format("Process ~p exited for reason ~p~n", [Pid, Reason]),
      loop({M,F,A})
  end.