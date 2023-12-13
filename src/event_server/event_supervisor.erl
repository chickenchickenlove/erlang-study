%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 12월 2023 오후 12:34
%%%-------------------------------------------------------------------
-module(event_supervisor).
-compile(export_all).

start(Mod, Args) ->
  spawn(?MODULE, init, [{Mod, Args}]).

start_link(Mod, Args) ->
  spawn_link(?MODULE, init, [{Mod, Args}]).


init({Mod, Args}) ->
  io:format("Supervisor PID ~p~n", [self()]),
  process_flag(trap_exit, true),
  loop({Mod, start_link, Args}).

loop({M, F, A}) ->
  Pid = apply(M, F, A),
  receive
    {'EXIT', _From, shutdown} ->
      exit(shutdown);
    {'EXIT', Pid, Reason} ->
      io:format("Process ~p exited for reason ~p~n", [Pid, Reason]),
      loop({M, F, A})
  end.