%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 12월 2023 오후 6:24
%%%-------------------------------------------------------------------
-module(e_trap).
-compile(export_all).



chain(0) ->
  io:format("it will be died~n"),
  receive
    _ -> ok
  after 2000 ->
    exit(kill)
  end;
chain(N) ->
  Pid = spawn(fun() -> chain(N-1) end),
  link(Pid),
  io:format("new Process created. Pid : ~p~n", [Pid]),
  receive
    _ -> ok
  end.

% spawn_link() :  Blocking
% spawn()      :  Non Blocking
system_process(N) ->
  process_flag(trap_exit, true),
  Pid = spawn(?MODULE, chain, [N]),
  link(Pid),
  receive
    X -> X
  end.



