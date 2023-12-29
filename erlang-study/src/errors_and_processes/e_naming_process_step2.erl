%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 12월 2023 오후 3:37
%%%-------------------------------------------------------------------
-module(e_naming_process_step2).
-compile(export_all).
% 이 경우 critic 프로세스가 죽었을 때, 살려주지만 Pid가 매번 바뀜.
start_critic() ->
  spawn(?MODULE, restarter, []).


restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic, []),
  io:format("critic Process Pid ~p~n", [Pid]),
  receive
    {'EXIT', Pid, normal} -> ok;
    {'EXIT', Pid, shutdown} -> ok;
    {'EXIT', Pid, _} -> restarter()
  end.


judge(Pid, Band, Album) ->
  Pid ! {self(), {Band, Album}},
  receive
    {_From, Msg} -> Msg
  after 2000 ->
    timeout
  end.

critic() ->
  receive
    {From, {"RATM", "A"}} ->
      From ! {self(), "They are great!"};
    {From, {"Nirvana", "B"}} ->
      From ! {self(), "They are good!"};
    {From, {_Band, _Album}} ->
      From ! {self(), "is it best?"}
  end.

