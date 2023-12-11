%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 12월 2023 오후 3:47
%%%-------------------------------------------------------------------
-module(e_naming_process_step4).
-compile(export_all).


% 앞에서 발생했던 동시성 문제에 의한 선택적 수신 실패를 해결하기 위해 Reference를 만들어서 전달함.
start_critic() ->
  spawn(?MODULE, restarter, []).

restarter() ->
  process_flag(trap_exit, true),
  erlang:register(critic, spawn_link(?MODULE, critic, [])),
  receive
    {'EXIT', Pid, normal} -> ok;
    {'EXIT', Pid, shutdown} -> ok;
    {'EXIT', Pid, _} -> restarter()
  end.


judge(Band, Album) ->
  Ref = make_ref(),
  critic ! {self(), Ref, {Band, Album}},
  receive
    {Ref, Msg} -> Msg
  after 2000 ->
    timeout
  end.

critic() ->
  receive
    {From, Ref, {"RATM", "A"}} ->
      From ! {Ref, "They are great!"};
    {From, Ref, {"Nirvana", "B"}} ->
      From ! {Ref, "They are good!"};
    {From, Ref, {_Band, _Album}} ->
      From ! {Ref, "is it best?"}
  end.

