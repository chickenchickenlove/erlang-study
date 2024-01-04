%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 12월 2023 오후 3:37
%%%-------------------------------------------------------------------
-module(e_naming_process_step3).
-compile(export_all).

% 이 경우 critic 이름으로 프로세스에 접근할 수 있음.
% 그러나 judge 함수에서 첫번째 라인에서 critic PID를 얻어온 후, 두번째 라인이 실행될 때까지 동시성 이슈를 보장할 수 없음.
% 1. Pid로 패턴 매칭을 통해 선택적 수신을 하고 있는데, 첫번째 ~ 두번째 라인 사이에 critic에 저장된 Pid가 바뀌어서 패턴 매칭이 영원히 안될 수도 있음.
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
  critic ! {self(), {Band, Album}},
  Pid = whereis(critic),
  receive
    {Pid, Msg} -> Msg
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

