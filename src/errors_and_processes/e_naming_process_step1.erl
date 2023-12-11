%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 12월 2023 오후 3:37
%%%-------------------------------------------------------------------
-module(e_naming_process_step1).
-compile(export_all).

% 이 경우 critic 프로세스가 죽었을 때, 살려주는 녀석이 없음.
start_critic() ->
  spawn(?MODULE, critic, []).

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

