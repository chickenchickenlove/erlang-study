%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 12월 2023 오후 6:25
%%%-------------------------------------------------------------------
-module(e_naming_process).
-compile(export_all).

%% API
-export([]).

start_critic() ->
  spawn(?MODULE, supervisor_critic, []).

supervisor_critic() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic, []),
  erlang:register(critic, Pid),
  receive
    {'EXIT', _Pid, normal} -> ok;
    {'EXIT', _Pid, shutdown} -> ok;
    {'EXIT', _Pid, _} ->
      io:format("Critic process died. we restart it. ~n"),
      supervisor_critic()
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

