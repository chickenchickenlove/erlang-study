%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 12월 2023 오후 3:24
%%%-------------------------------------------------------------------
-module(my_server).
-author("ojt90").

%% API
-export([]).

%% 프레임워크적인 부분만 추상화 해서 둔다.

start(Mod, InitState) ->
  spawn(fun() -> init(Mod, InitState) end).

start_link(Mod, InitState) ->
  spawn_link(fun() -> init(Mod, InitState) end).


init(Mod, InitState) ->
  loop(Mod, Mod:init(InitState)).


loop(Module, State) ->
  receive
    {async, Msg} ->
      loop(Module, Module:handle_cast(Msg, State));
    {sync, From = {_Pid, _Ref}, Msg} ->
      loop(Module, Module:handle_call(Msg, From, State))
  end.

call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {sync, {self(), Ref}, Msg},
  receive
    {Ref, Response} ->
      erlang:demonitor(Ref, [flush]),
      Response;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.

cast(Pid, Msg) ->
  Pid ! {async, self(), Msg},
  ok.

reply({Pid, Ref}, Msg) ->
  Pid ! {Ref, Msg}.