%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 12월 2023 오후 2:20
%%%-------------------------------------------------------------------
-module(my_server).
-author("ojt90").

%% API
-export([call/2, cast/2]).

start(Mod, InitialState) ->
  spawn(fun() -> init(Mod, InitialState) end).

start_link(Mod, InitialState) ->
  spawn_link(fun() -> init(Mod, InitialState) end).

init(Mod, InitialState) ->
  loop(Mod, Mod:init(InitialState)).

loop(Module, States) ->
  receive
    {sync, Pid, Ref, Msg} ->
      loop(Module, Module:handle_call(Msg, {Pid, Ref}, States));
    {async, Pid, Ref, Msg} ->
      loop(Module, Module:handle_cast(Msg, States))
  end.


call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {sync, self(), Ref, Msg},
  receive
    {Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.

cast(Pid, Msg) ->
  Pid ! {async, Msg},
  ok.

reply({Pid, Ref}, Reply) ->
  Pid ! {Ref, Reply}.
