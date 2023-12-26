%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 12월 2023 오후 5:13
%%%-------------------------------------------------------------------
-module(cat_fsm).
-author("ojt90").
-behaviour(gen_fsm).


%% API
-export([init/1, handle_event/3, handle_sync_event/4]).
-export([event/2, dont_give_crap/2, dont_give_crap/3]).


init([]) ->
  {ok, dont_give_crap, []}.

handle_event(_Event, _StateName, _StateData) ->
  erlang:error(not_implemented).

handle_sync_event(_Event, _From, _StateName, _StateData) ->
  erlang:error(not_implemented).


%%% StateName Callback.
dont_give_crap(_Event, From, State) ->
  io:format("Switching to 'dont_give_crap' state~n"),
  gen_fsm:reply(From, meh),
  {next_state, dont_give_crap, State}.


%%% State API. 기존 Cat API가 동기로 처리됨.
event(Pid, Event) ->
  gen_fsm:sync_send_event(Pid, Event, 5000).