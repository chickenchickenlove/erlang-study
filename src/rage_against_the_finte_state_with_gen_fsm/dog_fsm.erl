%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 12월 2023 오후 5:00
%%%-------------------------------------------------------------------
-module(dog_fsm).
-author("ojt90").
-behaviour(gen_fsm).


%% API
-export([init/1, handle_event/3, handle_sync_event/4]).
-export([bark/2, wag_tail/2, sit/2, handle_info/2]).
-export([squirrel/1, pet/1]).
-export([start_link/0]).

start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

init([]) ->
  {ok, bark, [], 2000}.

handle_event(Event, StateName, StateData) ->
  erlang:error(not_implemented).

handle_sync_event(Event, From, StateName, StateData) ->
  erlang:error(not_implemented).


%%% StateName callback.
bark(pet, State) ->
  io:format("Dog says: Bark! Bark! ~n"),
  {next_state, wag_tail, State, 2000};
bark(_, State) ->
  io:format("Dog is confused. Here ~n"),
  {next_state, bark, State, 2000}.

wag_tail(pet, State) ->
  io:format("Dog wags its tail~n"),
  {next_state, sit, State, 30000};
wag_tail(_, State) ->
  io:format("Dog is confused. ~n"),
  {next_state, wag_tail, State, 30000}.

sit(squirrel, State) ->
  io:format("Dog is sitting. Gooooooood boy! ~n"),
  {next_state, bark, State};
sit(_, State) ->
  io:format("Dog is confused. ~n"),
  {next_state, sit, State}.

%% Timeout -> bark
handle_info(_, State) ->
  {next_state, bark, State}.


%% FSM API : 기존 dog_fsm이 비동기로 처리됨.
squirrel(Pid) ->
  gen_fsm:send_event(Pid, squirrel).

pet(Pid) ->
  gen_fsm:send_event(Pid, pet).