%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 12월 2023 오전 11:45
%%%-------------------------------------------------------------------
-module(curling_accumulator).
-author("ojt90").
-behaviour(gen_event).

%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {teams=orddict:new(), round=0}).

init([]) ->
  {ok, #state{}}.

handle_event({set_teams, TeamA, TeamB}, S=#state{teams=T}) ->
  Teams = orddict:store(TeamA, 0, orddict:store(TeamB, 0, T)),
  {ok, S#state{teams=Teams}};
handle_event({add_points, Team, N}, S=#state{teams=T}) ->
  Teams = orddict:update_counter(Team, N, T),
  {ok, S#state{teams=Teams}};
handle_event(next_round, S=#state{}) ->
  {ok, S#state{round=S#state.round+1}};
handle_event(_Event, Pid)->
  {ok, Pid}.


handle_call(game_data, S=#state{teams=T, round=R}) ->
  {ok, {orddict:to_list(T), {round, R}}, S};
handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.