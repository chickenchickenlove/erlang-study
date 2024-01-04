%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 12월 2023 오후 5:38
%%%-------------------------------------------------------------------
-module(curling_scoreboard_hw).

%% API
-export([set_teams/2, next_round/0, add_point/1, reset_board/0]).

set_teams(TeamA, TeamB) ->
  io:format("Scoreboard: Team ~s vs. Team ~s~n", [TeamA, TeamB]).

next_round() ->
  io:format("Scoreboard: round over~n").

add_point(Team) ->
  io:format("Scoreboard: increased score of team ~s by 1~n", [Team]).

reset_board() ->
  io:format("Scoreboard: All teams are undefined and all scores ar 0~n").
