%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 12월 2023 오후 5:53
%%%-------------------------------------------------------------------
-module(curling).
-author("ojt90").

%% API
-export([start_link/2, set_teams/3, add_points/3, next_round/1, join_feed/2, leave_feed/2, game_info/1]).

%% gen_event:start_link()는 루프문을 도는 erlang 프로세스를 만들어 줌.
%% event loop 프로세스에 특정 모듈을 핸들러로 추가한다.
%% event loop에 notify()로 event를 수신하면, handle_event() 메서드가 호출될 것임.
%% handle_event()에서 패턴 매칭으로 처리될 듯.
%% notify()를 할 때 Pid는 이벤트 루프 Pid임.
%% EventManager에게 EventHandler가 State로 추가되고, 새로운 Event Msg가 전송 -> EventManager가 가지고 있는 모든 EventHandler를 실행해줌.

start_link(TeamA, TeamB) ->
  {ok, Pid} = gen_event:start_link(),
  gen_event:add_handler(Pid, curling_scoreboard, []),
  gen_event:add_handler(Pid, curling_accumulator, []),
  set_teams(Pid, TeamA, TeamB),
  {ok, Pid}.

set_teams(Pid, TeamA, TeamB) ->
  gen_event:notify(Pid, {set_teams, TeamA, TeamB}).

add_points(Pid, Team, N) ->
  gen_event:notify(Pid, {add_points, Team, N}).

next_round(Pid) ->
  gen_event:notify(Pid, next_round).


join_feed(EventManagerPid, ToPid) ->
  HandlerId = {curling_feed, make_ref()},
  gen_event:add_handler(EventManagerPid, HandlerId, [ToPid]),
  HandlerId.

leave_feed(EventManagerPid, HandlerId) ->
  gen_event:delete_handler(EventManagerPid, HandlerId, leave_feed).


game_info(Pid) ->
  gen_event:call(Pid, curling_accumulator, game_data).