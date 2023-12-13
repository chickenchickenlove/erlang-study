%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 12월 2023 오후 11:42
%%%-------------------------------------------------------------------
-module(musicians).
-behavior(gen_server).

%% API
-export([]).
-export([start_link/2, stop/1]).

-record(state, {name="", role, skill=good}).
% Macro. use ?DELAY.
-define(DELAY, 750).

start_link(Role, Skill) ->
  gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) -> gen_server:call(Role, stop).


init([Role, Skill]) ->
  process_flag(trap_exit, true),
  random:seed(now()),
  TimeToPlay = random:uniform(3000),
  Name = pick_name(),
  StrRole = atom_to_list(Role),
  io:format("Musician ~s, playing the ~s entered the room~n", [Name, StrRole]),
  {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.



pick_name() ->
  lists:nth(random:uniform(), firstnames())
  ++ " " ++
  lists:nth(random:uniform(), lastnames()).

firstnames() ->
  ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
    "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"].

lastnames() ->
  ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
    "Terese", "Tennelli", "Jamal", "Li", "Perlstein"].



% Handler
handle_call(stop, _From, S=#state{}) ->
  {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
  {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
  {noreply, S, ?DELAY}.


handle_info(timeout, S = #state{name=N, skill=good}) ->
  io:format("~s produced sound!~n", [N]),
  {noreply, S, ?DELAY};
handle_info(timeout, S = #state{name=N, skill=bad}) ->
  case random:uniform(5) of
    1 ->
      io:format("~s played a false note. Uh oh~n", [N]),
      {stop, bad_note, S};
    _ ->
      io:format("~s produced sound!~n", [N]),
      {noreply, S, ?DELAY}
  end;
handle_info(_Message, S) ->
  {noreply, S, ?DELAY}.




