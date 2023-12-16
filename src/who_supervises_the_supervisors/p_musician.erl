%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 12월 2023 오후 11:30
%%%-------------------------------------------------------------------
-module(p_musician).
-behavior(gen_server).


-record(state,
  {
    name="",
    role,
    skill=good}).
-define(DELAY, 750).

%% API
-export([start/2, start_link/2, stop/1, init/1, code_change/3, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

start_link(Role, Skill) ->
  gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

start(Role, Skill) ->
  gen_server:start({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) -> gen_server:call(Role, stop).

init([Role, Skill]) ->
%%  process_flag(trap_exit, true),
  random:seed(now()),
  TimeToPlay = random:uniform(3000),
  Name = pick_name(),
  StrRole = atom_to_list(Role),
  io:format("Musician ~s, playing the ~s entered the room~n", [Name, StrRole]),
  io:format("self : ~p~n", [self()]),
  {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.


handle_call(stop, _From, S) ->
  {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
  {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
  {noreply, S, ?DELAY}.

handle_info(timeout, S = #state{name=N, skill=good}) ->
  print_ok(N),
  {noreply, S, ?DELAY};
handle_info(timeout, S = #state{name=N, skill=bad}) ->
  case rand:uniform(5) of
    1 ->
      io:format("~s played a false note. Uh oh. ~n", [N]),
      {stop, bad_note, S};
    _ ->
      print_ok(N),
      {noreply, S, ?DELAY}
  end;
handle_info(_Message, S) ->
  {noreply, S, ?DELAY}.



terminate(normal, S) ->
  io:format("~s left the room (~s). ~n", [S#state.name, S#state.role]);
terminate(bad_note, S) ->
  io:format("~s sucks! kicked that member out of the band! (~s). ~n)",[S#state.name, S#state.role]);
terminate(shutdown, S) ->
  io:format("The manager is mad and fired the whole band!
      ~s just got back to playing in the subway. ~n", [S#state.name]);
terminate(_Reason, S) ->
  io:format("~s has been kicked out (~s). ~n", [S#state.name, S#state.role]).


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Private function
print_ok(N) ->
  io:format("~s produced sound!~n", [N]).

pick_name() ->
  lists:nth(rand:uniform(10), firstnames())
  ++ " " ++
    lists:nth(rand:uniform(10), lastnames()).


firstnames() ->
  ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
    "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"].

lastnames() ->
  ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
    "Terese", "Tennelli", "Jamal", "Li", "Perlstein"].