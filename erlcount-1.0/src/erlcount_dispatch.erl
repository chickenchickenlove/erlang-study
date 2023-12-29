%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 12월 2023 오전 11:52
%%%-------------------------------------------------------------------
-module(erlcount_dispatch).
-author("ojt90").
-behaviour(gen_fsm).
-record(data, {regex=[], refs=[]}).

%% API
-export([start_link/0, complete/4]).
-export([init/1, dispatching/2, listening/2, handle_info/3, handle_event/3, handle_sync_event/4, terminate/3, code_change/4]).
-define(POOL, erlcount).

%% Public API
start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

complete(Pid, Regex, Ref, Count) ->
  gen_fsm:send_all_state_event(Pid, {complete, Regex, Ref, Count}).



init([]) ->
  {ok, Re} = application:get_env(regex),
  {ok, Dir} = application:get_env(directory),
  {ok, MaxFiles} = application:get_env(max_files),
  pool_api:start_pool(?POOL, MaxFiles, {erlcount_counter, start_link, []}),
  case lists:all(fun valid_regex/1, Re) of
    true ->
      self() ! {start, Dir},
      {ok, dispatching, #data{regex=[{R,0} || R <- Re]}}; % 각 정규식에 카운터 추가함.
    false ->
      {stop, invalid_regex}
  end.


valid_regex(Re) ->
  try re:run("", Re) of
    _ -> true
  catch
    error:badarg -> false
  end.


handle_info({start, Dir}, State, Data) ->
  gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir)),
  {next_state, State, Data}.


dispatching({continue, File, Continuation}, Data = #data{regex=Re, refs=Refs}) ->
  F = fun({Regex, _Count}, NewRefs) ->
    Ref = make_ref(),
    pool_api:async_queue(?POOL, [self(), Ref, File, Regex]),
    [Ref|NewRefs]
      end,
  NewRefs = lists:foldl(F, Refs, Re),
  gen_fsm:send_event(self(), Continuation()),
  {next_state, dispatching, Data#data{refs = NewRefs}};
dispatching(done, Data) ->
  listening(done, Data).



listening(done, #data{regex=Re, refs=[]}) -> % all received
  [io:format("Reges ~s has ~p results~n", [R,C]) || {R, C} <- Re],
  {stop, normal, done};
listening(done, Data) ->
  {next_state, listening, Data}.


% Worker에게서 종료 메세지를 수신받은 경우
handle_event({complete, Regex, Ref, Count}, State, Data = #data{regex=Re, refs=Refs}) ->
  {Regex, OldCount} = lists:keyfind(Regex, 1, Re),
  NewRe = lists:keyreplace(Regex, 1, Re, {Regex, OldCount + Count}),
  NewData = Data#data{regex=NewRe, refs=Refs -- [Ref]},
  case State of
    dispatching -> {next_state, dispatching, NewData};
    listening -> listening(done, NewData)
  end.


handle_sync_event(Event, _From, State, Data) ->
  io:format("Unexpected event: ~p~n", [Event]),
  {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
  ok.

code_change(_OldVsn, State, Data, _Extra) ->
  {ok, State, Data}.