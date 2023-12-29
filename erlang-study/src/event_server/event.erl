%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 12월 2023 오전 11:07
%%%-------------------------------------------------------------------
-module(event).
-compile(export_all).
-record(state, {
  server,
  name="",
  to_go=0}
).


start(EventName, Datetime) ->
  spawn(?MODULE, init, [self(), EventName, Datetime]).

start_link(EventName, Datetime) ->
  spawn_link(?MODULE, init, [self(), EventName, Datetime]).


init(Server, EventName, Datetime) ->
  loop(#state{server=Server, name=EventName, to_go = time_to_go(Datetime)}).



loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T*1000 ->
    if Next =:= [] ->
      Server ! {done, S#state.name};
      Next =/= [] ->
        loop(S#state{to_go = Next})
    end
  end.


cancel(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      io:format("cancel successfully~n"),
      erlang:demonitor(Ref);
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

normalize(N) ->
  Limit = 49*24*60*60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].


time_to_go(TimeOut={
  {_,_,_},
  {_,_,_}
}) ->
  Now = calendar:local_time(),
  Togo = (calendar:datetime_to_gregorian_seconds(TimeOut)
    - calendar:datetime_to_gregorian_seconds(Now)),
  Secs = case Togo > 0 of
           true -> Togo;
           false -> 0
         end,
  io:format("Secs ~p~n", [Secs]),
  normalize(Secs).



