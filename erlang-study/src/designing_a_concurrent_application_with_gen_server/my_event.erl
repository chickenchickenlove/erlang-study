%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 12월 2023 오후 12:19
%%%-------------------------------------------------------------------
-module(my_event).
-author("ojt90").

%% API
-export([loop/1]).
-export([test1/0, test2/0, test3/0]).
-export([start/2, init/3]).
-record(state, {server, name="", to_go=0}).


start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, Delay) ->
  loop(#state{
    server=Server,
    name=EventName,
    to_go=normalize(Delay)}).

cancel(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T * 1000 ->
    if
      Next =:= [] -> Server ! {done, S#state.name};
      Next =/= [] -> loop(S#state{to_go=Next})
    end
  end.

normalize(Time) ->
  Limit = 49*24*60*60,
  [Time rem Limit | lists:duplicate(Time div Limit, Limit)].





%%%% TestCode
test1() ->
  % Given :
  spawn(?MODULE, loop, [#state{server=self(), name="test", to_go=5}]),
  Timeout = 5*1000,
  timer:sleep(Timeout),

  % When + Then
  receive
    Msg -> io:format("received Message : ~p~n",[Msg])
  after Timeout ->
    timeout
  end.

test2() ->
  Pid = spawn(?MODULE, loop, [#state{server=self(), name="test", to_go=500}]),
  ReplyRef = make_ref(),
  Pid ! {self(), ReplyRef, cancel},
  receive
    Msg -> io:format("received Message : ~p~n",[Msg])
  end.

% Next Year Case.
test3() ->
  %% Given
  Pid = spawn(?MODULE, loop, [#state{server=self(), name="test", to_go=365*24*60*60}]),
  timer:sleep(500*1000),
  ReplyRef = make_ref(),
  Pid ! {self(), ReplyRef, cancel},

  %% When + Then
  receive
    Msg -> io:format("received Message : ~p~n",[Msg])
  end.

