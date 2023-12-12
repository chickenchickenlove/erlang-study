%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 12월 2023 오후 2:08
%%%-------------------------------------------------------------------
-module(kitty_server).
-record(cat, {name, color=green, description}).

%% API
-export([]).


% Client API
start_link() ->
  spawn_link(?MODULE, init, []).


%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, {order, Name, Color, Description}},
  receive
    {Ref, Cat} ->
      erlang:demonitor(Ref, [flush]),
      Cat;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.

close_shop(Pid) ->
  Ref = erlang:demonitor(process, Pid),
  Pid ! {self(), Ref, terminate},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.

% Asynchronous
return_cat(Pid, Cat = #cat{}) ->
  Pid ! {return, Cat},
  ok.



%% Server function.
init() -> loop([]).

loop(Cats) ->
  receive
    {Pid, Ref, {order, Name, Color, Description}} ->
      if
      Cats =:= [] ->
        Pid ! {Ref, make_cat(Name, Color, Description)},
        loop(Cats);
      Cats =/= [] ->
        Pid ! {Ref, hd(Cats)},
        loop(tl(Cats))
      end;
    {return, Cat = #cat{}} ->
      loop([Cat|Cats]);
    {Pid, Ref, terminate} ->
      Pid ! {Ref, ok},
      terminate(Cats);
    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(Cats)
  end.

% private function.
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
  [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
  ok.