%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 12월 2023 오후 2:20
%%%-------------------------------------------------------------------
-module(kitty_server).
-author("ojt90").

-record(cat, {name, color=green, description}).

%% API
-export([]).



%% Server function.
start_link() -> my_server:start_link(?MODULE, []).

init([]) -> [].

order_cat(Pid, Name, Color, Description) ->
  my_server:call(Pid, {order, Name, Color, Description}).

close_shop(Pid) ->
  my_server:call(Pid, terminate).

return_cat(Pid, Cat) ->
  my_server:cast(Pid, {return, Cat}).

handle_call({order, Name, Color, Description}, From, Cats) ->
  if
    Cats =:= [] ->
      my_server:reply(From, make_cat(Name, Color, Description)),
      Cats;
    Cats =/= [] ->
      my_server:reply(From, hd(Cats)),
      tl(Cats)
  end;

handle_call(terminate, From, Cats) ->
  my_server:reply(From, ok),
  terminate(Cats);

handle_call(Unknown, _From, Cats) ->
  io:format("Unknown message: ~p~n", [Unknown]),
  Cats.

handle_cast({return, Cat}, Cats) ->
  [Cat|Cats].

% private function.
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
  [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
  ok.