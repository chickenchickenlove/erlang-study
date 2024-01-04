%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 12월 2023 오전 10:56
%%%-------------------------------------------------------------------
-module(kitty_server).

%% API
-export([start_link/0, start/0, init/1]).
-export([handle_info/2, handle_call/3, handle_cast/2]).
-export([order_cat/4, close_shop/1, return_cat/2]).
-record(cat, {name, color=green, description}).

-behaviour(gen_server).

start() ->
  gen_server:start(?MODULE, [], []).

start_link() ->
  gen_server:start_link(?MODULE, [], []).


init(_) ->
  io:format("HERE Init called. self(): ~p ~n",[self()]),
  {ok, []}.


order_cat(Pid, Name, Color, Description) ->
  gen_server:call(Pid, {order, Name, Color, Description}).

close_shop(Pid) ->
  gen_server:call(Pid, {terminate}).

return_cat(Pid, Cat = #cat{}) ->
  gen_server:cast(Pid, {return, Cat}).


handle_call({order, Name, Color, Description}, _From, Cats) ->
  if
    Cats =:= [] ->
      {reply, make_cat(Name, Color, Description), Cats};
    Cats =/= [] ->
      {reply, hd(Cats), tl(Cats)}
  end;
handle_call({terminate}, _From, Cats) ->
  {stop, normal, ok, Cats}.



handle_info(Msg, Cats) ->
  io:format("Unknown message: ~p~n", [Msg]),
  {noreply, Cats}.


handle_cast(_Request, State) ->
  {ok, State}.



terminate(normal, Cats) ->
  [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


% private function.
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.


