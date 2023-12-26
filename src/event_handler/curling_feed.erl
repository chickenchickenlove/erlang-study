%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 12월 2023 오전 11:12
%%%-------------------------------------------------------------------
-module(curling_feed).
-author("ojt90").
-behaviour(gen_event).

-export([init/1, handle_info/2, handle_call/2, handle_event/2, code_change/3, terminate/2]).

init([Pid]) ->
  io:format("init called.~n"),
  {ok, Pid}.

handle_event(Event, Pid) ->
  io:format("Handle Event Called.~n"),
  Pid ! {curling_feed, Event},
  {ok, Pid}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.


