%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 12월 2023 오전 11:17
%%%-------------------------------------------------------------------
-module(erlcount_sup).
-author("ojt90").
-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).


start_link() ->
  supervisor:start_link(?MODULE, []).

init([]) ->
  MaxRestart = 5,
  MaxTime = 100,
  SupFlags =
    #{
      strategy => one_for_one,
      intensity => MaxRestart,
      period => MaxTime},
  ChildrenSpec =
    [
      #{
        id => dispatch,
        start => {erlcount_dispatch, start_link, []},
        restart => transient,
        shutdown => 60000,
        type => worker,
        modules => [erlcount_dispatch]
      }
    ],
    {ok, {SupFlags, ChildrenSpec}}.