%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 12월 2023 오후 9:01
%%%-------------------------------------------------------------------
-module(pool_sup).
-behaviour(supervisor).


%% API
-export([start_link/3, init/1]).

start_link(Name, Limit, MFA) ->
  supervisor:start_link(?MODULE, {Name, Limit, MFA}).

init({Name, Limit, MFA}) ->
  SupervisorSpec =
    #{
      strategy => one_for_all, % Pool Server, Worker Supervisor 중 하나라도 죽으면 문제.
      intensity => 1,
      period => 3600
    },
  ChildSpecs =
    [
      #{
        id => Name,
        start => {ppool_server, start_link, [Name, Limit, self(), MFA]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [ppool_server]
      }
    ],
  {ok, {SupervisorSpec, ChildSpecs}}.