%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 12월 2023 오전 9:59
%%%-------------------------------------------------------------------
-module(ppool_worker_sup).
-behaviour(supervisor).

%% API
-export([]).

start_link(MFA = {_,_,_}) ->
  supervisor:start_link(?MODULE, MFA).


init({M, F, A}) ->
  SupervisorSpec =
    #{
      strategy => simple_one_for_one,
      intensity => 5,
      period => 3600
    },
  ChildSpecs =
  [
    #{
      id => ppool_worker,
      start => {M, F, A},
      restart => temporary,
      shutdown => 5000,
      type => worker,
      modules => [M]
    }
  ],
  {ok, {SupervisorSpec, ChildSpecs}}.
