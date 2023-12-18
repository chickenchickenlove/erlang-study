%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 12월 2023 오후 9:05
%%%-------------------------------------------------------------------
-module(pool_worker_sup).
-behaviour(supervisor).


%% API
-export([start_link/1, init/1]).

start_link(MFA={_, _, _}) ->
  supervisor:start_link(?MODULE, MFA).

init({M, F, A}) ->
  SupervisorSpec =
    #{
      strategy => simple_one_for_one, % Worker 프로세스는는 동적으로 생성됨.
      intensity => 5,
      period => 3600
    },
  ChildSpecs =
    [
      #{
        id => pool_worker,
        start => {M, F, A},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [M]
      }
    ],
  {ok, {SupervisorSpec, ChildSpecs}}.

