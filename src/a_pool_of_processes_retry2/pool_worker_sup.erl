%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 12월 2023 오후 10:49
%%%-------------------------------------------------------------------
-module(pool_worker_sup).
-behaviour(supervisor).
%% API
-export([init/1, start_link/1]).


start_link(MFA = {_, _, _}) ->
  supervisor:start_link(?MODULE, MFA).


init({M, F, A}) ->
  SupFlags =
    #{strategy => simple_one_for_one,
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
        modules => [pool_server]
      }
    ],
  {ok, {SupFlags, ChildSpecs}}.