%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 12월 2023 오전 9:57
%%%-------------------------------------------------------------------
-module(ppool_sup).
-behaviour(supervisor).


%% API
-export([]).

start_link(Name, Limit, MFA) ->
  supervisor:start_link(?MODULE, {Name, Limit, MFA}).


init({Name, Limit, MFA}) ->
  SupervisorSpec =
    #{
      strategy => one_for_all,
      intensity => 1,
      period => 3600
    },
  ChildSpecs =
    [
      #{
        id => Name,
        start => {ppool_serv, start_link, [Name, Limit, self(), MFA]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [ppool_serv]
      }
    ],
  {ok, {SupervisorSpec, ChildSpecs}}.
