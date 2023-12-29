%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 12월 2023 오후 10:40
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
        strategy => one_for_all,
        intensity => 1,
        period => 3600
      },
  ChildSpecs =
    [
      %pool_server
      #{
        id => serv,
        start => {pool_server, start_link, [Name, Limit, self(), MFA]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [pool_server]
      }
    ],
  {ok, {SupervisorSpec, ChildSpecs}}.