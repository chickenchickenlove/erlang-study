%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 12월 2023 오전 9:50
%%%-------------------------------------------------------------------
-module(ppool_supersup).
-behaviour(supervisor).

%% API
-export([]).

start_link() ->
  supervisor:start_link({local, ppool}, ?MODULE, []).

stop() ->
  case whereis(pool) of
    P when is_pid(P) -> exit(P, kill);
    _ -> ok
  end.

init([]) ->
  SupervisorSpec = #{
    strategy => one_for_one,
    intensity => 6,
    period => 3600
  },
  ChildSpecs = [],
  {ok, {SupervisorSpec, ChildSpecs}}.



% Name is pool name.
start_pool(Name, Limit, MFA) ->
  ChildSpec =
    #{
      id => Name,
      start => {ppool_sup, start_link, [Name, Limit, MFA]},
      restart => permanent,
      shutdown => 10500,
      type => supervisor,
      modules => [ppool_sup]
    },
  supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
  supervisor:terminate_child(ppool, Name),
  supervisor:delete_child(ppool, Name).