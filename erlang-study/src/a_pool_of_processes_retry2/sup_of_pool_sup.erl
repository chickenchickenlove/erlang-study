%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 12월 2023 오후 10:31
%%%-------------------------------------------------------------------
-module(sup_of_pool_sup).
-behaviour(supervisor).


%% API
-export([init/1, start_link/0, start_pool/3, stop_pool/1, stop/1]).

start_link() ->
  supervisor:start_link({local, ppool}, ?MODULE, []).

stop(P) ->
  case whereis(ppool) of
      P when is_pid(P) -> exit(P, kill);
      _                -> ok
  end.


% Each Pool Supervisor
start_pool(Name, Limit, MFA) ->
  ChildSpec =
    #{
      id => Name,
      start => {pool_sup, start_link, [Name, Limit, MFA]},
      restart => permanent,
      shutdown => 10500,
      type => supervisor,
      modules => [pool_sup]
    },
  supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
  supervisor:terminate_child(ppool, Name),
  supervisor:delete_child(ppool, Name).


init([]) ->
  SupFlags =
    #{strategy => one_for_one,
      intensity => 6,
      period => 3600
    },
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

