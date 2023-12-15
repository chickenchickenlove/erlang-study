%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 12월 2023 오후 4:56
%%%-------------------------------------------------------------------
-module(band_supervisor).
-behaviour(supervisor).


%% API
-export([start_link/1]).
-export([init/1]).


start_link(Type) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Type).

init(lenient) ->
  init({one_for_one, 3, 60});
init(angry) ->
  init({rest_for_one, 2, 60});
init(jerk) ->
  init({one_for_all, 1, 60});
init({RestartStrategy, MaxRestart, MaxTime}) ->
  SupervisorFlags =
    #{strategy => RestartStrategy,
      intensity => MaxRestart,
      period => MaxTime},
  ChildSpecs = [
    #{id => singer,
      start => {musicians, start_link, [singer, good]},
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [musicians]},
    #{id => bass,
      start => {musicians, start_link, [bass, good]},
      restart => temporary,
      shutdown => 1000,
      type => worker,
      modules => [musicians]
    },
    #{id => drum,
      start => {musicians, start_link, [drum, good]},
      restart => transient,
      shutdown => 1000,
      type => worker,
      modules => [musicians]
    },
    #{id => guitar,
      start => {musicians, start_link, [guitar, good]},
      restart => transient,
      shutdown => 1000,
      type => worker,
      modules => [musicians]
    }
  ],
  {ok, {SupervisorFlags, ChildSpecs}}.