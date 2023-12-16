%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 12월 2023 오후 6:53
%%%-------------------------------------------------------------------
-module(p_band_supervisor).

%% API
-export([init/1, start_link/1]).
-behavior(supervisor).

start_link(Type) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Type).

init(lenient) ->
  init({one_for_one, 3, 60});
init(angry) ->
  init({rest_for_one, 2, 60});
init(jerk) ->
  init({one_for_all, 1, 60});
init(jamband) ->
  SupervisorFlags =
  #{
    strategy => simple_one_for_one,
    intensity => 3,
    period => 60
  },

  ChildSpecs =
  [
    #{
      id => jam_musician,
      start => {p_musician, start_link, []},
      restart => temporary,
      shutdown => 1000,
      type => worker,
      modules => [p_musician]
    }
  ],
  {ok, {SupervisorFlags, ChildSpecs}};
init({RestartStrategy, MaxRestart, MaxTime}) ->
  SupervisorFlags =
    #{
      strategy => RestartStrategy,
      intensity => MaxRestart,
      period => MaxTime
    },

  ChildSpec = get_child_spec(),
  {ok, {SupervisorFlags, ChildSpec}}.

get_child_spec() ->
  [
    #{
      id => singer,
      start => {p_musician, start_link, [singer, good]},
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [p_musician]
    },
    #{
      id => bass,
      start => {p_musician, start_link, [bass, bad]},
      restart => temporary,
      shutdown => 1000,
      type => worker,
      modules => [p_musician]
    },
    #{
      id => drum,
      start => {p_musician, start_link, [drum, bad]},
      restart => transient,
      shutdown => 1000,
      type => worker,
      modules => [p_musician]
    }].