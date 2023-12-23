%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 12월 2023 오후 10:56
%%%-------------------------------------------------------------------
-module(pool_server).
-behaviour(gen_server).
-define(SPEC(MFA),
  {worker_sup,
    {pool_worker_sup, start_link, [MFA]},
    temporary,
    10000,
    supervisor,
    [pool_worker_sup]}).


%%start => [pool_server, start_link, [Name, Limit, self(), MFA]],
-record(state, {limit=0, sup, refs=gb_sets:empty(), queue=queue:new()}).

%% API
-export([start/4, start_link/4, init/1]).
-export([run/2, sync_queue/2, async_queue/2, stop/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit)->
  gen_server:start({local, Name}, ?MODULE, {Limit, Sup, MFA}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit)->
  gen_server:start_link({local, Name}, ?MODULE, {Limit, Sup, MFA}, []).


init({Limit, Sup, MFA}) ->
  self() ! {start_worker_supervisor, {Sup, MFA}},
  {ok, #state{limit=Limit, sup=Sup}}.



run(Name, Args) ->
  gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
  gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
  gen_server:cast(Name, {async, Args}).

stop(Name) ->
  gen_server:call(Name, stop).


handle_call({run, Args}, _From, S = #state{limit=N, sup=Sup, refs=Refs}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref, Refs)}};
handle_call({run, _Args}, _From, S = #state{limit=N}) when N =< 0 ->
  {reply, noalloc, S};
handle_call({sync, Args}, _From, S = #state{limit=N, sup=Sup, refs=Refs}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref, Refs)}};
handle_call({sync, Args}, From, S = #state{limit=N, queue = Queue}) when N =< 0 ->
  {noreply, S#state{queue = queue:in({From, Args}, Queue)}};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  {noreply, State}.


handle_cast({async, Args}, S = #state{limit = N, sup = Sup, refs = Refs}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {noreply, S#state{limit = N-1, refs = gb_sets:add(Ref, Refs)}};
handle_cast({async, Args}, S = #state{limit = N, queue = Q}) when N =< 0 ->
  {noreply, S#state{queue = queue:in(Args, Q)}};
handle_cast(_Msg, S) ->
  {noreply, S}.

handle_info({start_worker_supervisor, {Sup, MFA}}, S)->
  {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
  {noreply, S#state{sup = Pid}};
handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs=Refs}) ->
  case gb_sets:is_element(Ref, Refs) of
    true -> handle_down_worker(Ref, S);
    false -> S
  end;
handle_info(Msg, S) ->
  io:format("Unknown Msg : ~p.~n", [Msg]),
  {noreply, S}.

handle_down_worker(Ref, S = #state{limit=L, sup=Sup, refs=Refs, queue=Q}) ->
  case queue:out(Q) of
    % sync case
    {{value, {From, Args}}, Q} ->
      {ok, Pid} = supervisor:start_child(Sup, Args),
      NewRef = erlang:monitor(process, Pid),
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
      gen_server:reply(From, {ok, Pid}),
      {noreply, S#state{refs=NewRefs}};
    {{value, Args}, Q} ->
      {ok, Pid} = supervisor:start_child(Sup, Args),
      NewRef = erlang:monitor(process, Pid),
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
      {noreply, S#state{refs=NewRefs}};
    {empty, _} ->
      {noreply, S#state{limit = L+1, refs = gb_sets:delete(Ref, Refs)}}
  end.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.