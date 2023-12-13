%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 12월 2023 오후 1:24
%%%-------------------------------------------------------------------
-module(my_event_server_with_gen_server).
-author("ojt90").
-behaviour(gen_server).

%% API
-export([start/0, start_link/0, handle_call/3, init/1, handle_info/2]).
-export([add_event/4, subscribe/1, send_to_clients/2]).
-record(state, {events, clients}).
-record(event, {server, description="", pid, name="", to_go=0}).

start() ->
  {ok, Pid} = gen_server:start(?MODULE, [], [] ),
  Pid.

start_link() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], [] ),
  Pid.

init(_) ->
  {
    ok,
    #state{events=orddict:new(), clients=orddict:new()}
  }.

subscribe(Pid) ->
  gen_server:call(Pid, {subscribe, self()}).

add_event(Pid, Name, Description, Timeout) ->
  gen_server:call(Pid, {add, Name, Description, Timeout}).


handle_call({subscribe, Client}, _From, State) ->
  Ref = erlang:monitor(process, Client),
  NewClients = orddict:store(Ref, Client, State#state.clients),
  {reply, ok, State#state{clients=NewClients}};

handle_call({add, Name, Description, Timeout}, _From, State) ->
  EventPid = my_event_with_gen_server:start(Name, Timeout),
  NewEvents = orddict:store(
    Name,
    #event{name=Name, description=Description, pid=EventPid, to_go=Timeout},
    State#state.events),
  {reply, ok, State#state{events=NewEvents}};

handle_call({cancel, Name}, _From, State) ->
  NewEvents = case orddict:find(Name, State#state.events) of
                {ok, E} ->
                  event:cancel(E#event.pid),
                  orddict:erase(Name, State#state.events);
                error ->
                  State#state.events
              end,
  {reply, ok, State#state{events=NewEvents}};

% 이거 맞는지 모르겠음.
handle_call(shutdown, _From, State) ->
  {stop, shutdown, State};

handle_call({done, Name}, _From, State) ->
  NewState = case orddict:find(Name, State#state.events) of
                {ok, E} ->
                  send_to_clients({done, E#event.name, E#event.description}, State#state.clients),
                  NewEvents = orddict:erase(Name, State#state.events),
                  State#state{events=NewEvents};
                 error ->
                   State
             end,
  {noreply, NewState};

handle_call({'DOWN', Ref, process, _Pid, _Reason}, _From, State) ->
  NewClients = orddict:erase(Ref, State#state.clients),
  {noreply, State#state{clients=NewClients}}.

handle_info(Msg, State) ->
  io:format("Unknow Msg come. Msg : ~p~n", [Msg]),
  {noreply, State}.

send_to_clients(Msg, ClientDict) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).
