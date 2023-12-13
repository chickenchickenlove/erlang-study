%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 12월 2023 오후 12:55
%%%-------------------------------------------------------------------
-module(my_event_server).
-author("ojt90").

%% API
-export([]).

-record(state, {events, clients}).
-record(event, {server, description="", pid, name="", to_go=0}).

init() ->
  loop(#state{
    events=orddict:new(),
    clients=orddict:new()}).


loop(State) ->
  receive
    %% From Client
    {Pid, MsgRef, {subscribe, Client}} ->
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, State#state.clients),
      Pid ! {MsgRef, ok},
      loop(State#state{clients=NewClients});
    {Pid, MsgRef, {add, Name, Description, Timeout}} ->
      EventPid = event:start(Name, Timeout),
      NewEvents = orddict:store(
        Name,
        #event{name=Name, description=Description, pid=EventPid, to_go=Timeout},
        State#state.events),
      Pid ! {MsgRef, ok},
      loop(State#state{events=NewEvents});
    {Pid, MsgRef, {cancel, Name}} ->
      NewEvents = case orddict:find(Name, State#state.events) of
        {ok, E} ->
          event:cancel(E#event.pid),
          Pid ! {MsgRef, ok},
          orddict:erase(Name, State#state.events);
        error ->
          Pid ! {MsgRef, ok},
          State#state.events
      end,
      loop(State#state{events=NewEvents});
    shutdown ->
      exit(shutdown);
    %% From Event
    {done, Name} ->
      case orddict:find(Name, State#state.events) of
        {ok, E} ->
          send_to_clients({
            done,
            E#event.name,
            E#event.description,
            State#state.clients}),
          NewEvents = orddict:erase(Name, State#state.events),
          loop(State#state{events=NewEvents});
        error ->
          loop(State)
      end;
    {'DOWN', Ref, process, _Pid, _Reason} ->
      NewClients = orddict:erase(Ref, State#state.clients),
      loop(State#state{clients=NewClients});
    code_change ->
      ?MODULE:loop(State);
    Unknown ->
      io:format("Unknown Message come. ~p~n", [Unknown]),
      loop(State)
  end

send_to_clients(Msg, ClientDict) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).




start() ->
  Pid = spawn(?MODULE, init, []),
  register(?MODULE, Pid),
  Pid.

start_link() ->
  Pid = spawn_link(?MODULE, init, []),
  register(?MODULE, Pid),
  Pid.

terminate() ->
  ?MODULE ! shutdown.


subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(), Ref, {subscribe, Pid}},
  receive
    {Ref, ok} ->
      {ok, Ref};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {error, Reason}
  after 5000 ->
    {error, timeout}
  end.


add_event(Name, Description, Timeout) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(), Ref, {add, Name, Description, Timeout}},
  receive
    {Ref, Msg} -> Msg
  after 5000 ->
    {error, timeout}
  end.


cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.


% For listen notification
listen(Delay) ->
  receive
    M = {done, _Name, _Description} ->
      [M | listen(0)]
  after Delay ->
    []
  end.
