%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 12월 2023 오전 11:32
%%%-------------------------------------------------------------------
-module(event_server).
-compile(export_all).


-record(state, {
  events, % [#event, #event, #event, ...]
  clients % [Pid, Pid, Pid, ...]
}).

-record(event, {
  name="",
  description="",
  pid,
  timeout={{1970,1,1},{0,0,0}}}).


start() ->
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.

start_link() ->
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
  Pid.

terminate() ->
  ?MODULE ! shutdown.



init() ->
  loop(#state{events=orddict:new(), clients = orddict:new()}).


loop(State) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      Ref = erlang:monitor(process, Pid),
      NewClients = orddict:store(Ref, Client, State#state.clients),
      Pid ! {MsgRef, ok},
      loop(State#state{clients=NewClients});
    {Pid, MsgRef, {add, Name, Description, Timeout}} ->
      case valid_datetime(Timeout) of
        true ->
          EventPid = event:start_link(Name, Timeout),
          NewEvents = orddict:store(Name,
                                    #event{name=Name,
                                            description = Description,
                                            pid=EventPid,
                                            timeout=Timeout},
                                    State#state.events),
          Pid ! {MsgRef, ok},
          loop(State#state{events=NewEvents});
        false ->
          Pid ! {MsgRef, {error, bad_timeout}},
          loop(State)
      end;
    {Pid, MsgRef, {cancel, Name}} ->
      Events = case orddict:find(Name, State#state.events) of
                 {ok, E} ->
                   event:cancel(E#event.pid),
                   orddict:erase(Name, State#state.events);
                 error ->
                   State#state.events
               end,
      Pid ! {MsgRef, ok},
      loop(State#state{events = Events});
    {done, Name} ->
      case orddict:find(Name, State#state.events) of
        {ok, E} ->
          send_to_clients({done, E#event.name, E#event.description},
            State#state.clients),
          NewEvents = orddict:erase(Name, State#state.events),
          loop(State#state{events=NewEvents});
        error ->
          loop(State)
      end;
    shutdown ->
      exit(shutdown);
    {'DOWN', Ref, process, _Pid, _Reason} ->
      loop(State#state{clients = orddict:erase(Ref, State#state.clients)});
    code_change ->
      ?MODULE:loop(State);
    Unknown ->
     io:format("Unknown message: ~p~n", [Unknown]),
      loop(state)
  end.


valid_datetime({Date, Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause -> % not in  {{Y,M,D}, {H, Min, S}} format
      false
  end;
valid_datetime(_) ->
  false.

valid_time({H, M, S}) -> valid_time(H, M, S).

valid_time(H, M, S)
  when
      H >=0, H < 24,
      M >=0, M < 60,
      S >=0, S < 60 -> true;
valid_time(_, _, _) ->  false.

send_to_clients(Msg, ClientDict) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).



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
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Description, Timeout}},
  receive
    {Ref, Msg} -> Msg
  after 5000 ->
    timeout
  end.


cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.


listen(Delay) ->
  receive
    M = {done, _Name, _Description} ->
      Now = calendar:local_time(),
      io:format("God Message time ~p ~n.", [Now]),
      [M|listen(0)]
  after Delay*1000 ->
    []
  end.



