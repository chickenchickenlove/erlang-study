%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 12월 2023 오후 2:56
%%%-------------------------------------------------------------------
-module(trade_fsm).
-author("ojt90").
-behaviour(gen_fsm).

-record(state, {name="", other, ownItems=[], otherItems=[], monitor, from}).


%% API
-export([start/1, start_link/1, trade/2, accept_trade/1,
  make_offer/2, retract_offer/2, ready/1, cancel/1]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
  terminate/3, code_change/4,
% custom state names
  idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2,
  negotiate/3, wait/2, ready/2, ready/3]).

init(Name) ->
  {ok, idle, #state{name = Name}}.




%% Public API
start(Name) ->
  gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
  gen_fsm:start_link(?MODULE, [Name], []).

% 거래 요청
trade(OwnPid, OtherPid) ->
  gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

% 거래 수락
accept_trade(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, accept_negotiate).

% 아이템 제시
make_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {make_offer, Item}).

% 아이템 회수
retract_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {retract_offer, Item}).

% 거래 준비 완료
ready(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, ready, infinity).

% 거래 취소
cancel(OwnPid) ->
  gen_fsm:sync_send_all_state_event(OwnPid, cancel).

% FSM - FSM Interaction.
ask_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

accept_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

% 클라이언트 오퍼 전달
do_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {do_offer, Item}).

% 클라이언트 오퍼 취소
undo_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {undo_offer, Item}).

are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, are_you_ready).

not_yet(OtherPid) ->
  gen_fsm:send_event(OtherPid, not_yet).

am_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, 'ready!').

ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid, ack).

ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
  gen_fsm:send_all_state_event(OtherPid, cancel).




% Utility Function.
notice(#state{name=N}, Str, Args) ->
  io:format("~s: " ++Str++ "~n", [N|Args]).

unexpected(Msg, State) ->
  io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

add(Item, Items) ->
  [Item | Items].

remove(Item, Items) ->
  Items -- [Item].

priority(OwnPid, OtherPid) ->
  OwnPid > OtherPid.

commit(S=#state{name=Name, ownItems = OwnItems, otherItems = OtherItems}) ->
  io:format(
    "Transaction completed for ~s."
    "Items sent are:~n~p, ~n received are:~n~p.~n"
    "This operation should have some atomic save "
    "In a database. ~n",
    [Name, OwnItems, OtherItems]).


%% State Function
idle({ask_negotiate, OtherPid}, S=#state{}) ->
  Ref = erlang:monitor(process, OtherPid),
  notice(S, "~p asked for a trade negotiation", [OtherPid]),
  {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref}};
idle(Event, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

% Client가 자신의 FSM에게 요청했을 때 처리하는 코드.
idle({negotiate, OtherPid}, From, S=#state{}) ->
  ask_negotiate(OtherPid, self()),
  notice(S, "asking user ~p for a trade", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref, from = From}};
idle(Event, _From, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.


% should solve race condition.
idle_wait({ask_negotiate, OtherPid}, S=#state{other = OtherPid}) ->
  % 나도 상대방에게 응답을 기다리고 있는 상태인데 이걸 받음. Race Condition임.
  % 클라이언트에게 응답. (상대 FSM이 아님)
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};
idle_wait({accept_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  % 클라이언트에게 응답. (상대 FSM이 아님)
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};
idle_wait(Event, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, S=#state{other=OtherPid}) ->
  accept_negotiate(OtherPid, self()),
  notice(S, "accepting negotiation", []),
  {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.


negotiate({make_offer, Item}, S=#state{ownItems=OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S, "offering ~p", [Item]),
  {next_state, negotiate, S#state{ownItems=add(Item, OwnItems)}};
negotiate({retract_offer, Item}, S=#state{ownItems=OwnItems}) ->
  undo_offer(S#state.other, Item),
  notice(S, "cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{ownItems=remove(Item, OwnItems)}};
negotiate({do_offer, Item}, S=#state{ownItems=OwnItems}) ->
  notice(S, "other player offering ~p", [Item]),
  {next_State, negotiate, S#state{ownItems=add(Item, OwnItems)}};
negotiate({undo_offer, Item}, S=#state{ownItems=OwnItems}) ->
  notice(S, "other player cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{ownItems=remove(Item, OwnItems)}};
negotiate(are_you_ready, S=#state{other=OtherPid}) ->
  io:format("Other user ready to tread.~n"),
  notice(S, "Other user ready to transfer goods:~n You get ~p, The other side gets ~p", [S#state.otherItems, S#state.ownItems]),
  not_yet(OtherPid),
  {next_state, negotiate, S};
negotiate(Event, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.

negotiate(ready, From, S = #state{other = OtherPid}) ->
  are_you_ready(OtherPid),
  notice(S, "asking if ready, waiting", []),
  {next_state, wait, S#state{from=From}};
negotiate(Event, _From, S) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, S}.


wait({do_offer, Item}, S=#state{otherItems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other side offering ~p", [Item]),
  {next_state, negotiate, S#state{otherItems=add(Item, OtherItems)}};
wait({undo_offer, Item}, S=#state{otherItems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other side cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{otherItems=remove(Item, OtherItems)}};
wait(are_you_ready, S=#state{other = OtherPid}) ->
  am_ready(OtherPid),
  notice(S, "asked if ready, and I am. Waiting for same reply", []),
  {next_state, wait, S};
wait(not_yet, S=#state{}) ->
  notice(S, "Other not ready yet", []),
  {next_state, wait, S};
wait('ready!', S=#state{other = OtherPid, from = From}) ->
  am_ready(OtherPid),
  ack_trans(OtherPid),
  gen_fsm:reply(From, ok),
  notice(S, "other side is ready. Moving to ready state", []),
  {next_state, ready, S};
wait(Event, Data) ->
  unexpected(Event, wait),
  {next_state, wait, Data}.



ready(ack, S=#state{}) ->
  case priority(self(), S#state.other) of
    true ->
      try
        notice(S, "asking for commit", []),
        ready_commit = ask_commit(S#state.other),
        notice(S, "ordering commit", []),
        ok = do_commit(S#state.other),
        notice(S, "committing...", []),
        commit(S),
        {stop, normal, S}
      catch Class:Reason ->
        notice(S, "commit failed", []),
        {stop, {Class, Reason}, S}
      end;
    false ->
      {next_state, ready, S}
  end;
ready(Event, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.


ready(ask_commit, From, S) ->
  notice(S, "replying to ask_commit", []),
  {reply, ready_commit, ready, S};
ready(do_commit, From, S) ->
  notice(S, "comitting...", []),
  commit(S),
  {stop, normal, ok, S};
ready(Event, _From, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.


handle_event(cancel, _StateName, S=#state{}) ->
  notice(S, "received cancel event", []),
  {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_sync_event(cancel, _From, _StateName, S=#state{}) ->
  notify_cancel(S#state.other),
  notice(S, "cancelling trade, sending cancel event", []),
  {stop, cancelled, ok, S};
handle_sync_event(Event, _From, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.


handle_info({'DOWN', Ref, process, Pid, Reason}, _, S=#state{other=Pid, monitor=Ref}) ->
  notice(S, "Other side dead", []),
  {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName),
  {next_state, StateName, Data}.


code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

terminate(normal, ready, S=#state{}) ->
  notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) ->
  ok.