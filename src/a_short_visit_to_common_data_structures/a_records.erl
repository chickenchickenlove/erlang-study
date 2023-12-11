%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 12월 2023 오전 11:13
%%%-------------------------------------------------------------------
-module(a_records).
-compile(export_all).


-record(robot,
  {
    name,
    type=industrial,
    hobbies,
    details = []
  }
).


first_robot() ->
  #robot{
    name="mechatron",
    type=handmade,
    details = ["Move by a small man inside"]}.


car_factory(CorpName) ->
  #robot{
    name=CorpName,
    type=car,
    hobbies = "building cars"
  }.


% details = []인 경우에만 패턴 매칭됨.
% 만약 저렇게 패턴 매칭이 되는 경우라면 Robot의 값에 파라메터를 바인딩한다.
get_hobbies(Robot = #robot{name=Name, type=Type, details=[]}) ->
  io:format("Robot: ~p, Name: ~p, type: ~p~n", [Robot, Name, Type]).


made_robot_and_return_hobbies(Name, Hobbies) ->
  Robot = #robot{name = Name, hobbies = Hobbies},
  Robot#robot.hobbies.



-record(user, {id, name, group, age}).
admin_panel(#user{name=Name, group=admin}) ->
  io:format("~p is allowed ~n", [Name]);
admin_panel(#user{name=Name}) ->
  io:format("~p is not allowed ~n", [Name]).


should_adult(U = #user{}) when U#user.age > 19 ->
  io:format("This is adult. ~n");
should_adult(_) ->
  io:format("This is not adult. ~n").

should_no_detail_robot(Robot = #robot{details=[]}) ->
  io:format("~p has no details at all.~n", [Robot]);
should_no_detail_robot(Robot) ->
  io:format("~p has details. ~n",[Robot]).


% U = #user{name="first_name"}.
% a_records:update_user(U, "UpdatedName").
update_user(User = #user{}, Name) ->
  User#user{name=Name}.


-include("record_header.hrl").
is_included() ->
  #test_record(name="Hello").