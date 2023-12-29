%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 12월 2023 오전 11:30
%%%-------------------------------------------------------------------
-module(erlcount_lib).
-author("ojt90").
-include_lib("kernel/include/file.hrl").

%% API
-export([find_erl/1, regex_count/2]).

find_erl(Directory) ->
  find_erl(Directory, queue:new()).

find_erl(Name, Queue) ->
  {ok, F = #file_info{}} = file:read_file_info(Name),
  case F#file_info.type of
    directory -> handle_directory(Name, Queue);
    regular -> handle_regular_file(Name, Queue);
    _Other -> dequeue_and_run(Queue) % 다른 파일인 경우 무시함.
  end.


handle_directory(Dir, Queue) ->
  case file:list_dir(Dir) of
    {ok, []} -> dequeue_and_run(Queue);
    {ok, Files} -> dequeue_and_run(enqueue_many(Dir, Files, Queue)) % 파일이 존재하면 해당 파일을 대기열에 넣음. 그리고 현재 값은 디큐함.
  end.

handle_regular_file(Name, Queue) ->
  case filename:extension(Name) of
    ".erl" -> {continue, Name, fun() -> dequeue_and_run(Queue) end};
    _NonErl -> dequeue_and_run(Queue)
  end.


dequeue_and_run(Queue) ->
  case queue:out(Queue) of
    {empty, _} -> done;
    {{value, File}, NewQueue} -> find_erl(File, NewQueue)
  end.


enqueue_many(Path, Files, Queue) ->
  F = fun(File, Q) -> queue:in(filename:join(Path,File), Q) end,
  lists:foldl(F, Queue, Files).




% Added after
regex_count(Re, Str) ->
  case re:run(Str, Re, [global]) of
    nomatch -> 0;
    {mathc, List} -> length(List)
  end.



