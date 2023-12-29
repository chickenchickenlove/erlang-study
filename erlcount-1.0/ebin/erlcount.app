%%%-------------------------------------------------------------------
%%% @author ojt90
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 12월 2023 오후 3:31
%%%-------------------------------------------------------------------
{application, erlcount,
  [{vsn, "1.0.0"},
    {modules, [erlcount, erlcount_sup, erlcount_lib,
      erlcount_dispatch, erlcount_counter]},
    {applications, [ppool]},
    {registered, [erlcount]},
    {mod, {erlcount, []}},
    {env,
      [{directory, "."},
        {regex, ["if\\s.+->", "case\\s.+\\sof"]},
        {max_files, 10}]}
  ]}.