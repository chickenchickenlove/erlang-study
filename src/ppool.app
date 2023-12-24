%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 12월 2023 오전 9:24
%%%-------------------------------------------------------------------
{application, ppool, [
  {description, ""},
  {vsn, "1.0.0"},
  {registered, [ppool]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {sup_of_pool_sup, []}},
  {env, []},
  {modules, [sup_of_pool_sup, pool_api, pool_server, pool_sup, pool_worker_sup]}
]}.