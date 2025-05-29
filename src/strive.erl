%%%-------------------------------------------------------------------
%% @doc Main module for Strive application.
%% @end
%%%-------------------------------------------------------------------

-module(strive).

-export([start/0, rpc/2, run/1]).

start() ->
  spawn(?MODULE, run, [[]]).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    Response ->
      Response
  end.

run(Tasks) ->
  NewTasks =
    receive
      {Pid, {add, Task}} ->
        Pid ! {added, Task},
        Tasks ++ [Task];
      {Pid, {remove, Task}} ->
        Pid ! {removed, Task},
        lists:delete(Task, Tasks);
      {Pid, {complete, Task}} ->
        Pid ! {completed, Task},
        Completed = "Check " ++ Task,
        UpdatedTasks = lists:delete(Task, Tasks),
        [Completed | UpdatedTasks];
      {Pid, {list_completed}} ->
        CompletedTasks = lists:filter(fun(Task) -> string:prefix(Task, "Check ") end, Tasks),
        Pid ! {completed, CompletedTasks},
        Tasks;
      {Pid, {list_next}} ->
        Pid ! lists:nth(1, Tasks),
        Tasks;
      {Pid, {list_all}} ->
        SortedTasks = lists:sort(Tasks),
        Pid ! {all, SortedTasks},
        SortedTasks;
      {Pid, _} ->
        Pid ! {error, "Invalid request"},
        Tasks
    end,
  run(NewTasks).
