%%%-------------------------------------------------------------------
%% @doc Main module for Taskly application.
%% @end
%%%-------------------------------------------------------------------

-module(taskly).

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
        TaskIsCompleted = lists:prefix("Check", Task),
        if TaskIsCompleted == true ->
          Tasks;
        true ->
          Completed = "Check " ++ Task,
          UpdatedTasks = lists:delete(Task, Tasks),
          [Completed | UpdatedTasks]
        end;
      {Pid, {list_completed}} ->
        CompletedTasks = lists:filter(fun(Task) -> lists:prefix("Check", Task) end, Tasks),
        Pid ! {completed, CompletedTasks},
        Tasks;
      {Pid, {list_next}} ->
        Pid ! lists:nth(1, Tasks),
        Tasks;
      {Pid, {list_all}} ->
        SortedTasks = lists:sort(Tasks),
        Pid ! {all, SortedTasks},
        SortedTasks;
      {Pid, {save}} ->
        Pid ! {saved, Tasks},
        save_to_file(Tasks),
        Tasks;
      {Pid, {load}} ->
        LoadedTasks = load_from_file(),
        Pid ! {loaded, LoadedTasks},
        LoadedTasks;
      {Pid, _} ->
        Pid ! {error, "Invalid request"},
        Tasks
    end,
  run(NewTasks).

save_to_file(Tasks) ->
  file:write_file("tasks.txt", lists:join("\n", Tasks)).

load_from_file() ->
  case file:open("tasks.txt", [read]) of
    {ok, Device} ->
      Lines = read_all_lines(Device, []),
      file:close(Device),
      Lines;
    {error, enoent} ->
      []
  end.

read_all_lines(Device, Acc) ->
  case file:read_line(Device) of
      {ok, Line} ->
        CleanLine = string:trim(Line),
        read_all_lines(Device, [CleanLine | Acc]);
        eof ->
          lists:reverse(Acc);
      {error, Reason} ->
          {error, Reason}
  end.