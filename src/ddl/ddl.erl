%%%-------------------------------------------------------------------
%% @doc DDL module for database schema operations.
%% @end
%%%-------------------------------------------------------------------

-module(ddl).

-export([create_database/1]).
create_database(FilePath) ->
    case file:open(FilePath, [read]) of
        {ok, Device} ->
          Lines = read_all_lines(Device, []),
          file:close(Device),
          print_lines(Lines),
          {ok, Lines};
        {error, Reason} ->
            {error, Reason}
    end.

read_all_lines(Device, Acc) ->
  case file:read_line(Device) of
      {ok, Line} ->
          % Line includes the newline character, so you might want to strip it
          CleanLine = string:trim(Line),
          case CleanLine of
            "" -> read_all_lines(Device, Acc);
          "//" ++ _ -> read_all_lines(Device, Acc); % Most correct
          "%" ++ _ -> read_all_lines(Device, Acc); % Allowed
          "--" ++ _ -> read_all_lines(Device, Acc); % Allowed
          "#" ++ _ -> read_all_lines(Device, Acc); % Allowed
          _ ->
            read_all_lines(Device, [CleanLine | Acc])
          end;
        eof ->
          lists:reverse(Acc);
      {error, Reason} ->
          {error, Reason}
  end.

print_lines(Lines) ->
  lists:foreach(fun(Line) ->
      io:format("Line: ~p~n", [Line])
  end, Lines).