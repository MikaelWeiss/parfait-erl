%%%-------------------------------------------------------------------
%% @doc DDL module for database schema operations.
%% I started with a simple parser for a Database Definition Language (DDL) file.
%% I ended up deciding it was more complicated than I needed it to be right now.
%% @end
%%%-------------------------------------------------------------------

-module(ddl).

-export([create_database/1]).

create_database(FilePath) ->
    case file:open(FilePath, [read]) of
        {ok, Device} ->
          Lines = read_all_lines(Device, []),
          file:close(Device),
          parse_lines(Lines),
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

parse_lines(Lines) ->
  lists:foreach(fun(Line) ->
      parse_line(Line, {start})
  end, Lines).

parse_line(Line, State) ->
  Words = string:split(Line, " "),
  parse_words(Words, State).

parse_words(Words, State) ->
  case Words of
    [] ->
      State;
    [First | Rest] ->
      case First of
        "model" ->
          parse_words(Rest, {model});
        _ ->
          parse_words(Rest, State)
      end
  end.