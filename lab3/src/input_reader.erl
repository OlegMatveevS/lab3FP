-module(input_reader).

-behaviour(gen_server).

-export([start_link/0, new_point/3, input_reader_worker/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

start_link() ->
  {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_point(Pid, X, Y) -> transport_message(Pid, {X, Y}).

init([]) ->
  io:fwrite("input_reader has started!~n"),
  {ok, []}.

handle_call(_, _, _) -> throw("input_reader doesn't support call~n").

handle_cast({X, Y}, []) ->
  function_generator:add_point(function_generator, X, Y),
  {noreply, []}.

transport_message(Pid, Message) -> gen_server:cast(Pid, Message).

get_x_y() ->
  Line = io:get_line("numbers>"),
  case Line of
    eof -> eof;
    _ ->
      StrippedLine = string:strip(string:strip(Line, both, 13), both, 10),
      try
        {_, [X, Y], _} = io_lib:fread("~f~f", StrippedLine),
        {X, Y}
      catch
        error:{badmatch, _} -> get_x_y()
      end
  end.

input_reader_worker() ->
  case get_x_y() of
    eof -> {ok, self()};
    {X, Y} ->
      new_point(input_reader, X, Y),
      input_reader_worker()
  end.