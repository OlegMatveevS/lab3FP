-module(math_logger).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
  {ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
  io:fwrite("math_logger has started!~n"),
  {ok, []}.

handle_cast({X, Y}, _) ->
  io:format("~f;~f~n", [X, Y]),
  {noreply, []}.

handle_call(_, _, _) -> throw("logger doesn't support gen_server casts").

