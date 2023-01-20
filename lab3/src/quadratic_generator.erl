-module(quadratic_generator).
-behaviour(gen_server).
-export([start_link/1, add_point/3]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

-record(state, {generator_type, points_list = [], func_map = maps:new()}).

start_link(GeneratorType) ->
  {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, [GeneratorType], []).

add_point(Pid, X, Y) -> transport_message(Pid, {add_point, X, Y}).

init([GeneratorType]) ->
  io:fwrite("function_generator has started!~n"),
  {ok, #state{generator_type = GeneratorType}}.

handle_cast({add_point, X, Y}, #state{generator_type = quadratic} = State) -> function:handle_point(X, Y, 3, State).

handle_call(_, _, _) -> throw("function generator doesn't support gen_server calls").



transport_message(Pid, Message) -> gen_server:cast(Pid, Message).