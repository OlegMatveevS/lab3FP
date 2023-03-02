-module(linear_generator).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

-record(state, { points_list = [], func_map = maps:new()}).

start_link() ->
  {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_) ->
  io:fwrite("linear has started!~n"),
  {ok, #state{}}.

handle_cast({X, Y}, #state{} = State) -> handle_point(X, Y, 2, State).

handle_call(_, _, _) -> throw("function generator doesn't support gen_server calls").

handle_point(X, Y, MaxPoints, #state{
  points_list = PointList,
  func_map = FuncMap}) ->
  PointList1 = lists:append(PointList, [{X, Y}]),
  case length(PointList1) of
    MaxPoints -> {
      noreply,
      #state{
        points_list = [lists:nth(MaxPoints, PointList1)],
        func_map = generate_function(PointList1, FuncMap)}
    };
    _ -> {
      noreply,
      #state{
        points_list = PointList1,
        func_map = FuncMap}
    }
  end.

generate_function(PointsList, FuncMap) ->
  {X1, Y1} = lists:nth(1, PointsList),
  {X2, Y2} = lists:nth(2, PointsList),
  A1 = (Y2 - Y1) / (X2 - X1),
  A0 = Y1 - A1 * X1,
  Func = fun(X) -> A0 + A1 * X end,
  gen_server:call(points_generator, {X1, X2, Func}),
  maps:put({X1, X2}, Func, FuncMap).


