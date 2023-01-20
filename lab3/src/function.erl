-module(function).

%% API
-export([generate_function/3, handle_point/4]).

-record(state, {generator_type, points_list, func_map}).

handle_point(X, Y, MaxPoints, #state{
  generator_type = Type,
  points_list = PointList,
  func_map = FuncMap}) ->
  PointList1 = lists:append(PointList, [{X, Y}]),
  case length(PointList1) of
    MaxPoints -> {
      noreply,
      #state{
        generator_type = Type,
        points_list = [lists:nth(MaxPoints, PointList1)],
        func_map = function:generate_function(Type, PointList1, FuncMap)}
    };
    _ -> {
      noreply,
      #state{
        generator_type = Type,
        points_list = PointList1,
        func_map = FuncMap}
    }
  end.

generate_function(quadratic, PointsList, FuncMap) ->
  {X1, Y1} = lists:nth(1, PointsList),
  {X2, Y2} = lists:nth(2, PointsList),
  {X3, Y3} = lists:nth(3, PointsList),
  A2 = ((Y3 - Y1) / ((X3 - X1) * (X3 - X2))) - ((Y2 - Y1) / ((X2 - X1) * (X3 - X2))),
  A1 = ((Y2 - Y1) / (X2 - X1)) - (A2 * (X2 + X1)),
  A0 = Y1 - A1 * X1 - A2 * X1 * X1,
  Func = fun(X) -> A0 + A1 * X + A2 * X * X end,
  points_generator:send_new_function(points_generator, X1, X3, Func),
  maps:put({X1, X3}, Func, FuncMap);

generate_function(linear, PointsList, FuncMap) ->
  {X1, Y1} = lists:nth(1, PointsList),
  {X2, Y2} = lists:nth(2, PointsList),
  A1 = (Y2 - Y1) / (X2 - X1),
  A0 = Y1 - A1 * X1,
  Func = fun(X) -> A0 + A1 * X end,
  points_generator:send_new_function(points_generator, X1, X2, Func),
  maps:put({X1, X2}, Func, FuncMap).