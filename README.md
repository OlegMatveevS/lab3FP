# lab3FP
Министерство науки и высшего образования Российской Федерации федеральное государственное автономное образовательное учреждение высшего образования

«Национальный исследовательский университет ИТМО»

---
__ФПИиКТ, Системное и Прикладное Программное Обеспечение__

__Лабораторная работа №3__

по Функциональному программированию

Выполнил: Матвеев О.И.

Группа: P34112

Преподаватель: Пенской Александр Владимирович

###### Санкт-Петербург
###### 2022 г.
---

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, замыканиями, командной строкой.
В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "вычислительная математика" посвящённую апроксимации (в разные годы это лабораторная работа 3 или 4)

---

+ __Запуск всех модулей из супервизора__
``` erlang
-module(lab3_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Delta) ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, [Delta]),
  unlink(Pid).

init([Delta]) ->
  InputReader = #{id => input_reader,
    start => {input_reader, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [input_reader]},
  InputReaderWorker = #{id => input_reader_worker,
    start => {input_reader, input_type_worker, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [input_reader]},
  FunctionGenWorkerFirst = #{id => linear_generator,
    start => {linear_generator, start_link, [linear]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [linear_generator]},
  FunctionGenWorkerSecond = #{id => quadratic_generator,
    start => {quadratic_generator, start_link, [quadratic]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [quadratic_generator]},
  PointsGenWorker = #{id => points_generator,
    start => {points_generator, start_link, [Delta]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [points_generator]},
  MathLoggerWorker = #{id => math_logger,
    start => {math_logger, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [math_logger]},
  {ok, {#{strategy => one_for_all,
    intensity => 5,
    period => 30},
    [InputReader, FunctionGenWorkerFirst, FunctionGenWorkerSecond, PointsGenWorker, MathLoggerWorker, InputReaderWorker]}
  }.
  ```
  
  + __Чтение ввода данных__
  ``` erlang
-module(input_reader).

-behaviour(gen_server).

-export([start_link/0, new_point/4]).
-export([init/1, handle_call/3, handle_cast/2, input_type_worker/0]).


-define(SERVER, ?MODULE).

start_link() ->
  {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_point(Pid, X, Y, Line) -> transport_message(Pid, {X, Y, Line}).

init(_) ->
  io:fwrite("input_reader has started!~n"),
  {ok, []}.

handle_call(_, _, _) -> throw("input_reader doesn't support call~n").

handle_cast({X, Y, Line}, []) ->
  case (Line) of
    "linear" -> linear_generator:add_point(linear_generator, X, Y);
    "quadratic" -> quadratic_generator:add_point(quadratic_generator, X, Y);
    "all" -> linear_generator:add_point(linear_generator, X, Y), quadratic_generator:add_point(quadratic_generator, X, Y)
end,
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

input_type_worker() ->
  Line = io:get_line("Type> "),
  StrippedLine = string:strip(string:strip(Line, both, 13), both, 10),
  input_reader_worker(StrippedLine).

input_reader_worker(Type) ->
  case get_x_y() of
    eof -> {ok, self()};
    {X, Y} ->
      new_point(input_reader, X, Y, Type),
      input_reader_worker(Type)
  end.
  ```
  
  
  + __Функции__
  ```erlang
  start_link(GeneratorType) ->
  {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, [GeneratorType], []).

add_point(Pid, X, Y) -> transport_message(Pid, {add_point, X, Y}).

init([GeneratorType]) ->
  io:fwrite("function_generator has started!~n"),
  {ok, #state{generator_type = GeneratorType}}.

handle_cast({add_point, X, Y}, #state{generator_type = linear} = State) -> handle_point(X, Y, 2, State);

handle_cast({add_point, X, Y}, #state{generator_type = quadratic} = State) -> handle_point(X, Y, 3, State).

handle_call(_, _, _) -> throw("function generator doesn't support gen_server calls").

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
        func_map = generate_function(Type, PointList1, FuncMap)}
    };
    _ -> {
      noreply,
      #state{
        generator_type = Type,
        points_list = PointList1,
        func_map = FuncMap}
    }
  end.

generate_function(linear, PointsList, FuncMap) ->
  io:format("Points list: ~p~n", [PointsList]),
  {X1, Y1} = lists:nth(1, PointsList),
  {X2, Y2} = lists:nth(2, PointsList),
  A1 = (Y2 - Y1) / (X2 - X1),
  A0 = Y1 - A1 * X1,
  Func = fun(X) -> A0 + A1 * X end,
  points_generator:send_new_function(points_generator, X1, X2, Func),
  maps:put({X1, X2}, Func, FuncMap);

generate_function(quadratic, PointsList, FuncMap) ->
  {X1, Y1} = lists:nth(1, PointsList),
  {X2, Y2} = lists:nth(2, PointsList),
  {X3, Y3} = lists:nth(3, PointsList),
  A2 = ((Y3 - Y1) / ((X3 - X1) * (X3 - X2))) - ((Y2 - Y1) / ((X2 - X1) * (X3 - X2))),
  A1 = ((Y2 - Y1) / (X2 - X1)) - (A2 * (X2 + X1)),
  A0 = Y1 - A1 * X1 - A2 * X1 * X1,
  Func = fun(X) -> A0 + A1 * X + A2 * X * X end,
  points_generator:send_new_function(points_generator, X1, X3, Func),
  maps:put({X1, X3}, Func, FuncMap).

transport_message(Pid, Message) -> gen_server:cast(Pid, Message).
  ```
  
   + __Добавление точек__
  ```erlang
  start_link(Delta) ->
  {ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, [Delta], []).

send_new_function(Pid, X1, X2, Func) -> transport_message(Pid, {{X1, X2}, Func}).

init([Delta]) ->
  io:fwrite("points_generator has started!~n"),
  {ok, #state{delta = Delta}}.

handle_call({{X1, X2}, Func}, _, #state{delta = Delta}) ->
  List = lists:seq(1, abs(round((X2 - X1) / Delta))),
  {_, InterpolatedPoints} = lists:foldl(
    fun(_, {Acc, PointsList}) ->
      X = Acc,
      Y = Func(Acc),
      math_logger:log_point(math_logger, X, Y),
      {Acc + Delta, PointsList ++ [{X, Y}]}
    end,
    {X1, []},
    List
  ),
  {reply, InterpolatedPoints, #state{delta = Delta}}.

handle_cast(_, _) -> throw("points_generator doesn't support gen_server casts").

transport_message(Pid, Message) -> gen_server:call(Pid, Message).
  ```
  
   + __Вывод данных__
  ```erlang
  start_link() ->
  {ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log_point(Pid, X, Y) -> transport_message(Pid, {X, Y}).

init(_) ->
  io:fwrite("math_logger has started!~n"),
  {ok, []}.

handle_cast({X, Y}, _) ->
  io:format("~f;~f~n", [X, Y]),
  {noreply, []}.

handle_call(_, _, _) -> throw("logger doesn't support gen_server casts").

transport_message(Pid, Message) -> gen_server:cast(Pid, Message).
  ```
  
  + __Результат работы__
  ```erlang
  numbers>1.1 1.1
numbers>2.2 2.2
Points list: [{1.1,1.1},{2.2,2.2}]
1.100000;1.100000
1.350000;1.350000
1.600000;1.600000
1.850000;1.850000
numbers>3.3 3.4
Points list: [{2.2,2.2},{3.3,3.4}]
2.200000;2.200000
2.450000;2.472727
2.700000;2.745455
2.950000;3.018182
numbers>

  ```
  
