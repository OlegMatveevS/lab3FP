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
