-module(lab3_sup).

-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(Mode, Delta) ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, [Mode, Delta]),
  unlink(Pid).

init([Mode, Delta]) ->
  InputReader = #{id => input_reader,
    start => {input_reader, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [input_reader]},
  InputReaderWorker = #{id => input_reader_worker,
    start => {input_reader, input_reader_worker, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [input_reader]},
  FunctionGenWorker = #{id => function_generator,
    start => {function_generator, start_link, [Mode]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [function_generator]},
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
    [InputReader, FunctionGenWorker, PointsGenWorker, MathLoggerWorker, InputReaderWorker]}
  }.
