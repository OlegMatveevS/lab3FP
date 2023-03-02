-module(points_generator).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {delta}).

start_link(Delta) ->
  {ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, [Delta], []).

init([Delta]) ->
  io:fwrite("points_generator has started!~n"),
  {ok, #state{delta = Delta}}.

handle_call({X1, X2, Func}, _, #state{delta = Delta}) ->
  List = lists:seq(1, abs(round((X2 - X1) / Delta))),
  {_, InterpolatedPoints} = lists:foldl(
    fun(_, {Acc, PointsList}) ->
      X = Acc,
      Y = Func(Acc),
      gen_server:cast(math_logger, {X, Y}),
      {Acc + Delta, PointsList ++ [{X, Y}]}
    end,
    {X1, []},
    List
  ),
  {reply, InterpolatedPoints, #state{delta = Delta}}.

handle_cast(_, _) -> throw("points_generator doesn't support gen_server casts").




