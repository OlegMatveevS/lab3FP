-module(lab3).

-export([start/1]).

start(_) ->
  Type_line = io:get_line("Type> "),
  Type_stripped_line = string:strip(string:strip(Type_line, both, 13), both, 10),
  Delta_line = io:get_line("Delta> "),
  Delta_stripped_line = string:strip(string:strip(Delta_line, both, 13), both, 10),
  {_, [Delta], _} = io_lib:fread("~f", Delta_stripped_line),
  lab3_sup:start_link(Delta, Type_stripped_line),
  loop().

loop() -> loop().
