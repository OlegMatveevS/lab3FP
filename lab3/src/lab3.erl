-module(lab3).

-export([start/1]).

start(_) ->
  Type_line = io:get_line("Type> "),
  Type_stripped_line = string:strip(string:strip(Type_line, both, 13), both, 10),
  lab3_sup:start_link(0.25, Type_stripped_line),
  loop().

loop() -> loop().
