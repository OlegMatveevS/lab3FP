-module(lab3).

-export([start/1]).

start(_) ->
  lab3_sup:start_link(linear, 0.25),
  loop().

loop() -> loop().
