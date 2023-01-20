-module(lab3).

-export([start/1]).

start(_) ->
  lab3_sup:start_link(0.25),
  loop().

loop() -> loop().
