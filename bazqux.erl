-module(bazqux).
-export([inc/1, one/2, fish/1]).

inc(i) ->
    4 + 1.

one(j, k) ->
    X = fun() -> 45 end,
    X() - inc(3).

fish(J) ->
    X = J + 5,
    X + J,
    Y = J = 10,
    Y + X + J.
