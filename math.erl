-module(math).
-compile(export_all).

inc(I) ->
    I + 1.

append(I, L) ->
    lists:append(I, L).

append(I) ->
    lists:append(I, [1, 2, 3]).

