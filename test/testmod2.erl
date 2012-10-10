-module(testmod2).

-export([add/2]).

-spec add(number(), number()) -> number().
add(X, Y) ->
	X + Y.
