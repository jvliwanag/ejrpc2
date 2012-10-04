-module(testmod).

-export([subtract/2, greet_me/1]).

-spec subtract(number(), number()) -> number().
subtract(X, Y) ->
	X - Y.

-spec greet_me(any()) -> ok.
greet_me(Msg) ->
	self() ! {greet, Msg},
	ok.