-module(testmod).

-export([subtract/2, greet_me/1, safe_divide/2, grab/2]).

-spec subtract(number(), number()) -> number().
subtract(X, Y) ->
	X - Y.

-spec greet_me(any()) -> ok.
greet_me(Msg) ->
	self() ! {greet, Msg},
	ok.

-spec safe_divide(integer(), integer()) ->
		{ok, integer()} | {error, integer(), binary()}.
safe_divide(_, 0) ->
	{error, 400, <<"divide by zero">>};
safe_divide(X, Y) ->
	{ok, X div Y}.

-spec grab(integer(), integer()) -> {ok, integer(), integer()} |
		{error, 400, binary(), integer()}.
grab(N, Left) ->
	case Left > N of
		true ->
			{ok, N, Left - N};
		_ ->
			{error, 400, <<"not enough">>, Left}
	end.


