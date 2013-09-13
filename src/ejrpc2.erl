-module(ejrpc2).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("include/ejrpc2.hrl").
-include("include/ejrpc2_err.hrl").

-export([decode_request/1, encode_response/1, handle_req/2, handle_req/3]).

-spec decode_request(binary()) -> rpc_req() | {error, rpc_parse_err()}.
decode_request(Bin) ->
	decode_request(Bin, undefined).

-spec encode_response(rpc_resp()) -> {ok, iolist()}.
encode_response({ok, Id, Result}) ->
	{ok, term_to_json({[
		{jsonrpc, <<"2.0">>},
		{result, Result},
		{id, Id}]})};
encode_response({error, Id, Code, Message}) ->
	{ok, term_to_json({[
		{jsonrpc, <<"2.0">>},
		{error, {struct, [
			{code, Code},
			{message, Message}]}},
		{id, Id}]})};
encode_response({error, Id, Code, Message, Data}) ->
	{ok, term_to_json({[
		{jsonrpc, <<"2.0">>},
		{error, {struct, [
			{code, Code},
			{message, Message},
			{data, Data}]}},
		{id, Id}]})};
encode_response({error, Id, parse_error}) ->
	encode_response({error, Id, ?ERR_PARSE_ERROR_CODE, ?ERR_PARSE_ERROR_MSG});
encode_response({error, Id, invalid_request}) ->
	encode_response({error, Id, ?ERR_INVALID_REQUEST_CODE, ?ERR_INVALID_REQUEST_MSG});
encode_response({error, Id, method_not_found}) ->
	encode_response({error, Id, ?ERR_METHOD_NOT_FOUND_CODE, ?ERR_METHOD_NOT_FOUND_MSG});
encode_response({error, Id, invalid_params}) ->
	encode_response({error, Id, ?ERR_INVALID_PARAMS_CODE, ?ERR_INVALID_PARAMS_MSG});
encode_response({error, Id, internal_error}) ->
	encode_response({error, Id, ?ERR_INTERNAL_ERROR_CODE, ?ERR_INTERNAL_ERROR_MSG}).

-spec handle_req(rpc_mod() | [rpc_mod()], binary()) -> {ok, undefined | iolist(), any()}.
handle_req(Mod, Bin) ->
	handle_req(Mod, Bin, []).

-spec handle_req(rpc_mod() | [rpc_mod()], binary(), rpc_req_opts()) ->
	{ok, undefined | iolist(), any()}.
handle_req(Mod, Bin, Opts) ->
	Exports = get_exports(Mod, []),
	ArbHandler = proplists:get_value(arbitrary_json_h, Opts),

	%% TODO better to decode first, then find
	OnApplySuccess = fun(Method, Args, Id, F) ->
		case lists:keyfind({Method, length(Args)}, 3, Exports) of
			{M, Fun, _} ->
				Res = erlang:apply(M, Fun, Args),
				F(Res);
			false ->
				encode_response_no_eterm({error, Id, method_not_found})
		end
	end,

	case decode_request(Bin, ArbHandler) of
		{rpc, Id, Method, Args0} ->
			Args = case proplists:get_value(preargs, Opts, []) of
				[] -> Args0;
				L when is_list(L) -> L ++ Args0
			end,
			OnApplySuccess(Method, Args, Id, fun
				({ok, Res}) ->
					encode_response_no_eterm({ok, Id, Res});
				({ok, Res, ETerm}) ->
					encode_response_with_eterm({ok, Id, Res}, ETerm);
				({error, Code, Msg}) ->
					encode_response_no_eterm({error, Id, Code, Msg});
				({error, Code, Msg, ETerm}) ->
					encode_response_with_eterm({error, Id, Code, Msg}, ETerm);
				(Res) ->
					encode_response_no_eterm({ok, Id, Res})
			end);
		{notif, Method, Args} ->
			OnApplySuccess(Method, Args, null,
				fun(_) -> {ok, undefined, undefined} end);
		{error, _, _} = Err ->
			encode_response_no_eterm(Err);
		{arbitrary, Val} ->
			{ok, undefined, Val}
	end.

%% Internal

decode_request(Bin, ArbHandler) ->
	try json_to_term(Bin) of
		Req ->
			handle_arb_json(ArbHandler, Req,
				parse_decoded(Req))
	catch _:_ ->
			{error, null, parse_error}
	end.

handle_arb_json(ArbHandler, Req, {error, _, _})
		when is_function(ArbHandler, 1) ->
	{arbitrary, ArbHandler(Req)};
handle_arb_json(_, _, Res) ->
	Res.

json_to_term(Bin) ->
	ejrpc2_json:decode(Bin, [{format, eep18}]).

term_to_json(T) ->
	ejrpc2_json:encode(T).

parse_decoded({Props}) ->
	M = get_method(Props),
	P = get_params(Props),
	Id = proplists:get_value(<<"id">>, Props),
	EfId = case Id of undefined -> null; _ -> Id end,

	case {M, P} of
		{{ok, Method}, {ok, Params}} ->
			case Id of
				undefined ->
					{notif, Method, Params};
				_ ->
					{rpc, EfId, Method, Params}
			end;
		_ ->
			{error, EfId, invalid_request}
	end;
parse_decoded([_|_] = Reqs) ->
	{batch, [parse_decoded(X) || X <- Reqs]};
parse_decoded(_) ->
	{error, null, invalid_request}.

get_method(Props) ->
	case proplists:get_value(<<"method">>, Props) of
		MethodBin when is_binary(MethodBin) ->
			{ok, MethodBin};
		_ ->
			{error, invalid_method}
	end.

get_params(Props) ->
	case proplists:get_value(<<"params">>, Props, []) of
		{P} -> {ok, {params, P}};
		L when is_list(L) -> {ok, L};
		_ -> {error, invalid_params}
	end.


get_exports(M, []) when is_atom(M) ->
	get_exports({M, []}, []);
get_exports({M, Opts}, []) when is_atom(M) ->
	Pref = proplists:get_value(prefix, Opts),
	[{M, F, {get_fun_name_f(M, F, Pref), A}} || {F, A} <- M:module_info(exports)];

get_exports([], Acc) ->
	lists:flatten(Acc);
get_exports([H|R], Acc) ->
	get_exports(R, [get_exports(H, [])|Acc]).

get_fun_name_f(_, F, P) when is_binary(P) ->
	Name = erlang:atom_to_binary(F, utf8),
	<<P/binary, ".", Name/binary>>;
get_fun_name_f(_, F, P) when is_list(P) ->
	Name = erlang:atom_to_binary(F, utf8),
	PBin = erlang:list_to_binary(P),
	<<PBin/binary, ".", Name/binary>>;
get_fun_name_f(M, F, true) ->
	Name = erlang:atom_to_binary(F, utf8),
	P = erlang:atom_to_binary(M, utf8),
	<<P/binary, ".", Name/binary>>;
get_fun_name_f(_, F, _) ->
	erlang:atom_to_binary(F, utf8).

encode_response_with_eterm(R, E) ->
	{ok, X} = encode_response(R),
	{ok, X, E}.

encode_response_no_eterm(R) ->
	encode_response_with_eterm(R, undefined).

-ifdef(TEST).

-define(NOTIF(M, P), <<"{\"jsonrpc\":\"2.0\",\"method\":\"", M, "\",\"params\":", P, "}">>).
-define(REQ(M, P, I), <<"{\"jsonrpc\":\"2.0\",\"method\":\"", M, "\",\"params\":", P, ",\"id\":", I, "}">>).

%% Request Parsing

positional_test_() ->
	[?_assertEqual(
			{rpc, 1, <<"subtract">>, [42, 23]},
			decode_request(?REQ("subtract", "[42,23]", "1"))),
		?_assertEqual(
			{rpc, 2, <<"subtract">>, [23, 42]},
			decode_request(?REQ("subtract", "[23,42]", "2")))].

named_parameters_test() ->
	?assertEqual(
		{rpc, 3, <<"subtract">>, {params, [{<<"subtrahend">>, 23}, {<<"minuend">>, 42}]}},
		decode_request(?REQ("subtract", "{\"subtrahend\":23,\"minuend\":42}", "3"))).

notification_test() ->
	?assertEqual({notif, <<"subtract">>, [23, 42]},
		decode_request(?NOTIF("subtract", "[23,42]"))).

rpc_with_null_id_test() ->
	?assertEqual(
		{rpc, null, <<"subtract">>, [42, 23]},
		decode_request(?REQ("subtract", "[42,23]", "null"))).

no_parameters_test() ->
	?assertEqual(
		{notif, <<"greet">>, []},
		decode_request(<<"{\"jsonrpc\":\"2.0\",\"method\":\"greet\"}">>)).

invalid_json_test() ->
	?assertEqual({error, null, parse_error},
		decode_request(<<"{invalid">>)).

invalid_method_test() ->
	?assertEqual({error, null, invalid_request},
		decode_request(<<"{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\":[42,23]}">>)).

invalid_params_test() ->
	?assertEqual({error, 2, invalid_request},
		decode_request(?REQ("subtract", "53", "2"))).

empty_batch_test() ->
	?assertEqual({error, null, invalid_request},
		decode_request(<<"[]">>)).

rpc_call_invalid_batch_test() ->
	?assertEqual({batch, [{error, null, invalid_request},
		{error, null, invalid_request}, {error, null, invalid_request}]},
		decode_request(<<"[1,2,3]">>)).

rpc_call_batch_test() ->
	?assertEqual({batch, [
			{rpc, 1, <<"sum">>, [1,2,4]},
			{notif, <<"notify_hello">>, [7]},
			{rpc, 2, <<"subtract">>, [42,23]},
			{error, null, invalid_request},
			{rpc, 5, <<"foo.get">>, {params, [{<<"name">>, <<"myself">>}]}},
			{rpc, <<"9">>, <<"get_data">>, []}
		]},
		decode_request(iolist_to_binary(["[",
			?REQ("sum", "[1,2,4]", "1"), ",",
			?NOTIF("notify_hello", "[7]"), ",",
			?REQ("subtract", "[42,23]", "2"), ",",
			<<"{\"foo\":\"boo\"}">>, ",",
			?REQ("foo.get", "{\"name\":\"myself\"}", "5"), ",",
			<<"{\"jsonrpc\": \"2.0\",\"method\":\"get_data\",\"id\":\"9\"}">>,
			"]"
		]))).

%% Response Encoding

encode_result_test() ->
	{ok, IOList} = encode_response({ok, 1, <<"theresult">>}),
	{Props} = json_to_term(IOList),
	?assertEqual(<<"2.0">>, proplists:get_value(<<"jsonrpc">>, Props)),
	?assertEqual(<<"theresult">>, proplists:get_value(<<"result">>, Props)),
	?assertEqual(1, proplists:get_value(<<"id">>, Props)),
	?assertEqual(3, length(Props)).

encode_error_test() ->
	{ok, IOList} = encode_response({error, 1, 234, <<"errmsg">>, <<"errdata">>}),
	{Props} = json_to_term(IOList),
	?assertEqual(<<"2.0">>, proplists:get_value(<<"jsonrpc">>, Props)),
	?assertEqual(1, proplists:get_value(<<"id">>, Props)),
	?assertEqual(3, length(Props)),

	{EProps} = proplists:get_value(<<"error">>, Props),
	?assertEqual(234, proplists:get_value(<<"code">>, EProps)),
	?assertEqual(<<"errmsg">>, proplists:get_value(<<"message">>, EProps)),
	?assertEqual(<<"errdata">>, proplists:get_value(<<"data">>, EProps)),
	?assertEqual(3, length(EProps)).

encode_error_nodata_test() ->
	{ok, IOList} = encode_response({error, 1, 234, <<"errmsg">>}),
	{Props} = json_to_term(IOList),
	{EProps} = proplists:get_value(<<"error">>, Props),
	?assertEqual(undefined, proplists:get_value(<<"data">>, Props)),
	?assertEqual(2, length(EProps)).

test_known_err(Name, Code, Msg) ->
	fun() ->
		{ok, IOList} = encode_response({error, null, Name}),
		{Props} = json_to_term(IOList),
		{EProps} = proplists:get_value(<<"error">>, Props),

		?assertEqual(Code, proplists:get_value(<<"code">>, EProps)),
		?assertEqual(Msg, proplists:get_value(<<"message">>, EProps))
	end.

encode_known_err_test_() ->
	[test_known_err(parse_error, ?ERR_PARSE_ERROR_CODE, ?ERR_PARSE_ERROR_MSG),
	test_known_err(invalid_request, ?ERR_INVALID_REQUEST_CODE, ?ERR_INVALID_REQUEST_MSG),
	test_known_err(method_not_found, ?ERR_METHOD_NOT_FOUND_CODE, ?ERR_METHOD_NOT_FOUND_MSG),
	test_known_err(invalid_params, ?ERR_INVALID_PARAMS_CODE, ?ERR_INVALID_PARAMS_MSG),
	test_known_err(internal_error, ?ERR_INTERNAL_ERROR_CODE, ?ERR_INTERNAL_ERROR_MSG)].

%% Mod handling

handle_rpc_test() ->
	?assertEqual(
		encode_response_no_eterm({ok, 1, 3}),
		handle_req(testmod, ?REQ("subtract", "[5,2]", "1"))).


handle_notif_test() ->
	?assertEqual(
		{ok, undefined, undefined},
		handle_req(testmod, ?NOTIF("greet_me", "[\"helloworld\"]"))),
	Rcvd = receive A={greet, _} -> A after 0 -> none end,
	?assertEqual({greet, <<"helloworld">>}, Rcvd).

handle_decode_err_test() ->
	?assertEqual(
		encode_response_no_eterm({error, null, parse_error}),
		handle_req(testmod, <<"wrong">>)).

handle_undefined_method_test() ->
	non_existing_method, % Make sure atom is loaded
	?assertEqual(
		encode_response_no_eterm({error, 1, method_not_found}),
		handle_req(testmod, ?REQ("non_existing_method", "[]", "1"))).

%% Response Variations
handle_ok_resp_test() ->
	?assertEqual(
		encode_response_no_eterm({ok, 1, 3}),
		handle_req(testmod, ?REQ("safe_divide", "[6,2]", "1"))).

handle_error_resp_test() ->
	?assertEqual(
		encode_response_no_eterm({error, 1, 400, <<"divide by zero">>}),
		handle_req(testmod, ?REQ("safe_divide", "[6,0]", "1"))).

%% With ETerm

handle_rpc_with_eterm_test() ->
	?assertEqual(
		encode_response_with_eterm({ok, 1, 2}, 97),
		handle_req(testmod, ?REQ("grab", "[2, 99]", "1"))).

handle_error_with_eterm_test() ->
	?assertEqual(
		encode_response_with_eterm({error, 1, 400, <<"not enough">>}, 99),
		handle_req(testmod, ?REQ("grab", "[100, 99]", "1"))).

%% Multiple Mod test
handle_multimod_test() ->
	?assertEqual(
		encode_response_no_eterm({ok, 1, 7}),
		handle_req([testmod, testmod2], ?REQ("add", "[5,2]", "1"))).

%% Mod Options test
empty_mod_options_test() ->
	?assertEqual(
		encode_response_no_eterm({ok, 1, 3}),
		handle_req({testmod, []}, ?REQ("subtract", "[5,2]", "1"))).

prefix_mod_options_test_() ->
	[{"binary prefix", ?_assertEqual(
		encode_response_no_eterm({ok, 1, 3}),
		handle_req({testmod, [{prefix, <<"mod">>}]}, ?REQ("mod.subtract", "[5,2]", "1")))},
	{"list prefix", ?_assertEqual(
		encode_response_no_eterm({ok, 1, 3}),
		handle_req({testmod, [{prefix, "mod"}]}, ?REQ("mod.subtract", "[5,2]", "1")))},
	{"module prefix", ?_assertEqual(
		encode_response_no_eterm({ok, 1, 3}),
		handle_req({testmod, [prefix]}, ?REQ("testmod.subtract", "[5,2]", "1")))}].


%% handle Options
handle_preargs_test() ->
	?assertEqual(
		encode_response_no_eterm({ok, 1, 3}),
		handle_req(testmod, ?REQ("subtract", "[2]", "1"), [{preargs, [5]}])).

handle_arbitrary_json_test() ->
	?assertEqual({ok, undefined, 99}, handle_req(testmod, <<"{\"num\":5}">>,
		[{arbitrary_json_h, fun(_) -> 99 end}])).

-endif.