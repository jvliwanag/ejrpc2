-module(ejrpc2).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("include/ejrpc2.hrl").
-include("include/ejrpc2_err.hrl").

-export([decode_request/1, encode_response/1, handle_req/2]).

-spec decode_request(binary()) -> rpc_req() | {error, rpc_parse_err()}.
decode_request(Bin) ->
	try mochijson2:decode(Bin) of
		Req ->
			parse_decoded(Req)
	catch _:_ ->
			{error, null, parse_error}
	end.


-spec encode_response(rpc_resp()) -> {ok, iolist()}.
encode_response({ok, Id, Result}) ->
	{ok, mochijson2:encode({struct, [
		{jsonrpc, <<"2.0">>},
		{result, Result},
		{id, Id}]})};
encode_response({error, Id, Code, Message}) ->
	{ok, mochijson2:encode({struct, [
		{jsonrpc, <<"2.0">>},
		{error, {struct, [
			{code, Code},
			{message, Message}]}},
		{id, Id}]})};
encode_response({error, Id, Code, Message, Data}) ->
	{ok, mochijson2:encode({struct, [
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

-spec handle_req(Mod::atom(), binary()) -> {ok, iolist()} | ok.
handle_req(Mod, Bin) ->
	%% Load first so that atoms are recognizable
	Exports = Mod:module_info(exports),

	OnApplySuccess = fun(Method, Args, Id, F) ->
		case lists:member({Method, length(Args)}, Exports) of
			true ->
				Res = erlang:apply(Mod, Method, Args),
				F(Res);
			false ->
				encode_response({error, Id, method_not_found})
		end
	end,

	case decode_request(Bin) of
		{rpc, Id, Method, Args} ->
			OnApplySuccess(Method, Args, Id,
				fun(Res) -> encode_response({ok, Id, Res}) end);
		{notif, Method, Args} ->
			OnApplySuccess(Method, Args, null,
				fun(_) -> ok end);
		{error, _, _} = Err ->
			encode_response(Err)
	end.

%% Internal

parse_decoded({struct, Props}) ->
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
		{{error, method_not_found}, {ok, _}} ->
			{error, EfId, method_not_found};
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
			try
				{ok, binary_to_existing_atom(MethodBin, utf8)}
			catch
				_:_ -> {error, method_not_found}
			end;
		_ ->
			{error, invalid_method}
	end.

get_params(Props) ->
	case proplists:get_value(<<"params">>, Props, []) of
		{struct, P} -> {ok, {params, P}};
		L when is_list(L) -> {ok, L};
		_ -> {error, invalid_params}
	end.

-ifdef(TEST).

-define(NOTIF(M, P), <<"{\"jsonrpc\":\"2.0\",\"method\":\"", M, "\",\"params\":", P, "}">>).
-define(REQ(M, P, I), <<"{\"jsonrpc\":\"2.0\",\"method\":\"", M, "\",\"params\":", P, ",\"id\":", I, "}">>).

%% Request Parsing

positional_test_() ->
	[?_assertEqual(
			{rpc, 1, subtract, [42, 23]},
			decode_request(?REQ("subtract", "[42,23]", "1"))),
		?_assertEqual(
			{rpc, 2, subtract, [23, 42]},
			decode_request(?REQ("subtract", "[23,42]", "2")))].

named_parameters_test() ->
	?assertEqual(
		{rpc, 3, subtract, {params, [{<<"subtrahend">>, 23}, {<<"minuend">>, 42}]}},
		decode_request(?REQ("subtract", "{\"subtrahend\":23,\"minuend\":42}", "3"))).

notification_test() ->
	?assertEqual({notif, subtract, [23, 42]},
		decode_request(?NOTIF("subtract", "[23,42]"))).

rpc_with_null_id_test() ->
	?assertEqual(
		{rpc, null, subtract, [42, 23]},
		decode_request(?REQ("subtract", "[42,23]", "null"))).

no_parameters_test() ->
	?assertEqual(
		{notif, greet, []},
		decode_request(<<"{\"jsonrpc\":\"2.0\",\"method\":\"greet\"}">>)).

invalid_json_test() ->
	?assertEqual({error, null, parse_error},
		decode_request(<<"{invalid">>)).

invalid_method_test() ->
	?assertEqual({error, null, invalid_request},
		decode_request(<<"{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\":[42,23]}">>)).

unknown_method_atom_test() ->
	Method = list_to_binary(erlang:ref_to_list(make_ref())),
	?assertEqual({error, null, method_not_found},
		decode_request(<<"{\"jsonrpc\":\"2.0\",\"method\":\"", Method/bytes,"\",\"params\":[42,23]}">>)).

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
			{rpc, 1, sum, [1,2,4]},
			{notif, notify_hello, [7]},
			{rpc, 2, subtract, [42,23]},
			{error, null, invalid_request},
			{rpc, 5, 'foo.get', {params, [{<<"name">>, <<"myself">>}]}},
			{rpc, <<"9">>, get_data, []}
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
	{struct, Props} = mochijson2:decode(IOList),
	?assertEqual(<<"2.0">>, proplists:get_value(<<"jsonrpc">>, Props)),
	?assertEqual(<<"theresult">>, proplists:get_value(<<"result">>, Props)),
	?assertEqual(1, proplists:get_value(<<"id">>, Props)),
	?assertEqual(3, length(Props)).

encode_error_test() ->
	{ok, IOList} = encode_response({error, 1, 234, <<"errmsg">>, <<"errdata">>}),
	{struct, Props} = mochijson2:decode(IOList),
	?assertEqual(<<"2.0">>, proplists:get_value(<<"jsonrpc">>, Props)),
	?assertEqual(1, proplists:get_value(<<"id">>, Props)),
	?assertEqual(3, length(Props)),

	{struct, EProps} = proplists:get_value(<<"error">>, Props),
	?assertEqual(234, proplists:get_value(<<"code">>, EProps)),
	?assertEqual(<<"errmsg">>, proplists:get_value(<<"message">>, EProps)),
	?assertEqual(<<"errdata">>, proplists:get_value(<<"data">>, EProps)),
	?assertEqual(3, length(EProps)).

encode_error_nodata_test() ->
	{ok, IOList} = encode_response({error, 1, 234, <<"errmsg">>}),
	{struct, Props} = mochijson2:decode(IOList),
	{struct, EProps} = proplists:get_value(<<"error">>, Props),
	?assertEqual(undefined, proplists:get_value(<<"data">>, Props)),
	?assertEqual(2, length(EProps)).

test_known_err(Name, Code, Msg) ->
	fun() ->
		{ok, IOList} = encode_response({error, null, Name}),
		{struct, Props} = mochijson2:decode(IOList),
		{struct, EProps} = proplists:get_value(<<"error">>, Props),

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
		encode_response({ok, 1, 3}),
		handle_req(testmod, ?REQ("subtract", "[5,2]", "1"))).

handle_notif_test() ->
	?assertEqual(
		ok,
		handle_req(testmod, ?NOTIF("greet_me", "[\"helloworld\"]"))),
	Rcvd = receive A={greet, _} -> A after 0 -> none end,
	?assertEqual({greet, <<"helloworld">>}, Rcvd).

handle_decode_err_test() ->
	?assertEqual(
		encode_response({error, null, parse_error}),
		handle_req(testmod, <<"wrong">>)).

handle_undefined_method_test() ->
	non_existing_method, % Make sure atom is loaded
	?assertEqual(
		encode_response({error, 1, method_not_found}),
		handle_req(testmod, ?REQ("non_existing_method", "[]", "1"))).

-endif.