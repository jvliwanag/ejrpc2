-type rpc_id() :: null | binary() | integer().
-type rpc_args() :: [any()] | {params, [{binary(), any()}]}.
-type rpc_method() :: atom().
-type rpc_req_rpc() :: {rpc, rpc_id(), rpc_method(), rpc_args()}.
-type rpc_req_notif() :: {notif, rpc_method(), rpc_args()}.
-type rpc_req() :: rpc_req_rpc() | rpc_req_notif().

-type rpc_parse_err() :: parse_error | invalid_request.

-type rpc_result() :: any(). %% json_term().
-type rpc_errcode() :: integer().
-type rpc_errdata() :: any(). %% json_term().
-type rpc_errmsg() :: binary().
-type rpc_resp() :: {ok, rpc_id(), rpc_result()} |
	{error, rpc_id(), rpc_errcode(), rpc_errmsg(), rpc_errdata()} |
	{error, rpc_id(), rpc_errcode(), rpc_errmsg()}.

-type rpc_req_opts() :: [rpc_req_opts()].
-type rpc_req_opt() :: {preargs, [any()]}.