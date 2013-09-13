# ejrpc2

Erlang JSON-RPC 2 Utility


## Example

Given a module testmod:

```erlang
-module(testmod).
-export([subtract/2]).

subtract(X, Y) ->
  X - Y.
```

On the erlang shell:
```erlang
1> Str = "{\"jsonrpc\":\"2.0\", \"id\":1, \"method\":\"subtract\", \"params\":[250,3]}",
2> {ok, R, _} = ejrpc2:handle_req(testmod, Str).
3> io:format("Output: ~s~n", [R]).
Output: {"jsonrpc":"2.0","result":247,"id":1}
ok
```

## Usage of `ejrpc2:handle_req(Mod, Str, Opts)`.

* Mod
  - can either be a module name (atom), a module name and options {module(), mod_options()}, or a list of these two sorted according to priority.
  - module options:
    + `{prefix, Prefix}` - effective method names will be prefixed by `Prefix` followed by a '.'
* Str is the name of the method
* Opts is a list of one or more of the following
  * `{preargs, [any()]}` - pre arguments
  * `{arbitrary_json_h, fun/1}` - callback for valid json, but invalid json-rpc2
  * `{default_eterm, any()}` - sets the default Erlang term to return for the handler 

## JSON-RPC2 Callbacks should return either:
* `{ok, json()}`
* `{ok, json(), any()}`
* `{error, ErrCode, ErrMsg}`
* `{error, ErrCode, ErrMsg, any()}`
* `json()` - equivalent to `{ok, json()}`


### Notes
* Internal JSON encoding/decoding is done This uses mochijson2 from [Mochiweb](https://github.com/mochi/mochiweb).
* Mochiweb is distributed under the MIT License.
