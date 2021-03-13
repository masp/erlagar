%%%-------------------------------------------------------------------
%% @doc erlagar public API
%% @end
%%%-------------------------------------------------------------------

-module(erlagar_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, erlagar, "index.html"}},
            {"/websocket", erlagar_ws, []},
            {"/wasm_exec.js", cowboy_static, {priv_file, erlagar, "wasm_exec.js"}},
            {"/wasm/game.wasm", cowboy_static,
                {priv_file, erlagar, "wasm/game.wasm", [
                    {mimetypes, {<<"application">>, <<"wasm">>, []}}
                ]}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    erlagar_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% internal functions
