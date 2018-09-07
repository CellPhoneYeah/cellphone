-module(cellphone_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).
crypto_test() ->
    ?assertNot(ok == application:start(crypto)).
-endif.
-ifdef(TEST).
cowlib_test() ->
    ?assertNot(ok == application:start(cowlib)).
-endif.
-ifdef(TEST).
ranch_test() ->
    ?assertNot(ok == application:start(ranch)).
-endif.
-ifdef(TEST).
cowboy_test() ->
    ?assertNot(ok == application:start(cowboy)).
-endif.
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(crypto),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),
    Routes = route_helper:get_routes(),
    Dispatch = cowboy_router:compile(Routes),
    TransOpts = [{port, 8080}],
    ProtoOpts = [{env,
                  [
                   {dispatch, Dispatch}
                  ]}],
    cowboy:start_clear(cellphone_listener, TransOpts, ProtoOpts).

stop(_State) ->
    ok.
