-module(lib_config).

-export([
            listen_port/0
        ]).

listen_port() ->
    Port = application:get_env(cellphone, websocket_port, 8089),
    Port.
