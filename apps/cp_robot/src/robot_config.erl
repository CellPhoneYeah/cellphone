-module(robot_config).

-include("robot.hrl").

-export([
        get_host/0,
        get_port/0,
        get_connect_args/0,
        host_port/0,
        connect_header/0,
        info/0
        ]).

info() ->
    ?PRINT("host ~p", [get_host()]),
    ?PRINT("port ~p", [get_port()]),
    ?PRINT("header ~p", [binary_to_list(connect_header())]).

get_host() ->
    "127.0.0.1".

get_port() ->
    8089.

get_connect_args() ->
    [binary, {active, false}, {packet, 0}, {reuseaddr, true}].

host_port() ->
    list_to_binary(get_host() ++ ":" ++ integer_to_list(get_port())).

connect_header() ->
    <<"GET /websocket HTTP/1.1\r\nHost: ", (host_port())/binary, "\r\nConnection: Upgrade\r\nPragma: no-cache\r\nCache-Control: no-cache\r\nUser-Agent: Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.100 Safari/537.36\r\nUpgrade: websocket\r\nOrigin: http://coolaf.com\r\nSec-WebSocket-Version: 13\r\nAccept-Encoding: gzip, deflate\r\nAccept-Language: zh-CN,zh;q=0.9,fr;q=0.8,en;q=0.7\r\nCookie: io=FgeRjYd7THEQu9rPAAAB\r\nSec-WebSocket-Key: dzWAJu+FPAaeGLSIUw6bfw==\r\nSec-WebSocket-Extensions: permessage-deflate; client_max_window_bits\r\n\r\n">>.
