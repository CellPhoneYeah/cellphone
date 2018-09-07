-module(cellphone).

-export([
         start/0
        ]).

start() ->
    try_start(cellphone).

try_start(App) ->
    case application:start(App) of
        ok ->
            ok;
        {not_started, App1} ->
            start_app(App1),
            try_start(App);
        Error ->
            io:format("~p~n", [Error])
    end.

start_app(App) ->
    application:start(App).
