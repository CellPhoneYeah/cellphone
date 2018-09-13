-module(websocket_handler).

-export([
         init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3
        ]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    io:format("init ~p~n", [State]),
    {ok, State}.

websocket_handle({binary, Bin}, State) ->
    io:foramt("Bin ~p~n", [Bin]),
    {ok, State};
websocket_handle(Data, State) ->
    io:format("Data ~p~n", [Data]),
    {ok, State}.

websocket_info({toc, Msg}, State) ->
    io:format("Msg ~p~n", [Msg]),
    {ok, State};
websocket_info(stop, State) ->
    {stop, State};
websocket_info({'EXIT', _, _}, State) ->
    {stop, State};
websocket_info(Info, State) ->
    io:format("unknow Info ~p~n", [Info]),
    {ok, State}.

terminate(_, _, _) ->
    io:format("offline ~n"),
    role_server:off_line(self()),
    ok.
