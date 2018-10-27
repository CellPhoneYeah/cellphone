-module(websocket_handler).

-include("global.hrl").

-export([
         init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3
        ]).

-define(PING_TIMER, ping_timer).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => infinity}}.

websocket_init(State) ->
    io:format("init net gate ~p~n", [State]),
    net_login:set_last_ping_time(lib_tool:now()),
    start_ping_timer(),
    {ok, State}.

websocket_handle({binary, Bin}, State) ->
    case catch net_misc:decode(Bin) of
        {ok, Proto} ->
            case net_misc:msg_handle(Proto) of
                {ok, Reply} ->
                    {reply, {binary, Reply}, State};
                _ ->
                    {ok, State}
            end;
        Error ->
            io:format("decode fail ~p~n", [Error]),
            {ok, State}
    end;
websocket_handle(Data, State) ->
    io:format("Data ~p~n", [Data]),
    {ok, State}.

websocket_info({binary, Proto}, State) ->
    case net_misc:msg_handle(Proto) of
        {ok, Reply} ->
            {reply, {binary, Reply}, State};
        _ ->
            {ok, State}
    end;
websocket_info(?PING_TIMER, State) ->
    Interval = lib_tool:now() - net_misc:get_last_ping_time(),
    MaxInterval = 5 * ?MIN_SECOND,
    if
        Interval > MaxInterval ->
            {stop, State};
        true ->
            {ok, State}
    end;
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

start_ping_timer() ->
    erlang:send_after(?MIN_SECOND * 1000, self(), ?PING_TIMER).
