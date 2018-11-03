-module(websocket_handler).

-include("global.hrl").

-export([
         init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3
        ]).

-define(PING_CHECKER, ping_checker).

%%% =====
%%% call back
%%% =====
init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => infinity}}.

%% 收到客户端的连接请求
websocket_init(State) ->
    net_login:set_last_ping_time(lib_tool:now()),
    start_ping_checker(),
    {ok, State}.

%% 处理客户端的协议数据包
websocket_handle({binary, Bin}, State) ->
    case catch net_misc:decode_msg(Bin) of
        {ok, Proto} ->
            case catch net_misc:msg_handle(Proto) of
                {ok, RetBin} ->
                    {reply, {binary, RetBin}, State};
                ok ->
                    {ok, State}
            end;
        Error ->
            ?LOG_ERROR("~p~n", [Error]),
            {ok, State}
    end;
websocket_handle(Data, State) ->
    ?LOG_ERROR("unexpected data ~p~n", [Data]),
    {ok, State}.

%% 服务端内部停止网关
websocket_info(stop, State) ->
    ?LOG_INFO("stop"),
    {stop, State};
%% 服务端内部的请求
websocket_info({binary, Proto}, State) ->
    case net_misc:msg_handle(Proto) of
        {ok, Bin} ->
            {reply, {binary, Bin}, State};
        ok ->
            {ok, State}
    end;
%% 检查客户端是否已经断了
websocket_info(?PING_CHECKER, State) ->
    Interval = lib_tool:now() - net_login:get_last_ping_time(),
    MaxInterval = 5 * ?MIN_SECOND,
    if
        Interval > MaxInterval ->
            {stop, State};
        true ->
            {ok, State}
    end;
%% 服务端内部发给客户端的消息
websocket_info({toc, Bin}, State) ->
    {reply, {binary, Bin}, State};
%% 内部崩溃
websocket_info({'EXIT', _, _}, State) ->
    ?LOG_INFO("EXIT"),
    {stop, State};
websocket_info(Info, State) ->
    ?LOG_INFO("unknow Info ~p~n", [Info]),
    {ok, State}.

%% 网关停止，也必须保证在线信息的同步
terminate(_, _, _) ->
    RoleId = net_login:get_role_id(),
    case RoleId of
        ?UNDEF ->
            ok;
        _ ->
            net_server:del_role_netpid(RoleId),
            role_server:del_online_role(RoleId)
    end.

%% 定时检测客户端是否有活动
start_ping_checker() ->
    erlang:send_after(5 * ?MIN_SECOND * 1000, self(), ?PING_CHECKER).
