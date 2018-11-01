-module(robot_client).

-include("../../cellphone/src/include/global.hrl").
-include("robot.hrl").

-behaviour(gen_server).

-export([
        handle_info/2,
        handle_cast/2,
        handle_call/3,
        terminate/2,
        code_change/3
        ]).

-export([
         start_link/1,
         init/1
        ]).

-export([
         regi/1,
         logi/1,
         chat/1,
         send_proto/1,
         test/0
        ]).

%%% =====
%%% API
%%% =====
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

regi(RoleName) when is_list(RoleName) ->
    Proto = #register_tos{role_name = RoleName, psd = "123"},
    gen_server:cast(?MODULE, Proto).

logi(RoleName) ->
    Proto = #login_tos{role_name  = RoleName, psd = "123"},
    gen_server:cast(?MODULE, Proto).

chat(Content) ->
    Proto = #chat_tos{chat = #s_chat{content = Content}},
    gen_server:cast(?MODULE, Proto).

send_proto(Proto) ->
    gen_server:cast(?MODULE, Proto).

test() ->
    gen_server:cast(?MODULE, test).
%%% =====
%%% callback
%%% =====
init(Num) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{num = Num}, 1000}.

handle_info('EXIT', State) ->
    {stop, normal, State};
handle_info(Req, State) ->
    case catch do_handle_info(Req, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {stop, _, _} = St ->
            St;
        Error ->
            ?PRINT("~p", [Error]),
            {noreply, State}
    end.

handle_cast(Req, State) ->
    ?PRINT("Req ~p", [Req]),
    case catch do_handle_cast(Req, State) of
        {ok, NewState} ->
            {noreply, NewState};
        Error ->
            ?PRINT("~p~n", [Error]),
            {noreply, State}
    end.

handle_call(Req, _From, State) ->
    case catch do_handle_call(Req, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        Error ->
            ?PRINT("~p~n", [Error]),
            {noreply, State}
    end.

do_handle_info(timeout, State) ->
    ?PRINT("timeout"),
    Socket = hand_shake(),
    Header = robot_config:connect_header(),
    gen_tcp:send(Socket, Header),
    case gen_tcp:recv(Socket, 0) of
        {ok, _Ret} ->
            ok = inet:setopts(Socket, [{active, true}]),
            start_ping(),
            {ok, State#state{socket = Socket}};
        Error ->
            ?THROW(Error)
    end;
do_handle_info({tcp, _Socket, ResBin}, State) ->
    handle_toc(State, ResBin);
do_handle_info({tcp_closed, _Socket}, State) ->
    ?PRINT("tcp_closed"),
    {stop, tcp_closed, State};
do_handle_info({tcp_error, _Socket, Reason}, State) ->
    ?PRINT("tcp error ~p", [Reason]),
    {stop, tcp_error, State};
do_handle_info(ping, State) ->
    Tos = #ping_tos{time = lib_tool:now()},
    client_handler:send_tos(State, Tos),
    {ok, State};
do_handle_info(Other, State) ->
    ?PRINT("unknown message ~p~n", [Other]),
    {ok, State}.

do_handle_cast(#login_tos{role_name = RoleName} = Proto, State) ->
    client_handler:send_tos(State, Proto),
    {ok, State#state{name = RoleName}};
do_handle_cast(Proto, State) ->
    ok = client_handler:send_tos(State, Proto),
    {ok, State}.

do_handle_call(_Request, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% =====
%%% internal
%%% =====
handle_toc(State, ResBin) ->
    case client_handler:handle_toc(State, ResBin) of
        {ok, NewState} ->
            {ok, NewState};
        Error ->
            ?PRINT("~p", [Error]),
            {ok, State}
    end.

start_ping() ->
    erlang:send_after(?MIN_SECOND * 1000, self(), ping).

hand_shake() ->
    Args = robot_config:get_connect_args(),
    Port = robot_config:get_port(),
    Host = robot_config:get_host(),
    {ok, Socket} = gen_tcp:connect(Host, Port, Args),
    Socket.
