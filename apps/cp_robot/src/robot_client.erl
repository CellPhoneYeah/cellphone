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
         chat/2,
         send_proto/2,
         start_chat/1
        ]).

-define(CHAT_TIMER, chat_timer).

%%% =====
%%% API
%%% =====
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

chat(Num, Content) ->
    gen_server:cast(register_name(Num), {chat, Content}).

send_proto(Num, Proto) ->
    gen_server:cast(register_name(Num), {send_proto, Proto}).

start_chat(Num) ->
    Rand = rand:uniform(20),
    Content = io_lib:format("random to talk something ~p", [Rand]),
    erlang:send_after(Rand * 1000, register_name(Num), {chat, Content}).

%%% =====
%%% callback
%%% =====
init(Num) ->
    erlang:process_flag(trap_exit, true),
    RoleName = register_name(Num),
    register(RoleName, self()),
    {ok, #state{num = Num, name = atom_to_list(RoleName), psd = "123"}, 1000}.

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

do_handle_info(timeout, #state{name = RoleName} = State) ->
    Socket = hand_shake(),
    Header = robot_config:connect_header(),
    gen_tcp:send(Socket, Header),
    case gen_tcp:recv(Socket, 0) of
        {ok, _Ret} ->
            ok = inet:setopts(Socket, [{active, true}]),
            Tos = #register_tos{role_name = RoleName, psd = "123"},
            NewState = State#state{socket = Socket},
            client_handler:send_tos(NewState, Tos), % 尝试注册
            start_ping(),
            {ok, NewState};
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
    Tos = #ping_tos{},
    client_handler:send_tos(State, Tos),
    {ok, State};
do_handle_info({chat, Content}, #state{id = RoleId, name = RoleName} = State) ->
    Rand = rand:uniform(20),
    NextContent = io_lib:format("random to talk something ~p", [Rand]),
    erlang:send_after(Rand * 1000, self(), {chat, NextContent}),
    case RoleId of
        undefined ->
            ?PRINT("not login");
        _ ->
            Proto = #chat_tos{chat = #s_chat{role_id = RoleId, role_name = RoleName, content = Content}},
            client_handler:send_tos(State, Proto)
    end,
    {ok, State};

do_handle_info(Other, State) ->
    ?PRINT("unknown message ~p~n", [Other]),
    {ok, State}.

do_handle_cast(login_role, #state{name = RoleName, psd = Psd} = State) ->
    Proto = #login_tos{role_name = RoleName, psd = Psd},
    client_handler:send_tos(State, Proto),
    {ok, State};
do_handle_cast(register_role, #state{name = RoleName, psd = Psd} = State) ->
    Proto = #register_tos{role_name = RoleName, psd = Psd},
    client_handler:send_tos(State, Proto),
    {ok, State};
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

register_name(Num) ->
    list_to_atom("robot_" ++ integer_to_list(Num)).
