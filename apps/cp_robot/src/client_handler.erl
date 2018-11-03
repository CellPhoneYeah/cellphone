-module(client_handler).

-include("../../cellphone/src/include/global.hrl").
-include("robot.hrl").

-export([
         handle_toc/2,
         send_tos/2
        ]).
handle_toc(State, Bin) ->
    case robot_packet:decode(State, Bin) of
        stop ->
            ?PRINT("stop"),
            stop;
        {next, Dec, Rest} ->
            NewState =
            if
                Dec == ?UNDEF ->
                    State;
                true ->
                    {ok, State1} = do_handle_toc(State, Dec),
                    State1
            end,
            handle_toc(NewState#state{buffer = Rest}, <<>>);
        {more, Rest} ->
            {ok, State#state{buffer = Rest}}
    end.

do_handle_toc(State, #pong_toc{}) ->
    {ok, State};
do_handle_toc(#state{name = RoleName, psd = Psd} = State, #register_toc{code = Code, role = SRole}) ->
    NewState = case Code of
                   0 ->
                       State#state{id = SRole#s_role.role_id};
                   _ ->
                       State
               end,
    Tos = #login_tos{role_name = RoleName, psd = Psd},
    client_handler:send_tos(NewState, Tos),
    {ok, NewState};
do_handle_toc(State, #login_toc{code = Code, role = SRole}) ->
    NewState = case Code of
                   ?E_OK ->
                       #s_role{
                          role_id = RoleId,
                          role_name = Name
                         } = SRole,
                       #state{num = Num} = State,
                       robot_client:start_chat(Num),
                       State#state{name = Name, id = RoleId};
                   ?E_LOGIN_ROLE_NAME_NOT_REGISTERED -> % 未注册
                       State;
                   ?E_LOGIN_ROLE_NOT_FOUND ->
                       State;
                   ?E_LOGIN_PASSWORD_NOT_MATCH ->
                       State;
                   Error ->
                       ?PRINT("~p", [Error]),
                       State
               end,
    {ok, NewState};
do_handle_toc(State, #s_chat{role_id = _SenderId, role_name = _SenderName, content = _Content}) ->
    if
        _SenderId == State#state.id ->
            %io:format("~p ~p:~p~n", [SenderId, SenderName, Content]);
            ok;
        true ->
            ok
    end,
    {ok, State};
do_handle_toc(State, _Toc) ->
    {ok, State}.

send_tos(#state{socket = Socket}, Tos) ->
    {ok, Bin} = net_misc:encode_msg(Tos),
    Frame = cow_ws:masked_frame({binary, Bin}, undefined),
    gen_tcp:send(Socket, Frame).
