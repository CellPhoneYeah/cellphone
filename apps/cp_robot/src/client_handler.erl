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
            stop;
        {next, Dec, Rest} ->
            NewState =
            if
                Dec == ?UNDEF ->
                    State;
                true ->
                    ?PRINT("Toc ~p", [Dec]),
                    {ok, State1} = do_handle_toc(State, Dec),
                    State1
            end,
            handle_toc(NewState#state{buffer = Rest}, <<>>);
        {more, Rest} ->
            {ok, State#state{buffer = Rest}}
    end.

do_handle_toc(State, #pong_toc{}) ->
    {ok, State};
do_handle_toc(State, #login_toc{code = Code, role = SRole}) ->
    NewState = case Code of
                   ?E_OK ->
                       #s_role{
                          role_id = RoleId,
                          role_name = Name
                         } = SRole,
                       State#state{name = Name, id = RoleId};
                   ?E_LOGIN_ROLE_NAME_NOT_REGISTERED -> % 未注册
                       State;
                   ?E_LOGIN_ROLE_NOT_FOUND ->
                       State;
                   ?E_LOGIN_PASSWORD_NOT_MATCH ->
                       State
               end,
    {ok, NewState};
do_handle_toc(State, #register_toc{code = Code}) ->
    NewState = case Code of
                   ?E_OK ->
                       State;
                   ?E_REGISTER_NAME_HAS_BEEN_USED ->
                       ?PRINT("name has been used~n"),
                       State;
                   _ ->
                       State
               end,
    {ok, NewState};
do_handle_toc(State, #s_chat{role_id = SenderId, role_name = SenderName, content = Content}) ->
    ?PRINT("~p ~p:~p", [SenderId, SenderName, Content]),
    {ok, State};
do_handle_toc(State, _Toc) ->
    {ok, State}.

send_tos(#state{socket = Socket}, Tos) ->
    {ok, Bin} = net_misc:encode_msg(Tos),
    Frame = cow_ws:masked_frame({binary, Bin}, undefined),
    gen_tcp:send(Socket, Frame).
