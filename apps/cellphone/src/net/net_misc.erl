-module(net_misc).

-include("global.hrl").

-export([
         msg_handle/1,
         encode_msg/1,
         decode_msg/1
        ]).

%% 封包
encode_msg(Proto) ->
    Bin1 = phone:encode_msg(Proto),
    Name = lib_tool:get_record_name(Proto),
    NewMsg = #msg{name = erlang:atom_to_binary(Name, utf8)},
    Bin2 = phone:encode_msg(NewMsg),
    Size = size(Bin2),
    Bin = <<Size:8, Bin2/binary, Bin1/binary>>,
    {ok, Bin}.

%% 解包
decode_msg(Bin) ->
    <<N:8, Bin1:N/binary, Bin2/binary>> = Bin,
    #msg{name = NameBin} = phone:decode_msg(Bin1, msg),
    Name = erlang:list_to_atom(NameBin),
    Dec = phone:decode_msg(Bin2, Name),
    {ok, Dec}.

%% 安排进程处理协议信息
%% ok 则网关进程不用回复客户端
%% {ok, Bin} 则网关直接回复客户端消息
msg_handle(Proto) ->
    RecordName = lib_tool:get_record_name(Proto),
    Route = route_helper:route(RecordName),
    RoleId = net_login:get_role_id(), % 如果没有登录的话应该是undefined
    NetPid = self(),
    case Route of
        ?UNDEF ->
            ?LOG_ERROR("undefined handle proto ~p", [Proto]),
            ok;
        ping ->
            net_login:set_last_ping_time(lib_tool:now()),
            ok;
        login ->
            case catch net_login:login(Proto, NetPid) of
                {ok, Toc} ->
                    {ok, Bin} = net_misc:encode_msg(Toc),
                    {ok, Bin};
                Error ->
                    ?LOG_ERROR("~p~n", [Error]),
                    ok
            end;
        {role, Mod} ->
            if
                is_integer(RoleId) ->
                    lib_role:register_name(RoleId) ! {c2s, RoleId, NetPid, Mod, Proto},
                    ok;
                true ->
                    ok
            end
    end.
