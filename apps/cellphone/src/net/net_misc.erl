-module(net_misc).

-include("global.hrl").

-export([
         msg_handle/1,
         encode_msg/1,
         decode_msg/1
        ]).

encode_msg(Proto) ->
    Bin1 = phone:encode_msg(Proto),
    Name = lib_tool:get_record_name(Proto),
    NewMsg = #msg{name = erlang:atom_to_binary(Name, utf8)},
    Bin2 = phone:encode_msg(NewMsg),
    Size = size(Bin2),
    Bin = <<Size:8, Bin2/binary, Bin1/binary>>,
    {ok, Bin}.

decode_msg(Bin) ->
    <<N:8, Bin1:N/binary, Bin2/binary>> = Bin,
    #msg{name = NameBin} = phone:decode_msg(Bin1, msg),
    Name = erlang:list_to_atom(NameBin),
    Dec = phone:decode_msg(Bin2, Name),
    {ok, Dec}.

msg_handle(Proto) ->
    RecordName = lib_tool:get_record_name(Proto),
    Route = route_helper:route(RecordName),
    RoleId = net_login:get_role_id(),
    NetPid = self(),
    case Route of
        ?UNDEF ->
            ?LOG_ERROR("undefined handle proto~n");
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
                    ?LOG_INFO("RoleId ~p", [RoleId]),
            if
                is_integer(RoleId) ->
                    lib_role:register_name(RoleId) ! {c2s, RoleId, NetPid, Mod, Proto},
                    ok;
                true ->
                    ok
            end
    end.
