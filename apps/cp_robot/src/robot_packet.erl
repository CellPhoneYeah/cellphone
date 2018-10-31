-module(robot_packet).

-include("robot.hrl").
-include("../../cellphone/src/include/global.hrl").

-export([
         decode/2
        ]).

decode(#state{buffer = Buffer} = State, Data) ->
    AllBin = <<Buffer/binary, Data/binary>>,
    case parse_frame_header(AllBin) of
        {ok, Opcode, Packet, Rest} ->
            if 
                Opcode == 8 ->
                    % 服务端主动断开连接
                    stop;
                true ->
                    case catch net_misc:decode_msg(Packet) of
                        {ok, Dec} ->
                            {next, Dec, Rest};
                        Error ->
                            ?PRINT("robot decode error:~p, data:~p, role_id:~p", [Error, AllBin, State]),
                            {next, ?UNDEF, Rest}
                    end
            end;
        {more, Rest} ->
            {more, Rest}
    end.

%
parse_frame_header(<<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, Len:7, PayloadLen:16, Packet:PayloadLen/binary, Rest/binary>>) when Len == 126 -> 
    {ok, Opcode, Packet, Rest};
parse_frame_header(<<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, Len:7, PayloadLen:64, Packet:PayloadLen/binary, Rest/binary>>) when Len == 127 ->
    {ok, Opcode, Packet, Rest};
parse_frame_header(<<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, Len:7, Packet:Len/binary, Rest/binary>>) ->
    {ok, Opcode, Packet, Rest};
parse_frame_header(Bin) ->
    {more, Bin}.

