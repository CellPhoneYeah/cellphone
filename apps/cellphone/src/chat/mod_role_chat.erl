-module(mod_role_chat).

-include("global.hrl").

-export([
         handle_c2s/3
        ]).

handle_c2s(RoleId, NetPid, #chat_tos{channel = Channel, target_id = TargetId, chat = Chat}) ->
    handle_s_chat(RoleId, NetPid, Channel, TargetId, Chat).

handle_s_chat(RoleId, NetPid, _Channel, _TargetId, Chat) ->
    #tab_role{name = RoleName, picture = Picture} = mod_role:get_data(),
    NewChat = Chat#s_chat{role_id = RoleId, role_name = RoleName, time = lib_tool:now(), picture = Picture},
    role_server:cast_all(NewChat),
    chat_server:add_chat_records(NewChat),
    Toc = #chat_toc{code = ?E_OK},
    role_server:unicast(NetPid, Toc).
