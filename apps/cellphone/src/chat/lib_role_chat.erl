-module(lib_role_chat).

-include("global.hrl").

-export([
         to_tab_chat_record/1
        ]).

to_tab_chat_record(#s_chat{role_id = RoleId, role_name = RoleName, time = Time, picture = Picture, content = Content}) ->
    #tab_chat_record{
        role_id = RoleId,
        role_name = RoleName,
        picture = Picture,
        time = Time,
        content = Content
      }.
