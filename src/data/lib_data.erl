-module(lib_data).

-include("data.hrl").

-export([
         get_role_id_by_name/1,
         get_role_by_id/1,
         insert_role/2
        ]).

%% 用户db操作
get_role_id_by_name(Name) ->
    case catch mnesia:dirty_read(tab_account, Name) of
        [#tab_account{name = Name, role_id = RoleId}] ->
            RoleId;
        [] ->
            not_exists
    end.

get_role_by_id(Id) ->
    case catch mnesia:dirty_read(tab_role, Id) of
        [#tab_role{} = Role] ->
            Role;
        Reason ->
            Reason
    end.

insert_role(Name, Password) ->
    NewId = ?DEFAULT_ROLE_ID + mnesia:dirty_update_counter(?TAB_UNIQUE, ?TAB_ROLE, 1),
    NewAccount = #tab_account{role_id = NewId, name = Name},
    mnesia:dirty_write(tab_account, NewAccount),
    NewRole = #tab_role{id = NewId, name = Name, password = Password},
    mnesia:dirty_write(tab_role, NewRole).

%% 聊天记录db
