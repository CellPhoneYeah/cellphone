-module(role_server).

-behaviour(gen_server).

-include("global.hrl").

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([
         start_link/2,
         role_login/2,
         all_online_role/0,
         get_net_pid/0,
         add_online_role/1,
         del_online_role/1,
         get_role_id_by_name/1,
         add_name_role_id/2,
         get_role/0,
         set_role/1
        ]).

-define(NET_PID, net_pid).

%%% =====
%%% API
%%% =====
add_online_role(Role) ->
    ets:insert(?ETS_ROLE, Role).

del_online_role(RoleId) ->
    ets:delete(?ETS_ROLE, RoleId).

%% 增加名字和玩家id映射
add_name_role_id(RoleName, RoleId) ->
    ets:insert(?ETS_NAME_ROLE_ID, #ets_name_role_id{role_name = RoleName, role_id = RoleId}).

%% 用玩家id获取对应名字
get_role_id_by_name(RoleName) ->
    case ets:lookup(?ETS_NAME_ROLE_ID, RoleName) of
        [#ets_name_role_id{role_id = RoleId}] ->
            RoleId;
        _ ->
            ?UNDEF
    end.

start_link(Role, NetPid) ->
    ?LOG_INFO("start role RoleId ~p~n", [Role]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Role, NetPid], []).

role_login(Role, NetPid) ->
    set_net_pid(NetPid),
    ets:insert(?ETS_ROLE, Role).

all_online_role() ->
    First = ets:first(?ETS_ROLE),
    get_online_role(First, []).

get_net_pid() ->
    erlang:get(?NET_PID).

set_role(Role) ->
    erlang:put({?MODULE, role}, Role).

get_role() ->
    erlang:get({?MODULE, role}).

%%% =====
%%% call back
%%% =====
init([Role, NetPid]) ->
    set_net_pid(NetPid),
    set_role(Role),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({c2s, RoleId, NetPid, Mod, Proto}, State) ->
    Mod:handle_tos(RoleId, NetPid, Proto),
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.

terminate(_State, _Reason) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% =====
%%% internal
%%% =====
get_online_role(?EOT, AllOnlineRole) ->
    AllOnlineRole;
get_online_role(Key, AllOnlineRole) ->
    [OnlineRole] = ets:lookup(?ETS_ROLE, Key),
    get_online_role(ets:next(?ETS_ROLE, Key), [OnlineRole | AllOnlineRole]).

set_net_pid(NetPid) ->
    erlang:put(?NET_PID, NetPid).
