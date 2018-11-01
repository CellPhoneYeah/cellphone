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
         all_online_role/0,
         get_net_pid/0,
         add_online_role/1,
         del_online_role/1,
         get_role_id_by_name/1,
         add_name_role_id/2
        ]).

-export([
         unicast/2,
         cast_all/1
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
    gen_server:start_link(?MODULE, [Role, NetPid], []).

all_online_role() ->
    First = ets:first(?ETS_ROLE),
    get_online_role(First, []).

get_net_pid() ->
    erlang:get(?NET_PID).

cast_all(Msg) ->
    First = ets:first(?ETS_ROLE),
    cast_all(First, Msg).
cast_all(?EOT, _Msg) ->
    ok;
cast_all(RoleId, Msg) ->
    ?LOG_INFO("RoleId ~p", [RoleId]),
    unicast(RoleId, Msg),
    cast_all(ets:next(?ETS_ROLE, RoleId), Msg).

unicast(NetPid, Msg) when is_pid(NetPid) -> % 发到网关进程
    NetPid ! {toc, Msg};
unicast(RoleId, Msg) when is_integer(RoleId)->
    NetPid = net_server:get_role_netpid(RoleId),
    NetPid ! {toc, Msg}.

%%% =====
%%% call back
%%% =====
init([#tab_role{id = RoleId} = Role, NetPid]) ->
    erlang:monitor(process, NetPid), % 监控网关进程，网关进程死了，玩家进程也死掉{'DOWN', _, _, _, Reason}
    set_net_pid(NetPid),
    RegisterName = lib_role:register_name(RoleId),
    % erlang:unregister(RegisterName),
    erlang:register(RegisterName, self()),
    mod_role:set_role(Role),
    {ok, []}.

handle_call(Request, _From, State) ->
    Reply1 = case catch do_handle_call(Request) of
        {ok, Reply} ->
            Reply;
        {error, Error} ->
            ?LOG_ERROR("~p", [Error]),
            ?UNDEF
    end,
    {noreply, Reply1, State}.

handle_cast(Request, State) ->
    case catch do_handle_cast(Request) of
        ok ->
            ok;
        {error, Error} ->
            ?LOG_ERROR("~p", [Error])
    end,
    {noreply, State}.

handle_info(Request, State) ->
    case catch do_handle_info(Request) of
        ok ->
            {noreply, State};
        stop ->
            {stop, normal, State};
        {error, Error} ->
            ?LOG_ERROR("~p", [Error]),
            {noreply, State}
    end.

terminate(_State, _Reason) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% =====
%%% internal
%%% =====
%% 网关停止引起用户进程停止
do_handle_info({'DOWN', _, _, _, Reason}) ->
    ?LOG_ERROR("~p", [Reason]),
    stop;
do_handle_info({c2s, RoleId, NetPid, Mod, Proto}) ->
    Mod:handle_c2s(RoleId, NetPid, Proto),
    ok;
do_handle_info({s2s, {Mod, Proto}}) ->
    Mod:handle_s2s(Proto),
    ok;
do_handle_info(Other) ->
    ?LOG_ERROR("unknown request ~p", [Other]).

do_handle_cast(Other) ->
    ?LOG_ERROR("unknown request ~p", [Other]).

do_handle_call(Other) ->
    ?LOG_ERROR("unknown request ~p", [Other]).

get_online_role(?EOT, AllOnlineRole) ->
    AllOnlineRole;
get_online_role(Key, AllOnlineRole) ->
    [OnlineRole] = ets:lookup(?ETS_ROLE, Key),
    get_online_role(ets:next(?ETS_ROLE, Key), [OnlineRole | AllOnlineRole]).

set_net_pid(NetPid) ->
    erlang:put(?NET_PID, NetPid).
