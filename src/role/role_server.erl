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
         get_net_pid/0
        ]).

-define(NET_PID, net_pid).

%%% =====
%%% API
%%% =====
start_link(RoleId, NetPid) ->
    io:format("start role RoleId ~p~n", [RoleId]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [RoleId, NetPid], []).

role_login(Role, NetPid) ->
    set_net_pid(NetPid),
    ets:insert(?ETS_ROLE, Role).

all_online_role() ->
    First = ets:first(?ETS_ROLE),
    get_online_role(First, []).

get_net_pid() ->
    erlang:get(?NET_PID).

%%% =====
%%% call back
%%% =====
init([_RoleId, _NetPid]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

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
