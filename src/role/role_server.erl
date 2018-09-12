-module(role_server).

-behaviour(gen_server).

-include("role.hrl").
-include("data.hrl").

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([
         start_link/0,
         role_login/2,
         is_role_online/1,
         all_online_role/0
        ]).

%%% =====
%%% API
%%% =====
start_link() ->
    io:format("start role server~n"),
    ?ETS_ONLINE_ROLE = ets:new(?ETS_ONLINE_ROLE, [set, protected, named_table, {keypos, #ets_online_role.role_name}]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

role_login(Role, NetPid) ->
    #tab_role{name = RoleName} = Role,
    OnlineRole = #ets_online_role{net_pid = NetPid, role_name = RoleName},
    ets:insert(?ETS_ONLINE_ROLE, OnlineRole).

is_role_online(Name) ->
    ets:member(?ETS_ONLINE_ROLE, Name).

all_online_role() ->
    First = ets:first(?ETS_ONLINE_ROLE),
    get_online_role(First, []).

%%% =====
%%% call back
%%% =====
init([]) ->
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
get_online_role('$end_of_table', AllOnlineRole) ->
    AllOnlineRole;
get_online_role(Key, AllOnlineRole) ->
    [OnlineRole] = ets:lookup(?ETS_ONLINE_ROLE, Key),
    get_online_role(ets:next(?ETS_ONLINE_ROLE, Key), [OnlineRole | AllOnlineRole]).
