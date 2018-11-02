%%% =====
%%% @author yxf
%%% @doc
%%% id 管理进程,负责所有id的生成
%%% @end
%%% =====
-module(id_server).

-include("global.hrl").

-behaviour(gen_server).

-export([
         start_link/0,
         init/1
        ]).

-export([
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([
         get_max_id/1
        ]).

-define(MAX_ID, max_id).
-define(DEFAULT_ID, 10000).

-record(state, {}).

%%% =====
%%% API
%%% =====

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?ETS_ID_MANAGE = ets:new(?ETS_ID_MANAGE, [set, named_table]),
    init_role_id(),
    {ok, #state{}}.

get_max_id(Type) ->
    case ets:lookup(?ETS_ID_MANAGE, {Type, ?MAX_ID}) of
        [] ->
            ?DEFAULT_ID;
        [MaxId] ->
            MaxId
    end.

%%% =====
%%% call back
%%% =====
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
set_max_id(Type, MaxId) ->
    ets:insert(?ETS_ID_MANAGE, {{Type, ?MAX_ID}, MaxId}).

init_role_id() ->
    AllRoleIds = lib_data:dirty_all_keys(?TAB_ROLE),
    MaxId = case AllRoleIds of
                [] ->
                    ?DEFAULT_ROLE_ID;
                _ ->
                    lists:max(AllRoleIds)
            end,
    set_max_id(?TAB_ROLE, MaxId).
