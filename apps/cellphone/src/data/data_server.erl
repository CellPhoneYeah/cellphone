-module(data_server).

-behaviour(gen_server).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([
         start_link/0
        ]).

-include("global.hrl").

%%% =====
%%% API
%%% =====
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% =====
%%% call back
%%% =====
init([]) ->
    erlang:process_flag(trap_exit, true),
    do_init(),
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
do_init() ->
    ?IF(is_dir_exists(),
        ok,
        ?THROW("mnesia's path ~p is not exists~n", [mnesia:system_info(directory)])),

    case mnesia:system_info(extra_db_nodes) of
        [] ->
            mnesia:create_schema([node()]);
        _ ->
            ok
    end,
    ensure_start_mnesia(),

    ensure_tables_created(),

    ?IF(has_created_mnesia(),
        ok,
        ?THROW("mnesia dir is not used")),

    wait_for_tables().

%% 检查路径是否合法
is_dir_exists() ->
    Dir = mnesia:system_info(directory) ++ "/",
    case filelib:ensure_dir(Dir) of
        ok ->
            true;
        _ ->
            false
    end.

%% 获取实际使用的目录，确保已经启动
has_created_mnesia() ->
    mnesia:system_info(use_dir).

%% 确保启动了mnesia数据库进程
ensure_start_mnesia() ->
    application:start(mnesia, permanent),
    mnesia:change_table_copy_type(schema, node(), disc_copies).

%% 确保所有表都被创建
ensure_tables_created() ->
    [begin
         case mnesia:create_table(TabName, [{disc_copies, [node()]}, {type, set}, {attributes, Fields}]) of
             {atomic, _} ->
                 ok;
             {aborted, {already_exists, _}} ->
                 ok;
             Reason ->
                 ?THROW(Reason)
         end
     end || {TabName, _RecordName, Fields, _Mod} <- get_all_tables()].

%% 等待表格初始化
wait_for_tables() ->
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).

%% 获取所有表格
get_all_tables() ->
    ?ALL_TABLES.
