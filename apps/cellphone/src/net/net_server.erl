%%% =====
%%% @author yxf
%%% @doc
%%% 网络层的监控进程, 保证cowboy正常运行
%%% @end
%%% =====
-module(net_server).

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
        add_role_netpid/2,
        del_role_netpid/1,
        get_role_netpid/1
        ]).

-export([
         all/0,
         stop/1
        ]).

-record(state, {}).

%%% =====
%%% API
%%% =====
all() ->
    ets:match(?ETS_NETPID, '$1').

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?ETS_NETPID = ets:new(?ETS_NETPID, [set, named_table, public, {keypos, #ets_netpid.role_id}]),
    Router = [
              {'_', [
                     {"/websocket", websocket_handler, []}
                     %{"/", request_handler, []}
                    ]
              }
             ],

    Port = lib_config:listen_port(),
    Dispatch = cowboy_router:compile(Router),
    {ok, _} = cowboy:start_clear(cellphone_listener,
                                 [{port, Port}, {max_connections, 10240}, {num_acceptors, 10}],
                                #{env => #{dispatch => Dispatch}}
                                ),
    ?LOG_INFO("net_server started"),
    {ok, #state{}}.

add_role_netpid(RoleId, NetPid) ->
    ets:insert(?ETS_NETPID, #ets_netpid{role_id = RoleId, netpid = NetPid}).

get_role_netpid(RoleId) ->
    case ets:lookup(?ETS_NETPID, RoleId) of
        [] ->
            ?UNDEF;
        [#ets_netpid{netpid = NetPid}] ->
            NetPid
    end.

del_role_netpid(RoleId) ->
    ets:delete(?ETS_NETPID, RoleId).

stop(RoleId) ->
    NetPid = get_role_netpid(RoleId),
    NetPid ! stop,
    erlang:monitor(process, NetPid),
    receive
        {'DOWN', _, process, _, _} ->
            ok
    after 3000 ->
              ?LOG_ERROR("stop net pid fail"),
              del_role_netpid(RoleId),
              role_server:del_online_role(RoleId),
              exit(NetPid, kill)
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
