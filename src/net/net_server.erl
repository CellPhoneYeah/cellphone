%%% =====
%%% @author yxf
%%% @doc
%%% 网络层的监控进程
%%% @end
%%% =====
-module(net_server).

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

-record(state, {}).

%%% =====
%%% API
%%% =====
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % do start cowboy
    % application:start(cowlib),
    % application:start(ranch),
    % application:start(cowboy),
    % Router = route_helper:get_routes(),
    Router = [
              {'_', [{'_', request_handler, []}]}
             ],
    Dispatch = cowboy_router:compile(Router),
    {ok, _} = cowboy:start_clear(cellphone_listener,
                                 [{port, 8080}],
                                #{env => #{dispatch => Dispatch}}
                                ),
    io:format("cowboy started ~n"),
    {ok, #state{}}.

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
