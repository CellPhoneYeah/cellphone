-module(robot_client).

-include("global.hrl").

-behaviour(gen_server).

-export([
        handle_info/2,
        handle_cast/2,
        handle_call/3,
        terminate/2,
        code_change/3
        ]).

-export([
         start_link/0,
         init/1
        ]).

-export([
         register/2,
         login/2
        ]).

-record(state, {
          socket
         }).

%%% =====
%%% API
%%% =====
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(RoleName, Psd)
  when is_list(RoleName) andalso is_list(Psd) ->
    Proto = #register_tos{role_name = RoleName, psd = Psd},
    gen_server:cast(?MODULE, Proto).

login(RoleName, Psd) ->
    Proto = #login_tos{role_name  = RoleName, psd = Psd},
    gen_server:cast(?MODULE, Proto).

%%% =====
%%% callback
%%% =====
init([]) ->
    ?LOG_INFO("start link"),
    Args = [binary, {active, false}, {packet, raw}, {reuseaddr, true}],
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 8089, Args),
    {ok, #state{socket = Socket}}.

handle_info(ResBin, State) ->
    case net_misc:decode(ResBin) of
        {ok, Response} ->
            ?LOG_INFO("Response ~p~n", [Response]);
        Error ->
            ?LOG_ERROR(Error)
    end,
    {noreply, State}.

handle_cast(Proto, #state{socket = Socket} = State) ->
    {ok, Bin} = net_misc:encode(Proto),
    gen_tcp:send(Socket, Bin),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
