%%% =====
%%% @author yxf
%%% @doc
%%% cowboy请求处理模块
%%% @end
%%% =====
-module(cowboy_http_handler).

-export([
         init/3,
         terminate/3
        ]).

init(_, Req0, _Opts) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"hello erlang">>,
                           Req0),
    {ok, Req, undefined}.

% handle(Req, State) ->
%     {_Param1, Req1} = cowboy_req:qs_val(<<"param1">>, Req),
%     {_Param2, Req2} = cowboy_req:qs_val(<<"param2">>, Req1),
%     {ok, ReqReturn} = cowboy_req:reply(200, [{<<"content=type">>, <<"text/plain">>}], <<"hello world">>, Req2),
%     {ok, ReqReturn, State}.

terminate(_Reason, _Req, _State) ->
    ok.
