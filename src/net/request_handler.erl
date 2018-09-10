%%% =====
%%% @author yxf
%%% @doc
%%% cowboy请求处理模块
%%% @end
%%% =====
-module(request_handler).

-export([
         init/2
        ]).

init(Req, State) ->
    handle(Req, State),
    {ok, Req, State}.

handle(Req, State) ->                                          
    Path = path(Req),
    Resource = gen_resource(Path),
    io:format("resource ~p~n", [Resource]),
    serve_file(Resource, Req, State).

serve_file(File, Req, State) ->                            
    Val = file:read_file(File),                              
    case Val of                                              
        {error, _} ->                                            
            io:format("*** no page called ~s~n",[File]),         
            send_error(Req);
        {ok, Bin} ->                                             
            Ext = filename:extension(File),
            MimeType = get_mime_type(Ext),
            Req1 = send_page(MimeType, Bin, Req),
            {ok, Req1, State}                                      
    end.                                                     

path(Req) ->
    binary_to_list(cowboy_req:path(Req)).

gen_resource("/") ->
    get_web_files_path() ++ "index.html";
gen_resource(Path) ->
    get_web_files_path() ++ Path.

send_page(MimeType, Data, Req) ->                       
    cowboy_req:reply(200, #{<<"Content-Type">> =>   
                            MimeType},   
                     Data, Req).                            

send_error(Req) ->
    Error = <<"<pre>bad request</pre>">>,
    send_page(get_mime_type(error), Error, Req).

get_web_files_path() ->
    {ok, Path} = file:get_cwd(),
     Path ++ "/src/web_files/".

get_mime_type(".js") ->
   <<"application/x-javascript">>;
get_mime_type(".html") ->
    <<"text/html">>;
get_mime_type(_Other) ->
    <<"text/html">>.

%% websocket handle
%websocket_handle({text, Msg}, Req, Pid) ->               
%    %% This is a Json message from the browser
%    case catch decode(Msg) of
%        {'EXIT', _Why} ->
%            Pid ! {invalidMessageNotJSON, Msg};
%        {struct, _} = Z ->
%            X1 = atomize(Z),
%            Pid ! {self(), X1};
%        Other ->
%            Pid ! {invalidMessageNotStruct, Other}
%    end,
%    {ok, Req, Pid}.
%
%websocket_info({send,Str}, Req, Pid) ->
%    {reply, {text, Str}, Req, Pid, hibernate};
%websocket_info([{cmd,_}|_]=L, Req, Pid) ->
%    B = list_to_binary(encode([{struct,L}])),
%    {reply, {text, B}, Req, Pid, hibernate};
%websocket_info(Info, Req, Pid) ->
%    io:format("Handle_info Info:~p Pid:~p~n",[Info,Pid]),
%    {ok, Req, Pid, hibernate}.
%
%websocket_terminate(_Reason, _Req, Pid) ->
%    io:format("websocket.erl terminate:~n"),
%    exit(Pid, socketClosed),
%    ok.                               
%
%binary_to_atom(B) ->
%        list_to_atom(binary_to_list(B)).
%
%%tomize turns all the keys in a struct to atoms             
%atomize({struct,L}) ->
%    {struct, [{binary_to_atom(I), atomize(J)} || {I, J} <- L]};
%atomize(L) when is_list(L) ->
%    [atomize(I) || I <- L];
%atomize(X) ->
%    X.                       
