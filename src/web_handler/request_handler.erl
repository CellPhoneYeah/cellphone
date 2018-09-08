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
    io:format("Path ~p~n", [Path]),
    Resource = gen_resource(Path),
    serve_file(Resource, Req, State).

serve_file(File, Req, State) ->                            
    Val = file:read_file(File),                              
    case Val of                                              
        {error, _} ->                                            
            io:format("*** no page called ~s~n",[File]),         
            send_error(Req);
        {ok, Bin} ->                                             
            Req1 = send_page(Bin, Req),
            {ok, Req1, State}                                      
    end.                                                     

path(Req) ->
    binary_to_list(cowboy_req:path(Req)).

gen_resource("/") ->
    get_web_files_path() ++ "index.html";
gen_resource(Path) ->
    get_web_files_path() ++ Path.

send_page(Data, Req) ->                       
    cowboy_req:reply(200, #{<<"Content-Type">> =>   
                            <<"text/html">>},   
                     Data, Req).                            

send_error(Req) ->
    Error = <<"<pre>bad request</pre>">>,
    send_page(Error, Req).

get_web_files_path() ->
    {ok, Path} = file:get_cwd(),
     Path ++ "/src/web_files/".
