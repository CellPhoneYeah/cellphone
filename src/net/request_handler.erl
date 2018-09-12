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
    {ok, Data, _} = cowboy_req:read_body(Req),
    Data1 = binary_to_list(Data),
    handle(Data1, Req, State),
    {ok, Req, State}.

handle("", Req, State) ->                                          
    Path = path(Req),
    Resource = gen_resource(Path),
    io:format("get file ~p~n", [Resource]),
    serve_file(Resource, Req, State);
handle(["role_name=", Name, "&role_password=", Password], _Req, _State) ->
    io:format("register name ~p~n", [binary_to_list(Name)]),
    lib_role:role_register(Name, Password);
handle(["name=", Name, "psw=", Password], _Req, _State) ->
    io:format("login name ~p~n", [binary_to_list(Name)]),
    lib_role:role_login(Name, Password);
handle(["name=", Name, "msg=", Msg], _Req, _State) ->
    io:format("send msg name ~p~n msg ~p~n", [binary_to_list(Name), binary_to_list(Msg)]),
    lib_role:role_send_msg(Name, Msg);
handle(Other, Req, _State) ->
    io:format("other ~p~n", [Other]),
    send_error(Req).

serve_file(File, Req, State) ->                            
    Val = file:read_file(File),                              
    case Val of                                              
        {error, _} ->                                            
            %%io:format("*** no page called ~s~n",[File]),         
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
gen_resource("/getmsg") ->
    get_web_files_path() ++ "/getmsg";
gen_resource(Path) ->
    get_web_files_path() ++ Path.

send_page(MimeType, Data, Req) ->                       
    cowboy_req:reply(200, #{<<"Content-Type">> =>   
                            MimeType},   
                     Data, Req).                            

send_error(Req) ->
    Error = <<"
<html>
    <head>
        <meta http-equiv=\"content-type\" content=\"text/html; charset = utf-8\" />
        <meta name author=\"yxf\" content=\"https://www.cellphone.com\" />
        <title>cellphone 605</title>
        <link rel = \"icon\" href = \"data::base64, = \">
    </head>
    <body>
        <h1 id = \"welcome_content\"> 请求错误</h1>
    </body>
</html>
">>,
    send_page(get_mime_type(error), Error, Req).

get_web_files_path() ->
    {ok, Path} = file:get_cwd(),
     Path ++ "/src/web_files/".

get_mime_type(".js") ->
   <<"application/x-javascript">>;
get_mime_type(".html") ->
    <<"text/html">>;
get_mime_type(".css") ->
    <<"text/css">>;
get_mime_type(_Other) ->
    <<"text/html">>.
