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
-define(PAGE_NOT_FOUND, 0).
-define(BAD_REQUEST, 1).
-define(ROLE_NAME_HAS_USED, 2).
-define(ROLE_NOT_FOUND, 3).
-define(ROLE_PSW_WRONG, 4).

init(Req, _Opts) ->
    HasBody = cowboy_req:has_body(Req),
    {HttpCode, MimeType, Reply} = handle(HasBody, Req),
    Req1 = cowboy_req:reply(HttpCode, #{<<"Content-Type">> => MimeType}, Reply, Req),
    {ok, Req1, _Opts}.

handle(false, Req) ->
    Path = path(Req),
    send_page(Path);

handle(true, Req) ->
    {ok, KeyValues, _} = cowboy_req:read_urlencoded_body(Req),
    Path = cowboy_req:path(Req),
    KeyValues1 =  key_value_to_list(KeyValues),
    do_handle(Path, KeyValues1).

do_handle(_Path, _KeyValue) ->
    return_ok().

% do_handle(<<"/register.html">>, KeyValues) ->
%     Name = get_name(KeyValues),
%     Password = get_password(KeyValues),
%     case lib_role:role_register(Name, Password) of
%         ok ->
%             return_register_ok();
%         has_used ->
%             return_error(?ROLE_NAME_HAS_USED)
%     end;

% do_handle(<<"/login.html">>, KeyValues) ->
%     Name = get_name(KeyValues),
%     Password = get_password(KeyValues),
%     case lib_role:login(Name, Password) of
%         ok ->
%             return_ok();
%         not_found ->
%             return_error(?ROLE_NOT_FOUND);
%         password_wrong ->
%             return_error(?ROLE_PSW_WRONG)
%     end.

return_error(?BAD_REQUEST) ->
    {404, <<"text/plain">>, <<"bad request">>};
return_error(?PAGE_NOT_FOUND) ->
    {404, <<"text/plain">>, <<"page not found">>};
return_error(?ROLE_NAME_HAS_USED) ->
    {404, <<"text/plain">>, <<"role name has been used">>};
return_error(?ROLE_NOT_FOUND) ->
    {404, <<"text/plain">>, <<"role name not found">>};
return_error(?ROLE_PSW_WRONG) ->
    {404, <<"text/plain">>, <<"role password wrong">>};
return_error(_) ->
    return_error().

return_error() ->
    {404, <<"text/plain">>, <<"unknow error">>}.

return_ok() ->
    {200, <<"text/plain">>, <<"ok">>}.

send_page(Path) ->
    Resource = gen_resource(Path),
    io:format("Resource~p ~n", [Resource]),
    case filelib:is_file(Resource) of
        true ->
            {ok, Val} = file:read_file(Resource),                              
            Ext = filename:extension(Resource),
            MimeType = get_mime_type(Ext),
            {200, MimeType, Val};
        false ->
            io:format("page not found ~n"),
            return_error(?PAGE_NOT_FOUND)
    end.

key_value_to_list(KeyValues) ->
    key_value_to_list(KeyValues, []).
key_value_to_list([], List) ->
    List;
key_value_to_list([{Key, Value} | Rest], List) ->
    key_value_to_list(Rest, [{binary_to_list(Key), binary_to_list(Value)} | List]).

path(Req) ->
    binary_to_list(cowboy_req:path(Req)).

gen_resource("/") ->
    get_web_files_path() ++ "index.html";
gen_resource(Path) ->
    get_web_files_path() ++ Path.

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
