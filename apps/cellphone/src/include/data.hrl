%% 用户数据表
-define(TAB_ROLE, tab_role).
-record(?TAB_ROLE, {
          id = 0,           %% id
          name = "",        %% 名字
          picture = 0,      %% 头像
          password = ""     %% 密码
         }).
-define(DEFAULT_ROLE_ID, 10000).

%% 聊天记录
-define(TAB_CHAT_RECORD, tab_chat_record).
-record(?TAB_CHAT_RECORD, {
           time = 0,        %% 时间
           role_id = 0,     %% 用户id
           role_name = "",  %% 用户名
           picture = 0,     %% 头像
           content = ""}).  %% 内容


-define(ALL_TABLES,
        [
         {?TAB_ROLE, ?TAB_ROLE, record_info(fields, ?TAB_ROLE), mod_role},
         {?TAB_CHAT_RECORD, ?TAB_CHAT_RECORD, record_info(fields, ?TAB_CHAT_RECORD), mod_role_chat}
        ]).
