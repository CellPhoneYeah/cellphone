%% 用户数据表
-define(TAB_ROLE, tab_role).
-record(?TAB_ROLE, {
          id = 0,
          name = "",
          password = ""
         }).
-define(DEFAULT_ROLE_ID, 10000).


%% 用户名管理
-define(TAB_ACCOUNT, tab_account).
-record(?TAB_ACCOUNT, {
           name = "",
           role_id = 0}).

%% 唯一键最大值管理
-define(TAB_UNIQUE, tab_unique).
-record(?TAB_UNIQUE, {
           unique_id,
           unique_value}).

%% 聊天记录
-define(TAB_CHAT_RECORD, tab_chat_record).
-record(?TAB_CHAT_RECORD, {
           send_time = 0,
           role_id = 0,
           role_name = "",
           content = <<>>}).


-define(ALL_TABLES,
        [
         {?TAB_ROLE, ?TAB_ROLE, record_info(fields, ?TAB_ROLE)},
         {?TAB_ACCOUNT, ?TAB_ACCOUNT, record_info(fields, ?TAB_ACCOUNT)},
         {?TAB_CHAT_RECORD, ?TAB_CHAT_RECORD, record_info(fields, ?TAB_CHAT_RECORD)},
         {?TAB_UNIQUE, ?TAB_UNIQUE, record_info(fields, ?TAB_UNIQUE)}
        ]).
