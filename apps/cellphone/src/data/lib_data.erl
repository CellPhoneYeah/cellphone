-module(lib_data).

-include("data.hrl").

-export([
        dirty_read/2,
        dirty_write/2,
        dirty_all_keys/1,
        dirty_first/1,
        dirty_next/2
        ]).

dirty_all_keys(Tab) ->
    mnesia:dirty_all_keys(Tab).

dirty_read(Tab, Key) ->
    mnesia:dirty_read(Tab, Key).

dirty_write(Tab, Data) ->
    mnesia:dirty_write(Tab, Data).

dirty_first(Tab) ->
    mnesia:dirty_first(Tab).

dirty_next(Tab, Key) ->
    mnesia:dirty_next(Tab, Key).
