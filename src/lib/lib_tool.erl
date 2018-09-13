-module(lib_tool).

-export([
         parse_args/1
        ]).

parse_args(Bin) ->
    Key = <<>>,
    parse_key(Bin, Key, []).

parse_key(<<>>, _Key, List) ->
    {ok, List};
parse_key(<<$=, Rest/bits>>, Key, List) ->
    parse_value(Rest, Key, <<>>, List);
parse_key(<<B, Rest/bits>>, Key, List) ->
    parse_key(Rest, <<Key/binary, B>>, List).

parse_value(<<$&, Rest/bits>>, Key, Value, List) ->
    parse_key(Rest, <<>>, [List | {Key, Value}]);
parse_value(<<B, Rest/bits>>, Key, Value, List) ->
    parse_value(Rest, Key, <<Value/binary, B>>, List).
