-module(monotime_ffi).
-export([now_milliseconds/0]).

%% Return monotonic time in milliseconds as an integer.
now_milliseconds() ->
    erlang:convert_time_unit(erlang:monotonic_time(), native, millisecond).
