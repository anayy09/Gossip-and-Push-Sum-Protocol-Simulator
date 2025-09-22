-module(gh_diag_ffi).
-export([diag_line_bin/0]).

%% Returns a UTF-8 binary suitable for Gleam String.
diag_line_bin() ->
    SchedsOnline = erlang:system_info(schedulers_online),
    Schedulers = erlang:system_info(schedulers),
    ProcCount = erlang:system_info(process_count),
    ProcLimit = erlang:system_info(process_limit),
    Uptime = erlang:statistics(wall_clock),
    {_, SinceStartMs} = Uptime,
    Str = io_lib:format(
            "BEAM: sched_online=~p sched_total=~p proc=~p/~p uptime_ms=~p",
            [SchedsOnline, Schedulers, ProcCount, ProcLimit, SinceStartMs]
          ),
    unicode:characters_to_binary(Str).
