-module(beam_diag_ffi).
-export([diag_line/0]).

%% Returns a one-line diagnostics string about BEAM schedulers and processes.
diag_line() ->
    SchedsOnline = erlang:system_info(schedulers_online),
    Schedulers = erlang:system_info(schedulers),
    ProcCount = erlang:system_info(process_count),
    ProcLimit = erlang:system_info(process_limit),
    Uptime = erlang:statistics(wall_clock),
    {_, SinceStartMs} = Uptime,
    lists:flatten(io_lib:format(
      "BEAM: sched_online=~p sched_total=~p proc=~p/~p uptime_ms=~p",
      [SchedsOnline, Schedulers, ProcCount, ProcLimit, SinceStartMs]
    )).
