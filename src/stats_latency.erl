-module(stats_latency).

-define(is_positive(Num), is_integer(Num) andalso Num > 0).

-export([realtime_latency_on_load/3]).

-spec realtime_latency_on_load(fun(() -> any()), non_neg_integer(), non_neg_integer()) ->
    stats_sample:sample().
realtime_latency_on_load(Fun, JitterSamples, Tolerance)
  when is_function(Fun), ?is_positive(JitterSamples) ->
    % Create a printer process that tries to print regularly
    Me = self(),
    Printer = spawn(fun() -> realtime_printer(Me, erlang:monotonic_time()) end),

    % Create enough adversarial worker processes to saturate all cores
    Workers = [spawn(fun() -> realtime_worker(Fun) end)
               || _ <- lists:seq(1, erlang:system_info(schedulers))],

    % Gather jitter samples
    Jitters = case collect_jitters(JitterSamples, Tolerance) of
                  {ok, Acc} -> Acc;
                  {error, no_jitter_received} = Error -> Error
              end,
    lists:foreach(fun(Pid) -> erlang:exit(Pid, kill) end, [Printer | Workers]),
    FlushedJitters = flush_jitters(),
    case Jitters of
        {error, _} = Err -> throw(Err);
        _ ->
            stats_sample:calculate_values_given_sample(Jitters ++ FlushedJitters)
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

% Spins forever, running our function
realtime_worker(Fun) ->
    _Val = Fun(),
    realtime_worker(Fun).

% Attempt to run exactly every 100ms, and says how much we were off by, in nanoseconds
realtime_printer(Pid, LastRan) when is_pid(Pid) ->
    timer:sleep(100),
    DeltaNs = erlang:convert_time_unit(erlang:monotonic_time() - LastRan, native, nanosecond),
    JitterNs = DeltaNs - 100000000,
    JitterMs = JitterNs / 1000000,
    ct:pal("Time since last schedule Jitter: ~p ms~n", [abs(JitterMs)]),
    Pid ! {jitter, abs(JitterMs)},
    realtime_printer(Pid, erlang:monotonic_time()).

collect_jitters(Count, Tolerance) ->
    collect_jitters([], Count, Tolerance).

collect_jitters(Acc, 0, _) ->
    {ok, Acc};
collect_jitters(Acc, Count, Tolerance) ->
    receive
        {jitter, JitterMs} ->
            collect_jitters([JitterMs | Acc], Count - 1, Tolerance)
    after Tolerance ->
              {error, no_jitter_received}
    end.

flush_jitters() ->
    flush_jitters([]).

flush_jitters(Acc) ->
    receive
        {jitter, JitterMs} ->
            flush_jitters([JitterMs | Acc])
    after 0 ->
              Acc
    end.
