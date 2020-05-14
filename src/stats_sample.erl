-module(stats_sample).

-export([sample/3]).

-define(is_positive(Num), is_integer(Num) andalso Num > 0).

-type sample() ::
    #{
        min := pos_integer(),
        max := non_neg_integer(),
        mean := non_neg_integer(),
        variance := number(),
        deviation := non_neg_integer()
    }.
-export_type([sample/0]).

-spec sample(Strategy, Fun, SampleSize) -> Res when
    Strategy :: linear | parallel,
    Fun :: fun(() -> any()),
    SampleSize :: non_neg_integer(),
    Res :: sample().
sample(linear, Fun, SampleSize) when ?is_positive(SampleSize) ->
    Times = [ time_one_sample(Fun) || _ <- lists:seq(1, SampleSize)],
    calculate_values_given_sample(Times);
sample(parallel, Fun, SampleSize) ->
    Processes = [ one_parallel_sample(Fun) || _ <- lists:seq(1, SampleSize)],
    Times = [ get_time_from_process(Pid, Ref) || {Pid, Ref} <- Processes],
    calculate_values_given_sample(Times).


-spec calculate_values_given_sample([number()]) -> sample().
calculate_values_given_sample(Sample) ->
    Mean = average(Sample),
    Variance = average([ math:pow(Mean - T, 2) || T <- Sample ]),
    StandardDeviation = math:sqrt(Variance),
    #{
        min => lists:min(Sample),
        max => lists:max(Sample),
        mean => Mean,
        variance => Variance,
        deviation => StandardDeviation
    }.

-spec average([number()]) -> number().
average(Values) ->
    lists:sum(Values) / length(Values).

%%%'
-spec time_one_sample(fun(() -> any())) -> integer().
time_one_sample(Fun) ->
    T1 = erlang:monotonic_time(),
    _Val = Fun(),
    T2 = erlang:monotonic_time(),
    erlang:convert_time_unit(T2 - T1, native, microsecond).

-spec one_parallel_sample(fun(() -> any())) -> integer().
one_parallel_sample(Fun) ->
    MeasuringFun = fun() ->
                           T1 = erlang:monotonic_time(),
                           _Val = Fun(),
                           T2 = erlang:monotonic_time(),
                           Time = erlang:convert_time_unit(T2 - T1, native, microsecond),
                           erlang:exit(Time)
                   end,
    {_Pid, _Ref} = spawn_monitor(MeasuringFun).

-spec get_time_from_process(pid(), reference()) -> number().
get_time_from_process(Pid, Ref) ->
    receive
        {'DOWN', Ref, process, Pid, Info} ->
            Info
    end.
%%%.

%%%'
%%% $Id$
%%% Local Variables:
%%% mode: erlang
%%% End:
%%% vim: set filetype=erlang tabstop=8 foldmarker=%%%',%%%. foldmethod=marker:
%%%.
