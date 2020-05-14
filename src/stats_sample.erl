-module(stats_sample).

-export([sample/3, calculate_values_given_sample/1]).

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
sample(linear, Fun, SampleSize) when is_function(Fun), ?is_positive(SampleSize) ->
    Times = [ stats_helper:time_one_sample(Fun) || _ <- lists:seq(1, SampleSize)],
    calculate_values_given_sample(Times);
sample(parallel, Fun, SampleSize) when is_function(Fun), ?is_positive(SampleSize) ->
    Processes = [ stats_helper:one_parallel_sample(Fun) || _ <- lists:seq(1, SampleSize)],
    Times = [ stats_helper:get_time_from_parallel_sample(Pid, Ref) || {Pid, Ref} <- Processes],
    calculate_values_given_sample(Times).


-spec calculate_values_given_sample([number()]) -> sample().
calculate_values_given_sample(Sample) when is_list(Sample) ->
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
