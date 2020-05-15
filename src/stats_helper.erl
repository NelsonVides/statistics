-module(stats_helper).

-export([
         time_one_sample/1,
         one_parallel_sample/1,
         get_time_from_parallel_sample/2
        ]).

-spec time_one_sample(fun(() -> any())) -> integer().
time_one_sample(Fun) ->
    T1 = erlang:monotonic_time(),
    _Val = Fun(),
    T2 = erlang:monotonic_time(),
    erlang:convert_time_unit(T2 - T1, native, microsecond).

-spec one_parallel_sample(fun(() -> any())) -> {pid(), reference()}.
one_parallel_sample(Fun) ->
    MeasuringFun = fun() ->
                           T1 = erlang:monotonic_time(),
                           _Val = Fun(),
                           T2 = erlang:monotonic_time(),
                           Time = erlang:convert_time_unit(T2 - T1, native, microsecond),
                           erlang:exit(Time)
                   end,
    {_Pid, _Ref} = spawn_monitor(MeasuringFun).

-spec get_time_from_parallel_sample(pid(), reference()) -> number().
get_time_from_parallel_sample(Pid, Ref) ->
    receive
        {'DOWN', Ref, process, Pid, Info} ->
            Info
    end.
