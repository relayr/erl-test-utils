%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2010
%% @version 1.0
%% @doc test_utils_tests
%% @end
%%------------------------------------------------------------------------------
-module(test_utils_tests).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

%% =============================================================================
%% Tests
%% =============================================================================
-ifdef(TEST).
-include("../include/testing.hrl").

-define(TS, 1563443663000).

%% =============================================================================
%% Tests
%% =============================================================================

state_sleep_looper_empty_args_test() ->
    LoopTimeout = 1,
    Count = 3,

    OkFun = fun() ->
        inc_counter(ok_fun_calls),
        ok_fun_result
    end,
    Result1 = test_utils:state_sleep_looper(OkFun, [], LoopTimeout, Count),
    ?assertEqual(ok_fun_result, Result1),
    % function exited after first iteration
    ?assertEqual(1, get_counter(ok_fun_calls)),

    ErrorFun = fun() ->
        inc_counter(error_fun_calls),
        erlang:error(error_fun_result)
    end,
    try
        test_utils:state_sleep_looper(ErrorFun, [], LoopTimeout, Count)
    catch error:Exception ->
        ?assertEqual(error_fun_result, Exception)
    end,
    % function exited after Count iterations
    ?assertEqual(1, get_counter(error_fun_calls)).

% wrong parameters to fun
state_sleep_looper_wrong_arity_test() ->
    set_counter(function_calls, 0),
    Fun = fun(arg1, arg2) ->
        inc_counter(function_calls),
        value
    end,

    try
        test_utils:state_sleep_looper(Fun, [arg1, arg2, arg3], 1, 3)
    catch error:Exception ->
        ?assertEqual({badarity, {Fun, [arg1, arg2, arg3]}}, Exception)
    end,
    % function was never called
    ?assertEqual(0, get_counter(function_calls)).

state_sleep_looper_args_test() ->
    LoopTimeout = 1,
    Count = 3,
    ExpExc = {assert, info},
    Fun = fun
        (arg1, arg2) ->
            inc_counter(function_calls),
            FunCalls = get_counter(function_calls),
            case get_counter(exit_fun_after) of
                FunCalls ->
                    value;
                _ ->
                    erlang:error(ExpExc)
            end
    end,

    % function will exit in the 2nd iteration
    set_counter(exit_fun_after, 2),
    set_counter(function_calls, 0),
    Result1 = test_utils:state_sleep_looper(Fun, [arg1, arg2], LoopTimeout, Count),
    ?assertEqual(value, Result1),
    % function exited in the 2nd iteration
    ?assertEqual(2, get_counter(function_calls)),

    % function will exit after 4 calls but there're only 3 loops
    set_counter(exit_fun_after, 4),
    set_counter(function_calls, 0),
    try
        test_utils:state_sleep_looper(Fun, [arg1, arg2], LoopTimeout, Count)
    catch error:Exception1 ->
        ?assertEqual(ExpExc, Exception1)
    end,
    % function exited after Count iterations
    ?assertEqual(Count, get_counter(function_calls)).

wait_for_process_stopped_test() ->
    PID = spawn(fun() -> receive stop -> ok end end),
    true = register(proc, PID),
    ErrorMsg = lists:flatten(io_lib:format("Process ~p wasn't stopped!", [PID])),
    ?assertException(error, {assert, ErrorMsg}, test_utils:wait_for_process_stopped(PID, 300)),
    ?assertException(error, {assert, ErrorMsg}, test_utils:wait_for_process_stopped(proc, 300)),
    proc ! stop,
    ok = test_utils:wait_for_process_stopped(PID),
    ok = test_utils:wait_for_process_stopped(proc),
    % not existing process
    ok = test_utils:wait_for_process_stopped(proc2).

positive_comparision_of_json_test() ->
    JsonA = <<"{\"list1\":[1,2,{\"a\":1},[{\"b\":3,\"c\":4}]],\"list2\":[10,20,{\"a\":10},[{\"b\":30,\"c\":40}]]}">>,
    JsonB = <<"{\"list2\":[10,20,{\"a\":10},[{\"b\":30,\"c\":40}]],\"list1\":[1,2,{\"a\":1},[{\"b\":3,\"c\":4}]]}">>,
    ?assertJsonEqual(JsonA, JsonB).

second_positive_comparision_of_json_test() ->
    JsonC = <<"{
        \"list1\":[
            1,
            2,
            {\"a\":1},
            [{\"b\":3,\"c\":4}]
        ],
        \"list2\":[
            10,
            20,
            {\"a\":10},
            [{\"c\":40,\"b\":30}]
        ]
    }">>,
    JsonD = <<"{
        \"list2\":[
            10,
            20,
            {\"a\":10},
            [{\"b\":30,\"c\":40}]
        ],
        \"list1\":[
            1,
            2,
            {\"a\":1},
            [{\"b\":3,\"c\":4}]
        ]
    }">>,
    %% [{\"c\":40,\"b\":30}] -> [{\"b\":30,\"c\":40}] should make no difference
    ?assertJsonEqual(JsonC, JsonD).

other_json_sorting_cases_test() ->
    EmptyObjectJson = <<"{\"attributes\":{},\"aaa\":5}">>,
    ?assertJsonEqual(EmptyObjectJson, EmptyObjectJson),
    EmptyArrayJson = <<"{\"attributes\":[],\"aaa\":5}">>,
    ?assertJsonEqual(EmptyArrayJson, EmptyArrayJson),
    MixedKeyJsonTerm1 = [{bbb, <<"string">>}, {<<"aaa">>, 15}, {42, true}, {41, false}],
    MixedKeyJsonTerm2 = [{<<"bbb">>, <<"string">>}, {aaa, 15}, {42, true}, {<<"41">>, false}],
    ?assertJsonEqual(MixedKeyJsonTerm1, MixedKeyJsonTerm2).

negative_comparision_of_json_test() ->
    JsonC = <<"[
        [
            1,
            2,
            {\"a\":1},
            [{\"b\":3,\"c\":4}]
        ],
        [
            10,
            20,
            {\"a\":10},
            [{\"c\":40,\"b\":30}]
        ]
    ]">>,
    JsonD = <<"[
        [
            10,
            20,
            {\"a\":10},
            [{\"c\":40,\"b\":30}]
        ],
        [
            1,
            2,
            {\"a\":1},
            [{\"b\":3,\"c\":4}]
        ]
    ]">>,
    JC = ?SORT_PROPERTIES_IN_JSON(JsonC),
    JD = ?SORT_PROPERTIES_IN_JSON(JsonD),
    ?assertNotEqual(JC, JD).

meck_assert_called_once_not_found_test() ->
    ok = mock_ts_utils(),
    _ = ts_utils:get_os_timestamp(),
    _ = ts_utils:get_os_timestamp(secs),
    _ = ts_utils:get_os_timestamp(millis),
    %% For extra meck history entry. Should be ignored by filter in history invocation.
    _ = ts_utils:convert_timestamp(452312356),
    Error =
        {assert, [
            {module,test_utils_tests},
            {line, 212},
            {matcher, "ts_utils:get_os_timestamp(utc,millis) called 1 time(s)"},
            {expected, {ts_utils, get_os_timestamp, [utc, millis]}},
            {actual, [
                {ts_utils, get_os_timestamp, []},
                {ts_utils, get_os_timestamp, [secs]},
                {ts_utils, get_os_timestamp, [millis]}
            ]}
        ]},
    ?assertError(Error, ?assertCalledOnce(ts_utils, get_os_timestamp, [utc, millis])),
    ?assertEqual(3, test_utils:meck_num_calls(ts_utils, get_os_timestamp)).

meck_assert_not_called_test() ->
    ok = mock_ts_utils(),
    _ = ts_utils:get_os_timestamp(),
    _ = ts_utils:get_os_timestamp(secs),
    _ = ts_utils:get_os_timestamp(millis),
    %% For extra meck history entry. Should be ignored by filter in history invocation.
    _ = ts_utils:convert_timestamp(452312356),
    Error =
        {assert, [
            {module,test_utils_tests},
            {line, 234},
            {matcher, "ts_utils:get_os_timestamp(...) called 0 time(s)"},
            {expected, not_called},
            {actual, [
                {ts_utils, get_os_timestamp, []},
                {ts_utils, get_os_timestamp, [secs]},
                {ts_utils, get_os_timestamp, [millis]}
            ]}
        ]},
    ?assertError(Error, ?assertNotCalled(ts_utils, get_os_timestamp)),
    ?assertEqual(3, test_utils:meck_num_calls(ts_utils, get_os_timestamp)).

meck_assert_not_called_with_args_test() ->
    ok = mock_ts_utils(),
    _ = ts_utils:get_os_timestamp(),
    _ = ts_utils:get_os_timestamp(secs),
    _ = ts_utils:get_os_timestamp(millis),
    %% For extra meck history entry. Should be ignored by filter in history invocation.
    _ = ts_utils:convert_timestamp(452312356),
    Error =
        {assert, [
            {module,test_utils_tests},
            {line, 256},
            {matcher, "ts_utils:get_os_timestamp(millis) called 0 time(s)"},
            {expected, not_called},
            {actual, [
                {ts_utils, get_os_timestamp, []},
                {ts_utils, get_os_timestamp, [secs]},
                {ts_utils, get_os_timestamp, [millis]}
            ]}
        ]},
    ?assertError(Error, ?assertNotCalled(ts_utils, get_os_timestamp, [millis])),
    ?assertEqual(3, test_utils:meck_num_calls(ts_utils, get_os_timestamp)).

mock_ts_utils() ->
    % 'ts_utils' module is created on the fly by meck
    ?MECK_AND_RESET(ts_utils, [
        {get_os_timestamp, ?TS},
        {get_os_timestamp, fun
            (secs) -> ?TS div 1000;
            (millis) ->?TS
        end},
        {convert_timestamp, fun(T) -> T * 1000 end}
    ]).

meck_loop_module_test() ->
    ?MECK_LOOP(ts_utils, [
        {get_unix_timestamp, [1000, 2000, 3000]}
    ]),
    ?assertEqual(1000, ts_utils:get_unix_timestamp()),
    ?assertEqual(2000, ts_utils:get_unix_timestamp()),
    ?assertEqual(3000, ts_utils:get_unix_timestamp()),
    ?assertEqual(1000, ts_utils:get_unix_timestamp()).

meck_call_args_test() ->
    ok = mock_ts_utils(),
    ?assertEqual({ok, []}, test_utils:meck_last_call_args(ts_utils, get_os_timestamp)),
    ?assertEqual({ok, []}, test_utils:meck_call_args(ts_utils, get_os_timestamp)),
    _ = ts_utils:get_os_timestamp(),
    _ = ts_utils:get_os_timestamp(secs),
    _ = ts_utils:get_os_timestamp(millis),
    ?assertEqual({ok, [[millis]]}, test_utils:meck_last_call_args(ts_utils, get_os_timestamp)),
    ?assertEqual({ok, [[],[secs],[millis]]}, test_utils:meck_call_args(ts_utils, get_os_timestamp)).

stop_processes_test() ->
    PID1 = spawn(fun() -> receive stop -> ok end end),
    PID2 = spawn(fun() -> receive stop -> ok end end),
    PID3 = spawn(fun() -> receive stop -> ok end end),
    true = register(proc3, PID3),
    _Ref1 = erlang:monitor(process, PID1),
    Ref2 = erlang:monitor(process, PID2),
    Ref3 = erlang:monitor(process, PID3),
    % stop process identified by PID and name
    ok = test_utils:stop_processes([
        proc1,  % no process with such name
        PID2,
        PID2,   % another process with the same PID
        proc3,
        list_to_pid("<0.5701.2>")   % not existing process
    ]),
    receive {'DOWN', R2, process, P2, _} ->
        ?assertEqual(Ref2, R2),
        ?assertEqual(PID2, P2)
    after 1000 ->
        erlang:error(not_stopped)
    end,
    receive {'DOWN', R3, process, P3, _} ->
        ?assertEqual(Ref3, R3),
        ?assertEqual(PID3, P3)
    after 1000 ->
        erlang:error(not_stopped)
    end,
    % no other process should be stopped
    receive Msg ->
        erlang:error({stopped, Msg})
    after 0 ->
        ok
    end.

shuffle_test() ->
    ?assertEqual([], test_utils:shuffle([])),
    ?assertEqual([1], test_utils:shuffle([1])),
    ?assertEqual([2,2,2], test_utils:shuffle([2,2,2])),
    % random generated list
    ListLen = 10,
    RandomList = [rand:uniform(10) || _ <- lists:seq(1, ListLen)],
    ShuffledList = test_utils:shuffle(RandomList),
    ?assertEqual(ListLen, length(ShuffledList)),
    ?assertEqual(lists:sort(RandomList), lists:sort(ShuffledList)).

%% =============================================================================
%% Property based tests
%% =============================================================================

%% =============================================================================
%% Local functions
%% =============================================================================

%%------------------------------------------------------------------------------
%% @spec inc_counter(Name) -> void()
%% where
%%        Name = atom()
%% @doc Increment counter value in process dictionary.
%% @end
%%------------------------------------------------------------------------------
inc_counter(Name) ->
    case get_counter(Name) of
        undefined ->
            set_counter(Name, 1);
        N ->
            set_counter(Name, N + 1)
    end.

%%------------------------------------------------------------------------------
%% @spec dec_counter(Name) -> void()
%% where
%%        Name = atom()
%% @doc Decrement counter value in process dictionary.
%% @end
%%------------------------------------------------------------------------------
dec_counter(Name) ->
    case get_counter(Name) of
        undefined ->
            set_counter(Name, -1);
        N when is_integer(N) ->
            set_counter(Name, N - 1)
    end.

%%------------------------------------------------------------------------------
%% @spec get_counter(Name) -> Count
%% where
%%        Name = atom()
%%        Count = integer()
%% @doc Get counter value from process dictionary.
%% @end
%%------------------------------------------------------------------------------
get_counter(Name) ->
    get(Name).

%%------------------------------------------------------------------------------
%% @spec set_counter(Name, N) -> OldN
%% where
%%        Name = atom()
%%        N = integer()
%%        OldN = integer()
%% @doc Set counter value in process dictionary.
%% @end
%%------------------------------------------------------------------------------
set_counter(Name, N) ->
    put(Name, N).

-endif.
