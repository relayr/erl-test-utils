%%------------------------------------------------------------------------------
%% @author jodias
%% @version 1.0
%% @doc Various utilities for testing.
%% @end
%%------------------------------------------------------------------------------
-module(test_utils).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Function exports
%%------------------------------------------------------------------------------
-export([
    state_sleep_looper/4,
    wait_for_process_stopped/1,
    wait_for_process_stopped/2,
    stop_processes/1,
    meck_module/2,
    meck_loop_module/2,
    unmeck_modules/0,
    unmeck_module/1,
    meck_call_args/2,
    meck_last_call_args/2,
    meck_num_calls/2,
    shuffle/1
]).

%% =============================================================================
%% Exported functions
%% =============================================================================
%%------------------------------------------------------------------------------
%% @spec state_sleep_looper(Fun, Args, LoopTimeout, Count) -> Result
%% where
%%        Fun = fun()
%%        Args = list()
%%        LoopTimeout = integer()
%%        Count = integer()
%%        Result = any()
%% @doc Call function with given arguments. If function throws an exception then call
%%        function again after LoopTimeout milliseconds.
%%        If above operations failed for Count times then throw an exception outside
%%        the looper otherwise return Result.
%%        Function 'Fun' should return Result or throw an exception.
%% @end
%%------------------------------------------------------------------------------
state_sleep_looper(Fun, Args, LoopTimeout, Count) when is_function(Fun), is_list(Args), 
        LoopTimeout > 0, Count > 0 ->
    try
        erlang:apply(Fun, Args)
    catch 
        error:{Assertion, _Info} when Count > 1, (Assertion == assert) orelse (Assertion == assertEqual) orelse
                                                 (Assertion == assertNotEqual) orelse (Assertion == assertMatch) orelse
                                                 (Assertion == assertException) ->
            timer:sleep(LoopTimeout),
            state_sleep_looper(Fun, Args, LoopTimeout, Count - 1)
    end.

%%------------------------------------------------------------------------------
%% @spec wait_for_process_stopped(ProcessID) -> ok
%% where
%%        ProcessID = pid() | atom()
%% @doc Wait for process with given ProcessID to be stopped.
%% @end
%%------------------------------------------------------------------------------

wait_for_process_stopped(ProcessID) ->
    wait_for_process_stopped(ProcessID, 1000).

wait_for_process_stopped(undefined, _Timeout) ->
    ok;
wait_for_process_stopped(ProcessID, Timeout) when is_pid(ProcessID) ->
    wait_until_process_status_is_undefined(ProcessID, Timeout);
wait_for_process_stopped(ProcessName, Timeout) when is_atom(ProcessName) ->
    ok = wait_until_process_status_is_undefined(whereis(ProcessName), Timeout),
    wait_until_name_is_unregistered(ProcessName, Timeout).

wait_until_process_status_is_undefined(undefined, _Timeout) ->
    ok;
wait_until_process_status_is_undefined(ProcessID, Timeout) ->
    LoopTimeout = 100,
    LoopCount = Timeout div LoopTimeout,
    state_sleep_looper(
        fun(PID) ->
            case erlang:process_info(PID, status) of
                undefined ->
                    ok;
                _ ->
                    ErrorMsg = lists:flatten(io_lib:format("Process ~p wasn't stopped!", [PID])),
                    erlang:error({assert, ErrorMsg})
            end
        end,
        [ProcessID], LoopCount, LoopTimeout).

wait_until_name_is_unregistered(ProcessName, Timeout) ->
    LoopTimeout = 100,
    LoopCount = Timeout div LoopTimeout,
    state_sleep_looper(
        fun(Name) ->
            RegisteredNames = erlang:registered(),
            case lists:member(Name, RegisteredNames) of
                true ->
                    ErrorMsg = lists:flatten(io_lib:format("Process ~p wasn't stopped!", [Name])),
                    erlang:error({assert, ErrorMsg});
                false ->
                    ok
            end
        end,
        [ProcessName], LoopCount, LoopTimeout).

% @doc Stops processes in list and waits for their termination. The list can contain names or pids.
-spec stop_processes(Processes :: [atom() | pid()]) -> ok.
stop_processes(Processes) when is_list(Processes) ->
    lists:foreach(
      fun(P) ->
            stop_process(P)
      end, Processes).

stop_process(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            true = erlang:unregister(Name),
            stop_process(Pid)
    end;
stop_process(Pid) when is_pid(Pid) ->
    try
        true = erlang:unlink(Pid),
        erlang:exit(Pid, kill)
    catch 
        _Class:_Reason ->
            ok
    end,
    wait_for_process_stopped(Pid),
    ok.

-spec meck_module(Module :: atom(), Funs :: [{atom(), any()}, ...]) -> ok.
meck_module(Module, Funs) ->
    ok = meck_module_init(Module),
    lists:foreach(
        fun(FunctionSpec) ->
            ok = meck_function(Module, FunctionSpec)
        end,
    Funs).

meck_module_init(Module) ->
    DefaultOptions = [passthrough, non_strict],
    Options =
        try
            _ = Module:module_info(),
            [unstick] ++ DefaultOptions
        catch error:undef ->
            % module doesn't exist, try without unstick option
            DefaultOptions
        end,
    try
        meck:new(Module, Options)
    catch error:{already_started, _} ->
        ok
    end.
    
-spec meck_loop_module(Module :: atom(), Funs :: [{atom(), list()}, ...]) -> ok.
meck_loop_module(Module, Funs) ->
    ok = meck_module_init(Module),
    lists:foreach(
      fun({FunctionName, FunResults}) ->
      [FunResult|_] = FunResults,
      if is_function(FunResult) ->
        {arity, Arity} = erlang:fun_info(FunResult, arity),
        meck:loop(Module, FunctionName, Arity, FunResults);
      true ->
        Arities = get_function_arities(Module, FunctionName),
        _ = [ok = meck:loop(Module, FunctionName, Arity, FunResults) || Arity <- Arities]
      end
    end, Funs).

meck_function(Module, {FunctionName, Fun}) when is_function(Fun) ->
    meck:expect(Module, FunctionName, Fun);
meck_function(Module, {FunctionName, FunResult}) ->
    Arities = get_function_arities(Module, FunctionName),
    _ = [meck:expect(Module, FunctionName, [{lists:duplicate(Arity, '_'), FunResult}]) || Arity <- Arities],
    ok.

get_function_arities(Module, FunctionName) ->
    ModuleInfo = Module:module_info(functions),
    Arities = [Arity || {FN, Arity} <- ModuleInfo, FN == FunctionName],
    if Arities =:= [] ->
        % return 0 arity if none is specified
        [0];
    true ->
        Arities
    end.

unmeck_modules()  ->
    meck:unload().

unmeck_module(Module)  ->
    meck:unload(Module).

meck_call_args(Module, Function) ->
    History = meck:history(Module),
    Args = [Args || {_Pid, {Mod, Func, Args}, _Result} <- History, Func =:= Function, Mod =:= Module],
    {ok, Args}.

meck_num_calls(Module, Function) ->
    meck:num_calls(Module, Function, '_').

meck_last_call_args(Module, Function) ->
    {ok, CallArgs} = meck_call_args(Module, Function),
    if CallArgs =:= [] ->
        {ok, []};
    true ->
        LastCallArgs = lists:last(CallArgs),
        {ok, [LastCallArgs]}
    end.

shuffle([])     -> [];
shuffle([Elem]) -> [Elem];
shuffle(List)   -> shuffle(List, length(List), []).

shuffle([], 0, Result) ->
    Result;
shuffle(List, Len, Result) ->
    {Elem, Rest} = nth_rest(rand:uniform(Len), List),
    shuffle(Rest, Len - 1, [Elem|Result]).

nth_rest(N, List) -> nth_rest(N, List, []).

nth_rest(1, [E|List], Prefix) -> {E, Prefix ++ List};
nth_rest(N, [E|List], Prefix) -> nth_rest(N - 1, List, [E|Prefix]).