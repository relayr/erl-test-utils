%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2010
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
	recompile_module/2,
	stop_processes/1,
    meck_module/2,
	meck_loop_module/2,
    unmeck_modules/0,
	meck_call_args/2,
	meck_last_call_args/2,
	meck_assert_called_once/3,
	meck_assert_not_called/3,
	meck_assert_not_called/2,
	meck_num_calls/2,
	shuffle/1
]).

%% =============================================================================
%% Exported functions
%% =============================================================================
%%------------------------------------------------------------------------------
%% @spec state_sleep_looper(Fun, Args, LoopTimeout, Count) -> Result
%% where
%%		Fun = fun()
%%		Args = list()
%%		LoopTimeout = integer()
%%		Count = integer()
%%		Result = any()
%% @doc Call function with given arguments. If function throws an exception then call
%%		function again after LoopTimeout milliseconds.
%%		If above operations failed for Count times then throw an exception outside
%%		the looper otherwise return Result.
%%		Function 'Fun' should return Result or throw an exception.
%% @end
%%------------------------------------------------------------------------------
state_sleep_looper(Fun, Args, LoopTimeout, Count) when is_function(Fun), is_list(Args), 
		LoopTimeout > 0, Count > 0 ->
	try
		erlang:apply(Fun, Args)
	catch 
		error:{Assertion, _Info} when Count > 1, (Assertion == assertion_failed) orelse (Assertion == assertEqual_failed) orelse
                                                 (Assertion == assertNotEqual_failed) orelse (Assertion == assertMatch_failed) orelse
                                                 (Assertion == assertException_failed) ->
			timer:sleep(LoopTimeout),
			state_sleep_looper(Fun, Args, LoopTimeout, Count - 1)
	end.

%%------------------------------------------------------------------------------
%% @spec wait_for_process_stopped(ProcessID) -> ok
%% where
%%		ProcessID = pid() | atom()
%% @doc Wait for process with given ProcessID to be stopped.
%% @end
%%------------------------------------------------------------------------------
wait_for_process_stopped(undefined) ->
	ok;
wait_for_process_stopped(ProcessName) when is_atom(ProcessName) ->
	wait_for_process_stopped(whereis(ProcessName));
wait_for_process_stopped(ProcessID) when is_pid(ProcessID) ->
	state_sleep_looper(
		fun(PID) ->
			case erlang:process_info(PID, status) of
				undefined ->
					ok;
				_ ->
					ErrorMsg = lists:flatten(io_lib:format("Process ~p wasn't stopped!", [PID])),
					erlang:error({assertion_failed, ErrorMsg})
			end
		end,
		[ProcessID], 10, 100).

%%------------------------------------------------------------------------------
%% @spec recompile_module(Module, FunctionDefs) -> ok
%% where
%%		Module = atom()
%%		FunctionDefs = [list()]
%% @doc Replace functions definitions in a module.
%% @end
%%------------------------------------------------------------------------------
recompile_module(Module, FunctionDefs) when is_atom(Module), is_list(FunctionDefs) ->
	{Module, _Bin, ModuleFileName} = code:get_object_code(Module),
	ModuleDir = filename:dirname(ModuleFileName),
	ok = code:unstick_dir(ModuleDir),
	{ok, ModMetaData} = smerl:for_module(Module),
	{ok, NewModMetaData} = recompile_module(Module, ModMetaData, FunctionDefs),
	ok = smerl:compile(NewModMetaData, [export_all
									   %, {d,'TEST'} - doesn't work
									   ]),
	ok = code:stick_dir(ModuleDir).

recompile_module(_Module, ModMetaData, []) ->
	{ok, ModMetaData};
recompile_module(Module, ModMetaData, [FunctionDef | RestOfFunctionDefs]) ->
	{ok, NewModMetaData} = smerl:replace_func(ModMetaData, FunctionDef),
	recompile_module(Module, NewModMetaData, RestOfFunctionDefs).

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
           end, Funs).

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
	_ = [meck:expect(Module, FunctionName, create_fun_with_arity(Arity, FunResult)) || Arity <- Arities],
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

create_fun_with_arity(0, FunResult) ->
    fun() -> FunResult end;
create_fun_with_arity(1, FunResult) ->
    fun(_) -> FunResult end;
create_fun_with_arity(2, FunResult) ->
    fun(_, _) -> FunResult end;
create_fun_with_arity(3, FunResult) ->
    fun(_, _, _) -> FunResult end;
create_fun_with_arity(4, FunResult) ->
    fun(_, _, _, _) -> FunResult end;
create_fun_with_arity(5, FunResult) ->
    fun(_, _, _, _, _) -> FunResult end;
create_fun_with_arity(6, FunResult) ->
    fun(_, _, _, _, _, _) -> FunResult end;
create_fun_with_arity(7, FunResult) ->
    fun(_, _, _, _, _, _, _) -> FunResult end;
create_fun_with_arity(8, FunResult) ->
    fun(_, _, _, _, _, _, _, _) -> FunResult end;
create_fun_with_arity(9, FunResult) ->
    fun(_, _, _, _, _, _, _, _, _) -> FunResult end.

unmeck_modules()  ->
    meck:unload(),
    ok.

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

-spec meck_assert_called_once(Module :: atom(), Function :: atom(), Args :: list()) -> ok.
meck_assert_called_once(Module, Function, Args) ->
	?assertEqual(1, meck:num_calls(Module, Function, Args)),
	ok.

-spec meck_assert_not_called(Module :: atom(), Function :: atom(), Args :: list()) -> ok.
meck_assert_not_called(Module, Function, Args) ->
	?assertEqual(0, meck:num_calls(Module, Function, Args)),
	ok.

-spec meck_assert_not_called(Module :: atom(), Function :: atom()) -> ok.
meck_assert_not_called(Module, Function) ->
	?assertEqual(0, meck_num_calls(Module, Function)),
	ok.

shuffle([])     -> [];
shuffle([Elem]) -> [Elem];
shuffle(List)   -> shuffle(List, length(List), []).

shuffle([], 0, Result) ->
    Result;
shuffle(List, Len, Result) ->
    {Elem, Rest} = nth_rest(random:uniform(Len), List),
    shuffle(Rest, Len - 1, [Elem|Result]).

nth_rest(N, List) -> nth_rest(N, List, []).

nth_rest(1, [E|List], Prefix) -> {E, Prefix ++ List};
nth_rest(N, [E|List], Prefix) -> nth_rest(N - 1, List, [E|Prefix]).
