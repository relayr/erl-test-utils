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
-include_lib("eqc/include/eqc.hrl").
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
    unmeck_modules/0
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
		error:{Assertion, _Info} when Count > 1, (Assertion == assertion_failed) or (Assertion == assertEqual_failed) or (Assertion == assertMatch_failed) or (Assertion == assertException_failed) ->
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
			stop_process(Pid)
	end;
stop_process(Pid) when is_pid(Pid) ->
	try
		erlang:exit(Pid, kill)
	catch 
		_Class:_Reason ->
			ok
	end,
	wait_for_process_stopped(Pid),
	ok.

-spec meck_module(Module :: atom(), Funs :: [{atom(), any()} | {atom(), non_neg_integer(), any()}]) -> ok.
meck_module(Module, Funs) ->
    ok = meck:new(Module, [unstick, passthrough]),
    lists:foreach(
           fun(FunctionSpec) -> 
                   ok = meck_function(Module, FunctionSpec)
           end, Funs).

meck_function(Module, {FunctionName, Fun}) when is_function(Fun) ->
    meck:expect(Module, FunctionName, Fun);
meck_function(Module, {FunctionName, FunResult}) ->
    Arity = get_function_arity(Module, FunctionName),
    Fun = create_fun_with_arity(Arity, FunResult),                                           
    meck:expect(Module, FunctionName, Fun).

get_function_arity(Module, FunctionName) ->
    ModuleInfo = Module:module_info(functions),
    {FunctionName, Arity} = lists:keyfind(FunctionName, 1, ModuleInfo),
    Arity.

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
    fun(_, _, _, _, _, _, _) -> FunResult end.

unmeck_modules()  ->
    meck:unload(),
    ok.

