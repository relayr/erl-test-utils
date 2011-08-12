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
	recompile_module/2
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
	ok = smerl:compile(NewModMetaData, [export_all, {d,'TEST'}]),
	ok = code:stick_dir(ModuleDir).

recompile_module(_Module, ModMetaData, []) ->
	{ok, ModMetaData};
recompile_module(Module, ModMetaData, [FunctionDef | RestOfFunctionDefs]) ->
	{ok, NewModMetaData} = smerl:replace_func(ModMetaData, FunctionDef),
	recompile_module(Module, NewModMetaData, RestOfFunctionDefs).
