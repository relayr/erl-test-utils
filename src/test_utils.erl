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
	wait_for_process_stopped/1
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
%% @doc Call function with given arguments. If function exits with {error, RetValue}
%%		result or throws an exception then call function again after LoopTimeout
%%		milliseconds.
%%		If above operations failed for Count times then throw an exception otherwise 
%%		return RetValue.
%%		Function 'Fun' should return {ok, RetValue} or {error, RetValue} tuple.
%% @end
%%------------------------------------------------------------------------------
state_sleep_looper(Fun, Args, LoopTimeout, Count) when is_function(Fun), is_list(Args), 
		LoopTimeout > 0, Count > 0 ->
	try
		erlang:apply(Fun, Args)
	catch _Type:_Exception when Count > 1 ->
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
					erlang:error(ErrorMsg)
			end
		end,
		[ProcessID], 10, 100).