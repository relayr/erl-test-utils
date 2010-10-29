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
	state_sleep_looper/4
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
	Result = try
		erlang:apply(Fun, Args)
	catch T:E ->
		{error, {T, E}}
	end,
	case Result of
		{ok, RetValue} ->
			RetValue;
		{error, RetValue} when Count =< 1 ->
			erlang:error(RetValue);
		{error, _RetValue} ->
			timer:sleep(LoopTimeout),
			state_sleep_looper(Fun, Args, LoopTimeout, Count - 1)
	end.