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

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% =============================================================================
%% Tests
%% =============================================================================
state_sleep_looper_empty_arg_test() ->
	LoopTimeout = 1,
	Count = 3,
	
	OkFun = fun() ->
		inc_counter(ok_fun_calls),
		{ok, fun_result}
	end,
	Result1 = test_utils:state_sleep_looper(OkFun, [], LoopTimeout, Count),
	?assertEqual(fun_result, Result1),
	% function exited after first iteration
	?assertEqual(1, get_counter(ok_fun_calls)),
	
	ErrorFun = fun() ->
		inc_counter(error_fun_calls),
		{error, error_fun}
	end,
	try
		test_utils:state_sleep_looper(ErrorFun, [], LoopTimeout, Count)
	catch error:Exception ->
		?assertEqual(error_fun, Exception)
	end,
	% function exited after Count iterations
	?assertEqual(Count, get_counter(error_fun_calls)).

% Function should end either with {ok, RetValue} or {error, RetValue}
state_sleep_looper_wrong_fun_test() ->
	LoopTimeout = 1,
	Count = 3,
	
	WrongFun = fun() ->
		inc_counter(wrong_fun_calls),
		wrong_return_value
	end,
	try
		test_utils:state_sleep_looper(WrongFun, [], LoopTimeout, Count)
	catch error:Exception ->
		?assertEqual({case_clause, wrong_return_value}, Exception)
	end,
	% function exited after first iteration
	?assertEqual(1, get_counter(wrong_fun_calls)).
	
% wrong parameters to fun
state_sleep_looper_wrong_arity_test() ->
	set_counter(function_calls, 0),
	Fun = fun(arg1, arg2) ->
		inc_counter(function_calls),
		{ok, value}
	end,

	try
		test_utils:state_sleep_looper(Fun, [arg1, arg2, arg3], 1, 3)
	catch error:Exception ->
		?assertEqual({error, {badarity, {Fun, [arg1, arg2, arg3]}}}, Exception)
	end,
	% function was never called
	?assertEqual(0, get_counter(function_calls)).

state_sleep_looper_args_test() ->
	LoopTimeout = 1,
	Count = 3,
	
	Fun = fun
		(arg1, arg2) ->
			inc_counter(function_calls),
			FunCalls = get_counter(function_calls),
			case get_counter(exit_fun_after) of
				FunCalls ->
					{ok, value};
				_ ->
					{error, exit_fun}
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
		?assertEqual(exit_fun, Exception1)
	end,
	% function exited after Count iterations
	?assertEqual(Count, get_counter(function_calls)).

%% =============================================================================
%% Property based tests
%% =============================================================================

%% =============================================================================
%% Local functions
%% =============================================================================

%%------------------------------------------------------------------------------
%% @spec inc_counter(Name) -> void()
%% where
%%		Name = atom()
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
%%		Name = atom()
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
%%		Name = atom()
%%		Count = integer()
%% @doc Get counter value from process dictionary.
%% @end
%%------------------------------------------------------------------------------
get_counter(Name) ->
	get(Name).

%%------------------------------------------------------------------------------
%% @spec set_counter(Name, N) -> OldN
%% where
%%		Name = atom()
%%		N = integer()
%%		OldN = integer()
%% @doc Set counter value in process dictionary.
%% @end
%%------------------------------------------------------------------------------
set_counter(Name, N) ->
	put(Name, N).

-endif.