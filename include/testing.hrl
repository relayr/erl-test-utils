-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-compile({parse_transform, test_parse_transform}).

-define(TEST_FUN(), -test_function([])).

-define(LOOPER(Fun, Args, LoopTimeout, Count), 
		test_utils:state_sleep_looper(Fun, Args, LoopTimeout, Count)).
-define(WAIT_FOR_PROCESS_STOPPED(ProcessID), 
		test_utils:wait_for_process_stopped(ProcessID)).
-define(RECOMPILE(Module, FunctionDefs),
		test_utils:recompile_module(Module, FunctionDefs)).

-define(BEFORE_TEST, {current_function, {Module, Function, _Arity}} = erlang:process_info(self(), current_function),
	default_logger:log(4, "BEGIN ~p:~p", [Module, Function])).
-define(AFTER_TEST, {current_function, {Module, Function, _Arity}} = erlang:process_info(self(), current_function),
	default_logger:log(4, "END ~p:~p", [Module, Function])).

% QuickCheck generators
-define(EQC_ATOM_GEN, ?LET(Name, non_empty(list(choose($a,$z))), list_to_atom(Name))).
-define(EQC_LIST_GEN, ?LET(Name, non_empty(list(choose($a,$z))))).

-endif.
