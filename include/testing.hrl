-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(test_utils, [shuffle/1]).

-compile(export_all).
-compile({parse_transform, test_parse_transform}).

-define(TEST_FUN(), -test_function([])).

-define(LOOPER(Fun, Args, LoopTimeout, Count), 
		test_utils:state_sleep_looper(Fun, Args, LoopTimeout, Count)).
-define(WAIT_FOR_PROCESS_STOPPED(ProcessID), 
		test_utils:wait_for_process_stopped(ProcessID)).
-define(RECOMPILE(Module, FunctionDefs),
		test_utils:recompile_module(Module, FunctionDefs)).
-define(MECK(Module, Funs), test_utils:meck_module(Module, Funs)).
-define(MECK_LOOP(Module, Funs), test_utils:meck_loop_module(Module, Funs)).
-define(UNMECK, test_utils:unmeck_modules()).

% QuickCheck generators
-define(EQC_STRING_GEN, non_empty(list(choose($a,$z)))).
-define(EQC_ATOM_GEN, ?LET(Name, ?EQC_STRING_GEN, list_to_atom(Name))).
-define(EQC_LIST_GEN, ?LET(Name, non_empty(list(choose(0,255))), Name)).
-define(EQC_SORTED_UNIQUE_LIST_GEN(Gen), ?LET(Values2, list(Gen), lists:usort(Values2))).
-define(EQC_UNIQUE_LIST_GEN(Gen), ?LET(List, ?EQC_SORTED_UNIQUE_LIST_GEN(Gen), shuffle(List))).
-define(EQC_NESTED_LIST_GEN, ?LET(List, ?EQC_LIST_GEN, List)).
-define(EQC_BYTE_GEN, choose(0,255)).
-define(EQC_USHORT_GEN, choose(0,65535)).
-define(EQC_ULONG_GEN, choose(0,18446744073709551615)).
-define(EQC_BIN_GEN(MaxSize),
		?LET(Size,
			 choose(0, MaxSize),
			 ?LET(List,
				  vector(Size, ?EQC_BYTE_GEN),
				  list_to_binary(List)))).

-endif.
