-ifdef(TEST).

-ifndef(testing_hrl).

-define(testing_hrl, true).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(test_utils, [shuffle/1]).

-compile(export_all).
-compile({parse_transform, test_parse_transform}).
-compile({parse_transform, prop_test_parse_transform}).

-define(TEST_FUN(), -test_function([])).
-define(PROP_TEST_FUN(), -prop_test_function([])).
-define(TEST_CASE(FunName), {atom_to_list(FunName), fun FunName/0}).
-define(LOOPER(Fun, Args, LoopTimeout, Count), 
		test_utils:state_sleep_looper(Fun, Args, LoopTimeout, Count)).
-define(WAIT_FOR_PROCESS_STOPPED(ProcessRegisteredNameOrID),
		test_utils:wait_for_process_stopped(ProcessRegisteredNameOrID)).
-define(RECOMPILE(Module, FunctionDefs),
		test_utils:recompile_module(Module, FunctionDefs)).
-define(MECK(Module, Funs), test_utils:meck_module(Module, Funs)).
-define(MECK_LOOP(Module, Funs), test_utils:meck_loop_module(Module, Funs)).
-define(MECK_RESET(Module), meck:reset(Module)).
-define(MECK_AND_RESET(Module, Funs), ?MECK(Module, Funs), meck:reset(Module)).
-define(UNMECK, test_utils:unmeck_modules()).
-define(UNMECK(Module), test_utils:unmeck_module(Module)).
-define(SORT(List), lists:sort(List)).

-define(IDENTITY, fun(X) -> X end).
-define(IDENTITY_OK, fun(X) -> {ok, X} end).

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

-define(assertContains(Element, List), begin ((
	fun(E, L) ->
		case lists:member(E, L) of
			true -> ok;
			__V -> erlang:error({assert,
				[{module, ?MODULE},
					{line, ?LINE},
					{expected, L ++ [E]},
					{value, L}]})
		end
	end
)(Element, List))end).

-define(assertContainsAll(Elements, List), begin ((
	fun(Es, L) ->
		case lists:all(fun(E) -> lists:member(E, L) end, Es) of
			true -> ok;
			__V -> erlang:error({assert,
				[{module, ?MODULE},
					{line, ?LINE},
					{expected, L ++ Es},
					{value, L}]})
		end
	end
)(Elements, List))end).

-define(assertNotContains(Element, List), begin ((
		fun(E, L) ->
			case lists:member(E, L) of
				false -> ok;
				__V -> erlang:error({assert,
					[{module, ?MODULE},
						{line, ?LINE},
						{expeScted, L -- [E]},
						{value, L}]})
			end
		end
)(Element, List))end).


-define(SORT_JSON(U), begin((
	%For me is tricky - i leaved commented lines for better understanding and code review.
		fun
			 SSort([Unsorted]) when is_list(Unsorted) ->
				 %?debugFmt("1 List of lists: ~p", [Unsorted]),
				 R = [[SSort(Elem) || Elem <- [Unsorted]]],
				 %?debugFmt("1 R: ~p", [R]),
				 R;

			 SSort([Tuple]) when is_tuple(Tuple) ->
				 %?debugFmt("2 List of one tuple: ~p", [Tuple]),
				 {TupleName, TupleValue} = Tuple,
				 R = [{TupleName, SSort(TupleValue)}],
				 %?debugFmt("2 R: ~p", [R]),
				 R;

			 SSort([FirstTuple | RestOfTuples]) when is_tuple(FirstTuple) andalso is_list(RestOfTuples) ->
				 %?debugFmt("3 List of many tuples: ~p", [[FirstTuple, RestOfTuples]]),
				 R = ?SORT(lists:merge([SSort(FirstTuple)], SSort(RestOfTuples))),
				 %?debugFmt("3 R: ~p", [R]),
				 R;

			 SSort(ArrayOfElements) when is_list(ArrayOfElements) ->
				 %?debugFmt("4 List of many elements: ~p", [ArrayOfElements]),
				 R = [SSort(E) || E <- ArrayOfElements],
				 %?debugFmt("4 R: ~p", [R]),
				 R;

			 SSort(Tuple) when is_tuple(Tuple) ->
				 %?debugFmt("5 Tuple: ~p", [Tuple]),
				 {AttrName, AttrValue} = Tuple,
				 R = {AttrName, SSort(AttrValue)},
				 %?debugFmt("5 R: ~p", [R]),
				 R;

			 SSort(Primitive) ->
				 %?debugFmt("Primitive: ~p", [Primitive]),
				 Primitive
		 end
))(U)end).


-define(assertJsonEqualsOrdered(Expected, Actual), begin ((
	fun(Exp, Act) ->
		StripWhitespacesAndSort = fun
									  (Binary) when is_binary(Binary) ->
										  jsx:encode(?SORT_JSON(jsx:decode(Binary)));
									  (JSX) ->
										  jsx:encode(?SORT_JSON(JSX))
								  end,
		StripedE = StripWhitespacesAndSort(Exp),
		StripedA = StripWhitespacesAndSort(Act),
		io:format("~nExpected: ~p", [StripedE]),
		io:format("~nActual  : ~p", [StripedA]),
		?assertEqual(StripedE, StripedA)
	end
)(Expected, Actual))end).


-define(assertJson(Expected, Actual), begin ((
	fun(E, A) ->
		StripWhitespaces = fun
			  (Binary) when is_binary(Binary) ->
				  jsx:encode(jsx:decode(Binary));
			  (JSX) ->
				  jsx:encode(JSX)
		  end,
		StripedE = StripWhitespaces(E),
		StripedA = StripWhitespaces(A),
		io:format("~nExpected: ~p", [StripedE]),
		io:format("~nActual  : ~p", [StripedA]),
		?assertEqual(StripedE, StripedA)
	end
)(Expected, Actual))end).

-define(assertEqualUnordered(Expected, Actual), ?assertEqual(?SORT(Expected), ?SORT(Actual))).
-define(assertNotEqualUnordered(Expected, Actual), ?assertNotEqual(?SORT(Expected), ?SORT(Actual))).

-endif.

-endif.
