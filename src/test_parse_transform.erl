%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2010
%% @version 1.0
%% @doc parse_transform function used to transform '-test_function()' annotations
%%		before compilation.
%% @end
%%------------------------------------------------------------------------------
-module(test_parse_transform).

-export([parse_transform/2]).

parse_transform(Tree, _Options) ->
    transform_tree(Tree, [], false).

transform_tree([{attribute, _, module, [SubDirectoryAtom, ModuleNameAtom]} = A | Rest], Tree, IsATestFunction)
  		when is_atom(SubDirectoryAtom), is_atom(ModuleNameAtom) ->
	ModuleName = atom_to_list(SubDirectoryAtom) ++ "." ++ atom_to_list(ModuleNameAtom),
    put(module_name, ModuleName),
    transform_tree(Rest, [A | Tree], IsATestFunction);
transform_tree([{attribute, _, module, ModuleNameAtom} = A | Rest], Tree, IsATestFunction) when is_atom(ModuleNameAtom) ->
	ModuleName = atom_to_list(ModuleNameAtom),
    put(module_name, ModuleName),
    transform_tree(Rest, [A | Tree], IsATestFunction);
transform_tree([{attribute, _, test_function, _Elem} | Rest], Tree, _IsATestFunction) ->
    transform_tree(Rest, Tree, true);
transform_tree([F | Rest], Tree, false) ->
    transform_tree(Rest, [F | Tree], false);
transform_tree([{function, _, _, _, _} = F | Rest], Tree, true) ->
    NewF = transform_function(F),
    transform_tree(Rest, [NewF | Tree], false);
transform_tree([Element | Rest], Tree, IsATestFunction) ->
    transform_tree(Rest, [Element | Tree], IsATestFunction);
transform_tree([], Tree, _) ->
    lists:reverse(Tree).

transform_function({function, Line, FunNameAtom, Arity, [Clause]}) ->
	ModuleName = get(module_name),
	FunName = atom_to_list(FunNameAtom),
    put(function_name, FunName),
    put(function_arity, Arity),
	% append '_test' to the function name if not already appended
	{IsATestGenerator, NewFunName} = 
		case re:run(FunName, "_test$", []) of
            nomatch ->
				case re:run(FunName, "_test_$", []) of
					nomatch ->
						{false, FunName ++ "_test"};
					_ ->
						{true, FunName}
				end;
			_ ->
				{false, FunName}
		end,
    NewClause = 
		if IsATestGenerator ->
 			transform_test_suite_clause(Clause, ModuleName, FunName);
		true ->
			transform_test_clause(Clause, ModuleName, FunName, undefined, true)
		end,
    {function, Line, list_to_atom(NewFunName), Arity, [NewClause]}.

transform_test_clause(Clause, ModuleName, FunName, TestName, UnmockBeforeAndAfterClause) ->
    % suite tests are unmocked after teardown phase
    BeforeClause = transform_clause_before("BEGIN", Clause, ModuleName, FunName, TestName, UnmockBeforeAndAfterClause),
    transform_clause_after("END", BeforeClause, ModuleName, FunName, TestName, UnmockBeforeAndAfterClause).

transform_test_suite_clause({clause, L1, CArgs, Guards, FunctionClauses}, ModuleName, FunName) ->
	% get last element in tuple - test suite
	case lists:split(length(FunctionClauses) - 1, FunctionClauses) of
		{FunctionClausesFront, [{tuple, L2,
								 [{atom, L3, TestSuiteGenerator = inparallel},
								  TestSuiteFunctionClauses]}]} ->
 			TransformedTestSuiteFunctionClauses = transform_test_functions_clause(TestSuiteGenerator, TestSuiteFunctionClauses,
																				  ModuleName, FunName, 1),
			{clause, L1, CArgs, Guards, FunctionClausesFront ++ 
										[{tuple, L2, [{atom, L3, TestSuiteGenerator},
													  TransformedTestSuiteFunctionClauses]}]};
		{FunctionClausesFront, [{tuple, L2,
								 [{atom, L3, TestSuiteGenerator},
								  {'fun', L4, {clauses, [TestSuiteBeforeClause]}},
								  {'fun', L5, {clauses, [TestSuiteAfterClause]}},
								  TestSuiteFunctionClauses]}]} ->
			TransformedTestSuiteBeforeClause = transform_clause_before("BEFORE", TestSuiteBeforeClause, ModuleName, FunName, undefined, true),
			TransformedTestSuiteAfterClause = transform_clause_after("AFTER", TestSuiteAfterClause, ModuleName, FunName, undefined, true),
 			TransformedTestSuiteFunctionClauses = transform_test_functions_clause(TestSuiteGenerator, TestSuiteFunctionClauses,
																				  ModuleName, FunName, 1),
			{clause, L1, CArgs, Guards, FunctionClausesFront ++ 
										[{tuple, L2, [{atom, L3, TestSuiteGenerator},
													  {'fun', L4, {clauses, [TransformedTestSuiteBeforeClause]}},
													  {'fun', L5, {clauses, [TransformedTestSuiteAfterClause]}},
													  TransformedTestSuiteFunctionClauses]}]};
		{FunctionClausesFront, FunctionClausesEnd} ->
			{clause, L1, CArgs, Guards, FunctionClausesFront ++ FunctionClausesEnd}
	end.

transform_test_function_meta_data(ModuleName, FunName, TestName, {'fun', L, {clauses, [TestFunClause]}}) ->
	TransformedTestFunClause = transform_test_clause(TestFunClause, ModuleName, FunName, TestName, false),
	{'fun', L, {clauses, [TransformedTestFunClause]}};
transform_test_function_meta_data(ModuleName, FunName, TestName, {'fun', L, {function, TestFunName, 0}}) ->
	TestFunClause = {clause, L, [], [], [{call,L,{atom,L,TestFunName},[]}]},
	TransformedTestFunClause = transform_test_clause(TestFunClause, ModuleName, FunName, TestName, false),
	{'fun', L, {clauses, [TransformedTestFunClause]}}.

transform_test_functions_clause(_TestSuiteGenerator, {nil, L}, _ModuleName, _FunName, _TestNumber) ->
	{nil, L};
% [{X, fun() -> test() end | fun test/0}]
transform_test_functions_clause(TestSuiteGenerator = foreachx, {cons, L1,
                                                                {tuple, L2, [X, {'fun', L3, {function, _TestFunName, _FunArity} = FunMetaData}]},
                                                                NextTestFunctionsClause},
                                ModuleName, FunName, TestNumber) ->
    % TODO: transform correctly
%%     TransformedFunMetaData = transform_test_function_meta_data(ModuleName, FunName, test_name(L3, TestNumber), FunMetaData),
    TransformedFunMetaData = FunMetaData,
    TransformedNextTestFunctionsClause = transform_test_functions_clause(TestSuiteGenerator, NextTestFunctionsClause,
                                                                         ModuleName, FunName, TestNumber + 1),
    {cons, L1, {tuple, L2, [X, {'fun', L3, TransformedFunMetaData}]}, TransformedNextTestFunctionsClause};
% [{X, [{"Test name", fun() -> test() end | fun test/0}, ...]}]
transform_test_functions_clause(TestSuiteGenerator = foreachx, {cons, L1,
                                                                {tuple, L2, [X, {'fun', L3, {clauses, TestClausesForX}}]},
																NextTestFunctionsClause},
								ModuleName, FunName, TestNumber) ->
    {TransformedTestClausesForX, NextTestNumber} =
        lists:mapfoldl(fun({clause, L4, TestClauseArgs, [], [TestClauseForX]}, TestNumberAcc) ->
                            TransformedTestClauseForX = transform_test_functions_clause(foreach, TestClauseForX, ModuleName, FunName, TestNumberAcc),
                            {{clause, L4, TestClauseArgs, [], [TransformedTestClauseForX]}, TestNumberAcc + 1}
                       end,
                       TestNumber,
                       TestClausesForX),
	TransformedNextTestFunctionsClause = transform_test_functions_clause(TestSuiteGenerator, NextTestFunctionsClause,
																		 ModuleName, FunName, NextTestNumber),
	{cons, L1, {tuple, L2, [X, {'fun', L3, {clauses, TransformedTestClausesForX}}]}, TransformedNextTestFunctionsClause};
% [{"Test name", fun() -> test() end | fun test/0}]
transform_test_functions_clause(TestSuiteGenerator, {cons, L1,
													 {tuple, L2, [TestName, {'fun', _, _} = FunMetaData]},
													 NextTestFunctionsClause},
								ModuleName, FunName, TestNumber) ->
	TransformedFunMetaData = transform_test_function_meta_data(ModuleName, FunName, TestName, FunMetaData),
	TransformedNextTestFunctionsClause = transform_test_functions_clause(TestSuiteGenerator, NextTestFunctionsClause,
																		 ModuleName, FunName, TestNumber + 1),
	{cons, L1, {tuple, L2, [TestName, TransformedFunMetaData]}, TransformedNextTestFunctionsClause};
% [fun() -> test() end | fun test/0]
transform_test_functions_clause(TestSuiteGenerator, {cons, L1,
													{'fun', _, _} = FunMetaData, NextTestFunctionsClause},
								ModuleName, FunName, TestNumber) ->
	TransformedFunMetaData = transform_test_function_meta_data(ModuleName, FunName, test_name(L1, TestNumber), FunMetaData),
	TransformedNextTestFunctionsClause = transform_test_functions_clause(TestSuiteGenerator, NextTestFunctionsClause,
																		 ModuleName, FunName, TestNumber + 1),
	{cons, L1, TransformedFunMetaData, TransformedNextTestFunctionsClause};
% list comprehension
% [{"Test name", fun() -> test() end | fun test/0}]
transform_test_functions_clause(_TestSuiteGenerator, {lc, L1, {tuple, L2, [TestName, {'fun', _, _} = FunMetaData]}, LCInput},
								ModuleName, FunName, _TestNumber) ->
 	TransformedFunMetaData = transform_test_function_meta_data(ModuleName, FunName, TestName, FunMetaData),
	{lc, L1, {tuple, L2, [TestName, TransformedFunMetaData]}, LCInput}.

transform_clause_before(BeforeString, {clause, L, CArgs, Guards, Body}, ModuleName, FunName, undefined, UnmockBeforeClause) when is_list(ModuleName),
																							                                     is_list(FunName) ->
    FirstCall = {call,L,{remote,L,{atom,L,default_logger},{atom,L,log_once}},
		[{integer,L,4},{string,L, "~n======================================== " ++ BeforeString ++
							 " ~s:~s ========================================"},{cons,L,{string,L,ModuleName},
												  {cons,L,{string,L,FunName},
												  {nil,L}}},
		 {atom,L,false}]},
    UnmockCall = unmock_modules_clause(L, UnmockBeforeClause),
	NewBody = [FirstCall | UnmockCall ++ Body],
	{clause, L, CArgs, Guards, NewBody};
transform_clause_before(BeforeString, {clause, L, CArgs, Guards, Body}, ModuleName, FunName, TestName, UnmockBeforeClause) when is_list(ModuleName),
																											                    is_list(FunName) ->
    FirstCall = {call,L,{remote,L,{atom,L,default_logger},{atom,L,log_once}},
		[{integer,L,4},{string,L, "~n======================================== " ++ BeforeString ++
						" ~s:~s (~s) ========================================"},{cons,L,{string,L,ModuleName},
													 {cons,L,{string,L,FunName},
													 {cons,L,TestName,
													 {nil,L}}}},
		 {atom,L,false}]},
    UnmockCall = unmock_modules_clause(L, UnmockBeforeClause),
	NewBody = [FirstCall | UnmockCall ++ Body],
	{clause, L, CArgs, Guards, NewBody}.

transform_clause_after(AfterString, {clause, L, CArgs, Guards, Body}, ModuleName, FunName, undefined, UnmockAfterClause) when is_list(ModuleName),
																										                      is_list(FunName) ->
    LoggerCall = [{call,L,{remote,L,{atom,L,default_logger},{atom,L,log_once}},
		[{integer,L,4},{string,L, "======================================== " ++ AfterString ++
						   " ~s:~s ========================================~n"},{cons,L,{string,L,ModuleName},
												{cons,L,{string,L,FunName},
												{nil,L}}},
		 {atom,L,false}]}],
    UnmockCall = unmock_modules_clause(L, UnmockAfterClause),
	NewBody = Body ++ UnmockCall ++ LoggerCall,
	{clause, L, CArgs, Guards, NewBody};
transform_clause_after(AfterString, {clause, L, CArgs, Guards, Body}, ModuleName, FunName, TestName, UnmockAfterClause) when is_list(ModuleName),
																										                     is_list(FunName) ->
    LoggerCall = [{call,L,{remote,L,{atom,L,default_logger},{atom,L,log_once}},
		[{integer,L,4},{string,L, "======================================== " ++ AfterString ++
					  " ~s:~s (~s) ========================================~n"},{cons,L,{string,L,ModuleName},
													 {cons,L,{string,L,FunName},
													 {cons,L,TestName,
													 {nil,L}}}},
		 {atom,L,false}]}],
    UnmockCall = unmock_modules_clause(L, UnmockAfterClause),
	NewBody = Body ++ UnmockCall ++ LoggerCall,
	{clause, L, CArgs, Guards, NewBody}.

unmock_modules_clause(_L, false) ->
    [];
unmock_modules_clause(L, true) ->
    [{call,L,{remote,L,{atom,L,test_utils},{atom,L,unmeck_modules}}, []}].

test_name(L, TestNumber) ->
	{string, L, "Test " ++ integer_to_list(TestNumber)}.