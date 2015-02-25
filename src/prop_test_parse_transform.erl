%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2010
%% @version 1.0
%% @doc parse_transform function used to transform '-test_function()' annotations
%%		before compilation.
%% @end
%%------------------------------------------------------------------------------
-module(prop_test_parse_transform).

-include_lib("logger/include/logger.hrl").

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
transform_tree([{attribute, _, prop_test_function, _Elem} | Rest], Tree, _IsATestFunction) ->
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
	% prepend function name with 'prop_' if not already appended
	NewFunName =
		case re:run(FunName, "^prop_", []) of
            nomatch ->
                "prop_" ++ FunName;
			_ ->
				FunName
		end,
    NewClause = transform_test_clause(Clause, ModuleName, NewFunName, true),
    {function, Line, list_to_atom(NewFunName), Arity, [NewClause]}.

transform_test_clause(Clause, ModuleName, FunName, UnmockBeforeAndAfterClause) ->
    % suite tests are unmocked after teardown phase
    transform_clause_before("BEGIN", Clause, ModuleName, FunName, UnmockBeforeAndAfterClause).

transform_clause_before(BeforeString, {clause, L, CArgs, Guards, Body}, ModuleName, FunName, UnmockBeforeClause) when is_list(ModuleName),
																							                                     is_list(FunName) ->
    FirstCall = {call,L,{remote,L,{atom,L,default_logger},{atom,L,log_once}},
		[{integer,L,4},{string,L, "~n======================================== " ++ BeforeString ++
							 " ~s:~s ========================================"},{cons,L,{string,L,ModuleName},
												  {cons,L,{string,L,FunName},
												  {nil,L}}},
		 {atom,L,false}]},
    UnmockCall = unmock_modules_clause(L, UnmockBeforeClause),
	NewBody = [FirstCall | UnmockCall ++ Body],
	{clause, L, CArgs, Guards, NewBody}.

unmock_modules_clause(_L, false) ->
    [];
unmock_modules_clause(L, true) ->
    [{call,L,{remote,L,{atom,L,test_utils},{atom,L,unmeck_modules}}, []}].