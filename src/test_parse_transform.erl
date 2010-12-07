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
    transform_tree(Tree, [], [], []).

transform_tree([{attribute, _, module, Name} = A | Rest], Tree, [], []) ->
    put(module_name, Name),
    transform_tree(Rest, [A | Tree], [], []);
transform_tree([{attribute, _, test_function, Elem} | Rest], Tree, Before, After) ->
    transform_tree(Rest, Tree, [Elem | Before], After);
transform_tree([F | Rest], Tree, [], []) ->
    transform_tree(Rest, [F | Tree], [], []);
transform_tree([{function, _, _, _, _} = F | Rest], Tree, Before, After) ->
    NewF = transform_function(F, lists:reverse(Before), lists:reverse(After)),
    transform_tree(Rest, [NewF | Tree], [], []);
transform_tree([Element | Rest], Tree, Before, After) ->
    transform_tree(Rest, [Element | Tree], Before, After);
transform_tree([], Tree, _, _) ->
    lists:reverse(Tree).

transform_function({function, Line, FunName, Arity, Clauses}, _Before, _After) ->
    put(function_name, FunName),
    put(function_arity, Arity),
	CurFunName = atom_to_list(FunName),
	% append '_test' to the function name if not already appended
	{IsATestGenerator, NewFunName} = 
		case re:run(CurFunName, "_test$", []) of
            nomatch ->
				case re:run(CurFunName, "_test_$", []) of
					nomatch ->
						{false, CurFunName ++ "_test"};
					_ ->
						% test generators shouldn't have function body changed
						{true, CurFunName}
				end;
			_ ->
				{false, CurFunName}
		end,
    NewClauses = 
		if IsATestGenerator ->
			Clauses;
		true ->
			transform_clause(Clauses, FunName, [])
		end,
    {function, Line, list_to_atom(NewFunName), Arity, NewClauses}.

transform_clause([OrgClause | Rest], FunName, Clauses) ->
    BeforeClause = transform_clause_before(OrgClause, FunName),
    AfterClause = transform_clause_after(BeforeClause, FunName),
    transform_clause(Rest, FunName, [AfterClause | Clauses]);
transform_clause([], _FunName, Clauses) ->
    lists:reverse(Clauses).

transform_clause_before({clause, L, CArgs, Guards, Body}, FunName) when is_atom(FunName) ->
	ModuleName = get(module_name),
    FirstCall = {call,L,{remote,L,{atom,L,log4erl},{atom,L,info}},
		[{string,L,"BEGIN ~p:~p"},{cons,L,{atom,L,ModuleName},{cons,L,{atom,L,FunName},{nil,L}}}]},
	NewBody = [FirstCall | Body],
	{clause, L, CArgs, Guards, NewBody}.

transform_clause_after({clause, L, CArgs, Guards, Body}, FunName) when is_atom(FunName) ->
	ModuleName = get(module_name),
    LastCall = {call,L,{remote,L,{atom,L,log4erl},{atom,L,info}},
		[{string,L,"END ~p:~p"},{cons,L,{atom,L,ModuleName},{cons,L,{atom,L,FunName},{nil,L}}}]},
	NewBody = Body ++ [LastCall],
	{clause, L, CArgs, Guards, NewBody}.