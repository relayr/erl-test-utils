-ifdef(TEST).

-ifndef(testing_hrl).

-define(testing_hrl, true).

-include("prop_testing.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile({parse_transform, test_parse_transform}).

-define(TEST_FUN(), -test_function([])).
-define(TEST_CASE(FunName), {atom_to_list(FunName), fun FunName/0}).
-define(LOOPER(Fun, Args, LoopTimeout, Count), 
        test_utils:state_sleep_looper(Fun, Args, LoopTimeout, Count)).
-define(WAIT_FOR_PROCESS_STOPPED(ProcessRegisteredNameOrID),
        test_utils:wait_for_process_stopped(ProcessRegisteredNameOrID)).
-define(MECK(Module, Funs), test_utils:meck_module(Module, Funs)).
-define(MECK_LOOP(Module, Funs), test_utils:meck_loop_module(Module, Funs)).
-define(MECK_RESET(Module), meck:reset(Module)).
-define(MECK_AND_RESET(Module, Funs), ?MECK(Module, Funs), meck:reset(Module)).
-define(UNMECK, test_utils:unmeck_modules()).
-define(UNMECK(Module), test_utils:unmeck_module(Module)).
-define(SORT(List), lists:sort(List)).

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
                        {expected, L -- [E]},
                        {value, L}]})
            end
        end
)(Element, List))end).


-define(SORT_PROPERTIES_IN_JSON(U), begin((
    %For me is tricky - i leaved commented lines for better understanding and code review.
        fun
             SSort([FirstTuple | RestOfTuples]) when is_tuple(FirstTuple) ->
                 %?debugFmt("3 List of many tuples: ~p", [[FirstTuple, RestOfTuples]]),
                 R = ?SORT(lists:merge([SSort(FirstTuple)], SSort(RestOfTuples))),
                 %?debugFmt("3 R: ~p", [R]),
                 R;

             SSort(ArrayOfElements) when is_list(ArrayOfElements) ->
                 %?debugFmt("4 List of many elements: ~p", [ArrayOfElements]),
                 R = [SSort(E) || E <- ArrayOfElements],
                 %?debugFmt("4 R: ~p", [R]),
                 R;

             SSort({AttrName, AttrValue}) when is_atom(AttrName) ->
                 SSort({atom_to_binary(AttrName, utf8), AttrValue});

             SSort({AttrName, AttrValue}) when is_integer(AttrName) ->
                 SSort({integer_to_binary(AttrName), AttrValue});

             SSort({AttrName, AttrValue}) ->
                 %?debugFmt("5 Tuple: ~p", [{AttrName, AttrValue}]),
                 R = {AttrName, SSort(AttrValue)},
                 %?debugFmt("5 R: ~p", [R]),
                 R;

             SSort(Primitive) ->
                 %?debugFmt("Primitive: ~p", [Primitive]),
                 Primitive
         end
))(U)end).

-define(assertJson(Expected, Actual), begin ((
    fun(E, A) ->
        StripWhitespaces = fun
              (Binary) when is_binary(Binary) ->
                  jsx:encode(?SORT_PROPERTIES_IN_JSON(jsx:decode(Binary)));
              (JSX) ->
                  jsx:encode(?SORT_PROPERTIES_IN_JSON(JSX))
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

-define(assertCalledOnce(Module, Function, Args), ?assertCalled(1, Module, Function, Args)).

-define(assertNotCalled(Module, Function), ?assertNotCalled(Module, Function, '_')).
-define(assertNotCalled(Module, Function, Args), ?assertCalled(0, Module, Function, Args)).

-define(assertCalled(NumCalls, ModuleName, FunctionName, ArgsList),
    (fun(Module, Function, Args) ->
        try ?assertEqual(NumCalls, meck:num_calls(Module, Function, Args)) of
            _ ->
                ok
        catch
            error:_ ->
                History = lists:filtermap(fun(Tuple) ->
                    {M, F, _A} = MFA = erlang:element(2, Tuple),
                    if M =:= Module andalso F =:= Function->
                        {true, MFA};
                    true ->
                        false
                    end
                end, meck:history(Module)),
                Expected =
                    case NumCalls of
                        0 ->
                            not_called;
                        _ ->
                            {Module, Function, Args}
                    end,
                ArgsStr =
                    if Args =:= '_' ->
                        "...";
                    true ->
                        string:join([io_lib:format("~p", [Arg]) || Arg <- Args], ",")
                    end,
                erlang:error({assert, [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {matcher, lists:flatten(io_lib:format(
                        "~s:~s(~s) called ~p time(s)", [Module, Function, ArgsStr, NumCalls])
                    )},
                    {expected, Expected},
                    {actual, History}
                ]})
        end
    end)(ModuleName, FunctionName, ArgsList)
).

-endif.

-endif.
