-ifdef(TEST).

-ifndef(prop_testing_hrl).

-define(prop_testing_hrl, true).

-include_lib("proper/include/proper.hrl").

-compile({parse_transform, prop_test_parse_transform}).

-define(PROP_TEST_FUN(), -prop_test_function([])).

% QuickCheck generators
-define(EQC_STRING_GEN, non_empty(list(choose(32,126)))).   % printable ASCII characters
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

-endif.