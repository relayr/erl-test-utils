# test_utils

[![Build Status](https://img.shields.io/github/workflow/status/relayr/erl-test-utils/Erlang%20CI)](https://github.com/relayr/erl-test-utils/actions?query=workflow%3A%22Erlang+CI%22) [![Hex.pm](https://img.shields.io/hexpm/v/test_utils.svg?style=flat)](https://hex.pm/packages/test_utils) [![Coverage Status](https://coveralls.io/repos/github/relayr/erl-test-utils/badge.svg?branch=master)](https://coveralls.io/github/relayr/erl-test-utils?branch=master)

## Description

Erlang application that can be imported in your project to help in development of unit and property-based tests (using [PropEr](https://github.com/proper-testing/proper)).

To import `test_utils` you need to add it to your rebar.config file e.g.
```
{profiles, [
    {test, [
        {deps, [test_utils]}
    ]}
]}.
```
Next step is to include `testing.hrl` in your testing modules e.g.
```
-module(my_module_tests).

-include_lib("test_utils/include/testing.hrl").
...
```

## Unit tests helpers

#### ?LOOPER(Fun, Args, LoopTimeout, Count)
This macro is used to call function `Fun` with arity matching number of arguments in the `Args` list for `Count` times until all assertions in the function pass. There's a timeout of `LoopTimeout` milliseconds between consecutive calls of the function. If function fails for `Count` times then output from last call is returned to the test.
```
ExpectedState = #state{id = ?KEY, waiting = []},
?LOOPER(fun(Key) ->
        {ok, State} = server:get_state(Key),
        ?assertEqual(ExpectedState, State)
    end,
    [?KEY],
    50, 10
).
```

#### ?WAIT_FOR_PROCESS_STOPPED(NameOrPID)
Wait 1 second until process specified by its registered name or PID is stopped. If registered name is used this macro waits until process name is unregistered.
```
?WAIT_FOR_PROCESS_STOPPED(test_server).
```

In case process isn't stopped the following error is returned in the test:
```
  1) server_tests:process_stopped_test/0
     Failure/Error: unknown assert: "Process <0.341.0> wasn't stopped!"
```

#### ?assertContains(Elem, List)
Assert that element `Elem` exists in list of elements `List`.

#### ?assertContainsAll(Elems, List)
Assert that all of elements in list `Elems` exist in list of elements `List`.

#### ?assertNotContains(Elem, List)
Assert that element `Elem` doesn't exist in list of elements `List`.

#### ?assertJsonEquals(Expected, Actual)
Assert that JSON objects are equal. Both of them can be either encoded as a string or decoded to format compatible with [jsx](https://github.com/talentdeficit/jsx).

## Function mock helpers
Macros described in this section can be used to mock behavior of tested functions using [meck](https://github.com/eproxus/meck) library and assert if tested code was called with expected arguments.

#### ?MECK(Module, Funs)

Mock module `Module` with a list of functions in the following format:
```
Funs :: [
    {FunctionName :: atom(), Fun :: fun()} |
    {FunctionName :: atom(), FunResult :: any()}
]
```
If mocked function exists and `Fun` with specific arity is used for mocking the result then only function with such arity is mocked.

If function result is the Erlang term `FunResult` then all functions with matching name (no matter what arity) returns this Erlang term.
If function doesn't exist then function with arity 0 is created and returns function result.

```
-define(TS, 1563443613000).

?MECK(ts_utils, [
    {get_os_timestamp, ?TS},
    {get_os_timestamp, fun
        (secs) -> ?TS div 1000;
        (millis) ->?TS
    end},
    {convert_timestamp, fun(T) -> T * 1000 end}
]).

---

1> ts_utils:get_os_timestamp().
1563443613000
2> ts_utils:get_os_timestamp(millis).
1563443613000
3> ts_utils:get_os_timestamp(secs).
1563443613
4> ts_utils:convert_timestamp(1563443781).
1563443781000
```

#### ?MECK_LOOP(Module, Funs)
Mock module `Module` with a list of functions in the following format:
```
Funs :: [
    {FunctionName :: atom(), FunResults :: [any(), ...]}
]
```
Each mocked function result is specified as a list of values to be returned. Values in the list are returned sequentially in consecutive calls to mocked function. If end of the list is reached then values are returned from the beginning of the list again.

```
-define(TS, 1563443613000).

?MECK(ts_utils, [
    {get_os_timestamp, [?TS, ?TS + 1000, ?TS + 2000]}
]).

---

1> ts_utils:get_os_timestamp().
1563443613000
2> ts_utils:get_os_timestamp().
1563443614000
3> ts_utils:get_os_timestamp().
1563443615000
4> ts_utils:get_os_timestamp().
1563443613000
```

#### ?assertCalledOnce(Module, Function, Args)

Assert if mocked function was called with specific arguments exactly once. Matchers like `'_'` can be used for matching of the arguments.

```
?assertCalledOnce(ts_utils, get_os_timestamp, [millis])
```

If mocked function was not called or was called more than 1 time then the assertion error is returned and outputs history of calls to mocked function.

```
Failure/Error: ?assertThat("ts_utils:get_os_timestamp(millis) called 1 time(s)")
  expected: {ts_utils,get_os_timestamp,[millis]}
       got: [{ts_utils,get_os_timestamp,[secs]},
             {ts_utils,get_os_timestamp,[millis]},
             {ts_utils,get_os_timestamp,[millis]}]
```

#### ?assertNotCalled(Module, Function)
Assert if mocked function was not called at all.
```
?assertNotCalled(ts_utils, get_os_timestamp)
```

Assertion error is returned if there was a call made to function of any arity.
```
Failure/Error: ?assertThat("ts_utils:get_os_timestamp(...) called 0 time(s)")
  expected: not_called
       got: [{ts_utils,get_os_timestamp,[]},
             {ts_utils,get_os_timestamp,[secs]},
             {ts_utils,get_os_timestamp,[millis]}]
```

#### ?assertNotCalled(Module, Function, Args)

Assert if mocked function was not called at all with given arguments. Matchers like `'_'` can be used for matching of the arguments.

```
?assertNotCalled(ts_utils, get_os_timestamp, ['_'])
```

#### ?assertCalled(NumCalls, Module, Function, Args)

Assert if mocked function was called with specific arguments exactly some given number of times. Matchers like `'_'` can be used for matching of the arguments.

```
?assertCalled(3, ts_utils, get_os_timestamp, [millis])
```

#### ?MECK_RESET(Module)

Reset all counters for calls to mocked functions. Counters are used by macros like `?assertCalled` and `?assertNotCalled` for checking function calls.

#### ?UNMECK(Module)

Reset mocks to given modules. If mocks were overwriting existing functions then the default function results are returned back again.

## Property based tests generators

There's a set of helpful generators defined in `prop_testing.hrl` that can be used in your property based tests.

#### ?EQC_STRING_GEN
Generate random string with printable ASCII characters (ASCII codes 32 - 126)
```
"s"
"_"
"n"
"P"
...
"#1[<v8+.O9MQw"
"N<E?s=658Vt>@$&zV@\"-jY@#X7xtS-8NRR>@lvZWl"
"w1Sf <LQ\\2y_**U.:\")SYhuj4d"
"`\"al/s!muWWZ{L0`mBaX_y[C(j{~"
```

#### ?EQC_ATOM_GEN
Generate random atom with printable ASCII characters (ASCII codes 32 - 126)

```
'}'
'\\'
w
'4'
...
'C&XAUV[a2pZ"y{`7?'
'FBnuB H3Xr`(d3Jq/FQl3{dXyIxS]515n'
'@<$@Fu*kyPaI].gXL1P9}vmUTDL"hw'
')03jvN"OD8OEaWnoV!/v`5\'jZ/g~6i`AKt/a'
```
#### ?EQC_BYTE_GEN
Generate random unsigned byte value (values 0..255)
```
141
99
169
180
...
242
16
158
150
```

#### ?EQC_USHORT_GEN
Generate random unsigned short value (values 0..65535)

```
17211
20042
23461
40286
...
8457
37070
28076
31370
```
#### ?EQC_ULONG_GEN
Generate random unsigned long value (values 0..18446744073709551615)
```
0
0
3
2
...
75
68
25
13
```
#### ?EQC_BIN_GEN(MaxSize)
Generate random binary of size `0..MaxSize`
```
<<94,30,5,155>>
<<58,23>>
<<67,128,216,104,20,73,91>>
<<77,189,139,35>>
...
<<94,200,83,119,36,130>>
<<230,124,131,78,0,145,114,34,222,196>>
<<254,116,230,135,197,27>>
```
#### ?EQC_UNIQUE_LIST_GEN(Gen)
Generate list of values generated by `Gen` generator that contain unique values

```
[]
[1]
[1,0]
[5]
[4,1]
...
[4,5,1,3,0,2]
[5,2,4,1]
[4,0,3,1,5,2]
[2,0,5,1,4,3]
```
#### ?EQC_SORTED_UNIQUE_LIST_GEN(Gen)
Generate list of values generated by `Gen` generator that contain unique and sorted values
```
[5]
[]
[2,5]
[4,5]
[0,3]
...
[1,2,3,4,5]
[0,1,2,3,5]
[0,1,2,3,4,5]
[0,1,2,3,4,5]
```

## Unit test progress logging
When you've added `test_utils` as a dependency to your project you can override default [eunit_progress.erl](https://github.com/seancribbs/eunit_formatters/blob/master/src/eunit_progress.erl) reporting module that is used by `rebar3 eunit`. To do this you need to add the following configuration to your `rebar.config` file:
```
{eunit_opts, [
    {report, {test_progress_logger, #{
        handler_config => #{config => #{file => "test.eunit"}}
    }}}
]}.
```
After running unit tests you'll end up with `test.eunit` file that contains application and test logs.

The following options for `test_progress_logger` are supported:
* __eunit_progress__ - options passed to default reporting module that is used in the backend, default: `[colored, profile]`
* __handler_config__ - configuration for [OTP logger handler](http://erlang.org/doc/man/logger.html#type-handler_config) that overwrites the default logger (you can specify log level or even your own logger handler or formatter) e.g.
```
{eunit_opts, [
  {report, {test_progress_logger, #{
    handler_config => #{
      level => info,
      config => #{type => standard_io},
      module => my_logger_handler,
      formatter => {my_logger_formatter, #{
        template => [time," ",level," ",msg,"\n"],
        single_line => false
      }}
    }
  }}}
]}.
```
