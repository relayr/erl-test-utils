%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright relayr GmbH 2020
%%------------------------------------------------------------------------------
-module(test_progress_logger_tests).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

%% =============================================================================
%% Tests
%% =============================================================================
-ifdef(TEST).
-include("../include/testing.hrl").

%% =============================================================================
%% Tests
%% =============================================================================

start_default_test() ->
    ok = setup(),
    _ = test_progress_logger:start(),
    ok = assert_logger_handler_added(logger_std_h, #{
        level => all,
        formatter => {logger_formatter,
            #{
                template => [time," [",level,"] ",mfa, " ", pid, " ",msg,"\n"],
                single_line => false
            }
        }
    }).

start_with_another_log_level_test() ->
    ok = setup(),
    _ = test_progress_logger:start(#{
        handler_config => #{
            level => info
        }
    }),
    ok = assert_logger_handler_added(logger_std_h, #{
        level => info,
        formatter => {logger_formatter,
            #{
                template => [time," [",level,"] ",mfa, " ", pid, " ",msg,"\n"],
                single_line => false
            }
        }
    }).

start_with_custom_handler_and_formatter_test() ->
    ok = setup(),
    _ = test_progress_logger:start(#{
        handler_config => #{
            module => my_logger_handler,
            formatter => {my_logger_formatter, #{
                template => [time," ",level," ",msg,"\n"]
            }}
        }
    }),
    ok = assert_logger_handler_added(my_logger_handler, #{
        level => all,
        module => my_logger_handler,
        formatter => {my_logger_formatter, #{
            template => [time," ",level," ",msg,"\n"]
        }}
    }).

setup() ->
    ?MECK(logger, [
        {update_primary_config, ok},
        {remove_handler, ok},
        {add_handler, ok}
    ]).

assert_logger_handler_added(ExpectedModule, ExpectedConfig) ->
    ?assertCalledOnce(logger, add_handler, [default, ExpectedModule, ExpectedConfig]).

-endif.
