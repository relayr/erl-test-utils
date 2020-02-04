%% Copyright 2020 relayr GmbH
%% @doc A wrapper to 'eunit_progress' that configures custom logger.
-module(test_progress_logger).
-behaviour(eunit_listener).

-export([
    start/0,
    start/1
]).

%% eunit_listener callbacks
-export([
    init/1,
    handle_begin/3,
    handle_end/3,
    handle_cancel/3,
    terminate/2
]).

-define(DEFAULT_HANDLER_MODULE, logger_std_h).
-define(HANDLER_NAME, default).
-define(EUNIT_PROGRESS_OPTIONS, [colored, profile]).

%% Startup
start() ->
    start([]).

start(Options) ->
    EunitProgressOptions = maps:get(eunit_progress, Options, ?EUNIT_PROGRESS_OPTIONS),
    HandlerConfig = maps:get(handler_config, Options, #{}),
    HandlerModule = maps:get(module, HandlerConfig, ?DEFAULT_HANDLER_MODULE),
    Level = maps:get(level, HandlerConfig, all),
    UpdatedHandlerConfig = maps:merge(#{
        level => Level,
        formatter => {logger_formatter,
            #{
                %% default log format is:
                %% 2020-02-04T09:36:22.115931+01:00 [info] test_module:call/3 <0.315.0> msg - call request
                template => [time," [",level,"] ",mfa, " ", pid, " ",msg,"\n"],
                single_line => false
            }
        }
    }, HandlerConfig),
    % update default logger configuration
    ok = logger:update_primary_config(#{level => Level}),
    ok = logger:remove_handler(?HANDLER_NAME),
    ok = logger:add_handler(?HANDLER_NAME, HandlerModule, UpdatedHandlerConfig),
    % start tracing unit test progress
    eunit_listener:start(?MODULE, EunitProgressOptions).

%%------------------------------------------
%% eunit_listener callbacks
%%------------------------------------------
init(Options) ->
    eunit_progress:init(Options).

handle_begin(Type = test, Data, St) ->
    {Module, Function, Arity} = proplists:get_value(source, Data),
    logger:info("~n~n~s BEGIN ~p:~p/~p ~s", [sep(), Module, Function, Arity, sep()]),
    eunit_progress:handle_begin(Type, Data, St);
handle_begin(Type, Data, St) ->
    eunit_progress:handle_begin(Type, Data, St).

handle_end(Type = test, Data, St) ->
    {Module, Function, Arity} = proplists:get_value(source, Data),
    logger:info("~n~s END ~p:~p/~p   ~s~n", [sep(), Module, Function, Arity, sep()]),
    eunit_progress:handle_end(Type, Data, St);
handle_end(Type, Data, St) ->
    eunit_progress:handle_end(Type, Data, St).

handle_cancel(Type, Data, St) ->
    eunit_progress:handle_cancel(Type, Data, St).

terminate(Result, St) ->
    % write buffer with logs to disk file (applicable only if default handler module 'logger_std_h' is used)
    ok = logger_h_common:filesync(?DEFAULT_HANDLER_MODULE, ?HANDLER_NAME),
    eunit_progress:terminate(Result, St).

%%------------------------------------------
%% local functions
%%------------------------------------------
sep() ->
    "================================".