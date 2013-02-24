%%  Copyright (C) 2012 - Molchanov Maxim

-module(mm_kernel).
-author('author Maxim Molchanov <elzor.job@gmail.com>').
-export([start/0, stop/0]).


%% @spec start() -> ok
%% @doc Start the webmachine server.
start() ->
    application:start(mm_kernel).

%% @spec stop() -> ok
%% @doc Stop the webmachine server.
stop() ->
    application:stop(mm_kernel).