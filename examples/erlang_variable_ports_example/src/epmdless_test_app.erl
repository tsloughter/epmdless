%%%-------------------------------------------------------------------
%% @doc epmdless_test public API
%% @end
%%%-------------------------------------------------------------------

-module(epmdless_test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    epmdless_test_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
