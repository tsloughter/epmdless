-module(epmdless_app).

-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).


start() ->
    application:start(?MODULE).


start(_StartType, _StartArgs) ->
    epmdless_dist_sup:start_link().


stop(_State) ->
    ok.
