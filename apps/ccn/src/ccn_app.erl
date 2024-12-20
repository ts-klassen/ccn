%%%-------------------------------------------------------------------
%% @doc ccn public API
%% @end
%%%-------------------------------------------------------------------

-module(ccn_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ccn_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
