%%%-------------------------------------------------------------------
%% @doc parfait public API
%% @end
%%%-------------------------------------------------------------------

-module(parfait_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    parfait_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
