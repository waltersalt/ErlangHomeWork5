%%%-------------------------------------------------------------------
%% @doc homework5 public API
%% @end
%%%-------------------------------------------------------------------

-module(erlangHomeWork5_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    homework5_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
