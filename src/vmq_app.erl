-module(vmq_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:ensure_all_started(mnesia),
    vmnesia:bootstrap(),
    vmq_gs_sup:start_link().

stop(_State) ->
    ok.
