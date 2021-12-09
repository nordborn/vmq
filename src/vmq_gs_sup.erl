%%%-------------------------------------------------------------------
%%% @author vladimirb
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(vmq_gs_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1000,
        period => 1
    },
    ChildSpec = [
        #{
            id => vmq_qs_sup,
            start => {vmq_gs, start_link, []},
            restart => permanent
        }
    ],

    {ok, {SupFlags, ChildSpec}}.
