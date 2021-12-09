%%%-------------------------------------------------------------------
%%% @author vladimirb
%%% @doc
%%% PRIVATE, use vmq instead
%%% mq mnesia backend (default)
%%% RIRO (random in, random out)
%%% DOESN'T provide message order (no FIFO/LIFO), but simple, persistent and almost fast
%%% Useful as 'pool reactor' (scraping jobs, etc)
%%% @end
%%%-------------------------------------------------------------------
-module(vmq_mnesia).

-author("vladimirb").

-behavior(vmq_backend_behavior).

-export([
    topic_new/1,
    topic_clean/1,
    topic_delete/1,
    topic_length/1,
    put/2,
    put_many/2,
    consume/1,
    consume_all/1
]).

-define('REC', vmq).

-record(vmq, {
    data :: vmq:data(),
    val :: ok
}).

%%%===================================================================
%%% PUBLIC
%%%===================================================================

-spec tab(vmq:topic()) -> atom().
tab(Topic) when is_binary(Topic) -> binary_to_atom(<<"_vmq_", Topic/binary>>);
tab(Topic) when is_atom(Topic) -> binary_to_atom(<<"_vmq_", (atom_to_binary(Topic))/binary>>).

-spec topic_new(vmq:topic()) -> ok.
%% @doc Creates new table
topic_new(Topic) ->
    Tab = tab(Topic),
    ok = vmnesia:validate_table_creation(
        vmnesia:create_tab_default(#{
            nodes => [node()],
            tab => Tab,
            record => ?REC,
            fields => record_info(fields, ?REC),
            type => set
        })
    ),
    %% ok = vmnesia:validate_table_creation(Tab),
    ok = mnesia:wait_for_tables([Tab], 60 * 1000).

-spec topic_clean(vmq:topic()) -> ok.
%% @doc Deletes all items
topic_clean(Topic) ->
    {atomic, ok} = mnesia:clear_table(tab(Topic)),
    ok.

-spec topic_delete(vmq:topic()) -> ok.
topic_delete(Topic) ->
    {atomic, ok} = mnesia:delete_table(tab(Topic)),
    ok.

-spec topic_length(vmq:topic()) -> non_neg_integer().
topic_length(Topic) ->
    {atomic, Len} = mnesia:transaction(fun() -> mnesia:table_info(tab(Topic), size) end),
    Len.

-spec put(vmq:topic(), vmq:data()) -> ok.
%% @doc Adds new item
put(Topic, Data) ->
    Tab = tab(Topic),
    El = #vmq{data = Data, val = ok},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Tab, El, sticky_write) end),
    ok.

-spec put_many(vmq:topic(), [vmq:data()]) -> ok.
%% @doc Adds new item
put_many(Topic, DataList) ->
    Tab = tab(Topic),
    {atomic, [ok | _]} = mnesia:transaction(
        fun() -> [
            mnesia:write(Tab, #vmq{data = Data, val = ok}, sticky_write)
            || Data <- DataList
        ]
        end
    ),
    ok.

-spec consume(vmq:topic()) -> vmq:data() | '$empty'.
consume(Topic) ->
    Tab = tab(Topic),
    {atomic, Data} = mnesia:transaction(fun() ->
        case mnesia:first(Tab) of
            '$end_of_table' ->
                '$empty';
            Data ->
                mnesia:delete(Tab, Data, sticky_write),
                Data
        end
    end),
    Data.

-spec consume_all(vmq:topic()) -> [vmq:data()].
consume_all(Topic) ->
    Tab = tab(Topic),
    {atomic, Recs} = mnesia:transaction(fun() -> mnesia:match_object(Tab, {vmq, '_', '_'}, read) end),
    topic_clean(Topic),
    [Data || #vmq{data = Data} <- Recs].
