%%%-------------------------------------------------------------------
%%% @author vladimirb
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% MQ adapter, API module
%%% @end
%%%-------------------------------------------------------------------
-module(vmq).

%% Topic API
-export([
    topic_new/1,
    topic_clean/1,
    topic_delete/1,
    topic_length/1
]).

%% Item API
-export([
    put/2,
    put_many/2,
    consume/1,
    consume_all/1
]).

%% Backend API
-export([
    backend_module/0,
    backend_module/1,
    backend_module_set_default/0
]).

-export_type([
    topic/0,
    data/0
]).

-on_load(backend_module_set_default/0).

-type topic() :: atom() | binary().
-type data() :: any() | '_'.

%%% TOPIC %%%

-spec topic_new(topic()) -> ok.
%% @doc Creates new topic
topic_new(Topic) ->
    apply(backend_module(), ?FUNCTION_NAME, [Topic]),
    ok.

-spec topic_clean(topic()) -> ok.
%% @doc Deletes all items from the topic
topic_clean(Topic) ->
    apply(backend_module(), ?FUNCTION_NAME, [Topic]).

-spec topic_delete(topic()) -> ok.
%% @doc Deletes topic (table)
topic_delete(Topic) ->
    apply(backend_module(), ?FUNCTION_NAME, [Topic]).

-spec topic_length(topic()) -> non_neg_integer().
%% @doc Number of items in the topic
topic_length(Topic) ->
    apply(backend_module(), ?FUNCTION_NAME, [Topic]).

%%% ITEM %%%

-spec put(topic(), data()) -> ok.
%% @doc Adds new item
put(Topic, Data) ->
    apply(backend_module(), ?FUNCTION_NAME, [Topic, Data]).

-spec put_many(topic(), [data()]) -> ok.
%% @doc Adds many items
put_many(Topic, DataList) ->
    apply(backend_module(), ?FUNCTION_NAME, [Topic, DataList]).

-spec consume(topic()) -> data() | '$empty'.
%% @doc Consumes (get + delete from the topic) one item
consume(Topic) ->
    apply(backend_module(), ?FUNCTION_NAME, [Topic]).

-spec consume_all(topic()) -> [data()].
%% @doc Consumes (get + delete from the topic) all items
consume_all(Topic) ->
    apply(backend_module(), ?FUNCTION_NAME, [Topic]).

%%% BACKEND %%%

-spec backend_module_set_default() -> ok.
%% @doc Sets default backend module vmq_mnesia
backend_module_set_default() ->
    backend_module(vmq_mnesia).

-spec backend_module(atom()) -> ok.
%% @doc Set any module as the backend. See vmq_backend_behavior
backend_module(Module) ->
    persistent_term:put('__vmq_backend', Module).

-spec backend_module() -> atom().
%% @doc Get backend module
backend_module() ->
    persistent_term:get('__vmq_backend').
