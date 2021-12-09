%%%-------------------------------------------------------------------
%%% @author vladimirb
%%% @doc
%%% PRIVATE, use vmq instead
%%% in-memory queue backend, LIFO
%%% To use this backend, call
%%%   vmq:backend_module(vmq_gs).
%%% @end
%%%-------------------------------------------------------------------
-module(vmq_gs).

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

-export([
    start_link/0,
    init/1,
    handle_call/3,
    terminate/2
]).

%% STUBS
-export([
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-type state() :: #{
    vmq:topic() => [vmq:data()]
}.

-define(DUMPFILE, "vmq_gs.dat").

%%%===================================================================
%%% GEN SERVER START
%%%===================================================================


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, [{timeout, 60 * 1000}]).

-spec init({}) -> {ok, state()}.
init({}) ->
    process_flag(trap_exit, true),
    State =
        try
            logger:info("~p~n", [{vmq_qs, restoring}]),
            {ok, Bin} = file:read_file(?DUMPFILE),
            binary_to_term(Bin)
        catch
            error:Reason ->
                logger:error("~p~n", [{vmq_qs, cant_restore, Reason}]),
                #{}
        end,
    {ok, State}.


%%%===================================================================
%%% PUBLIC API
%%%===================================================================

-spec tab(vmq:topic()) -> atom().
tab(Topic) when is_binary(Topic) -> binary_to_atom(<<"_vmq_", Topic/binary>>);
tab(Topic) when is_atom(Topic) -> binary_to_atom(<<"_vmq_", (atom_to_binary(Topic))/binary>>).

-spec topic_new(vmq:topic()) -> ok.
%% @doc Creates new table
topic_new(Topic) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, tab(Topic)}).

-spec topic_clean(vmq:topic()) -> ok.
%% @doc Deletes all items
topic_clean(Topic) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, tab(Topic)}).

-spec topic_delete(vmq:topic()) -> ok.
topic_delete(Topic) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, tab(Topic)}).

-spec topic_length(vmq:topic()) -> non_neg_integer().
topic_length(Topic) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, tab(Topic)}).

-spec put(vmq:topic(), vmq:data()) -> ok.
%% @doc Adds new item
put(Topic, Data) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, tab(Topic), Data}).

-spec put_many(vmq:topic(), [vmq:data()]) -> ok.
%% @doc Adds new items
put_many(Topic, DataList) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, tab(Topic), DataList}).

-spec consume(vmq:topic()) -> vmq:data() | '$empty'.
consume(Topic) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, tab(Topic)}).

-spec consume_all(vmq:topic()) -> [vmq:data()].
consume_all(Topic) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, tab(Topic)}).


%%%===================================================================
%%% GEN SERVER CB
%%%===================================================================

handle_call({topic_new, Topic}, _From, State) ->
    {reply, ok, State#{Topic => []}};
handle_call({topic_clean, Topic}, _From, State) ->
    {reply, ok, State#{Topic := []}};
handle_call({topic_delete, Topic}, _From, State) ->
    {reply, ok, maps:remove(Topic, State)};
handle_call({topic_length, Topic}, _From, State) ->
    Q = maps:get(Topic, State),
    {reply, length(Q), State};
handle_call({put, Topic, Data}, _From, State) ->
    Q = maps:get(Topic, State),
    {reply, ok, State#{Topic := [Data | Q]}};
handle_call({put_many, Topic, DataList}, _From, State) ->
    Q = maps:get(Topic, State),
    {reply, ok, State#{Topic := DataList ++ Q}};
handle_call({consume, Topic}, _From, State) ->
    Q = maps:get(Topic, State),
    case Q of
        [] ->
            {reply, '$empty', State#{Topic := []}};
        [H | T] ->
            {reply, H, State#{Topic := T}}
    end;
handle_call({consume_all, Topic}, _From, State) ->
    Q = maps:get(Topic, State),
    {reply, Q, State#{Topic := []}}.

terminate(_Reason, State) ->
    logger:warning("~p~n", [{vmq, saving_on_termination}]),
    file:write_file(?DUMPFILE, term_to_binary(State)),
    logger:info("~p~n", [{vmq, saved_on_termination}]).


%%%===================================================================
%%% STUBS
%%%===================================================================

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% PRIV
%%%===================================================================


%% for utf-8 data term_to_binary/binary_to_term
%%write_terms(Filename, List) when is_list(List) ->
%%    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
%%    Text = unicode:characters_to_binary(lists:map(Format, List)),
%%    file:write_file(Filename, Text).

