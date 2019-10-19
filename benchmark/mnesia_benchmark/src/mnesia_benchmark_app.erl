%%%-------------------------------------------------------------------
%% @doc mnesia_benchmark public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_benchmark_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([benchmark_mnesia/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    mnesia_benchmark_sup:start_link().

%% mnesia_benchmark_app:benchmark_mnesia().
benchmark_mnesia() ->
    mnesia_benchmark:create_table(),
    mnesia_benchmark:insert_data(),
    ProcessCount = 100,
    do_benchmark_mnesia(ProcessCount).

do_benchmark_mnesia(ProcessCount) ->
    Data = data_reader:read_data("data.txt"),
    Keys = [Key || [Key, Val] <- Data],

    KeysChunks = chunk_list(ProcessCount, Keys),
    KeysChunksCount = length(KeysChunks),
    Monitor = spawn(fun() -> benchmark_mnesia_monitor(KeysChunksCount) end),
    Works = [spawn(fun() -> benchmark_mnesia_worker(Keys, Monitor) end) || Keys <- KeysChunks],
    Monitor ! start,
    [Worker ! start || Worker <- Works].

benchmark_mnesia_monitor(StartedAt, 0) ->
    EndedAt = ts(),
    io:format("Takes ~p", [EndedAt - StartedAt]);

benchmark_mnesia_monitor(StartedAt, TotalCount) ->
    receive
        finish ->
            benchmark_mnesia_monitor(StartedAt, TotalCount - 1)
    end.

benchmark_mnesia_monitor(TotalCount) ->
    receive
        start ->
            io:format("monitor started"),
            benchmark_mnesia_monitor(ts(), TotalCount)
    end.

benchmark_mnesia_worker(Keys, Monitor) ->
    receive
        start ->
            [mnesia_benchmark:find(K) || K <- Keys],
            io:format("find finished ~n"),
            Monitor ! finish
    end.

chunk_list(ChunkCount, List) ->
    Len = length(List),
    EachChunkLength = round(Len / ChunkCount), %% suppose no reminder
    do_chunk_list(EachChunkLength, ChunkCount, List).

do_chunk_list(EachChunkLength, ChunkCount, List) ->
    do_chunk_list([], EachChunkLength, ChunkCount, List).

do_chunk_list(Acc, EachChunkLength, 0, List) ->
    Acc;
do_chunk_list(Acc, EachChunkLength, ChunkCount, List) ->
    {H, T} =  lists:split(EachChunkLength, List),
    NewAcc = lists:append(Acc, [H]),
    do_chunk_list(NewAcc, EachChunkLength, ChunkCount - 1, T).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
ts() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).
