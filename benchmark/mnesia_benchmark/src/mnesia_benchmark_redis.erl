-module(mnesia_benchmark_redis).

-export([start_client/0, insert_data/0, find/2, set/3]).

-record(kv_record, {key, val}).

start_client() ->
    eredis:start_link("127.0.0.1", 6379).

insert_data() ->
    Data = data_reader:read_data("data.txt"),
    {ok, Client} = start_client(),

    do_insert_data(Data, Client).

do_insert_data([], _) ->
    ok;
do_insert_data([D|Data], Client) ->
    [Key,Val] = D,
    set(Client, Key, Val),
    do_insert_data(Data, Client).

%% {ok, C} = mnesia_benchmark_redis:start_client().
%% mnesia_benchmark_redis:set(C, 1, 1024).
set(Client, Key, Val) ->
    eredis:q(Client, ["SET", Key, Val]).

%% mnesia_benchmark_redis:find(C, 1).
find(Client, Key) ->
    eredis:q(Client, ["GET", Key]).
