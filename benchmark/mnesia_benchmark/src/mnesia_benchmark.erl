-module(mnesia_benchmark).

-export([create_table/0, insert_data/0, find/1]).

-record(kv_record, {key, val}).

create_table() ->
    mnesia:start(),
    mnesia:create_table(kv_record,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, kv_record)}]).

insert_data() ->
    Data = data_reader:read_data("data.txt"),
    do_insert_data(Data).

do_insert_data([]) ->
    ok;
do_insert_data([D|Data]) ->
    [Key,Val] = D,
    Record = #kv_record{key=Key, val= Val},
    F = fun() -> mnesia:write(Record) end,
    {atomic, ok} = mnesia:transaction(F),

    do_insert_data(Data).

find(Key) ->
    F = fun () -> mnesia:read(kv_record, Key) end,
    {atomic, [Item]} = mnesia:transaction(F),
    Item.
