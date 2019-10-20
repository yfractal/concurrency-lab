-module(data_reader).

-export([read_data/1]).

read_lines(File) ->
    {ok, Device} = file:open(File, [read]),
    try read_all_lines(Device)
    after file:close(Device)
    end.

read_all_lines(Device) ->
    read_all_lines(Device, []).

read_all_lines(Device, Acc) ->
    case io:get_line(Device, "") of
        eof  -> lists:reverse(Acc);
        Line ->
            read_all_lines(Device, [Line|Acc])
    end.

read_data(File) ->
    Lines = read_lines(File),
    [string:tokens(string:strip(Line, right, $\n), " ") || Line <- Lines].
