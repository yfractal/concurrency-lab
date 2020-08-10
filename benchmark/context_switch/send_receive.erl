-module(send_receive).

-export([start/0]).

start() ->
    Reportor = start_reportor(1),
    start_loop(10000000, Reportor).

send_then_receive(0, Reportor, _) ->
    Reportor ! finish;
send_then_receive(N, Reportor, Self) ->
    Self ! {ping, self()},
    receive
        {ping, _} ->
            send_then_receive(N-1, Reportor, Self)
    end.

loop(N, Reportor) ->
    io:format("N is:~p~n", [N]),
    send_then_receive(N, Reportor, self()).

reportor_loop(StartedTs, 0) ->
    io:format("Takes: ~p us~n", [micro_second() - StartedTs]);
reportor_loop(StartedTs, N) ->
    receive
        finish ->
            reportor_loop(StartedTs, N - 1)
    end.

start_reportor(N) ->
    StartedTs = micro_second(),
    spawn(fun() -> reportor_loop(StartedTs, N) end).


start_loop(N, Reportor) ->
    spawn(fun() -> loop(N, Reportor) end).

micro_second() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.
