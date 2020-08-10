-module(ping_pong).

-export([start/0]).

start() ->
    Reportor = start_reportor(2),
    P1 = start_loop(10000000, Reportor),
    P2 = start_loop(10000000, Reportor),
    P1 ! {ping, P2}.

ping(0, Reportor) ->
    Reportor ! finish;
ping(N, Reportor) ->
    receive
        {ping, Sender} ->
            Sender ! {ping, self()},
            ping(N-1, Reportor)
    end.

loop(N, Reportor) ->
    io:format("N is:~p~n", [N]),
    ping(N, Reportor).

reportor_loop(StartedTs, 0) ->
    io:format("Takes: ~p us~n", [micro_second() - StartedTs]);
reportor_loop(StartedTs, N) ->
    receive
        finish ->
            reportor_loop(StartedTs, N - 1)
    end.

start_reportor(N) ->
    StartedTs = micro_second(),
    io:format("Started ts: ~p us~n", [StartedTs]),
    spawn(fun() -> reportor_loop(StartedTs, N) end).

micro_second() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.

start_loop(N, Reportor) ->
    spawn(fun() -> loop(N, Reportor) end).
