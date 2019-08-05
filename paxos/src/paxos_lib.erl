-module(paxos_lib).

-export([shuffle/1]).

shuffle(L) ->
    [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].
