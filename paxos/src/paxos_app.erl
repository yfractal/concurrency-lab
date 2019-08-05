%%%-------------------------------------------------------------------
%% @doc paxos public API
%% @end
%%%-------------------------------------------------------------------

-module(paxos_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    AcceptorNum = 20,
    ProposalNum = 10,
    AcceptorPids =
        lists:map(fun(I) -> {ok, Pid} = paxos_acceptor:start_link(I), Pid end,
                  paxos_lib:shuffle(lists:seq(1, AcceptorNum))),

    _ProposalPids =
        lists:map(fun(I) -> {ok, Pid} = paxos_proposer:start_link(I, AcceptorPids), Pid end,
                  paxos_lib:shuffle(lists:seq(1, ProposalNum))),
    paxos_event_recorder:start_link(),
    paxos_event_recorder:add_handler(),
    paxos_event_recorder:start_proposal(ProposalNum),
    paxos_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
