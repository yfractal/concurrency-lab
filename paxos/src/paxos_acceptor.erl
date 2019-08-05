-module(paxos_acceptor).

-behaviour(gen_server).

%% API
-export([start_link/1,
         prepare/3,
         accept/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {id, highest_seq_num, accepted_proposal}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

prepare(Pid, From, SeqNum) ->
    gen_server:cast(Pid, {prepare, From, SeqNum}).

accept(Pid, From, {SeqNum, NewProposal}) ->
    gen_server:cast(Pid, {accept, From, {SeqNum, NewProposal}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Id]) ->
    process_flag(trap_exit, true),
    {ok, #state{id=Id, highest_seq_num=-1}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({prepare, From, SeqNum}, State) ->
    State2 = do_prepare(From, SeqNum, State),
    {noreply, State2};

handle_cast({accept, From, {SeqNum, NewProposal}}, State) ->
    State2 = do_accept(From, {SeqNum, NewProposal}, State),
    {noreply, State2};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_prepare(From, SeqNum,
           #state{highest_seq_num=HighestSeqNum, accepted_proposal=Proposal, id=Id} = State)
  when SeqNum > HighestSeqNum ->
    paxos_proposer:prepare(From, {SeqNum, Proposal, Id}),
    State#state{highest_seq_num=SeqNum};

do_prepare(_, SeqNum, State) ->
    State.

do_accept(From, {SeqNum, NewProposal},
          #state{id=Id, highest_seq_num=HighestSeqNum} = State)
  when SeqNum >= HighestSeqNum ->
    paxos_proposer:proposal_accepted(From, {SeqNum, NewProposal}),

    State#state{highest_seq_num=SeqNum, accepted_proposal=NewProposal};
do_accept(_, {SeqNum, NewProposal},
          #state{id=Id, highest_seq_num=HighestSeqNum} = State) ->
    State.
