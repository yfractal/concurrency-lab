-module(paxos_event_recorder).

-behaviour(gen_event).

%% API
-export([start_link/0,
         add_handler/0,
         start_proposal/1,
         proposal_updated/1,
         acceptor_accepted_proposal/1,
         show_acceptor_event/0,
         save_proposals_to_file/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {proposal_count, proposal_map,
                acceptor_proposal_map}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

start_proposal(ProposalCount) ->
    gen_event:notify(?SERVER, {start_proposal, ProposalCount}).

proposal_updated({ProposalId, Proposal, SeqNum}) ->
    gen_event:notify(?SERVER, {proposal_updated, ProposalId, Proposal, SeqNum}).

acceptor_accepted_proposal({AcceptorId, Proposal, SeqNum})->
    gen_event:notify(?SERVER, {acceptor_accepted_proposal, AcceptorId, Proposal, SeqNum}).

save_proposals_to_file() ->
    gen_event:notify(?SERVER, {save_proposals_to_file}).

show_acceptor_event() ->
    gen_event:notify(?SERVER, {show_acceptor_event}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
    {ok, #state{proposal_map=maps:new()}}.

handle_event({show_acceptor_event}, State) ->
    {ok, State};
handle_event({save_proposals_to_file}, State) ->
    file:write_file("./foo", io_lib:fwrite("~p.\n", [State])),
    {ok, State};
handle_event({start_proposal, ProposalCount}, State) ->
    {ok, State#state{proposal_count=ProposalCount}};
handle_event({proposal_updated, ProposalID, Proposal, SeqNum},
             #state{proposal_map=ProposalMap}= State) ->
    Proposals = maps:get(ProposalID, ProposalMap, []),
    NewProposals = lists:append(Proposals, [{Proposal, SeqNum}]),
    NewProposalDict = maps:put(ProposalID, NewProposals, ProposalMap),
    NewState = State#state{proposal_map=NewProposalDict},
    {ok, NewState};
handle_event({acceptor_accepted_proposal, AcceptorID, Proposal, SeqNum},
             #state{acceptor_proposal_map=Map}= State) ->
    Proposals = maps:get(AcceptorID, Map, []),
    NewProposals = lists:append(Proposals, [{Proposal, SeqNum}]),
    NewProposalDict = maps:put(AcceptorID, NewProposals, Map),
    NewState = State#state{acceptor_proposal_map=NewProposalDict},
    {ok, NewState};

handle_event(_Event, State) ->
    io:formt("debug Event=~p~n", [_Event]),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
