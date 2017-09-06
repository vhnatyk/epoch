%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Service holding the chain of block headers and blocks.
%%% @TODO Persistence.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_chain).

-behaviour(gen_server).

%% API
-export([start_link/1,
         stop/0]).
-export([top_header/0,
         top_block/0,
         get_header_by_hash/1,
         get_block_by_hash/1,
         get_header_by_height/1,
         get_block_by_height/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("common.hrl").
-include("blocks.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_CALL_TIMEOUT, infinity). %% For synchronous persistence.

-record(state, {top_header :: header(), %% TODO Both in-memory and persisted database.
                top_block :: block(), %% TODO Without state trees. TODO In-memory only? Identified at startup? Delayed startup?
                headers :: dict:dict(block_header_hash(), header()), %% TODO Persisted database.
                blocks :: dict:dict(block_header_hash(), block()) %% TODO Persisted database. %% TODO Without state trees.
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(GenesisBlock) ->
    Args = [GenesisBlock],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:stop(?SERVER).

%% Returns the highest block header in the chain.
top_header() ->
    gen_server:call(?SERVER, {top_header}, ?DEFAULT_CALL_TIMEOUT).

%% Returns the highest known block in the chain.
top_block() ->
    gen_server:call(?SERVER, {top_block}, ?DEFAULT_CALL_TIMEOUT).

get_header_by_hash(HeaderHash) ->
    gen_server:call(?SERVER, {get_header_by_hash, HeaderHash},
                    ?DEFAULT_CALL_TIMEOUT).

get_block_by_hash(HeaderHash) ->
    gen_server:call(?SERVER, {get_block_by_hash, HeaderHash},
                    ?DEFAULT_CALL_TIMEOUT).

get_header_by_height(Height) ->
    gen_server:call(?SERVER, {get_header_by_height, Height},
                    ?DEFAULT_CALL_TIMEOUT).

get_block_by_height(Height) ->
    gen_server:call(?SERVER, {get_block_by_height, Height},
                    ?DEFAULT_CALL_TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args = [GenesisBlock]) ->
    process_flag(trap_exit, true),

    %% Hardcode expectations on specified genesis block.
    0 = aec_blocks:height(GenesisBlock),
    <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>> = aec_blocks:prev_hash(GenesisBlock),

    GenesisBlockHeader = aec_blocks:to_header(GenesisBlock),
    {ok, GenesisBlockHeaderHash} =
        aec_headers:hash_internal_representation(GenesisBlockHeader),

    %% TODO Remove state trees.
    State = #state{top_header = aec_blocks:to_header(GenesisBlock),
                   top_block = GenesisBlock,
                   headers = dict:store(
                               GenesisBlockHeaderHash, GenesisBlockHeader,
                               dict:new()),
                   blocks = dict:store(
                              GenesisBlockHeaderHash, GenesisBlock,
                              dict:new())
                  },
    {ok, State}.

handle_call({top_header}, _From, State) ->
    Reply = {ok, State#state.top_header},
    {reply, Reply, State};
handle_call({top_block}, _From, State) ->
    Reply = {ok, State#state.top_block},
    {reply, Reply, State};
handle_call({get_header_by_hash,
             HeaderHash = <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>>},
            _From, State) ->
    Reply = do_get_header_by_hash(HeaderHash, State#state.headers),
    {reply, Reply, State};
handle_call({get_block_by_hash,
             HeaderHash = <<_:?BLOCK_HEADER_HASH_BYTES/unit:8>>},
            _From, State) ->
    Reply = do_get_block_by_hash(HeaderHash, State#state.blocks),
    {reply, Reply, State};
handle_call({get_header_by_height, Height}, _From, State)
  when is_integer(Height), Height >= 0 ->
    Reply =
        do_get_header_by_height(Height,
                                State#state.top_header, State#state.headers),
    {reply, Reply, State};
handle_call({get_block_by_height, Height}, _From, State)
  when is_integer(Height), Height >= 0 ->
    Reply =
        do_get_block_by_height(Height,
                               State#state.top_header, State#state.headers,
                               State#state.blocks),
    {reply, Reply, State};
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Ignoring unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    %% TODO Close databases.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_get_header_by_hash(HeaderHash, Headers) ->
    case dict:find(HeaderHash, Headers) of
        {ok, Header} ->
            {ok, Header};
        error ->
            {error, header_not_found}
    end.

do_get_block_by_hash(HeaderHash, Blocks) ->
    case dict:find(HeaderHash, Blocks) of
        {ok, Block} ->
            {ok, Block};
        error ->
            {error, block_not_found}
    end.

do_get_header_by_height(Height, TopHeader, Headers) ->
    ChainHeight = aec_headers:height(TopHeader),
    if
        Height > ChainHeight ->
            {error, {chain_too_short, {{chain_height, ChainHeight},
                                       {top_header, TopHeader}
                                      }
                    }};
        Height =:= ChainHeight ->
            {ok, TopHeader};
        Height < ChainHeight ->
            do_get_past_header(ChainHeight - Height, TopHeader, Headers)
    end.

do_get_past_header(Distance, CurrentHeader, Headers)
  when is_integer(Distance), Distance > 1 ->
    PreviousHeaderHash = aec_headers:prev_hash(CurrentHeader),
    PreviousHeader = dict:fetch( %% If not found, database is corrupt: fail.
                       PreviousHeaderHash, Headers),
    case Distance of
        1 ->
            {ok, PreviousHeader};
        _ ->
            do_get_past_header(Distance - 1, PreviousHeader, Headers)
    end.

do_get_block_by_height(Height, TopHeader, Headers, Blocks) ->
    case do_get_header_by_height(Height, TopHeader, Headers) of
        {error, {chain_too_short, _}} = Err ->
            Err;
        {ok, Header} ->
            {ok, HeaderHash} = aec_headers:hash_internal_representation(Header),
            case dict:find(HeaderHash, Blocks) of
                {ok, Block} ->
                    {ok, Block};
                error ->
                    {error, {block_not_found, {top_header, TopHeader}}}
            end
    end.
