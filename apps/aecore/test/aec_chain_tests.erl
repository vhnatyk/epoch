-module(aec_chain_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("blocks.hrl").

fake_genesis_block() ->
    #block{height = 0, prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>}.

genesis_test_() ->
    {setup,
     fun() -> {ok, Pid} = aec_chain:start_link(fake_genesis_block()), Pid end,
     fun(_ChainPid) -> ok end,
     fun() ->
             GB = fake_genesis_block(),
             ?assertEqual({ok, GB}, aec_chain:top_block()),
             GH = aec_blocks:to_header(GB),
             ?assertEqual({ok, GH}, aec_chain:top_header()),

             ?assertEqual({ok, GH}, aec_chain:get_header_by_height(0)),
             ?assertEqual({ok, GB}, aec_chain:get_block_by_height(0)),

             {ok, GHH} = aec_blocks:hash_internal_representation(GB),
             ?assertEqual({ok, GH}, aec_chain:get_header_by_hash(GHH)),
             ?assertEqual({ok, GB}, aec_chain:get_block_by_hash(GHH))
     end}.

header_chain_test_() ->
    {setup,
     fun() -> {ok, Pid} = aec_chain:start_link(fake_genesis_block()), Pid end,
     fun(_ChainPid) -> ok end,
     fun() ->
             %% Check chain is at genesis.
             B0 = fake_genesis_block(),
             BH0 = aec_blocks:to_header(B0),
             ?assertEqual({ok, BH0}, aec_chain:top_header()),

             %% Add headers - not blocks - to chain.
             {ok, B0H} = aec_blocks:hash_internal_representation(B0),
             BH1 = aec_blocks:to_header(#block{height = 1, prev_hash = B0H}),
             %% TODO Put BH1.
             {ok, B1H} = aec_headers:hash_internal_representation(BH1),
             BH2 = aec_blocks:to_header(#block{height = 2, prev_hash = B1H}),
             %% TODO Put BH2.

             %% Check highest header.
             ?assertEqual({ok, BH2}, aec_chain:top_header()),
             %% Check heighest known block - still genesis.
             ?assertEqual({ok, B0}, aec_chain:top_block()),

             %% Check by hash.
             ?assertEqual({ok, BH0}, aec_chain:get_header_by_hash(B0H)),
             ?assertEqual({ok, B0}, aec_chain:get_block_by_hash(B0H)),
             ?assertEqual({ok, BH1}, aec_chain:get_header_by_hash(B1H)),
             ?assertEqual({error, block_not_found},
                          aec_chain:get_block_by_hash(B1H)),
             {ok, B2H} = aec_headers:hash_internal_representation(BH2),
             ?assertEqual({ok, BH2}, aec_chain:get_header_by_hash(B2H)),
             ?assertEqual({error, block_not_found},
                          aec_chain:get_block_by_hash(B2H)),

             %% Check by height.
             ?assertEqual({ok, BH0}, aec_chain:get_header_by_height(0)),
             ?assertEqual({ok, B0}, aec_chain:get_block_by_height(0)),
             ?assertEqual({ok, BH1}, aec_chain:get_header_by_height(1)),
             ?assertEqual({error, {block_not_found, {top_header, BH2}
                                  }}, aec_chain:get_block_by_height(1)),
             ?assertEqual({ok, BH2}, aec_chain:get_header_by_height(2)),
             ?assertEqual({error, {block_not_found, {top_header, BH2}
                                  }}, aec_chain:get_block_by_height(2)),
             ?assertEqual({error, {chain_too_short, {{chain_height, 2},
                                                     {top_header, BH2}}
                                  }}, aec_chain:get_header_by_height(3)),
             ?assertEqual({error, {chain_too_short, {{chain_height, 2},
                                                     {top_header, BH2}}
                                  }}, aec_chain:get_block_by_height(3))
     end}.

%% block_chain_test() ->

%% smoke_test() ->
%%     aec_chain:top
%%     save_headers_by_hash
%% get_header_by_hash
%% get_header_by_hash

%% save_by_hash
%% get_by_hash

%% -export([top/0,
%%          top_block/0, %% TODO See aec_blocks:top and tests using it.
%%          prolong_chain/1,
         
%%          save_header/1,
%%          get_header_by_hash/1,
%%          save_block/1,
%%          get_block_by_hash/1
%%         ]).
