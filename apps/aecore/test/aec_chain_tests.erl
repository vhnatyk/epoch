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
             ?assertEqual({ok, GB}, aec_chain:get_block_by_hash(GHH)),
             ok
     end}.

%% header_chain_test_() ->

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
