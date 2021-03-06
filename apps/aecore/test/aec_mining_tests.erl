-module(aec_mining_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_mining).
-define(MAX_DIFFICULTY, 256 * 256 + 255).

mine_block_test_() ->
    {foreach,
     fun() ->
             application:start(crypto),
             meck:new(aec_blocks, [passthrough]),
             meck:new(aec_headers, [passthrough]),
             meck:new(aec_pow_sha256, [passthrough]),
             meck:new(aec_tx, [passthrough]),
             meck:new(aec_governance, [passthrough])
     end,
     fun(_) ->
             application:stop(crypto),
             meck:unload(aec_blocks),
             meck:unload(aec_headers),
             meck:unload(aec_pow_sha256),
             meck:unload(aec_tx),
             meck:unload(aec_governance)
     end,
     [
      {"Find a new block",
       fun() ->
               Trees = #trees{accounts = [#account{pubkey = <<"pubkey">>}]},
               meck:expect(aec_blocks, top, 0, {ok, #block{difficulty = ?MAX_DIFFICULTY}}),
               meck:expect(aec_pow_sha256, pick_nonce, 0, 1),
               meck:expect(aec_tx, apply_signed, 3, {ok, Trees}),

               {ok, Block} = ?TEST_MODULE:mine(),

               ?assertEqual(1, Block#block.height),
               ?assertEqual(1, Block#block.nonce),
               ?assertEqual(1, length(Block#block.txs))
       end},
      {"Proof of work fails with generation_count_exhausted",
       fun() ->
               Trees = #trees{accounts = [#account{pubkey = <<"pubkey">>}]},
               meck:expect(aec_blocks, top, 0, {ok, #block{difficulty = 1}}),
               meck:expect(aec_pow_sha256, pick_nonce, 0, 1),
               meck:expect(aec_tx, apply_signed, 3, {ok, Trees}),

               ?assertEqual({error, generation_count_exhausted}, ?TEST_MODULE:mine())
       end},
      {"Cannot apply signed tx",
       fun() ->
               meck:expect(aec_tx, apply_signed, 3, {error, tx_failed}),

               ?assertEqual({error, tx_failed}, ?TEST_MODULE:mine())
       end},
      {"For good mining speed mine block with the same difficulty",
       fun() ->
               Trees = #trees{accounts = [#account{pubkey = <<"pubkey">>}]},
               Now = 1504731164584,
               meck:expect(aec_blocks, top, 0, {ok, #block{}}),
               meck:expect(aec_blocks, new, 3,
                           {ok, #block{height = 30,
                                       difficulty = ?MAX_DIFFICULTY,
                                       time = Now}}),
               meck:expect(aec_headers, get_by_height, 1,
                           {ok, #header{height = 20,
                                        time = Now - 50000}}),
               meck:expect(aec_pow_sha256, pick_nonce, 0, 1),
               meck:expect(aec_tx, apply_signed, 3, {ok, Trees}),
               meck:expect(aec_governance, recalculate_difficulty_frequency, 0, 10),
               meck:expect(aec_governance, expected_block_mine_rate, 0, 5),

               {ok, Block} = ?TEST_MODULE:mine(),

               ?assertEqual(30, Block#block.height),
               ?assertEqual(1, Block#block.nonce),
               ?assertEqual(?MAX_DIFFICULTY, Block#block.difficulty),
               ?assertEqual(2, meck:num_calls(aec_governance, recalculate_difficulty_frequency, 0)),
               ?assertEqual(1, meck:num_calls(aec_governance, expected_block_mine_rate, 0))
       end},
      {"Too few blocks mined in time increases new block's difficulty",
       fun() ->
               Trees = #trees{accounts = [#account{pubkey = <<"pubkey">>}]},
               Now = 1504731164584,
               meck:expect(aec_blocks, top, 0, {ok, #block{}}),
               meck:expect(aec_blocks, new, 3,
                           {ok, #block{height = 200,
                                       difficulty = ?MAX_DIFFICULTY,
                                       time = Now}}),
               meck:expect(aec_headers, get_by_height, 1,
                           {ok, #header{height = 190,
                                        time = Now - 11000}}),
               meck:expect(aec_pow_sha256, pick_nonce, 0, 1),
               meck:expect(aec_tx, apply_signed, 3, {ok, Trees}),
               meck:expect(aec_governance, recalculate_difficulty_frequency, 0, 10),
               meck:expect(aec_governance, expected_block_mine_rate, 0, 100000),

               {ok, Block} = ?TEST_MODULE:mine(),

               ?assertEqual(200, Block#block.height),
               ?assertEqual(1, Block#block.nonce),
               ?assertEqual(true, ?MAX_DIFFICULTY < Block#block.difficulty)
       end}
     ]
    }.

-endif.
