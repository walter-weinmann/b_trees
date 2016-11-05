%%%-------------------------------------------------------------------
%%% File        : performance_SUITE.erl
%%% Description : Test Suite for module: b_trees.
%%%
%%% Created     : 18.10.2016
%%%
%%% Copyright (C) 2016 Walter Weinmann
%%%-------------------------------------------------------------------

-module(performance_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LARGEST_KEY_VALUE, {"k_2000", "v_2000"}).
-define(NUMBER_DELETES, 2000).
-define(NUMBER_INSERTS, 2000).
-define(NUMBER_KEYS, 10000).
-define(NUMBER_UPDATES, 2000).
-define(SMALLEST_KEY_VALUE, {"k_0001", "v_0001"}).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - SUITE
%%--------------------------------------------------------------------

suite() ->
    [
        {timetrap, {minutes, 10}}
    ].

init_per_suite(Config) ->
    [
        {key_values_from, test_generator:generate_key_values_from(?NUMBER_INSERTS, 4)},
        {key_values_from_even_odd, test_generator:generate_key_values_from_even(?NUMBER_INSERTS, 4) ++ test_generator:generate_key_values_from_odd(?NUMBER_INSERTS, 4)},
        {key_values_from_odd_even, test_generator:generate_key_values_from_odd(?NUMBER_INSERTS, 4) ++ test_generator:generate_key_values_from_even(?NUMBER_INSERTS, 4)},
        {key_values_from_update, test_generator:generate_key_values_from_update(?NUMBER_INSERTS, 4)},
        {key_values_rand_update, test_generator:generate_key_values_rand_update(?NUMBER_UPDATES, ?NUMBER_UPDATES, 4)},
        {keys_from, test_generator:generate_keys_from(?NUMBER_INSERTS, 4)},
        {keys_from_even_odd, test_generator:generate_keys_from_even(?NUMBER_INSERTS, 4) ++ test_generator:generate_keys_from_odd(?NUMBER_INSERTS, 4)},
        {keys_from_odd_even, test_generator:generate_keys_from_odd(?NUMBER_INSERTS, 4) ++ test_generator:generate_keys_from_even(?NUMBER_INSERTS, 4)},
        {keys_rand, test_generator:generate_keys_rand(?NUMBER_INSERTS, ?NUMBER_KEYS, 4)},

        {gbtree, test_generator:generate_gb_tree_from_number(?NUMBER_INSERTS, 4)},
        {gbtree_new, test_generator:generate_gb_tree_from_number_update(?NUMBER_INSERTS, 4)},

        {btree_4, test_generator:generate_b_tree_from_number(4, ?NUMBER_INSERTS, 4)},
        {btree_4_new, test_generator:generate_b_tree_from_number_update(4, ?NUMBER_INSERTS, 4)},
        {btree_6, test_generator:generate_b_tree_from_number(6, ?NUMBER_INSERTS, 4)},
        {btree_6_new, test_generator:generate_b_tree_from_number_update(6, ?NUMBER_INSERTS, 4)},
        {btree_6_till, test_generator:generate_b_tree_till_number(6, ?NUMBER_INSERTS, 4)},
        {btree_8, test_generator:generate_b_tree_from_number(8, ?NUMBER_INSERTS, 4)},
        {btree_8_new, test_generator:generate_b_tree_from_number_update(8, ?NUMBER_INSERTS, 4)},
        {btree_16, test_generator:generate_b_tree_from_number(16, ?NUMBER_INSERTS, 4)},
        {btree_16_new, test_generator:generate_b_tree_from_number_update(16, ?NUMBER_INSERTS, 4)},
        {btree_32, test_generator:generate_b_tree_from_number(32, ?NUMBER_INSERTS, 4)},
        {btree_32_new, test_generator:generate_b_tree_from_number_update(32, ?NUMBER_INSERTS, 4)},
        {btree_64, test_generator:generate_b_tree_from_number(64, ?NUMBER_INSERTS, 4)},
        {btree_64_new, test_generator:generate_b_tree_from_number_update(64, ?NUMBER_INSERTS, 4)},
        {btree_128, test_generator:generate_b_tree_from_number(128, ?NUMBER_INSERTS, 4)},
        {btree_128_new, test_generator:generate_b_tree_from_number_update(128, ?NUMBER_INSERTS, 4)},
        {btree_256, test_generator:generate_b_tree_from_number(256, ?NUMBER_INSERTS, 4)},
        {btree_256_new, test_generator:generate_b_tree_from_number_update(256, ?NUMBER_INSERTS, 4)},
        {btree_612, test_generator:generate_b_tree_from_number(512, ?NUMBER_INSERTS, 4)},
        {btree_612_new, test_generator:generate_b_tree_from_number_update(512, ?NUMBER_INSERTS, 4)},
        {btree_1024, test_generator:generate_b_tree_from_number(1024, ?NUMBER_INSERTS, 4)},
        {btree_1024_new, test_generator:generate_b_tree_from_number_update(1024, ?NUMBER_INSERTS, 4)}
        | Config
    ].

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%--------------------------------------------------------------------

all() ->
    [
        delete_gb_tree_test,
        delete_b_tree_order_4_even_odd_test,
        delete_b_tree_order_4_odd_even_test,
        delete_b_tree_order_4_test,
        delete_b_tree_order_6_test,
        delete_b_tree_order_8_even_odd_test,
        delete_b_tree_order_8_odd_even_test,
        delete_b_tree_order_8_test,
        delete_b_tree_order_16_test,
        delete_b_tree_order_32_test,
        delete_b_tree_order_64_test,
        delete_b_tree_order_128_test,
        delete_b_tree_order_256_test,
        delete_b_tree_order_612_test,
        delete_b_tree_order_1024_test,

        enter_gb_tree_test,
        enter_b_tree_order_4_test,
        enter_b_tree_order_6_test,
        enter_b_tree_order_8_test,
        enter_b_tree_order_16_test,
        enter_b_tree_order_32_test,
        enter_b_tree_order_64_test,
        enter_b_tree_order_128_test,
        enter_b_tree_order_256_test,
        enter_b_tree_order_612_test,
        enter_b_tree_order_1024_test,

        from_dict_gb_tree_test,
        from_dict_b_tree_order_4_test,
        from_dict_b_tree_order_6_test,
        from_dict_b_tree_order_8_even_odd_test,
        from_dict_b_tree_order_8_odd_even_test,
        from_dict_b_tree_order_8_test,
        from_dict_b_tree_order_16_test,
        from_dict_b_tree_order_32_test,
        from_dict_b_tree_order_64_test,
        from_dict_b_tree_order_128_test,
        from_dict_b_tree_order_256_test,
        from_dict_b_tree_order_612_test,
        from_dict_b_tree_order_1024_test,

        get_gb_tree_test,
        get_b_tree_order_4_test,
        get_b_tree_order_6_test,
        get_b_tree_order_8_test,
        get_b_tree_order_16_test,
        get_b_tree_order_32_test,
        get_b_tree_order_64_test,
        get_b_tree_order_128_test,
        get_b_tree_order_256_test,
        get_b_tree_order_612_test,
        get_b_tree_order_1024_test,

        insert_gb_tree_test,
        insert_b_tree_order_4_test,
        insert_b_tree_order_6_test,
        insert_b_tree_order_6_till_test,
        insert_b_tree_order_8_even_odd_test,
        insert_b_tree_order_8_odd_even_test,
        insert_b_tree_order_8_test,
        insert_b_tree_order_16_test,
        insert_b_tree_order_32_test,
        insert_b_tree_order_64_test,
        insert_b_tree_order_128_test,
        insert_b_tree_order_256_test,
        insert_b_tree_order_612_test,
        insert_b_tree_order_1024_test,

        is_defined_gb_tree_test,
        is_defined_b_tree_order_4_test,
        is_defined_b_tree_order_6_test,
        is_defined_b_tree_order_8_test,
        is_defined_b_tree_order_16_test,
        is_defined_b_tree_order_32_test,
        is_defined_b_tree_order_64_test,
        is_defined_b_tree_order_128_test,
        is_defined_b_tree_order_256_test,
        is_defined_b_tree_order_612_test,
        is_defined_b_tree_order_1024_test,

        iterate_next_gb_tree_test,
        iterate_next_b_tree_order_4_test,
        iterate_next_b_tree_order_6_test,
        iterate_next_b_tree_order_8_test,
        iterate_next_b_tree_order_16_test,
        iterate_next_b_tree_order_32_test,
        iterate_next_b_tree_order_64_test,
        iterate_next_b_tree_order_128_test,
        iterate_next_b_tree_order_256_test,
        iterate_next_b_tree_order_612_test,
        iterate_next_b_tree_order_1024_test,

        keys_gb_tree_test,
        keys_b_tree_order_4_test,
        keys_b_tree_order_6_test,
        keys_b_tree_order_8_test,
        keys_b_tree_order_16_test,
        keys_b_tree_order_32_test,
        keys_b_tree_order_64_test,
        keys_b_tree_order_128_test,
        keys_b_tree_order_256_test,
        keys_b_tree_order_612_test,
        keys_b_tree_order_1024_test,

        largest_gb_tree_test,
        largest_b_tree_order_4_test,
        largest_b_tree_order_6_test,
        largest_b_tree_order_8_test,
        largest_b_tree_order_16_test,
        largest_b_tree_order_32_test,
        largest_b_tree_order_64_test,
        largest_b_tree_order_128_test,
        largest_b_tree_order_256_test,
        largest_b_tree_order_612_test,
        largest_b_tree_order_1024_test,

        lookup_gb_tree_test,
        lookup_b_tree_order_4_test,
        lookup_b_tree_order_6_test,
        lookup_b_tree_order_8_test,
        lookup_b_tree_order_16_test,
        lookup_b_tree_order_32_test,
        lookup_b_tree_order_64_test,
        lookup_b_tree_order_128_test,
        lookup_b_tree_order_256_test,
        lookup_b_tree_order_612_test,
        lookup_b_tree_order_1024_test,

        map_gb_tree_test,
        map_b_tree_order_4_test,
        map_b_tree_order_6_test,
        map_b_tree_order_8_test,
        map_b_tree_order_16_test,
        map_b_tree_order_32_test,
        map_b_tree_order_64_test,
        map_b_tree_order_128_test,
        map_b_tree_order_256_test,
        map_b_tree_order_612_test,
        map_b_tree_order_1024_test,

        smallest_gb_tree_test,
        smallest_b_tree_order_4_test,
        smallest_b_tree_order_6_test,
        smallest_b_tree_order_8_test,
        smallest_b_tree_order_16_test,
        smallest_b_tree_order_32_test,
        smallest_b_tree_order_64_test,
        smallest_b_tree_order_128_test,
        smallest_b_tree_order_256_test,
        smallest_b_tree_order_612_test,
        smallest_b_tree_order_1024_test,

        take_largest_gb_tree_test,
        take_largest_b_tree_order_4_test,
        take_largest_b_tree_order_6_test,
        take_largest_b_tree_order_8_test,
        take_largest_b_tree_order_16_test,
        take_largest_b_tree_order_32_test,
        take_largest_b_tree_order_64_test,
        take_largest_b_tree_order_128_test,
        take_largest_b_tree_order_256_test,
        take_largest_b_tree_order_612_test,
        take_largest_b_tree_order_1024_test,

        take_smallest_gb_tree_test,
        take_smallest_b_tree_order_4_test,
        take_smallest_b_tree_order_6_test,
        take_smallest_b_tree_order_8_test,
        take_smallest_b_tree_order_16_test,
        take_smallest_b_tree_order_32_test,
        take_smallest_b_tree_order_64_test,
        take_smallest_b_tree_order_128_test,
        take_smallest_b_tree_order_256_test,
        take_smallest_b_tree_order_612_test,
        take_smallest_b_tree_order_1024_test,

        to_list_gb_tree_test,
        to_list_b_tree_order_4_test,
        to_list_b_tree_order_6_test,
        to_list_b_tree_order_8_test,
        to_list_b_tree_order_16_test,
        to_list_b_tree_order_32_test,
        to_list_b_tree_order_64_test,
        to_list_b_tree_order_128_test,
        to_list_b_tree_order_256_test,
        to_list_b_tree_order_612_test,
        to_list_b_tree_order_1024_test,

        update_gb_tree_test,
        update_b_tree_order_4_test,
        update_b_tree_order_6_test,
        update_b_tree_order_8_test,
        update_b_tree_order_16_test,
        update_b_tree_order_32_test,
        update_b_tree_order_64_test,
        update_b_tree_order_128_test,
        update_b_tree_order_256_test,
        update_b_tree_order_612_test,
        update_b_tree_order_1024_test,

        values_gb_tree_test,
        values_b_tree_order_4_test,
        values_b_tree_order_6_test,
        values_b_tree_order_8_test,
        values_b_tree_order_16_test,
        values_b_tree_order_32_test,
        values_b_tree_order_64_test,
        values_b_tree_order_128_test,
        values_b_tree_order_256_test,
        values_b_tree_order_612_test,
        values_b_tree_order_1024_test
    ].

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 1024
%%--------------------------------------------------------------------

delete_b_tree_order_1024_test(_Config) ->
    ?assertEqual(b_trees:empty(1024), test_generator:delete_b_tree_from(1024, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 128
%%--------------------------------------------------------------------

delete_b_tree_order_128_test(_Config) ->
    ?assertEqual(b_trees:empty(128), test_generator:delete_b_tree_from(128, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 16
%%--------------------------------------------------------------------

delete_b_tree_order_16_test(_Config) ->
    ?assertEqual(b_trees:empty(16), test_generator:delete_b_tree_from(16, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 256
%%--------------------------------------------------------------------

delete_b_tree_order_256_test(_Config) ->
    ?assertEqual(b_trees:empty(256), test_generator:delete_b_tree_from(256, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 32
%%--------------------------------------------------------------------

delete_b_tree_order_32_test(_Config) ->
    ?assertEqual(b_trees:empty(32), test_generator:delete_b_tree_from(32, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - even / odd
%%--------------------------------------------------------------------

delete_b_tree_order_4_even_odd_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(4, ?NUMBER_INSERTS, 4),
    _BTree_Odd = test_generator:delete_b_tree_from_even(4, ?NUMBER_DELETES, 4, _BTree),
    _BTree_Empty = test_generator:delete_b_tree_from_odd(4, ?NUMBER_DELETES, 4, _BTree_Odd),
    ?assertEqual(b_trees:empty(4), _BTree_Empty),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - odd / even
%%--------------------------------------------------------------------

delete_b_tree_order_4_odd_even_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(4, ?NUMBER_INSERTS, 4),
    _BTree_Even = test_generator:delete_b_tree_from_odd(4, ?NUMBER_DELETES, 4, _BTree),
    _BTree_Empty = test_generator:delete_b_tree_from_even(4, ?NUMBER_DELETES, 4, _BTree_Even),
    ?assertEqual(b_trees:empty(4), _BTree_Empty),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4
%%--------------------------------------------------------------------

delete_b_tree_order_4_test(_Config) ->
    ?assertEqual(b_trees:empty(4), test_generator:delete_b_tree_from(4, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 6
%%--------------------------------------------------------------------

delete_b_tree_order_6_test(_Config) ->
    ?assertEqual(b_trees:empty(6), test_generator:delete_b_tree_from(6, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 512
%%--------------------------------------------------------------------

delete_b_tree_order_612_test(_Config) ->
    ?assertEqual(b_trees:empty(512), test_generator:delete_b_tree_from(512, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 64
%%--------------------------------------------------------------------

delete_b_tree_order_64_test(_Config) ->
    ?assertEqual(b_trees:empty(64), test_generator:delete_b_tree_from(64, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 8 - even / odd
%%--------------------------------------------------------------------

delete_b_tree_order_8_even_odd_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(8, ?NUMBER_INSERTS, 4),
    _BTree_Odd = test_generator:delete_b_tree_from_even(8, ?NUMBER_DELETES, 4, _BTree),
    _BTree_Empty = test_generator:delete_b_tree_from_odd(8, ?NUMBER_DELETES, 4, _BTree_Odd),
    ?assertEqual(b_trees:empty(8), _BTree_Empty),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 8 - odd / even
%%--------------------------------------------------------------------

delete_b_tree_order_8_odd_even_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(8, ?NUMBER_INSERTS, 4),
    _BTree_Even = test_generator:delete_b_tree_from_odd(8, ?NUMBER_DELETES, 4, _BTree),
    _BTree_Empty = test_generator:delete_b_tree_from_even(8, ?NUMBER_DELETES, 4, _BTree_Even),
    ?assertEqual(b_trees:empty(8), _BTree_Empty),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 8
%%--------------------------------------------------------------------

delete_b_tree_order_8_test(_Config) ->
    ?assertEqual(b_trees:empty(8), test_generator:delete_b_tree_from(8, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete gb_tree
%%--------------------------------------------------------------------

delete_gb_tree_test(_Config) ->
    ?assertEqual(gb_trees:empty(), test_generator:delete_gb_tree_from(?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 1024
%%--------------------------------------------------------------------

enter_b_tree_order_1024_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(1024)),
    ?assertEqual(?config(btree_1024, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_1024_new, Config), _BTreeUpdate),
    ok.

enter_b_tree([], BTree) ->
    BTree;
enter_b_tree([{Key, Value} | Tail], BTree) ->
    enter_b_tree(Tail, b_trees:enter(Key, Value, BTree)).

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 128
%%--------------------------------------------------------------------

enter_b_tree_order_128_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(128)),
    ?assertEqual(?config(btree_128, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_128_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 16
%%--------------------------------------------------------------------

enter_b_tree_order_16_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(16)),
    ?assertEqual(?config(btree_16, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_16_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 256
%%--------------------------------------------------------------------

enter_b_tree_order_256_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(256)),
    ?assertEqual(?config(btree_256, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_256_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 32
%%--------------------------------------------------------------------

enter_b_tree_order_32_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(32)),
    ?assertEqual(?config(btree_32, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_32_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 4
%%--------------------------------------------------------------------

enter_b_tree_order_4_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(4)),
    ?assertEqual(?config(btree_4, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_4_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 6
%%--------------------------------------------------------------------

enter_b_tree_order_6_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(6)),
    ?assertEqual(?config(btree_6, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_6_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 512
%%--------------------------------------------------------------------

enter_b_tree_order_612_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(512)),
    ?assertEqual(?config(btree_612, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_612_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 64
%%--------------------------------------------------------------------

enter_b_tree_order_64_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(64)),
    ?assertEqual(?config(btree_64, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_64_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 8
%%--------------------------------------------------------------------

enter_b_tree_order_8_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(8)),
    ?assertEqual(?config(btree_8, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_8_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter gb_tree
%%--------------------------------------------------------------------

enter_gb_tree_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    GBTree = enter_gb_tree(KeyValues, gb_trees:empty()),
    ?assertEqual(?config(gbtree, Config), GBTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _GBTreeUpdate = enter_gb_tree(_KeyValuesUpdate, GBTree),
    ?assertEqual(?config(gbtree_new, Config), _GBTreeUpdate),
    ok.

enter_gb_tree([], GBTree) ->
    GBTree;
enter_gb_tree([{Key, Value} | Tail], GBTree) ->
    enter_gb_tree(Tail, gb_trees:enter(Key, Value, GBTree)).

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 1024
%%--------------------------------------------------------------------

from_dict_b_tree_order_1024_test(Config) ->
    _BTree = ?config(btree_1024, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(1024, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 128
%%--------------------------------------------------------------------

from_dict_b_tree_order_128_test(Config) ->
    _BTree = ?config(btree_128, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(128, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 16
%%--------------------------------------------------------------------

from_dict_b_tree_order_16_test(Config) ->
    _BTree = ?config(btree_16, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(16, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 256
%%--------------------------------------------------------------------

from_dict_b_tree_order_256_test(Config) ->
    _BTree = ?config(btree_256, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(256, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 32
%%--------------------------------------------------------------------

from_dict_b_tree_order_32_test(Config) ->
    _BTree = ?config(btree_32, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(32, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 4
%%--------------------------------------------------------------------

from_dict_b_tree_order_4_test(Config) ->
    _BTree = ?config(btree_4, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(4, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 6
%%--------------------------------------------------------------------

from_dict_b_tree_order_6_test(Config) ->
    _BTree = ?config(btree_6, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(6, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 512
%%--------------------------------------------------------------------

from_dict_b_tree_order_612_test(Config) ->
    _BTree = ?config(btree_612, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(512, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 64
%%--------------------------------------------------------------------

from_dict_b_tree_order_64_test(Config) ->
    _BTree = ?config(btree_64, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(64, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 8 - even / odd
%%--------------------------------------------------------------------

from_dict_b_tree_order_8_even_odd_test(Config) ->
    _BTree = ?config(btree_8, Config),
    _KeyValues = ?config(key_values_from_even_odd, Config),
    ?assertEqual(?NUMBER_INSERTS, b_trees:number_key_values(b_trees:from_dict(8, _KeyValues))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 8 - odd / even
%%--------------------------------------------------------------------

from_dict_b_tree_order_8_odd_even_test(Config) ->
    _BTree = ?config(btree_8, Config),
    _KeyValues = ?config(key_values_from_odd_even, Config),
    ?assertEqual(?NUMBER_INSERTS, b_trees:number_key_values(b_trees:from_dict(8, _KeyValues))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 8
%%--------------------------------------------------------------------

from_dict_b_tree_order_8_test(Config) ->
    _BTree = ?config(btree_8, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(8, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict gb_tree
%%--------------------------------------------------------------------

from_dict_gb_tree_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    gb_trees:from_orddict(KeyValues),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 1024
%%--------------------------------------------------------------------

get_b_tree_order_1024_test(Config) ->
    BTree = ?config(btree_1024, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

get_b_tree([], _) ->
    none;
get_b_tree([Key | Tail], BTree) ->
    b_trees:get(Key, BTree),
    get_b_tree(Tail, BTree).

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 128
%%--------------------------------------------------------------------

get_b_tree_order_128_test(Config) ->
    BTree = ?config(btree_128, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 16
%%--------------------------------------------------------------------

get_b_tree_order_16_test(Config) ->
    BTree = ?config(btree_16, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 256
%%--------------------------------------------------------------------

get_b_tree_order_256_test(Config) ->
    BTree = ?config(btree_256, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 32
%%--------------------------------------------------------------------

get_b_tree_order_32_test(Config) ->
    BTree = ?config(btree_32, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 4
%%--------------------------------------------------------------------

get_b_tree_order_4_test(Config) ->
    BTree = ?config(btree_4, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 6
%%--------------------------------------------------------------------

get_b_tree_order_6_test(Config) ->
    BTree = ?config(btree_6, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 512
%%--------------------------------------------------------------------

get_b_tree_order_612_test(Config) ->
    BTree = ?config(btree_612, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 64
%%--------------------------------------------------------------------

get_b_tree_order_64_test(Config) ->
    BTree = ?config(btree_64, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 8
%%--------------------------------------------------------------------

get_b_tree_order_8_test(Config) ->
    BTree = ?config(btree_8, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get gb_tree
%%--------------------------------------------------------------------

get_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    Keys = ?config(keys_rand, Config),
    get_gb_tree(Keys, GBTree),
    ok.

get_gb_tree([], _) ->
    none;
get_gb_tree([Key | Tail], GBTree) ->
    gb_trees:get(Key, GBTree),
    get_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 1024
%%--------------------------------------------------------------------

insert_b_tree_order_1024_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(1024, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_1024, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(1024 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 128
%%--------------------------------------------------------------------

insert_b_tree_order_128_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(128, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_128, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(128 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 16
%%--------------------------------------------------------------------

insert_b_tree_order_16_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(16, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_16, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(16 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 256
%%--------------------------------------------------------------------

insert_b_tree_order_256_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(256, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_256, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(256 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 32
%%--------------------------------------------------------------------

insert_b_tree_order_32_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(32, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_32, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(32 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 4
%%--------------------------------------------------------------------

insert_b_tree_order_4_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(4, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_4, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(4 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 6
%%--------------------------------------------------------------------

insert_b_tree_order_6_test(_Config) ->
    _BTree = test_generator:generate_b_tree_till_number(6, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_6_till, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(6 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 6 - till
%%--------------------------------------------------------------------

insert_b_tree_order_6_till_test(_Config) ->
    _BTree = test_generator:generate_b_tree_till_number(6, ?NUMBER_INSERTS, 4),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(6 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 512
%%--------------------------------------------------------------------

insert_b_tree_order_612_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(512, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_612, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(512 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 64
%%--------------------------------------------------------------------

insert_b_tree_order_64_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(64, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_64, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(64 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 8 - even / odd
%%--------------------------------------------------------------------

insert_b_tree_order_8_even_odd_test(Config) ->
    KeyValues = ?config(key_values_from_even_odd, Config),
    _BTree = test_generator:generate_b_tree_from_list(8, KeyValues),
    ?assertEqual(?NUMBER_INSERTS, b_trees:number_key_values(_BTree)),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(8 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 8 - odd / even
%%--------------------------------------------------------------------

insert_b_tree_order_8_odd_even_test(Config) ->
    KeyValues = ?config(key_values_from_odd_even, Config),
    _BTree = test_generator:generate_b_tree_from_list(8, KeyValues),
    ?assertEqual(?NUMBER_INSERTS, b_trees:number_key_values(_BTree)),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(8 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 8
%%--------------------------------------------------------------------

insert_b_tree_order_8_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(8, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_8, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(8 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: insert gb_tree
%%--------------------------------------------------------------------

insert_gb_tree_test(_Config) ->
    ?assertEqual(?config(gbtree, _Config), test_generator:generate_gb_tree_from_number(?NUMBER_INSERTS, 4)).

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 1024
%%--------------------------------------------------------------------

is_defined_b_tree_order_1024_test(Config) ->
    BTree = ?config(btree_1024, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

is_defined_b_tree([], _) ->
    none;
is_defined_b_tree([Key | Tail], BTree) ->
    b_trees:is_defined(Key, BTree),
    is_defined_b_tree(Tail, BTree).

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 128
%%--------------------------------------------------------------------

is_defined_b_tree_order_128_test(Config) ->
    BTree = ?config(btree_128, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 16
%%--------------------------------------------------------------------

is_defined_b_tree_order_16_test(Config) ->
    BTree = ?config(btree_16, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 256
%%--------------------------------------------------------------------

is_defined_b_tree_order_256_test(Config) ->
    BTree = ?config(btree_256, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 32
%%--------------------------------------------------------------------

is_defined_b_tree_order_32_test(Config) ->
    BTree = ?config(btree_32, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 4
%%--------------------------------------------------------------------

is_defined_b_tree_order_4_test(Config) ->
    BTree = ?config(btree_4, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 6
%%--------------------------------------------------------------------

is_defined_b_tree_order_6_test(Config) ->
    BTree = ?config(btree_6, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 512
%%--------------------------------------------------------------------

is_defined_b_tree_order_612_test(Config) ->
    BTree = ?config(btree_612, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 64
%%--------------------------------------------------------------------

is_defined_b_tree_order_64_test(Config) ->
    BTree = ?config(btree_64, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 8
%%--------------------------------------------------------------------

is_defined_b_tree_order_8_test(Config) ->
    BTree = ?config(btree_8, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined gb_tree
%%--------------------------------------------------------------------

is_defined_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_gb_tree(Keys, GBTree),
    ok.

is_defined_gb_tree([], _) ->
    none;
is_defined_gb_tree([Key | Tail], GBTree) ->
    gb_trees:is_defined(Key, GBTree),
    is_defined_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 1024
%%--------------------------------------------------------------------

iterate_next_b_tree_order_1024_test(Config) ->
    BTree = ?config(btree_1024, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_INSERTS, [])),
    ok.

iterate_next_b_tree(_, 0, KeyValues) ->
    KeyValues;
iterate_next_b_tree(Iterator, Count, KeyValues) ->
    {Key, Value, IteratorNew} = b_trees:next(Iterator),
    iterate_next_b_tree(IteratorNew, Count - 1, KeyValues ++ [{Key, Value}]).

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 128
%%--------------------------------------------------------------------

iterate_next_b_tree_order_128_test(Config) ->
    BTree = ?config(btree_128, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_INSERTS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 16
%%--------------------------------------------------------------------

iterate_next_b_tree_order_16_test(Config) ->
    BTree = ?config(btree_16, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_INSERTS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 256
%%--------------------------------------------------------------------

iterate_next_b_tree_order_256_test(Config) ->
    BTree = ?config(btree_256, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_INSERTS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 32
%%--------------------------------------------------------------------

iterate_next_b_tree_order_32_test(Config) ->
    BTree = ?config(btree_32, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_INSERTS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 4
%%--------------------------------------------------------------------

iterate_next_b_tree_order_4_test(Config) ->
    BTree = ?config(btree_4, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_INSERTS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 6
%%--------------------------------------------------------------------

iterate_next_b_tree_order_6_test(Config) ->
    BTree = ?config(btree_6, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_INSERTS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 512
%%--------------------------------------------------------------------

iterate_next_b_tree_order_612_test(Config) ->
    BTree = ?config(btree_612, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_INSERTS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 64
%%--------------------------------------------------------------------

iterate_next_b_tree_order_64_test(Config) ->
    BTree = ?config(btree_64, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_INSERTS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 8
%%--------------------------------------------------------------------

iterate_next_b_tree_order_8_test(Config) ->
    BTree = ?config(btree_8, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_INSERTS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next gb_tree
%%--------------------------------------------------------------------

iterate_next_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    _Iterator = gb_trees:iterator(GBTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_gb_tree(_Iterator, ?NUMBER_INSERTS, [])),
    ok.

iterate_next_gb_tree(_, 0, KeyValues) ->
    KeyValues;
iterate_next_gb_tree(Iterator, Count, KeyValues) ->
    {Key, Value, IteratorNew} = gb_trees:next(Iterator),
    iterate_next_gb_tree(IteratorNew, Count - 1, KeyValues ++ [{Key, Value}]).

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 1024
%%--------------------------------------------------------------------

keys_b_tree_order_1024_test(Config) ->
    BTree = ?config(btree_1024, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 128
%%--------------------------------------------------------------------

keys_b_tree_order_128_test(Config) ->
    BTree = ?config(btree_128, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 16
%%--------------------------------------------------------------------

keys_b_tree_order_16_test(Config) ->
    BTree = ?config(btree_16, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 256
%%--------------------------------------------------------------------

keys_b_tree_order_256_test(Config) ->
    BTree = ?config(btree_256, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 32
%%--------------------------------------------------------------------

keys_b_tree_order_32_test(Config) ->
    BTree = ?config(btree_32, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 4
%%--------------------------------------------------------------------

keys_b_tree_order_4_test(Config) ->
    BTree = ?config(btree_4, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 6
%%--------------------------------------------------------------------

keys_b_tree_order_6_test(Config) ->
    BTree = ?config(btree_6, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 512
%%--------------------------------------------------------------------

keys_b_tree_order_612_test(Config) ->
    BTree = ?config(btree_612, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 64
%%--------------------------------------------------------------------

keys_b_tree_order_64_test(Config) ->
    BTree = ?config(btree_64, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 8
%%--------------------------------------------------------------------

keys_b_tree_order_8_test(Config) ->
    BTree = ?config(btree_8, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys_rand gb_tree
%%--------------------------------------------------------------------

keys_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    _Keys = gb_trees:keys(GBTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 1024
%%--------------------------------------------------------------------

largest_b_tree_order_1024_test(Config) ->
    _BTree = ?config(btree_1024, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 128
%%--------------------------------------------------------------------

largest_b_tree_order_128_test(Config) ->
    _BTree = ?config(btree_128, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 16
%%--------------------------------------------------------------------

largest_b_tree_order_16_test(Config) ->
    _BTree = ?config(btree_16, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 256
%%--------------------------------------------------------------------

largest_b_tree_order_256_test(Config) ->
    _BTree = ?config(btree_256, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 32
%%--------------------------------------------------------------------

largest_b_tree_order_32_test(Config) ->
    _BTree = ?config(btree_32, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 4
%%--------------------------------------------------------------------

largest_b_tree_order_4_test(Config) ->
    _BTree = ?config(btree_4, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 6
%%--------------------------------------------------------------------

largest_b_tree_order_6_test(Config) ->
    _BTree = ?config(btree_6, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 512
%%--------------------------------------------------------------------

largest_b_tree_order_612_test(Config) ->
    _BTree = ?config(btree_612, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 64
%%--------------------------------------------------------------------

largest_b_tree_order_64_test(Config) ->
    _BTree = ?config(btree_64, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 8
%%--------------------------------------------------------------------

largest_b_tree_order_8_test(Config) ->
    _BTree = ?config(btree_8, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest gb_tree
%%--------------------------------------------------------------------

largest_gb_tree_test(_Config) ->
    ?assertEqual(?LARGEST_KEY_VALUE, gb_trees:largest(?config(gbtree, _Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 1024
%%--------------------------------------------------------------------

lookup_b_tree_order_1024_test(Config) ->
    BTree = ?config(btree_1024, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

lookup_b_tree([], _) ->
    none;
lookup_b_tree([Key | Tail], BTree) ->
    b_trees:lookup(Key, BTree),
    lookup_b_tree(Tail, BTree).

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 128
%%--------------------------------------------------------------------

lookup_b_tree_order_128_test(Config) ->
    BTree = ?config(btree_128, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 16
%%--------------------------------------------------------------------

lookup_b_tree_order_16_test(Config) ->
    BTree = ?config(btree_16, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 256
%%--------------------------------------------------------------------

lookup_b_tree_order_256_test(Config) ->
    BTree = ?config(btree_256, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 32
%%--------------------------------------------------------------------

lookup_b_tree_order_32_test(Config) ->
    BTree = ?config(btree_32, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 4
%%--------------------------------------------------------------------

lookup_b_tree_order_4_test(Config) ->
    BTree = ?config(btree_4, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 6
%%--------------------------------------------------------------------

lookup_b_tree_order_6_test(Config) ->
    BTree = ?config(btree_6, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 512
%%--------------------------------------------------------------------

lookup_b_tree_order_612_test(Config) ->
    BTree = ?config(btree_612, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 64
%%--------------------------------------------------------------------

lookup_b_tree_order_64_test(Config) ->
    BTree = ?config(btree_64, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 8
%%--------------------------------------------------------------------

lookup_b_tree_order_8_test(Config) ->
    BTree = ?config(btree_8, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup gb_tree
%%--------------------------------------------------------------------

lookup_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    Keys = ?config(keys_rand, Config),
    lookup_gb_tree(Keys, GBTree),
    ok.

lookup_gb_tree([], _) ->
    none;
lookup_gb_tree([Key | Tail], GBTree) ->
    gb_trees:lookup(Key, GBTree),
    lookup_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 1024
%%--------------------------------------------------------------------

map_b_tree_order_1024_test(Config) ->
    _BTree = ?config(btree_1024, Config),
    _BTreeNew = ?config(btree_1024_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

map_value_to_new(_, Value) ->
    Value ++ "_new".

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 128
%%--------------------------------------------------------------------

map_b_tree_order_128_test(Config) ->
    _BTree = ?config(btree_128, Config),
    _BTreeNew = ?config(btree_128_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 16
%%--------------------------------------------------------------------

map_b_tree_order_16_test(Config) ->
    _BTree = ?config(btree_16, Config),
    _BTreeNew = ?config(btree_16_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 256
%%--------------------------------------------------------------------

map_b_tree_order_256_test(Config) ->
    _BTree = ?config(btree_256, Config),
    _BTreeNew = ?config(btree_256_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 32
%%--------------------------------------------------------------------

map_b_tree_order_32_test(Config) ->
    _BTree = ?config(btree_32, Config),
    _BTreeNew = ?config(btree_32_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 4
%%--------------------------------------------------------------------

map_b_tree_order_4_test(Config) ->
    _BTree = ?config(btree_4, Config),
    _BTreeNew = ?config(btree_4_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 6
%%--------------------------------------------------------------------

map_b_tree_order_6_test(Config) ->
    _BTree = ?config(btree_6, Config),
    _BTreeNew = ?config(btree_6_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 512
%%--------------------------------------------------------------------

map_b_tree_order_612_test(Config) ->
    _BTree = ?config(btree_612, Config),
    _BTreeNew = ?config(btree_612_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 64
%%--------------------------------------------------------------------

map_b_tree_order_64_test(Config) ->
    _BTree = ?config(btree_64, Config),
    _BTreeNew = ?config(btree_64_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 8
%%--------------------------------------------------------------------

map_b_tree_order_8_test(Config) ->
    _BTree = ?config(btree_8, Config),
    _BTreeNew = ?config(btree_8_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map gb_tree
%%--------------------------------------------------------------------

map_gb_tree_test(_Config) ->
    ?assertEqual(?config(gbtree_new, _Config), gb_trees:map(fun map_value_to_new/2, ?config(gbtree, _Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 1024
%%--------------------------------------------------------------------

smallest_b_tree_order_1024_test(Config) ->
    _BTree = ?config(btree_1024, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 128
%%--------------------------------------------------------------------

smallest_b_tree_order_128_test(Config) ->
    _BTree = ?config(btree_128, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 16
%%--------------------------------------------------------------------

smallest_b_tree_order_16_test(Config) ->
    _BTree = ?config(btree_16, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 256
%%--------------------------------------------------------------------

smallest_b_tree_order_256_test(Config) ->
    _BTree = ?config(btree_256, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 32
%%--------------------------------------------------------------------

smallest_b_tree_order_32_test(Config) ->
    _BTree = ?config(btree_32, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 4
%%--------------------------------------------------------------------

smallest_b_tree_order_4_test(Config) ->
    _BTree = ?config(btree_4, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 6
%%--------------------------------------------------------------------

smallest_b_tree_order_6_test(Config) ->
    _BTree = ?config(btree_6, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 512
%%--------------------------------------------------------------------

smallest_b_tree_order_612_test(Config) ->
    _BTree = ?config(btree_612, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 64
%%--------------------------------------------------------------------

smallest_b_tree_order_64_test(Config) ->
    _BTree = ?config(btree_64, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 8
%%--------------------------------------------------------------------

smallest_b_tree_order_8_test(Config) ->
    _BTree = ?config(btree_8, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest gb_tree
%%--------------------------------------------------------------------

smallest_gb_tree_test(_Config) ->
    ?assertEqual(?SMALLEST_KEY_VALUE, gb_trees:smallest(?config(gbtree, _Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 1024
%%--------------------------------------------------------------------

take_largest_b_tree_order_1024_test(_Config) ->
    ?assertEqual(b_trees:empty(1024), test_generator:take_largest_b_tree(1024, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 128
%%--------------------------------------------------------------------

take_largest_b_tree_order_128_test(_Config) ->
    ?assertEqual(b_trees:empty(128), test_generator:take_largest_b_tree(128, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 16
%%--------------------------------------------------------------------

take_largest_b_tree_order_16_test(_Config) ->
    ?assertEqual(b_trees:empty(16), test_generator:take_largest_b_tree(16, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 256
%%--------------------------------------------------------------------

take_largest_b_tree_order_256_test(_Config) ->
    ?assertEqual(b_trees:empty(256), test_generator:take_largest_b_tree(256, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 32
%%--------------------------------------------------------------------

take_largest_b_tree_order_32_test(_Config) ->
    ?assertEqual(b_trees:empty(32), test_generator:take_largest_b_tree(32, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 4
%%--------------------------------------------------------------------

take_largest_b_tree_order_4_test(_Config) ->
    ?assertEqual(b_trees:empty(4), test_generator:take_largest_b_tree(4, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 6
%%--------------------------------------------------------------------

take_largest_b_tree_order_6_test(_Config) ->
    ?assertEqual(b_trees:empty(6), test_generator:take_largest_b_tree(6, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 512
%%--------------------------------------------------------------------

take_largest_b_tree_order_612_test(_Config) ->
    ?assertEqual(b_trees:empty(512), test_generator:take_largest_b_tree(512, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 64
%%--------------------------------------------------------------------

take_largest_b_tree_order_64_test(_Config) ->
    ?assertEqual(b_trees:empty(64), test_generator:take_largest_b_tree(64, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 8
%%--------------------------------------------------------------------

take_largest_b_tree_order_8_test(_Config) ->
    ?assertEqual(b_trees:empty(8), test_generator:take_largest_b_tree(8, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest gb_tree
%%--------------------------------------------------------------------

take_largest_gb_tree_test(_Config) ->
    ?assertEqual(gb_trees:empty(), test_generator:take_largest_gb_tree(?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 1024
%%--------------------------------------------------------------------

take_smallest_b_tree_order_1024_test(_Config) ->
    ?assertEqual(b_trees:empty(1024), test_generator:take_smallest_b_tree(1024, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 128
%%--------------------------------------------------------------------

take_smallest_b_tree_order_128_test(_Config) ->
    ?assertEqual(b_trees:empty(128), test_generator:take_smallest_b_tree(128, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 16
%%--------------------------------------------------------------------

take_smallest_b_tree_order_16_test(_Config) ->
    ?assertEqual(b_trees:empty(16), test_generator:take_smallest_b_tree(16, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 256
%%--------------------------------------------------------------------

take_smallest_b_tree_order_256_test(_Config) ->
    ?assertEqual(b_trees:empty(256), test_generator:take_smallest_b_tree(256, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 32
%%--------------------------------------------------------------------

take_smallest_b_tree_order_32_test(_Config) ->
    ?assertEqual(b_trees:empty(32), test_generator:take_smallest_b_tree(32, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 4
%%--------------------------------------------------------------------

take_smallest_b_tree_order_4_test(_Config) ->
    ?assertEqual(b_trees:empty(4), test_generator:take_smallest_b_tree(4, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 6
%%--------------------------------------------------------------------

take_smallest_b_tree_order_6_test(_Config) ->
    ?assertEqual(b_trees:empty(6), test_generator:take_smallest_b_tree(6, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 512
%%--------------------------------------------------------------------

take_smallest_b_tree_order_612_test(_Config) ->
    ?assertEqual(b_trees:empty(512), test_generator:take_smallest_b_tree(512, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 64
%%--------------------------------------------------------------------

take_smallest_b_tree_order_64_test(_Config) ->
    ?assertEqual(b_trees:empty(64), test_generator:take_smallest_b_tree(64, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 8
%%--------------------------------------------------------------------

take_smallest_b_tree_order_8_test(_Config) ->
    ?assertEqual(b_trees:empty(8), test_generator:take_smallest_b_tree(8, ?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest gb_tree
%%--------------------------------------------------------------------

take_smallest_gb_tree_test(_Config) ->
    ?assertEqual(gb_trees:empty(), test_generator:take_smallest_gb_tree(?NUMBER_DELETES, 4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 1024
%%--------------------------------------------------------------------

to_list_b_tree_order_1024_test(Config) ->
    BTree = ?config(btree_1024, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 128
%%--------------------------------------------------------------------

to_list_b_tree_order_128_test(Config) ->
    BTree = ?config(btree_128, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 16
%%--------------------------------------------------------------------

to_list_b_tree_order_16_test(Config) ->
    BTree = ?config(btree_16, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 256
%%--------------------------------------------------------------------

to_list_b_tree_order_256_test(Config) ->
    BTree = ?config(btree_256, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 32
%%--------------------------------------------------------------------

to_list_b_tree_order_32_test(Config) ->
    BTree = ?config(btree_32, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 4
%%--------------------------------------------------------------------

to_list_b_tree_order_4_test(Config) ->
    BTree = ?config(btree_4, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 6
%%--------------------------------------------------------------------

to_list_b_tree_order_6_test(Config) ->
    BTree = ?config(btree_6, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 512
%%--------------------------------------------------------------------

to_list_b_tree_order_612_test(Config) ->
    BTree = ?config(btree_612, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 64
%%--------------------------------------------------------------------

to_list_b_tree_order_64_test(Config) ->
    BTree = ?config(btree_64, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 8
%%--------------------------------------------------------------------

to_list_b_tree_order_8_test(Config) ->
    BTree = ?config(btree_8, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list gb_tree
%%--------------------------------------------------------------------

to_list_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    _KeyValues = gb_trees:to_list(GBTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 1024
%%--------------------------------------------------------------------

update_b_tree_order_1024_test(Config) ->
    _BTree = ?config(btree_1024, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_1024_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

update_b_tree([], BTree) ->
    BTree;
update_b_tree([{Key, Value} | Tail], BTree) ->
    update_b_tree(Tail, b_trees:update(Key, Value, BTree)).

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 128
%%--------------------------------------------------------------------

update_b_tree_order_128_test(Config) ->
    _BTree = ?config(btree_128, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_128_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 16
%%--------------------------------------------------------------------

update_b_tree_order_16_test(Config) ->
    _BTree = ?config(btree_16, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_16_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 256
%%--------------------------------------------------------------------

update_b_tree_order_256_test(Config) ->
    _BTree = ?config(btree_256, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_256_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 32
%%--------------------------------------------------------------------

update_b_tree_order_32_test(Config) ->
    _BTree = ?config(btree_32, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_32_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 4
%%--------------------------------------------------------------------

update_b_tree_order_4_test(Config) ->
    _BTree = ?config(btree_4, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_4_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 6
%%--------------------------------------------------------------------

update_b_tree_order_6_test(Config) ->
    _BTree = ?config(btree_6, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_6_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 512
%%--------------------------------------------------------------------

update_b_tree_order_612_test(Config) ->
    _BTree = ?config(btree_612, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_612_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 64
%%--------------------------------------------------------------------

update_b_tree_order_64_test(Config) ->
    _BTree = ?config(btree_64, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_64_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 8
%%--------------------------------------------------------------------

update_b_tree_order_8_test(Config) ->
    _BTree = ?config(btree_8, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_8_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update gb_tree
%%--------------------------------------------------------------------

update_gb_tree_test(Config) ->
    _GBTree = ?config(gbtree, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(gbtree_new, Config), update_gb_tree(_KeyValuesUpdate, _GBTree)),
    ok.

update_gb_tree([], GBTree) ->
    GBTree;
update_gb_tree([{Key, Value} | Tail], GBTree) ->
    update_gb_tree(Tail, gb_trees:update(Key, Value, GBTree)).

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 1024
%%--------------------------------------------------------------------

values_b_tree_order_1024_test(Config) ->
    BTree = ?config(btree_1024, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 128
%%--------------------------------------------------------------------

values_b_tree_order_128_test(Config) ->
    BTree = ?config(btree_128, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 16
%%--------------------------------------------------------------------

values_b_tree_order_16_test(Config) ->
    BTree = ?config(btree_16, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 256
%%--------------------------------------------------------------------

values_b_tree_order_256_test(Config) ->
    BTree = ?config(btree_256, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 32
%%--------------------------------------------------------------------

values_b_tree_order_32_test(Config) ->
    BTree = ?config(btree_32, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 4
%%--------------------------------------------------------------------

values_b_tree_order_4_test(Config) ->
    BTree = ?config(btree_4, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 6
%%--------------------------------------------------------------------

values_b_tree_order_6_test(Config) ->
    BTree = ?config(btree_6, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 512
%%--------------------------------------------------------------------

values_b_tree_order_612_test(Config) ->
    BTree = ?config(btree_612, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 64
%%--------------------------------------------------------------------

values_b_tree_order_64_test(Config) ->
    BTree = ?config(btree_64, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 8
%%--------------------------------------------------------------------

values_b_tree_order_8_test(Config) ->
    BTree = ?config(btree_8, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values gb_tree
%%--------------------------------------------------------------------

values_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    _Keys = gb_trees:values(GBTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.
