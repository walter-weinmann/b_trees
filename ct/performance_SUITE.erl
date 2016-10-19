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

-define(NODEBUG, true).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LARGEST_KEY_VALUE, {"k_2000", "v_2000"}).
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
        {gbtree, test_generator:generate_gb_tree_from_number(?NUMBER_INSERTS, 4)},
        {btree_5, test_generator:generate_b_tree_from_number(5, ?NUMBER_INSERTS, 4)},
        {btree_9, test_generator:generate_b_tree_from_number(9, ?NUMBER_INSERTS, 4)},
        {btree_17, test_generator:generate_b_tree_from_number(17, ?NUMBER_INSERTS, 4)},
        {btree_33, test_generator:generate_b_tree_from_number(33, ?NUMBER_INSERTS, 4)},
        {btree_65, test_generator:generate_b_tree_from_number(65, ?NUMBER_INSERTS, 4)},
        {btree_129, test_generator:generate_b_tree_from_number(129, ?NUMBER_INSERTS, 4)},
        {btree_257, test_generator:generate_b_tree_from_number(257, ?NUMBER_INSERTS, 4)},
        {btree_513, test_generator:generate_b_tree_from_number(513, ?NUMBER_INSERTS, 4)},
        {btree_1025, test_generator:generate_b_tree_from_number(1025, ?NUMBER_INSERTS, 4)},
        {key_values, test_generator:generate_key_values_from(?NUMBER_INSERTS, 4)},
        {key_values_update, test_generator:generate_key_values_rand_update(?NUMBER_UPDATES, ?NUMBER_UPDATES, 4)},
        {keys, test_generator:generate_keys_rand(?NUMBER_INSERTS, ?NUMBER_KEYS, 4)}
        | Config
    ].

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%--------------------------------------------------------------------

all() ->
    [
        performance_enter_b_tree_order_5_test,
        performance_enter_b_tree_order_9_test,
        performance_enter_b_tree_order_17_test,
        performance_enter_b_tree_order_33_test,
        performance_enter_b_tree_order_65_test,
        performance_enter_b_tree_order_129_test,
        performance_enter_b_tree_order_257_test,
        performance_enter_b_tree_order_513_test,
        performance_enter_b_tree_order_1025_test,
        performance_enter_gb_tree_test,

        performance_from_dict_b_tree_order_5_test,
        performance_from_dict_b_tree_order_9_test,
        performance_from_dict_b_tree_order_17_test,
        performance_from_dict_b_tree_order_33_test,
        performance_from_dict_b_tree_order_65_test,
        performance_from_dict_b_tree_order_129_test,
        performance_from_dict_b_tree_order_257_test,
        performance_from_dict_b_tree_order_513_test,
        performance_from_dict_b_tree_order_1025_test,
        performance_from_dict_gb_tree_test,

        performance_get_b_tree_order_5_test,
        performance_get_b_tree_order_9_test,
        performance_get_b_tree_order_17_test,
        performance_get_b_tree_order_33_test,
        performance_get_b_tree_order_65_test,
        performance_get_b_tree_order_129_test,
        performance_get_b_tree_order_257_test,
        performance_get_b_tree_order_513_test,
        performance_get_b_tree_order_1025_test,
        performance_get_gb_tree_test,

        performance_insert_b_tree_order_5_test,
        performance_insert_b_tree_order_9_test,
        performance_insert_b_tree_order_17_test,
        performance_insert_b_tree_order_33_test,
        performance_insert_b_tree_order_65_test,
        performance_insert_b_tree_order_129_test,
        performance_insert_b_tree_order_257_test,
        performance_insert_b_tree_order_513_test,
        performance_insert_b_tree_order_1025_test,
        performance_insert_gb_tree_test,

        performance_is_defined_b_tree_order_5_test,
        performance_is_defined_b_tree_order_9_test,
        performance_is_defined_b_tree_order_17_test,
        performance_is_defined_b_tree_order_33_test,
        performance_is_defined_b_tree_order_65_test,
        performance_is_defined_b_tree_order_129_test,
        performance_is_defined_b_tree_order_257_test,
        performance_is_defined_b_tree_order_513_test,
        performance_is_defined_b_tree_order_1025_test,
        performance_is_defined_gb_tree_test,

        performance_keys_b_tree_order_5_test,
        performance_keys_b_tree_order_9_test,
        performance_keys_b_tree_order_17_test,
        performance_keys_b_tree_order_33_test,
        performance_keys_b_tree_order_65_test,
        performance_keys_b_tree_order_129_test,
        performance_keys_b_tree_order_257_test,
        performance_keys_b_tree_order_513_test,
        performance_keys_b_tree_order_1025_test,
        performance_keys_gb_tree_test,

        performance_largest_b_tree_order_5_test,
        performance_largest_b_tree_order_9_test,
        performance_largest_b_tree_order_17_test,
        performance_largest_b_tree_order_33_test,
        performance_largest_b_tree_order_65_test,
        performance_largest_b_tree_order_129_test,
        performance_largest_b_tree_order_257_test,
        performance_largest_b_tree_order_513_test,
        performance_largest_b_tree_order_1025_test,
        performance_largest_gb_tree_test,

        performance_lookup_b_tree_order_5_test,
        performance_lookup_b_tree_order_9_test,
        performance_lookup_b_tree_order_17_test,
        performance_lookup_b_tree_order_33_test,
        performance_lookup_b_tree_order_65_test,
        performance_lookup_b_tree_order_129_test,
        performance_lookup_b_tree_order_257_test,
        performance_lookup_b_tree_order_513_test,
        performance_lookup_b_tree_order_1025_test,
        performance_lookup_gb_tree_test,

        performance_smallest_b_tree_order_5_test,
        performance_smallest_b_tree_order_9_test,
        performance_smallest_b_tree_order_17_test,
        performance_smallest_b_tree_order_33_test,
        performance_smallest_b_tree_order_65_test,
        performance_smallest_b_tree_order_129_test,
        performance_smallest_b_tree_order_257_test,
        performance_smallest_b_tree_order_513_test,
        performance_smallest_b_tree_order_1025_test,
        performance_smallest_gb_tree_test,

        performance_to_list_b_tree_order_5_test,
        performance_to_list_b_tree_order_9_test,
        performance_to_list_b_tree_order_17_test,
        performance_to_list_b_tree_order_33_test,
        performance_to_list_b_tree_order_65_test,
        performance_to_list_b_tree_order_129_test,
        performance_to_list_b_tree_order_257_test,
        performance_to_list_b_tree_order_513_test,
        performance_to_list_b_tree_order_1025_test,
        performance_to_list_gb_tree_test,

        performance_update_b_tree_order_5_test,
        performance_update_b_tree_order_9_test,
        performance_update_b_tree_order_17_test,
        performance_update_b_tree_order_33_test,
        performance_update_b_tree_order_65_test,
        performance_update_b_tree_order_129_test,
        performance_update_b_tree_order_257_test,
        performance_update_b_tree_order_513_test,
        performance_update_b_tree_order_1025_test,
        performance_update_gb_tree_test,

        performance_values_b_tree_order_5_test,
        performance_values_b_tree_order_9_test,
        performance_values_b_tree_order_17_test,
        performance_values_b_tree_order_33_test,
        performance_values_b_tree_order_65_test,
        performance_values_b_tree_order_129_test,
        performance_values_b_tree_order_257_test,
        performance_values_b_tree_order_513_test,
        performance_values_b_tree_order_1025_test,
        performance_values_gb_tree_test
    ].

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 1025
%%--------------------------------------------------------------------

performance_enter_b_tree_order_1025_test(Config) ->
    BTree = b_trees:empty(1025),
    KeyValues = ?config(key_values, Config),
    enter_b_tree(KeyValues, BTree),
    KeyValuesUpdate = ?config(key_values_update, Config),
    enter_b_tree(KeyValuesUpdate, BTree),
    ok.

enter_b_tree([], _) ->
    none;
enter_b_tree([{Key, Value} | Tail], BTree) ->
    b_trees:enter(Key, Value, BTree),
    enter_b_tree(Tail, BTree).

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 129
%%--------------------------------------------------------------------

performance_enter_b_tree_order_129_test(Config) ->
    BTree = b_trees:empty(129),
    KeyValues = ?config(key_values, Config),
    enter_b_tree(KeyValues, BTree),
    KeyValuesUpdate = ?config(key_values_update, Config),
    enter_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 17
%%--------------------------------------------------------------------

performance_enter_b_tree_order_17_test(Config) ->
    BTree = b_trees:empty(17),
    KeyValues = ?config(key_values, Config),
    enter_b_tree(KeyValues, BTree),
    KeyValuesUpdate = ?config(key_values_update, Config),
    enter_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 257
%%--------------------------------------------------------------------

performance_enter_b_tree_order_257_test(Config) ->
    BTree = b_trees:empty(257),
    KeyValues = ?config(key_values, Config),
    enter_b_tree(KeyValues, BTree),
    KeyValuesUpdate = ?config(key_values_update, Config),
    enter_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 33
%%--------------------------------------------------------------------

performance_enter_b_tree_order_33_test(Config) ->
    BTree = b_trees:empty(33),
    KeyValues = ?config(key_values, Config),
    enter_b_tree(KeyValues, BTree),
    KeyValuesUpdate = ?config(key_values_update, Config),
    enter_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 5
%%--------------------------------------------------------------------

performance_enter_b_tree_order_5_test(Config) ->
    BTree = b_trees:empty(5),
    KeyValues = ?config(key_values, Config),
    enter_b_tree(KeyValues, BTree),
    KeyValuesUpdate = ?config(key_values_update, Config),
    enter_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 513
%%--------------------------------------------------------------------

performance_enter_b_tree_order_513_test(Config) ->
    BTree = b_trees:empty(513),
    KeyValues = ?config(key_values, Config),
    enter_b_tree(KeyValues, BTree),
    KeyValuesUpdate = ?config(key_values_update, Config),
    enter_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 65
%%--------------------------------------------------------------------

performance_enter_b_tree_order_65_test(Config) ->
    BTree = b_trees:empty(65),
    KeyValues = ?config(key_values, Config),
    enter_b_tree(KeyValues, BTree),
    KeyValuesUpdate = ?config(key_values_update, Config),
    enter_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 9
%%--------------------------------------------------------------------

performance_enter_b_tree_order_9_test(Config) ->
    BTree = b_trees:empty(9),
    KeyValues = ?config(key_values, Config),
    enter_b_tree(KeyValues, BTree),
    KeyValuesUpdate = ?config(key_values_update, Config),
    enter_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter gb_tree
%%--------------------------------------------------------------------

performance_enter_gb_tree_test(Config) ->
    GBTree = gb_trees:empty(),
    KeyValues = ?config(key_values, Config),
    enter_gb_tree(KeyValues, GBTree),
    KeyValuesUpdate = ?config(key_values_update, Config),
    enter_gb_tree(KeyValuesUpdate, GBTree),
    ok.

enter_gb_tree([], _) ->
    none;
enter_gb_tree([{Key, Value} | Tail], GBTree) ->
    gb_trees:enter(Key, Value, GBTree),
    enter_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 1025
%%--------------------------------------------------------------------

performance_from_dict_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    KeyValues = ?config(key_values, Config),
    ?assertEqual(BTree, b_trees:from_dict(1025, KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 129
%%--------------------------------------------------------------------

performance_from_dict_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    KeyValues = ?config(key_values, Config),
    ?assertEqual(BTree, b_trees:from_dict(129, KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 17
%%--------------------------------------------------------------------

performance_from_dict_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    KeyValues = ?config(key_values, Config),
    ?assertEqual(BTree, b_trees:from_dict(17, KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 257
%%--------------------------------------------------------------------

performance_from_dict_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    KeyValues = ?config(key_values, Config),
    ?assertEqual(BTree, b_trees:from_dict(257, KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 33
%%--------------------------------------------------------------------

performance_from_dict_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    KeyValues = ?config(key_values, Config),
    ?assertEqual(BTree, b_trees:from_dict(33, KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 5
%%--------------------------------------------------------------------

performance_from_dict_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    KeyValues = ?config(key_values, Config),
    ?assertEqual(BTree, b_trees:from_dict(5, KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 513
%%--------------------------------------------------------------------

performance_from_dict_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    KeyValues = ?config(key_values, Config),
    ?assertEqual(BTree, b_trees:from_dict(513, KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 65
%%--------------------------------------------------------------------

performance_from_dict_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    KeyValues = ?config(key_values, Config),
    ?assertEqual(BTree, b_trees:from_dict(65, KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 9
%%--------------------------------------------------------------------

performance_from_dict_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    KeyValues = ?config(key_values, Config),
    ?assertEqual(BTree, b_trees:from_dict(9, KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict gb_tree
%%--------------------------------------------------------------------

performance_from_dict_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    KeyValues = ?config(key_values, Config),
    gb_trees:from_orddict(KeyValues),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 1025
%%--------------------------------------------------------------------

performance_get_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    Keys = ?config(keys, Config),
    get_b_tree(Keys, BTree),
    ok.

get_b_tree([], _) ->
    none;
get_b_tree([Key | Tail], BTree) ->
    b_trees:get(Key, BTree),
    get_b_tree(Tail, BTree).

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 129
%%--------------------------------------------------------------------

performance_get_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    Keys = ?config(keys, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 17
%%--------------------------------------------------------------------

performance_get_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    Keys = ?config(keys, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 257
%%--------------------------------------------------------------------

performance_get_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    Keys = ?config(keys, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 33
%%--------------------------------------------------------------------

performance_get_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    Keys = ?config(keys, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 5
%%--------------------------------------------------------------------

performance_get_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    Keys = ?config(keys, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 513
%%--------------------------------------------------------------------

performance_get_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    Keys = ?config(keys, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 65
%%--------------------------------------------------------------------

performance_get_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    Keys = ?config(keys, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 9
%%--------------------------------------------------------------------

performance_get_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    Keys = ?config(keys, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get gb_tree
%%--------------------------------------------------------------------

performance_get_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    Keys = ?config(keys, Config),
    get_gb_tree(Keys, GBTree),
    ok.

get_gb_tree([], _) ->
    none;
get_gb_tree([Key | Tail], GBTree) ->
    gb_trees:get(Key, GBTree),
    get_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 1025
%%--------------------------------------------------------------------

performance_insert_b_tree_order_1025_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(1025, ?NUMBER_INSERTS, 4),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(1025 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 129
%%--------------------------------------------------------------------

performance_insert_b_tree_order_129_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(129, ?NUMBER_INSERTS, 4),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(129 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 17
%%--------------------------------------------------------------------

performance_insert_b_tree_order_17_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(17, ?NUMBER_INSERTS, 4),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(17 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 257
%%--------------------------------------------------------------------

performance_insert_b_tree_order_257_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(257, ?NUMBER_INSERTS, 4),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(257 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 33
%%--------------------------------------------------------------------

performance_insert_b_tree_order_33_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(33, ?NUMBER_INSERTS, 4),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(33 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 5
%%--------------------------------------------------------------------

performance_insert_b_tree_order_5_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(5, ?NUMBER_INSERTS, 4),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(5 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 513
%%--------------------------------------------------------------------

performance_insert_b_tree_order_513_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(513, ?NUMBER_INSERTS, 4),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(513 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 65
%%--------------------------------------------------------------------

performance_insert_b_tree_order_65_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(65, ?NUMBER_INSERTS, 4),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(65 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 9
%%--------------------------------------------------------------------

performance_insert_b_tree_order_9_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(9, ?NUMBER_INSERTS, 4),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(9 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert gb_tree
%%--------------------------------------------------------------------

performance_insert_gb_tree_test(_Config) ->
    test_generator:generate_gb_tree_from_number(?NUMBER_INSERTS, 4).

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 1025
%%--------------------------------------------------------------------

performance_is_defined_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    Keys = ?config(keys, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

is_defined_b_tree([], _) ->
    none;
is_defined_b_tree([Key | Tail], BTree) ->
    b_trees:is_defined(Key, BTree),
    is_defined_b_tree(Tail, BTree).

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 129
%%--------------------------------------------------------------------

performance_is_defined_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    Keys = ?config(keys, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 17
%%--------------------------------------------------------------------

performance_is_defined_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    Keys = ?config(keys, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 257
%%--------------------------------------------------------------------

performance_is_defined_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    Keys = ?config(keys, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 33
%%--------------------------------------------------------------------

performance_is_defined_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    Keys = ?config(keys, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 5
%%--------------------------------------------------------------------

performance_is_defined_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    Keys = ?config(keys, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 513
%%--------------------------------------------------------------------

performance_is_defined_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    Keys = ?config(keys, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 65
%%--------------------------------------------------------------------

performance_is_defined_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    Keys = ?config(keys, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 9
%%--------------------------------------------------------------------

performance_is_defined_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    Keys = ?config(keys, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined gb_tree
%%--------------------------------------------------------------------

performance_is_defined_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    Keys = ?config(keys, Config),
    is_defined_gb_tree(Keys, GBTree),
    ok.

is_defined_gb_tree([], _) ->
    none;
is_defined_gb_tree([Key | Tail], GBTree) ->
    gb_trees:is_defined(Key, GBTree),
    is_defined_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: performance keys b_tree order 1025
%%--------------------------------------------------------------------

performance_keys_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    Keys = b_trees:keys(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys b_tree order 129
%%--------------------------------------------------------------------

performance_keys_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    Keys = b_trees:keys(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys b_tree order 17
%%--------------------------------------------------------------------

performance_keys_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    Keys = b_trees:keys(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys b_tree order 257
%%--------------------------------------------------------------------

performance_keys_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    Keys = b_trees:keys(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys b_tree order 33
%%--------------------------------------------------------------------

performance_keys_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    Keys = b_trees:keys(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys b_tree order 5
%%--------------------------------------------------------------------

performance_keys_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    Keys = b_trees:keys(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys b_tree order 513
%%--------------------------------------------------------------------

performance_keys_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    Keys = b_trees:keys(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys b_tree order 65
%%--------------------------------------------------------------------

performance_keys_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    Keys = b_trees:keys(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys b_tree order 9
%%--------------------------------------------------------------------

performance_keys_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    Keys = b_trees:keys(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys gb_tree
%%--------------------------------------------------------------------

performance_keys_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    Keys = gb_trees:keys(GBTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 1025
%%--------------------------------------------------------------------

performance_largest_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 129
%%--------------------------------------------------------------------

performance_largest_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 17
%%--------------------------------------------------------------------

performance_largest_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 257
%%--------------------------------------------------------------------

performance_largest_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 33
%%--------------------------------------------------------------------

performance_largest_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 5
%%--------------------------------------------------------------------

performance_largest_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 513
%%--------------------------------------------------------------------

performance_largest_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 65
%%--------------------------------------------------------------------

performance_largest_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 9
%%--------------------------------------------------------------------

performance_largest_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest gb_tree
%%--------------------------------------------------------------------

performance_largest_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, gb_trees:largest(GBTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 1025
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    Keys = ?config(keys, Config),
    lookup_b_tree(Keys, BTree),
    ok.

lookup_b_tree([], _) ->
    none;
lookup_b_tree([Key | Tail], BTree) ->
    b_trees:lookup(Key, BTree),
    lookup_b_tree(Tail, BTree).

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 129
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    Keys = ?config(keys, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 17
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    Keys = ?config(keys, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 257
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    Keys = ?config(keys, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 33
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    Keys = ?config(keys, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 5
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    Keys = ?config(keys, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 513
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    Keys = ?config(keys, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 65
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    Keys = ?config(keys, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 9
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    Keys = ?config(keys, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup gb_tree
%%--------------------------------------------------------------------

performance_lookup_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    Keys = ?config(keys, Config),
    lookup_gb_tree(Keys, GBTree),
    ok.

lookup_gb_tree([], _) ->
    none;
lookup_gb_tree([Key | Tail], GBTree) ->
    gb_trees:lookup(Key, GBTree),
    lookup_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 1025
%%--------------------------------------------------------------------

performance_smallest_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 129
%%--------------------------------------------------------------------

performance_smallest_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 17
%%--------------------------------------------------------------------

performance_smallest_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 257
%%--------------------------------------------------------------------

performance_smallest_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 33
%%--------------------------------------------------------------------

performance_smallest_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 5
%%--------------------------------------------------------------------

performance_smallest_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 513
%%--------------------------------------------------------------------

performance_smallest_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 65
%%--------------------------------------------------------------------

performance_smallest_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 9
%%--------------------------------------------------------------------

performance_smallest_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest gb_tree
%%--------------------------------------------------------------------

performance_smallest_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, gb_trees:smallest(GBTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 1025
%%--------------------------------------------------------------------

performance_to_list_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 129
%%--------------------------------------------------------------------

performance_to_list_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 17
%%--------------------------------------------------------------------

performance_to_list_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 257
%%--------------------------------------------------------------------

performance_to_list_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 33
%%--------------------------------------------------------------------

performance_to_list_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 5
%%--------------------------------------------------------------------

performance_to_list_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 513
%%--------------------------------------------------------------------

performance_to_list_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 65
%%--------------------------------------------------------------------

performance_to_list_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 9
%%--------------------------------------------------------------------

performance_to_list_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list gb_tree
%%--------------------------------------------------------------------

performance_to_list_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    KeyValues = gb_trees:to_list(GBTree),
    ?assertEqual(?NUMBER_INSERTS, length(KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 1025
%%--------------------------------------------------------------------

performance_update_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    KeyValuesUpdate = ?config(key_values_update, Config),
    update_b_tree(KeyValuesUpdate, BTree),
    ok.

update_b_tree([], _) ->
    none;
update_b_tree([{Key, Value} | Tail], BTree) ->
    b_trees:update(Key, Value, BTree),
    update_b_tree(Tail, BTree).

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 129
%%--------------------------------------------------------------------

performance_update_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    KeyValuesUpdate = ?config(key_values_update, Config),
    update_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 17
%%--------------------------------------------------------------------

performance_update_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    KeyValuesUpdate = ?config(key_values_update, Config),
    update_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 257
%%--------------------------------------------------------------------

performance_update_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    KeyValuesUpdate = ?config(key_values_update, Config),
    update_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 33
%%--------------------------------------------------------------------

performance_update_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    KeyValuesUpdate = ?config(key_values_update, Config),
    update_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 5
%%--------------------------------------------------------------------

performance_update_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    KeyValuesUpdate = ?config(key_values_update, Config),
    update_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 513
%%--------------------------------------------------------------------

performance_update_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    KeyValuesUpdate = ?config(key_values_update, Config),
    update_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 65
%%--------------------------------------------------------------------

performance_update_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    KeyValuesUpdate = ?config(key_values_update, Config),
    update_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 9
%%--------------------------------------------------------------------

performance_update_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    KeyValuesUpdate = ?config(key_values_update, Config),
    update_b_tree(KeyValuesUpdate, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update gb_tree
%%--------------------------------------------------------------------

performance_update_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    KeyValuesUpdate = ?config(key_values_update, Config),
    update_gb_tree(KeyValuesUpdate, GBTree),
    ok.

update_gb_tree([], _) ->
    none;
update_gb_tree([{Key, Value} | Tail], GBTree) ->
    gb_trees:update(Key, Value, GBTree),
    update_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 1025
%%--------------------------------------------------------------------

performance_values_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 129
%%--------------------------------------------------------------------

performance_values_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 17
%%--------------------------------------------------------------------

performance_values_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 257
%%--------------------------------------------------------------------

performance_values_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 33
%%--------------------------------------------------------------------

performance_values_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 5
%%--------------------------------------------------------------------

performance_values_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 513
%%--------------------------------------------------------------------

performance_values_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 65
%%--------------------------------------------------------------------

performance_values_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 9
%%--------------------------------------------------------------------

performance_values_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values gb_tree
%%--------------------------------------------------------------------

performance_values_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    Keys = gb_trees:values(GBTree),
    ?assertEqual(?NUMBER_INSERTS, length(Keys)),
    ok.

%%--------------------------------------------------------------------
%% Helper functions.
%%--------------------------------------------------------------------

int_ceil(X) ->
    T = trunc(X),
    if
        X > T -> T + 1;
        true -> T
    end.
