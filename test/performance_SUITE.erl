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
        {key_values_from, test_generator:generate_key_values_from(?NUMBER_INSERTS, 4)},
        {key_values_from_update, test_generator:generate_key_values_from_update(?NUMBER_INSERTS, 4)},
        {key_values_rand_update, test_generator:generate_key_values_rand_update(?NUMBER_UPDATES, ?NUMBER_UPDATES, 4)},
        {keys_from, test_generator:generate_keys_from(?NUMBER_INSERTS, 4)},
        {keys_rand, test_generator:generate_keys_rand(?NUMBER_INSERTS, ?NUMBER_KEYS, 4)},

        {gbtree, test_generator:generate_gb_tree_from_number(?NUMBER_INSERTS, 4)},
        {gbtree_new, test_generator:generate_gb_tree_from_number_update(?NUMBER_INSERTS, 4)},

        {btree_5, test_generator:generate_b_tree_from_number(5, ?NUMBER_INSERTS, 4)},
        {btree_5_new, test_generator:generate_b_tree_from_number_update(5, ?NUMBER_INSERTS, 4)},
        {btree_5_till, test_generator:generate_b_tree_till_number(5, ?NUMBER_INSERTS, 4)},
        {btree_9, test_generator:generate_b_tree_from_number(9, ?NUMBER_INSERTS, 4)},
        {btree_9_new, test_generator:generate_b_tree_from_number_update(9, ?NUMBER_INSERTS, 4)},
        {btree_17, test_generator:generate_b_tree_from_number(17, ?NUMBER_INSERTS, 4)},
        {btree_17_new, test_generator:generate_b_tree_from_number_update(17, ?NUMBER_INSERTS, 4)},
        {btree_33, test_generator:generate_b_tree_from_number(33, ?NUMBER_INSERTS, 4)},
        {btree_33_new, test_generator:generate_b_tree_from_number_update(33, ?NUMBER_INSERTS, 4)},
        {btree_65, test_generator:generate_b_tree_from_number(65, ?NUMBER_INSERTS, 4)},
        {btree_65_new, test_generator:generate_b_tree_from_number_update(65, ?NUMBER_INSERTS, 4)},
        {btree_129, test_generator:generate_b_tree_from_number(129, ?NUMBER_INSERTS, 4)},
        {btree_129_new, test_generator:generate_b_tree_from_number_update(129, ?NUMBER_INSERTS, 4)},
        {btree_257, test_generator:generate_b_tree_from_number(257, ?NUMBER_INSERTS, 4)},
        {btree_257_new, test_generator:generate_b_tree_from_number_update(257, ?NUMBER_INSERTS, 4)},
        {btree_513, test_generator:generate_b_tree_from_number(513, ?NUMBER_INSERTS, 4)},
        {btree_513_new, test_generator:generate_b_tree_from_number_update(513, ?NUMBER_INSERTS, 4)},
        {btree_1025, test_generator:generate_b_tree_from_number(1025, ?NUMBER_INSERTS, 4)},
        {btree_1025_new, test_generator:generate_b_tree_from_number_update(1025, ?NUMBER_INSERTS, 4)}
        | Config
    ].

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%--------------------------------------------------------------------

all() ->
    [
        enter_b_tree_order_5_test,
        enter_b_tree_order_9_test,
        enter_b_tree_order_17_test,
        enter_b_tree_order_33_test,
        enter_b_tree_order_65_test,
        enter_b_tree_order_129_test,
        enter_b_tree_order_257_test,
        enter_b_tree_order_513_test,
        enter_b_tree_order_1025_test,
        enter_gb_tree_test,

        from_dict_b_tree_order_5_test,
        from_dict_b_tree_order_9_test,
        from_dict_b_tree_order_17_test,
        from_dict_b_tree_order_33_test,
        from_dict_b_tree_order_65_test,
        from_dict_b_tree_order_129_test,
        from_dict_b_tree_order_257_test,
        from_dict_b_tree_order_513_test,
        from_dict_b_tree_order_1025_test,
        from_dict_gb_tree_test,

        get_b_tree_order_5_test,
        get_b_tree_order_9_test,
        get_b_tree_order_17_test,
        get_b_tree_order_33_test,
        get_b_tree_order_65_test,
        get_b_tree_order_129_test,
        get_b_tree_order_257_test,
        get_b_tree_order_513_test,
        get_b_tree_order_1025_test,
        get_gb_tree_test,

        insert_b_tree_order_5_test,
        insert_b_tree_order_5_till_test,
        insert_b_tree_order_9_test,
        insert_b_tree_order_17_test,
        insert_b_tree_order_33_test,
        insert_b_tree_order_65_test,
        insert_b_tree_order_129_test,
        insert_b_tree_order_257_test,
        insert_b_tree_order_513_test,
        insert_b_tree_order_1025_test,
        insert_gb_tree_test,

        is_defined_b_tree_order_5_test,
        is_defined_b_tree_order_9_test,
        is_defined_b_tree_order_17_test,
        is_defined_b_tree_order_33_test,
        is_defined_b_tree_order_65_test,
        is_defined_b_tree_order_129_test,
        is_defined_b_tree_order_257_test,
        is_defined_b_tree_order_513_test,
        is_defined_b_tree_order_1025_test,
        is_defined_gb_tree_test,

        keys_b_tree_order_5_test,
        keys_b_tree_order_9_test,
        keys_b_tree_order_17_test,
        keys_b_tree_order_33_test,
        keys_b_tree_order_65_test,
        keys_b_tree_order_129_test,
        keys_b_tree_order_257_test,
        keys_b_tree_order_513_test,
        keys_b_tree_order_1025_test,
        keys_gb_tree_test,

        largest_b_tree_order_5_test,
        largest_b_tree_order_9_test,
        largest_b_tree_order_17_test,
        largest_b_tree_order_33_test,
        largest_b_tree_order_65_test,
        largest_b_tree_order_129_test,
        largest_b_tree_order_257_test,
        largest_b_tree_order_513_test,
        largest_b_tree_order_1025_test,
        largest_gb_tree_test,

        lookup_b_tree_order_5_test,
        lookup_b_tree_order_9_test,
        lookup_b_tree_order_17_test,
        lookup_b_tree_order_33_test,
        lookup_b_tree_order_65_test,
        lookup_b_tree_order_129_test,
        lookup_b_tree_order_257_test,
        lookup_b_tree_order_513_test,
        lookup_b_tree_order_1025_test,
        lookup_gb_tree_test,

        map_b_tree_order_5_test,
        map_b_tree_order_9_test,
        map_b_tree_order_17_test,
        map_b_tree_order_33_test,
        map_b_tree_order_65_test,
        map_b_tree_order_129_test,
        map_b_tree_order_257_test,
        map_b_tree_order_513_test,
        map_b_tree_order_1025_test,
        map_gb_tree_test,

        smallest_b_tree_order_5_test,
        smallest_b_tree_order_9_test,
        smallest_b_tree_order_17_test,
        smallest_b_tree_order_33_test,
        smallest_b_tree_order_65_test,
        smallest_b_tree_order_129_test,
        smallest_b_tree_order_257_test,
        smallest_b_tree_order_513_test,
        smallest_b_tree_order_1025_test,
        smallest_gb_tree_test,

        to_list_b_tree_order_5_test,
        to_list_b_tree_order_9_test,
        to_list_b_tree_order_17_test,
        to_list_b_tree_order_33_test,
        to_list_b_tree_order_65_test,
        to_list_b_tree_order_129_test,
        to_list_b_tree_order_257_test,
        to_list_b_tree_order_513_test,
        to_list_b_tree_order_1025_test,
        to_list_gb_tree_test,

        update_b_tree_order_5_test,
        update_b_tree_order_9_test,
        update_b_tree_order_17_test,
        update_b_tree_order_33_test,
        update_b_tree_order_65_test,
        update_b_tree_order_129_test,
        update_b_tree_order_257_test,
        update_b_tree_order_513_test,
        update_b_tree_order_1025_test,
        update_gb_tree_test,

        values_b_tree_order_5_test,
        values_b_tree_order_9_test,
        values_b_tree_order_17_test,
        values_b_tree_order_33_test,
        values_b_tree_order_65_test,
        values_b_tree_order_129_test,
        values_b_tree_order_257_test,
        values_b_tree_order_513_test,
        values_b_tree_order_1025_test,
        values_gb_tree_test
    ].

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 1025
%%--------------------------------------------------------------------

enter_b_tree_order_1025_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(1025)),
    ?assertEqual(?config(btree_1025, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_1025_new, Config), _BTreeUpdate),
    ok.

enter_b_tree([], BTree) ->
    BTree;
enter_b_tree([{Key, Value} | Tail], BTree) ->
    enter_b_tree(Tail, b_trees:enter(Key, Value, BTree)).

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 129
%%--------------------------------------------------------------------

enter_b_tree_order_129_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(129)),
    ?assertEqual(?config(btree_129, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_129_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 17
%%--------------------------------------------------------------------

enter_b_tree_order_17_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(17)),
    ?assertEqual(?config(btree_17, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_17_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 257
%%--------------------------------------------------------------------

enter_b_tree_order_257_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(257)),
    ?assertEqual(?config(btree_257, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_257_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 33
%%--------------------------------------------------------------------

enter_b_tree_order_33_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(33)),
    ?assertEqual(?config(btree_33, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_33_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 5
%%--------------------------------------------------------------------

enter_b_tree_order_5_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(5)),
    ?assertEqual(?config(btree_5, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_5_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 513
%%--------------------------------------------------------------------

enter_b_tree_order_513_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(513)),
    ?assertEqual(?config(btree_513, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_513_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 65
%%--------------------------------------------------------------------

enter_b_tree_order_65_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(65)),
    ?assertEqual(?config(btree_65, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_65_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter b_tree order 9
%%--------------------------------------------------------------------

enter_b_tree_order_9_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(9)),
    ?assertEqual(?config(btree_9, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(btree_9_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance enter gb_tree
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
%% TEST CASES: performance from_dict b_tree order 1025
%%--------------------------------------------------------------------

from_dict_b_tree_order_1025_test(Config) ->
    _BTree = ?config(btree_1025, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(1025, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 129
%%--------------------------------------------------------------------

from_dict_b_tree_order_129_test(Config) ->
    _BTree = ?config(btree_129, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(129, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 17
%%--------------------------------------------------------------------

from_dict_b_tree_order_17_test(Config) ->
    _BTree = ?config(btree_17, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(17, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 257
%%--------------------------------------------------------------------

from_dict_b_tree_order_257_test(Config) ->
    _BTree = ?config(btree_257, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(257, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 33
%%--------------------------------------------------------------------

from_dict_b_tree_order_33_test(Config) ->
    _BTree = ?config(btree_33, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(33, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 5
%%--------------------------------------------------------------------

from_dict_b_tree_order_5_test(Config) ->
    _BTree = ?config(btree_5, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(5, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 513
%%--------------------------------------------------------------------

from_dict_b_tree_order_513_test(Config) ->
    _BTree = ?config(btree_513, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(513, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 65
%%--------------------------------------------------------------------

from_dict_b_tree_order_65_test(Config) ->
    _BTree = ?config(btree_65, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(65, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict b_tree order 9
%%--------------------------------------------------------------------

from_dict_b_tree_order_9_test(Config) ->
    _BTree = ?config(btree_9, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(9, _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance from_dict gb_tree
%%--------------------------------------------------------------------

from_dict_gb_tree_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    gb_trees:from_orddict(KeyValues),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 1025
%%--------------------------------------------------------------------

get_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    Keys = ?config(keys_rand, Config),
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

get_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 17
%%--------------------------------------------------------------------

get_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 257
%%--------------------------------------------------------------------

get_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 33
%%--------------------------------------------------------------------

get_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 5
%%--------------------------------------------------------------------

get_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 513
%%--------------------------------------------------------------------

get_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 65
%%--------------------------------------------------------------------

get_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get b_tree order 9
%%--------------------------------------------------------------------

get_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    Keys = ?config(keys_rand, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance get gb_tree
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
%% TEST CASES: performance insert b_tree order 1025
%%--------------------------------------------------------------------

insert_b_tree_order_1025_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(1025, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_1025, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(1025 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 129
%%--------------------------------------------------------------------

insert_b_tree_order_129_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(129, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_129, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(129 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 17
%%--------------------------------------------------------------------

insert_b_tree_order_17_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(17, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_17, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(17 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 257
%%--------------------------------------------------------------------

insert_b_tree_order_257_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(257, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_257, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(257 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 33
%%--------------------------------------------------------------------

insert_b_tree_order_33_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(33, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_33, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(33 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 5
%%--------------------------------------------------------------------

insert_b_tree_order_5_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(5, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_5, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(5 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 5 - till
%%--------------------------------------------------------------------

insert_b_tree_order_5_till_test(_Config) ->
    _BTree = test_generator:generate_b_tree_till_number(5, ?NUMBER_INSERTS, 4),
    ?assert(b_trees:height(_BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(5 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 513
%%--------------------------------------------------------------------

insert_b_tree_order_513_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(513, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_513, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(513 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 65
%%--------------------------------------------------------------------

insert_b_tree_order_65_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(65, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_65, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(65 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 9
%%--------------------------------------------------------------------

insert_b_tree_order_9_test(_Config) ->
    _BTree = test_generator:generate_b_tree_from_number(9, ?NUMBER_INSERTS, 4),
    ?assertEqual(?config(btree_9, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(9 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert gb_tree
%%--------------------------------------------------------------------

insert_gb_tree_test(_Config) ->
    ?assertEqual(?config(gbtree, _Config), test_generator:generate_gb_tree_from_number(?NUMBER_INSERTS, 4)).

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 1025
%%--------------------------------------------------------------------

is_defined_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    Keys = ?config(keys_rand, Config),
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

is_defined_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 17
%%--------------------------------------------------------------------

is_defined_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 257
%%--------------------------------------------------------------------

is_defined_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 33
%%--------------------------------------------------------------------

is_defined_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 5
%%--------------------------------------------------------------------

is_defined_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 513
%%--------------------------------------------------------------------

is_defined_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 65
%%--------------------------------------------------------------------

is_defined_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined b_tree order 9
%%--------------------------------------------------------------------

is_defined_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    Keys = ?config(keys_rand, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance is_defined gb_tree
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
%% TEST CASES: performance keys_rand b_tree order 1025
%%--------------------------------------------------------------------

keys_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys_rand b_tree order 129
%%--------------------------------------------------------------------

keys_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys_rand b_tree order 17
%%--------------------------------------------------------------------

keys_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys_rand b_tree order 257
%%--------------------------------------------------------------------

keys_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys_rand b_tree order 33
%%--------------------------------------------------------------------

keys_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys_rand b_tree order 5
%%--------------------------------------------------------------------

keys_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys_rand b_tree order 513
%%--------------------------------------------------------------------

keys_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys_rand b_tree order 65
%%--------------------------------------------------------------------

keys_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys_rand b_tree order 9
%%--------------------------------------------------------------------

keys_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance keys_rand gb_tree
%%--------------------------------------------------------------------

keys_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    _Keys = gb_trees:keys(GBTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 1025
%%--------------------------------------------------------------------

largest_b_tree_order_1025_test(Config) ->
    _BTree = ?config(btree_1025, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 129
%%--------------------------------------------------------------------

largest_b_tree_order_129_test(Config) ->
    _BTree = ?config(btree_129, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 17
%%--------------------------------------------------------------------

largest_b_tree_order_17_test(Config) ->
    _BTree = ?config(btree_17, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 257
%%--------------------------------------------------------------------

largest_b_tree_order_257_test(Config) ->
    _BTree = ?config(btree_257, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 33
%%--------------------------------------------------------------------

largest_b_tree_order_33_test(Config) ->
    _BTree = ?config(btree_33, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 5
%%--------------------------------------------------------------------

largest_b_tree_order_5_test(Config) ->
    _BTree = ?config(btree_5, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 513
%%--------------------------------------------------------------------

largest_b_tree_order_513_test(Config) ->
    _BTree = ?config(btree_513, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 65
%%--------------------------------------------------------------------

largest_b_tree_order_65_test(Config) ->
    _BTree = ?config(btree_65, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest b_tree order 9
%%--------------------------------------------------------------------

largest_b_tree_order_9_test(Config) ->
    _BTree = ?config(btree_9, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance largest gb_tree
%%--------------------------------------------------------------------

largest_gb_tree_test(_Config) ->
    ?assertEqual(?LARGEST_KEY_VALUE, gb_trees:largest(?config(gbtree, _Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 1025
%%--------------------------------------------------------------------

lookup_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    Keys = ?config(keys_rand, Config),
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

lookup_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 17
%%--------------------------------------------------------------------

lookup_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 257
%%--------------------------------------------------------------------

lookup_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 33
%%--------------------------------------------------------------------

lookup_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 5
%%--------------------------------------------------------------------

lookup_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 513
%%--------------------------------------------------------------------

lookup_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 65
%%--------------------------------------------------------------------

lookup_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 9
%%--------------------------------------------------------------------

lookup_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    Keys = ?config(keys_rand, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup gb_tree
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
%% TEST CASES: performance map b_tree order 1025
%%--------------------------------------------------------------------

map_b_tree_order_1025_test(Config) ->
    _BTree = ?config(btree_1025, Config),
    _BTreeNew = ?config(btree_1025_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

map_value_to_new(_, Value) ->
    Value ++ "_new".

%%--------------------------------------------------------------------
%% TEST CASES: performance map b_tree order 129
%%--------------------------------------------------------------------

map_b_tree_order_129_test(Config) ->
    _BTree = ?config(btree_129, Config),
    _BTreeNew = ?config(btree_129_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance map b_tree order 17
%%--------------------------------------------------------------------

map_b_tree_order_17_test(Config) ->
    _BTree = ?config(btree_17, Config),
    _BTreeNew = ?config(btree_17_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance map b_tree order 257
%%--------------------------------------------------------------------

map_b_tree_order_257_test(Config) ->
    _BTree = ?config(btree_257, Config),
    _BTreeNew = ?config(btree_257_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance map b_tree order 33
%%--------------------------------------------------------------------

map_b_tree_order_33_test(Config) ->
    _BTree = ?config(btree_33, Config),
    _BTreeNew = ?config(btree_33_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance map b_tree order 5
%%--------------------------------------------------------------------

map_b_tree_order_5_test(Config) ->
    _BTree = ?config(btree_5, Config),
    _BTreeNew = ?config(btree_5_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance map b_tree order 513
%%--------------------------------------------------------------------

map_b_tree_order_513_test(Config) ->
    _BTree = ?config(btree_513, Config),
    _BTreeNew = ?config(btree_513_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance map b_tree order 65
%%--------------------------------------------------------------------

map_b_tree_order_65_test(Config) ->
    _BTree = ?config(btree_65, Config),
    _BTreeNew = ?config(btree_65_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance map b_tree order 9
%%--------------------------------------------------------------------

map_b_tree_order_9_test(Config) ->
    _BTree = ?config(btree_9, Config),
    _BTreeNew = ?config(btree_9_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance map gb_tree
%%--------------------------------------------------------------------

map_gb_tree_test(_Config) ->
    ?assertEqual(?config(gbtree_new, _Config), gb_trees:map(fun map_value_to_new/2, ?config(gbtree, _Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 1025
%%--------------------------------------------------------------------

smallest_b_tree_order_1025_test(Config) ->
    _BTree = ?config(btree_1025, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 129
%%--------------------------------------------------------------------

smallest_b_tree_order_129_test(Config) ->
    _BTree = ?config(btree_129, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 17
%%--------------------------------------------------------------------

smallest_b_tree_order_17_test(Config) ->
    _BTree = ?config(btree_17, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 257
%%--------------------------------------------------------------------

smallest_b_tree_order_257_test(Config) ->
    _BTree = ?config(btree_257, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 33
%%--------------------------------------------------------------------

smallest_b_tree_order_33_test(Config) ->
    _BTree = ?config(btree_33, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 5
%%--------------------------------------------------------------------

smallest_b_tree_order_5_test(Config) ->
    _BTree = ?config(btree_5, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 513
%%--------------------------------------------------------------------

smallest_b_tree_order_513_test(Config) ->
    _BTree = ?config(btree_513, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 65
%%--------------------------------------------------------------------

smallest_b_tree_order_65_test(Config) ->
    _BTree = ?config(btree_65, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest b_tree order 9
%%--------------------------------------------------------------------

smallest_b_tree_order_9_test(Config) ->
    _BTree = ?config(btree_9, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance smallest gb_tree
%%--------------------------------------------------------------------

smallest_gb_tree_test(_Config) ->
    ?assertEqual(?SMALLEST_KEY_VALUE, gb_trees:smallest(?config(gbtree, _Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 1025
%%--------------------------------------------------------------------

to_list_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 129
%%--------------------------------------------------------------------

to_list_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 17
%%--------------------------------------------------------------------

to_list_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 257
%%--------------------------------------------------------------------

to_list_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 33
%%--------------------------------------------------------------------

to_list_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 5
%%--------------------------------------------------------------------

to_list_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 513
%%--------------------------------------------------------------------

to_list_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 65
%%--------------------------------------------------------------------

to_list_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list b_tree order 9
%%--------------------------------------------------------------------

to_list_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance to_list gb_tree
%%--------------------------------------------------------------------

to_list_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    _KeyValues = gb_trees:to_list(GBTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_INSERTS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 1025
%%--------------------------------------------------------------------

update_b_tree_order_1025_test(Config) ->
    _BTree = ?config(btree_1025, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_1025_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

update_b_tree([], BTree) ->
    BTree;
update_b_tree([{Key, Value} | Tail], BTree) ->
    update_b_tree(Tail, b_trees:update(Key, Value, BTree)).

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 129
%%--------------------------------------------------------------------

update_b_tree_order_129_test(Config) ->
    _BTree = ?config(btree_129, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_129_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 17
%%--------------------------------------------------------------------

update_b_tree_order_17_test(Config) ->
    _BTree = ?config(btree_17, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_17_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 257
%%--------------------------------------------------------------------

update_b_tree_order_257_test(Config) ->
    _BTree = ?config(btree_257, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_257_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 33
%%--------------------------------------------------------------------

update_b_tree_order_33_test(Config) ->
    _BTree = ?config(btree_33, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_33_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 5
%%--------------------------------------------------------------------

update_b_tree_order_5_test(Config) ->
    _BTree = ?config(btree_5, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_5_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 513
%%--------------------------------------------------------------------

update_b_tree_order_513_test(Config) ->
    _BTree = ?config(btree_513, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_513_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 65
%%--------------------------------------------------------------------

update_b_tree_order_65_test(Config) ->
    _BTree = ?config(btree_65, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_65_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update b_tree order 9
%%--------------------------------------------------------------------

update_b_tree_order_9_test(Config) ->
    _BTree = ?config(btree_9, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(btree_9_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance update gb_tree
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
%% TEST CASES: performance values b_tree order 1025
%%--------------------------------------------------------------------

values_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 129
%%--------------------------------------------------------------------

values_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 17
%%--------------------------------------------------------------------

values_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 257
%%--------------------------------------------------------------------

values_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 33
%%--------------------------------------------------------------------

values_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 5
%%--------------------------------------------------------------------

values_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 513
%%--------------------------------------------------------------------

values_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 65
%%--------------------------------------------------------------------

values_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values b_tree order 9
%%--------------------------------------------------------------------

values_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance values gb_tree
%%--------------------------------------------------------------------

values_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    _Keys = gb_trees:values(GBTree),
    ?assertEqual(?NUMBER_INSERTS, length(_Keys)),
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
