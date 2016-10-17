%%%-------------------------------------------------------------------
%%% File        : b_trees_SUITE.erl
%%% Description : Test Suite for module: b_trees.
%%%
%%% Created     : 09.09.2016
%%%
%%% Copyright (C) 2016 Walter Weinmann
%%%-------------------------------------------------------------------
-module(b_trees_SUITE).

-compile(export_all).

-define(NODEBUG, true).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/b_trees_templates.hrl").

-define(NUMBER_INSERTS, 2000).
-define(NUMBER_LOOKUPS, 10000).

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
        {btree_9, test_generator:generate_b_tree_till_number(9, ?NUMBER_INSERTS, 4)},
        {btree_17, test_generator:generate_b_tree_from_number(17, ?NUMBER_INSERTS, 4)},
        {btree_33, test_generator:generate_b_tree_till_number(33, ?NUMBER_INSERTS, 4)},
        {btree_65, test_generator:generate_b_tree_from_number(65, ?NUMBER_INSERTS, 4)},
        {btree_129, test_generator:generate_b_tree_till_number(129, ?NUMBER_INSERTS, 4)},
        {btree_257, test_generator:generate_b_tree_from_number(257, ?NUMBER_INSERTS, 4)},
        {btree_513, test_generator:generate_b_tree_from_number(513, ?NUMBER_INSERTS, 4)},
        {btree_1025, test_generator:generate_b_tree_from_number(1025, ?NUMBER_INSERTS, 4)},
        {lookUps, test_generator:generate_keys_rand(?NUMBER_INSERTS, ?NUMBER_LOOKUPS, 4)}
        | Config
    ].

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%--------------------------------------------------------------------

all() ->
    [
        empty_test,
        insert_b_tree_order_5_test,
        insert_b_tree_order_7_test,
        insert_b_tree_order_8_test,
        insert_key_exists_test,
        is_empty_test,
        lookup_test,
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
        size_test
    ].

%%--------------------------------------------------------------------
%% TEST CASES: empty
%%--------------------------------------------------------------------

empty_test(_Config) ->
    ?assertEqual(?B_TREE_33_00, b_trees:empty(33)),
    ?assert(b_trees:is_empty(b_trees:empty(33))),
    ?assertEqual(0, b_trees:size(b_trees:empty(33))),

    ?assertEqual(?B_STAR_TREE_33_00, b_trees:empty(33, b_star)),
    ?assert(b_trees:is_empty(b_trees:empty(33, b_star))),
    ?assertEqual(0, b_trees:size(b_trees:empty(33, b_star))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert key exists
%%--------------------------------------------------------------------

insert_key_exists_test(_Config) ->
    ?assertException(error, {key_exists, "k_01"}, b_trees:insert("k_01", "v_01", test_generator:generate_b_tree_from_number(5, 5, 2))),
    ?assertException(error, {key_exists, "k_02"}, b_trees:insert("k_02", "v_02", test_generator:generate_b_tree_from_number(5, 5, 2))),
    ?assertException(error, {key_exists, "k_03"}, b_trees:insert("k_03", "v_03", test_generator:generate_b_tree_from_number(5, 5, 2))),
    ?assertException(error, {key_exists, "k_04"}, b_trees:insert("k_04", "v_04", test_generator:generate_b_tree_from_number(5, 5, 2))),
    ?assertException(error, {key_exists, "k_05"}, b_trees:insert("k_05", "v_05", test_generator:generate_b_tree_from_number(5, 5, 2))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert order 5
%%--------------------------------------------------------------------

insert_b_tree_order_5_test(_Config) ->
    ?assertEqual(?B_TREE_05_00, b_trees:empty(5)),
    ?assertEqual(0, b_trees:number_key_values(?B_TREE_05_00)),
    ?assertEqual(0, b_trees:height(?B_TREE_05_00)),
    ?assertEqual(0, b_trees:size(?B_TREE_05_00)),

    ?assertEqual(?B_TREE_05_01, b_trees:insert("k_01", "v_01", ?B_TREE_05_00)),
    ?assertEqual(1, b_trees:number_key_values(?B_TREE_05_01)),
    ?assertEqual(1, b_trees:height(?B_TREE_05_01)),
    ?assertEqual(1, b_trees:size(?B_TREE_05_01)),

    ?assertEqual(?B_TREE_05_02, b_trees:insert("k_02", "v_02", test_generator:generate_b_tree_from_number(5, 1, 2))),
    ?assertEqual(2, b_trees:number_key_values(?B_TREE_05_02)),
    ?assertEqual(1, b_trees:height(?B_TREE_05_02)),
    ?assertEqual(1, b_trees:size(?B_TREE_05_02)),

    ?assertEqual(?B_TREE_05_03, b_trees:insert("k_03", "v_03", test_generator:generate_b_tree_from_number(5, 2, 2))),
    ?assertEqual(3, b_trees:number_key_values(?B_TREE_05_03)),
    ?assertEqual(1, b_trees:height(?B_TREE_05_03)),
    ?assertEqual(1, b_trees:size(?B_TREE_05_03)),

    ?assertEqual(?B_TREE_05_04, b_trees:insert("k_04", "v_04", test_generator:generate_b_tree_from_number(5, 3, 2))),
    ?assertEqual(4, b_trees:number_key_values(?B_TREE_05_04)),
    ?assertEqual(1, b_trees:height(?B_TREE_05_04)),
    ?assertEqual(1, b_trees:size(?B_TREE_05_04)),

    ?assertEqual(?B_TREE_05_05, b_trees:insert("k_05", "v_05", test_generator:generate_b_tree_from_number(5, 4, 2))),
    ?assertEqual(5, b_trees:number_key_values(?B_TREE_05_05)),
    ?assertEqual(2, b_trees:height(?B_TREE_05_05)),
    ?assertEqual(3, b_trees:size(?B_TREE_05_05)),

    ?assertEqual(?B_TREE_05_09, b_trees:insert("k_09", "v_09", test_generator:generate_b_tree_from_number(5, 8, 2))),
    ?assertEqual(9, b_trees:number_key_values(?B_TREE_05_09)),
    ?assertEqual(2, b_trees:height(?B_TREE_05_09)),
    ?assertEqual(4, b_trees:size(?B_TREE_05_09)),

    ?assertEqual(?B_TREE_05_13, b_trees:insert("k_13", "v_13", test_generator:generate_b_tree_from_number(5, 12, 2))),
    ?assertEqual(13, b_trees:number_key_values(?B_TREE_05_13)),
    ?assertEqual(2, b_trees:height(?B_TREE_05_13)),
    ?assertEqual(5, b_trees:size(?B_TREE_05_13)),

    ?assertEqual(?B_TREE_05_17, b_trees:insert("k_17", "v_17", test_generator:generate_b_tree_from_number(5, 16, 2))),
    ?assertEqual(17, b_trees:number_key_values(?B_TREE_05_17)),
    ?assertEqual(3, b_trees:height(?B_TREE_05_17)),
    ?assertEqual(9, b_trees:size(?B_TREE_05_17)),

    ?assertEqual(?B_TREE_05_21, b_trees:insert("k_21", "v_21", test_generator:generate_b_tree_from_number(5, 20, 2))),
    ?assertEqual(21, b_trees:number_key_values(?B_TREE_05_21)),
    ?assertEqual(3, b_trees:height(?B_TREE_05_21)),
    ?assertEqual(10, b_trees:size(?B_TREE_05_21)),

    ?assertEqual(?B_TREE_05_25, b_trees:insert("k_25", "v_25", test_generator:generate_b_tree_from_number(5, 24, 2))),
    ?assertEqual(25, b_trees:number_key_values(?B_TREE_05_25)),
    ?assertEqual(3, b_trees:height(?B_TREE_05_25)),
    ?assertEqual(12, b_trees:size(?B_TREE_05_25)),

    ?assertEqual(?B_TREE_05_29, b_trees:insert("k_29", "v_29", test_generator:generate_b_tree_from_number(5, 28, 2))),
    ?assertEqual(29, b_trees:number_key_values(?B_TREE_05_29)),
    ?assertEqual(3, b_trees:height(?B_TREE_05_29)),
    ?assertEqual(14, b_trees:size(?B_TREE_05_29)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert order 7
%%--------------------------------------------------------------------

insert_b_tree_order_7_test(_Config) ->
    ?assertEqual(?B_TREE_07_00, b_trees:empty(7)),
    ?assertEqual(0, b_trees:number_key_values(?B_TREE_07_00)),
    ?assertEqual(0, b_trees:height(?B_TREE_07_00)),
    ?assertEqual(0, b_trees:size(?B_TREE_07_00)),

    ?assertEqual(?B_TREE_07_07, b_trees:insert("k_07", "v_07", test_generator:generate_b_tree_from_number(7, 6, 2))),
    ?assertEqual(7, b_trees:number_key_values(?B_TREE_07_07)),
    ?assertEqual(2, b_trees:height(?B_TREE_07_07)),
    ?assertEqual(3, b_trees:size(?B_TREE_07_07)),

    ?assertEqual(?B_TREE_07_80, test_generator:generate_b_tree_from_list(7,
        [
            11, 17, 23, 31, 41, 47, 59, 67, 73, 83, 97,
            103, 109, 127, 137, 149, 157, 167, 179, 191, 197,
            211, 227, 233, 241, 257, 269, 277, 283,
            307, 313, 331, 347, 353, 367, 379, 389,
            401, 419, 431, 439, 449, 461, 467, 487, 499,
            509, 523, 547, 563, 571, 587, 599,
            607, 617, 631, 643, 653, 661, 677, 691,
            709, 727, 739, 751, 761, 773, 797,
            811, 823, 829, 853, 859, 877, 883,
            907, 919, 937, 947, 967
        ],
        3)),
    ?assertEqual(80, b_trees:number_key_values(?B_TREE_07_80)),
    ?assertEqual(3, b_trees:height(?B_TREE_07_80)),
    ?assertEqual(26, b_trees:size(?B_TREE_07_80)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert order 8
%%--------------------------------------------------------------------

insert_b_tree_order_8_test(_Config) ->
    ?assertEqual(?B_TREE_08_16, b_trees:insert("k_16", "v_16", test_generator:generate_b_tree_from_number(8, 15, 2))),
    ?assertEqual(16, b_trees:number_key_values(?B_TREE_08_16)),
    ?assertEqual(2, b_trees:height(?B_TREE_08_16)),
    ?assertEqual(4, b_trees:size(?B_TREE_08_16)),

    ?assertEqual(?B_TREE_08_32, b_trees:insert("k_32", "v_32", test_generator:generate_b_tree_from_number(8, 31, 2))),
    ?assertEqual(32, b_trees:number_key_values(?B_TREE_08_32)),
    ?assertEqual(2, b_trees:height(?B_TREE_08_32)),
    ?assertEqual(7, b_trees:size(?B_TREE_08_32)),

    ?assertEqual(?B_TREE_08_64, b_trees:insert("k_64", "v_64", test_generator:generate_b_tree_from_number(8, 63, 2))),
    ?assertEqual(64, b_trees:number_key_values(?B_TREE_08_64)),
    ?assertEqual(3, b_trees:height(?B_TREE_08_64)),
    ?assertEqual(17, b_trees:size(?B_TREE_08_64)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_empty
%%--------------------------------------------------------------------

is_empty_test(_Config) ->
    ?assertEqual(false, b_trees:is_empty(?B_TREE_33_01)),

    ?assertEqual(true, b_trees:is_empty(?B_TREE_33_00)),

    ?assertEqual(true, b_trees:is_empty(?B_STAR_TREE_07_00)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup
%%--------------------------------------------------------------------

lookup_test(_Config) ->
    ?assertEqual(none, b_trees:lookup("k_00", ?B_TREE_04_00)),

    ?assertEqual(none, b_trees:lookup("k_00", ?B_TREE_04_04)),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", ?B_TREE_04_04)),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", ?B_TREE_04_04)),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", ?B_TREE_04_04)),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", ?B_TREE_04_04)),
    ?assertEqual(none, b_trees:lookup("k_05", ?B_TREE_04_04)),

    ?assertEqual(none, b_trees:lookup("k_00", ?B_TREE_07_07)),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", ?B_TREE_07_07)),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", ?B_TREE_07_07)),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", ?B_TREE_07_07)),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", ?B_TREE_07_07)),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", ?B_TREE_07_07)),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", ?B_TREE_07_07)),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", ?B_TREE_07_07)),
    ?assertEqual(none, b_trees:lookup("k_08", ?B_TREE_07_07)),

    ?assertEqual(none, b_trees:lookup("k_00", ?B_TREE_10_10)),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", ?B_TREE_10_10)),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", ?B_TREE_10_10)),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", ?B_TREE_10_10)),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", ?B_TREE_10_10)),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", ?B_TREE_10_10)),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", ?B_TREE_10_10)),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", ?B_TREE_10_10)),
    ?assertEqual({value, "v_08"}, b_trees:lookup("k_08", ?B_TREE_10_10)),
    ?assertEqual({value, "v_09"}, b_trees:lookup("k_09", ?B_TREE_10_10)),
    ?assertEqual({value, "v_10"}, b_trees:lookup("k_10", ?B_TREE_10_10)),
    ?assertEqual(none, b_trees:lookup("k_11", ?B_TREE_10_10)),

    ?assertEqual(none, b_trees:lookup("k_00", ?B_TREE_13_13)),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", ?B_TREE_13_13)),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", ?B_TREE_13_13)),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", ?B_TREE_13_13)),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", ?B_TREE_13_13)),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", ?B_TREE_13_13)),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", ?B_TREE_13_13)),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", ?B_TREE_13_13)),
    ?assertEqual({value, "v_08"}, b_trees:lookup("k_08", ?B_TREE_13_13)),
    ?assertEqual({value, "v_09"}, b_trees:lookup("k_09", ?B_TREE_13_13)),
    ?assertEqual({value, "v_10"}, b_trees:lookup("k_10", ?B_TREE_13_13)),
    ?assertEqual({value, "v_11"}, b_trees:lookup("k_11", ?B_TREE_13_13)),
    ?assertEqual({value, "v_12"}, b_trees:lookup("k_12", ?B_TREE_13_13)),
    ?assertEqual({value, "v_13"}, b_trees:lookup("k_13", ?B_TREE_13_13)),
    ?assertEqual(none, b_trees:lookup("k_14", ?B_TREE_13_13)),

    ?assertEqual(none, b_trees:lookup("k_00", ?B_TREE_16_16)),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", ?B_TREE_16_16)),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", ?B_TREE_16_16)),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", ?B_TREE_16_16)),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", ?B_TREE_16_16)),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", ?B_TREE_16_16)),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", ?B_TREE_16_16)),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", ?B_TREE_16_16)),
    ?assertEqual({value, "v_08"}, b_trees:lookup("k_08", ?B_TREE_16_16)),
    ?assertEqual({value, "v_09"}, b_trees:lookup("k_09", ?B_TREE_16_16)),
    ?assertEqual({value, "v_10"}, b_trees:lookup("k_10", ?B_TREE_16_16)),
    ?assertEqual({value, "v_11"}, b_trees:lookup("k_11", ?B_TREE_16_16)),
    ?assertEqual({value, "v_12"}, b_trees:lookup("k_12", ?B_TREE_16_16)),
    ?assertEqual({value, "v_13"}, b_trees:lookup("k_13", ?B_TREE_16_16)),
    ?assertEqual({value, "v_14"}, b_trees:lookup("k_14", ?B_TREE_16_16)),
    ?assertEqual({value, "v_15"}, b_trees:lookup("k_15", ?B_TREE_16_16)),
    ?assertEqual({value, "v_16"}, b_trees:lookup("k_16", ?B_TREE_16_16)),
    ?assertEqual(none, b_trees:lookup("k_17", ?B_TREE_16_16)),

    ?assertEqual(none, b_trees:lookup("k_00", ?B_TREE_19_19)),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", ?B_TREE_19_19)),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", ?B_TREE_19_19)),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", ?B_TREE_19_19)),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", ?B_TREE_19_19)),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", ?B_TREE_19_19)),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", ?B_TREE_19_19)),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", ?B_TREE_19_19)),
    ?assertEqual({value, "v_08"}, b_trees:lookup("k_08", ?B_TREE_19_19)),
    ?assertEqual({value, "v_09"}, b_trees:lookup("k_09", ?B_TREE_19_19)),
    ?assertEqual({value, "v_10"}, b_trees:lookup("k_10", ?B_TREE_19_19)),
    ?assertEqual({value, "v_11"}, b_trees:lookup("k_11", ?B_TREE_19_19)),
    ?assertEqual({value, "v_12"}, b_trees:lookup("k_12", ?B_TREE_19_19)),
    ?assertEqual({value, "v_13"}, b_trees:lookup("k_13", ?B_TREE_19_19)),
    ?assertEqual({value, "v_14"}, b_trees:lookup("k_14", ?B_TREE_19_19)),
    ?assertEqual({value, "v_15"}, b_trees:lookup("k_15", ?B_TREE_19_19)),
    ?assertEqual({value, "v_16"}, b_trees:lookup("k_16", ?B_TREE_19_19)),
    ?assertEqual({value, "v_17"}, b_trees:lookup("k_17", ?B_TREE_19_19)),
    ?assertEqual({value, "v_18"}, b_trees:lookup("k_18", ?B_TREE_19_19)),
    ?assertEqual({value, "v_19"}, b_trees:lookup("k_19", ?B_TREE_19_19)),
    ?assertEqual(none, b_trees:lookup("k_20", ?B_TREE_19_19)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 1025
%%--------------------------------------------------------------------

performance_insert_b_tree_order_1025_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(1025, ?NUMBER_INSERTS, 4),
    ?debugFmt("wwe debugging performance_insert_b_tree_order_1025_test/1 ===> Start ~n Height: ~p~n Number Key Values: ~p~n Size: ~p~n Log: ~p~n", [b_trees:height(BTree), b_trees:number_key_values(BTree), b_trees:size(BTree), math:log(((?NUMBER_INSERTS + 1) / 2)) / math:log(1025 div 2)]),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(1025 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 129
%%--------------------------------------------------------------------

performance_insert_b_tree_order_129_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(129, ?NUMBER_INSERTS, 4),
    ?debugFmt("wwe debugging performance_insert_b_tree_order_129_test/1 ===> Start ~n Height: ~p~n Number Key Values: ~p~n Size: ~p~n Log: ~p~n", [b_trees:height(BTree), b_trees:number_key_values(BTree), b_trees:size(BTree), math:log(((?NUMBER_INSERTS + 1) / 2)) / math:log(129 div 2)]),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(129 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 17
%%--------------------------------------------------------------------

performance_insert_b_tree_order_17_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(17, ?NUMBER_INSERTS, 4),
    ?debugFmt("wwe debugging performance_insert_b_tree_order_17_test/1 ===> Start ~n Height: ~p~n Number Key Values: ~p~n Size: ~p~n Log: ~p~n", [b_trees:height(BTree), b_trees:number_key_values(BTree), b_trees:size(BTree), math:log(((?NUMBER_INSERTS + 1) / 2)) / math:log(17 div 2)]),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(17 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 257
%%--------------------------------------------------------------------

performance_insert_b_tree_order_257_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(257, ?NUMBER_INSERTS, 4),
    ?debugFmt("wwe debugging performance_insert_b_tree_order_257_test/1 ===> Start ~n Height: ~p~n Number Key Values: ~p~n Size: ~p~n Log: ~p~n", [b_trees:height(BTree), b_trees:number_key_values(BTree), b_trees:size(BTree), math:log(((?NUMBER_INSERTS + 1) / 2)) / math:log(257 div 2)]),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(257 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 33
%%--------------------------------------------------------------------

performance_insert_b_tree_order_33_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(33, ?NUMBER_INSERTS, 4),
    ?debugFmt("wwe debugging performance_insert_b_tree_order_33_test/1 ===> Start ~n Height: ~p~n Number Key Values: ~p~n Size: ~p~n Log: ~p~n", [b_trees:height(BTree), b_trees:number_key_values(BTree), b_trees:size(BTree), math:log(((?NUMBER_INSERTS + 1) / 2)) / math:log(33 div 2)]),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(33 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 5
%%--------------------------------------------------------------------

performance_insert_b_tree_order_5_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(5, ?NUMBER_INSERTS, 4),
    ?debugFmt("wwe debugging performance_insert_b_tree_order_5_test/1 ===> Start ~n Height: ~p~n Number Key Values: ~p~n Size: ~p~n Log: ~p~n", [b_trees:height(BTree), b_trees:number_key_values(BTree), b_trees:size(BTree), math:log(((?NUMBER_INSERTS + 1) / 2)) / math:log(5 div 2)]),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(5 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 513
%%--------------------------------------------------------------------

performance_insert_b_tree_order_513_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(513, ?NUMBER_INSERTS, 4),
    ?debugFmt("wwe debugging performance_insert_b_tree_order_513_test/1 ===> Start ~n Height: ~p~n Number Key Values: ~p~n Size: ~p~n Log: ~p~n", [b_trees:height(BTree), b_trees:number_key_values(BTree), b_trees:size(BTree), math:log(((?NUMBER_INSERTS + 1) / 2)) / math:log(513 div 2)]),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(513 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 65
%%--------------------------------------------------------------------

performance_insert_b_tree_order_65_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(65, ?NUMBER_INSERTS, 4),
    ?debugFmt("wwe debugging performance_insert_b_tree_order_65_test/1 ===> Start ~n Height: ~p~n Number Key Values: ~p~n Size: ~p~n Log: ~p~n", [b_trees:height(BTree), b_trees:number_key_values(BTree), b_trees:size(BTree), math:log(((?NUMBER_INSERTS + 1) / 2)) / math:log(65 div 2)]),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(65 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 9
%%--------------------------------------------------------------------

performance_insert_b_tree_order_9_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(9, ?NUMBER_INSERTS, 4),
    ?debugFmt("wwe debugging performance_insert_b_tree_order_9_test/1 ===> Start ~n Height: ~p~n Number Key Values: ~p~n Size: ~p~n Log: ~p~n", [b_trees:height(BTree), b_trees:number_key_values(BTree), b_trees:size(BTree), math:log(((?NUMBER_INSERTS + 1) / 2)) / math:log(9 div 2)]),
    ?assert(b_trees:height(BTree) =< int_ceil((math:log((?NUMBER_INSERTS + 1) / 2) / math:log(9 div 2)))).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert gb_tree
%%--------------------------------------------------------------------

performance_insert_gb_tree_test(_Config) ->
    test_generator:generate_gb_tree_from_number(?NUMBER_INSERTS, 4).

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 1025
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_1025_test(Config) ->
    BTree = ?config(btree_1025, Config),
    LookUps = ?config(lookUps, Config),
    lookup_b_tree(LookUps, BTree),
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
    LookUps = ?config(lookUps, Config),
    lookup_b_tree(LookUps, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 17
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_17_test(Config) ->
    BTree = ?config(btree_17, Config),
    LookUps = ?config(lookUps, Config),
    lookup_b_tree(LookUps, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 257
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_257_test(Config) ->
    BTree = ?config(btree_257, Config),
    LookUps = ?config(lookUps, Config),
    lookup_b_tree(LookUps, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 33
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_33_test(Config) ->
    BTree = ?config(btree_33, Config),
    LookUps = ?config(lookUps, Config),
    lookup_b_tree(LookUps, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 5
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_5_test(Config) ->
    BTree = ?config(btree_5, Config),
    LookUps = ?config(lookUps, Config),
    lookup_b_tree(LookUps, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 513
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_513_test(Config) ->
    BTree = ?config(btree_513, Config),
    LookUps = ?config(lookUps, Config),
    lookup_b_tree(LookUps, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 65
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_65_test(Config) ->
    BTree = ?config(btree_65, Config),
    LookUps = ?config(lookUps, Config),
    lookup_b_tree(LookUps, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 9
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_9_test(Config) ->
    BTree = ?config(btree_9, Config),
    LookUps = ?config(lookUps, Config),
    lookup_b_tree(LookUps, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup gb_tree
%%--------------------------------------------------------------------

performance_lookup_gb_tree_test(Config) ->
    GBTree = ?config(gbtree, Config),
    LookUps = ?config(lookUps, Config),
    lookup_gb_tree(LookUps, GBTree),
    ok.

lookup_gb_tree([], _) ->
    none;
lookup_gb_tree([Key | Tail], GBTree) ->
    gb_trees:lookup(Key, GBTree),
    lookup_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: size
%%--------------------------------------------------------------------

size_test(_Config) ->

    ?assertEqual(0, b_trees:size(b_trees:empty(5))),

    ?assertEqual(1, b_trees:size(?B_TREE_05_01)),

    ?assertEqual(3, b_trees:size(?B_TREE_05_05)),

    ?assertEqual(5, b_trees:size(?B_TREE_05_11)),

    ?assertEqual(8, b_trees:size(?B_TREE_05_16)),

    ?assertEqual(10, b_trees:size(?B_TREE_05_21)),

    ?assertEqual(0, b_trees:size(b_trees:empty(5, b_star))),

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
