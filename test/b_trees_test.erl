%%%-------------------------------------------------------------------
%%% File        : b_trees_test.erl
%%% Description : Eunit tests for module: b_trees.
%%%
%%% Created     : 09.09.2016
%%%
%%% Copyright (C) 2016 Walter Weinmann
%%%-------------------------------------------------------------------

-module(b_trees_test).

-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/b_trees_templates.hrl").

-define(TIMEOUT, 60).

%%--------------------------------------------------------------------
%% TEST CASES: empty - b-tree
%%--------------------------------------------------------------------

empty_b_tree_test() ->
    ?assertEqual(?B_TREE_33_00, b_trees:empty(33)),
    ?assert(b_trees:is_empty(b_trees:empty(33))),
    ?assertEqual(0, b_trees:size(b_trees:empty(33))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: empty - b*-tree
%%--------------------------------------------------------------------

empty_b_star_tree_test() ->
    ?assertEqual(?B_TREE_33_00, b_trees:empty(33)),
    ?assert(b_trees:is_empty(b_trees:empty(33))),
    ?assertEqual(0, b_trees:size(b_trees:empty(33))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter - order 4
%%--------------------------------------------------------------------

enter_b_tree_order_4_test() ->
    BTree_04_15_01 = b_trees:enter("k_01", "v_01", ?B_TREE_04_15),
    BTree_04_15_02 = b_trees:enter("k_02", "v_02", BTree_04_15_01),
    BTree_04_15_03 = b_trees:enter("k_03", "v_03", BTree_04_15_02),
    BTree_04_15_04 = b_trees:enter("k_04", "v_04", BTree_04_15_03),
    BTree_04_15_05 = b_trees:enter("k_05", "v_05", BTree_04_15_04),
    BTree_04_15_06 = b_trees:enter("k_06", "v_06", BTree_04_15_05),
    BTree_04_15_07 = b_trees:enter("k_07", "v_07", BTree_04_15_06),
    BTree_04_15_08 = b_trees:enter("k_08", "v_08", BTree_04_15_07),
    BTree_04_15_09 = b_trees:enter("k_09", "v_09", BTree_04_15_08),
    BTree_04_15_10 = b_trees:enter("k_10", "v_10", BTree_04_15_09),
    BTree_04_15_11 = b_trees:enter("k_11", "v_11", BTree_04_15_10),
    BTree_04_15_12 = b_trees:enter("k_12", "v_12", BTree_04_15_11),
    BTree_04_15_13 = b_trees:enter("k_13", "v_13", BTree_04_15_12),
    BTree_04_15_14 = b_trees:enter("k_14", "v_14", BTree_04_15_13),
    BTree_04_15_15 = b_trees:enter("k_15", "v_15", BTree_04_15_14),
    ?assertEqual(?B_TREE_04_15, BTree_04_15_15),

    BTree_04_15_01_NEW = b_trees:enter("k_01", "v_01_new", BTree_04_15_15),
    BTree_04_15_02_NEW = b_trees:enter("k_02", "v_02_new", BTree_04_15_01_NEW),
    BTree_04_15_03_NEW = b_trees:enter("k_03", "v_03_new", BTree_04_15_02_NEW),
    BTree_04_15_04_NEW = b_trees:enter("k_04", "v_04_new", BTree_04_15_03_NEW),
    BTree_04_15_05_NEW = b_trees:enter("k_05", "v_05_new", BTree_04_15_04_NEW),
    BTree_04_15_06_NEW = b_trees:enter("k_06", "v_06_new", BTree_04_15_05_NEW),
    BTree_04_15_07_NEW = b_trees:enter("k_07", "v_07_new", BTree_04_15_06_NEW),
    BTree_04_15_08_NEW = b_trees:enter("k_08", "v_08_new", BTree_04_15_07_NEW),
    BTree_04_15_09_NEW = b_trees:enter("k_09", "v_09_new", BTree_04_15_08_NEW),
    BTree_04_15_10_NEW = b_trees:enter("k_10", "v_10_new", BTree_04_15_09_NEW),
    BTree_04_15_11_NEW = b_trees:enter("k_11", "v_11_new", BTree_04_15_10_NEW),
    BTree_04_15_12_NEW = b_trees:enter("k_12", "v_12_new", BTree_04_15_11_NEW),
    BTree_04_15_13_NEW = b_trees:enter("k_13", "v_13_new", BTree_04_15_12_NEW),
    BTree_04_15_14_NEW = b_trees:enter("k_14", "v_14_new", BTree_04_15_13_NEW),
    ?assertEqual(?B_TREE_04_15_UPDATE, b_trees:enter("k_15", "v_15_new", BTree_04_15_14_NEW)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict - b-tree
%%--------------------------------------------------------------------

from_dict_b_tree_test() ->
    ?assertEqual(?B_TREE_04_04, b_trees:from_dict(4, test_generator:generate_key_values_from(4, 2))),
    ?assertEqual(?B_TREE_05_01, b_trees:from_dict(5, test_generator:generate_key_values_from(1, 2))),
    ?assertEqual(?B_TREE_05_29, b_trees:from_dict(5, test_generator:generate_key_values_from(29, 2))),
    ?assertEqual(?B_TREE_19_19, b_trees:from_dict(19, test_generator:generate_key_values_from(19, 2))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict - b*-tree
%%--------------------------------------------------------------------

from_dict_b_star_tree_test() ->
    ?assertEqual(?B_STAR_TREE_33_01, b_trees:from_dict(33, b_star, test_generator:generate_key_values_from(1, 2))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get
%%--------------------------------------------------------------------

get_test() ->
    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", ?B_TREE_04_00)),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", ?B_TREE_04_04)),
    ?assertEqual("v_01", b_trees:get("k_01", ?B_TREE_04_04)),
    ?assertEqual("v_02", b_trees:get("k_02", ?B_TREE_04_04)),
    ?assertEqual("v_03", b_trees:get("k_03", ?B_TREE_04_04)),
    ?assertEqual("v_04", b_trees:get("k_04", ?B_TREE_04_04)),
    ?assertException(error, {key_not_found, "k_05"}, b_trees:get("k_05", ?B_TREE_04_04)),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", ?B_TREE_07_07)),
    ?assertEqual("v_01", b_trees:get("k_01", ?B_TREE_07_07)),
    ?assertEqual("v_02", b_trees:get("k_02", ?B_TREE_07_07)),
    ?assertEqual("v_03", b_trees:get("k_03", ?B_TREE_07_07)),
    ?assertEqual("v_04", b_trees:get("k_04", ?B_TREE_07_07)),
    ?assertEqual("v_05", b_trees:get("k_05", ?B_TREE_07_07)),
    ?assertEqual("v_06", b_trees:get("k_06", ?B_TREE_07_07)),
    ?assertEqual("v_07", b_trees:get("k_07", ?B_TREE_07_07)),
    ?assertException(error, {key_not_found, "k_08"}, b_trees:get("k_08", ?B_TREE_07_07)),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", ?B_TREE_10_10)),
    ?assertEqual("v_01", b_trees:get("k_01", ?B_TREE_10_10)),
    ?assertEqual("v_02", b_trees:get("k_02", ?B_TREE_10_10)),
    ?assertEqual("v_03", b_trees:get("k_03", ?B_TREE_10_10)),
    ?assertEqual("v_04", b_trees:get("k_04", ?B_TREE_10_10)),
    ?assertEqual("v_05", b_trees:get("k_05", ?B_TREE_10_10)),
    ?assertEqual("v_06", b_trees:get("k_06", ?B_TREE_10_10)),
    ?assertEqual("v_07", b_trees:get("k_07", ?B_TREE_10_10)),
    ?assertEqual("v_08", b_trees:get("k_08", ?B_TREE_10_10)),
    ?assertEqual("v_09", b_trees:get("k_09", ?B_TREE_10_10)),
    ?assertEqual("v_10", b_trees:get("k_10", ?B_TREE_10_10)),
    ?assertException(error, {key_not_found, "k_11"}, b_trees:get("k_11", ?B_TREE_10_10)),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", ?B_TREE_13_13)),
    ?assertEqual("v_01", b_trees:get("k_01", ?B_TREE_13_13)),
    ?assertEqual("v_02", b_trees:get("k_02", ?B_TREE_13_13)),
    ?assertEqual("v_03", b_trees:get("k_03", ?B_TREE_13_13)),
    ?assertEqual("v_04", b_trees:get("k_04", ?B_TREE_13_13)),
    ?assertEqual("v_05", b_trees:get("k_05", ?B_TREE_13_13)),
    ?assertEqual("v_06", b_trees:get("k_06", ?B_TREE_13_13)),
    ?assertEqual("v_07", b_trees:get("k_07", ?B_TREE_13_13)),
    ?assertEqual("v_08", b_trees:get("k_08", ?B_TREE_13_13)),
    ?assertEqual("v_09", b_trees:get("k_09", ?B_TREE_13_13)),
    ?assertEqual("v_10", b_trees:get("k_10", ?B_TREE_13_13)),
    ?assertEqual("v_11", b_trees:get("k_11", ?B_TREE_13_13)),
    ?assertEqual("v_12", b_trees:get("k_12", ?B_TREE_13_13)),
    ?assertEqual("v_13", b_trees:get("k_13", ?B_TREE_13_13)),
    ?assertException(error, {key_not_found, "k_14"}, b_trees:get("k_14", ?B_TREE_13_13)),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", ?B_TREE_16_16)),
    ?assertEqual("v_01", b_trees:get("k_01", ?B_TREE_16_16)),
    ?assertEqual("v_02", b_trees:get("k_02", ?B_TREE_16_16)),
    ?assertEqual("v_03", b_trees:get("k_03", ?B_TREE_16_16)),
    ?assertEqual("v_04", b_trees:get("k_04", ?B_TREE_16_16)),
    ?assertEqual("v_05", b_trees:get("k_05", ?B_TREE_16_16)),
    ?assertEqual("v_06", b_trees:get("k_06", ?B_TREE_16_16)),
    ?assertEqual("v_07", b_trees:get("k_07", ?B_TREE_16_16)),
    ?assertEqual("v_08", b_trees:get("k_08", ?B_TREE_16_16)),
    ?assertEqual("v_09", b_trees:get("k_09", ?B_TREE_16_16)),
    ?assertEqual("v_10", b_trees:get("k_10", ?B_TREE_16_16)),
    ?assertEqual("v_11", b_trees:get("k_11", ?B_TREE_16_16)),
    ?assertEqual("v_12", b_trees:get("k_12", ?B_TREE_16_16)),
    ?assertEqual("v_13", b_trees:get("k_13", ?B_TREE_16_16)),
    ?assertEqual("v_14", b_trees:get("k_14", ?B_TREE_16_16)),
    ?assertEqual("v_15", b_trees:get("k_15", ?B_TREE_16_16)),
    ?assertEqual("v_16", b_trees:get("k_16", ?B_TREE_16_16)),
    ?assertException(error, {key_not_found, "k_17"}, b_trees:get("k_17", ?B_TREE_16_16)),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", ?B_TREE_19_19)),
    ?assertEqual("v_01", b_trees:get("k_01", ?B_TREE_19_19)),
    ?assertEqual("v_02", b_trees:get("k_02", ?B_TREE_19_19)),
    ?assertEqual("v_03", b_trees:get("k_03", ?B_TREE_19_19)),
    ?assertEqual("v_04", b_trees:get("k_04", ?B_TREE_19_19)),
    ?assertEqual("v_05", b_trees:get("k_05", ?B_TREE_19_19)),
    ?assertEqual("v_06", b_trees:get("k_06", ?B_TREE_19_19)),
    ?assertEqual("v_07", b_trees:get("k_07", ?B_TREE_19_19)),
    ?assertEqual("v_08", b_trees:get("k_08", ?B_TREE_19_19)),
    ?assertEqual("v_09", b_trees:get("k_09", ?B_TREE_19_19)),
    ?assertEqual("v_10", b_trees:get("k_10", ?B_TREE_19_19)),
    ?assertEqual("v_11", b_trees:get("k_11", ?B_TREE_19_19)),
    ?assertEqual("v_12", b_trees:get("k_12", ?B_TREE_19_19)),
    ?assertEqual("v_13", b_trees:get("k_13", ?B_TREE_19_19)),
    ?assertEqual("v_14", b_trees:get("k_14", ?B_TREE_19_19)),
    ?assertEqual("v_15", b_trees:get("k_15", ?B_TREE_19_19)),
    ?assertEqual("v_16", b_trees:get("k_16", ?B_TREE_19_19)),
    ?assertEqual("v_17", b_trees:get("k_17", ?B_TREE_19_19)),
    ?assertEqual("v_18", b_trees:get("k_18", ?B_TREE_19_19)),
    ?assertEqual("v_19", b_trees:get("k_19", ?B_TREE_19_19)),
    ?assertException(error, {key_not_found, "k_20"}, b_trees:get("k_20", ?B_TREE_19_19)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: height
%%--------------------------------------------------------------------

height_test() ->
    ?assertEqual(0, b_trees:height(?B_TREE_05_00)),
    ?assertEqual(1, b_trees:height(?B_TREE_05_02)),
    ?assertEqual(2, b_trees:height(?B_TREE_05_05)),
    ?assertEqual(3, b_trees:height(?B_TREE_05_16)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert - error
%%--------------------------------------------------------------------

insert_error_test() ->
    ?assertException(error, {key_exists, "k_01"}, b_trees:insert("k_01", "v_01", test_generator:generate_b_tree_from_number(5, 5, 2))),
    ?assertException(error, {key_exists, "k_02"}, b_trees:insert("k_02", "v_02", test_generator:generate_b_tree_from_number(5, 5, 2))),
    ?assertException(error, {key_exists, "k_03"}, b_trees:insert("k_03", "v_03", test_generator:generate_b_tree_from_number(5, 5, 2))),
    ?assertException(error, {key_exists, "k_04"}, b_trees:insert("k_04", "v_04", test_generator:generate_b_tree_from_number(5, 5, 2))),
    ?assertException(error, {key_exists, "k_05"}, b_trees:insert("k_05", "v_05", test_generator:generate_b_tree_from_number(5, 5, 2))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert - order 5
%%--------------------------------------------------------------------

insert_b_tree_order_5_test() ->
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
%% TEST CASES: insert - order 7
%%--------------------------------------------------------------------

insert_b_tree_order_7_test() ->
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
%% TEST CASES: insert - order 8
%%--------------------------------------------------------------------

insert_b_tree_order_8_test() ->
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
%% TEST CASES: is_defined
%%--------------------------------------------------------------------

is_defined_test() ->
    ?assertNot(b_trees:is_defined("k_00", ?B_TREE_04_00)),

    ?assertNot(b_trees:is_defined("k_00", ?B_TREE_04_04)),
    ?assert(b_trees:is_defined("k_01", ?B_TREE_04_04)),
    ?assert(b_trees:is_defined("k_02", ?B_TREE_04_04)),
    ?assert(b_trees:is_defined("k_03", ?B_TREE_04_04)),
    ?assert(b_trees:is_defined("k_04", ?B_TREE_04_04)),
    ?assertNot(b_trees:is_defined("k_05", ?B_TREE_04_04)),

    ?assertNot(b_trees:is_defined("k_00", ?B_TREE_07_07)),
    ?assert(b_trees:is_defined("k_01", ?B_TREE_07_07)),
    ?assert(b_trees:is_defined("k_02", ?B_TREE_07_07)),
    ?assert(b_trees:is_defined("k_03", ?B_TREE_07_07)),
    ?assert(b_trees:is_defined("k_04", ?B_TREE_07_07)),
    ?assert(b_trees:is_defined("k_05", ?B_TREE_07_07)),
    ?assert(b_trees:is_defined("k_06", ?B_TREE_07_07)),
    ?assert(b_trees:is_defined("k_07", ?B_TREE_07_07)),
    ?assertNot(b_trees:is_defined("k_08", ?B_TREE_07_07)),

    ?assertNot(b_trees:is_defined("k_00", ?B_TREE_10_10)),
    ?assert(b_trees:is_defined("k_01", ?B_TREE_10_10)),
    ?assert(b_trees:is_defined("k_02", ?B_TREE_10_10)),
    ?assert(b_trees:is_defined("k_03", ?B_TREE_10_10)),
    ?assert(b_trees:is_defined("k_04", ?B_TREE_10_10)),
    ?assert(b_trees:is_defined("k_05", ?B_TREE_10_10)),
    ?assert(b_trees:is_defined("k_06", ?B_TREE_10_10)),
    ?assert(b_trees:is_defined("k_07", ?B_TREE_10_10)),
    ?assert(b_trees:is_defined("k_08", ?B_TREE_10_10)),
    ?assert(b_trees:is_defined("k_09", ?B_TREE_10_10)),
    ?assert(b_trees:is_defined("k_10", ?B_TREE_10_10)),
    ?assertNot(b_trees:is_defined("k_11", ?B_TREE_10_10)),

    ?assertNot(b_trees:is_defined("k_00", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_01", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_02", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_03", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_04", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_05", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_06", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_07", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_08", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_09", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_10", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_11", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_12", ?B_TREE_13_13)),
    ?assert(b_trees:is_defined("k_13", ?B_TREE_13_13)),
    ?assertNot(b_trees:is_defined("k_14", ?B_TREE_13_13)),

    ?assertNot(b_trees:is_defined("k_00", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_01", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_02", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_03", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_04", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_05", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_06", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_07", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_08", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_09", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_10", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_11", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_12", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_13", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_14", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_15", ?B_TREE_16_16)),
    ?assert(b_trees:is_defined("k_16", ?B_TREE_16_16)),
    ?assertNot(b_trees:is_defined("k_17", ?B_TREE_16_16)),

    ?assertNot(b_trees:is_defined("k_00", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_01", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_02", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_03", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_04", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_05", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_06", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_07", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_08", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_09", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_10", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_11", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_12", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_13", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_14", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_15", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_16", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_17", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_18", ?B_TREE_19_19)),
    ?assert(b_trees:is_defined("k_19", ?B_TREE_19_19)),
    ?assertNot(b_trees:is_defined("k_20", ?B_TREE_19_19)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_empty
%%--------------------------------------------------------------------

is_empty_test() ->
    ?assertEqual(false, b_trees:is_empty(?B_TREE_33_01)),
    ?assertEqual(true, b_trees:is_empty(?B_TREE_33_00)),
    ?assertEqual(true, b_trees:is_empty(?B_STAR_TREE_07_00)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys
%%--------------------------------------------------------------------

keys_test() ->
    ?assertEqual([], b_trees:keys(?B_TREE_05_00)),
    ?assertEqual(["k_01"], b_trees:keys(?B_TREE_05_01)),
    ?assertEqual(2, length(b_trees:keys(?B_TREE_05_02))),
    ?assertEqual(5, length(b_trees:keys(?B_TREE_05_05))),
    ?assertEqual(9, length(b_trees:keys(?B_TREE_05_09))),
    ?assertEqual(16, length(b_trees:keys(?B_TREE_05_16))),
    ?assertEqual(80, length(b_trees:keys(?B_TREE_07_80))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest
%%--------------------------------------------------------------------

largest_test() ->
    ?assertException(error, {empty_tree, _}, b_trees:largest(?B_TREE_05_00)),
    ?assertEqual({"k_01", "v_01"}, b_trees:largest(?B_TREE_05_01)),
    ?assertEqual({"k_02", "v_02"}, b_trees:largest(?B_TREE_05_02)),
    ?assertEqual({"k_05", "v_05"}, b_trees:largest(?B_TREE_05_05)),
    ?assertEqual({"k_09", "v_09"}, b_trees:largest(?B_TREE_05_09)),
    ?assertEqual({"k_16", "v_16"}, b_trees:largest(?B_TREE_05_16)),
    ?assertEqual({"k_19", "v_19"}, b_trees:largest(?B_TREE_19_19)),
    ?assertEqual({"k_967", "v_967"}, b_trees:largest(?B_TREE_07_80)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup
%%--------------------------------------------------------------------

lookup_test() ->
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
%% TEST CASES: map - order 4
%%--------------------------------------------------------------------

map_b_tree_order_4_test() ->
    ?assertEqual(?B_TREE_04_15_UPDATE, b_trees:map(fun map_value_to_new/2, ?B_TREE_04_15)),

    ok.

map_value_to_new(_, Value) ->
    Value ++ "_new".

%%--------------------------------------------------------------------
%% TEST CASES: map - error
%%--------------------------------------------------------------------

map_error_test() ->
    ?assertException(error, {empty_tree, ?B_TREE_05_00}, b_trees:map(fun map_value_to_new/2, ?B_TREE_05_00)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: number_key_values
%%--------------------------------------------------------------------

number_key_values_test() ->
    ?assertEqual(0, b_trees:number_key_values(?B_TREE_05_00)),
    ?assertEqual(2, b_trees:number_key_values(?B_TREE_05_02)),
    ?assertEqual(5, b_trees:number_key_values(?B_TREE_05_05)),
    ?assertEqual(16, b_trees:number_key_values(?B_TREE_05_16)),
    ?assertEqual(29, b_trees:number_key_values(?B_TREE_05_29)),
    ?assertEqual(19, b_trees:number_key_values(?B_TREE_19_19)),
    ?assertEqual(80, b_trees:number_key_values(?B_TREE_07_80)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: size
%%--------------------------------------------------------------------

size_test() ->
    ?assertEqual(0, b_trees:size(b_trees:empty(5))),
    ?assertEqual(1, b_trees:size(?B_TREE_05_01)),
    ?assertEqual(3, b_trees:size(?B_TREE_05_05)),
    ?assertEqual(5, b_trees:size(?B_TREE_05_11)),
    ?assertEqual(8, b_trees:size(?B_TREE_05_16)),
    ?assertEqual(10, b_trees:size(?B_TREE_05_21)),
    ?assertEqual(0, b_trees:size(b_trees:empty(5, b_star))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest
%%--------------------------------------------------------------------

smallest_test() ->
    ?assertException(error, {empty_tree, _}, b_trees:smallest(?B_TREE_05_00)),
    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(?B_TREE_05_01)),
    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(?B_TREE_05_02)),
    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(?B_TREE_05_05)),
    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(?B_TREE_05_09)),
    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(?B_TREE_05_16)),
    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(?B_TREE_19_19)),
    ?assertEqual({"k_011", "v_011"}, b_trees:smallest(?B_TREE_07_80)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list
%%--------------------------------------------------------------------

to_list_test() ->
    ?assertException(error, {empty_tree, _}, b_trees:to_list(?B_TREE_05_00)),
    ?assertEqual([{"k_01", "v_01"}], b_trees:to_list(?B_TREE_05_01)),
    ?assertEqual(2, length(b_trees:to_list(?B_TREE_05_02))),
    ?assertEqual(5, length(b_trees:to_list(?B_TREE_05_05))),
    ?assertEqual(9, length(b_trees:to_list(?B_TREE_05_09))),
    ?assertEqual(16, length(b_trees:to_list(?B_TREE_05_16))),
    ?assertEqual(80, length(b_trees:to_list(?B_TREE_07_80))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update - order 4
%%--------------------------------------------------------------------

update_b_tree_order_4_test() ->
    BTree_04_15_K_01 = b_trees:update("k_01", "v_01_new", ?B_TREE_04_15),
    BTree_04_15_K_02 = b_trees:update("k_02", "v_02_new", BTree_04_15_K_01),
    BTree_04_15_K_03 = b_trees:update("k_03", "v_03_new", BTree_04_15_K_02),
    BTree_04_15_K_04 = b_trees:update("k_04", "v_04_new", BTree_04_15_K_03),
    BTree_04_15_K_05 = b_trees:update("k_05", "v_05_new", BTree_04_15_K_04),
    BTree_04_15_K_06 = b_trees:update("k_06", "v_06_new", BTree_04_15_K_05),
    BTree_04_15_K_07 = b_trees:update("k_07", "v_07_new", BTree_04_15_K_06),
    BTree_04_15_K_08 = b_trees:update("k_08", "v_08_new", BTree_04_15_K_07),
    BTree_04_15_K_09 = b_trees:update("k_09", "v_09_new", BTree_04_15_K_08),
    BTree_04_15_K_10 = b_trees:update("k_10", "v_10_new", BTree_04_15_K_09),
    BTree_04_15_K_11 = b_trees:update("k_11", "v_11_new", BTree_04_15_K_10),
    BTree_04_15_K_12 = b_trees:update("k_12", "v_12_new", BTree_04_15_K_11),
    BTree_04_15_K_13 = b_trees:update("k_13", "v_13_new", BTree_04_15_K_12),
    BTree_04_15_K_14 = b_trees:update("k_14", "v_14_new", BTree_04_15_K_13),
    ?assertEqual(?B_TREE_04_15_UPDATE, b_trees:update("k_15", "v_15_new", BTree_04_15_K_14)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update - error
%%--------------------------------------------------------------------

update_error_test() ->
    ?assertException(error, {empty_tree, ?B_TREE_05_00}, b_trees:update("k_00", "v_00_new", ?B_TREE_05_00)),
    ?assertException(error, {key_not_found, "k_00"}, b_trees:update("k_00", "v_00_new", ?B_TREE_05_02)),
    ?assertException(error, {key_not_found, "k_00"}, b_trees:update("k_00", "v_00_new", ?B_TREE_05_29)),
    ?assertException(error, {key_not_found, "k_30"}, b_trees:update("k_30", "v_30_new", ?B_TREE_05_29)),
    ?assertException(error, {key_not_found, "k_000"}, b_trees:update("k_000", "v_000_new", ?B_TREE_07_80)),
    ?assertException(error, {key_not_found, "k_999"}, b_trees:update("k_999", "v_999_new", ?B_TREE_07_80)),
    ?assertException(error, {key_not_found, "k_00"}, b_trees:update("k_00", "v_00_new", ?B_TREE_08_64)),
    ?assertException(error, {key_not_found, "k_65"}, b_trees:update("k_65", "v_65_new", ?B_TREE_08_64)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values
%%--------------------------------------------------------------------

values_test() ->
    ?assertEqual([], b_trees:values(?B_TREE_05_00)),
    ?assertEqual(["v_01"], b_trees:values(?B_TREE_05_01)),
    ?assertEqual(2, length(b_trees:values(?B_TREE_05_02))),
    ?assertEqual(5, length(b_trees:values(?B_TREE_05_05))),
    ?assertEqual(9, length(b_trees:values(?B_TREE_05_09))),
    ?assertEqual(16, length(b_trees:values(?B_TREE_05_16))),
    ?assertEqual(80, length(b_trees:values(?B_TREE_07_80))),

    ok.
