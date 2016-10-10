%%%-------------------------------------------------------------------
%%% File        : b_trees_SUITE.erl
%%% Description : Test Suite for module: b_trees.
%%%
%%% Created     : 09.09.2016
%%%-------------------------------------------------------------------
-module(b_trees_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - SUITE
%%--------------------------------------------------------------------

suite() ->
    [
        {timetrap, {minutes, 10}}
    ].

init_per_suite(Config) ->
    [
        {gbtree, test_generator:generate_gb_tree_from_number(2000, 4)},
        {btree_3, test_generator:generate_b_tree_from_number(3, 2000, 4)},
        {btree_5, test_generator:generate_b_tree_from_number(5, 2000, 4)},
        {btree_9, test_generator:generate_b_tree_from_number(9, 2000, 4)},
        {btree_17, test_generator:generate_b_tree_from_number(17, 2000, 4)},
        {btree_33, test_generator:generate_b_tree_from_number(33, 2000, 4)},
        {btree_65, test_generator:generate_b_tree_from_number(65, 2000, 4)},
        {btree_129, test_generator:generate_b_tree_from_number(129, 2000, 4)},
        {btree_257, test_generator:generate_b_tree_from_number(257, 2000, 4)},
        {lookUps, test_generator:generate_keys_rand(2000, 10000, 4)}
        | Config
    ].

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%--------------------------------------------------------------------

all() ->
    [
        binary_search_test,
        empty_test,
        insert_b_tree_order_3_test,
        insert_b_tree_order_5_test,
        insert_key_exists_test,
        is_empty_test,
        lookup_test,
        lookup_nodes_test,
        performance_insert_b_tree_order_3_test,
        performance_insert_b_tree_order_5_test,
        performance_insert_b_tree_order_9_test,
        performance_insert_b_tree_order_17_test,
        performance_insert_b_tree_order_33_test,
        performance_insert_b_tree_order_65_test,
        performance_insert_b_tree_order_129_test,
        performance_insert_b_tree_order_257_test,
        performance_insert_gb_tree_test,
        performance_lookup_b_tree_order_3_test,
        performance_lookup_b_tree_order_5_test,
        performance_lookup_b_tree_order_9_test,
        performance_lookup_b_tree_order_17_test,
        performance_lookup_b_tree_order_33_test,
        performance_lookup_b_tree_order_65_test,
        performance_lookup_b_tree_order_129_test,
        performance_lookup_b_tree_order_257_test,
        performance_lookup_gb_tree_test,
        size_test
    ].

%%--------------------------------------------------------------------
%% TEST CASES: binary_search
%%--------------------------------------------------------------------

binary_search_test(_Config) ->
    _Node_1 = [{0, a}],
    ?assertEqual({none, 1}, b_trees:binary_search(_Node_1, -1, 1, 1)),
    ?assertEqual({a, 1}, b_trees:binary_search(_Node_1, 0, 1, 1)),
    ?assertEqual({none, 2}, b_trees:binary_search(_Node_1, 1, 1, 1)),

    _Node_2 = [{0, a}, {2, b}],
    ?assertEqual({none, 1}, b_trees:binary_search(_Node_2, -1, 1, 2)),
    ?assertEqual({a, 1}, b_trees:binary_search(_Node_2, 0, 1, 2)),
    ?assertEqual({none, 2}, b_trees:binary_search(_Node_2, 1, 1, 2)),
    ?assertEqual({b, 2}, b_trees:binary_search(_Node_2, 2, 1, 2)),
    ?assertEqual({none, 3}, b_trees:binary_search(_Node_2, 3, 1, 2)),

    _Node_3 = [{0, a}, {2, b}, {4, c}],
    ?assertEqual({none, 1}, b_trees:binary_search(_Node_3, -1, 1, 3)),
    ?assertEqual({a, 1}, b_trees:binary_search(_Node_3, 0, 1, 3)),
    ?assertEqual({none, 2}, b_trees:binary_search(_Node_3, 1, 1, 3)),
    ?assertEqual({b, 2}, b_trees:binary_search(_Node_3, 2, 1, 3)),
    ?assertEqual({none, 3}, b_trees:binary_search(_Node_3, 3, 1, 3)),
    ?assertEqual({c, 3}, b_trees:binary_search(_Node_3, 4, 1, 3)),
    ?assertEqual({none, 4}, b_trees:binary_search(_Node_3, 5, 1, 3)),

    _Node_4 = [{0, a}, {2, b}, {4, c}, {6, d}],
    ?assertEqual({none, 1}, b_trees:binary_search(_Node_4, -1, 1, 4)),
    ?assertEqual({a, 1}, b_trees:binary_search(_Node_4, 0, 1, 4)),
    ?assertEqual({none, 2}, b_trees:binary_search(_Node_4, 1, 1, 4)),
    ?assertEqual({b, 2}, b_trees:binary_search(_Node_4, 2, 1, 4)),
    ?assertEqual({none, 3}, b_trees:binary_search(_Node_4, 3, 1, 4)),
    ?assertEqual({c, 3}, b_trees:binary_search(_Node_4, 4, 1, 4)),
    ?assertEqual({none, 4}, b_trees:binary_search(_Node_4, 5, 1, 4)),
    ?assertEqual({d, 4}, b_trees:binary_search(_Node_4, 6, 1, 4)),
    ?assertEqual({none, 5}, b_trees:binary_search(_Node_4, 7, 1, 4)),

    _Node_5 = [{0, a}, {2, b}, {4, c}, {6, d}, {8, e}],
    ?assertEqual({none, 1}, b_trees:binary_search(_Node_5, -1, 1, 5)),
    ?assertEqual({a, 1}, b_trees:binary_search(_Node_5, 0, 1, 5)),
    ?assertEqual({none, 2}, b_trees:binary_search(_Node_5, 1, 1, 5)),
    ?assertEqual({b, 2}, b_trees:binary_search(_Node_5, 2, 1, 5)),
    ?assertEqual({none, 3}, b_trees:binary_search(_Node_5, 3, 1, 5)),
    ?assertEqual({c, 3}, b_trees:binary_search(_Node_5, 4, 1, 5)),
    ?assertEqual({none, 4}, b_trees:binary_search(_Node_5, 5, 1, 5)),
    ?assertEqual({d, 4}, b_trees:binary_search(_Node_5, 6, 1, 5)),
    ?assertEqual({none, 5}, b_trees:binary_search(_Node_5, 7, 1, 5)),
    ?assertEqual({e, 5}, b_trees:binary_search(_Node_5, 8, 1, 5)),
    ?assertEqual({none, 6}, b_trees:binary_search(_Node_5, 9, 1, 5)).

%%--------------------------------------------------------------------
%% TEST CASES: empty
%%--------------------------------------------------------------------

empty_test(_Config) ->
    _BTree_7_0 = b_trees:empty(7),
    ?assertEqual({[], 3, 6}, _BTree_7_0),
    ?assert(b_trees:is_empty(_BTree_7_0)),
    ?assertEqual(0, b_trees:size(_BTree_7_0)),

    _BStarTree_7_0 = b_trees:empty(7, b_star),
    ?assertEqual({[], 4, 6}, _BStarTree_7_0),
    ?assert(b_trees:is_empty(_BStarTree_7_0)),
    ?assertEqual(0, b_trees:size(_BStarTree_7_0)).

%%--------------------------------------------------------------------
%% TEST CASES: insert key exists
%%--------------------------------------------------------------------

insert_key_exists_test(_Config) ->
    BTree = test_generator:generate_b_tree_from_number(5, 5, 2),
    ?assertException(error, {key_exists, "k_02"}, b_trees:insert("k_02", "v_02", BTree)).

%%--------------------------------------------------------------------
%% TEST CASES: insert order 3
%%--------------------------------------------------------------------

insert_b_tree_order_3_test(_Config) ->
    ?assertEqual(b_trees:empty(3), test_generator:generate_b_tree_from_number(3, 0, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}]}
    ], 1, 2}, test_generator:generate_b_tree_from_number(3, 1, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]}
    ], 1, 2}, test_generator:generate_b_tree_from_number(3, 2, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3], [{"k_02", "v_02"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}]},
        {3, 2, 1, [], [{"k_03", "v_03"}]}
    ], 1, 2}, test_generator:generate_b_tree_from_number(3, 3, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3, 4], [{"k_02", "v_02"}, {"k_04", "v_04"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}]},
        {3, 2, 1, [], [{"k_03", "v_03"}]},
        {4, 2, 1, [], [{"k_05", "v_05"}]}
    ], 1, 2}, test_generator:generate_b_tree_from_number(3, 5, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3], [{"k_04", "v_04"}]},
        {2, 2, 1, [4, 5], [{"k_02", "v_02"}]},
        {3, 2, 1, [6, 7], [{"k_06", "v_06"}]},
        {4, 3, 2, [], [{"k_01", "v_01"}]},
        {5, 3, 2, [], [{"k_03", "v_03"}]},
        {6, 3, 3, [], [{"k_05", "v_05"}]},
        {7, 3, 3, [], [{"k_07", "v_07"}]}
    ], 1, 2}, test_generator:generate_b_tree_from_number(3, 7, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3], [{"k_04", "v_04"}]},
        {2, 2, 1, [4, 5], [{"k_02", "v_02"}]},
        {3, 2, 1, [6, 7, 8], [{"k_06", "v_06"}, {"k_08", "v_08"}]},
        {4, 3, 2, [], [{"k_01", "v_01"}]},
        {5, 3, 2, [], [{"k_03", "v_03"}]},
        {6, 3, 3, [], [{"k_05", "v_05"}]},
        {7, 3, 3, [], [{"k_07", "v_07"}]},
        {8, 3, 3, [], [{"k_09", "v_09"}]}
    ], 1, 2}, test_generator:generate_b_tree_from_number(3, 9, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3, 4], [{"k_04", "v_04"}, {"k_08", "v_08"}]},
        {2, 2, 1, [5, 6], [{"k_02", "v_02"}]},
        {3, 2, 1, [7, 8], [{"k_06", "v_06"}]},
        {4, 2, 1, [9, 10], [{"k_10", "v_10"}]},
        {5, 3, 2, [], [{"k_01", "v_01"}]},
        {6, 3, 2, [], [{"k_03", "v_03"}]},
        {7, 3, 3, [], [{"k_05", "v_05"}]},
        {8, 3, 3, [], [{"k_07", "v_07"}]},
        {9, 3, 4, [], [{"k_09", "v_09"}]},
        {10, 3, 4, [], [{"k_11", "v_11"}]}
    ], 1, 2}, test_generator:generate_b_tree_from_number(3, 11, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3, 4], [{"k_04", "v_04"}, {"k_08", "v_08"}]},
        {2, 2, 1, [5, 6], [{"k_02", "v_02"}]},
        {3, 2, 1, [7, 8], [{"k_06", "v_06"}]},
        {4, 2, 1, [9, 10, 11], [{"k_10", "v_10"}, {"k_12", "v_12"}]},
        {5, 3, 2, [], [{"k_01", "v_01"}]},
        {6, 3, 2, [], [{"k_03", "v_03"}]},
        {7, 3, 3, [], [{"k_05", "v_05"}]},
        {8, 3, 3, [], [{"k_07", "v_07"}]},
        {9, 3, 4, [], [{"k_09", "v_09"}]},
        {10, 3, 4, [], [{"k_11", "v_11"}]},
        {11, 3, 4, [], [{"k_13", "v_13"}]}
    ], 1, 2}, test_generator:generate_b_tree_from_number(3, 13, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3], [{"k_08", "v_08"}]},
        {2, 2, 1, [4, 5], [{"k_04", "v_04"}]},
        {3, 2, 1, [6, 7], [{"k_12", "v_12"}]},
        {4, 3, 2, [8, 9], [{"k_02", "v_02"}]},
        {5, 3, 2, [10, 11], [{"k_06", "v_06"}]},
        {6, 3, 3, [12, 13], [{"k_10", "v_10"}]},
        {7, 3, 3, [14, 15], [{"k_14", "v_14"}]},
        {8, 4, 4, [], [{"k_01", "v_01"}]},
        {9, 4, 4, [], [{"k_03", "v_03"}]},
        {10, 4, 5, [], [{"k_05", "v_05"}]},
        {11, 4, 5, [], [{"k_07", "v_07"}]},
        {12, 4, 6, [], [{"k_09", "v_09"}]},
        {13, 4, 6, [], [{"k_11", "v_11"}]},
        {14, 4, 7, [], [{"k_13", "v_13"}]},
        {15, 4, 7, [], [{"k_15", "v_15"}]}
    ], 1, 2}, test_generator:generate_b_tree_from_number(3, 15, 2)).

%%--------------------------------------------------------------------
%% TEST CASES: insert order 5
%%--------------------------------------------------------------------

insert_b_tree_order_5_test(_Config) ->
    ?assertEqual(b_trees:empty(5), test_generator:generate_b_tree_from_number(5, 0, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}]}
    ], 2, 4}, test_generator:generate_b_tree_from_number(5, 1, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]}
    ], 2, 4}, test_generator:generate_b_tree_from_number(5, 2, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}, {"k_02", "v_02"}, {"k_03", "v_03"}]}
    ], 2, 4}, test_generator:generate_b_tree_from_number(5, 3, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}, {"k_02", "v_02"}, {"k_03", "v_03"}, {"k_04", "v_04"}]}
    ], 2, 4}, test_generator:generate_b_tree_from_number(5, 4, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3], [{"k_03", "v_03"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]},
        {3, 2, 1, [], [{"k_04", "v_04"}, {"k_05", "v_05"}]}
    ], 2, 4}, test_generator:generate_b_tree_from_number(5, 5, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3, 4], [{"k_03", "v_03"}, {"k_06", "v_06"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]},
        {3, 2, 1, [], [{"k_04", "v_04"}, {"k_05", "v_05"}]},
        {4, 2, 1, [], [{"k_07", "v_07"}, {"k_08", "v_08"}]}
    ], 2, 4}, test_generator:generate_b_tree_from_number(5, 8, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3, 4, 5], [{"k_03", "v_03"}, {"k_06", "v_06"}, {"k_09", "v_09"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]},
        {3, 2, 1, [], [{"k_04", "v_04"}, {"k_05", "v_05"}]},
        {4, 2, 1, [], [{"k_07", "v_07"}, {"k_08", "v_08"}]},
        {5, 2, 1, [], [{"k_10", "v_10"}, {"k_11", "v_11"}]}
    ], 2, 4}, test_generator:generate_b_tree_from_number(5, 11, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3, 4, 5, 6], [{"k_03", "v_03"}, {"k_06", "v_06"}, {"k_09", "v_09"}, {"k_12", "v_12"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]},
        {3, 2, 1, [], [{"k_04", "v_04"}, {"k_05", "v_05"}]},
        {4, 2, 1, [], [{"k_07", "v_07"}, {"k_08", "v_08"}]},
        {5, 2, 1, [], [{"k_10", "v_10"}, {"k_11", "v_11"}]},
        {6, 2, 1, [], [{"k_13", "v_13"}, {"k_14", "v_14"}]}
    ], 2, 4}, test_generator:generate_b_tree_from_number(5, 14, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3], [{"k_09", "v_09"}]},
        {2, 2, 1, [4, 5, 6], [{"k_03", "v_03"}, {"k_06", "v_06"}]},
        {3, 2, 1, [7, 8, 9], [{"k_12", "v_12"}, {"k_15", "v_15"}]},
        {4, 3, 2, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]},
        {5, 3, 2, [], [{"k_04", "v_04"}, {"k_05", "v_05"}]},
        {6, 3, 2, [], [{"k_07", "v_07"}, {"k_08", "v_08"}]},
        {7, 3, 3, [], [{"k_10", "v_10"}, {"k_11", "v_11"}]},
        {8, 3, 3, [], [{"k_13", "v_13"}, {"k_14", "v_14"}]},
        {9, 3, 3, [], [{"k_16", "v_16"}, {"k_17", "v_17"}]}
    ], 2, 4}, test_generator:generate_b_tree_from_number(5, 17, 2)),

    ?assertEqual(test_generator:generate_b_tree_from_number(5, 5, 2), test_generator:generate_b_tree_till_number(5, 5, 2)),
    ?assertEqual(test_generator:generate_b_tree_from_number(5, 8, 2), test_generator:generate_b_tree_till_number(5, 8, 2)),
    ?assertEqual(test_generator:generate_b_tree_from_number(5, 11, 2), test_generator:generate_b_tree_till_number(5, 11, 2)),
    ?assertEqual(test_generator:generate_b_tree_from_number(5, 17, 2), test_generator:generate_b_tree_till_number(5, 17, 2)),
    ?assertEqual(test_generator:generate_b_tree_from_number(5, 26, 2), test_generator:generate_b_tree_till_number(5, 26, 2)),
    ?assertEqual(test_generator:generate_b_tree_from_number(5, 35, 2), test_generator:generate_b_tree_till_number(5, 35, 2)),
    ?assertEqual(test_generator:generate_b_tree_from_number(5, 44, 2), test_generator:generate_b_tree_till_number(5, 44, 2)).

%%--------------------------------------------------------------------
%% TEST CASES: is_empty
%%--------------------------------------------------------------------

is_empty_test(_Config) ->
    _BTree_1 = {
        [
            {1, 1, 0, [], [{k_2, v_2}]}
        ], 3, 6
    },
    ?assertEqual(false, b_trees:is_empty(_BTree_1)),

    _BTree_7_0 = b_trees:empty(7),
    ?assertEqual(true, b_trees:is_empty(_BTree_7_0)),

    _BStarTree_7_0 = b_trees:empty(6, b_star),
    ?assertEqual(true, b_trees:is_empty(_BStarTree_7_0)).

%%--------------------------------------------------------------------
%% TEST CASES: lookup
%%--------------------------------------------------------------------

lookup_test(_Config) ->
    _BTree_6_0 = b_trees:empty(7),
    ?assertEqual(none, b_trees:lookup('k_1', _BTree_6_0)),

    _BTree_1 = {
        [
            {1, 1, 0, [], [{k_2, v_2}]}
        ], 3, 6
    },
    ?assertEqual(none, b_trees:lookup('k_1', _BTree_1)),
    ?assertEqual({value, 'v_2'}, b_trees:lookup('k_2', _BTree_1)),
    ?assertEqual(none, b_trees:lookup('k_3', _BTree_1)),

    _BTree_3 = {
        [
            {1, 1, 0, [2, 3, 4], [{k_07, v_07}, {k_16, v_16}]},
            {2, 1, 1, [], [{k_01, v_01}, {k_02, v_02}, {k_05, v_05}, {k_06, v_06}]},
            {2, 2, 1, [], [{k_09, v_09}, {k_12, v_12}]},
            {2, 3, 1, [], [{k_18, v_18}, {k_21, v_21}]}
        ], 3, 6
    },
    ?assertEqual(none, b_trees:lookup('k_00', _BTree_3)),
    ?assertEqual({value, 'v_01'}, b_trees:lookup('k_01', _BTree_3)),
    ?assertEqual({value, 'v_02'}, b_trees:lookup('k_02', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_03', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_04', _BTree_3)),
    ?assertEqual({value, 'v_05'}, b_trees:lookup('k_05', _BTree_3)),
    ?assertEqual({value, 'v_06'}, b_trees:lookup('k_06', _BTree_3)),
    ?assertEqual({value, 'v_07'}, b_trees:lookup('k_07', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_08', _BTree_3)),
    ?assertEqual({value, 'v_09'}, b_trees:lookup('k_09', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_10', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_11', _BTree_3)),
    ?assertEqual({value, 'v_12'}, b_trees:lookup('k_12', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_13', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_14', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_15', _BTree_3)),
    ?assertEqual({value, 'v_16'}, b_trees:lookup('k_16', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_17', _BTree_3)),
    ?assertEqual({value, 'v_18'}, b_trees:lookup('k_18', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_19', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_20', _BTree_3)),
    ?assertEqual({value, 'v_21'}, b_trees:lookup('k_21', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_22', _BTree_3)).

%%--------------------------------------------------------------------
%% TEST CASES: lookup_nodes
%%--------------------------------------------------------------------

lookup_nodes_test(_Config) ->
    _BTree_1 = {
        [
            {1, 1, 0, [], [{k_2, v_2}]}
        ], 3, 6
    },
    ?assertEqual(none, b_trees:lookup_nodes(1, 'k_1', _BTree_1)),
    ?assertEqual({value, 'v_2'}, b_trees:lookup_nodes(1, 'k_2', _BTree_1)),
    ?assertEqual(none, b_trees:lookup_nodes(1, 'k_3', _BTree_1)),

    _BTree_3 = {
        [
            {1, 1, 0, [2, 3, 4], [{k_07, v_07}, {k_16, v_16}]},
            {2, 1, 1, [], [{k_01, v_01}, {k_02, v_02}, {k_05, v_05}, {k_06, v_06}]},
            {2, 2, 1, [], [{k_09, v_09}, {k_12, v_12}]},
            {2, 3, 1, [], [{k_18, v_18}, {k_21, v_21}]}
        ], 3, 6
    },
    ?assertEqual(none, b_trees:lookup_nodes(2, 'k_00', _BTree_3)),
    ?assertEqual({value, 'v_01'}, b_trees:lookup_nodes(2, 'k_01', _BTree_3)),
    ?assertEqual({value, 'v_02'}, b_trees:lookup_nodes(2, 'k_02', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(2, 'k_03', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(2, 'k_04', _BTree_3)),
    ?assertEqual({value, 'v_05'}, b_trees:lookup_nodes(2, 'k_05', _BTree_3)),
    ?assertEqual({value, 'v_06'}, b_trees:lookup_nodes(2, 'k_06', _BTree_3)),
    ?assertEqual({value, 'v_07'}, b_trees:lookup_nodes(1, 'k_07', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_08', _BTree_3)),
    ?assertEqual({value, 'v_09'}, b_trees:lookup_nodes(3, 'k_09', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_10', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_11', _BTree_3)),
    ?assertEqual({value, 'v_12'}, b_trees:lookup_nodes(3, 'k_12', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_13', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_14', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_15', _BTree_3)),
    ?assertEqual({value, 'v_16'}, b_trees:lookup_nodes(1, 'k_16', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(4, 'k_17', _BTree_3)),
    ?assertEqual({value, 'v_18'}, b_trees:lookup_nodes(4, 'k_18', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(4, 'k_19', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(4, 'k_20', _BTree_3)),
    ?assertEqual({value, 'v_21'}, b_trees:lookup_nodes(4, 'k_21', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(4, 'k_22', _BTree_3)).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 129
%%--------------------------------------------------------------------

performance_insert_b_tree_order_129_test(_Config) ->
    test_generator:generate_b_tree_from_number(129, 2000, 4).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 17
%%--------------------------------------------------------------------

performance_insert_b_tree_order_17_test(_Config) ->
    test_generator:generate_b_tree_from_number(17, 2000, 4).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 257
%%--------------------------------------------------------------------

performance_insert_b_tree_order_257_test(_Config) ->
    test_generator:generate_b_tree_from_number(257, 2000, 4).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 3
%%--------------------------------------------------------------------

performance_insert_b_tree_order_3_test(_Config) ->
    test_generator:generate_b_tree_from_number(3, 2000, 4).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 33
%%--------------------------------------------------------------------

performance_insert_b_tree_order_33_test(_Config) ->
    test_generator:generate_b_tree_from_number(33, 2000, 4).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 5
%%--------------------------------------------------------------------

performance_insert_b_tree_order_5_test(_Config) ->
    test_generator:generate_b_tree_from_number(5, 2000, 4).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 65
%%--------------------------------------------------------------------

performance_insert_b_tree_order_65_test(_Config) ->
    test_generator:generate_b_tree_from_number(65, 2000, 4).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert b_tree order 9
%%--------------------------------------------------------------------

performance_insert_b_tree_order_9_test(_Config) ->
    test_generator:generate_b_tree_from_number(9, 2000, 4).

%%--------------------------------------------------------------------
%% TEST CASES: performance insert gb_tree
%%--------------------------------------------------------------------

performance_insert_gb_tree_test(_Config) ->
    test_generator:generate_gb_tree_from_number(2000, 4).

%%--------------------------------------------------------------------
%% TEST CASES: performance lookup b_tree order 129
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_129_test(Config) ->
    BTree = ?config(btree_129, Config),
    LookUps = ?config(lookUps, Config),
    lookup_b_tree(LookUps, BTree),
    ok.

lookup_b_tree([], _) ->
    none;
lookup_b_tree([Key | Tail], BTree) ->
    b_trees:lookup(Key, BTree),
    lookup_b_tree(Tail, BTree).

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
%% TEST CASES: performance lookup b_tree order 3
%%--------------------------------------------------------------------

performance_lookup_b_tree_order_3_test(Config) ->
    BTree = ?config(btree_3, Config),
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
    _BStarTree_7_0 = b_trees:empty(6, b_star),
    ?assertEqual(0, b_trees:size(_BStarTree_7_0)),

    _BTree_1 = {
        [
            {1, 1, 0, [], [{k_2, v_2}]}
        ], 3, 6
    },
    ?assertEqual(1, b_trees:size(_BTree_1)),

    _BTree_3 = {
        [
            {1, 1, 0, [2, 3, 4], [{k_07, v_07}, {k_16, v_16}]},
            {2, 1, 1, [], [{k_01, v_01}, {k_02, v_02}, {k_05, v_05}, {k_06, v_06}]},
            {2, 2, 1, [], [{k_09, v_09}, {k_12, v_12}]},
            {2, 3, 1, [], [{k_18, v_18}, {k_21, v_21}]}
        ], 3, 6
    },
    ?assertEqual(4, b_trees:size(_BTree_3)),

    _BTree_7_0 = b_trees:empty(7),
    ?assertEqual(0, b_trees:size(_BTree_7_0)).
