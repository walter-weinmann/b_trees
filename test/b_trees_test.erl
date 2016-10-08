-module(b_trees_test).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 60).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

binary_search_test() ->
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
    ?assertEqual({none, 6}, b_trees:binary_search(_Node_5, 9, 1, 5)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_test() ->
    _BTree_7_0 = b_trees:empty(7),
    ?assertEqual({[], 3, 6}, _BTree_7_0),
    ?assert(b_trees:is_empty(_BTree_7_0)),
    ?assertEqual(0, b_trees:size(_BTree_7_0)),

    _BStarTree_7_0 = b_trees:empty(7, b_star),
    ?assertEqual({[], 4, 6}, _BStarTree_7_0),
    ?assert(b_trees:is_empty(_BStarTree_7_0)),
    ?assertEqual(0, b_trees:size(_BStarTree_7_0)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_order_3_test() ->
    ?assertEqual(b_trees:empty(3), b_trees:generate_b_tree_from_number(3, 0, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}]}
    ], 1, 2}, b_trees:generate_b_tree_from_number(3, 1, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]}
    ], 1, 2}, b_trees:generate_b_tree_from_number(3, 2, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3], [{"k_02", "v_02"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}]},
        {3, 2, 1, [], [{"k_03", "v_03"}]}
    ], 1, 2}, b_trees:generate_b_tree_from_number(3, 3, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3, 4], [{"k_02", "v_02"}, {"k_04", "v_04"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}]},
        {3, 2, 1, [], [{"k_03", "v_03"}]},
        {4, 2, 1, [], [{"k_05", "v_05"}]}
    ], 1, 2}, b_trees:generate_b_tree_from_number(3, 5, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3], [{"k_04", "v_04"}]},
        {2, 2, 1, [4, 5], [{"k_02", "v_02"}]},
        {3, 2, 1, [6, 7], [{"k_06", "v_06"}]},
        {4, 3, 2, [], [{"k_01", "v_01"}]},
        {5, 3, 2, [], [{"k_03", "v_03"}]},
        {6, 3, 3, [], [{"k_05", "v_05"}]},
        {7, 3, 3, [], [{"k_07", "v_07"}]}
    ], 1, 2}, b_trees:generate_b_tree_from_number(3, 7, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3], [{"k_04", "v_04"}]},
        {2, 2, 1, [4, 5], [{"k_02", "v_02"}]},
        {3, 2, 1, [6, 7, 8], [{"k_06", "v_06"}, {"k_08", "v_08"}]},
        {4, 3, 2, [], [{"k_01", "v_01"}]},
        {5, 3, 2, [], [{"k_03", "v_03"}]},
        {6, 3, 3, [], [{"k_05", "v_05"}]},
        {7, 3, 3, [], [{"k_07", "v_07"}]},
        {8, 3, 3, [], [{"k_09", "v_09"}]}
    ], 1, 2}, b_trees:generate_b_tree_from_number(3, 9, 2)),

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
    ], 1, 2}, b_trees:generate_b_tree_from_number(3, 11, 2)),

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
    ], 1, 2}, b_trees:generate_b_tree_from_number(3, 13, 2)),

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
    ], 1, 2}, b_trees:generate_b_tree_from_number(3, 15, 2)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_order_5_test() ->
    ?assertEqual(b_trees:empty(5), b_trees:generate_b_tree_from_number(5, 0, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}]}
    ], 2, 4}, b_trees:generate_b_tree_from_number(5, 1, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]}
    ], 2, 4}, b_trees:generate_b_tree_from_number(5, 2, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}, {"k_02", "v_02"}, {"k_03", "v_03"}]}
    ], 2, 4}, b_trees:generate_b_tree_from_number(5, 3, 2)),

    ?assertEqual({[
        {1, 1, 0, [], [{"k_01", "v_01"}, {"k_02", "v_02"}, {"k_03", "v_03"}, {"k_04", "v_04"}]}
    ], 2, 4}, b_trees:generate_b_tree_from_number(5, 4, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3], [{"k_03", "v_03"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]},
        {3, 2, 1, [], [{"k_04", "v_04"}, {"k_05", "v_05"}]}
    ], 2, 4}, b_trees:generate_b_tree_from_number(5, 5, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3, 4], [{"k_03", "v_03"}, {"k_06", "v_06"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]},
        {3, 2, 1, [], [{"k_04", "v_04"}, {"k_05", "v_05"}]},
        {4, 2, 1, [], [{"k_07", "v_07"}, {"k_08", "v_08"}]}
    ], 2, 4}, b_trees:generate_b_tree_from_number(5, 8, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3, 4, 5], [{"k_03", "v_03"}, {"k_06", "v_06"}, {"k_09", "v_09"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]},
        {3, 2, 1, [], [{"k_04", "v_04"}, {"k_05", "v_05"}]},
        {4, 2, 1, [], [{"k_07", "v_07"}, {"k_08", "v_08"}]},
        {5, 2, 1, [], [{"k_10", "v_10"}, {"k_11", "v_11"}]}
    ], 2, 4}, b_trees:generate_b_tree_from_number(5, 11, 2)),

    ?assertEqual({[
        {1, 1, 0, [2, 3, 4, 5, 6], [{"k_03", "v_03"}, {"k_06", "v_06"}, {"k_09", "v_09"}, {"k_12", "v_12"}]},
        {2, 2, 1, [], [{"k_01", "v_01"}, {"k_02", "v_02"}]},
        {3, 2, 1, [], [{"k_04", "v_04"}, {"k_05", "v_05"}]},
        {4, 2, 1, [], [{"k_07", "v_07"}, {"k_08", "v_08"}]},
        {5, 2, 1, [], [{"k_10", "v_10"}, {"k_11", "v_11"}]},
        {6, 2, 1, [], [{"k_13", "v_13"}, {"k_14", "v_14"}]}
    ], 2, 4}, b_trees:generate_b_tree_from_number(5, 14, 2)),

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
    ], 2, 4}, b_trees:generate_b_tree_from_number(5, 17, 2)),

    ?assertEqual(b_trees:generate_b_tree_from_number(5, 5, 2), b_trees:generate_b_tree_till_number(5, 5, 2)),
    ?assertEqual(b_trees:generate_b_tree_from_number(5, 8, 2), b_trees:generate_b_tree_till_number(5, 8, 2)),
    ?assertEqual(b_trees:generate_b_tree_from_number(5, 11, 2), b_trees:generate_b_tree_till_number(5, 11, 2)),
    ?assertEqual(b_trees:generate_b_tree_from_number(5, 17, 2), b_trees:generate_b_tree_till_number(5, 17, 2)),
    ?assertEqual(b_trees:generate_b_tree_from_number(5, 26, 2), b_trees:generate_b_tree_till_number(5, 26, 2)),
    ?assertEqual(b_trees:generate_b_tree_from_number(5, 35, 2), b_trees:generate_b_tree_till_number(5, 35, 2)),
    ?assertEqual(b_trees:generate_b_tree_from_number(5, 44, 2), b_trees:generate_b_tree_till_number(5, 44, 2)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_empty_test() ->
    _BTree_1 = {
        [
            {1, 1, 0, [], [{k_2, v_2}]}
        ], 3, 6
    },
    ?assertEqual(false, b_trees:is_empty(_BTree_1)),

    _BTree_7_0 = b_trees:empty(7),
    ?assertEqual(true, b_trees:is_empty(_BTree_7_0)),

    _BStarTree_7_0 = b_trees:empty(6, b_star),
    ?assertEqual(true, b_trees:is_empty(_BStarTree_7_0)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_test() ->
    _BTree_6_0 = b_trees:empty(7),
    ?assertEqual(none, b_trees:lookup('k_1', _BTree_6_0)),

    _BTree_1 = {
        [
            {1, 1, 0, [], [{k_2, v_2}]}
        ], 3, 6
    },
    ?assertEqual(none, b_trees:lookup('k_1', _BTree_1)),
    ?assertEqual('v_2', b_trees:lookup('k_2', _BTree_1)),
    ?assertEqual(none, b_trees:lookup('k_3', _BTree_1)),

    _BTree_3 = {
        [
            {1, 1, 0, [2, 3, 4], [{k_07, v_07}, {k_16, v_16}]},
            {2, 2, 1, [], [{k_01, v_01}, {k_02, v_02}, {k_05, v_05}, {k_06, v_06}]},
            {3, 2, 1, [], [{k_09, v_09}, {k_12, v_12}]},
            {4, 2, 1, [], [{k_18, v_18}, {k_21, v_21}]}
        ], 3, 6
    },
    ?assertEqual(none, b_trees:lookup('k_00', _BTree_3)),
    ?assertEqual('v_01', b_trees:lookup('k_01', _BTree_3)),
    ?assertEqual('v_02', b_trees:lookup('k_02', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_03', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_04', _BTree_3)),
    ?assertEqual('v_05', b_trees:lookup('k_05', _BTree_3)),
    ?assertEqual('v_06', b_trees:lookup('k_06', _BTree_3)),
    ?assertEqual('v_07', b_trees:lookup('k_07', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_08', _BTree_3)),
    ?assertEqual('v_09', b_trees:lookup('k_09', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_10', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_11', _BTree_3)),
    ?assertEqual('v_12', b_trees:lookup('k_12', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_13', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_14', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_15', _BTree_3)),
    ?assertEqual('v_16', b_trees:lookup('k_16', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_17', _BTree_3)),
    ?assertEqual('v_18', b_trees:lookup('k_18', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_19', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_20', _BTree_3)),
    ?assertEqual('v_21', b_trees:lookup('k_21', _BTree_3)),
    ?assertEqual(none, b_trees:lookup('k_22', _BTree_3)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_nodes_test() ->
    _BTree_1 = {
        [
            {1, 1, 0, [], [{k_2, v_2}]}
        ], 3, 6
    },
    ?assertEqual(none, b_trees:lookup_nodes(1, 'k_1', _BTree_1)),
    ?assertEqual('v_2', b_trees:lookup_nodes(1, 'k_2', _BTree_1)),
    ?assertEqual(none, b_trees:lookup_nodes(1, 'k_3', _BTree_1)),

    _BTree_3 = {
        [
            {1, 1, 0, [2, 3, 4], [{k_07, v_07}, {k_16, v_16}]},
            {2, 2, 1, [], [{k_01, v_01}, {k_02, v_02}, {k_05, v_05}, {k_06, v_06}]},
            {3, 2, 1, [], [{k_09, v_09}, {k_12, v_12}]},
            {4, 2, 1, [], [{k_18, v_18}, {k_21, v_21}]}
        ], 3, 6
    },
    ?assertEqual(none, b_trees:lookup_nodes(2, 'k_00', _BTree_3)),
    ?assertEqual('v_01', b_trees:lookup_nodes(2, 'k_01', _BTree_3)),
    ?assertEqual('v_02', b_trees:lookup_nodes(2, 'k_02', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(2, 'k_03', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(2, 'k_04', _BTree_3)),
    ?assertEqual('v_05', b_trees:lookup_nodes(2, 'k_05', _BTree_3)),
    ?assertEqual('v_06', b_trees:lookup_nodes(2, 'k_06', _BTree_3)),
    ?assertEqual('v_07', b_trees:lookup_nodes(1, 'k_07', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_08', _BTree_3)),
    ?assertEqual('v_09', b_trees:lookup_nodes(3, 'k_09', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_10', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_11', _BTree_3)),
    ?assertEqual('v_12', b_trees:lookup_nodes(3, 'k_12', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_13', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_14', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(3, 'k_15', _BTree_3)),
    ?assertEqual('v_16', b_trees:lookup_nodes(1, 'k_16', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(4, 'k_17', _BTree_3)),
    ?assertEqual('v_18', b_trees:lookup_nodes(4, 'k_18', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(4, 'k_19', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(4, 'k_20', _BTree_3)),
    ?assertEqual('v_21', b_trees:lookup_nodes(4, 'k_21', _BTree_3)),
    ?assertEqual(none, b_trees:lookup_nodes(4, 'k_22', _BTree_3)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size_test() ->
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
            {2, 2, 1, [], [{k_01, v_01}, {k_02, v_02}, {k_05, v_05}, {k_06, v_06}]},
            {3, 2, 1, [], [{k_09, v_09}, {k_12, v_12}]},
            {4, 2, 1, [], [{k_18, v_18}, {k_21, v_21}]}
        ], 3, 6
    },
    ?assertEqual(4, b_trees:size(_BTree_3)),

    _BTree_7_0 = b_trees:empty(7),
    ?assertEqual(0, b_trees:size(_BTree_7_0)),

    ok.
