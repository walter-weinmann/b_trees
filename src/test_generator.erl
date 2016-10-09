-define(NODEBUG, true).

-include_lib("eunit/include/eunit.hrl").

-module(test_generator).

-export([
    generate_b_tree_from_number/3,
    generate_b_tree_from_number/4,
    generate_b_tree_till_number/3,
    generate_b_tree_till_number/4,
    generate_gb_tree_from_number/2]).

generate_b_tree_from_number(Order, Number, Width) when Order > 2, Number >= 0 ->
    ?debugFmt("wwe debugging generate_b_tree_from_number/3 ===> Start B Tree ~n Order : ~p~n Number: ~p~n Width : ~p~n", [Order, Number, Width]),
    generate_b_tree_1(lists:seq(1, Number), Width, b_trees:empty(Order)).
generate_b_tree_from_number(Order, b_star, Number, Width) when Order > 2, Number >= 0 ->
    ?debugFmt("wwe debugging generate_b_tree_from_number/3 ===> Start B* Tree ~n Order : ~p~n Number: ~p~n Width : ~p~n", [Order, Number, Width]),
    generate_b_tree_1(lists:seq(1, Number), Width, b_trees:empty(Order, b_star)).

generate_b_tree_till_number(Order, Number, Width) when Order > 2, Number >= 0 ->
    ?debugFmt("wwe debugging generate_b_tree_from_number/3 ===> Start B Tree ~n Order : ~p~n Number: ~p~n Width : ~p~n", [Order, Number, Width]),
    generate_b_tree_1(lists:seq(Number, 1, -1), Width, b_trees:empty(Order)).
generate_b_tree_till_number(Order, b_star, Number, Width) when Order > 2, Number >= 0 ->
    ?debugFmt("wwe debugging generate_b_tree_from_number/3 ===> Start B* Tree ~n Order : ~p~n Number: ~p~n Width : ~p~n", [Order, Number, Width]),
    generate_b_tree_1(lists:seq(Number, 1, -1), Width, b_trees:empty(Order, b_star)).

generate_gb_tree_from_number(Number, Width) when Number >= 0 ->
    ?debugFmt("wwe debugging generate_gb_tree_from_number/3 ===> Start GB Tree ~n Order : ~p~n Number: ~p~n Width : ~p~n", [Order, Number, Width]),
    generate_gb_tree_1(lists:seq(1, Number), Width, gb_trees:empty()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_b_tree_1(Nodes, Width, BTree) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_b_tree_2(Nodes, Format, BTree).

generate_b_tree_2([], _, BTree) ->
    BTree;
generate_b_tree_2([Node | Tail], Format, BTree) ->
    LastString = lists:flatten(io_lib:format(Format, [Node])),
    generate_b_tree_2(Tail, Format, b_trees:insert("k_" ++ LastString, "v_" ++ LastString, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_gb_tree_1(Nodes, Width, GBTree) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_gb_tree_2(Nodes, Format, GBTree).

generate_gb_tree_2([], _, GBTree) ->
    GBTree;
generate_gb_tree_2([Node | Tail], Format, GBTree) ->
    LastString = lists:flatten(io_lib:format(Format, [Node])),
    generate_gb_tree_2(Tail, Format, gb_trees:insert("k_" ++ LastString, "v_" ++ LastString, GBTree)).
