%%%-------------------------------------------------------------------
%%% File        : test_generator.erl
%%% Description : Generating B-trees for testing purposes.
%%%
%%% Created     : 09.09.2016
%%%
%%% Copyright (C) 2016 Walter Weinmann
%%%-------------------------------------------------------------------

-module(test_generator).

-include_lib("eunit/include/eunit.hrl").

-export([
    check_equal/2,
    delete_b_tree_from/3,
    delete_b_tree_from/4,
    delete_b_tree_from_even/4,
    delete_b_tree_from_odd/4,
    delete_b_tree_list/2,
    delete_b_tree_till/3,
    delete_gb_tree_from/2,
    generate_b_tree_from_number/3,
    generate_b_tree_from_number/4,
    generate_b_tree_from_number_ets/3,
    generate_b_tree_from_number_ets/4,
    generate_b_tree_from_number_update/3,
    generate_b_tree_list_and_btree/2,
    generate_b_tree_list_and_order/2,
    generate_b_tree_list_key_number/3,
    generate_b_tree_till_number/3,
    generate_gb_tree_from_number/2,
    generate_gb_tree_from_number_update/2,
    generate_gb_tree_list/1,
    generate_key_values_from/2,
    generate_key_values_from_even/2,
    generate_key_values_from_odd/2,
    generate_key_values_from_update/2,
    generate_key_values_random/2,
    generate_key_values_random_update/2,
    generate_key_values_till/2,
    generate_keys_from/2,
    generate_keys_from_even/2,
    generate_keys_from_odd/2,
    generate_keys_random/2,
    generate_keys_till/2,
    iterate_next_b_tree/3,
    map_value_to_new/2,
    persistance_by_ets/3,
    prepare_template_asc/1,
    prepare_template_desc/1,
    take_largest_b_tree/3,
    take_largest_b_tree/4,
    take_largest_gb_tree/2,
    take_smallest_b_tree/3,
    take_smallest_b_tree/4,
    take_smallest_gb_tree/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_b_tree_from(pos_integer(), pos_integer(), pos_integer()) -> b_trees:b_tree().

delete_b_tree_from(Order, Number, Width) when Order > 3, Number > 0 ->
    BTree = test_generator:generate_b_tree_from_number(Order, Number, Width),
    Keys = test_generator:generate_keys_from(Number, Width),
    delete_b_tree_1(Keys, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_b_tree_from(pos_integer(), pos_integer(), pos_integer(), b_trees:b_tree()) -> b_trees:b_tree().

delete_b_tree_from(Order, Number, Width, BTree) when Order > 3, Number > 0 ->
    Keys = test_generator:generate_keys_from(Number, Width),
    delete_b_tree_1(Keys, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_b_tree_from_even(pos_integer(), pos_integer(), pos_integer(), b_trees:b_tree()) -> b_trees:b_tree().

delete_b_tree_from_even(Order, Number, Width, BTree) when Order > 3, Number > 0 ->
    Keys = test_generator:generate_keys_from_even(Number, Width),
    delete_b_tree_1(Keys, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_b_tree_from_odd(pos_integer(), pos_integer(), pos_integer(), b_trees:b_tree()) -> b_trees:b_tree().

delete_b_tree_from_odd(Order, Number, Width, BTree) when Order > 3, Number > 0 ->
    Keys = test_generator:generate_keys_from_odd(Number, Width),
    delete_b_tree_1(Keys, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_b_tree_list([any()], b_trees:b_tree()) -> b_trees:b_tree().

delete_b_tree_list(Keys, BTree) ->
    delete_b_tree_1(Keys, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_b_tree_till(pos_integer(), pos_integer(), pos_integer()) -> b_trees:b_tree().

delete_b_tree_till(Order, Number, Width) when Order > 3, Number > 0 ->
    BTree = test_generator:generate_b_tree_from_number(Order, Number, Width),
    Keys = test_generator:generate_keys_till(Number, Width),
    delete_b_tree_1(Keys, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_gb_tree_from(pos_integer(), pos_integer()) -> gb_trees:tree().

delete_gb_tree_from(Number, Width) when Number > 0 ->
    GBTree = test_generator:generate_gb_tree_from_number(Number, Width),
    Keys = test_generator:generate_keys_from(Number, Width),
    delete_gb_tree_1(Keys, GBTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_b_tree_from_number(pos_integer(), pos_integer(), pos_integer()) -> b_trees:b_tree().

generate_b_tree_from_number(Order, Number, Width) when Order > 3, Number >= 0 ->
    generate_b_tree_by_key_1(lists:seq(1, Number), [], Width, b_trees:empty(Order)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_b_tree_from_number_ets(pos_integer(), pos_integer(), pos_integer()) -> b_trees:b_tree().

generate_b_tree_from_number_ets(Order, Number, Width) when Order > 3, Number >= 0 ->
    B_TREE = b_trees:set_parameter(b_trees:empty(Order), state, {ets:new(b_trees, [ordered_set, {keypos, 1}]), fun persistance_by_ets/3, fun persistance_by_ets/3, fun persistance_by_ets/3}),

    generate_b_tree_by_key_1(lists:seq(1, Number), [], Width, B_TREE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_b_tree_from_number(pos_integer(), pos_integer(), pos_integer(), b_trees:sort_function()) -> b_trees:b_tree().

generate_b_tree_from_number(Order, Number, Width, Function) when Order > 3, Number >= 0, is_function(Function, 2) ->
    generate_b_tree_by_key_1(lists:seq(1, Number), [], Width, b_trees:empty(Order, Function)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_b_tree_from_number_ets(pos_integer(), pos_integer(), pos_integer(), b_trees:sort_function()) -> b_trees:b_tree().

generate_b_tree_from_number_ets(Order, Number, Width, Function) when Order > 3, Number >= 0, is_function(Function, 2) ->
    B_TREE = b_trees:set_parameter(b_trees:empty(Order, Function), state, {ets:new(b_trees, [ordered_set, {keypos, 1}]), fun persistance_by_ets/3, fun persistance_by_ets/3, fun persistance_by_ets/3}),

    generate_b_tree_by_key_1(lists:seq(1, Number), [], Width, B_TREE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_b_tree_from_number_update(pos_integer(), pos_integer(), pos_integer()) -> b_trees:b_tree().

generate_b_tree_from_number_update(Order, Number, Width) when Order > 3, Number >= 0 ->
    generate_b_tree_by_key_1(lists:seq(1, Number), "_new", Width, b_trees:empty(Order)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_b_tree_list_and_btree([{any(), any()}], b_trees:b_tree()) -> b_trees:b_tree().

generate_b_tree_list_and_btree(KeyValues, BTree) ->
    generate_b_tree_by_key_value_1(KeyValues, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_b_tree_list_and_order([{any(), any()}], pos_integer()) -> b_trees:b_tree().

generate_b_tree_list_and_order(KeyValues, Order) when Order > 3 ->
    generate_b_tree_by_key_value_1(KeyValues, b_trees:empty(Order)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_b_tree_list_key_number(pos_integer(), [non_neg_integer()], pos_integer()) -> b_trees:b_tree().

generate_b_tree_list_key_number(Order, Keys, Width) when Order > 3 ->
    generate_b_tree_by_key_number_1(Keys, [], Width, b_trees:empty(Order)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_b_tree_till_number(pos_integer(), pos_integer(), pos_integer()) -> b_trees:b_tree().

generate_b_tree_till_number(Order, Number, Width) when Order > 3, Number >= 0 ->
    generate_b_tree_by_key_1(lists:seq(Number, 1, -1), [], Width, b_trees:empty(Order)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_gb_tree_from_number(pos_integer(), pos_integer()) -> gb_trees:tree().

generate_gb_tree_from_number(Number, Width) when Number >= 0 ->
    generate_gb_tree_by_key_1(lists:seq(1, Number), [], Width, gb_trees:empty()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_gb_tree_from_number_update(pos_integer(), pos_integer()) -> gb_trees:tree().

generate_gb_tree_from_number_update(Number, Width) when Number >= 0 ->
    generate_gb_tree_by_key_1(lists:seq(1, Number), "_new", Width, gb_trees:empty()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_gb_tree_list([{any(), any()}]) -> gb_trees:tree().

generate_gb_tree_list(KeyValues) ->
    generate_gb_tree_by_key_value_1(KeyValues, gb_trees:empty()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_key_values_from(pos_integer(), pos_integer()) -> [{any(), any()}].

generate_key_values_from(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_key_values_1(Keys, [], Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_key_values_from_even(pos_integer(), pos_integer()) -> [{any(), any()}].

generate_key_values_from_even(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_key_values_1_even(Keys, [], Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_key_values_from_odd(pos_integer(), pos_integer()) -> [{any(), any()}].

generate_key_values_from_odd(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_key_values_1_odd(Keys, [], Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_key_values_from_update(pos_integer(), pos_integer()) -> [{any(), any()}].

generate_key_values_from_update(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_key_values_1(Keys, "_new", Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_key_values_random(pos_integer(), pos_integer()) -> [{any(), any()}].

generate_key_values_random(Number, Width) when Number >= 0 ->
    Keys = lists:sort(fun compare/2, lists:seq(1, Number)),
    generate_key_values_1(Keys, [], Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_key_values_random_update(pos_integer(), pos_integer()) -> [{any(), any()}].

generate_key_values_random_update(Number, Width) when Number >= 0 ->
    Keys = lists:sort(fun compare/2, lists:seq(1, Number)),
    generate_key_values_1(Keys, "_new", Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_key_values_till(pos_integer(), pos_integer()) -> [{any(), any()}].

generate_key_values_till(Number, Width) when Number >= 0 ->
    Keys = lists:seq(Number, 1, -1),
    generate_key_values_1(Keys, [], Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_keys_from(pos_integer(), pos_integer()) -> [any()].

generate_keys_from(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_keys_1(Keys, Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_keys_from_even(pos_integer(), pos_integer()) -> [any()].

generate_keys_from_even(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_keys_1_even(Keys, Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_keys_from_odd(pos_integer(), pos_integer()) -> [any()].

generate_keys_from_odd(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_keys_1_odd(Keys, Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_keys_random(pos_integer(), pos_integer()) -> [any()].

generate_keys_random(Number, Width) when Number >= 0 ->
    Keys = lists:sort(fun compare/2, lists:seq(1, Number)),
    generate_keys_1(Keys, Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_keys_till(pos_integer(), pos_integer()) -> [any()].

generate_keys_till(Number, Width) when Number >= 0 ->
    Keys = lists:seq(Number, 1, -1),
    generate_keys_1(Keys, Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_largest_b_tree(pos_integer(), pos_integer(), pos_integer()) -> b_trees:b_tree().

take_largest_b_tree(Order, Number, Width) when Order > 3, Number > 0 ->
    BTree = test_generator:generate_b_tree_from_number(Order, Number, Width),
    take_largest_b_tree_1(Number, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_largest_b_tree(pos_integer(), pos_integer(), pos_integer(), b_trees:sort_function()) -> b_trees:b_tree().

take_largest_b_tree(Order, Number, Width, Function) when Order > 3, Number > 0, is_function(Function, 2) ->
    BTree = test_generator:generate_b_tree_from_number(Order, Number, Width, Function),
    take_largest_b_tree_1(Number, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_largest_gb_tree(pos_integer(), pos_integer()) -> gb_trees:tree().

take_largest_gb_tree(Number, Width) when Number > 0 ->
    GBTree = test_generator:generate_gb_tree_from_number(Number, Width),
    take_largest_gb_tree_1(Number, GBTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_smallest_b_tree(pos_integer(), pos_integer(), pos_integer()) -> b_trees:b_tree().

take_smallest_b_tree(Order, Number, Width) when Order > 3, Number > 0 ->
    BTree = test_generator:generate_b_tree_from_number(Order, Number, Width),
    take_smallest_b_tree_1(Number, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_smallest_b_tree(pos_integer(), pos_integer(), pos_integer(), b_trees:sort_function()) -> b_trees:b_tree().

take_smallest_b_tree(Order, Number, Width, Function) when Order > 3, Number > 0, is_function(Function, 2) ->
    BTree = test_generator:generate_b_tree_from_number(Order, Number, Width, Function),
    take_smallest_b_tree_1(Number, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_smallest_gb_tree(pos_integer(), pos_integer()) -> gb_trees:tree().

take_smallest_gb_tree(Number, Width) when Number > 0 ->
    GBTree = test_generator:generate_gb_tree_from_number(Number, Width),
    take_smallest_gb_tree_1(Number, GBTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Eunit & Common Test Helper Functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec check_equal(any(), any()) -> atom().

check_equal(Value_1, Value_2) when is_tuple(Value_2), tuple_size(Value_2) == 6 ->
    ?assertEqual(Value_1, setelement(4, Value_2, sort));
check_equal(Value_1, Value_2) ->
    ?assertEqual(Value_1, Value_2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec iterate_next_b_tree(b_trees:iterator(), non_neg_integer(), b_trees:key_values()) -> b_trees:key_values().

iterate_next_b_tree(_, 0, KeyValues) ->
    KeyValues;
iterate_next_b_tree(Iterator, Count, KeyValues) ->
    {Key, Value, IteratorNew} = b_trees:next(Iterator),
    iterate_next_b_tree(IteratorNew, Count - 1, KeyValues ++ [{Key, Value}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec map_value_to_new(b_trees:key(), b_trees:value()) -> b_trees:key_value().

map_value_to_new(_, Value) ->
    Value ++ "_new".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

persistance_by_ets(StateTarget, delete, SubtreesKey) ->
    case is_list(SubtreesKey) of
        true ->
            true;
        _ ->
            ets:delete(StateTarget, SubtreesKey)
    end;
persistance_by_ets(StateTarget, insert, Subtrees) ->
    case is_list(Subtrees) of
        true ->
            case Subtrees == [] of
                true ->
                    Subtrees;
                _ ->
                    SubtreesKey = erlang:phash2(Subtrees),
                    true = ets:insert(StateTarget, {SubtreesKey, Subtrees}),
                    SubtreesKey
            end;
        _ ->
            Subtrees
    end;
persistance_by_ets(StateTarget, lookup, SubtreesKey) ->
    case is_list(SubtreesKey) of
        true ->
            SubtreesKey;
        _ ->
            [{SubtreesKey, Subtrees}] = ets:lookup(StateTarget, SubtreesKey),
            Subtrees
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec prepare_template_asc(b_trees:b_tree()) -> b_trees:b_tree().

prepare_template_asc(BTree) ->
    setelement(4, BTree, fun b_trees:sort_ascending/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec prepare_template_desc(b_trees:b_tree()) -> b_trees:b_tree().

prepare_template_desc(BTree) ->
    setelement(4, BTree, fun b_trees:sort_descending/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generator helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec compare(any(), any()) -> boolean().

compare(A, B) ->
    erlang:phash2(A) < erlang:phash2(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_b_tree_1([non_neg_integer()], b_trees:b_tree()) -> b_trees:b_tree().

delete_b_tree_1([], BTree) ->
    BTree;
delete_b_tree_1([Key | Tail], BTree) ->
    delete_b_tree_1(Tail, b_trees:delete(Key, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_gb_tree_1([non_neg_integer()], gb_trees:tree()) -> gb_trees:tree().

delete_gb_tree_1([], GBTree) ->
    GBTree;
delete_gb_tree_1([Key | Tail], GBTree) ->
    delete_gb_tree_1(Tail, gb_trees:delete(Key, GBTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_b_tree_by_key_1([non_neg_integer()], string(), pos_integer(), b_trees:b_tree()) -> b_trees:b_tree().

generate_b_tree_by_key_1(Keys, Suffix, Width, BTree) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_b_tree_by_key_2(Keys, Suffix, Format, BTree).

-spec generate_b_tree_by_key_2([non_neg_integer()], string(), string(), b_trees:b_tree()) -> b_trees:b_tree().

generate_b_tree_by_key_2([], _, _, BTree) ->
    BTree;
generate_b_tree_by_key_2([Key | Tail], Suffix, Format, BTree) ->
    LastString = lists:flatten(io_lib:format(Format, [Key])),
    generate_b_tree_by_key_2(Tail, Suffix, Format, b_trees:insert("k_" ++ LastString, "v_" ++ LastString ++ Suffix, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_b_tree_by_key_number_1(Nodes, Suffix, Width, BTree) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_b_tree_by_key_number_2(Nodes, Suffix, Format, BTree).

generate_b_tree_by_key_number_2([], _, _, BTree) ->
    BTree;
generate_b_tree_by_key_number_2([Node | Tail], Suffix, Format, BTree) ->
    LastString = lists:flatten(io_lib:format(Format, [Node])),
    generate_b_tree_by_key_number_2(Tail, Suffix, Format, b_trees:insert("k_" ++ LastString, "v_" ++ LastString ++ Suffix, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_b_tree_by_key_value_1([{any(), any()}], b_trees:b_tree()) -> b_trees:b_tree().

generate_b_tree_by_key_value_1(KeyValues, BTree) ->
    generate_b_tree_by_key_value_2(KeyValues, BTree).

-spec generate_b_tree_by_key_value_2([{any(), any()}], b_trees:b_tree()) -> b_trees:b_tree().

generate_b_tree_by_key_value_2([], BTree) ->
    BTree;
generate_b_tree_by_key_value_2([{Key, Value} | Tail], BTree) ->
    generate_b_tree_by_key_value_2(Tail, b_trees:insert(Key, Value, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_gb_tree_by_key_1([non_neg_integer()], string(), pos_integer(), gb_trees:tree()) -> gb_trees:tree().

generate_gb_tree_by_key_1(Keys, Suffix, Width, GBTree) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_gb_tree_by_key_2(Keys, Suffix, Format, GBTree).

-spec generate_gb_tree_by_key_2([non_neg_integer()], string(), string(), gb_trees:tree()) -> gb_trees:tree().

generate_gb_tree_by_key_2([], _, _, GBTree) ->
    GBTree;
generate_gb_tree_by_key_2([Key | Tail], Suffix, Format, GBTree) ->
    LastString = lists:flatten(io_lib:format(Format, [Key])),
    generate_gb_tree_by_key_2(Tail, Suffix, Format, gb_trees:insert("k_" ++ LastString, "v_" ++ LastString ++ Suffix, GBTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_gb_tree_by_key_value_1([{any(), any()}], gb_trees:tree()) -> gb_trees:tree().

generate_gb_tree_by_key_value_1(KeyValues, GBTree) ->
    generate_gb_tree_by_key_value_2(KeyValues, GBTree).

-spec generate_gb_tree_by_key_value_2([{any(), any()}], gb_trees:tree()) -> gb_trees:tree().

generate_gb_tree_by_key_value_2([], GBTree) ->
    GBTree;
generate_gb_tree_by_key_value_2([{Key, Value} | Tail], GBTree) ->
    generate_gb_tree_by_key_value_2(Tail, gb_trees:insert(Key, Value, GBTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_key_values_1([non_neg_integer()], string(), pos_integer()) -> [{any(), any()}].

generate_key_values_1(Keys, Suffix, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_key_values_2(Keys, Suffix, Format, []).

-spec generate_key_values_1_even([non_neg_integer()], string(), pos_integer()) -> [{any(), any()}].

generate_key_values_1_even(Keys, Suffix, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_key_values_2_even(Keys, Suffix, Format, []).

-spec generate_key_values_1_odd([non_neg_integer()], string(), pos_integer()) -> [{any(), any()}].

generate_key_values_1_odd(Keys, Suffix, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_key_values_2_odd(Keys, Suffix, Format, []).

-spec generate_key_values_2([non_neg_integer()], string(), string(), [{any(), any()}]) -> [{any(), any()}].

generate_key_values_2([], _, _, KeyValues) ->
    KeyValues;
generate_key_values_2([Key | Tail], Suffix, Format, KeyValues) ->
    LastString = lists:flatten(io_lib:format(Format, [Key])),
    generate_key_values_2(Tail, Suffix, Format, KeyValues ++ [{"k_" ++ LastString, "v_" ++ LastString ++ Suffix}]).

-spec generate_key_values_2_even([non_neg_integer()], string(), string(), [{any(), any()}]) -> [{any(), any()}].

generate_key_values_2_even([], _, _, KeyValues) ->
    KeyValues;
generate_key_values_2_even([Key | Tail], Suffix, Format, KeyValues) ->
    case Key rem 2 of
        0 ->
            LastString = lists:flatten(io_lib:format(Format, [Key])),
            generate_key_values_2_even(Tail, Suffix, Format, KeyValues ++ [{"k_" ++ LastString, "v_" ++ LastString ++ Suffix}]);
        _ ->
            generate_key_values_2_even(Tail, Suffix, Format, KeyValues)
    end.

-spec generate_key_values_2_odd([non_neg_integer()], string(), string(), [{any(), any()}]) -> [{any(), any()}].

generate_key_values_2_odd([], _, _, KeyValues) ->
    KeyValues;
generate_key_values_2_odd([Key | Tail], Suffix, Format, KeyValues) ->
    case Key rem 2 of
        0 ->
            generate_key_values_2_odd(Tail, Suffix, Format, KeyValues);
        _ ->
            LastString = lists:flatten(io_lib:format(Format, [Key])),
            generate_key_values_2_odd(Tail, Suffix, Format, KeyValues ++ [{"k_" ++ LastString, "v_" ++ LastString ++ Suffix}])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_keys_1([non_neg_integer()], pos_integer()) -> [any()].

generate_keys_1(Keys, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_keys_2(Keys, Format, []).

-spec generate_keys_1_even([non_neg_integer()], pos_integer()) -> [any()].

generate_keys_1_even(Keys, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_keys_2_even(Keys, Format, []).

-spec generate_keys_1_odd([non_neg_integer()], pos_integer()) -> [any()].

generate_keys_1_odd(Keys, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_keys_2_odd(Keys, Format, []).

-spec generate_keys_2([non_neg_integer()], string(), [any()]) -> [any()].

generate_keys_2([], _, Keys) ->
    Keys;
generate_keys_2([Key | Tail], Format, Keys) ->
    LastString = lists:flatten(io_lib:format(Format, [Key])),
    generate_keys_2(Tail, Format, Keys ++ ["k_" ++ LastString]).

-spec generate_keys_2_even([non_neg_integer()], string(), [any()]) -> [any()].

generate_keys_2_even([], _, Keys) ->
    Keys;
generate_keys_2_even([Key | Tail], Format, Keys) ->
    case Key rem 2 of
        0 ->
            LastString = lists:flatten(io_lib:format(Format, [Key])),
            generate_keys_2_even(Tail, Format, Keys ++ ["k_" ++ LastString]);
        _ ->
            generate_keys_2_even(Tail, Format, Keys)
    end.


-spec generate_keys_2_odd([non_neg_integer()], string(), [any()]) -> [any()].

generate_keys_2_odd([], _, Keys) ->
    Keys;
generate_keys_2_odd([Key | Tail], Format, Keys) ->
    case Key rem 2 of
        1 ->
            LastString = lists:flatten(io_lib:format(Format, [Key])),
            generate_keys_2_odd(Tail, Format, Keys ++ ["k_" ++ LastString]);
        _ ->
            generate_keys_2_odd(Tail, Format, Keys)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_largest_b_tree_1(non_neg_integer(), b_trees:b_tree()) -> b_trees:b_tree().

take_largest_b_tree_1(0, BTree) ->
    BTree;
take_largest_b_tree_1(Number, BTree) ->
    {_, _, BTReeNew} = b_trees:take_largest(BTree),
    take_largest_b_tree_1(Number - 1, BTReeNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_largest_gb_tree_1(non_neg_integer(), gb_trees:tree()) -> gb_trees:tree().

take_largest_gb_tree_1(0, GBTree) ->
    GBTree;
take_largest_gb_tree_1(Number, GBTree) ->
    {_, _, GBTReeNew} = gb_trees:take_largest(GBTree),
    take_largest_gb_tree_1(Number - 1, GBTReeNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_smallest_b_tree_1(non_neg_integer(), b_trees:b_tree()) -> b_trees:b_tree().

take_smallest_b_tree_1(0, BTree) ->
    BTree;
take_smallest_b_tree_1(Number, BTree) ->
    {_, _, BTReeNew} = b_trees:take_smallest(BTree),
    take_smallest_b_tree_1(Number - 1, BTReeNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_smallest_gb_tree_1(non_neg_integer(), gb_trees:tree()) -> gb_trees:tree().

take_smallest_gb_tree_1(0, GBTree) ->
    GBTree;
take_smallest_gb_tree_1(Number, GBTree) ->
    {_, _, GBTReeNew} = gb_trees:take_smallest(GBTree),
    take_smallest_gb_tree_1(Number - 1, GBTReeNew).
