%%%-------------------------------------------------------------------
%%% File        : test_generator.erl
%%% Description : Generating B-trees for testing purposes.
%%%
%%% Created     : 09.09.2016
%%%
%%% Copyright (C) 2016 Walter Weinmann
%%%-------------------------------------------------------------------

-module(test_generator).

-export([
    delete_b_tree_from/3,
    delete_b_tree_from/4,
    delete_b_tree_from_even/4,
    delete_b_tree_from_odd/4,
    delete_b_tree_till/3,
    delete_gb_tree_from/2,
    generate_b_tree_from_list/3,
    generate_b_tree_from_list/4,
    generate_b_tree_from_number/3,
    generate_b_tree_from_number/4,
    generate_b_tree_from_number_update/3,
    generate_b_tree_from_number_update/4,
    generate_b_tree_till_number/3,
    generate_b_tree_till_number/4,
    generate_gb_tree_from_list/2,
    generate_gb_tree_from_number/2,
    generate_gb_tree_from_number_update/2,
    generate_key_values_from/2,
    generate_key_values_from_even/2,
    generate_key_values_from_odd/2,
    generate_key_values_from_update/2,
    generate_key_values_rand/3,
    generate_key_values_rand_update/3,
    generate_keys_from/2,
    generate_keys_from_even/2,
    generate_keys_from_odd/2,
    generate_keys_rand/3,
    generate_keys_till/2,
    take_largest_b_tree/3,
    take_largest_gb_tree/2,
    take_smallest_b_tree/3,
    take_smallest_gb_tree/2
]).

delete_b_tree_from(Order, Number, Width) when Order > 3, Number > 0 ->
    BTree = test_generator:generate_b_tree_from_number(Order, Number, Width),
    Keys = test_generator:generate_keys_from(Number, Width),
    delete_b_tree_1(Keys, BTree).

delete_b_tree_from(Order, Number, Width, BTree) when Order > 3, Number > 0 ->
    Keys = test_generator:generate_keys_from(Number, Width),
    delete_b_tree_1(Keys, BTree).

delete_b_tree_from_even(Order, Number, Width, BTree) when Order > 3, Number > 0 ->
    Keys = test_generator:generate_keys_from_even(Number, Width),
    delete_b_tree_1(Keys, BTree).

delete_b_tree_from_odd(Order, Number, Width, BTree) when Order > 3, Number > 0 ->
    Keys = test_generator:generate_keys_from_odd(Number, Width),
    delete_b_tree_1(Keys, BTree).

delete_b_tree_till(Order, Number, Width) when Order > 3, Number > 0 ->
    BTree = test_generator:generate_b_tree_from_number(Order, Number, Width),
    Keys = test_generator:generate_keys_till(Number, Width),
    delete_b_tree_1(Keys, BTree).

delete_gb_tree_from(Number, Width) when Number > 0 ->
    GBTree = test_generator:generate_gb_tree_from_number(Number, Width),
    Keys = test_generator:generate_keys_from(Number, Width),
    delete_gb_tree_1(Keys, GBTree).

generate_b_tree_from_list(Order, Keys, Width) when Order > 3 ->
    generate_b_tree_1(Keys, [], Width, b_trees:empty(Order)).

generate_b_tree_from_list(Order, Keys, Width, BTree) when Order > 3 ->
    generate_b_tree_1(Keys, [], Width, BTree).

generate_b_tree_from_number(Order, Number, Width) when Order > 3, Number >= 0 ->
    generate_b_tree_1(lists:seq(1, Number), [], Width, b_trees:empty(Order)).
generate_b_tree_from_number(Order, b_star, Number, Width) when Order > 3, Number >= 0 ->
    generate_b_tree_1(lists:seq(1, Number), [], Width, b_trees:empty(Order, b_star)).

generate_b_tree_from_number_update(Order, Number, Width) when Order > 3, Number >= 0 ->
    generate_b_tree_1(lists:seq(1, Number), "_new", Width, b_trees:empty(Order)).
generate_b_tree_from_number_update(Order, b_star, Number, Width) when Order > 3, Number >= 0 ->
    generate_b_tree_1(lists:seq(1, Number), "_new", Width, b_trees:empty(Order, b_star)).

generate_b_tree_till_number(Order, Number, Width) when Order > 3, Number >= 0 ->
    generate_b_tree_1(lists:seq(Number, 1, -1), [], Width, b_trees:empty(Order)).
generate_b_tree_till_number(Order, b_star, Number, Width) when Order > 3, Number >= 0 ->
    generate_b_tree_1(lists:seq(Number, 1, -1), [], Width, b_trees:empty(Order, b_star)).

generate_gb_tree_from_list(Keys, Width) ->
    generate_gb_tree_1(Keys, [], Width, gb_trees:empty()).

generate_gb_tree_from_number(Number, Width) when Number >= 0 ->
    generate_gb_tree_1(lists:seq(1, Number), [], Width, gb_trees:empty()).

generate_gb_tree_from_number_update(Number, Width) when Number >= 0 ->
    generate_gb_tree_1(lists:seq(1, Number), "_new", Width, gb_trees:empty()).

generate_key_values_from(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_key_values_1(Keys, [], Width).

generate_key_values_from_even(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_key_values_1_even(Keys, [], Width).

generate_key_values_from_odd(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_key_values_1_odd(Keys, [], Width).

generate_key_values_from_update(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_key_values_1(Keys, "_new", Width).

generate_key_values_rand(Range, Number, Width) when Number >= 0 ->
    Keys = [rand:uniform(Range) || _ <- lists:seq(1, Number)],
    generate_key_values_1(Keys, [], Width).

generate_key_values_rand_update(Range, Number, Width) when Number >= 0 ->
    Keys = [rand:uniform(Range) || _ <- lists:seq(1, Number)],
    generate_key_values_1(Keys, "_new", Width).

generate_keys_from(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_keys_1(Keys, Width).

generate_keys_from_even(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_keys_1_even(Keys, Width).

generate_keys_from_odd(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_keys_1_odd(Keys, Width).

generate_keys_rand(Range, Number, Width) when Number >= 0 ->
    Keys = [rand:uniform(Range) || _ <- lists:seq(1, Number)],
    generate_keys_1(Keys, Width).

generate_keys_till(Number, Width) when Number >= 0 ->
    Keys = lists:seq(Number, 1, -1),
    generate_keys_1(Keys, Width).

take_largest_b_tree(Order, Number, Width) when Order > 3, Number > 0 ->
    BTree = test_generator:generate_b_tree_from_number(Order, Number, Width),
    take_largest_b_tree_1(Number, BTree).

take_largest_gb_tree(Number, Width) when Number > 0 ->
    GBTree = test_generator:generate_gb_tree_from_number(Number, Width),
    take_largest_gb_tree_1(Number, GBTree).

take_smallest_b_tree(Order, Number, Width) when Order > 3, Number > 0 ->
    BTree = test_generator:generate_b_tree_from_number(Order, Number, Width),
    take_smallest_b_tree_1(Number, BTree).

take_smallest_gb_tree(Number, Width) when Number > 0 ->
    GBTree = test_generator:generate_gb_tree_from_number(Number, Width),
    take_smallest_gb_tree_1(Number, GBTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete_b_tree_1([], BTree) ->
    BTree;
delete_b_tree_1([Key | Tail], BTree) ->
    delete_b_tree_1(Tail, b_trees:delete(Key, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete_gb_tree_1([], GBTree) ->
    GBTree;
delete_gb_tree_1([Key | Tail], GBTree) ->
    delete_gb_tree_1(Tail, gb_trees:delete(Key, GBTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_b_tree_1(Nodes, Suffix, Width, BTree) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_b_tree_2(Nodes, Suffix, Format, BTree).

generate_b_tree_2([], _, _, BTree) ->
    BTree;
generate_b_tree_2([Node | Tail], Suffix, Format, BTree) ->
    LastString = lists:flatten(io_lib:format(Format, [Node])),
    generate_b_tree_2(Tail, Suffix, Format, b_trees:insert("k_" ++ LastString, "v_" ++ LastString ++ Suffix, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_gb_tree_1(Nodes, Suffix, Width, GBTree) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_gb_tree_2(Nodes, Suffix, Format, GBTree).

generate_gb_tree_2([], _, _, GBTree) ->
    GBTree;
generate_gb_tree_2([Node | Tail], Suffix, Format, GBTree) ->
    LastString = lists:flatten(io_lib:format(Format, [Node])),
    generate_gb_tree_2(Tail, Suffix, Format, gb_trees:insert("k_" ++ LastString, "v_" ++ LastString ++ Suffix, GBTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_key_values_1(Keys, Suffix, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_key_values_2(Keys, Suffix, Format, []).

generate_key_values_1_even(Keys, Suffix, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_key_values_2_even(Keys, Suffix, Format, []).

generate_key_values_1_odd(Keys, Suffix, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_key_values_2_odd(Keys, Suffix, Format, []).

generate_key_values_2([], _, _, KeyValues) ->
    KeyValues;
generate_key_values_2([Key | Tail], Suffix, Format, KeyValues) ->
    LastString = lists:flatten(io_lib:format(Format, [Key])),
    generate_key_values_2(Tail, Suffix, Format, KeyValues ++ [{"k_" ++ LastString, "v_" ++ LastString ++ Suffix}]).

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

generate_key_values_2_odd([], _, _, KeyValues) ->
    KeyValues;
generate_key_values_2_odd([Key | Tail], Suffix, Format, KeyValues) ->
    case Key rem 2 of
        0 ->
            LastString = lists:flatten(io_lib:format(Format, [Key])),
            generate_key_values_2_odd(Tail, Suffix, Format, KeyValues ++ [{"k_" ++ LastString, "v_" ++ LastString ++ Suffix}]);
        _ ->
            generate_key_values_2_odd(Tail, Suffix, Format, KeyValues)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_keys_1(Keys, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_keys_2(Keys, Format, []).

generate_keys_1_even(Keys, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_keys_2_even(Keys, Format, []).

generate_keys_1_odd(Keys, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_keys_2_odd(Keys, Format, []).

generate_keys_2([], _, Keys) ->
    Keys;
generate_keys_2([Key | Tail], Format, Keys) ->
    LastString = lists:flatten(io_lib:format(Format, [Key])),
    generate_keys_2(Tail, Format, Keys ++ ["k_" ++ LastString]).

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

take_largest_b_tree_1(0, BTree) ->
    BTree;
take_largest_b_tree_1(Number, BTree) ->
    {_, _, BTReeNew} = b_trees:take_largest(BTree),
    take_largest_b_tree_1(Number - 1, BTReeNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

take_largest_gb_tree_1(0, GBTree) ->
    GBTree;
take_largest_gb_tree_1(Number, GBTree) ->
    {_, _, GBTReeNew} = gb_trees:take_largest(GBTree),
    take_largest_gb_tree_1(Number - 1, GBTReeNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

take_smallest_b_tree_1(0, BTree) ->
    BTree;
take_smallest_b_tree_1(Number, BTree) ->
    {_, _, BTReeNew} = b_trees:take_smallest(BTree),
    take_smallest_b_tree_1(Number - 1, BTReeNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

take_smallest_gb_tree_1(0, GBTree) ->
    GBTree;
take_smallest_gb_tree_1(Number, GBTree) ->
    {_, _, GBTReeNew} = gb_trees:take_smallest(GBTree),
    take_smallest_gb_tree_1(Number - 1, GBTReeNew).
