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
    generate_b_tree_from_list/3,
    generate_b_tree_from_number/3,
    generate_b_tree_from_number/4,
    generate_b_tree_till_number/3,
    generate_b_tree_till_number/4,
    generate_gb_tree_from_number/2,
    generate_key_values_from/2,
    generate_key_values_rand/3,
    generate_key_values_rand_update/3,
    generate_keys_rand/3]).

generate_b_tree_from_list(Order, Keys, Width) when Order > 3 ->
    generate_b_tree_1(Keys, Width, b_trees:empty(Order)).

generate_b_tree_from_number(Order, Number, Width) when Order > 3, Number >= 0 ->
    generate_b_tree_1(lists:seq(1, Number), Width, b_trees:empty(Order)).
generate_b_tree_from_number(Order, b_star, Number, Width) when Order > 2, Number >= 0 ->
    generate_b_tree_1(lists:seq(1, Number), Width, b_trees:empty(Order, b_star)).

generate_b_tree_till_number(Order, Number, Width) when Order > 3, Number >= 0 ->
    generate_b_tree_1(lists:seq(Number, 1, -1), Width, b_trees:empty(Order)).
generate_b_tree_till_number(Order, b_star, Number, Width) when Order > 2, Number >= 0 ->
    generate_b_tree_1(lists:seq(Number, 1, -1), Width, b_trees:empty(Order, b_star)).

generate_gb_tree_from_number(Number, Width) when Number >= 0 ->
    generate_gb_tree_1(lists:seq(1, Number), Width, gb_trees:empty()).

generate_key_values_from(Number, Width) when Number >= 0 ->
    Keys = lists:seq(1, Number),
    generate_key_values_1(Keys, Width).

generate_key_values_rand(Range, Number, Width) when Number >= 0 ->
    Keys = [rand:uniform(Range) || _ <- lists:seq(1, Number)],
    generate_key_values_1(Keys, Width).

generate_key_values_rand_update(Range, Number, Width) when Number >= 0 ->
    Keys = [rand:uniform(Range) || _ <- lists:seq(1, Number)],
    generate_key_values_update_1(Keys, Width).

generate_keys_rand(Range, Number, Width) when Number >= 0 ->
    Keys = [rand:uniform(Range) || _ <- lists:seq(1, Number)],
    generate_keys_1(Keys, Width).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_key_values_1(Keys, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_key_values_2(Keys, Format, []).

generate_key_values_2([], _, KeyValues) ->
    KeyValues;
generate_key_values_2([Key | Tail], Format, KeyValues) ->
    LastString = lists:flatten(io_lib:format(Format, [Key])),
    generate_key_values_2(Tail, Format, KeyValues ++ [{"k_" ++ LastString, "v_" ++ LastString}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_key_values_update_1(Keys, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_key_values_update_2(Keys, Format, []).

generate_key_values_update_2([], _, KeyValues) ->
    KeyValues;
generate_key_values_update_2([Key | Tail], Format, KeyValues) ->
    LastString = lists:flatten(io_lib:format(Format, [Key])),
    generate_key_values_update_2(Tail, Format, KeyValues ++ [{"k_" ++ LastString, "v_" ++ LastString ++ "_new"}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_keys_1(Keys, Width) ->
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_keys_2(Keys, Format, []).

generate_keys_2([], _, Keys) ->
    Keys;
generate_keys_2([Key | Tail], Format, Keys) ->
    LastString = lists:flatten(io_lib:format(Format, [Key])),
    generate_keys_2(Tail, Format, Keys ++ ["k_" ++ LastString]).
