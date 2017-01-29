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

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(B_TREE_POS_STATE, 5).
-define(DIRECTORY_DETS, "test/tmp/").
-define(OTP_RELEASE, erlang:system_info(otp_release)).

%%% Performance tests
%%-define(LARGEST_KEY_VALUE, {"k_10000", ?LARGEST_VALUE}).
%%-define(LARGEST_VALUE, "v_10000").
%%-define(NUMBER_ACTIONS, 10000).
%%-define(SMALLEST_KEY_VALUE, {"k_00001", "v_00001"}).
%%-define(WIDTH, 5).

% Standard tests
-define(LARGEST_KEY_VALUE, {"k_2000", ?LARGEST_VALUE}).
-define(LARGEST_VALUE, "v_2000").
-define(NUMBER_ACTIONS, 2000).
-define(SMALLEST_KEY_VALUE, {"k_0001", "v_0001"}).
-define(WIDTH, 4).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - SUITE
%%--------------------------------------------------------------------

suite() ->
    [
        {timetrap, {minutes, 30}}
    ].

init_per_suite(Config) ->
    ok = filelib:ensure_dir(?DIRECTORY_DETS),
    ok = b_trees_generator:delete_directory(?DIRECTORY_DETS),
    ok = filelib:ensure_dir(?DIRECTORY_DETS),

    case ok == mnesia:create_schema([node()]) of
        true ->
            ok;
        _ ->
            ok = mnesia:delete_schema([node()]),
            mnesia:create_schema([node()])
    end,
    ok = mnesia:start(),
    [
        {key_values_from, b_trees_generator:generate_key_values_from(?NUMBER_ACTIONS, ?WIDTH)},
        {key_values_from_even_odd, lists:append([
            b_trees_generator:generate_key_values_from_even(?NUMBER_ACTIONS, ?WIDTH),
            b_trees_generator:generate_key_values_from_odd(?NUMBER_ACTIONS, ?WIDTH)
        ])},
        {key_values_from_odd_even, lists:append([
            b_trees_generator:generate_key_values_from_odd(?NUMBER_ACTIONS, ?WIDTH),
            b_trees_generator:generate_key_values_from_even(?NUMBER_ACTIONS, ?WIDTH)
        ])},
        {key_values_from_update, b_trees_generator:generate_key_values_from_update(?NUMBER_ACTIONS, ?WIDTH)},
        {key_values_random, b_trees_generator:generate_key_values_random(?NUMBER_ACTIONS, ?WIDTH)},
        {key_values_random_update, b_trees_generator:generate_key_values_random_update(?NUMBER_ACTIONS, ?WIDTH)},

        {keys_from, b_trees_generator:generate_keys_from(?NUMBER_ACTIONS, ?WIDTH)},
        {keys_from_even_odd, lists:append([
            b_trees_generator:generate_keys_from_even(?NUMBER_ACTIONS, ?WIDTH),
            b_trees_generator:generate_keys_from_odd(?NUMBER_ACTIONS, ?WIDTH)
        ])},
        {keys_from_odd_even, lists:append([
            b_trees_generator:generate_keys_from_odd(?NUMBER_ACTIONS, ?WIDTH),
            b_trees_generator:generate_keys_from_even(?NUMBER_ACTIONS, ?WIDTH)
        ])},
        {keys_random, b_trees_generator:generate_keys_random(?NUMBER_ACTIONS, ?WIDTH)},

        {gb_tree, b_trees_generator:generate_gb_tree_from_number(?NUMBER_ACTIONS, ?WIDTH)},
        {gb_tree_new, b_trees_generator:generate_gb_tree_from_number_update(?NUMBER_ACTIONS, ?WIDTH)},

        {b_tree_4, b_trees_generator:generate_b_tree_from_number(4, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_4_desc, b_trees_generator:generate_b_tree_from_number(4, ?NUMBER_ACTIONS, ?WIDTH, fun b_trees:sort_descending/2)},
        {b_tree_4_new, b_trees_generator:generate_b_tree_from_number_update(4, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_4_till, b_trees_generator:generate_b_tree_till_number(4, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_5, b_trees_generator:generate_b_tree_from_number(5, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_5_new, b_trees_generator:generate_b_tree_from_number_update(5, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_5_till, b_trees_generator:generate_b_tree_till_number(5, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_6, b_trees_generator:generate_b_tree_from_number(6, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_6_new, b_trees_generator:generate_b_tree_from_number_update(6, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_6_till, b_trees_generator:generate_b_tree_till_number(6, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_8, b_trees_generator:generate_b_tree_from_number(8, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_8_new, b_trees_generator:generate_b_tree_from_number_update(8, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_16, b_trees_generator:generate_b_tree_from_number(16, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_16_new, b_trees_generator:generate_b_tree_from_number_update(16, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_32, b_trees_generator:generate_b_tree_from_number(32, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_32_new, b_trees_generator:generate_b_tree_from_number_update(32, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_64, b_trees_generator:generate_b_tree_from_number(64, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_64_new, b_trees_generator:generate_b_tree_from_number_update(64, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_128, b_trees_generator:generate_b_tree_from_number(128, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_128_new, b_trees_generator:generate_b_tree_from_number_update(128, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_256, b_trees_generator:generate_b_tree_from_number(256, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_256_new, b_trees_generator:generate_b_tree_from_number_update(256, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_512, b_trees_generator:generate_b_tree_from_number(512, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_512_new, b_trees_generator:generate_b_tree_from_number_update(512, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_1024, b_trees_generator:generate_b_tree_from_number(1024, ?NUMBER_ACTIONS, ?WIDTH)},
        {b_tree_1024_new, b_trees_generator:generate_b_tree_from_number_update(1024, ?NUMBER_ACTIONS, ?WIDTH)},

        {b_tree_4_dets, b_trees_generator:generate_b_tree_from_number_dets(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_dets)},
        {b_tree_4_dets_map, b_trees_generator:generate_b_tree_from_number_dets(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_dets_map)},
        {b_tree_4_dets_new, b_trees_generator:generate_b_tree_from_number_update_dets(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_dets_new)},
        {b_tree_4_dets_update, b_trees_generator:generate_b_tree_from_number_dets(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_dets_update)},

        {b_tree_1024_dets, b_trees_generator:generate_b_tree_from_number_dets(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_dets)},
        {b_tree_1024_dets_map, b_trees_generator:generate_b_tree_from_number_dets(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_dets_map)},
        {b_tree_1024_dets_new, b_trees_generator:generate_b_tree_from_number_update_dets(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_dets_new)},
        {b_tree_1024_dets_update, b_trees_generator:generate_b_tree_from_number_dets(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_dets_update)},

        {b_tree_4_ets, b_trees_generator:generate_b_tree_from_number_ets(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_ets, spawn(fun b_trees_generator:ets_owner/0))},
        {b_tree_4_ets_map, b_trees_generator:generate_b_tree_from_number_ets(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_ets_map, spawn(fun b_trees_generator:ets_owner/0))},
        {b_tree_4_ets_new, b_trees_generator:generate_b_tree_from_number_update_ets(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_ets_new, spawn(fun b_trees_generator:ets_owner/0))},
        {b_tree_4_ets_update, b_trees_generator:generate_b_tree_from_number_ets(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_ets_update, spawn(fun b_trees_generator:ets_owner/0))},

        {b_tree_1024_ets, b_trees_generator:generate_b_tree_from_number_ets(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_ets, spawn(fun b_trees_generator:ets_owner/0))},
        {b_tree_1024_ets_map, b_trees_generator:generate_b_tree_from_number_ets(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_ets_map, spawn(fun b_trees_generator:ets_owner/0))},
        {b_tree_1024_ets_new, b_trees_generator:generate_b_tree_from_number_update_ets(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_ets_new, spawn(fun b_trees_generator:ets_owner/0))},
        {b_tree_1024_ets_update, b_trees_generator:generate_b_tree_from_number_ets(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_ets_update, spawn(fun b_trees_generator:ets_owner/0))},

        {b_tree_4_mnesia, b_trees_generator:generate_b_tree_from_number_mnesia(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_mnesia)},
        {b_tree_4_mnesia_map, b_trees_generator:generate_b_tree_from_number_mnesia(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_mnesia_map)},
        {b_tree_4_mnesia_new, b_trees_generator:generate_b_tree_from_number_update_mnesia(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_mnesia_new)},
        {b_tree_4_mnesia_update, b_trees_generator:generate_b_tree_from_number_mnesia(4, ?NUMBER_ACTIONS, ?WIDTH, b_tree_4_mnesia_update)},

        {b_tree_1024_mnesia, b_trees_generator:generate_b_tree_from_number_mnesia(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_mnesia)},
        {b_tree_1024_mnesia_map, b_trees_generator:generate_b_tree_from_number_mnesia(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_mnesia_map)},
        {b_tree_1024_mnesia_new, b_trees_generator:generate_b_tree_from_number_update_mnesia(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_mnesia_new)},
        {b_tree_1024_mnesia_update, b_trees_generator:generate_b_tree_from_number_mnesia(1024, ?NUMBER_ACTIONS, ?WIDTH, b_tree_1024_mnesia_update)}

        | Config
    ].

end_per_suite(_Config) ->
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%--------------------------------------------------------------------

all() ->
    [
        delete_gb_tree_test,
        delete_b_tree_order_4_desc_test,
        delete_b_tree_order_4_dets_desc_test,
        delete_b_tree_order_4_dets_test,
        delete_b_tree_order_4_ets_desc_test,
        delete_b_tree_order_4_ets_even_odd_test,
        delete_b_tree_order_4_ets_odd_even_test,
        delete_b_tree_order_4_ets_random_desc_test,
        delete_b_tree_order_4_ets_random_test,
        delete_b_tree_order_4_ets_test,
        delete_b_tree_order_4_even_odd_test,
        delete_b_tree_order_4_mnesia_desc_test,
        delete_b_tree_order_4_mnesia_test,
        delete_b_tree_order_4_odd_even_test,
        delete_b_tree_order_4_random_desc_test,
        delete_b_tree_order_4_random_test,
        delete_b_tree_order_4_test,
        delete_b_tree_order_5_ets_even_odd_test,
        delete_b_tree_order_5_ets_odd_even_test,
        delete_b_tree_order_5_ets_random_test,
        delete_b_tree_order_5_ets_test,
        delete_b_tree_order_5_even_odd_test,
        delete_b_tree_order_5_odd_even_test,
        delete_b_tree_order_5_random_test,
        delete_b_tree_order_5_test,
        delete_b_tree_order_6_ets_test,
        delete_b_tree_order_6_test,
        delete_b_tree_order_8_ets_even_odd_test,
        delete_b_tree_order_8_ets_odd_even_test,
        delete_b_tree_order_8_ets_random_test,
        delete_b_tree_order_8_ets_test,
        delete_b_tree_order_8_even_odd_test,
        delete_b_tree_order_8_odd_even_test,
        delete_b_tree_order_8_random_test,
        delete_b_tree_order_8_test,
        delete_b_tree_order_16_ets_test,
        delete_b_tree_order_16_test,
        delete_b_tree_order_32_ets_test,
        delete_b_tree_order_32_test,
        delete_b_tree_order_64_ets_test,
        delete_b_tree_order_64_test,
        delete_b_tree_order_128_ets_test,
        delete_b_tree_order_128_test,
        delete_b_tree_order_256_ets_test,
        delete_b_tree_order_256_test,
        delete_b_tree_order_512_ets_test,
        delete_b_tree_order_512_test,
        delete_b_tree_order_1024_dets_test,
        delete_b_tree_order_1024_ets_test,
        delete_b_tree_order_1024_mnesia_test,
        delete_b_tree_order_1024_test,

        enter_gb_tree_test,
        enter_b_tree_order_4_dets_test,
        enter_b_tree_order_4_ets_test,
        enter_b_tree_order_4_mnesia_test,
        enter_b_tree_order_4_test,
        enter_b_tree_order_5_test,
        enter_b_tree_order_6_test,
        enter_b_tree_order_8_test,
        enter_b_tree_order_16_test,
        enter_b_tree_order_32_test,
        enter_b_tree_order_64_test,
        enter_b_tree_order_128_test,
        enter_b_tree_order_256_test,
        enter_b_tree_order_512_test,
        enter_b_tree_order_1024_dets_test,
        enter_b_tree_order_1024_ets_test,
        enter_b_tree_order_1024_mnesia_test,
        enter_b_tree_order_1024_test,

        from_dict_gb_tree_test,
        from_dict_b_tree_order_4_dets_test,
        from_dict_b_tree_order_4_ets_test,
        from_dict_b_tree_order_4_even_odd_test,
        from_dict_b_tree_order_4_mnesia_test,
        from_dict_b_tree_order_4_odd_even_test,
        from_dict_b_tree_order_4_random_test,
        from_dict_b_tree_order_4_test,
        from_dict_b_tree_order_5_even_odd_test,
        from_dict_b_tree_order_5_odd_even_test,
        from_dict_b_tree_order_5_random_test,
        from_dict_b_tree_order_5_test,
        from_dict_b_tree_order_6_test,
        from_dict_b_tree_order_8_even_odd_test,
        from_dict_b_tree_order_8_odd_even_test,
        from_dict_b_tree_order_8_random_test,
        from_dict_b_tree_order_8_test,
        from_dict_b_tree_order_16_test,
        from_dict_b_tree_order_32_test,
        from_dict_b_tree_order_64_test,
        from_dict_b_tree_order_128_test,
        from_dict_b_tree_order_256_test,
        from_dict_b_tree_order_512_test,
        from_dict_b_tree_order_1024_dets_test,
        from_dict_b_tree_order_1024_ets_test,
        from_dict_b_tree_order_1024_mnesia_test,
        from_dict_b_tree_order_1024_test,

        get_gb_tree_test,
        get_b_tree_order_4_dets_test,
        get_b_tree_order_4_ets_test,
        get_b_tree_order_4_mnesia_test,
        get_b_tree_order_4_test,
        get_b_tree_order_5_test,
        get_b_tree_order_6_test,
        get_b_tree_order_8_test,
        get_b_tree_order_16_test,
        get_b_tree_order_32_test,
        get_b_tree_order_64_test,
        get_b_tree_order_128_test,
        get_b_tree_order_256_test,
        get_b_tree_order_512_test,
        get_b_tree_order_1024_test,

        insert_gb_tree_test,
        insert_b_tree_order_4_dets_test,
        insert_b_tree_order_4_ets_test,
        insert_b_tree_order_4_even_odd_test,
        insert_b_tree_order_4_mnesia_test,
        insert_b_tree_order_4_odd_even_test,
        insert_b_tree_order_4_random_test,
        insert_b_tree_order_4_test,
        insert_b_tree_order_4_till_test,
        insert_b_tree_order_5_even_odd_test,
        insert_b_tree_order_5_odd_even_test,
        insert_b_tree_order_5_random_test,
        insert_b_tree_order_5_test,
        insert_b_tree_order_5_till_test,
        insert_b_tree_order_6_test,
        insert_b_tree_order_6_till_test,
        insert_b_tree_order_8_even_odd_test,
        insert_b_tree_order_8_odd_even_test,
        insert_b_tree_order_8_random_test,
        insert_b_tree_order_8_test,
        insert_b_tree_order_16_test,
        insert_b_tree_order_32_test,
        insert_b_tree_order_64_test,
        insert_b_tree_order_128_test,
        insert_b_tree_order_256_test,
        insert_b_tree_order_512_test,
        insert_b_tree_order_1024_dets_test,
        insert_b_tree_order_1024_ets_test,
        insert_b_tree_order_1024_mnesia_test,
        insert_b_tree_order_1024_test,

        is_defined_gb_tree_test,
        is_defined_b_tree_order_4_dets_test,
        is_defined_b_tree_order_4_ets_test,
        is_defined_b_tree_order_4_mnesia_test,
        is_defined_b_tree_order_4_test,
        is_defined_b_tree_order_5_test,
        is_defined_b_tree_order_6_test,
        is_defined_b_tree_order_8_test,
        is_defined_b_tree_order_16_test,
        is_defined_b_tree_order_32_test,
        is_defined_b_tree_order_64_test,
        is_defined_b_tree_order_128_test,
        is_defined_b_tree_order_256_test,
        is_defined_b_tree_order_512_test,
        is_defined_b_tree_order_1024_test,

        iterate_next_gb_tree_test,
        iterate_next_b_tree_order_4_dets_test,
        iterate_next_b_tree_order_4_ets_test,
        iterate_next_b_tree_order_4_mnesia_test,
        iterate_next_b_tree_order_4_test,
        iterate_next_b_tree_order_5_test,
        iterate_next_b_tree_order_6_test,
        iterate_next_b_tree_order_8_test,
        iterate_next_b_tree_order_16_test,
        iterate_next_b_tree_order_32_test,
        iterate_next_b_tree_order_64_test,
        iterate_next_b_tree_order_128_test,
        iterate_next_b_tree_order_256_test,
        iterate_next_b_tree_order_512_test,
        iterate_next_b_tree_order_1024_dets_test,
        iterate_next_b_tree_order_1024_ets_test,
        iterate_next_b_tree_order_1024_mnesia_test,
        iterate_next_b_tree_order_1024_test,

        keys_gb_tree_test,
        keys_b_tree_order_4_dets_test,
        keys_b_tree_order_4_ets_test,
        keys_b_tree_order_4_mnesia_test,
        keys_b_tree_order_4_test,
        keys_b_tree_order_5_test,
        keys_b_tree_order_6_test,
        keys_b_tree_order_8_test,
        keys_b_tree_order_16_test,
        keys_b_tree_order_32_test,
        keys_b_tree_order_64_test,
        keys_b_tree_order_128_test,
        keys_b_tree_order_256_test,
        keys_b_tree_order_512_test,
        keys_b_tree_order_1024_test,

        largest_gb_tree_test,
        largest_b_tree_order_4_dets_test,
        largest_b_tree_order_4_ets_test,
        largest_b_tree_order_4_mnesia_test,
        largest_b_tree_order_4_test,
        largest_b_tree_order_5_test,
        largest_b_tree_order_6_test,
        largest_b_tree_order_8_test,
        largest_b_tree_order_16_test,
        largest_b_tree_order_32_test,
        largest_b_tree_order_64_test,
        largest_b_tree_order_128_test,
        largest_b_tree_order_256_test,
        largest_b_tree_order_512_test,
        largest_b_tree_order_1024_test,

        lookup_gb_tree_test,
        lookup_b_tree_order_4_dets_test,
        lookup_b_tree_order_4_ets_test,
        lookup_b_tree_order_4_mnesia_test,
        lookup_b_tree_order_4_test,
        lookup_b_tree_order_5_test,
        lookup_b_tree_order_6_test,
        lookup_b_tree_order_8_test,
        lookup_b_tree_order_16_test,
        lookup_b_tree_order_32_test,
        lookup_b_tree_order_64_test,
        lookup_b_tree_order_128_test,
        lookup_b_tree_order_256_test,
        lookup_b_tree_order_512_test,
        lookup_b_tree_order_1024_test,

        map_gb_tree_test,
        map_b_tree_order_4_dets_test,
        map_b_tree_order_4_ets_test,
        map_b_tree_order_4_mnesia_test,
        map_b_tree_order_4_test,
        map_b_tree_order_5_test,
        map_b_tree_order_6_test,
        map_b_tree_order_8_test,
        map_b_tree_order_16_test,
        map_b_tree_order_32_test,
        map_b_tree_order_64_test,
        map_b_tree_order_128_test,
        map_b_tree_order_256_test,
        map_b_tree_order_512_test,
        map_b_tree_order_1024_dets_test,
        map_b_tree_order_1024_ets_test,
        map_b_tree_order_1024_mnesia_test,
        map_b_tree_order_1024_test,

        smallest_gb_tree_test,
        smallest_b_tree_order_4_dets_test,
        smallest_b_tree_order_4_ets_test,
        smallest_b_tree_order_4_mnesia_test,
        smallest_b_tree_order_4_test,
        smallest_b_tree_order_5_test,
        smallest_b_tree_order_6_test,
        smallest_b_tree_order_8_test,
        smallest_b_tree_order_16_test,
        smallest_b_tree_order_32_test,
        smallest_b_tree_order_64_test,
        smallest_b_tree_order_128_test,
        smallest_b_tree_order_256_test,
        smallest_b_tree_order_512_test,
        smallest_b_tree_order_1024_test,

        take_any_gb_tree_test,
        take_any_b_tree_order_4_test,
        take_any_b_tree_order_5_test,
        take_any_b_tree_order_6_test,
        take_any_b_tree_order_8_test,
        take_any_b_tree_order_16_test,
        take_any_b_tree_order_32_test,
        take_any_b_tree_order_64_test,
        take_any_b_tree_order_128_test,
        take_any_b_tree_order_256_test,
        take_any_b_tree_order_512_test,
        take_any_b_tree_order_1024_test,

        take_gb_tree_test,
        take_b_tree_order_4_test,
        take_b_tree_order_5_test,
        take_b_tree_order_6_test,
        take_b_tree_order_8_test,
        take_b_tree_order_16_test,
        take_b_tree_order_32_test,
        take_b_tree_order_64_test,
        take_b_tree_order_128_test,
        take_b_tree_order_256_test,
        take_b_tree_order_512_test,
        take_b_tree_order_1024_test,

        take_largest_gb_tree_test,
        take_largest_b_tree_order_4_dets_test,
        take_largest_b_tree_order_4_ets_test,
        take_largest_b_tree_order_4_mnesia_test,
        take_largest_b_tree_order_4_test,
        take_largest_b_tree_order_5_test,
        take_largest_b_tree_order_6_test,
        take_largest_b_tree_order_8_test,
        take_largest_b_tree_order_16_test,
        take_largest_b_tree_order_32_test,
        take_largest_b_tree_order_64_test,
        take_largest_b_tree_order_128_test,
        take_largest_b_tree_order_256_test,
        take_largest_b_tree_order_512_test,
        take_largest_b_tree_order_1024_dets_test,
        take_largest_b_tree_order_1024_ets_test,
        take_largest_b_tree_order_1024_mnesia_test,
        take_largest_b_tree_order_1024_test,

        take_smallest_gb_tree_test,
        take_smallest_b_tree_order_4_dets_test,
        take_smallest_b_tree_order_4_ets_test,
        take_smallest_b_tree_order_4_mnesia_test,
        take_smallest_b_tree_order_4_test,
        take_smallest_b_tree_order_5_test,
        take_smallest_b_tree_order_6_test,
        take_smallest_b_tree_order_8_test,
        take_smallest_b_tree_order_16_test,
        take_smallest_b_tree_order_32_test,
        take_smallest_b_tree_order_64_test,
        take_smallest_b_tree_order_128_test,
        take_smallest_b_tree_order_256_test,
        take_smallest_b_tree_order_512_test,
        take_smallest_b_tree_order_1024_dets_test,
        take_smallest_b_tree_order_1024_ets_test,
        take_smallest_b_tree_order_1024_mnesia_test,
        take_smallest_b_tree_order_1024_test,

        to_list_gb_tree_test,
        to_list_b_tree_order_4_dets_test,
        to_list_b_tree_order_4_ets_test,
        to_list_b_tree_order_4_mnesia_test,
        to_list_b_tree_order_4_test,
        to_list_b_tree_order_5_test,
        to_list_b_tree_order_6_test,
        to_list_b_tree_order_8_test,
        to_list_b_tree_order_16_test,
        to_list_b_tree_order_32_test,
        to_list_b_tree_order_64_test,
        to_list_b_tree_order_128_test,
        to_list_b_tree_order_256_test,
        to_list_b_tree_order_512_test,
        to_list_b_tree_order_1024_test,

        update_gb_tree_test,
        update_b_tree_order_4_dets_test,
        update_b_tree_order_4_ets_test,
        update_b_tree_order_4_mnesia_test,
        update_b_tree_order_4_random_test,
        update_b_tree_order_4_test,
        update_b_tree_order_5_random_test,
        update_b_tree_order_5_test,
        update_b_tree_order_6_test,
        update_b_tree_order_8_random_test,
        update_b_tree_order_8_test,
        update_b_tree_order_16_test,
        update_b_tree_order_32_test,
        update_b_tree_order_64_test,
        update_b_tree_order_128_test,
        update_b_tree_order_256_test,
        update_b_tree_order_512_test,
        update_b_tree_order_1024_dets_test,
        update_b_tree_order_1024_ets_test,
        update_b_tree_order_1024_mnesia_test,
        update_b_tree_order_1024_test,

        values_gb_tree_test,
        values_b_tree_order_4_dets_test,
        values_b_tree_order_4_ets_test,
        values_b_tree_order_4_mnesia_test,
        values_b_tree_order_4_test,
        values_b_tree_order_5_test,
        values_b_tree_order_6_test,
        values_b_tree_order_8_test,
        values_b_tree_order_16_test,
        values_b_tree_order_32_test,
        values_b_tree_order_64_test,
        values_b_tree_order_128_test,
        values_b_tree_order_256_test,
        values_b_tree_order_512_test,
        values_b_tree_order_1024_test
    ].

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 1024 - persistence by dets
%%--------------------------------------------------------------------

delete_b_tree_order_1024_dets_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(1024), b_trees_generator:delete_b_tree_from_dets(1024, ?NUMBER_ACTIONS, ?WIDTH, delete_order_1024_dets)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 1024 - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_1024_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets(1024, ?NUMBER_ACTIONS, ?WIDTH, delete_order_1024_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(1024), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 1024 - persistence by mnesia
%%--------------------------------------------------------------------

delete_b_tree_order_1024_mnesia_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(1024), b_trees_generator:delete_b_tree_from_mnesia(1024, ?NUMBER_ACTIONS, ?WIDTH, delete_order_1024_mnesia)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 1024
%%--------------------------------------------------------------------

delete_b_tree_order_1024_test(_Config) ->
    ?assertEqual(b_trees:empty(1024), b_trees_generator:delete_b_tree_from(1024, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 128 - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_128_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets(128, ?NUMBER_ACTIONS, ?WIDTH, delete_order_128_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(128), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 128
%%--------------------------------------------------------------------

delete_b_tree_order_128_test(_Config) ->
    ?assertEqual(b_trees:empty(128), b_trees_generator:delete_b_tree_from(128, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 16 - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_16_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets(16, ?NUMBER_ACTIONS, ?WIDTH, delete_order_16_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(16), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 16
%%--------------------------------------------------------------------

delete_b_tree_order_16_test(_Config) ->
    ?assertEqual(b_trees:empty(16), b_trees_generator:delete_b_tree_from(16, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 256 - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_256_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets(256, ?NUMBER_ACTIONS, ?WIDTH, delete_order_256_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(256), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 256
%%--------------------------------------------------------------------

delete_b_tree_order_256_test(_Config) ->
    ?assertEqual(b_trees:empty(256), b_trees_generator:delete_b_tree_from(256, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 32 - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_32_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets(32, ?NUMBER_ACTIONS, ?WIDTH, delete_order_32_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(32), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 32
%%--------------------------------------------------------------------

delete_b_tree_order_32_test(_Config) ->
    ?assertEqual(b_trees:empty(32), b_trees_generator:delete_b_tree_from(32, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - descending
%%--------------------------------------------------------------------

delete_b_tree_order_4_desc_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(4), b_trees_generator:delete_b_tree_from_desc(4, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - persistence by dets - descending
%%--------------------------------------------------------------------

delete_b_tree_order_4_dets_desc_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(4), b_trees_generator:delete_b_tree_from_dets_desc(4, ?NUMBER_ACTIONS, ?WIDTH, delete_order_4_dets_desc)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

delete_b_tree_order_4_dets_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(4), b_trees_generator:delete_b_tree_from_dets(4, ?NUMBER_ACTIONS, ?WIDTH, delete_order_4_dets)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - persistence by ets - descending
%%--------------------------------------------------------------------

delete_b_tree_order_4_ets_desc_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets_desc(4, ?NUMBER_ACTIONS, ?WIDTH, delete_order_4_ets_desc, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(4), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - even / odd - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_4_ets_even_odd_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:generate_b_tree_from_number_ets(4, ?NUMBER_ACTIONS, ?WIDTH, delete_order_4_ets_even_odd, spawn(fun b_trees_generator:ets_owner/0)),
    BTree_Odd = b_trees_generator:delete_b_tree_from_even(4, ?NUMBER_ACTIONS, ?WIDTH, BTree),
    BTree_Empty = b_trees_generator:delete_b_tree_from_odd(4, ?NUMBER_ACTIONS, ?WIDTH, BTree_Odd),
    b_trees_generator:check_equal(b_trees:empty(4), BTree_Empty),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree_Empty),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - odd / even - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_4_ets_odd_even_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:generate_b_tree_from_number_ets(4, ?NUMBER_ACTIONS, ?WIDTH, delete_order_4_ets_odd_even, spawn(fun b_trees_generator:ets_owner/0)),
    BTree_Even = b_trees_generator:delete_b_tree_from_odd(4, ?NUMBER_ACTIONS, ?WIDTH, BTree),
    BTree_Empty = b_trees_generator:delete_b_tree_from_even(4, ?NUMBER_ACTIONS, ?WIDTH, BTree_Even),
    b_trees_generator:check_equal(b_trees:empty(4), BTree_Empty),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree_Empty),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - random - descending - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_4_ets_random_desc_test(Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_1 = b_trees_generator:generate_b_tree_from_number_ets(4, ?NUMBER_ACTIONS, ?WIDTH, delete_order_4_ets_random_desc, spawn(fun b_trees_generator:ets_owner/0)),
    B_TREE_2 = b_trees_generator:delete_b_tree_list(?config(keys_random, Config), B_TREE_1),
    b_trees_generator:check_equal(b_trees:empty(4), B_TREE_2),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE_2),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - random - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_4_ets_random_test(Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_1 = b_trees_generator:generate_b_tree_from_number_ets(4, ?NUMBER_ACTIONS, ?WIDTH, delete_order_4_ets_random, spawn(fun b_trees_generator:ets_owner/0)),
    B_TREE_2 = b_trees_generator:delete_b_tree_list(?config(keys_random, Config), B_TREE_1),
    b_trees_generator:check_equal(b_trees:empty(4), B_TREE_2),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE_2),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_4_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets(4, ?NUMBER_ACTIONS, ?WIDTH, delete_order_4_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(4), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - even / odd
%%--------------------------------------------------------------------

delete_b_tree_order_4_even_odd_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(4, ?NUMBER_ACTIONS, ?WIDTH),
    _BTree_Odd = b_trees_generator:delete_b_tree_from_even(4, ?NUMBER_ACTIONS, ?WIDTH, _BTree),
    _BTree_Empty = b_trees_generator:delete_b_tree_from_odd(4, ?NUMBER_ACTIONS, ?WIDTH, _BTree_Odd),
    ?assertEqual(b_trees:empty(4), _BTree_Empty),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - persistence by mnesia - descending
%%--------------------------------------------------------------------

delete_b_tree_order_4_mnesia_desc_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(4), b_trees_generator:delete_b_tree_from_mnesia_desc(4, ?NUMBER_ACTIONS, ?WIDTH, delete_order_4_mnesia_desc)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

delete_b_tree_order_4_mnesia_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(4), b_trees_generator:delete_b_tree_from_mnesia(4, ?NUMBER_ACTIONS, ?WIDTH, delete_order_4_mnesia)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - odd / even
%%--------------------------------------------------------------------

delete_b_tree_order_4_odd_even_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(4, ?NUMBER_ACTIONS, ?WIDTH),
    _BTree_Even = b_trees_generator:delete_b_tree_from_odd(4, ?NUMBER_ACTIONS, ?WIDTH, _BTree),
    _BTree_Empty = b_trees_generator:delete_b_tree_from_even(4, ?NUMBER_ACTIONS, ?WIDTH, _BTree_Even),
    ?assertEqual(b_trees:empty(4), _BTree_Empty),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - random - descending
%%--------------------------------------------------------------------

delete_b_tree_order_4_random_desc_test(Config) ->
    b_trees_generator:check_equal(b_trees:empty(4), b_trees_generator:delete_b_tree_list(?config(keys_random, Config), ?config(b_tree_4_desc, Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4 - random
%%--------------------------------------------------------------------

delete_b_tree_order_4_random_test(Config) ->
    ?assertEqual(b_trees:empty(4), b_trees_generator:delete_b_tree_list(?config(keys_random, Config), ?config(b_tree_4, Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 4
%%--------------------------------------------------------------------

delete_b_tree_order_4_test(_Config) ->
    ?assertEqual(b_trees:empty(4), b_trees_generator:delete_b_tree_from(4, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 512 - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_512_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets(512, ?NUMBER_ACTIONS, ?WIDTH, delete_order_512_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(512), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 512
%%--------------------------------------------------------------------

delete_b_tree_order_512_test(_Config) ->
    ?assertEqual(b_trees:empty(512), b_trees_generator:delete_b_tree_from(512, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 5 - even / odd - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_5_ets_even_odd_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:generate_b_tree_from_number_ets(5, ?NUMBER_ACTIONS, ?WIDTH, delete_order_5_ets_even_odd, spawn(fun b_trees_generator:ets_owner/0)),
    BTree_Odd = b_trees_generator:delete_b_tree_from_even(5, ?NUMBER_ACTIONS, ?WIDTH, BTree),
    BTree_Empty = b_trees_generator:delete_b_tree_from_odd(5, ?NUMBER_ACTIONS, ?WIDTH, BTree_Odd),
    b_trees_generator:check_equal(b_trees:empty(5), BTree_Empty),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree_Empty),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 5 - odd / even - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_5_ets_odd_even_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:generate_b_tree_from_number_ets(5, ?NUMBER_ACTIONS, ?WIDTH, delete_order_5_ets_odd_even, spawn(fun b_trees_generator:ets_owner/0)),
    BTree_Even = b_trees_generator:delete_b_tree_from_odd(5, ?NUMBER_ACTIONS, ?WIDTH, BTree),
    BTree_Empty = b_trees_generator:delete_b_tree_from_even(5, ?NUMBER_ACTIONS, ?WIDTH, BTree_Even),
    b_trees_generator:check_equal(b_trees:empty(5), BTree_Empty),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree_Empty),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 5 - random - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_5_ets_random_test(Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_1 = b_trees_generator:generate_b_tree_from_number_ets(5, ?NUMBER_ACTIONS, ?WIDTH, delete_order_5_ets_random, spawn(fun b_trees_generator:ets_owner/0)),
    B_TREE_2 = b_trees_generator:delete_b_tree_list(?config(keys_random, Config), B_TREE_1),
    b_trees_generator:check_equal(b_trees:empty(5), B_TREE_2),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE_2),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 5 - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_5_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets(5, ?NUMBER_ACTIONS, ?WIDTH, delete_order_5_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(5), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 5 - even / odd
%%--------------------------------------------------------------------

delete_b_tree_order_5_even_odd_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(5, ?NUMBER_ACTIONS, ?WIDTH),
    _BTree_Odd = b_trees_generator:delete_b_tree_from_even(5, ?NUMBER_ACTIONS, ?WIDTH, _BTree),
    _BTree_Empty = b_trees_generator:delete_b_tree_from_odd(5, ?NUMBER_ACTIONS, ?WIDTH, _BTree_Odd),
    ?assertEqual(b_trees:empty(5), _BTree_Empty),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 5 - odd / even
%%--------------------------------------------------------------------

delete_b_tree_order_5_odd_even_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(5, ?NUMBER_ACTIONS, ?WIDTH),
    _BTree_Even = b_trees_generator:delete_b_tree_from_odd(5, ?NUMBER_ACTIONS, ?WIDTH, _BTree),
    _BTree_Empty = b_trees_generator:delete_b_tree_from_even(5, ?NUMBER_ACTIONS, ?WIDTH, _BTree_Even),
    ?assertEqual(b_trees:empty(5), _BTree_Empty),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 5 - random
%%--------------------------------------------------------------------

delete_b_tree_order_5_random_test(Config) ->
    ?assertEqual(b_trees:empty(5), b_trees_generator:delete_b_tree_list(?config(keys_random, Config), ?config(b_tree_5, Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 5
%%--------------------------------------------------------------------

delete_b_tree_order_5_test(_Config) ->
    ?assertEqual(b_trees:empty(5), b_trees_generator:delete_b_tree_from(5, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 64 - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_64_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets(64, ?NUMBER_ACTIONS, ?WIDTH, delete_order_64_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(64), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 64
%%--------------------------------------------------------------------

delete_b_tree_order_64_test(_Config) ->
    ?assertEqual(b_trees:empty(64), b_trees_generator:delete_b_tree_from(64, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 6 - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_6_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets(6, ?NUMBER_ACTIONS, ?WIDTH, delete_order_6_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(6), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 6
%%--------------------------------------------------------------------

delete_b_tree_order_6_test(_Config) ->
    ?assertEqual(b_trees:empty(6), b_trees_generator:delete_b_tree_from(6, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 8 - even / odd - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_8_ets_even_odd_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:generate_b_tree_from_number_ets(8, ?NUMBER_ACTIONS, ?WIDTH, delete_order_8_ets_even_odd, spawn(fun b_trees_generator:ets_owner/0)),
    BTree_Odd = b_trees_generator:delete_b_tree_from_even(8, ?NUMBER_ACTIONS, ?WIDTH, BTree),
    BTree_Empty = b_trees_generator:delete_b_tree_from_odd(8, ?NUMBER_ACTIONS, ?WIDTH, BTree_Odd),
    b_trees_generator:check_equal(b_trees:empty(8), BTree_Empty),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree_Empty),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 8 - odd / even - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_8_ets_odd_even_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:generate_b_tree_from_number_ets(8, ?NUMBER_ACTIONS, ?WIDTH, delete_order_8_ets_odd_even, spawn(fun b_trees_generator:ets_owner/0)),
    BTree_Even = b_trees_generator:delete_b_tree_from_odd(8, ?NUMBER_ACTIONS, ?WIDTH, BTree),
    BTree_Empty = b_trees_generator:delete_b_tree_from_even(8, ?NUMBER_ACTIONS, ?WIDTH, BTree_Even),
    b_trees_generator:check_equal(b_trees:empty(8), BTree_Empty),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree_Empty),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 8 - random - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_8_ets_random_test(Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_1 = b_trees_generator:generate_b_tree_from_number_ets(8, ?NUMBER_ACTIONS, ?WIDTH, delete_order_8_ets_random, spawn(fun b_trees_generator:ets_owner/0)),
    B_TREE_2 = b_trees_generator:delete_b_tree_list(?config(keys_random, Config), B_TREE_1),
    b_trees_generator:check_equal(b_trees:empty(8), B_TREE_2),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE_2),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 8 - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_order_8_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE = b_trees_generator:delete_b_tree_from_ets(8, ?NUMBER_ACTIONS, ?WIDTH, delete_order_8_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(8), B_TREE),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 8 - even / odd
%%--------------------------------------------------------------------

delete_b_tree_order_8_even_odd_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(8, ?NUMBER_ACTIONS, ?WIDTH),
    _BTree_Odd = b_trees_generator:delete_b_tree_from_even(8, ?NUMBER_ACTIONS, ?WIDTH, _BTree),
    _BTree_Empty = b_trees_generator:delete_b_tree_from_odd(8, ?NUMBER_ACTIONS, ?WIDTH, _BTree_Odd),
    ?assertEqual(b_trees:empty(8), _BTree_Empty),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 8 - odd / even
%%--------------------------------------------------------------------

delete_b_tree_order_8_odd_even_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(8, ?NUMBER_ACTIONS, ?WIDTH),
    _BTree_Even = b_trees_generator:delete_b_tree_from_odd(8, ?NUMBER_ACTIONS, ?WIDTH, _BTree),
    _BTree_Empty = b_trees_generator:delete_b_tree_from_even(8, ?NUMBER_ACTIONS, ?WIDTH, _BTree_Even),
    ?assertEqual(b_trees:empty(8), _BTree_Empty),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 8 - random
%%--------------------------------------------------------------------

delete_b_tree_order_8_random_test(Config) ->
    ?assertEqual(b_trees:empty(8), b_trees_generator:delete_b_tree_list(?config(keys_random, Config), ?config(b_tree_8, Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete b_tree order 8
%%--------------------------------------------------------------------

delete_b_tree_order_8_test(_Config) ->
    ?assertEqual(b_trees:empty(8), b_trees_generator:delete_b_tree_from(8, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete gb_tree
%%--------------------------------------------------------------------

delete_gb_tree_test(_Config) ->
    ?assertEqual(gb_trees:empty(), b_trees_generator:delete_gb_tree_from(?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 1024 - persistence by dets
%%--------------------------------------------------------------------

enter_b_tree_order_1024_dets_test(Config) ->
    {ok, _} = dets:open_file(b_tree_1024_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_1024_dets"}]),
    {ok, _} = dets:open_file(b_tree_1024_dets_new, [{file, ?DIRECTORY_DETS ++ "b_tree_1024_dets_new"}]),
    KeyValues = ?config(key_values_from, Config),
    {ok, _} = dets:open_file(enter_order_1024, [{file, ?DIRECTORY_DETS ++ "enter_order_1024"}]),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(1024), state, {enter_order_1024, fun b_trees_generator:persistence_by_dets/3, fun b_trees_generator:persistence_by_dets/3, fun b_trees_generator:persistence_by_dets/3}),
    BTree = enter_b_tree(KeyValues, B_TREE_EMPTY),
    b_trees_generator:check_equal(?config(b_tree_1024_dets, Config), BTree),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    BTreeUpdate = enter_b_tree(KeyValuesUpdate, BTree),
    b_trees_generator:check_equal(?config(b_tree_1024_dets_new, Config), BTreeUpdate),
    ok = dets:close(enter_order_1024),
    ok = dets:close(b_tree_1024_dets_new),
    dets:close(b_tree_1024_dets).

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 1024 - persistence by ets
%%--------------------------------------------------------------------

enter_b_tree_order_1024_ets_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    StateTarget = ets:new(enter_order_1024, []),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(1024), state, {StateTarget, fun b_trees_generator:persistence_by_ets/3, fun b_trees_generator:persistence_by_ets/3, fun b_trees_generator:persistence_by_ets/3}),
    BTree = enter_b_tree(KeyValues, B_TREE_EMPTY),
    b_trees_generator:check_equal(?config(b_tree_1024_ets, Config), BTree),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    BTreeUpdate = enter_b_tree(KeyValuesUpdate, BTree),
    b_trees_generator:check_equal(?config(b_tree_1024_ets_new, Config), BTreeUpdate),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTreeUpdate),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    true = ets:delete(StateTarget),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 1024 - persistence by mnesia
%%--------------------------------------------------------------------

enter_b_tree_order_1024_mnesia_test(Config) ->

    KeyValues = ?config(key_values_from, Config),
    {atomic, ok} = mnesia:create_table(enter_order_1024, [{record_name, subtrees}]),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(1024), state, {enter_order_1024, fun b_trees_generator:persistence_by_mnesia/3, fun b_trees_generator:persistence_by_mnesia/3, fun b_trees_generator:persistence_by_mnesia/3}),
    BTree = enter_b_tree(KeyValues, B_TREE_EMPTY),
    b_trees_generator:check_equal(?config(b_tree_1024_mnesia, Config), BTree),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    BTreeUpdate = enter_b_tree(KeyValuesUpdate, BTree),
    b_trees_generator:check_equal(?config(b_tree_1024_mnesia_new, Config), BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 1024
%%--------------------------------------------------------------------

enter_b_tree_order_1024_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(1024)),
    ?assertEqual(?config(b_tree_1024, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(b_tree_1024_new, Config), _BTreeUpdate),
    ok.

enter_b_tree([], BTree) ->
    BTree;
enter_b_tree([{Key, Value} | Tail], BTree) ->
    enter_b_tree(Tail, b_trees:enter(Key, Value, BTree)).

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 128
%%--------------------------------------------------------------------

enter_b_tree_order_128_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(128)),
    ?assertEqual(?config(b_tree_128, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(b_tree_128_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 16
%%--------------------------------------------------------------------

enter_b_tree_order_16_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(16)),
    ?assertEqual(?config(b_tree_16, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(b_tree_16_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 256
%%--------------------------------------------------------------------

enter_b_tree_order_256_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(256)),
    ?assertEqual(?config(b_tree_256, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(b_tree_256_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 32
%%--------------------------------------------------------------------

enter_b_tree_order_32_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(32)),
    ?assertEqual(?config(b_tree_32, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(b_tree_32_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

enter_b_tree_order_4_dets_test(Config) ->
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    {ok, _} = dets:open_file(b_tree_4_dets_new, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets_new"}]),
    KeyValues = ?config(key_values_from, Config),
    {ok, _} = dets:open_file(enter_order_4, [{file, ?DIRECTORY_DETS ++ "enter_order_4"}]),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(4), state, {enter_order_4, fun b_trees_generator:persistence_by_dets/3, fun b_trees_generator:persistence_by_dets/3, fun b_trees_generator:persistence_by_dets/3}),
    BTree = enter_b_tree(KeyValues, B_TREE_EMPTY),
    b_trees_generator:check_equal(?config(b_tree_4_dets, Config), BTree),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    BTreeUpdate = enter_b_tree(KeyValuesUpdate, BTree),
    b_trees_generator:check_equal(?config(b_tree_4_dets_new, Config), BTreeUpdate),
    ok = dets:close(enter_order_4),
    ok = dets:close(b_tree_4_dets_new),
    dets:close(b_tree_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

enter_b_tree_order_4_ets_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    StateTarget = ets:new(enter_order_4, []),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(4), state, {StateTarget, fun b_trees_generator:persistence_by_ets/3, fun b_trees_generator:persistence_by_ets/3, fun b_trees_generator:persistence_by_ets/3}),
    BTree = enter_b_tree(KeyValues, B_TREE_EMPTY),
    b_trees_generator:check_equal(?config(b_tree_4_ets, Config), BTree),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    BTreeUpdate = enter_b_tree(KeyValuesUpdate, BTree),
    b_trees_generator:check_equal(?config(b_tree_4_ets_new, Config), BTreeUpdate),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTreeUpdate),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    true = ets:delete(StateTarget),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

enter_b_tree_order_4_mnesia_test(Config) ->

    KeyValues = ?config(key_values_from, Config),
    {atomic, ok} = mnesia:create_table(enter_order_4, [{record_name, subtrees}]),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(4), state, {enter_order_4, fun b_trees_generator:persistence_by_mnesia/3, fun b_trees_generator:persistence_by_mnesia/3, fun b_trees_generator:persistence_by_mnesia/3}),
    BTree = enter_b_tree(KeyValues, B_TREE_EMPTY),
    b_trees_generator:check_equal(?config(b_tree_4_mnesia, Config), BTree),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    BTreeUpdate = enter_b_tree(KeyValuesUpdate, BTree),
    b_trees_generator:check_equal(?config(b_tree_4_mnesia_new, Config), BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 4
%%--------------------------------------------------------------------

enter_b_tree_order_4_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(4)),
    ?assertEqual(?config(b_tree_4, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(b_tree_4_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 5
%%--------------------------------------------------------------------

enter_b_tree_order_5_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(5)),
    ?assertEqual(?config(b_tree_5, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(b_tree_5_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 6
%%--------------------------------------------------------------------

enter_b_tree_order_6_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(6)),
    ?assertEqual(?config(b_tree_6, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(b_tree_6_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 512
%%--------------------------------------------------------------------

enter_b_tree_order_512_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(512)),
    ?assertEqual(?config(b_tree_512, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(b_tree_512_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 64
%%--------------------------------------------------------------------

enter_b_tree_order_64_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(64)),
    ?assertEqual(?config(b_tree_64, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(b_tree_64_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter b_tree order 8
%%--------------------------------------------------------------------

enter_b_tree_order_8_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    BTree = enter_b_tree(KeyValues, b_trees:empty(8)),
    ?assertEqual(?config(b_tree_8, Config), BTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _BTreeUpdate = enter_b_tree(_KeyValuesUpdate, BTree),
    ?assertEqual(?config(b_tree_8_new, Config), _BTreeUpdate),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter gb_tree
%%--------------------------------------------------------------------

enter_gb_tree_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    GBTree = enter_gb_tree(KeyValues, gb_trees:empty()),
    ?assertEqual(?config(gb_tree, Config), GBTree),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    _GBTreeUpdate = enter_gb_tree(_KeyValuesUpdate, GBTree),
    ?assertEqual(?config(gb_tree_new, Config), _GBTreeUpdate),
    ok.

enter_gb_tree([], GBTree) ->
    GBTree;
enter_gb_tree([{Key, Value} | Tail], GBTree) ->
    enter_gb_tree(Tail, gb_trees:enter(Key, Value, GBTree)).

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 1024 - persistence by dets
%%--------------------------------------------------------------------

from_dict_b_tree_order_1024_dets_test(Config) ->
    {ok, _} = dets:open_file(b_tree_1024_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_1024_dets"}]),
    BTree = ?config(b_tree_1024_dets, Config),
    KeyValues = ?config(key_values_from, Config),
    {ok, _} = dets:open_file(from_dict_order_1024, [{file, ?DIRECTORY_DETS ++ "from_dict_order_1024"}]),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(1024), state, {from_dict_order_1024, fun b_trees_generator:persistence_by_dets/3, fun b_trees_generator:persistence_by_dets/3, fun b_trees_generator:persistence_by_dets/3}),
    b_trees_generator:check_equal(BTree, b_trees:from_dict(B_TREE_EMPTY, KeyValues)),
    dets:close(b_tree_1024_dets).

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 1024 - persistence by ets
%%--------------------------------------------------------------------

from_dict_b_tree_order_1024_ets_test(Config) ->
    BTree = ?config(b_tree_1024_ets, Config),
    KeyValues = ?config(key_values_from, Config),
    StateTarget = ets:new(from_dict_order_1024, []),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(1024), state, {StateTarget, fun b_trees_generator:persistence_by_ets/3, fun b_trees_generator:persistence_by_ets/3, fun b_trees_generator:persistence_by_ets/3}),
    B_TREE_FROM_DICT = b_trees:from_dict(B_TREE_EMPTY, KeyValues),
    b_trees_generator:check_equal(BTree, B_TREE_FROM_DICT),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE_FROM_DICT),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    true = ets:delete(StateTarget),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 1024 - persistence by mnesia
%%--------------------------------------------------------------------

from_dict_b_tree_order_1024_mnesia_test(Config) ->
    BTree = ?config(b_tree_1024_mnesia, Config),
    KeyValues = ?config(key_values_from, Config),
    {atomic, ok} = mnesia:create_table(from_dict_order_1024, [{record_name, subtrees}]),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(1024), state, {from_dict_order_1024, fun b_trees_generator:persistence_by_mnesia/3, fun b_trees_generator:persistence_by_mnesia/3, fun b_trees_generator:persistence_by_mnesia/3}),
    b_trees_generator:check_equal(BTree, b_trees:from_dict(B_TREE_EMPTY, KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 1024
%%--------------------------------------------------------------------

from_dict_b_tree_order_1024_test(Config) ->
    _BTree = ?config(b_tree_1024, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(b_trees:empty(1024), _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 128
%%--------------------------------------------------------------------

from_dict_b_tree_order_128_test(Config) ->
    _BTree = ?config(b_tree_128, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(b_trees:empty(128), _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 16
%%--------------------------------------------------------------------

from_dict_b_tree_order_16_test(Config) ->
    _BTree = ?config(b_tree_16, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(b_trees:empty(16), _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 256
%%--------------------------------------------------------------------

from_dict_b_tree_order_256_test(Config) ->
    _BTree = ?config(b_tree_256, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(b_trees:empty(256), _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 32
%%--------------------------------------------------------------------

from_dict_b_tree_order_32_test(Config) ->
    _BTree = ?config(b_tree_32, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(b_trees:empty(32), _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 4 - even / odd
%%--------------------------------------------------------------------

from_dict_b_tree_order_4_even_odd_test(Config) ->
    _BTree = ?config(b_tree_4, Config),
    _KeyValues = ?config(key_values_from_even_odd, Config),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(b_trees:from_dict(b_trees:empty(4), _KeyValues))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 4 - odd / even
%%--------------------------------------------------------------------

from_dict_b_tree_order_4_odd_even_test(Config) ->
    _BTree = ?config(b_tree_4, Config),
    _KeyValues = ?config(key_values_from_odd_even, Config),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(b_trees:from_dict(b_trees:empty(4), _KeyValues))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 4 - random
%%--------------------------------------------------------------------

from_dict_b_tree_order_4_random_test(Config) ->
    _BTree = ?config(b_tree_4, Config),
    _KeyValues = ?config(key_values_random, Config),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(b_trees:from_dict(b_trees:empty(4), _KeyValues))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

from_dict_b_tree_order_4_dets_test(Config) ->
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    BTree = ?config(b_tree_4_dets, Config),
    KeyValues = ?config(key_values_from, Config),
    {ok, _} = dets:open_file(from_dict_order_4, [{file, ?DIRECTORY_DETS ++ "from_dict_order_4"}]),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(4), state, {from_dict_order_4, fun b_trees_generator:persistence_by_dets/3, fun b_trees_generator:persistence_by_dets/3, fun b_trees_generator:persistence_by_dets/3}),
    b_trees_generator:check_equal(BTree, b_trees:from_dict(B_TREE_EMPTY, KeyValues)),
    dets:close(b_tree_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

from_dict_b_tree_order_4_ets_test(Config) ->
    BTree = ?config(b_tree_4_ets, Config),
    KeyValues = ?config(key_values_from, Config),
    StateTarget = ets:new(from_dict_order_4, []),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(4), state, {StateTarget, fun b_trees_generator:persistence_by_ets/3, fun b_trees_generator:persistence_by_ets/3, fun b_trees_generator:persistence_by_ets/3}),
    B_TREE_FROM_DICT = b_trees:from_dict(B_TREE_EMPTY, KeyValues),
    b_trees_generator:check_equal(BTree, B_TREE_FROM_DICT),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(B_TREE_FROM_DICT),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    true = ets:delete(StateTarget),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

from_dict_b_tree_order_4_mnesia_test(Config) ->
    BTree = ?config(b_tree_4_mnesia, Config),
    KeyValues = ?config(key_values_from, Config),
    {atomic, ok} = mnesia:create_table(from_dict_order_4, [{record_name, subtrees}]),
    B_TREE_EMPTY = b_trees:set_parameter(b_trees:empty(4), state, {from_dict_order_4, fun b_trees_generator:persistence_by_mnesia/3, fun b_trees_generator:persistence_by_mnesia/3, fun b_trees_generator:persistence_by_mnesia/3}),
    b_trees_generator:check_equal(BTree, b_trees:from_dict(B_TREE_EMPTY, KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 4
%%--------------------------------------------------------------------

from_dict_b_tree_order_4_test(Config) ->
    _BTree = ?config(b_tree_4, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(b_trees:empty(4), _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 5 - even / odd
%%--------------------------------------------------------------------

from_dict_b_tree_order_5_even_odd_test(Config) ->
    _BTree = ?config(b_tree_5, Config),
    _KeyValues = ?config(key_values_from_even_odd, Config),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(b_trees:from_dict(b_trees:empty(5), _KeyValues))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 5 - odd / even
%%--------------------------------------------------------------------

from_dict_b_tree_order_5_odd_even_test(Config) ->
    _BTree = ?config(b_tree_5, Config),
    _KeyValues = ?config(key_values_from_odd_even, Config),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(b_trees:from_dict(b_trees:empty(5), _KeyValues))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 5 - random
%%--------------------------------------------------------------------

from_dict_b_tree_order_5_random_test(Config) ->
    _BTree = ?config(b_tree_5, Config),
    _KeyValues = ?config(key_values_random, Config),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(b_trees:from_dict(b_trees:empty(5), _KeyValues))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 5
%%--------------------------------------------------------------------

from_dict_b_tree_order_5_test(Config) ->
    _BTree = ?config(b_tree_5, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(b_trees:empty(5), _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 6
%%--------------------------------------------------------------------

from_dict_b_tree_order_6_test(Config) ->
    _BTree = ?config(b_tree_6, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(b_trees:empty(6), _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 512
%%--------------------------------------------------------------------

from_dict_b_tree_order_512_test(Config) ->
    _BTree = ?config(b_tree_512, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(b_trees:empty(512), _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 64
%%--------------------------------------------------------------------

from_dict_b_tree_order_64_test(Config) ->
    _BTree = ?config(b_tree_64, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(b_trees:empty(64), _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 8 - even / odd
%%--------------------------------------------------------------------

from_dict_b_tree_order_8_even_odd_test(Config) ->
    _BTree = ?config(b_tree_8, Config),
    _KeyValues = ?config(key_values_from_even_odd, Config),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(b_trees:from_dict(b_trees:empty(8), _KeyValues))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 8 - odd / even
%%--------------------------------------------------------------------

from_dict_b_tree_order_8_odd_even_test(Config) ->
    _BTree = ?config(b_tree_8, Config),
    _KeyValues = ?config(key_values_from_odd_even, Config),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(b_trees:from_dict(b_trees:empty(8), _KeyValues))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 8 - random
%%--------------------------------------------------------------------

from_dict_b_tree_order_8_random_test(Config) ->
    _BTree = ?config(b_tree_8, Config),
    _KeyValues = ?config(key_values_random, Config),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(b_trees:from_dict(b_trees:empty(8), _KeyValues))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict b_tree order 8
%%--------------------------------------------------------------------

from_dict_b_tree_order_8_test(Config) ->
    _BTree = ?config(b_tree_8, Config),
    _KeyValues = ?config(key_values_from, Config),
    ?assertEqual(_BTree, b_trees:from_dict(b_trees:empty(8), _KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict gb_tree
%%--------------------------------------------------------------------

from_dict_gb_tree_test(Config) ->
    KeyValues = ?config(key_values_from, Config),
    gb_trees:from_orddict(KeyValues),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 1024
%%--------------------------------------------------------------------

get_b_tree_order_1024_test(Config) ->
    BTree = ?config(b_tree_1024, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

get_b_tree([], _) ->
    none;
get_b_tree([Key | Tail], BTree) ->
    b_trees:get(Key, BTree),
    get_b_tree(Tail, BTree).

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 128
%%--------------------------------------------------------------------

get_b_tree_order_128_test(Config) ->
    BTree = ?config(b_tree_128, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 16
%%--------------------------------------------------------------------

get_b_tree_order_16_test(Config) ->
    BTree = ?config(b_tree_16, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 256
%%--------------------------------------------------------------------

get_b_tree_order_256_test(Config) ->
    BTree = ?config(b_tree_256, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 32
%%--------------------------------------------------------------------

get_b_tree_order_32_test(Config) ->
    BTree = ?config(b_tree_32, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

get_b_tree_order_4_dets_test(Config) ->
    BTree = ?config(b_tree_4_dets, Config),
    Keys = ?config(keys_random, Config),
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    get_b_tree(Keys, BTree),
    dets:close(b_tree_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

get_b_tree_order_4_ets_test(Config) ->
    BTree = ?config(b_tree_4_ets, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

get_b_tree_order_4_mnesia_test(Config) ->
    BTree = ?config(b_tree_4_mnesia, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 4
%%--------------------------------------------------------------------

get_b_tree_order_4_test(Config) ->
    BTree = ?config(b_tree_4, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 5
%%--------------------------------------------------------------------

get_b_tree_order_5_test(Config) ->
    BTree = ?config(b_tree_5, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 6
%%--------------------------------------------------------------------

get_b_tree_order_6_test(Config) ->
    BTree = ?config(b_tree_6, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 512
%%--------------------------------------------------------------------

get_b_tree_order_512_test(Config) ->
    BTree = ?config(b_tree_512, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 64
%%--------------------------------------------------------------------

get_b_tree_order_64_test(Config) ->
    BTree = ?config(b_tree_64, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get b_tree order 8
%%--------------------------------------------------------------------

get_b_tree_order_8_test(Config) ->
    BTree = ?config(b_tree_8, Config),
    Keys = ?config(keys_random, Config),
    get_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get gb_tree
%%--------------------------------------------------------------------

get_gb_tree_test(Config) ->
    GBTree = ?config(gb_tree, Config),
    Keys = ?config(keys_random, Config),
    get_gb_tree(Keys, GBTree),
    ok.

get_gb_tree([], _) ->
    none;
get_gb_tree([Key | Tail], GBTree) ->
    gb_trees:get(Key, GBTree),
    get_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 1024 - persistence by dets
%%--------------------------------------------------------------------

insert_b_tree_order_1024_dets_test(_Config) ->
    BTree = b_trees_generator:generate_b_tree_from_number_dets(1024, ?NUMBER_ACTIONS, ?WIDTH, insert_order_1024_dets),
    {ok, _} = dets:open_file(b_tree_1024_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_1024_dets"}]),
    {ok, _} = dets:open_file(insert_order_1024_dets, [{file, ?DIRECTORY_DETS ++ "insert_order_1024_dets"}]),
    b_trees_generator:check_equal(?config(b_tree_1024_dets, _Config), BTree),
    ?assert(b_trees:height(BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(1024 div 2)))),
    dets:close(b_tree_1024_dets),
    dets:close(insert_order_1024_dets).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 1024 - persistence by ets
%%--------------------------------------------------------------------

insert_b_tree_order_1024_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:generate_b_tree_from_number_ets(1024, ?NUMBER_ACTIONS, ?WIDTH, insert_order_1024_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(?config(b_tree_1024_ets, _Config), BTree),
    ?assert(b_trees:height(BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(1024 div 2)))),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 1024 - persistence by mnesia
%%--------------------------------------------------------------------

insert_b_tree_order_1024_mnesia_test(_Config) ->
    BTree = b_trees_generator:generate_b_tree_from_number_mnesia(1024, ?NUMBER_ACTIONS, ?WIDTH, insert_order_1024_mnesia),
    b_trees_generator:check_equal(?config(b_tree_1024_mnesia, _Config), BTree),
    ?assert(b_trees:height(BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(1024 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 1024
%%--------------------------------------------------------------------

insert_b_tree_order_1024_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(1024, ?NUMBER_ACTIONS, ?WIDTH),
    ?assertEqual(?config(b_tree_1024, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(1024 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 128
%%--------------------------------------------------------------------

insert_b_tree_order_128_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(128, ?NUMBER_ACTIONS, ?WIDTH),
    ?assertEqual(?config(b_tree_128, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(128 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 16
%%--------------------------------------------------------------------

insert_b_tree_order_16_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(16, ?NUMBER_ACTIONS, ?WIDTH),
    ?assertEqual(?config(b_tree_16, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(16 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 256
%%--------------------------------------------------------------------

insert_b_tree_order_256_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(256, ?NUMBER_ACTIONS, ?WIDTH),
    ?assertEqual(?config(b_tree_256, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(256 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 32
%%--------------------------------------------------------------------

insert_b_tree_order_32_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(32, ?NUMBER_ACTIONS, ?WIDTH),
    ?assertEqual(?config(b_tree_32, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(32 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

insert_b_tree_order_4_dets_test(_Config) ->
    BTree = b_trees_generator:generate_b_tree_from_number_dets(4, ?NUMBER_ACTIONS, ?WIDTH, insert_order_4_dets),
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    {ok, _} = dets:open_file(insert_order_4_dets, [{file, ?DIRECTORY_DETS ++ "insert_order_4_dets"}]),
    b_trees_generator:check_equal(?config(b_tree_4_dets, _Config), BTree),
    ?assert(b_trees:height(BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(4 div 2)))),
    dets:close(b_tree_4_dets),
    dets:close(insert_order_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

insert_b_tree_order_4_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:generate_b_tree_from_number_ets(4, ?NUMBER_ACTIONS, ?WIDTH, insert_order_4_ets, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(?config(b_tree_4_ets, _Config), BTree),
    ?assert(b_trees:height(BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(4 div 2)))),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 4 - even / odd
%%--------------------------------------------------------------------

insert_b_tree_order_4_even_odd_test(Config) ->
    KeyValues = ?config(key_values_from_even_odd, Config),
    _BTree = b_trees_generator:generate_b_tree_list_and_order(KeyValues, 4),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(_BTree)),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(4 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

insert_b_tree_order_4_mnesia_test(_Config) ->
    BTree = b_trees_generator:generate_b_tree_from_number_mnesia(4, ?NUMBER_ACTIONS, ?WIDTH, insert_order_4_mnesia),
    b_trees_generator:check_equal(?config(b_tree_4_mnesia, _Config), BTree),
    ?assert(b_trees:height(BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(4 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 4 - odd / even
%%--------------------------------------------------------------------

insert_b_tree_order_4_odd_even_test(Config) ->
    KeyValues = ?config(key_values_from_odd_even, Config),
    _BTree = b_trees_generator:generate_b_tree_list_and_order(KeyValues, 4),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(_BTree)),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(4 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 4 - random
%%--------------------------------------------------------------------

insert_b_tree_order_4_random_test(Config) ->
    KeyValues = ?config(key_values_random, Config),
    _BTree = b_trees_generator:generate_b_tree_list_and_order(KeyValues, 4),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(_BTree)),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(4 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 4
%%--------------------------------------------------------------------

insert_b_tree_order_4_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(4, ?NUMBER_ACTIONS, ?WIDTH),
    ?assertEqual(?config(b_tree_4, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(4 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 4 - till
%%--------------------------------------------------------------------

insert_b_tree_order_4_till_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_till_number(4, ?NUMBER_ACTIONS, ?WIDTH),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(4 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 5 - even / odd
%%--------------------------------------------------------------------

insert_b_tree_order_5_even_odd_test(Config) ->
    KeyValues = ?config(key_values_from_even_odd, Config),
    _BTree = b_trees_generator:generate_b_tree_list_and_order(KeyValues, 5),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(_BTree)),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(5 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 5 - odd / even
%%--------------------------------------------------------------------

insert_b_tree_order_5_odd_even_test(Config) ->
    KeyValues = ?config(key_values_from_odd_even, Config),
    _BTree = b_trees_generator:generate_b_tree_list_and_order(KeyValues, 5),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(_BTree)),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(5 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 5 - random
%%--------------------------------------------------------------------

insert_b_tree_order_5_random_test(Config) ->
    KeyValues = ?config(key_values_random, Config),
    _BTree = b_trees_generator:generate_b_tree_list_and_order(KeyValues, 5),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(_BTree)),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(5 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 5
%%--------------------------------------------------------------------

insert_b_tree_order_5_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(5, ?NUMBER_ACTIONS, ?WIDTH),
    ?assertEqual(?config(b_tree_5, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(5 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 5 - till
%%--------------------------------------------------------------------

insert_b_tree_order_5_till_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_till_number(5, ?NUMBER_ACTIONS, ?WIDTH),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(5 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 6
%%--------------------------------------------------------------------

insert_b_tree_order_6_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_till_number(6, ?NUMBER_ACTIONS, ?WIDTH),
    ?assertEqual(?config(b_tree_6_till, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(6 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 6 - till
%%--------------------------------------------------------------------

insert_b_tree_order_6_till_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_till_number(6, ?NUMBER_ACTIONS, ?WIDTH),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(6 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 512
%%--------------------------------------------------------------------

insert_b_tree_order_512_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(512, ?NUMBER_ACTIONS, ?WIDTH),
    ?assertEqual(?config(b_tree_512, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(512 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 64
%%--------------------------------------------------------------------

insert_b_tree_order_64_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(64, ?NUMBER_ACTIONS, ?WIDTH),
    ?assertEqual(?config(b_tree_64, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(64 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 8 - even / odd
%%--------------------------------------------------------------------

insert_b_tree_order_8_even_odd_test(Config) ->
    KeyValues = ?config(key_values_from_even_odd, Config),
    _BTree = b_trees_generator:generate_b_tree_list_and_order(KeyValues, 8),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(_BTree)),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(8 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 8 - odd / even
%%--------------------------------------------------------------------

insert_b_tree_order_8_odd_even_test(Config) ->
    KeyValues = ?config(key_values_from_odd_even, Config),
    _BTree = b_trees_generator:generate_b_tree_list_and_order(KeyValues, 8),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(_BTree)),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(8 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 8 - random
%%--------------------------------------------------------------------

insert_b_tree_order_8_random_test(Config) ->
    KeyValues = ?config(key_values_random, Config),
    _BTree = b_trees_generator:generate_b_tree_list_and_order(KeyValues, 8),
    ?assertEqual(?NUMBER_ACTIONS, b_trees:size_key_values(_BTree)),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(8 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert b_tree order 8
%%--------------------------------------------------------------------

insert_b_tree_order_8_test(_Config) ->
    _BTree = b_trees_generator:generate_b_tree_from_number(8, ?NUMBER_ACTIONS, ?WIDTH),
    ?assertEqual(?config(b_tree_8, _Config), _BTree),
    ?assert(b_trees:height(_BTree) =< trunc((math:log((?NUMBER_ACTIONS + 1) / 2) / math:log(8 div 2)))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert gb_tree
%%--------------------------------------------------------------------

insert_gb_tree_test(_Config) ->
    ?assertEqual(?config(gb_tree, _Config), b_trees_generator:generate_gb_tree_from_number(?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 1024
%%--------------------------------------------------------------------

is_defined_b_tree_order_1024_test(Config) ->
    BTree = ?config(b_tree_1024, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

is_defined_b_tree([], _) ->
    none;
is_defined_b_tree([Key | Tail], BTree) ->
    b_trees:is_defined(Key, BTree),
    is_defined_b_tree(Tail, BTree).

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 128
%%--------------------------------------------------------------------

is_defined_b_tree_order_128_test(Config) ->
    BTree = ?config(b_tree_128, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 16
%%--------------------------------------------------------------------

is_defined_b_tree_order_16_test(Config) ->
    BTree = ?config(b_tree_16, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 256
%%--------------------------------------------------------------------

is_defined_b_tree_order_256_test(Config) ->
    BTree = ?config(b_tree_256, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 32
%%--------------------------------------------------------------------

is_defined_b_tree_order_32_test(Config) ->
    BTree = ?config(b_tree_32, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

is_defined_b_tree_order_4_dets_test(Config) ->
    BTree = ?config(b_tree_4_dets, Config),
    Keys = ?config(keys_random, Config),
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    is_defined_b_tree(Keys, BTree),
    dets:close(b_tree_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

is_defined_b_tree_order_4_ets_test(Config) ->
    BTree = ?config(b_tree_4_ets, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

is_defined_b_tree_order_4_mnesia_test(Config) ->
    BTree = ?config(b_tree_4_mnesia, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 4
%%--------------------------------------------------------------------

is_defined_b_tree_order_4_test(Config) ->
    BTree = ?config(b_tree_4, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 5
%%--------------------------------------------------------------------

is_defined_b_tree_order_5_test(Config) ->
    BTree = ?config(b_tree_5, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 6
%%--------------------------------------------------------------------

is_defined_b_tree_order_6_test(Config) ->
    BTree = ?config(b_tree_6, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 512
%%--------------------------------------------------------------------

is_defined_b_tree_order_512_test(Config) ->
    BTree = ?config(b_tree_512, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 64
%%--------------------------------------------------------------------

is_defined_b_tree_order_64_test(Config) ->
    BTree = ?config(b_tree_64, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined b_tree order 8
%%--------------------------------------------------------------------

is_defined_b_tree_order_8_test(Config) ->
    BTree = ?config(b_tree_8, Config),
    Keys = ?config(keys_random, Config),
    is_defined_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined gb_tree
%%--------------------------------------------------------------------

is_defined_gb_tree_test(Config) ->
    GBTree = ?config(gb_tree, Config),
    Keys = ?config(keys_random, Config),
    is_defined_gb_tree(Keys, GBTree),
    ok.

is_defined_gb_tree([], _) ->
    none;
is_defined_gb_tree([Key | Tail], GBTree) ->
    gb_trees:is_defined(Key, GBTree),
    is_defined_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 1024 - persistence by dets
%%--------------------------------------------------------------------

iterate_next_b_tree_order_1024_dets_test(Config) ->
    BTree = ?config(b_tree_1024_dets, Config),
    {ok, _} = dets:open_file(b_tree_1024_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_1024_dets"}]),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    dets:close(b_tree_1024_dets).

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 1024 - persistence by ets
%%--------------------------------------------------------------------

iterate_next_b_tree_order_1024_ets_test(Config) ->
    BTree = ?config(b_tree_1024_ets, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 1024 - persistence by mnesia
%%--------------------------------------------------------------------

iterate_next_b_tree_order_1024_mnesia_test(Config) ->
    BTree = ?config(b_tree_1024_mnesia, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 1024
%%--------------------------------------------------------------------

iterate_next_b_tree_order_1024_test(Config) ->
    BTree = ?config(b_tree_1024, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

iterate_next_b_tree(_, 0, KeyValues) ->
    KeyValues;
iterate_next_b_tree(Iterator, Count, KeyValues) ->
    {Key, Value, IteratorNew} = b_trees:next(Iterator),
    iterate_next_b_tree(
        IteratorNew,
        Count - 1,
        lists:append([
            KeyValues,
            [{Key, Value}]
        ])).

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 128
%%--------------------------------------------------------------------

iterate_next_b_tree_order_128_test(Config) ->
    BTree = ?config(b_tree_128, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 16
%%--------------------------------------------------------------------

iterate_next_b_tree_order_16_test(Config) ->
    BTree = ?config(b_tree_16, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 256
%%--------------------------------------------------------------------

iterate_next_b_tree_order_256_test(Config) ->
    BTree = ?config(b_tree_256, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 32
%%--------------------------------------------------------------------

iterate_next_b_tree_order_32_test(Config) ->
    BTree = ?config(b_tree_32, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

iterate_next_b_tree_order_4_dets_test(Config) ->
    BTree = ?config(b_tree_4_dets, Config),
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    dets:close(b_tree_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

iterate_next_b_tree_order_4_ets_test(Config) ->
    BTree = ?config(b_tree_4_ets, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

iterate_next_b_tree_order_4_mnesia_test(Config) ->
    BTree = ?config(b_tree_4_mnesia, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 4
%%--------------------------------------------------------------------

iterate_next_b_tree_order_4_test(Config) ->
    BTree = ?config(b_tree_4, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 5
%%--------------------------------------------------------------------

iterate_next_b_tree_order_5_test(Config) ->
    BTree = ?config(b_tree_5, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 6
%%--------------------------------------------------------------------

iterate_next_b_tree_order_6_test(Config) ->
    BTree = ?config(b_tree_6, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 512
%%--------------------------------------------------------------------

iterate_next_b_tree_order_512_test(Config) ->
    BTree = ?config(b_tree_512, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 64
%%--------------------------------------------------------------------

iterate_next_b_tree_order_64_test(Config) ->
    BTree = ?config(b_tree_64, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next b_tree order 8
%%--------------------------------------------------------------------

iterate_next_b_tree_order_8_test(Config) ->
    BTree = ?config(b_tree_8, Config),
    _Iterator = b_trees:iterator(BTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_b_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterate & next gb_tree
%%--------------------------------------------------------------------

iterate_next_gb_tree_test(Config) ->
    GBTree = ?config(gb_tree, Config),
    _Iterator = gb_trees:iterator(GBTree),
    ?assertEqual(?config(key_values_from, Config), iterate_next_gb_tree(_Iterator, ?NUMBER_ACTIONS, [])),
    ok.

iterate_next_gb_tree(_, 0, KeyValues) ->
    KeyValues;
iterate_next_gb_tree(Iterator, Count, KeyValues) ->
    {Key, Value, IteratorNew} = gb_trees:next(Iterator),
    iterate_next_gb_tree(
        IteratorNew,
        Count - 1,
        lists:append([
            KeyValues,
            [{Key, Value}]
        ])).

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 1024
%%--------------------------------------------------------------------

keys_b_tree_order_1024_test(Config) ->
    BTree = ?config(b_tree_1024, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 128
%%--------------------------------------------------------------------

keys_b_tree_order_128_test(Config) ->
    BTree = ?config(b_tree_128, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 16
%%--------------------------------------------------------------------

keys_b_tree_order_16_test(Config) ->
    BTree = ?config(b_tree_16, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 256
%%--------------------------------------------------------------------

keys_b_tree_order_256_test(Config) ->
    BTree = ?config(b_tree_256, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 32
%%--------------------------------------------------------------------

keys_b_tree_order_32_test(Config) ->
    BTree = ?config(b_tree_32, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

keys_b_tree_order_4_dets_test(Config) ->
    BTree = ?config(b_tree_4_dets, Config),
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    dets:close(b_tree_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

keys_b_tree_order_4_ets_test(Config) ->
    BTree = ?config(b_tree_4_ets, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

keys_b_tree_order_4_mnesia_test(Config) ->
    BTree = ?config(b_tree_4_mnesia, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 4
%%--------------------------------------------------------------------

keys_b_tree_order_4_test(Config) ->
    BTree = ?config(b_tree_4, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 5
%%--------------------------------------------------------------------

keys_b_tree_order_5_test(Config) ->
    BTree = ?config(b_tree_5, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 6
%%--------------------------------------------------------------------

keys_b_tree_order_6_test(Config) ->
    BTree = ?config(b_tree_6, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 512
%%--------------------------------------------------------------------

keys_b_tree_order_512_test(Config) ->
    BTree = ?config(b_tree_512, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 64
%%--------------------------------------------------------------------

keys_b_tree_order_64_test(Config) ->
    BTree = ?config(b_tree_64, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys b_tree order 8
%%--------------------------------------------------------------------

keys_b_tree_order_8_test(Config) ->
    BTree = ?config(b_tree_8, Config),
    _Keys = b_trees:keys(BTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys_random gb_tree
%%--------------------------------------------------------------------

keys_gb_tree_test(Config) ->
    GBTree = ?config(gb_tree, Config),
    _Keys = gb_trees:keys(GBTree),
    ?assertEqual(?config(keys_from, Config), _Keys),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 1024
%%--------------------------------------------------------------------

largest_b_tree_order_1024_test(Config) ->
    _BTree = ?config(b_tree_1024, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 128
%%--------------------------------------------------------------------

largest_b_tree_order_128_test(Config) ->
    _BTree = ?config(b_tree_128, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 16
%%--------------------------------------------------------------------

largest_b_tree_order_16_test(Config) ->
    _BTree = ?config(b_tree_16, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 256
%%--------------------------------------------------------------------

largest_b_tree_order_256_test(Config) ->
    _BTree = ?config(b_tree_256, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 32
%%--------------------------------------------------------------------

largest_b_tree_order_32_test(Config) ->
    _BTree = ?config(b_tree_32, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

largest_b_tree_order_4_dets_test(Config) ->
    _BTree = ?config(b_tree_4_dets, Config),
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    dets:close(b_tree_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

largest_b_tree_order_4_ets_test(Config) ->
    _BTree = ?config(b_tree_4_ets, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

largest_b_tree_order_4_mnesia_test(Config) ->
    _BTree = ?config(b_tree_4_mnesia, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 4
%%--------------------------------------------------------------------

largest_b_tree_order_4_test(Config) ->
    _BTree = ?config(b_tree_4, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 5
%%--------------------------------------------------------------------

largest_b_tree_order_5_test(Config) ->
    _BTree = ?config(b_tree_5, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 6
%%--------------------------------------------------------------------

largest_b_tree_order_6_test(Config) ->
    _BTree = ?config(b_tree_6, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 512
%%--------------------------------------------------------------------

largest_b_tree_order_512_test(Config) ->
    _BTree = ?config(b_tree_512, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 64
%%--------------------------------------------------------------------

largest_b_tree_order_64_test(Config) ->
    _BTree = ?config(b_tree_64, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest b_tree order 8
%%--------------------------------------------------------------------

largest_b_tree_order_8_test(Config) ->
    _BTree = ?config(b_tree_8, Config),
    ?assertEqual(?LARGEST_KEY_VALUE, b_trees:largest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest gb_tree
%%--------------------------------------------------------------------

largest_gb_tree_test(_Config) ->
    ?assertEqual(?LARGEST_KEY_VALUE, gb_trees:largest(?config(gb_tree, _Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 1024
%%--------------------------------------------------------------------

lookup_b_tree_order_1024_test(Config) ->
    BTree = ?config(b_tree_1024, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

lookup_b_tree([], _) ->
    none;
lookup_b_tree([Key | Tail], BTree) ->
    b_trees:lookup(Key, BTree),
    lookup_b_tree(Tail, BTree).

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 128
%%--------------------------------------------------------------------

lookup_b_tree_order_128_test(Config) ->
    BTree = ?config(b_tree_128, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 16
%%--------------------------------------------------------------------

lookup_b_tree_order_16_test(Config) ->
    BTree = ?config(b_tree_16, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 256
%%--------------------------------------------------------------------

lookup_b_tree_order_256_test(Config) ->
    BTree = ?config(b_tree_256, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 32
%%--------------------------------------------------------------------

lookup_b_tree_order_32_test(Config) ->
    BTree = ?config(b_tree_32, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

lookup_b_tree_order_4_dets_test(Config) ->
    BTree = ?config(b_tree_4_dets, Config),
    Keys = ?config(keys_random, Config),
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    lookup_b_tree(Keys, BTree),
    dets:close(b_tree_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

lookup_b_tree_order_4_ets_test(Config) ->
    BTree = ?config(b_tree_4_ets, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

lookup_b_tree_order_4_mnesia_test(Config) ->
    BTree = ?config(b_tree_4_mnesia, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 4
%%--------------------------------------------------------------------

lookup_b_tree_order_4_test(Config) ->
    BTree = ?config(b_tree_4, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 5
%%--------------------------------------------------------------------

lookup_b_tree_order_5_test(Config) ->
    BTree = ?config(b_tree_5, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 6
%%--------------------------------------------------------------------

lookup_b_tree_order_6_test(Config) ->
    BTree = ?config(b_tree_6, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 512
%%--------------------------------------------------------------------

lookup_b_tree_order_512_test(Config) ->
    BTree = ?config(b_tree_512, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 64
%%--------------------------------------------------------------------

lookup_b_tree_order_64_test(Config) ->
    BTree = ?config(b_tree_64, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup b_tree order 8
%%--------------------------------------------------------------------

lookup_b_tree_order_8_test(Config) ->
    BTree = ?config(b_tree_8, Config),
    Keys = ?config(keys_random, Config),
    lookup_b_tree(Keys, BTree),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup gb_tree
%%--------------------------------------------------------------------

lookup_gb_tree_test(Config) ->
    GBTree = ?config(gb_tree, Config),
    Keys = ?config(keys_random, Config),
    lookup_gb_tree(Keys, GBTree),
    ok.

lookup_gb_tree([], _) ->
    none;
lookup_gb_tree([Key | Tail], GBTree) ->
    gb_trees:lookup(Key, GBTree),
    lookup_gb_tree(Tail, GBTree).

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 1024 - persistence by dets
%%--------------------------------------------------------------------

map_b_tree_order_1024_dets_test(Config) ->
    BTree = ?config(b_tree_1024_dets_map, Config),
    BTreeNew = ?config(b_tree_1024_dets_new, Config),
    {ok, _} = dets:open_file(b_tree_1024_dets_map, [{file, ?DIRECTORY_DETS ++ "b_tree_1024_dets_map"}]),
    b_trees_generator:check_equal(BTreeNew, b_trees:map(fun map_value_to_new/2, BTree)),
    dets:close(b_tree_1024_dets_map).

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 1024 - persistence by ets
%%--------------------------------------------------------------------

map_b_tree_order_1024_ets_test(Config) ->
    BTree = ?config(b_tree_1024_ets_map, Config),
    BTreeNew = ?config(b_tree_1024_ets_new, Config),
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTreeMap = b_trees:map(fun map_value_to_new/2, BTree),
    b_trees_generator:check_equal(BTreeNew, BTreeMap),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTreeMap),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 1024 - persistence by mnesia
%%--------------------------------------------------------------------

map_b_tree_order_1024_mnesia_test(Config) ->
    BTree = ?config(b_tree_1024_mnesia_map, Config),
    BTreeNew = ?config(b_tree_1024_mnesia_new, Config),
    b_trees_generator:check_equal(BTreeNew, b_trees:map(fun map_value_to_new/2, BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 1024
%%--------------------------------------------------------------------

map_b_tree_order_1024_test(Config) ->
    _BTree = ?config(b_tree_1024, Config),
    _BTreeNew = ?config(b_tree_1024_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

map_value_to_new(_, Value) ->
    lists:append([Value , "_new"]).

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 128
%%--------------------------------------------------------------------

map_b_tree_order_128_test(Config) ->
    _BTree = ?config(b_tree_128, Config),
    _BTreeNew = ?config(b_tree_128_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 16
%%--------------------------------------------------------------------

map_b_tree_order_16_test(Config) ->
    _BTree = ?config(b_tree_16, Config),
    _BTreeNew = ?config(b_tree_16_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 256
%%--------------------------------------------------------------------

map_b_tree_order_256_test(Config) ->
    _BTree = ?config(b_tree_256, Config),
    _BTreeNew = ?config(b_tree_256_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 32
%%--------------------------------------------------------------------

map_b_tree_order_32_test(Config) ->
    _BTree = ?config(b_tree_32, Config),
    _BTreeNew = ?config(b_tree_32_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

map_b_tree_order_4_dets_test(Config) ->
    BTree = ?config(b_tree_4_dets_map, Config),
    BTreeNew = ?config(b_tree_4_dets_new, Config),
    {ok, _} = dets:open_file(b_tree_4_dets_map, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets_map"}]),
    b_trees_generator:check_equal(BTreeNew, b_trees:map(fun map_value_to_new/2, BTree)),
    dets:close(b_tree_4_dets_map).

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

map_b_tree_order_4_ets_test(Config) ->
    BTree = ?config(b_tree_4_ets_map, Config),
    BTreeNew = ?config(b_tree_4_ets_new, Config),
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTreeMap = b_trees:map(fun map_value_to_new/2, BTree),
    b_trees_generator:check_equal(BTreeNew, BTreeMap),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTreeMap),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

map_b_tree_order_4_mnesia_test(Config) ->
    BTree = ?config(b_tree_4_mnesia_map, Config),
    BTreeNew = ?config(b_tree_4_mnesia_new, Config),
    b_trees_generator:check_equal(BTreeNew, b_trees:map(fun map_value_to_new/2, BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 4
%%--------------------------------------------------------------------

map_b_tree_order_4_test(Config) ->
    _BTree = ?config(b_tree_4, Config),
    _BTreeNew = ?config(b_tree_4_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 5
%%--------------------------------------------------------------------

map_b_tree_order_5_test(Config) ->
    _BTree = ?config(b_tree_5, Config),
    _BTreeNew = ?config(b_tree_5_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 6
%%--------------------------------------------------------------------

map_b_tree_order_6_test(Config) ->
    _BTree = ?config(b_tree_6, Config),
    _BTreeNew = ?config(b_tree_6_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 512
%%--------------------------------------------------------------------

map_b_tree_order_512_test(Config) ->
    _BTree = ?config(b_tree_512, Config),
    _BTreeNew = ?config(b_tree_512_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 64
%%--------------------------------------------------------------------

map_b_tree_order_64_test(Config) ->
    _BTree = ?config(b_tree_64, Config),
    _BTreeNew = ?config(b_tree_64_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map b_tree order 8
%%--------------------------------------------------------------------

map_b_tree_order_8_test(Config) ->
    _BTree = ?config(b_tree_8, Config),
    _BTreeNew = ?config(b_tree_8_new, Config),
    ?assertEqual(_BTreeNew, b_trees:map(fun map_value_to_new/2, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map gb_tree
%%--------------------------------------------------------------------

map_gb_tree_test(_Config) ->
    ?assertEqual(?config(gb_tree_new, _Config), gb_trees:map(fun map_value_to_new/2, ?config(gb_tree, _Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 1024
%%--------------------------------------------------------------------

smallest_b_tree_order_1024_test(Config) ->
    _BTree = ?config(b_tree_1024, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 128
%%--------------------------------------------------------------------

smallest_b_tree_order_128_test(Config) ->
    _BTree = ?config(b_tree_128, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 16
%%--------------------------------------------------------------------

smallest_b_tree_order_16_test(Config) ->
    _BTree = ?config(b_tree_16, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 256
%%--------------------------------------------------------------------

smallest_b_tree_order_256_test(Config) ->
    _BTree = ?config(b_tree_256, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 32
%%--------------------------------------------------------------------

smallest_b_tree_order_32_test(Config) ->
    _BTree = ?config(b_tree_32, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

smallest_b_tree_order_4_dets_test(Config) ->
    _BTree = ?config(b_tree_4_dets, Config),
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    dets:close(b_tree_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

smallest_b_tree_order_4_ets_test(Config) ->
    _BTree = ?config(b_tree_4_ets, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

smallest_b_tree_order_4_mnesia_test(Config) ->
    _BTree = ?config(b_tree_4_mnesia, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 4
%%--------------------------------------------------------------------

smallest_b_tree_order_4_test(Config) ->
    _BTree = ?config(b_tree_4, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 5
%%--------------------------------------------------------------------

smallest_b_tree_order_5_test(Config) ->
    _BTree = ?config(b_tree_5, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 6
%%--------------------------------------------------------------------

smallest_b_tree_order_6_test(Config) ->
    _BTree = ?config(b_tree_6, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 512
%%--------------------------------------------------------------------

smallest_b_tree_order_512_test(Config) ->
    _BTree = ?config(b_tree_512, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 64
%%--------------------------------------------------------------------

smallest_b_tree_order_64_test(Config) ->
    _BTree = ?config(b_tree_64, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest b_tree order 8
%%--------------------------------------------------------------------

smallest_b_tree_order_8_test(Config) ->
    _BTree = ?config(b_tree_8, Config),
    ?assertEqual(?SMALLEST_KEY_VALUE, b_trees:smallest(_BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest gb_tree
%%--------------------------------------------------------------------

smallest_gb_tree_test(_Config) ->
    ?assertEqual(?SMALLEST_KEY_VALUE, gb_trees:smallest(?config(gb_tree, _Config))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any b_tree order 1024
%%--------------------------------------------------------------------

take_any_b_tree_order_1024_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(1024)}, b_trees_generator:take_any_b_tree_from(1024, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any b_tree order 128
%%--------------------------------------------------------------------

take_any_b_tree_order_128_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(128)}, b_trees_generator:take_any_b_tree_from(128, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any b_tree order 16
%%--------------------------------------------------------------------

take_any_b_tree_order_16_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(16)}, b_trees_generator:take_any_b_tree_from(16, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any b_tree order 256
%%--------------------------------------------------------------------

take_any_b_tree_order_256_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(256)}, b_trees_generator:take_any_b_tree_from(256, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any b_tree order 32
%%--------------------------------------------------------------------

take_any_b_tree_order_32_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(32)}, b_trees_generator:take_any_b_tree_from(32, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any b_tree order 4
%%--------------------------------------------------------------------

take_any_b_tree_order_4_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(4)}, b_trees_generator:take_any_b_tree_from(4, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any b_tree order 512
%%--------------------------------------------------------------------

take_any_b_tree_order_512_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(512)}, b_trees_generator:take_any_b_tree_from(512, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any b_tree order 5
%%--------------------------------------------------------------------

take_any_b_tree_order_5_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(5)}, b_trees_generator:take_any_b_tree_from(5, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any b_tree order 64
%%--------------------------------------------------------------------

take_any_b_tree_order_64_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(64)}, b_trees_generator:take_any_b_tree_from(64, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any b_tree order 6
%%--------------------------------------------------------------------

take_any_b_tree_order_6_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(6)}, b_trees_generator:take_any_b_tree_from(6, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any b_tree order 8
%%--------------------------------------------------------------------

take_any_b_tree_order_8_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(8)}, b_trees_generator:take_any_b_tree_from(8, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_any gb_tree
%%--------------------------------------------------------------------

take_any_gb_tree_test(_Config) ->
    case ?OTP_RELEASE > "19" of
        true ->
            ?assertEqual({?LARGEST_VALUE, gb_trees:empty()}, b_trees_generator:take_any_gb_tree_from(?NUMBER_ACTIONS, ?WIDTH)),
            ok;
        _ ->
            not_available
    end.

%%--------------------------------------------------------------------
%% TEST CASES: take b_tree order 1024
%%--------------------------------------------------------------------

take_b_tree_order_1024_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(1024)}, b_trees_generator:take_b_tree_from(1024, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take b_tree order 128
%%--------------------------------------------------------------------

take_b_tree_order_128_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(128)}, b_trees_generator:take_b_tree_from(128, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take b_tree order 16
%%--------------------------------------------------------------------

take_b_tree_order_16_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(16)}, b_trees_generator:take_b_tree_from(16, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take b_tree order 256
%%--------------------------------------------------------------------

take_b_tree_order_256_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(256)}, b_trees_generator:take_b_tree_from(256, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take b_tree order 32
%%--------------------------------------------------------------------

take_b_tree_order_32_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(32)}, b_trees_generator:take_b_tree_from(32, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take b_tree order 4
%%--------------------------------------------------------------------

take_b_tree_order_4_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(4)}, b_trees_generator:take_b_tree_from(4, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take b_tree order 512
%%--------------------------------------------------------------------

take_b_tree_order_512_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(512)}, b_trees_generator:take_b_tree_from(512, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take b_tree order 5
%%--------------------------------------------------------------------

take_b_tree_order_5_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(5)}, b_trees_generator:take_b_tree_from(5, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take b_tree order 64
%%--------------------------------------------------------------------

take_b_tree_order_64_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(64)}, b_trees_generator:take_b_tree_from(64, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take b_tree order 6
%%--------------------------------------------------------------------

take_b_tree_order_6_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(6)}, b_trees_generator:take_b_tree_from(6, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take b_tree order 8
%%--------------------------------------------------------------------

take_b_tree_order_8_test(_Config) ->
    ?assertEqual({?LARGEST_VALUE, b_trees:empty(8)}, b_trees_generator:take_b_tree_from(8, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take gb_tree
%%--------------------------------------------------------------------

take_gb_tree_test(_Config) ->
    case ?OTP_RELEASE > "19" of
        true ->
            ?assertEqual({?LARGEST_VALUE, gb_trees:empty()}, b_trees_generator:take_gb_tree_from(?NUMBER_ACTIONS, ?WIDTH)),
            ok;
        _ ->
            not_available
    end.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 1024 - persistence by dets
%%--------------------------------------------------------------------

take_largest_b_tree_order_1024_dets_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(1024), b_trees_generator:take_largest_b_tree_dets(1024, ?NUMBER_ACTIONS, ?WIDTH, take_largest_order_1024)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 1024 - persistence by ets
%%--------------------------------------------------------------------

take_largest_b_tree_order_1024_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:take_largest_b_tree_ets(1024, ?NUMBER_ACTIONS, ?WIDTH, take_largest_order_1024, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(1024), BTree),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 1024 - persistence by mnesia
%%--------------------------------------------------------------------

take_largest_b_tree_order_1024_mnesia_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(1024), b_trees_generator:take_largest_b_tree_mnesia(1024, ?NUMBER_ACTIONS, ?WIDTH, take_largest_order_1024)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 1024
%%--------------------------------------------------------------------

take_largest_b_tree_order_1024_test(_Config) ->
    ?assertEqual(b_trees:empty(1024), b_trees_generator:take_largest_b_tree(1024, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 128
%%--------------------------------------------------------------------

take_largest_b_tree_order_128_test(_Config) ->
    ?assertEqual(b_trees:empty(128), b_trees_generator:take_largest_b_tree(128, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 16
%%--------------------------------------------------------------------

take_largest_b_tree_order_16_test(_Config) ->
    ?assertEqual(b_trees:empty(16), b_trees_generator:take_largest_b_tree(16, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 256
%%--------------------------------------------------------------------

take_largest_b_tree_order_256_test(_Config) ->
    ?assertEqual(b_trees:empty(256), b_trees_generator:take_largest_b_tree(256, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 32
%%--------------------------------------------------------------------

take_largest_b_tree_order_32_test(_Config) ->
    ?assertEqual(b_trees:empty(32), b_trees_generator:take_largest_b_tree(32, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

take_largest_b_tree_order_4_dets_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(4), b_trees_generator:take_largest_b_tree_dets(4, ?NUMBER_ACTIONS, ?WIDTH, take_largest_order_4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

take_largest_b_tree_order_4_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:take_largest_b_tree_ets(4, ?NUMBER_ACTIONS, ?WIDTH, take_largest_order_4, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(4), BTree),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

take_largest_b_tree_order_4_mnesia_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(4), b_trees_generator:take_largest_b_tree_mnesia(4, ?NUMBER_ACTIONS, ?WIDTH, take_largest_order_4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 4
%%--------------------------------------------------------------------

take_largest_b_tree_order_4_test(_Config) ->
    ?assertEqual(b_trees:empty(4), b_trees_generator:take_largest_b_tree(4, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 5
%%--------------------------------------------------------------------

take_largest_b_tree_order_5_test(_Config) ->
    ?assertEqual(b_trees:empty(5), b_trees_generator:take_largest_b_tree(5, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 6
%%--------------------------------------------------------------------

take_largest_b_tree_order_6_test(_Config) ->
    ?assertEqual(b_trees:empty(6), b_trees_generator:take_largest_b_tree(6, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 512
%%--------------------------------------------------------------------

take_largest_b_tree_order_512_test(_Config) ->
    ?assertEqual(b_trees:empty(512), b_trees_generator:take_largest_b_tree(512, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 64
%%--------------------------------------------------------------------

take_largest_b_tree_order_64_test(_Config) ->
    ?assertEqual(b_trees:empty(64), b_trees_generator:take_largest_b_tree(64, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest b_tree order 8
%%--------------------------------------------------------------------

take_largest_b_tree_order_8_test(_Config) ->
    ?assertEqual(b_trees:empty(8), b_trees_generator:take_largest_b_tree(8, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest gb_tree
%%--------------------------------------------------------------------

take_largest_gb_tree_test(_Config) ->
    ?assertEqual(gb_trees:empty(), b_trees_generator:take_largest_gb_tree(?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 1024 - persistence by dets
%%--------------------------------------------------------------------

take_smallest_b_tree_order_1024_dets_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(1024), b_trees_generator:take_smallest_b_tree_dets(1024, ?NUMBER_ACTIONS, ?WIDTH, take_smallest_order_1024)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 1024 - persistence by ets
%%--------------------------------------------------------------------

take_smallest_b_tree_order_1024_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:take_smallest_b_tree_ets(1024, ?NUMBER_ACTIONS, ?WIDTH, take_smallest_order_1024, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(1024), BTree),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 1024 - persistence by mnesia
%%--------------------------------------------------------------------

take_smallest_b_tree_order_1024_mnesia_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(1024), b_trees_generator:take_smallest_b_tree_mnesia(1024, ?NUMBER_ACTIONS, ?WIDTH, take_smallest_order_1024)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 1024
%%--------------------------------------------------------------------

take_smallest_b_tree_order_1024_test(_Config) ->
    ?assertEqual(b_trees:empty(1024), b_trees_generator:take_smallest_b_tree(1024, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 128
%%--------------------------------------------------------------------

take_smallest_b_tree_order_128_test(_Config) ->
    ?assertEqual(b_trees:empty(128), b_trees_generator:take_smallest_b_tree(128, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 16
%%--------------------------------------------------------------------

take_smallest_b_tree_order_16_test(_Config) ->
    ?assertEqual(b_trees:empty(16), b_trees_generator:take_smallest_b_tree(16, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 256
%%--------------------------------------------------------------------

take_smallest_b_tree_order_256_test(_Config) ->
    ?assertEqual(b_trees:empty(256), b_trees_generator:take_smallest_b_tree(256, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 32
%%--------------------------------------------------------------------

take_smallest_b_tree_order_32_test(_Config) ->
    ?assertEqual(b_trees:empty(32), b_trees_generator:take_smallest_b_tree(32, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

take_smallest_b_tree_order_4_dets_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(4), b_trees_generator:take_smallest_b_tree_dets(4, ?NUMBER_ACTIONS, ?WIDTH, take_smallest_order_4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

take_smallest_b_tree_order_4_ets_test(_Config) ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTree = b_trees_generator:take_smallest_b_tree_ets(4, ?NUMBER_ACTIONS, ?WIDTH, take_smallest_order_4, spawn(fun b_trees_generator:ets_owner/0)),
    b_trees_generator:check_equal(b_trees:empty(4), BTree),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTree),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

take_smallest_b_tree_order_4_mnesia_test(_Config) ->
    b_trees_generator:check_equal(b_trees:empty(4), b_trees_generator:take_smallest_b_tree_mnesia(4, ?NUMBER_ACTIONS, ?WIDTH, take_smallest_order_4)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 4
%%--------------------------------------------------------------------

take_smallest_b_tree_order_4_test(_Config) ->
    ?assertEqual(b_trees:empty(4), b_trees_generator:take_smallest_b_tree(4, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 5
%%--------------------------------------------------------------------

take_smallest_b_tree_order_5_test(_Config) ->
    ?assertEqual(b_trees:empty(5), b_trees_generator:take_smallest_b_tree(5, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 6
%%--------------------------------------------------------------------

take_smallest_b_tree_order_6_test(_Config) ->
    ?assertEqual(b_trees:empty(6), b_trees_generator:take_smallest_b_tree(6, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 512
%%--------------------------------------------------------------------

take_smallest_b_tree_order_512_test(_Config) ->
    ?assertEqual(b_trees:empty(512), b_trees_generator:take_smallest_b_tree(512, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 64
%%--------------------------------------------------------------------

take_smallest_b_tree_order_64_test(_Config) ->
    ?assertEqual(b_trees:empty(64), b_trees_generator:take_smallest_b_tree(64, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest b_tree order 8
%%--------------------------------------------------------------------

take_smallest_b_tree_order_8_test(_Config) ->
    ?assertEqual(b_trees:empty(8), b_trees_generator:take_smallest_b_tree(8, ?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest gb_tree
%%--------------------------------------------------------------------

take_smallest_gb_tree_test(_Config) ->
    ?assertEqual(gb_trees:empty(), b_trees_generator:take_smallest_gb_tree(?NUMBER_ACTIONS, ?WIDTH)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 1024
%%--------------------------------------------------------------------

to_list_b_tree_order_1024_test(Config) ->
    BTree = ?config(b_tree_1024, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 128
%%--------------------------------------------------------------------

to_list_b_tree_order_128_test(Config) ->
    BTree = ?config(b_tree_128, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 16
%%--------------------------------------------------------------------

to_list_b_tree_order_16_test(Config) ->
    BTree = ?config(b_tree_16, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 256
%%--------------------------------------------------------------------

to_list_b_tree_order_256_test(Config) ->
    BTree = ?config(b_tree_256, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 32
%%--------------------------------------------------------------------

to_list_b_tree_order_32_test(Config) ->
    BTree = ?config(b_tree_32, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

to_list_b_tree_order_4_dets_test(Config) ->
    BTree = ?config(b_tree_4_dets, Config),
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    dets:close(b_tree_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

to_list_b_tree_order_4_ets_test(Config) ->
    BTree = ?config(b_tree_4_ets, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

to_list_b_tree_order_4_mnesia_test(Config) ->
    BTree = ?config(b_tree_4_mnesia, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 4
%%--------------------------------------------------------------------

to_list_b_tree_order_4_test(Config) ->
    BTree = ?config(b_tree_4, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 5
%%--------------------------------------------------------------------

to_list_b_tree_order_5_test(Config) ->
    BTree = ?config(b_tree_5, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 6
%%--------------------------------------------------------------------

to_list_b_tree_order_6_test(Config) ->
    BTree = ?config(b_tree_6, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 512
%%--------------------------------------------------------------------

to_list_b_tree_order_512_test(Config) ->
    BTree = ?config(b_tree_512, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 64
%%--------------------------------------------------------------------

to_list_b_tree_order_64_test(Config) ->
    BTree = ?config(b_tree_64, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list b_tree order 8
%%--------------------------------------------------------------------

to_list_b_tree_order_8_test(Config) ->
    BTree = ?config(b_tree_8, Config),
    _KeyValues = b_trees:to_list(BTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list gb_tree
%%--------------------------------------------------------------------

to_list_gb_tree_test(Config) ->
    GBTree = ?config(gb_tree, Config),
    _KeyValues = gb_trees:to_list(GBTree),
    ?assertEqual(?config(key_values_from, Config), _KeyValues),
    ?assertEqual(?NUMBER_ACTIONS, length(_KeyValues)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 1024 - persistence by dets
%%--------------------------------------------------------------------

update_b_tree_order_1024_dets_test(Config) ->
    BTree = ?config(b_tree_1024_dets_update, Config),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    {ok, _} = dets:open_file(b_tree_1024_dets_update, [{file, ?DIRECTORY_DETS ++ "b_tree_1024_dets_update"}]),
    b_trees_generator:check_equal(?config(b_tree_1024_dets_new, Config), update_b_tree(KeyValuesUpdate, BTree)),
    dets:close(b_tree_1024_dets_update).

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 1024 - persistence by ets
%%--------------------------------------------------------------------

update_b_tree_order_1024_ets_test(Config) ->
    BTree = ?config(b_tree_1024_ets_update, Config),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTreeUpdate = update_b_tree(KeyValuesUpdate, BTree),
    b_trees_generator:check_equal(?config(b_tree_1024_ets_new, Config), BTreeUpdate),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTreeUpdate),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 1024 - persistence by mnesia
%%--------------------------------------------------------------------

update_b_tree_order_1024_mnesia_test(Config) ->
    BTree = ?config(b_tree_1024_mnesia_update, Config),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    b_trees_generator:check_equal(?config(b_tree_1024_mnesia_new, Config), update_b_tree(KeyValuesUpdate, BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 1024
%%--------------------------------------------------------------------

update_b_tree_order_1024_test(Config) ->
    _BTree = ?config(b_tree_1024, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(b_tree_1024_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

update_b_tree([], BTree) ->
    BTree;
update_b_tree([{Key, Value} | Tail], BTree) ->
    update_b_tree(Tail, b_trees:update(Key, Value, BTree)).

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 128
%%--------------------------------------------------------------------

update_b_tree_order_128_test(Config) ->
    _BTree = ?config(b_tree_128, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(b_tree_128_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 16
%%--------------------------------------------------------------------

update_b_tree_order_16_test(Config) ->
    _BTree = ?config(b_tree_16, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(b_tree_16_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 256
%%--------------------------------------------------------------------

update_b_tree_order_256_test(Config) ->
    _BTree = ?config(b_tree_256, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(b_tree_256_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 32
%%--------------------------------------------------------------------

update_b_tree_order_32_test(Config) ->
    _BTree = ?config(b_tree_32, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(b_tree_32_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

update_b_tree_order_4_dets_test(Config) ->
    BTree = ?config(b_tree_4_dets_update, Config),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    {ok, _} = dets:open_file(b_tree_4_dets_update, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets_update"}]),
    b_trees_generator:check_equal(?config(b_tree_4_dets_new, Config), update_b_tree(KeyValuesUpdate, BTree)),
    dets:close(b_tree_4_dets_update).

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

update_b_tree_order_4_ets_test(Config) ->
    BTree = ?config(b_tree_4_ets_update, Config),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    {_, _, _, _, {StateTarget, _, _, _}, _} = BTreeUpdate = update_b_tree(KeyValuesUpdate, BTree),
    b_trees_generator:check_equal(?config(b_tree_4_ets_new, Config), BTreeUpdate),
    {SizeNodesTotal, SizeNodesLeafes} = b_trees:size_nodes(BTreeUpdate),
    ?assertEqual(SizeNodesTotal - SizeNodesLeafes, length(ets:tab2list(StateTarget))),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

update_b_tree_order_4_mnesia_test(Config) ->
    BTree = ?config(b_tree_4_mnesia_update, Config),
    KeyValuesUpdate = ?config(key_values_from_update, Config),
    b_trees_generator:check_equal(?config(b_tree_4_mnesia_new, Config), update_b_tree(KeyValuesUpdate, BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 4 - random
%%--------------------------------------------------------------------

update_b_tree_order_4_random_test(Config) ->
    _BTree = ?config(b_tree_4, Config),
    _KeyValuesUpdate = ?config(key_values_random_update, Config),
    ?assertEqual(?config(b_tree_4_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 4
%%--------------------------------------------------------------------

update_b_tree_order_4_test(Config) ->
    _BTree = ?config(b_tree_4, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(b_tree_4_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 5 - random
%%--------------------------------------------------------------------

update_b_tree_order_5_random_test(Config) ->
    _BTree = ?config(b_tree_5, Config),
    _KeyValuesUpdate = ?config(key_values_random_update, Config),
    ?assertEqual(?config(b_tree_5_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 5
%%--------------------------------------------------------------------

update_b_tree_order_5_test(Config) ->
    _BTree = ?config(b_tree_5, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(b_tree_5_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 6
%%--------------------------------------------------------------------

update_b_tree_order_6_test(Config) ->
    _BTree = ?config(b_tree_6, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(b_tree_6_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 512
%%--------------------------------------------------------------------

update_b_tree_order_512_test(Config) ->
    _BTree = ?config(b_tree_512, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(b_tree_512_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 64
%%--------------------------------------------------------------------

update_b_tree_order_64_test(Config) ->
    _BTree = ?config(b_tree_64, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(b_tree_64_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 8 - random
%%--------------------------------------------------------------------

update_b_tree_order_8_random_test(Config) ->
    _BTree = ?config(b_tree_8, Config),
    _KeyValuesUpdate = ?config(key_values_random_update, Config),
    ?assertEqual(?config(b_tree_8_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update b_tree order 8
%%--------------------------------------------------------------------

update_b_tree_order_8_test(Config) ->
    _BTree = ?config(b_tree_8, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(b_tree_8_new, Config), update_b_tree(_KeyValuesUpdate, _BTree)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update gb_tree
%%--------------------------------------------------------------------

update_gb_tree_test(Config) ->
    _GBTree = ?config(gb_tree, Config),
    _KeyValuesUpdate = ?config(key_values_from_update, Config),
    ?assertEqual(?config(gb_tree_new, Config), update_gb_tree(_KeyValuesUpdate, _GBTree)),
    ok.

update_gb_tree([], GBTree) ->
    GBTree;
update_gb_tree([{Key, Value} | Tail], GBTree) ->
    update_gb_tree(Tail, gb_trees:update(Key, Value, GBTree)).

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 1024
%%--------------------------------------------------------------------

values_b_tree_order_1024_test(Config) ->
    BTree = ?config(b_tree_1024, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 128
%%--------------------------------------------------------------------

values_b_tree_order_128_test(Config) ->
    BTree = ?config(b_tree_128, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 16
%%--------------------------------------------------------------------

values_b_tree_order_16_test(Config) ->
    BTree = ?config(b_tree_16, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 256
%%--------------------------------------------------------------------

values_b_tree_order_256_test(Config) ->
    BTree = ?config(b_tree_256, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 32
%%--------------------------------------------------------------------

values_b_tree_order_32_test(Config) ->
    BTree = ?config(b_tree_32, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 4 - persistence by dets
%%--------------------------------------------------------------------

values_b_tree_order_4_dets_test(Config) ->
    BTree = ?config(b_tree_4_dets, Config),
    {ok, _} = dets:open_file(b_tree_4_dets, [{file, ?DIRECTORY_DETS ++ "b_tree_4_dets"}]),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    dets:close(b_tree_4_dets).

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 4 - persistence by ets
%%--------------------------------------------------------------------

values_b_tree_order_4_ets_test(Config) ->
    BTree = ?config(b_tree_4_ets, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 4 - persistence by mnesia
%%--------------------------------------------------------------------

values_b_tree_order_4_mnesia_test(Config) ->
    BTree = ?config(b_tree_4_mnesia, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 4
%%--------------------------------------------------------------------

values_b_tree_order_4_test(Config) ->
    BTree = ?config(b_tree_4, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 5
%%--------------------------------------------------------------------

values_b_tree_order_5_test(Config) ->
    BTree = ?config(b_tree_5, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 6
%%--------------------------------------------------------------------

values_b_tree_order_6_test(Config) ->
    BTree = ?config(b_tree_6, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 512
%%--------------------------------------------------------------------

values_b_tree_order_512_test(Config) ->
    BTree = ?config(b_tree_512, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 64
%%--------------------------------------------------------------------

values_b_tree_order_64_test(Config) ->
    BTree = ?config(b_tree_64, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values b_tree order 8
%%--------------------------------------------------------------------

values_b_tree_order_8_test(Config) ->
    BTree = ?config(b_tree_8, Config),
    _Keys = b_trees:values(BTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values gb_tree
%%--------------------------------------------------------------------

values_gb_tree_test(Config) ->
    GBTree = ?config(gb_tree, Config),
    _Keys = gb_trees:values(GBTree),
    ?assertEqual(?NUMBER_ACTIONS, length(_Keys)),
    ok.
