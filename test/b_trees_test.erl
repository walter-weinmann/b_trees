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
%% TEST CASES: delete_any - persistence by ets
%%--------------------------------------------------------------------

delete_any_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_04_32_32 = test_generator:generate_b_tree_from_number_ets(4, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    B_TREE_04_32_32 = b_trees:delete_any("k_00", B_TREE_04_32_32),
    B_TREE_04_32_01 = b_trees:delete_any("k_01", B_TREE_04_32_32),
    B_TREE_04_32_02 = b_trees:delete_any("k_02", B_TREE_04_32_01),
    B_TREE_04_32_03 = b_trees:delete_any("k_03", B_TREE_04_32_02),
    B_TREE_04_32_04 = b_trees:delete_any("k_04", B_TREE_04_32_03),
    B_TREE_04_32_05 = b_trees:delete_any("k_05", B_TREE_04_32_04),
    B_TREE_04_32_06 = b_trees:delete_any("k_06", B_TREE_04_32_05),
    B_TREE_04_32_07 = b_trees:delete_any("k_07", B_TREE_04_32_06),
    B_TREE_04_32_08 = b_trees:delete_any("k_08", B_TREE_04_32_07),
    B_TREE_04_32_09 = b_trees:delete_any("k_09", B_TREE_04_32_08),
    B_TREE_04_32_10 = b_trees:delete_any("k_10", B_TREE_04_32_09),
    B_TREE_04_32_11 = b_trees:delete_any("k_11", B_TREE_04_32_10),
    B_TREE_04_32_12 = b_trees:delete_any("k_12", B_TREE_04_32_11),
    B_TREE_04_32_13 = b_trees:delete_any("k_13", B_TREE_04_32_12),
    B_TREE_04_32_14 = b_trees:delete_any("k_14", B_TREE_04_32_13),
    B_TREE_04_32_15 = b_trees:delete_any("k_15", B_TREE_04_32_14),
    B_TREE_04_32_16 = b_trees:delete_any("k_16", B_TREE_04_32_15),
    B_TREE_04_32_17 = b_trees:delete_any("k_17", B_TREE_04_32_16),
    B_TREE_04_32_18 = b_trees:delete_any("k_18", B_TREE_04_32_17),
    B_TREE_04_32_19 = b_trees:delete_any("k_19", B_TREE_04_32_18),
    B_TREE_04_32_20 = b_trees:delete_any("k_20", B_TREE_04_32_19),
    B_TREE_04_32_21 = b_trees:delete_any("k_21", B_TREE_04_32_20),
    B_TREE_04_32_22 = b_trees:delete_any("k_22", B_TREE_04_32_21),
    B_TREE_04_32_23 = b_trees:delete_any("k_23", B_TREE_04_32_22),
    B_TREE_04_32_24 = b_trees:delete_any("k_24", B_TREE_04_32_23),
    B_TREE_04_32_25 = b_trees:delete_any("k_25", B_TREE_04_32_24),
    B_TREE_04_32_26 = b_trees:delete_any("k_26", B_TREE_04_32_25),
    B_TREE_04_32_27 = b_trees:delete_any("k_27", B_TREE_04_32_26),
    B_TREE_04_32_28 = b_trees:delete_any("k_28", B_TREE_04_32_27),
    B_TREE_04_32_29 = b_trees:delete_any("k_29", B_TREE_04_32_28),
    B_TREE_04_32_30 = b_trees:delete_any("k_30", B_TREE_04_32_29),
    B_TREE_04_32_31 = b_trees:delete_any("k_31", B_TREE_04_32_30),
    B_TREE_04_32_31 = b_trees:delete_any("k_33", B_TREE_04_32_31),
    B_TREE_04_32_00 = b_trees:delete_any("k_32", B_TREE_04_32_31),

    ?assertEqual(0, b_trees:number_key_values(B_TREE_04_32_00)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete_any
%%--------------------------------------------------------------------

delete_any_test() ->
    test_generator:check_equal(?B_TREE_06_00, b_trees:delete_any("k_00", test_generator:prepare_template_asc(?B_TREE_06_00))),
    test_generator:check_equal(?B_TREE_06_00, b_trees:delete_any("k_01", test_generator:prepare_template_asc(?B_TREE_06_01))),
    test_generator:check_equal(?B_TREE_06_01, b_trees:delete_any("k_00", test_generator:prepare_template_asc(?B_TREE_06_01))),
    test_generator:check_equal(?B_TREE_06_03, b_trees:delete_any("k_04", test_generator:prepare_template_asc(?B_TREE_06_04))),
    test_generator:check_equal(?B_TREE_06_06, b_trees:delete_any("k_07", test_generator:prepare_template_asc(?B_TREE_06_07))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete - order 4
%%--------------------------------------------------------------------

delete_b_tree_order_4_test() ->
    test_generator:check_equal(?B_TREE_04_10_MINUS_02, b_trees:delete("k_02", test_generator:prepare_template_asc(?B_TREE_04_10))),

    test_generator:check_equal(?B_TREE_04_17_MINUS_02, b_trees:delete("k_02", test_generator:prepare_template_asc(?B_TREE_04_17))),
    test_generator:check_equal(?B_TREE_04_17_MINUS_02_04, b_trees:delete("k_04", test_generator:prepare_template_asc(?B_TREE_04_17_MINUS_02))),
    test_generator:check_equal(?B_TREE_04_17_MINUS_02_TILL_12, b_trees:delete("k_12", test_generator:prepare_template_asc(?B_TREE_04_17_MINUS_02_TILL_10))),

    test_generator:check_equal(?B_TREE_04_20_MINUS_02, b_trees:delete("k_02", test_generator:prepare_template_asc(?B_TREE_04_20))),
    test_generator:check_equal(?B_TREE_04_20_MINUS_04, b_trees:delete("k_04", test_generator:prepare_template_asc(?B_TREE_04_20))),
    test_generator:check_equal(?B_TREE_04_20_MINUS_06, b_trees:delete("k_06", test_generator:prepare_template_asc(?B_TREE_04_20))),
    test_generator:check_equal(?B_TREE_04_20_MINUS_08, b_trees:delete("k_08", test_generator:prepare_template_asc(?B_TREE_04_20))),
    test_generator:check_equal(?B_TREE_04_20_MINUS_10, b_trees:delete("k_10", test_generator:prepare_template_asc(?B_TREE_04_20))),
    test_generator:check_equal(?B_TREE_04_20_MINUS_12, b_trees:delete("k_12", test_generator:prepare_template_asc(?B_TREE_04_20))),
    test_generator:check_equal(?B_TREE_04_20_MINUS_14, b_trees:delete("k_14", test_generator:prepare_template_asc(?B_TREE_04_20))),
    test_generator:check_equal(?B_TREE_04_20_MINUS_16, b_trees:delete("k_16", test_generator:prepare_template_asc(?B_TREE_04_20))),
    test_generator:check_equal(?B_TREE_04_20_MINUS_18, b_trees:delete("k_18", test_generator:prepare_template_asc(?B_TREE_04_20))),

    test_generator:check_equal(?B_TREE_04_33_MINUS_02_TILL_18, b_trees:delete("k_18", test_generator:prepare_template_asc(?B_TREE_04_33_MINUS_02_TILL_16))),
    test_generator:check_equal(?B_TREE_04_33_MINUS_02_TILL_20, b_trees:delete("k_20", test_generator:prepare_template_asc(?B_TREE_04_33_MINUS_02_TILL_18))),
    test_generator:check_equal(?B_TREE_04_33_MINUS_02_TILL_22, b_trees:delete("k_22", test_generator:prepare_template_asc(?B_TREE_04_33_MINUS_02_TILL_20))),
    test_generator:check_equal(?B_TREE_04_33_MINUS_02_TILL_30, b_trees:delete("k_30", test_generator:prepare_template_asc(?B_TREE_04_33_MINUS_02_TILL_28))),

    test_generator:check_equal(?B_TREE_04_64_MINUS_08, b_trees:delete("k_08", test_generator:prepare_template_asc(?B_TREE_04_64))),
    test_generator:check_equal(?B_TREE_04_64_MINUS_16, b_trees:delete("k_16", test_generator:prepare_template_asc(?B_TREE_04_64))),
    test_generator:check_equal(?B_TREE_04_64_MINUS_32, b_trees:delete("k_32", test_generator:prepare_template_asc(?B_TREE_04_64))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete - order 6
%%--------------------------------------------------------------------

delete_b_tree_order_6_test() ->
    test_generator:check_equal(?B_TREE_06_00, b_trees:delete("k_01", test_generator:prepare_template_asc(?B_TREE_06_01))),

    test_generator:check_equal(?B_TREE_06_06_MINUS_01, b_trees:delete("k_01", test_generator:prepare_template_asc(?B_TREE_06_06))),
    test_generator:check_equal(?B_TREE_06_06_MINUS_02, b_trees:delete("k_02", test_generator:prepare_template_asc(?B_TREE_06_06))),
    test_generator:check_equal(?B_TREE_06_06_MINUS_03, b_trees:delete("k_03", test_generator:prepare_template_asc(?B_TREE_06_06))),
    test_generator:check_equal(?B_TREE_06_06_MINUS_04, b_trees:delete("k_04", test_generator:prepare_template_asc(?B_TREE_06_06))),
    test_generator:check_equal(?B_TREE_06_06_MINUS_05, b_trees:delete("k_05", test_generator:prepare_template_asc(?B_TREE_06_06))),
    test_generator:check_equal(?B_TREE_06_06_MINUS_06, b_trees:delete("k_06", test_generator:prepare_template_asc(?B_TREE_06_06))),

    test_generator:check_equal(?B_TREE_06_09_MINUS_01, b_trees:delete("k_01", test_generator:prepare_template_asc(?B_TREE_06_09))),
    test_generator:check_equal(?B_TREE_06_09_MINUS_02, b_trees:delete("k_02", test_generator:prepare_template_asc(?B_TREE_06_09))),
    test_generator:check_equal(?B_TREE_06_09_MINUS_03, b_trees:delete("k_03", test_generator:prepare_template_asc(?B_TREE_06_09))),
    test_generator:check_equal(?B_TREE_06_09_MINUS_04, b_trees:delete("k_04", test_generator:prepare_template_asc(?B_TREE_06_09))),
    test_generator:check_equal(?B_TREE_06_09_MINUS_05, b_trees:delete("k_05", test_generator:prepare_template_asc(?B_TREE_06_09))),
    test_generator:check_equal(?B_TREE_06_09_MINUS_06, b_trees:delete("k_06", test_generator:prepare_template_asc(?B_TREE_06_09))),
    test_generator:check_equal(?B_TREE_06_09_MINUS_07, b_trees:delete("k_07", test_generator:prepare_template_asc(?B_TREE_06_09))),
    test_generator:check_equal(?B_TREE_06_09_MINUS_08, b_trees:delete("k_08", test_generator:prepare_template_asc(?B_TREE_06_09))),
    test_generator:check_equal(?B_TREE_06_09_MINUS_09, b_trees:delete("k_09", test_generator:prepare_template_asc(?B_TREE_06_09))),

    test_generator:check_equal(?B_TREE_06_10_MINUS_01, b_trees:delete("k_01", test_generator:prepare_template_asc(?B_TREE_06_10))),
    test_generator:check_equal(?B_TREE_06_10_MINUS_02, b_trees:delete("k_02", test_generator:prepare_template_asc(?B_TREE_06_10))),
    test_generator:check_equal(?B_TREE_06_10_MINUS_03, b_trees:delete("k_03", test_generator:prepare_template_asc(?B_TREE_06_10))),
    test_generator:check_equal(?B_TREE_06_10_MINUS_04, b_trees:delete("k_04", test_generator:prepare_template_asc(?B_TREE_06_10))),
    test_generator:check_equal(?B_TREE_06_10_MINUS_05, b_trees:delete("k_05", test_generator:prepare_template_asc(?B_TREE_06_10))),
    test_generator:check_equal(?B_TREE_06_10_MINUS_06, b_trees:delete("k_06", test_generator:prepare_template_asc(?B_TREE_06_10))),
    test_generator:check_equal(?B_TREE_06_10_MINUS_07, b_trees:delete("k_07", test_generator:prepare_template_asc(?B_TREE_06_10))),
    test_generator:check_equal(?B_TREE_06_10_MINUS_08, b_trees:delete("k_08", test_generator:prepare_template_asc(?B_TREE_06_10))),
    test_generator:check_equal(?B_TREE_06_10_MINUS_09, b_trees:delete("k_09", test_generator:prepare_template_asc(?B_TREE_06_10))),
    test_generator:check_equal(?B_TREE_06_10_MINUS_10, b_trees:delete("k_10", test_generator:prepare_template_asc(?B_TREE_06_10))),

    test_generator:check_equal(?B_TREE_06_00, test_generator:delete_b_tree_from(6, 1, 4)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:delete_b_tree_from(6, 5, 4)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:delete_b_tree_from(6, 8, 4)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:delete_b_tree_from(6, 11, 4)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:delete_b_tree_from(6, 14, 4)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:delete_b_tree_from(6, 17, 4)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:delete_b_tree_from(6, 20, 4)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:delete_b_tree_till(6, 100, 4)),

    test_generator:check_equal(?B_TREE_06_00, test_generator:delete_b_tree_from(6, 32, 2, test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete - persistence by ets
%%--------------------------------------------------------------------

delete_b_tree_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_04_32_32 = test_generator:generate_b_tree_from_number_ets(4, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    B_TREE_04_32_01 = b_trees:delete("k_01", B_TREE_04_32_32),
    B_TREE_04_32_02 = b_trees:delete("k_02", B_TREE_04_32_01),
    B_TREE_04_32_03 = b_trees:delete("k_03", B_TREE_04_32_02),
    B_TREE_04_32_04 = b_trees:delete("k_04", B_TREE_04_32_03),
    B_TREE_04_32_05 = b_trees:delete("k_05", B_TREE_04_32_04),
    B_TREE_04_32_06 = b_trees:delete("k_06", B_TREE_04_32_05),
    B_TREE_04_32_07 = b_trees:delete("k_07", B_TREE_04_32_06),
    B_TREE_04_32_08 = b_trees:delete("k_08", B_TREE_04_32_07),
    B_TREE_04_32_09 = b_trees:delete("k_09", B_TREE_04_32_08),
    B_TREE_04_32_10 = b_trees:delete("k_10", B_TREE_04_32_09),
    B_TREE_04_32_11 = b_trees:delete("k_11", B_TREE_04_32_10),
    B_TREE_04_32_12 = b_trees:delete("k_12", B_TREE_04_32_11),
    B_TREE_04_32_13 = b_trees:delete("k_13", B_TREE_04_32_12),
    B_TREE_04_32_14 = b_trees:delete("k_14", B_TREE_04_32_13),
    B_TREE_04_32_15 = b_trees:delete("k_15", B_TREE_04_32_14),
    B_TREE_04_32_16 = b_trees:delete("k_16", B_TREE_04_32_15),
    B_TREE_04_32_17 = b_trees:delete("k_17", B_TREE_04_32_16),
    B_TREE_04_32_18 = b_trees:delete("k_18", B_TREE_04_32_17),
    B_TREE_04_32_19 = b_trees:delete("k_19", B_TREE_04_32_18),
    B_TREE_04_32_20 = b_trees:delete("k_20", B_TREE_04_32_19),
    B_TREE_04_32_21 = b_trees:delete("k_21", B_TREE_04_32_20),
    B_TREE_04_32_22 = b_trees:delete("k_22", B_TREE_04_32_21),
    B_TREE_04_32_23 = b_trees:delete("k_23", B_TREE_04_32_22),
    B_TREE_04_32_24 = b_trees:delete("k_24", B_TREE_04_32_23),
    B_TREE_04_32_25 = b_trees:delete("k_25", B_TREE_04_32_24),
    B_TREE_04_32_26 = b_trees:delete("k_26", B_TREE_04_32_25),
    B_TREE_04_32_27 = b_trees:delete("k_27", B_TREE_04_32_26),
    B_TREE_04_32_28 = b_trees:delete("k_28", B_TREE_04_32_27),
    B_TREE_04_32_29 = b_trees:delete("k_29", B_TREE_04_32_28),
    B_TREE_04_32_30 = b_trees:delete("k_30", B_TREE_04_32_29),
    B_TREE_04_32_31 = b_trees:delete("k_31", B_TREE_04_32_30),
    B_TREE_04_32_00 = b_trees:delete("k_32", B_TREE_04_32_31),

    ?assertEqual(0, b_trees:number_key_values(B_TREE_04_32_00)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete - Examples based on
%%                      CLRS Introduction to Algorithms
%%--------------------------------------------------------------------

delete_clrs_test() ->
    % case 1
    _B_TREE_CLRS_500_MINUS_F = b_trees:delete("k_f", test_generator:prepare_template_asc(?B_TREE_CLRS_500)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F, _B_TREE_CLRS_500_MINUS_F),

    % case 2a
    _B_TREE_CLRS_500_MINUS_F_M = b_trees:delete("k_m", test_generator:prepare_template_asc(?B_TREE_CLRS_500_MINUS_F)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F_M, _B_TREE_CLRS_500_MINUS_F_M),

    % case 2b
    _B_TREE_CLRS_500_MINUS_F_L = b_trees:delete("k_l", test_generator:prepare_template_asc(?B_TREE_CLRS_500_MINUS_F_2)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F_L, _B_TREE_CLRS_500_MINUS_F_L),

    % case 2c
    _B_TREE_CLRS_500_MINUS_F_M_G = b_trees:delete("k_g", test_generator:prepare_template_asc(?B_TREE_CLRS_500_MINUS_F_M)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F_M_G, _B_TREE_CLRS_500_MINUS_F_M_G),

    % case 3a - delete left
    _B_TREE_CLRS_500_MINUS_F_M_G_D_A = b_trees:delete("k_a", test_generator:prepare_template_asc(?B_TREE_CLRS_500_MINUS_F_M_G_D)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F_M_G_D_A, _B_TREE_CLRS_500_MINUS_F_M_G_D_A),
    _B_TREE_CLRS_500_MINUS_F_M_G_D_B = b_trees:delete("k_b", test_generator:prepare_template_asc(?B_TREE_CLRS_500_MINUS_F_M_G_D)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F_M_G_D_B, _B_TREE_CLRS_500_MINUS_F_M_G_D_B),
    _B_TREE_CLRS_500_2_MINUS_F_M_G_D_E = b_trees:delete("k_e", test_generator:prepare_template_asc(?B_TREE_CLRS_500_2_MINUS_F_M_G_D)),
    test_generator:check_equal(?B_TREE_CLRS_500_2_MINUS_F_M_G_D_E, _B_TREE_CLRS_500_2_MINUS_F_M_G_D_E),
    _B_TREE_CLRS_500_2_MINUS_F_M_G_D_J = b_trees:delete("k_j", test_generator:prepare_template_asc(?B_TREE_CLRS_500_2_MINUS_F_M_G_D)),
    test_generator:check_equal(?B_TREE_CLRS_500_2_MINUS_F_M_G_D_J, _B_TREE_CLRS_500_2_MINUS_F_M_G_D_J),
    _B_TREE_CLRS_500_MINUS_F_M_G_D_N = b_trees:delete("k_n", test_generator:prepare_template_asc(?B_TREE_CLRS_500_MINUS_F_M_G_D)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F_M_G_D_N, _B_TREE_CLRS_500_MINUS_F_M_G_D_N),
    _B_TREE_CLRS_500_MINUS_F_M_G_D_O = b_trees:delete("k_o", test_generator:prepare_template_asc(?B_TREE_CLRS_500_MINUS_F_M_G_D)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F_M_G_D_O, _B_TREE_CLRS_500_MINUS_F_M_G_D_O),

    % case 3a delete right
    _B_TREE_CLRS_500_MINUS_F_M_G_D_U = b_trees:delete("k_u", test_generator:prepare_template_asc(?B_TREE_CLRS_500_MINUS_F_M_G_D)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F_M_G_D_U, _B_TREE_CLRS_500_MINUS_F_M_G_D_U),
    _B_TREE_CLRS_500_MINUS_F_M_G_D_V = b_trees:delete("k_v", test_generator:prepare_template_asc(?B_TREE_CLRS_500_MINUS_F_M_G_D)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F_M_G_D_V, _B_TREE_CLRS_500_MINUS_F_M_G_D_V),

    % case 3b
    _B_TREE_CLRS_500_MINUS_F_M_G_D = b_trees:delete("k_d", test_generator:prepare_template_asc(?B_TREE_CLRS_500_MINUS_F_M_G)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F_M_G_D, _B_TREE_CLRS_500_MINUS_F_M_G_D),
    _B_TREE_CLRS_500_MINUS_F_M_G_U = b_trees:delete("k_u", test_generator:prepare_template_asc(?B_TREE_CLRS_500_MINUS_F_M_G)),
    test_generator:check_equal(?B_TREE_CLRS_500_MINUS_F_M_G_U, _B_TREE_CLRS_500_MINUS_F_M_G_U),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: delete - error
%%--------------------------------------------------------------------

delete_error_test() ->
    ?assertException(error, {key_not_found, "k_00"}, b_trees:delete("k_00", test_generator:prepare_template_asc(?B_TREE_06_00))),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:delete("k_00", test_generator:prepare_template_asc(?B_TREE_06_01))),

    ?assertException(error, {key_not_found, "k_10"}, b_trees:delete("k_10", test_generator:prepare_template_asc(?B_TREE_06_09))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: empty
%%--------------------------------------------------------------------

empty_b_tree_test() ->
    test_generator:check_equal(?B_TREE_04_00, b_trees:empty(4)),
    ?assert(b_trees:is_empty(b_trees:empty(4))),
    ?assertEqual({0, 0}, b_trees:number_nodes(b_trees:empty(4))),

    test_generator:check_equal(?B_TREE_06_00, b_trees:empty(6)),
    ?assert(b_trees:is_empty(b_trees:empty(6))),
    ?assertEqual({0, 0}, b_trees:number_nodes(b_trees:empty(6))),

    test_generator:check_equal(?B_TREE_32_00, b_trees:empty(32)),
    ?assert(b_trees:is_empty(b_trees:empty(32))),
    ?assertEqual({0, 0}, b_trees:number_nodes(b_trees:empty(32))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter - order 4
%%--------------------------------------------------------------------

enter_b_tree_order_4_test() ->
    BTree_04_15_01 = b_trees:enter("k_01", "v_01", test_generator:prepare_template_asc(?B_TREE_04_00)),
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
    test_generator:check_equal(?B_TREE_04_15, BTree_04_15_15),

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
    test_generator:check_equal(?B_TREE_04_15_UPDATE, b_trees:enter("k_15", "v_15_new", BTree_04_15_14_NEW)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: enter - persistence by ets
%%--------------------------------------------------------------------

enter_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_04_32_30 = test_generator:generate_b_tree_from_number_ets(4, 30, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    B_TREE_04_32_31 = b_trees:enter("k_31", "v_31", B_TREE_04_32_30),
    B_TREE_04_32_32 = b_trees:enter("k_32", "v_32", B_TREE_04_32_31),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    B_TREE_04_32_01_NEW = b_trees:enter("k_01", "v_01_new", B_TREE_04_32_32),
    B_TREE_04_32_02_NEW = b_trees:enter("k_02", "v_02_new", B_TREE_04_32_01_NEW),
    B_TREE_04_32_03_NEW = b_trees:enter("k_03", "v_03_new", B_TREE_04_32_02_NEW),
    B_TREE_04_32_04_NEW = b_trees:enter("k_04", "v_04_new", B_TREE_04_32_03_NEW),
    B_TREE_04_32_05_NEW = b_trees:enter("k_05", "v_05_new", B_TREE_04_32_04_NEW),
    B_TREE_04_32_06_NEW = b_trees:enter("k_06", "v_06_new", B_TREE_04_32_05_NEW),
    B_TREE_04_32_07_NEW = b_trees:enter("k_07", "v_07_new", B_TREE_04_32_06_NEW),
    B_TREE_04_32_08_NEW = b_trees:enter("k_08", "v_08_new", B_TREE_04_32_07_NEW),
    B_TREE_04_32_09_NEW = b_trees:enter("k_09", "v_09_new", B_TREE_04_32_08_NEW),
    B_TREE_04_32_10_NEW = b_trees:enter("k_10", "v_10_new", B_TREE_04_32_09_NEW),
    B_TREE_04_32_11_NEW = b_trees:enter("k_11", "v_11_new", B_TREE_04_32_10_NEW),
    B_TREE_04_32_12_NEW = b_trees:enter("k_12", "v_12_new", B_TREE_04_32_11_NEW),
    B_TREE_04_32_13_NEW = b_trees:enter("k_13", "v_13_new", B_TREE_04_32_12_NEW),
    B_TREE_04_32_14_NEW = b_trees:enter("k_14", "v_14_new", B_TREE_04_32_13_NEW),
    B_TREE_04_32_15_NEW = b_trees:enter("k_15", "v_15_new", B_TREE_04_32_14_NEW),
    B_TREE_04_32_16_NEW = b_trees:enter("k_16", "v_16_new", B_TREE_04_32_15_NEW),
    B_TREE_04_32_17_NEW = b_trees:enter("k_17", "v_17_new", B_TREE_04_32_16_NEW),
    B_TREE_04_32_18_NEW = b_trees:enter("k_18", "v_18_new", B_TREE_04_32_17_NEW),
    B_TREE_04_32_19_NEW = b_trees:enter("k_19", "v_19_new", B_TREE_04_32_18_NEW),
    B_TREE_04_32_20_NEW = b_trees:enter("k_20", "v_20_new", B_TREE_04_32_19_NEW),
    B_TREE_04_32_21_NEW = b_trees:enter("k_21", "v_21_new", B_TREE_04_32_20_NEW),
    B_TREE_04_32_22_NEW = b_trees:enter("k_22", "v_22_new", B_TREE_04_32_21_NEW),
    B_TREE_04_32_23_NEW = b_trees:enter("k_23", "v_23_new", B_TREE_04_32_22_NEW),
    B_TREE_04_32_24_NEW = b_trees:enter("k_24", "v_24_new", B_TREE_04_32_23_NEW),
    B_TREE_04_32_25_NEW = b_trees:enter("k_25", "v_25_new", B_TREE_04_32_24_NEW),
    B_TREE_04_32_26_NEW = b_trees:enter("k_26", "v_26_new", B_TREE_04_32_25_NEW),
    B_TREE_04_32_27_NEW = b_trees:enter("k_27", "v_27_new", B_TREE_04_32_26_NEW),
    B_TREE_04_32_28_NEW = b_trees:enter("k_28", "v_28_new", B_TREE_04_32_27_NEW),
    B_TREE_04_32_29_NEW = b_trees:enter("k_29", "v_29_new", B_TREE_04_32_28_NEW),
    B_TREE_04_32_30_NEW = b_trees:enter("k_30", "v_30_new", B_TREE_04_32_29_NEW),
    B_TREE_04_32_31_NEW = b_trees:enter("k_31", "v_31_new", B_TREE_04_32_30_NEW),
    B_TREE_04_32_32_NEW = b_trees:enter("k_32", "v_32_new", B_TREE_04_32_31_NEW),

    B_TREE_04_32_MAPPED_VALUES = b_trees:to_list(B_TREE_04_32_32_NEW),

    ?assertEqual(B_TREE_04_32_MAPPED_VALUES, test_generator:generate_key_values_from_update(32, 2)),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict
%%--------------------------------------------------------------------

from_dict_b_tree_test() ->
    ?assertException(error, {tree_not_empty, _}, b_trees:from_dict(?B_TREE_04_04, test_generator:generate_key_values_from(4, 2))),

    test_generator:check_equal(?B_TREE_04_04, b_trees:from_dict(b_trees:empty(4), test_generator:generate_key_values_from(4, 2))),
    test_generator:check_equal(?B_TREE_06_01, b_trees:from_dict(b_trees:empty(6), test_generator:generate_key_values_from(1, 2))),
    test_generator:check_equal(?B_TREE_06_29, b_trees:from_dict(b_trees:empty(6), test_generator:generate_key_values_from(29, 2))),
    test_generator:check_equal(?B_TREE_18_19, b_trees:from_dict(b_trees:empty(18), test_generator:generate_key_values_from(19, 2))),

    B_TREE_06_32_00 = b_trees:set_parameter(b_trees:empty(6), sort, fun b_trees:sort_descending/2),
    test_generator:check_equal(?B_TREE_06_32_DESC, b_trees:from_dict(B_TREE_06_32_00, test_generator:generate_key_values_from(32, 2))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: from_dict - persistence by ets
%%--------------------------------------------------------------------

from_dict_persistence_by_ets_test() ->
    StateTarget = ets:new(b_trees, []),

    B_TREE_06_00 = b_trees:set_parameter(b_trees:empty(6), state, {StateTarget, fun test_generator:persistence_by_ets/3, fun test_generator:persistence_by_ets/3, fun test_generator:persistence_by_ets/3}),

    ?assertEqual(32, b_trees:number_key_values(b_trees:from_dict(B_TREE_06_00, test_generator:generate_key_values_from(32, 2)))),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get - persistence by ets
%%--------------------------------------------------------------------

get_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(4, length(ets:tab2list(StateTarget))),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", B_TREE_06_32)),
    ?assertEqual("v_01", b_trees:get("k_01", B_TREE_06_32)),
    ?assertEqual("v_02", b_trees:get("k_02", B_TREE_06_32)),
    ?assertEqual("v_03", b_trees:get("k_03", B_TREE_06_32)),
    ?assertEqual("v_04", b_trees:get("k_04", B_TREE_06_32)),
    ?assertEqual("v_05", b_trees:get("k_05", B_TREE_06_32)),
    ?assertEqual("v_06", b_trees:get("k_06", B_TREE_06_32)),
    ?assertEqual("v_07", b_trees:get("k_07", B_TREE_06_32)),
    ?assertEqual("v_08", b_trees:get("k_08", B_TREE_06_32)),
    ?assertEqual("v_09", b_trees:get("k_09", B_TREE_06_32)),
    ?assertEqual("v_10", b_trees:get("k_10", B_TREE_06_32)),
    ?assertEqual("v_11", b_trees:get("k_11", B_TREE_06_32)),
    ?assertEqual("v_12", b_trees:get("k_12", B_TREE_06_32)),
    ?assertEqual("v_13", b_trees:get("k_13", B_TREE_06_32)),
    ?assertEqual("v_14", b_trees:get("k_14", B_TREE_06_32)),
    ?assertEqual("v_15", b_trees:get("k_15", B_TREE_06_32)),
    ?assertEqual("v_16", b_trees:get("k_16", B_TREE_06_32)),
    ?assertEqual("v_17", b_trees:get("k_17", B_TREE_06_32)),
    ?assertEqual("v_18", b_trees:get("k_18", B_TREE_06_32)),
    ?assertEqual("v_19", b_trees:get("k_19", B_TREE_06_32)),
    ?assertEqual("v_20", b_trees:get("k_20", B_TREE_06_32)),
    ?assertEqual("v_21", b_trees:get("k_21", B_TREE_06_32)),
    ?assertEqual("v_22", b_trees:get("k_22", B_TREE_06_32)),
    ?assertEqual("v_23", b_trees:get("k_23", B_TREE_06_32)),
    ?assertEqual("v_24", b_trees:get("k_24", B_TREE_06_32)),
    ?assertEqual("v_25", b_trees:get("k_25", B_TREE_06_32)),
    ?assertEqual("v_26", b_trees:get("k_26", B_TREE_06_32)),
    ?assertEqual("v_27", b_trees:get("k_27", B_TREE_06_32)),
    ?assertEqual("v_28", b_trees:get("k_28", B_TREE_06_32)),
    ?assertEqual("v_29", b_trees:get("k_29", B_TREE_06_32)),
    ?assertEqual("v_30", b_trees:get("k_30", B_TREE_06_32)),
    ?assertEqual("v_31", b_trees:get("k_31", B_TREE_06_32)),
    ?assertEqual("v_32", b_trees:get("k_32", B_TREE_06_32)),
    ?assertException(error, {key_not_found, "k_33"}, b_trees:get("k_33", B_TREE_06_32)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: get
%%--------------------------------------------------------------------

get_test() ->
    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", test_generator:prepare_template_asc(?B_TREE_04_00))),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assertEqual("v_01", b_trees:get("k_01", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assertEqual("v_02", b_trees:get("k_02", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assertEqual("v_03", b_trees:get("k_03", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assertEqual("v_04", b_trees:get("k_04", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assertException(error, {key_not_found, "k_05"}, b_trees:get("k_05", test_generator:prepare_template_asc(?B_TREE_04_04))),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual("v_01", b_trees:get("k_01", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual("v_02", b_trees:get("k_02", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual("v_03", b_trees:get("k_03", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual("v_04", b_trees:get("k_04", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual("v_05", b_trees:get("k_05", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual("v_06", b_trees:get("k_06", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual("v_07", b_trees:get("k_07", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertException(error, {key_not_found, "k_08"}, b_trees:get("k_08", test_generator:prepare_template_asc(?B_TREE_06_07))),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual("v_01", b_trees:get("k_01", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual("v_02", b_trees:get("k_02", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual("v_03", b_trees:get("k_03", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual("v_04", b_trees:get("k_04", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual("v_05", b_trees:get("k_05", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual("v_06", b_trees:get("k_06", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual("v_07", b_trees:get("k_07", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual("v_08", b_trees:get("k_08", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual("v_09", b_trees:get("k_09", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual("v_10", b_trees:get("k_10", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertException(error, {key_not_found, "k_11"}, b_trees:get("k_11", test_generator:prepare_template_asc(?B_TREE_10_10))),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_01", b_trees:get("k_01", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_02", b_trees:get("k_02", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_03", b_trees:get("k_03", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_04", b_trees:get("k_04", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_05", b_trees:get("k_05", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_06", b_trees:get("k_06", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_07", b_trees:get("k_07", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_08", b_trees:get("k_08", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_09", b_trees:get("k_09", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_10", b_trees:get("k_10", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_11", b_trees:get("k_11", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_12", b_trees:get("k_12", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual("v_13", b_trees:get("k_13", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertException(error, {key_not_found, "k_14"}, b_trees:get("k_14", test_generator:prepare_template_asc(?B_TREE_12_13))),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_01", b_trees:get("k_01", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_02", b_trees:get("k_02", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_03", b_trees:get("k_03", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_04", b_trees:get("k_04", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_05", b_trees:get("k_05", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_06", b_trees:get("k_06", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_07", b_trees:get("k_07", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_08", b_trees:get("k_08", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_09", b_trees:get("k_09", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_10", b_trees:get("k_10", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_11", b_trees:get("k_11", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_12", b_trees:get("k_12", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_13", b_trees:get("k_13", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_14", b_trees:get("k_14", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_15", b_trees:get("k_15", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual("v_16", b_trees:get("k_16", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertException(error, {key_not_found, "k_17"}, b_trees:get("k_17", test_generator:prepare_template_asc(?B_TREE_16_16))),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_01", b_trees:get("k_01", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_02", b_trees:get("k_02", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_03", b_trees:get("k_03", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_04", b_trees:get("k_04", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_05", b_trees:get("k_05", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_06", b_trees:get("k_06", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_07", b_trees:get("k_07", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_08", b_trees:get("k_08", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_09", b_trees:get("k_09", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_10", b_trees:get("k_10", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_11", b_trees:get("k_11", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_12", b_trees:get("k_12", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_13", b_trees:get("k_13", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_14", b_trees:get("k_14", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_15", b_trees:get("k_15", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_16", b_trees:get("k_16", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_17", b_trees:get("k_17", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_18", b_trees:get("k_18", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual("v_19", b_trees:get("k_19", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertException(error, {key_not_found, "k_20"}, b_trees:get("k_20", test_generator:prepare_template_asc(?B_TREE_18_19))),

    ?assertException(error, {key_not_found, "k_00"}, b_trees:get("k_00", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_01", b_trees:get("k_01", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_02", b_trees:get("k_02", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_03", b_trees:get("k_03", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_04", b_trees:get("k_04", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_05", b_trees:get("k_05", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_06", b_trees:get("k_06", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_07", b_trees:get("k_07", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_08", b_trees:get("k_08", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_09", b_trees:get("k_09", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_10", b_trees:get("k_10", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_11", b_trees:get("k_11", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_12", b_trees:get("k_12", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_13", b_trees:get("k_13", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_14", b_trees:get("k_14", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_15", b_trees:get("k_15", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_16", b_trees:get("k_16", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_17", b_trees:get("k_17", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_18", b_trees:get("k_18", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_19", b_trees:get("k_19", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_20", b_trees:get("k_20", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_21", b_trees:get("k_21", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_22", b_trees:get("k_22", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_23", b_trees:get("k_23", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_24", b_trees:get("k_24", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_25", b_trees:get("k_25", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_26", b_trees:get("k_26", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_27", b_trees:get("k_27", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_28", b_trees:get("k_28", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_29", b_trees:get("k_29", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_30", b_trees:get("k_30", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_31", b_trees:get("k_31", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual("v_32", b_trees:get("k_32", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertException(error, {key_not_found, "k_33"}, b_trees:get("k_33", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: height - persistence by ets
%%--------------------------------------------------------------------

height_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(4, length(ets:tab2list(StateTarget))),

    ?assertEqual(2, b_trees:height(test_generator:prepare_template_desc(B_TREE_06_32))),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: height
%%--------------------------------------------------------------------

height_test() ->
    ?assertException(error, {empty_tree, _}, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_00))),

    ?assertEqual(0, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_02))),
    ?assertEqual(1, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_06))),
    ?assertEqual(2, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_21))),

    ?assertEqual(2, b_trees:height(test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert - order 4
%%--------------------------------------------------------------------

insert_b_tree_order_4_test() ->
    test_generator:check_equal(?B_TREE_04_00, b_trees:empty(4)),

    test_generator:check_equal(?B_TREE_04_01, test_generator:generate_b_tree_from_number(4, 1, 2)),
    test_generator:check_equal(?B_TREE_04_02, test_generator:generate_b_tree_from_number(4, 2, 2)),
    test_generator:check_equal(?B_TREE_04_03, test_generator:generate_b_tree_from_number(4, 3, 2)),
    test_generator:check_equal(?B_TREE_04_04, test_generator:generate_b_tree_from_number(4, 4, 2)),
    test_generator:check_equal(?B_TREE_04_05, test_generator:generate_b_tree_from_number(4, 5, 2)),
    test_generator:check_equal(?B_TREE_04_06, test_generator:generate_b_tree_from_number(4, 6, 2)),
    test_generator:check_equal(?B_TREE_04_07, test_generator:generate_b_tree_from_number(4, 7, 2)),
    test_generator:check_equal(?B_TREE_04_08, test_generator:generate_b_tree_from_number(4, 8, 2)),
    test_generator:check_equal(?B_TREE_04_09, test_generator:generate_b_tree_from_number(4, 9, 2)),
    test_generator:check_equal(?B_TREE_04_10, test_generator:generate_b_tree_from_number(4, 10, 2)),

    test_generator:check_equal(?B_TREE_04_11, test_generator:generate_b_tree_from_number(4, 11, 2)),
    test_generator:check_equal(?B_TREE_04_12, test_generator:generate_b_tree_from_number(4, 12, 2)),
    test_generator:check_equal(?B_TREE_04_13, test_generator:generate_b_tree_from_number(4, 13, 2)),
    test_generator:check_equal(?B_TREE_04_14, test_generator:generate_b_tree_from_number(4, 14, 2)),
    test_generator:check_equal(?B_TREE_04_15, test_generator:generate_b_tree_from_number(4, 15, 2)),
    test_generator:check_equal(?B_TREE_04_16, test_generator:generate_b_tree_from_number(4, 16, 2)),
    test_generator:check_equal(?B_TREE_04_17, test_generator:generate_b_tree_from_number(4, 17, 2)),
    test_generator:check_equal(?B_TREE_04_18, test_generator:generate_b_tree_from_number(4, 18, 2)),
    test_generator:check_equal(?B_TREE_04_19, test_generator:generate_b_tree_from_number(4, 19, 2)),
    test_generator:check_equal(?B_TREE_04_20, test_generator:generate_b_tree_from_number(4, 20, 2)),

    ok.


%%--------------------------------------------------------------------
%% TEST CASES: insert - order 6
%%--------------------------------------------------------------------

insert_b_tree_order_6_test() ->
    test_generator:check_equal(?B_TREE_06_00, b_trees:empty(6)),
    ?assertEqual(0, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_00))),
    ?assertException(error, {empty_tree, _}, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_00))),
    ?assertEqual({0, 0}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_00))),

    test_generator:check_equal(?B_TREE_06_01, b_trees:insert("k_01", "v_01", test_generator:prepare_template_asc(?B_TREE_06_00))),
    ?assertEqual(1, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_01))),
    ?assertEqual(0, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_01))),
    ?assertEqual({1, 1}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_01))),

    test_generator:check_equal(?B_TREE_06_02, b_trees:insert("k_02", "v_02", test_generator:generate_b_tree_from_number(6, 1, 2))),
    ?assertEqual(2, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_02))),
    ?assertEqual(0, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_02))),
    ?assertEqual({1, 1}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_02))),

    test_generator:check_equal(?B_TREE_06_03, b_trees:insert("k_03", "v_03", test_generator:generate_b_tree_from_number(6, 2, 2))),
    ?assertEqual(3, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_03))),
    ?assertEqual(0, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_03))),
    ?assertEqual({1, 1}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_03))),

    test_generator:check_equal(?B_TREE_06_04, b_trees:insert("k_04", "v_04", test_generator:generate_b_tree_from_number(6, 3, 2))),
    ?assertEqual(4, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_04))),
    ?assertEqual(0, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_04))),
    ?assertEqual({1, 1}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_04))),

    test_generator:check_equal(?B_TREE_06_05, b_trees:insert("k_05", "v_05", test_generator:generate_b_tree_from_number(6, 4, 2))),
    ?assertEqual(5, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_05))),
    ?assertEqual(0, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_05))),
    ?assertEqual({1, 1}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_05))),

    test_generator:check_equal(?B_TREE_06_09, b_trees:insert("k_09", "v_09", test_generator:generate_b_tree_from_number(6, 8, 2))),
    ?assertEqual(9, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_09))),
    ?assertEqual(1, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_09))),
    ?assertEqual({4, 3}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_09))),

    test_generator:check_equal(?B_TREE_06_13, b_trees:insert("k_13", "v_13", test_generator:generate_b_tree_from_number(6, 12, 2))),
    ?assertEqual(13, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_13))),
    ?assertEqual(1, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_13))),
    ?assertEqual({5, 4}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_13))),

    test_generator:check_equal(?B_TREE_06_17, b_trees:insert("k_17", "v_17", test_generator:generate_b_tree_from_number(6, 16, 2))),
    ?assertEqual(17, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_17))),
    ?assertEqual(1, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_17))),
    ?assertEqual({6, 5}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_17))),

    test_generator:check_equal(?B_TREE_06_21, b_trees:insert("k_21", "v_21", test_generator:generate_b_tree_from_number(6, 20, 2))),
    ?assertEqual(21, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_21))),
    ?assertEqual(2, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_21))),
    ?assertEqual({10, 7}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_21))),

    test_generator:check_equal(?B_TREE_06_25, b_trees:insert("k_25", "v_25", test_generator:generate_b_tree_from_number(6, 24, 2))),
    ?assertEqual(25, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_25))),
    ?assertEqual(2, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_25))),
    ?assertEqual({11, 8}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_25))),

    test_generator:check_equal(?B_TREE_06_29, b_trees:insert("k_29", "v_29", test_generator:generate_b_tree_from_number(6, 28, 2))),
    ?assertEqual(29, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_29))),
    ?assertEqual(2, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_29))),
    ?assertEqual({13, 9}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_29))),

    test_generator:check_equal(?B_TREE_06_30, b_trees:insert("k_30", "v_30", test_generator:generate_b_tree_from_number(6, 29, 2))),
    ?assertEqual(30, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_30))),
    ?assertEqual(2, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_30))),
    ?assertEqual({14, 10}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_30))),

    test_generator:check_equal(?B_TREE_06_32_DESC, b_trees:insert("k_32", "v_32", test_generator:generate_b_tree_from_number(6, 31, 2, fun b_trees:sort_descending/2))),
    ?assertEqual(32, b_trees:number_key_values(test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual(2, b_trees:height(test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({14, 10}, b_trees:number_nodes(test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert - order 8
%%--------------------------------------------------------------------

insert_b_tree_order_8_test() ->
    test_generator:check_equal(?B_TREE_08_00, b_trees:empty(8)),
    ?assertEqual(0, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_08_00))),
    ?assertException(error, {empty_tree, _}, b_trees:height(test_generator:prepare_template_asc(?B_TREE_08_00))),
    ?assertEqual({0, 0}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_08_00))),

    test_generator:check_equal(?B_TREE_06_07, b_trees:insert("k_07", "v_07", test_generator:generate_b_tree_from_number(6, 6, 2))),
    ?assertEqual(7, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual(1, b_trees:height(test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual({3, 2}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_07))),

    test_generator:check_equal(?B_TREE_08_16, b_trees:insert("k_16", "v_16", test_generator:generate_b_tree_from_number(8, 15, 2))),
    ?assertEqual(16, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_08_16))),
    ?assertEqual(1, b_trees:height(test_generator:prepare_template_asc(?B_TREE_08_16))),
    ?assertEqual({5, 4}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_08_16))),

    test_generator:check_equal(?B_TREE_08_32, b_trees:insert("k_32", "v_32", test_generator:generate_b_tree_from_number(8, 31, 2))),
    ?assertEqual(32, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_08_32))),
    ?assertEqual(1, b_trees:height(test_generator:prepare_template_asc(?B_TREE_08_32))),
    ?assertEqual({9, 8}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_08_32))),

    test_generator:check_equal(?B_TREE_08_64, b_trees:insert("k_64", "v_64", test_generator:generate_b_tree_from_number(8, 63, 2))),
    ?assertEqual(64, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_08_64))),
    ?assertEqual(2, b_trees:height(test_generator:prepare_template_asc(?B_TREE_08_64))),
    ?assertEqual({20, 16}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_08_64))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert - persistence by ets
%%--------------------------------------------------------------------

insert_b_tree_persistence_by_ets_test() ->
    StateTarget = ets:new(b_trees, []),

    B_TREE_04_00 = b_trees:set_parameter(b_trees:empty(4), state, {StateTarget, fun test_generator:persistence_by_ets/3, fun test_generator:persistence_by_ets/3, fun test_generator:persistence_by_ets/3}),

    B_TREE_04_01 = b_trees:insert("k_01", "v_01", B_TREE_04_00),
    B_TREE_04_02 = b_trees:insert("k_02", "v_02", B_TREE_04_01),
    B_TREE_04_03 = b_trees:insert("k_03", "v_03", B_TREE_04_02),
    B_TREE_04_04 = b_trees:insert("k_04", "v_04", B_TREE_04_03),
    B_TREE_04_05 = b_trees:insert("k_05", "v_05", B_TREE_04_04),
    B_TREE_04_06 = b_trees:insert("k_06", "v_06", B_TREE_04_05),
    B_TREE_04_07 = b_trees:insert("k_07", "v_07", B_TREE_04_06),
    B_TREE_04_08 = b_trees:insert("k_08", "v_08", B_TREE_04_07),
    B_TREE_04_09 = b_trees:insert("k_09", "v_09", B_TREE_04_08),
    B_TREE_04_10 = b_trees:insert("k_10", "v_10", B_TREE_04_09),
    B_TREE_04_11 = b_trees:insert("k_11", "v_11", B_TREE_04_10),
    B_TREE_04_12 = b_trees:insert("k_12", "v_12", B_TREE_04_11),
    B_TREE_04_13 = b_trees:insert("k_13", "v_13", B_TREE_04_12),
    B_TREE_04_14 = b_trees:insert("k_14", "v_14", B_TREE_04_13),
    B_TREE_04_15 = b_trees:insert("k_15", "v_15", B_TREE_04_14),
    B_TREE_04_16 = b_trees:insert("k_16", "v_16", B_TREE_04_15),
    B_TREE_04_17 = b_trees:insert("k_17", "v_17", B_TREE_04_16),
    B_TREE_04_18 = b_trees:insert("k_18", "v_18", B_TREE_04_17),
    B_TREE_04_19 = b_trees:insert("k_19", "v_19", B_TREE_04_18),
    B_TREE_04_20 = b_trees:insert("k_20", "v_20", B_TREE_04_19),
    B_TREE_04_21 = b_trees:insert("k_21", "v_21", B_TREE_04_20),
    B_TREE_04_22 = b_trees:insert("k_22", "v_22", B_TREE_04_21),
    B_TREE_04_23 = b_trees:insert("k_23", "v_23", B_TREE_04_22),
    B_TREE_04_24 = b_trees:insert("k_24", "v_24", B_TREE_04_23),
    B_TREE_04_25 = b_trees:insert("k_25", "v_25", B_TREE_04_24),
    B_TREE_04_26 = b_trees:insert("k_26", "v_26", B_TREE_04_25),
    B_TREE_04_27 = b_trees:insert("k_27", "v_27", B_TREE_04_26),
    B_TREE_04_28 = b_trees:insert("k_28", "v_28", B_TREE_04_27),
    B_TREE_04_29 = b_trees:insert("k_29", "v_29", B_TREE_04_28),
    B_TREE_04_30 = b_trees:insert("k_30", "v_30", B_TREE_04_29),
    B_TREE_04_31 = b_trees:insert("k_31", "v_31", B_TREE_04_30),
    b_trees:insert("k_32", "v_32", B_TREE_04_31),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    true = ets:delete_all_objects(StateTarget),

    test_generator:generate_b_tree_from_number_ets(4, 99, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert - Examples based on
%%                      CLRS Introduction to Algorithms
%%--------------------------------------------------------------------

insert_clrs_test() ->
    _B_TREE_CLRS_498_PLUS_B = b_trees:insert("k_b", "v_b", test_generator:prepare_template_asc(?B_TREE_CLRS_498)),
    test_generator:check_equal(?B_TREE_CLRS_498_PLUS_B, _B_TREE_CLRS_498_PLUS_B),

    _B_TREE_CLRS_498_PLUS_B_Q = b_trees:insert("k_q", "v_q", test_generator:prepare_template_asc(?B_TREE_CLRS_498_PLUS_B)),
    test_generator:check_equal(?B_TREE_CLRS_498_PLUS_B_Q, _B_TREE_CLRS_498_PLUS_B_Q),

    _B_TREE_CLRS_498_PLUS_B_Q_L = b_trees:insert("k_l", "v_l", test_generator:prepare_template_asc(?B_TREE_CLRS_498_PLUS_B_Q)),
    test_generator:check_equal(?B_TREE_CLRS_498_PLUS_B_Q_L, _B_TREE_CLRS_498_PLUS_B_Q_L),

    _B_TREE_CLRS_498_PLUS_B_Q_L_F = b_trees:insert("k_f", "v_f", test_generator:prepare_template_asc(?B_TREE_CLRS_498_PLUS_B_Q_L)),
    test_generator:check_equal(?B_TREE_CLRS_498_PLUS_B_Q_L_F, _B_TREE_CLRS_498_PLUS_B_Q_L_F),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: insert - error
%%--------------------------------------------------------------------

insert_error_test() ->
    ?assertException(error, {key_exists, "k_01"}, b_trees:insert("k_01", "v_01", test_generator:generate_b_tree_from_number(6, 5, 2))),
    ?assertException(error, {key_exists, "k_02"}, b_trees:insert("k_02", "v_02", test_generator:generate_b_tree_from_number(6, 5, 2))),
    ?assertException(error, {key_exists, "k_03"}, b_trees:insert("k_03", "v_03", test_generator:generate_b_tree_from_number(6, 5, 2))),
    ?assertException(error, {key_exists, "k_04"}, b_trees:insert("k_04", "v_04", test_generator:generate_b_tree_from_number(6, 5, 2))),
    ?assertException(error, {key_exists, "k_05"}, b_trees:insert("k_05", "v_05", test_generator:generate_b_tree_from_number(6, 5, 2))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined - persistence by ets
%%--------------------------------------------------------------------

is_defined_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(4, length(ets:tab2list(StateTarget))),

    ?assertNot(b_trees:is_defined("k_00", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_01", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_02", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_03", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_04", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_05", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_06", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_07", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_08", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_09", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_10", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_11", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_12", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_13", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_14", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_15", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_16", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_17", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_18", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_19", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_20", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_21", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_22", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_23", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_24", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_25", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_26", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_27", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_28", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_29", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_30", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_31", B_TREE_06_32)),
    ?assert(b_trees:is_defined("k_32", B_TREE_06_32)),
    ?assertNot(b_trees:is_defined("k_33", B_TREE_06_32)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_defined
%%--------------------------------------------------------------------

is_defined_test() ->
    ?assertNot(b_trees:is_defined("k_00", test_generator:prepare_template_asc(?B_TREE_04_00))),

    ?assertNot(b_trees:is_defined("k_00", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assert(b_trees:is_defined("k_01", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assert(b_trees:is_defined("k_02", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assert(b_trees:is_defined("k_03", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assert(b_trees:is_defined("k_04", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assertNot(b_trees:is_defined("k_05", test_generator:prepare_template_asc(?B_TREE_04_04))),

    ?assertNot(b_trees:is_defined("k_00", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assert(b_trees:is_defined("k_01", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assert(b_trees:is_defined("k_02", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assert(b_trees:is_defined("k_03", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assert(b_trees:is_defined("k_04", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assert(b_trees:is_defined("k_05", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assert(b_trees:is_defined("k_06", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assert(b_trees:is_defined("k_07", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertNot(b_trees:is_defined("k_08", test_generator:prepare_template_asc(?B_TREE_06_07))),

    ?assertNot(b_trees:is_defined("k_00", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assert(b_trees:is_defined("k_01", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assert(b_trees:is_defined("k_02", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assert(b_trees:is_defined("k_03", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assert(b_trees:is_defined("k_04", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assert(b_trees:is_defined("k_05", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assert(b_trees:is_defined("k_06", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assert(b_trees:is_defined("k_07", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assert(b_trees:is_defined("k_08", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assert(b_trees:is_defined("k_09", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assert(b_trees:is_defined("k_10", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertNot(b_trees:is_defined("k_11", test_generator:prepare_template_asc(?B_TREE_10_10))),

    ?assertNot(b_trees:is_defined("k_00", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_01", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_02", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_03", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_04", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_05", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_06", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_07", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_08", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_09", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_10", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_11", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_12", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assert(b_trees:is_defined("k_13", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertNot(b_trees:is_defined("k_14", test_generator:prepare_template_asc(?B_TREE_12_13))),

    ?assertNot(b_trees:is_defined("k_00", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_01", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_02", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_03", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_04", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_05", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_06", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_07", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_08", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_09", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_10", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_11", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_12", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_13", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_14", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_15", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assert(b_trees:is_defined("k_16", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertNot(b_trees:is_defined("k_17", test_generator:prepare_template_asc(?B_TREE_16_16))),

    ?assertNot(b_trees:is_defined("k_00", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_01", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_02", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_03", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_04", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_05", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_06", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_07", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_08", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_09", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_10", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_11", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_12", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_13", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_14", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_15", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_16", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_17", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_18", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assert(b_trees:is_defined("k_19", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertNot(b_trees:is_defined("k_20", test_generator:prepare_template_asc(?B_TREE_18_19))),

    ?assertNot(b_trees:is_defined("k_00", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_01", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_02", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_03", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_04", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_05", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_06", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_07", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_08", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_09", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_10", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_11", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_12", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_13", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_14", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_15", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_16", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_17", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_18", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_19", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_20", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_21", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_22", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_23", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_24", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_25", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_26", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_27", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_28", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_29", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_30", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_31", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assert(b_trees:is_defined("k_32", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertNot(b_trees:is_defined("k_33", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),

    ok.


%%--------------------------------------------------------------------
%% TEST CASES: is_empty - persistence by ets
%%--------------------------------------------------------------------

is_empty_persistence_by_ets_test() ->
    StateTarget = ets:new(b_trees, []),

    B_TREE_06_00 = b_trees:set_parameter(b_trees:empty(6), state, {StateTarget, fun test_generator:persistence_by_ets/3, fun test_generator:persistence_by_ets/3, fun test_generator:persistence_by_ets/3}),

    ?assertEqual(true, b_trees:is_empty(test_generator:prepare_template_asc(B_TREE_06_00))),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: is_empty
%%--------------------------------------------------------------------

is_empty_test() ->
    ?assertEqual(false, b_trees:is_empty(test_generator:prepare_template_asc(?B_TREE_32_01))),
    ?assertEqual(true, b_trees:is_empty(test_generator:prepare_template_asc(?B_TREE_32_00))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next
%%--------------------------------------------------------------------

iterator_from_next_00_test() ->
    BTree_04_00 = b_trees:empty(4),
    _Iterator_04_00_00 = b_trees:iterator_from("k_01", BTree_04_00),
    ?assertEqual(none, b_trees:next(_Iterator_04_00_00)),

    BTree_04_01 = test_generator:generate_b_tree_from_number(4, 1, 2),
    Iterator_04_01_00 = b_trees:iterator_from("k_00", BTree_04_01),
    {_Key_04_01_01, _Value_04_01_01, _Iterator_04_01_01} = b_trees:next(Iterator_04_01_00),
    ?assertEqual({"k_01", "v_01"}, {_Key_04_01_01, _Value_04_01_01}),
    ?assertEqual(none, b_trees:next(_Iterator_04_01_01)),

    BTree_04_16 = test_generator:generate_b_tree_from_number(4, 16, 2),
    Iterator_04_16_01 = b_trees:iterator_from("k_00", BTree_04_16),
    {_Key_04_16_01, _Value_04_16_01, Iterator_04_16_02} = b_trees:next(Iterator_04_16_01),
    ?assertEqual({"k_01", "v_01"}, {_Key_04_16_01, _Value_04_16_01}),
    {_Key_04_16_02, _Value_04_16_02, _Iterator_04_16_03} = b_trees:next(Iterator_04_16_02),
    ?assertEqual({"k_02", "v_02"}, {_Key_04_16_02, _Value_04_16_02}),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next
%%--------------------------------------------------------------------

iterator_from_next_01_test() ->
    BTree_04_16 = test_generator:generate_b_tree_from_number(4, 16, 2),
    Iterator_04_16_01 = b_trees:iterator_from("k_01", BTree_04_16),
    {_Key_04_16_01, _Value_04_16_01, Iterator_04_16_02} = b_trees:next(Iterator_04_16_01),
    ?assertEqual({"k_01", "v_01"}, {_Key_04_16_01, _Value_04_16_01}),
    {_Key_04_16_02, _Value_04_16_02, _Iterator_04_16_03} = b_trees:next(Iterator_04_16_02),
    ?assertEqual({"k_02", "v_02"}, {_Key_04_16_02, _Value_04_16_02}),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next
%%--------------------------------------------------------------------

iterator_from_next_02_test() ->
    BTree_04_16 = test_generator:generate_b_tree_from_number(4, 16, 2),
    Iterator_04_16_02 = b_trees:iterator_from("k_02", BTree_04_16),
    {_Key_04_16_02, _Value_04_16_02, Iterator_04_16_03} = b_trees:next(Iterator_04_16_02),
    ?assertEqual({"k_02", "v_02"}, {_Key_04_16_02, _Value_04_16_02}),
    {_Key_04_16_03, _Value_04_16_03, _Iterator_04_16_04} = b_trees:next(Iterator_04_16_03),
    ?assertEqual({"k_03", "v_03"}, {_Key_04_16_03, _Value_04_16_03}),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next
%%--------------------------------------------------------------------

iterator_from_next_03_test() ->
    BTree_04_16 = test_generator:generate_b_tree_from_number(4, 16, 2),
    Iterator_04_16_03 = b_trees:iterator_from("k_03", BTree_04_16),
    {_Key_04_16_03, _Value_04_16_03, Iterator_04_16_04} = b_trees:next(Iterator_04_16_03),
    ?assertEqual({"k_03", "v_03"}, {_Key_04_16_03, _Value_04_16_03}),
    {_Key_04_16_04, _Value_04_16_04, _Iterator_04_16_05} = b_trees:next(Iterator_04_16_04),
    ?assertEqual({"k_04", "v_04"}, {_Key_04_16_04, _Value_04_16_04}),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next
%%--------------------------------------------------------------------

iterator_from_next_04_test() ->
    BTree_04_16 = test_generator:generate_b_tree_from_number(4, 16, 2),
    Iterator_04_16_04 = b_trees:iterator_from("k_04", BTree_04_16),
    {_Key_04_16_04, _Value_04_16_04, Iterator_04_16_05} = b_trees:next(Iterator_04_16_04),
    ?assertEqual({"k_04", "v_04"}, {_Key_04_16_04, _Value_04_16_04}),
    {_Key_04_16_05, _Value_04_16_05, _Iterator_04_16_06} = b_trees:next(Iterator_04_16_05),
    ?assertEqual({"k_05", "v_05"}, {_Key_04_16_05, _Value_04_16_05}),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next
%%--------------------------------------------------------------------

iterator_from_next_04_16_test() ->
    Number = 16,

    BTree = test_generator:generate_b_tree_from_number(4, Number, 2),
    KeyValues = test_generator:generate_key_values_from(Number, 2),

    ?assertEqual(lists:sublist(KeyValues, 1, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_00", BTree), Number - 0, [])),
    ?assertEqual(lists:sublist(KeyValues, 2, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_02", BTree), Number - 1, [])),
    ?assertEqual(lists:sublist(KeyValues, 3, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_03", BTree), Number - 2, [])),
    ?assertEqual(lists:sublist(KeyValues, 4, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_04", BTree), Number - 3, [])),
    ?assertEqual(lists:sublist(KeyValues, 5, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_05", BTree), Number - 4, [])),
    ?assertEqual(lists:sublist(KeyValues, 6, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_06", BTree), Number - 5, [])),
    ?assertEqual(lists:sublist(KeyValues, 7, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_07", BTree), Number - 6, [])),
    ?assertEqual(lists:sublist(KeyValues, 8, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_08", BTree), Number - 7, [])),
    ?assertEqual(lists:sublist(KeyValues, 9, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_09", BTree), Number - 8, [])),
    ?assertEqual(lists:sublist(KeyValues, 10, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_10", BTree), Number - 9, [])),
    ?assertEqual(lists:sublist(KeyValues, 11, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_11", BTree), Number - 10, [])),
    ?assertEqual(lists:sublist(KeyValues, 12, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_12", BTree), Number - 11, [])),
    ?assertEqual(lists:sublist(KeyValues, 13, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_13", BTree), Number - 12, [])),
    ?assertEqual(lists:sublist(KeyValues, 14, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_14", BTree), Number - 13, [])),
    ?assertEqual(lists:sublist(KeyValues, 15, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_15", BTree), Number - 14, [])),
    ?assertEqual(lists:sublist(KeyValues, 16, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_16", BTree), Number - 15, [])),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next
%%--------------------------------------------------------------------

iterator_from_next_06_test() ->
    BTree_04_16 = test_generator:generate_b_tree_from_number(4, 16, 2),
    Iterator_04_16_06 = b_trees:iterator_from("k_06", BTree_04_16),
    {_Key_04_16_06, _Value_04_16_06, Iterator_04_16_07} = b_trees:next(Iterator_04_16_06),
    ?assertEqual({"k_06", "v_06"}, {_Key_04_16_06, _Value_04_16_06}),
    {_Key_04_16_07, _Value_04_16_07, _Iterator_04_16_08} = b_trees:next(Iterator_04_16_07),
    ?assertEqual({"k_07", "v_07"}, {_Key_04_16_07, _Value_04_16_07}),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next
%%--------------------------------------------------------------------

iterator_from_next_06_32_test() ->
    Number = 32,

    BTree = test_generator:generate_b_tree_from_number(6, Number, 2, fun b_trees:sort_descending/2),
    KeyValues = test_generator:generate_key_values_till(Number, 2),

    ?assertEqual(lists:sublist(KeyValues, 1, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_33", BTree), Number - 0, [])),
    ?assertEqual(lists:sublist(KeyValues, 2, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_31", BTree), Number - 1, [])),
    ?assertEqual(lists:sublist(KeyValues, 3, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_30", BTree), Number - 2, [])),
    ?assertEqual(lists:sublist(KeyValues, 4, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_29", BTree), Number - 3, [])),
    ?assertEqual(lists:sublist(KeyValues, 5, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_28", BTree), Number - 4, [])),
    ?assertEqual(lists:sublist(KeyValues, 6, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_27", BTree), Number - 5, [])),
    ?assertEqual(lists:sublist(KeyValues, 7, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_26", BTree), Number - 6, [])),
    ?assertEqual(lists:sublist(KeyValues, 8, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_25", BTree), Number - 7, [])),
    ?assertEqual(lists:sublist(KeyValues, 9, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_24", BTree), Number - 8, [])),
    ?assertEqual(lists:sublist(KeyValues, 10, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_23", BTree), Number - 9, [])),
    ?assertEqual(lists:sublist(KeyValues, 11, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_22", BTree), Number - 10, [])),
    ?assertEqual(lists:sublist(KeyValues, 12, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_21", BTree), Number - 11, [])),
    ?assertEqual(lists:sublist(KeyValues, 13, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_20", BTree), Number - 12, [])),
    ?assertEqual(lists:sublist(KeyValues, 14, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_19", BTree), Number - 13, [])),
    ?assertEqual(lists:sublist(KeyValues, 15, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_18", BTree), Number - 14, [])),
    ?assertEqual(lists:sublist(KeyValues, 16, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_17", BTree), Number - 15, [])),
    ?assertEqual(lists:sublist(KeyValues, 17, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_16", BTree), Number - 16, [])),
    ?assertEqual(lists:sublist(KeyValues, 18, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_15", BTree), Number - 17, [])),
    ?assertEqual(lists:sublist(KeyValues, 19, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_14", BTree), Number - 18, [])),
    ?assertEqual(lists:sublist(KeyValues, 20, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_13", BTree), Number - 19, [])),
    ?assertEqual(lists:sublist(KeyValues, 21, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_12", BTree), Number - 20, [])),
    ?assertEqual(lists:sublist(KeyValues, 22, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_11", BTree), Number - 21, [])),
    ?assertEqual(lists:sublist(KeyValues, 23, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_10", BTree), Number - 22, [])),
    ?assertEqual(lists:sublist(KeyValues, 24, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_09", BTree), Number - 23, [])),
    ?assertEqual(lists:sublist(KeyValues, 25, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_08", BTree), Number - 24, [])),
    ?assertEqual(lists:sublist(KeyValues, 26, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_07", BTree), Number - 25, [])),
    ?assertEqual(lists:sublist(KeyValues, 27, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_06", BTree), Number - 26, [])),
    ?assertEqual(lists:sublist(KeyValues, 28, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_05", BTree), Number - 27, [])),
    ?assertEqual(lists:sublist(KeyValues, 29, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_04", BTree), Number - 28, [])),
    ?assertEqual(lists:sublist(KeyValues, 30, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_03", BTree), Number - 29, [])),
    ?assertEqual(lists:sublist(KeyValues, 31, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_02", BTree), Number - 30, [])),
    ?assertEqual(lists:sublist(KeyValues, 32, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_01", BTree), Number - 31, [])),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next
%%--------------------------------------------------------------------

iterator_from_next_08_256_test() ->
    Number = 256,

    BTree = test_generator:generate_b_tree_from_number(4, Number, 3),
    KeyValues = test_generator:generate_key_values_from(Number, 3),

    ?assertEqual(lists:sublist(KeyValues, 1, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_000", BTree), Number - 0, [])),
    ?assertEqual(lists:sublist(KeyValues, 2, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_002", BTree), Number - 1, [])),
    ?assertEqual(lists:sublist(KeyValues, 3, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_003", BTree), Number - 2, [])),
    ?assertEqual(lists:sublist(KeyValues, 4, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_004", BTree), Number - 3, [])),
    ?assertEqual(lists:sublist(KeyValues, 5, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_005", BTree), Number - 4, [])),
    ?assertEqual(lists:sublist(KeyValues, 7, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_007", BTree), Number - 6, [])),
    ?assertEqual(lists:sublist(KeyValues, 8, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_008", BTree), Number - 7, [])),
    ?assertEqual(lists:sublist(KeyValues, 11, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_011", BTree), Number - 10, [])),
    ?assertEqual(lists:sublist(KeyValues, 13, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_013", BTree), Number - 12, [])),
    ?assertEqual(lists:sublist(KeyValues, 16, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_016", BTree), Number - 15, [])),
    ?assertEqual(lists:sublist(KeyValues, 17, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_017", BTree), Number - 16, [])),
    ?assertEqual(lists:sublist(KeyValues, 19, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_019", BTree), Number - 18, [])),
    ?assertEqual(lists:sublist(KeyValues, 23, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_023", BTree), Number - 22, [])),
    ?assertEqual(lists:sublist(KeyValues, 29, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_029", BTree), Number - 28, [])),
    ?assertEqual(lists:sublist(KeyValues, 31, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_031", BTree), Number - 30, [])),
    ?assertEqual(lists:sublist(KeyValues, 32, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_032", BTree), Number - 31, [])),
    ?assertEqual(lists:sublist(KeyValues, 37, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_037", BTree), Number - 36, [])),
    ?assertEqual(lists:sublist(KeyValues, 41, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_041", BTree), Number - 40, [])),
    ?assertEqual(lists:sublist(KeyValues, 43, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_043", BTree), Number - 42, [])),
    ?assertEqual(lists:sublist(KeyValues, 47, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_047", BTree), Number - 46, [])),
    ?assertEqual(lists:sublist(KeyValues, 49, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_049", BTree), Number - 48, [])),
    ?assertEqual(lists:sublist(KeyValues, 64, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_064", BTree), Number - 63, [])),
    ?assertEqual(lists:sublist(KeyValues, 128, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_128", BTree), Number - 127, [])),
    ?assertEqual(lists:sublist(KeyValues, 256, Number), test_generator:iterate_next_b_tree(b_trees:iterator_from("k_256", BTree), Number - 255, [])),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next
%%--------------------------------------------------------------------

iterator_from_next_16_test() ->
    BTree_04_16 = test_generator:generate_b_tree_from_number(4, 16, 2),
    Iterator_04_16_16 = b_trees:iterator_from("k_16", BTree_04_16),
    {_Key_04_16_16, _Value_04_16_16, Iterator_04_16_17} = b_trees:next(Iterator_04_16_16),
    ?assertEqual({"k_16", "v_16"}, {_Key_04_16_16, _Value_04_16_16}),
    none = b_trees:next(Iterator_04_16_17),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next
%%--------------------------------------------------------------------

iterator_from_next_17_test() ->
    BTree_04_16 = test_generator:generate_b_tree_from_number(4, 16, 2),
    Iterator_04_16_17 = b_trees:iterator_from("k_17", BTree_04_16),
    none = b_trees:next(Iterator_04_16_17),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator_from & next - persistence by ets
%%--------------------------------------------------------------------

iterator_next_from_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_04_32 = test_generator:generate_b_tree_from_number_ets(4, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    Iterator_04_32_15 = b_trees:iterator_from("k_16", B_TREE_04_32),
    {_Key_04_32_16, _Value_05_30_16, Iterator_04_32_16} = b_trees:next(Iterator_04_32_15),
    ?assertEqual({"k_16", "v_16"}, {_Key_04_32_16, _Value_05_30_16}),
    {_Key_04_32_17, _Value_05_30_17, Iterator_04_32_17} = b_trees:next(Iterator_04_32_16),
    ?assertEqual({"k_17", "v_17"}, {_Key_04_32_17, _Value_05_30_17}),
    {_Key_04_32_18, _Value_05_30_18, Iterator_04_32_18} = b_trees:next(Iterator_04_32_17),
    ?assertEqual({"k_18", "v_18"}, {_Key_04_32_18, _Value_05_30_18}),
    {_Key_04_32_19, _Value_05_30_19, Iterator_04_32_19} = b_trees:next(Iterator_04_32_18),
    ?assertEqual({"k_19", "v_19"}, {_Key_04_32_19, _Value_05_30_19}),
    {_Key_04_32_20, _Value_05_30_20, Iterator_04_32_20} = b_trees:next(Iterator_04_32_19),
    ?assertEqual({"k_20", "v_20"}, {_Key_04_32_20, _Value_05_30_20}),
    {_Key_04_32_21, _Value_05_30_21, Iterator_04_32_21} = b_trees:next(Iterator_04_32_20),
    ?assertEqual({"k_21", "v_21"}, {_Key_04_32_21, _Value_05_30_21}),
    {_Key_04_32_22, _Value_05_30_22, Iterator_04_32_22} = b_trees:next(Iterator_04_32_21),
    ?assertEqual({"k_22", "v_22"}, {_Key_04_32_22, _Value_05_30_22}),
    {_Key_04_32_23, _Value_05_30_23, Iterator_04_32_23} = b_trees:next(Iterator_04_32_22),
    ?assertEqual({"k_23", "v_23"}, {_Key_04_32_23, _Value_05_30_23}),
    {_Key_04_32_24, _Value_05_30_24, Iterator_04_32_24} = b_trees:next(Iterator_04_32_23),
    ?assertEqual({"k_24", "v_24"}, {_Key_04_32_24, _Value_05_30_24}),
    {_Key_04_32_25, _Value_05_30_25, Iterator_04_32_25} = b_trees:next(Iterator_04_32_24),
    ?assertEqual({"k_25", "v_25"}, {_Key_04_32_25, _Value_05_30_25}),
    {_Key_04_32_26, _Value_05_30_26, Iterator_04_32_26} = b_trees:next(Iterator_04_32_25),
    ?assertEqual({"k_26", "v_26"}, {_Key_04_32_26, _Value_05_30_26}),
    {_Key_04_32_27, _Value_05_30_27, Iterator_04_32_27} = b_trees:next(Iterator_04_32_26),
    ?assertEqual({"k_27", "v_27"}, {_Key_04_32_27, _Value_05_30_27}),
    {_Key_04_32_28, _Value_05_30_28, Iterator_04_32_28} = b_trees:next(Iterator_04_32_27),
    ?assertEqual({"k_28", "v_28"}, {_Key_04_32_28, _Value_05_30_28}),
    {_Key_04_32_29, _Value_05_30_29, _Iterator_04_32_29} = b_trees:next(Iterator_04_32_28),
    ?assertEqual({"k_29", "v_29"}, {_Key_04_32_29, _Value_05_30_29}),
    {_Key_04_32_30, _Value_05_30_30, _Iterator_04_32_30} = b_trees:next(_Iterator_04_32_29),
    ?assertEqual({"k_30", "v_30"}, {_Key_04_32_30, _Value_05_30_30}),
    {_Key_04_32_31, _Value_05_30_31, _Iterator_04_32_31} = b_trees:next(_Iterator_04_32_30),
    ?assertEqual({"k_31", "v_31"}, {_Key_04_32_31, _Value_05_30_31}),
    {_Key_04_32_32, _Value_05_30_32, _Iterator_04_32_32} = b_trees:next(_Iterator_04_32_31),
    ?assertEqual({"k_32", "v_32"}, {_Key_04_32_32, _Value_05_30_32}),

    ?assertEqual(none, b_trees:next(_Iterator_04_32_32)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator & next - persistence by ets
%%--------------------------------------------------------------------

iterator_next_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_04_32 = test_generator:generate_b_tree_from_number_ets(4, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    Iterator_04_32_00 = b_trees:iterator(B_TREE_04_32),
    {_Key_04_32_01, _Value_05_30_01, Iterator_04_32_01} = b_trees:next(Iterator_04_32_00),
    ?assertEqual({"k_01", "v_01"}, {_Key_04_32_01, _Value_05_30_01}),
    {_Key_04_32_02, _Value_05_30_02, Iterator_04_32_02} = b_trees:next(Iterator_04_32_01),
    ?assertEqual({"k_02", "v_02"}, {_Key_04_32_02, _Value_05_30_02}),
    {_Key_04_32_03, _Value_05_30_03, Iterator_04_32_03} = b_trees:next(Iterator_04_32_02),
    ?assertEqual({"k_03", "v_03"}, {_Key_04_32_03, _Value_05_30_03}),
    {_Key_04_32_04, _Value_05_30_04, Iterator_04_32_04} = b_trees:next(Iterator_04_32_03),
    ?assertEqual({"k_04", "v_04"}, {_Key_04_32_04, _Value_05_30_04}),
    {_Key_04_32_05, _Value_05_30_05, Iterator_04_32_05} = b_trees:next(Iterator_04_32_04),
    ?assertEqual({"k_05", "v_05"}, {_Key_04_32_05, _Value_05_30_05}),
    {_Key_04_32_06, _Value_05_30_06, Iterator_04_32_06} = b_trees:next(Iterator_04_32_05),
    ?assertEqual({"k_06", "v_06"}, {_Key_04_32_06, _Value_05_30_06}),
    {_Key_04_32_07, _Value_05_30_07, Iterator_04_32_07} = b_trees:next(Iterator_04_32_06),
    ?assertEqual({"k_07", "v_07"}, {_Key_04_32_07, _Value_05_30_07}),
    {_Key_04_32_08, _Value_05_30_08, Iterator_04_32_08} = b_trees:next(Iterator_04_32_07),
    ?assertEqual({"k_08", "v_08"}, {_Key_04_32_08, _Value_05_30_08}),
    {_Key_04_32_09, _Value_05_30_09, Iterator_04_32_09} = b_trees:next(Iterator_04_32_08),
    ?assertEqual({"k_09", "v_09"}, {_Key_04_32_09, _Value_05_30_09}),
    {_Key_04_32_10, _Value_05_30_10, Iterator_04_32_10} = b_trees:next(Iterator_04_32_09),
    ?assertEqual({"k_10", "v_10"}, {_Key_04_32_10, _Value_05_30_10}),
    {_Key_04_32_11, _Value_05_30_11, Iterator_04_32_11} = b_trees:next(Iterator_04_32_10),
    ?assertEqual({"k_11", "v_11"}, {_Key_04_32_11, _Value_05_30_11}),
    {_Key_04_32_12, _Value_05_30_12, Iterator_04_32_12} = b_trees:next(Iterator_04_32_11),
    ?assertEqual({"k_12", "v_12"}, {_Key_04_32_12, _Value_05_30_12}),
    {_Key_04_32_13, _Value_05_30_13, Iterator_04_32_13} = b_trees:next(Iterator_04_32_12),
    ?assertEqual({"k_13", "v_13"}, {_Key_04_32_13, _Value_05_30_13}),
    {_Key_04_32_14, _Value_05_30_14, Iterator_04_32_14} = b_trees:next(Iterator_04_32_13),
    ?assertEqual({"k_14", "v_14"}, {_Key_04_32_14, _Value_05_30_14}),
    {_Key_04_32_15, _Value_05_30_15, Iterator_04_32_15} = b_trees:next(Iterator_04_32_14),
    ?assertEqual({"k_15", "v_15"}, {_Key_04_32_15, _Value_05_30_15}),
    {_Key_04_32_16, _Value_05_30_16, Iterator_04_32_16} = b_trees:next(Iterator_04_32_15),
    ?assertEqual({"k_16", "v_16"}, {_Key_04_32_16, _Value_05_30_16}),
    {_Key_04_32_17, _Value_05_30_17, Iterator_04_32_17} = b_trees:next(Iterator_04_32_16),
    ?assertEqual({"k_17", "v_17"}, {_Key_04_32_17, _Value_05_30_17}),
    {_Key_04_32_18, _Value_05_30_18, Iterator_04_32_18} = b_trees:next(Iterator_04_32_17),
    ?assertEqual({"k_18", "v_18"}, {_Key_04_32_18, _Value_05_30_18}),
    {_Key_04_32_19, _Value_05_30_19, Iterator_04_32_19} = b_trees:next(Iterator_04_32_18),
    ?assertEqual({"k_19", "v_19"}, {_Key_04_32_19, _Value_05_30_19}),
    {_Key_04_32_20, _Value_05_30_20, Iterator_04_32_20} = b_trees:next(Iterator_04_32_19),
    ?assertEqual({"k_20", "v_20"}, {_Key_04_32_20, _Value_05_30_20}),
    {_Key_04_32_21, _Value_05_30_21, Iterator_04_32_21} = b_trees:next(Iterator_04_32_20),
    ?assertEqual({"k_21", "v_21"}, {_Key_04_32_21, _Value_05_30_21}),
    {_Key_04_32_22, _Value_05_30_22, Iterator_04_32_22} = b_trees:next(Iterator_04_32_21),
    ?assertEqual({"k_22", "v_22"}, {_Key_04_32_22, _Value_05_30_22}),
    {_Key_04_32_23, _Value_05_30_23, Iterator_04_32_23} = b_trees:next(Iterator_04_32_22),
    ?assertEqual({"k_23", "v_23"}, {_Key_04_32_23, _Value_05_30_23}),
    {_Key_04_32_24, _Value_05_30_24, Iterator_04_32_24} = b_trees:next(Iterator_04_32_23),
    ?assertEqual({"k_24", "v_24"}, {_Key_04_32_24, _Value_05_30_24}),
    {_Key_04_32_25, _Value_05_30_25, Iterator_04_32_25} = b_trees:next(Iterator_04_32_24),
    ?assertEqual({"k_25", "v_25"}, {_Key_04_32_25, _Value_05_30_25}),
    {_Key_04_32_26, _Value_05_30_26, Iterator_04_32_26} = b_trees:next(Iterator_04_32_25),
    ?assertEqual({"k_26", "v_26"}, {_Key_04_32_26, _Value_05_30_26}),
    {_Key_04_32_27, _Value_05_30_27, Iterator_04_32_27} = b_trees:next(Iterator_04_32_26),
    ?assertEqual({"k_27", "v_27"}, {_Key_04_32_27, _Value_05_30_27}),
    {_Key_04_32_28, _Value_05_30_28, Iterator_04_32_28} = b_trees:next(Iterator_04_32_27),
    ?assertEqual({"k_28", "v_28"}, {_Key_04_32_28, _Value_05_30_28}),
    {_Key_04_32_29, _Value_05_30_29, _Iterator_04_32_29} = b_trees:next(Iterator_04_32_28),
    ?assertEqual({"k_29", "v_29"}, {_Key_04_32_29, _Value_05_30_29}),
    {_Key_04_32_30, _Value_05_30_30, _Iterator_04_32_30} = b_trees:next(_Iterator_04_32_29),
    ?assertEqual({"k_30", "v_30"}, {_Key_04_32_30, _Value_05_30_30}),
    {_Key_04_32_31, _Value_05_30_31, _Iterator_04_32_31} = b_trees:next(_Iterator_04_32_30),
    ?assertEqual({"k_31", "v_31"}, {_Key_04_32_31, _Value_05_30_31}),
    {_Key_04_32_32, _Value_05_30_32, _Iterator_04_32_32} = b_trees:next(_Iterator_04_32_31),
    ?assertEqual({"k_32", "v_32"}, {_Key_04_32_32, _Value_05_30_32}),

    ?assertEqual(none, b_trees:next(_Iterator_04_32_32)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: iterator & next
%%--------------------------------------------------------------------

iterator_next_test() ->
    BTree_06_00 = b_trees:empty(6),
    _Iterator_06_00_00 = b_trees:iterator(BTree_06_00),
    ?assertEqual(none, b_trees:next(_Iterator_06_00_00)),

    BTree_06_01 = test_generator:generate_b_tree_from_number(6, 1, 2),
    Iterator_06_01_00 = b_trees:iterator(BTree_06_01),
    {_Key_06_01_01, _Value_05_01_01, _Iterator_06_01_01} = b_trees:next(Iterator_06_01_00),
    ?assertEqual({"k_01", "v_01"}, {_Key_06_01_01, _Value_05_01_01}),
    ?assertEqual(none, b_trees:next(_Iterator_06_01_01)),

    BTree_06_30 = test_generator:generate_b_tree_from_number(6, 30, 2),
    Iterator_06_30_00 = b_trees:iterator(BTree_06_30),
    {_Key_06_30_01, _Value_05_30_01, Iterator_06_30_01} = b_trees:next(Iterator_06_30_00),
    ?assertEqual({"k_01", "v_01"}, {_Key_06_30_01, _Value_05_30_01}),
    {_Key_06_30_02, _Value_05_30_02, Iterator_06_30_02} = b_trees:next(Iterator_06_30_01),
    ?assertEqual({"k_02", "v_02"}, {_Key_06_30_02, _Value_05_30_02}),
    {_Key_06_30_03, _Value_05_30_03, Iterator_06_30_03} = b_trees:next(Iterator_06_30_02),
    ?assertEqual({"k_03", "v_03"}, {_Key_06_30_03, _Value_05_30_03}),
    {_Key_06_30_04, _Value_05_30_04, Iterator_06_30_04} = b_trees:next(Iterator_06_30_03),
    ?assertEqual({"k_04", "v_04"}, {_Key_06_30_04, _Value_05_30_04}),
    {_Key_06_30_05, _Value_05_30_05, Iterator_06_30_05} = b_trees:next(Iterator_06_30_04),
    ?assertEqual({"k_05", "v_05"}, {_Key_06_30_05, _Value_05_30_05}),
    {_Key_06_30_06, _Value_05_30_06, Iterator_06_30_06} = b_trees:next(Iterator_06_30_05),
    ?assertEqual({"k_06", "v_06"}, {_Key_06_30_06, _Value_05_30_06}),
    {_Key_06_30_07, _Value_05_30_07, Iterator_06_30_07} = b_trees:next(Iterator_06_30_06),
    ?assertEqual({"k_07", "v_07"}, {_Key_06_30_07, _Value_05_30_07}),
    {_Key_06_30_08, _Value_05_30_08, Iterator_06_30_08} = b_trees:next(Iterator_06_30_07),
    ?assertEqual({"k_08", "v_08"}, {_Key_06_30_08, _Value_05_30_08}),
    {_Key_06_30_09, _Value_05_30_09, Iterator_06_30_09} = b_trees:next(Iterator_06_30_08),
    ?assertEqual({"k_09", "v_09"}, {_Key_06_30_09, _Value_05_30_09}),
    {_Key_06_30_10, _Value_05_30_10, Iterator_06_30_10} = b_trees:next(Iterator_06_30_09),
    ?assertEqual({"k_10", "v_10"}, {_Key_06_30_10, _Value_05_30_10}),
    {_Key_06_30_11, _Value_05_30_11, Iterator_06_30_11} = b_trees:next(Iterator_06_30_10),
    ?assertEqual({"k_11", "v_11"}, {_Key_06_30_11, _Value_05_30_11}),
    {_Key_06_30_12, _Value_05_30_12, Iterator_06_30_12} = b_trees:next(Iterator_06_30_11),
    ?assertEqual({"k_12", "v_12"}, {_Key_06_30_12, _Value_05_30_12}),
    {_Key_06_30_13, _Value_05_30_13, Iterator_06_30_13} = b_trees:next(Iterator_06_30_12),
    ?assertEqual({"k_13", "v_13"}, {_Key_06_30_13, _Value_05_30_13}),
    {_Key_06_30_14, _Value_05_30_14, Iterator_06_30_14} = b_trees:next(Iterator_06_30_13),
    ?assertEqual({"k_14", "v_14"}, {_Key_06_30_14, _Value_05_30_14}),
    {_Key_06_30_15, _Value_05_30_15, Iterator_06_30_15} = b_trees:next(Iterator_06_30_14),
    ?assertEqual({"k_15", "v_15"}, {_Key_06_30_15, _Value_05_30_15}),
    {_Key_06_30_16, _Value_05_30_16, Iterator_06_30_16} = b_trees:next(Iterator_06_30_15),
    ?assertEqual({"k_16", "v_16"}, {_Key_06_30_16, _Value_05_30_16}),
    {_Key_06_30_17, _Value_05_30_17, Iterator_06_30_17} = b_trees:next(Iterator_06_30_16),
    ?assertEqual({"k_17", "v_17"}, {_Key_06_30_17, _Value_05_30_17}),
    {_Key_06_30_18, _Value_05_30_18, Iterator_06_30_18} = b_trees:next(Iterator_06_30_17),
    ?assertEqual({"k_18", "v_18"}, {_Key_06_30_18, _Value_05_30_18}),
    {_Key_06_30_19, _Value_05_30_19, Iterator_06_30_19} = b_trees:next(Iterator_06_30_18),
    ?assertEqual({"k_19", "v_19"}, {_Key_06_30_19, _Value_05_30_19}),
    {_Key_06_30_20, _Value_05_30_20, Iterator_06_30_20} = b_trees:next(Iterator_06_30_19),
    ?assertEqual({"k_20", "v_20"}, {_Key_06_30_20, _Value_05_30_20}),
    {_Key_06_30_21, _Value_05_30_21, Iterator_06_30_21} = b_trees:next(Iterator_06_30_20),
    ?assertEqual({"k_21", "v_21"}, {_Key_06_30_21, _Value_05_30_21}),
    {_Key_06_30_22, _Value_05_30_22, Iterator_06_30_22} = b_trees:next(Iterator_06_30_21),
    ?assertEqual({"k_22", "v_22"}, {_Key_06_30_22, _Value_05_30_22}),
    {_Key_06_30_23, _Value_05_30_23, Iterator_06_30_23} = b_trees:next(Iterator_06_30_22),
    ?assertEqual({"k_23", "v_23"}, {_Key_06_30_23, _Value_05_30_23}),
    {_Key_06_30_24, _Value_05_30_24, Iterator_06_30_24} = b_trees:next(Iterator_06_30_23),
    ?assertEqual({"k_24", "v_24"}, {_Key_06_30_24, _Value_05_30_24}),
    {_Key_06_30_25, _Value_05_30_25, Iterator_06_30_25} = b_trees:next(Iterator_06_30_24),
    ?assertEqual({"k_25", "v_25"}, {_Key_06_30_25, _Value_05_30_25}),
    {_Key_06_30_26, _Value_05_30_26, Iterator_06_30_26} = b_trees:next(Iterator_06_30_25),
    ?assertEqual({"k_26", "v_26"}, {_Key_06_30_26, _Value_05_30_26}),
    {_Key_06_30_27, _Value_05_30_27, Iterator_06_30_27} = b_trees:next(Iterator_06_30_26),
    ?assertEqual({"k_27", "v_27"}, {_Key_06_30_27, _Value_05_30_27}),
    {_Key_06_30_28, _Value_05_30_28, Iterator_06_30_28} = b_trees:next(Iterator_06_30_27),
    ?assertEqual({"k_28", "v_28"}, {_Key_06_30_28, _Value_05_30_28}),
    {_Key_06_30_29, _Value_05_30_29, Iterator_06_30_29} = b_trees:next(Iterator_06_30_28),
    ?assertEqual({"k_29", "v_29"}, {_Key_06_30_29, _Value_05_30_29}),
    {_Key_06_30_30, _Value_05_30_30, _Iterator_06_30_30} = b_trees:next(Iterator_06_30_29),
    ?assertEqual({"k_30", "v_30"}, {_Key_06_30_30, _Value_05_30_30}),
    ?assertEqual(none, b_trees:next(_Iterator_06_30_30)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys - persistence by ets
%%--------------------------------------------------------------------

keys_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(4, length(ets:tab2list(StateTarget))),

    _Keys_06_32 = b_trees:keys(test_generator:prepare_template_asc(B_TREE_06_32)),
    ?assertEqual(test_generator:generate_keys_from(32, 2), _Keys_06_32),
    ?assertEqual(32, length(_Keys_06_32)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: keys
%%--------------------------------------------------------------------

keys_test() ->
    ?assertEqual([], b_trees:keys(test_generator:prepare_template_asc(?B_TREE_06_00))),

    ?assertEqual(["k_01"], b_trees:keys(test_generator:prepare_template_asc(?B_TREE_06_01))),

    _Keys_06_02 = b_trees:keys(test_generator:prepare_template_asc(?B_TREE_06_02)),
    ?assertEqual(test_generator:generate_keys_from(2, 2), _Keys_06_02),
    ?assertEqual(2, length(_Keys_06_02)),

    _Keys_06_05 = b_trees:keys(test_generator:prepare_template_asc(?B_TREE_06_05)),
    ?assertEqual(test_generator:generate_keys_from(5, 2), _Keys_06_05),
    ?assertEqual(5, length(_Keys_06_05)),

    _Keys_06_09 = b_trees:keys(test_generator:prepare_template_asc(?B_TREE_06_09)),
    ?assertEqual(test_generator:generate_keys_from(9, 2), _Keys_06_09),
    ?assertEqual(9, length(_Keys_06_09)),

    _Keys_06_16 = b_trees:keys(test_generator:prepare_template_asc(?B_TREE_06_16)),
    ?assertEqual(test_generator:generate_keys_from(16, 2), _Keys_06_16),
    ?assertEqual(16, length(_Keys_06_16)),

    _Keys_06_32 = b_trees:keys(test_generator:prepare_template_desc(?B_TREE_06_32_DESC)),
    ?assertEqual(test_generator:generate_keys_till(32, 2), _Keys_06_32),
    ?assertEqual(32, length(_Keys_06_32)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest - persistence by ets
%%--------------------------------------------------------------------

largest_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(4, length(ets:tab2list(StateTarget))),

    ?assertEqual({"k_32", "v_32"}, b_trees:largest(B_TREE_06_32)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: largest
%%--------------------------------------------------------------------

largest_test() ->
    ?assertException(error, {empty_tree, _}, b_trees:largest(test_generator:prepare_template_asc(?B_TREE_06_00))),

    ?assertEqual({"k_01", "v_01"}, b_trees:largest(test_generator:prepare_template_asc(?B_TREE_06_01))),
    ?assertEqual({"k_02", "v_02"}, b_trees:largest(test_generator:prepare_template_asc(?B_TREE_06_02))),
    ?assertEqual({"k_05", "v_05"}, b_trees:largest(test_generator:prepare_template_asc(?B_TREE_06_05))),
    ?assertEqual({"k_09", "v_09"}, b_trees:largest(test_generator:prepare_template_asc(?B_TREE_06_09))),
    ?assertEqual({"k_16", "v_16"}, b_trees:largest(test_generator:prepare_template_asc(?B_TREE_06_16))),

    ?assertEqual({"k_19", "v_19"}, b_trees:largest(test_generator:prepare_template_asc(?B_TREE_18_19))),

    ?assertEqual({"k_01", "v_01"}, b_trees:largest(test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup - persistence by ets
%%--------------------------------------------------------------------

lookup_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(4, length(ets:tab2list(StateTarget))),

    ?assertEqual(none, b_trees:lookup("k_00", B_TREE_06_32)),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", B_TREE_06_32)),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", B_TREE_06_32)),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", B_TREE_06_32)),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", B_TREE_06_32)),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", B_TREE_06_32)),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", B_TREE_06_32)),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", B_TREE_06_32)),
    ?assertEqual({value, "v_08"}, b_trees:lookup("k_08", B_TREE_06_32)),
    ?assertEqual({value, "v_09"}, b_trees:lookup("k_09", B_TREE_06_32)),
    ?assertEqual({value, "v_10"}, b_trees:lookup("k_10", B_TREE_06_32)),
    ?assertEqual({value, "v_11"}, b_trees:lookup("k_11", B_TREE_06_32)),
    ?assertEqual({value, "v_12"}, b_trees:lookup("k_12", B_TREE_06_32)),
    ?assertEqual({value, "v_13"}, b_trees:lookup("k_13", B_TREE_06_32)),
    ?assertEqual({value, "v_14"}, b_trees:lookup("k_14", B_TREE_06_32)),
    ?assertEqual({value, "v_15"}, b_trees:lookup("k_15", B_TREE_06_32)),
    ?assertEqual({value, "v_16"}, b_trees:lookup("k_16", B_TREE_06_32)),
    ?assertEqual({value, "v_17"}, b_trees:lookup("k_17", B_TREE_06_32)),
    ?assertEqual({value, "v_18"}, b_trees:lookup("k_18", B_TREE_06_32)),
    ?assertEqual({value, "v_19"}, b_trees:lookup("k_19", B_TREE_06_32)),
    ?assertEqual({value, "v_20"}, b_trees:lookup("k_20", B_TREE_06_32)),
    ?assertEqual({value, "v_21"}, b_trees:lookup("k_21", B_TREE_06_32)),
    ?assertEqual({value, "v_22"}, b_trees:lookup("k_22", B_TREE_06_32)),
    ?assertEqual({value, "v_23"}, b_trees:lookup("k_23", B_TREE_06_32)),
    ?assertEqual({value, "v_24"}, b_trees:lookup("k_24", B_TREE_06_32)),
    ?assertEqual({value, "v_25"}, b_trees:lookup("k_25", B_TREE_06_32)),
    ?assertEqual({value, "v_26"}, b_trees:lookup("k_26", B_TREE_06_32)),
    ?assertEqual({value, "v_27"}, b_trees:lookup("k_27", B_TREE_06_32)),
    ?assertEqual({value, "v_28"}, b_trees:lookup("k_28", B_TREE_06_32)),
    ?assertEqual({value, "v_29"}, b_trees:lookup("k_29", B_TREE_06_32)),
    ?assertEqual({value, "v_30"}, b_trees:lookup("k_30", B_TREE_06_32)),
    ?assertEqual({value, "v_31"}, b_trees:lookup("k_31", B_TREE_06_32)),
    ?assertEqual({value, "v_32"}, b_trees:lookup("k_32", B_TREE_06_32)),
    ?assertEqual(none, b_trees:lookup("k_33", B_TREE_06_32)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: lookup
%%--------------------------------------------------------------------

lookup_test() ->
    ?assertEqual(none, b_trees:lookup("k_00", test_generator:prepare_template_asc(?B_TREE_04_00))),

    ?assertEqual(none, b_trees:lookup("k_00", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", test_generator:prepare_template_asc(?B_TREE_04_04))),
    ?assertEqual(none, b_trees:lookup("k_05", test_generator:prepare_template_asc(?B_TREE_04_04))),

    ?assertEqual(none, b_trees:lookup("k_00", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", test_generator:prepare_template_asc(?B_TREE_06_07))),
    ?assertEqual(none, b_trees:lookup("k_08", test_generator:prepare_template_asc(?B_TREE_06_07))),

    ?assertEqual(none, b_trees:lookup("k_00", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual({value, "v_08"}, b_trees:lookup("k_08", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual({value, "v_09"}, b_trees:lookup("k_09", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual({value, "v_10"}, b_trees:lookup("k_10", test_generator:prepare_template_asc(?B_TREE_10_10))),
    ?assertEqual(none, b_trees:lookup("k_11", test_generator:prepare_template_asc(?B_TREE_10_10))),

    ?assertEqual(none, b_trees:lookup("k_00", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_08"}, b_trees:lookup("k_08", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_09"}, b_trees:lookup("k_09", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_10"}, b_trees:lookup("k_10", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_11"}, b_trees:lookup("k_11", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_12"}, b_trees:lookup("k_12", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual({value, "v_13"}, b_trees:lookup("k_13", test_generator:prepare_template_asc(?B_TREE_12_13))),
    ?assertEqual(none, b_trees:lookup("k_14", test_generator:prepare_template_asc(?B_TREE_12_13))),

    ?assertEqual(none, b_trees:lookup("k_00", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_08"}, b_trees:lookup("k_08", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_09"}, b_trees:lookup("k_09", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_10"}, b_trees:lookup("k_10", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_11"}, b_trees:lookup("k_11", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_12"}, b_trees:lookup("k_12", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_13"}, b_trees:lookup("k_13", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_14"}, b_trees:lookup("k_14", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_15"}, b_trees:lookup("k_15", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual({value, "v_16"}, b_trees:lookup("k_16", test_generator:prepare_template_asc(?B_TREE_16_16))),
    ?assertEqual(none, b_trees:lookup("k_17", test_generator:prepare_template_asc(?B_TREE_16_16))),

    ?assertEqual(none, b_trees:lookup("k_00", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_08"}, b_trees:lookup("k_08", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_09"}, b_trees:lookup("k_09", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_10"}, b_trees:lookup("k_10", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_11"}, b_trees:lookup("k_11", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_12"}, b_trees:lookup("k_12", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_13"}, b_trees:lookup("k_13", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_14"}, b_trees:lookup("k_14", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_15"}, b_trees:lookup("k_15", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_16"}, b_trees:lookup("k_16", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_17"}, b_trees:lookup("k_17", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_18"}, b_trees:lookup("k_18", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual({value, "v_19"}, b_trees:lookup("k_19", test_generator:prepare_template_asc(?B_TREE_18_19))),
    ?assertEqual(none, b_trees:lookup("k_20", test_generator:prepare_template_asc(?B_TREE_18_19))),

    ?assertEqual(none, b_trees:lookup("k_00", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_01"}, b_trees:lookup("k_01", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_02"}, b_trees:lookup("k_02", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_03"}, b_trees:lookup("k_03", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_04"}, b_trees:lookup("k_04", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_05"}, b_trees:lookup("k_05", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_06"}, b_trees:lookup("k_06", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_07"}, b_trees:lookup("k_07", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_08"}, b_trees:lookup("k_08", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_09"}, b_trees:lookup("k_09", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_10"}, b_trees:lookup("k_10", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_11"}, b_trees:lookup("k_11", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_12"}, b_trees:lookup("k_12", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_13"}, b_trees:lookup("k_13", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_14"}, b_trees:lookup("k_14", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_15"}, b_trees:lookup("k_15", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_16"}, b_trees:lookup("k_16", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_17"}, b_trees:lookup("k_17", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_18"}, b_trees:lookup("k_18", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_19"}, b_trees:lookup("k_19", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_20"}, b_trees:lookup("k_20", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_21"}, b_trees:lookup("k_21", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_22"}, b_trees:lookup("k_22", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_23"}, b_trees:lookup("k_23", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_24"}, b_trees:lookup("k_24", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_25"}, b_trees:lookup("k_25", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_26"}, b_trees:lookup("k_26", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_27"}, b_trees:lookup("k_27", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_28"}, b_trees:lookup("k_28", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_29"}, b_trees:lookup("k_29", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_30"}, b_trees:lookup("k_30", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_31"}, b_trees:lookup("k_31", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual({value, "v_32"}, b_trees:lookup("k_32", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),
    ?assertEqual(none, b_trees:lookup("k_33", test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map - persistence by ets
%%--------------------------------------------------------------------

map_b_tree_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_04_32 = test_generator:generate_b_tree_from_number_ets(4, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    B_TREE_04_32_MAPPED = b_trees:map(fun test_generator:map_value_to_new/2, B_TREE_04_32),

    B_TREE_04_32_MAPPED_VALUES = b_trees:to_list(B_TREE_04_32_MAPPED),

    ?assertEqual(B_TREE_04_32_MAPPED_VALUES, test_generator:generate_key_values_from_update(32, 2)),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map
%%--------------------------------------------------------------------

map_b_tree_test() ->
    test_generator:check_equal(?B_TREE_04_15_UPDATE, b_trees:map(fun test_generator:map_value_to_new/2, test_generator:prepare_template_asc(?B_TREE_04_15))),

    test_generator:check_equal(?B_TREE_06_32_DESC_UPDATE, b_trees:map(fun test_generator:map_value_to_new/2, test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: map - error
%%--------------------------------------------------------------------

map_error_test() ->
    ?assertException(error, {empty_tree, _}, b_trees:map(fun test_generator:map_value_to_new/2, test_generator:prepare_template_asc(?B_TREE_06_00))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: number_key_values - persistence by ets
%%--------------------------------------------------------------------

number_key_values_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(4, length(ets:tab2list(StateTarget))),

    ?assertEqual(32, b_trees:number_key_values(test_generator:prepare_template_desc(B_TREE_06_32))),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: number_key_values
%%--------------------------------------------------------------------

number_key_values_test() ->
    ?assertEqual(0, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_00))),
    ?assertEqual(2, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_02))),
    ?assertEqual(5, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_05))),
    ?assertEqual(16, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_16))),
    ?assertEqual(29, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_06_29))),

    ?assertEqual(32, b_trees:number_key_values(test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),

    ?assertEqual(19, b_trees:number_key_values(test_generator:prepare_template_asc(?B_TREE_18_19))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: number_nodes - persistence by ets
%%--------------------------------------------------------------------

number_nodes_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(4, length(ets:tab2list(StateTarget))),

    ?assertEqual({14, 10}, b_trees:number_nodes(test_generator:prepare_template_desc(B_TREE_06_32))),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: number_nodes
%%--------------------------------------------------------------------

number_nodes_test() ->
    ?assertEqual({0, 0}, b_trees:number_nodes(b_trees:empty(6))),

    ?assertEqual({1, 1}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_03))),
    ?assertEqual({3, 2}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_08))),
    ?assertEqual({5, 4}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_13))),
    ?assertEqual({7, 6}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_20))),
    ?assertEqual({10, 7}, b_trees:number_nodes(test_generator:prepare_template_asc(?B_TREE_06_21))),

    ?assertEqual({14, 10}, b_trees:number_nodes(test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: set_parameter
%%--------------------------------------------------------------------

set_parameter_test() ->
    BTree_06_00 = b_trees:empty(6),
    BTree_06_00_DESC = b_trees:set_parameter(BTree_06_00, sort, fun b_trees:sort_descending/2),
    KeyValues = test_generator:generate_key_values_from(32, 2),
    test_generator:check_equal(?B_TREE_06_32_DESC, test_generator:generate_b_tree_list_and_btree(KeyValues, BTree_06_00_DESC)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest - persistence by ets
%%--------------------------------------------------------------------

smallest_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(4, length(ets:tab2list(StateTarget))),

    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(B_TREE_06_32)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: smallest
%%--------------------------------------------------------------------

smallest_test() ->
    ?assertException(error, {empty_tree, _}, b_trees:smallest(test_generator:prepare_template_asc(?B_TREE_06_00))),

    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(test_generator:prepare_template_asc(?B_TREE_06_01))),
    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(test_generator:prepare_template_asc(?B_TREE_06_02))),
    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(test_generator:prepare_template_asc(?B_TREE_06_05))),
    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(test_generator:prepare_template_asc(?B_TREE_06_09))),
    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(test_generator:prepare_template_asc(?B_TREE_06_16))),

    ?assertEqual({"k_32", "v_32"}, b_trees:smallest(test_generator:prepare_template_desc(?B_TREE_06_32_DESC))),

    ?assertEqual({"k_01", "v_01"}, b_trees:smallest(test_generator:prepare_template_asc(?B_TREE_18_19))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest - persistence by ets
%%--------------------------------------------------------------------

take_largest_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_04_32_32 = test_generator:generate_b_tree_from_number_ets(4, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    {"k_32", "v_32", B_TREE_04_32_31} = b_trees:take_largest(B_TREE_04_32_32),
    {"k_31", "v_31", B_TREE_04_32_30} = b_trees:take_largest(B_TREE_04_32_31),
    {"k_30", "v_30", B_TREE_04_32_29} = b_trees:take_largest(B_TREE_04_32_30),
    {"k_29", "v_29", B_TREE_04_32_28} = b_trees:take_largest(B_TREE_04_32_29),
    {"k_28", "v_28", B_TREE_04_32_27} = b_trees:take_largest(B_TREE_04_32_28),
    {"k_27", "v_27", B_TREE_04_32_26} = b_trees:take_largest(B_TREE_04_32_27),
    {"k_26", "v_26", B_TREE_04_32_25} = b_trees:take_largest(B_TREE_04_32_26),
    {"k_25", "v_25", B_TREE_04_32_24} = b_trees:take_largest(B_TREE_04_32_25),
    {"k_24", "v_24", B_TREE_04_32_23} = b_trees:take_largest(B_TREE_04_32_24),
    {"k_23", "v_23", B_TREE_04_32_22} = b_trees:take_largest(B_TREE_04_32_23),
    {"k_22", "v_22", B_TREE_04_32_21} = b_trees:take_largest(B_TREE_04_32_22),
    {"k_21", "v_21", B_TREE_04_32_20} = b_trees:take_largest(B_TREE_04_32_21),
    {"k_20", "v_20", B_TREE_04_32_19} = b_trees:take_largest(B_TREE_04_32_20),
    {"k_19", "v_19", B_TREE_04_32_18} = b_trees:take_largest(B_TREE_04_32_19),
    {"k_18", "v_18", B_TREE_04_32_17} = b_trees:take_largest(B_TREE_04_32_18),
    {"k_17", "v_17", B_TREE_04_32_16} = b_trees:take_largest(B_TREE_04_32_17),
    {"k_16", "v_16", B_TREE_04_32_15} = b_trees:take_largest(B_TREE_04_32_16),
    {"k_15", "v_15", B_TREE_04_32_14} = b_trees:take_largest(B_TREE_04_32_15),
    {"k_14", "v_14", B_TREE_04_32_13} = b_trees:take_largest(B_TREE_04_32_14),
    {"k_13", "v_13", B_TREE_04_32_12} = b_trees:take_largest(B_TREE_04_32_13),
    {"k_12", "v_12", B_TREE_04_32_11} = b_trees:take_largest(B_TREE_04_32_12),
    {"k_11", "v_11", B_TREE_04_32_10} = b_trees:take_largest(B_TREE_04_32_11),
    {"k_10", "v_10", B_TREE_04_32_09} = b_trees:take_largest(B_TREE_04_32_10),
    {"k_09", "v_09", B_TREE_04_32_08} = b_trees:take_largest(B_TREE_04_32_09),
    {"k_08", "v_08", B_TREE_04_32_07} = b_trees:take_largest(B_TREE_04_32_08),
    {"k_07", "v_07", B_TREE_04_32_06} = b_trees:take_largest(B_TREE_04_32_07),
    {"k_06", "v_06", B_TREE_04_32_05} = b_trees:take_largest(B_TREE_04_32_06),
    {"k_05", "v_05", B_TREE_04_32_04} = b_trees:take_largest(B_TREE_04_32_05),
    {"k_04", "v_04", B_TREE_04_32_03} = b_trees:take_largest(B_TREE_04_32_04),
    {"k_03", "v_03", B_TREE_04_32_02} = b_trees:take_largest(B_TREE_04_32_03),
    {"k_02", "v_02", B_TREE_04_32_01} = b_trees:take_largest(B_TREE_04_32_02),
    {"k_01", "v_01", B_TREE_04_32_00} = b_trees:take_largest(B_TREE_04_32_01),

    ?assertEqual(0, b_trees:number_key_values(B_TREE_04_32_00)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_largest
%%--------------------------------------------------------------------

take_largest_test() ->
    ?assertException(error, {empty_tree, _}, b_trees:take_largest(?B_TREE_06_00)),

    test_generator:check_equal(?B_TREE_04_00, test_generator:take_largest_b_tree(4, 3, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_largest_b_tree(4, 6, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_largest_b_tree(4, 9, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_largest_b_tree(4, 12, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_largest_b_tree(4, 15, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_largest_b_tree(4, 18, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_largest_b_tree(4, 21, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_largest_b_tree(4, 24, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_largest_b_tree(4, 27, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_largest_b_tree(4, 30, 2)),

    test_generator:check_equal(?B_TREE_06_00, test_generator:take_largest_b_tree(6, 4, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_largest_b_tree(6, 8, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_largest_b_tree(6, 12, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_largest_b_tree(6, 16, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_largest_b_tree(6, 20, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_largest_b_tree(6, 24, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_largest_b_tree(6, 28, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_largest_b_tree(6, 32, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_largest_b_tree(6, 32, 2, fun b_trees:sort_descending/2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_largest_b_tree(6, 36, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_largest_b_tree(6, 40, 2)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest - persistence by ets
%%--------------------------------------------------------------------

take_smallest_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_04_32_32 = test_generator:generate_b_tree_from_number_ets(4, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    {"k_01", "v_01", B_TREE_04_32_01} = b_trees:take_smallest(B_TREE_04_32_32),
    {"k_02", "v_02", B_TREE_04_32_02} = b_trees:take_smallest(B_TREE_04_32_01),
    {"k_03", "v_03", B_TREE_04_32_03} = b_trees:take_smallest(B_TREE_04_32_02),
    {"k_04", "v_04", B_TREE_04_32_04} = b_trees:take_smallest(B_TREE_04_32_03),
    {"k_05", "v_05", B_TREE_04_32_05} = b_trees:take_smallest(B_TREE_04_32_04),
    {"k_06", "v_06", B_TREE_04_32_06} = b_trees:take_smallest(B_TREE_04_32_05),
    {"k_07", "v_07", B_TREE_04_32_07} = b_trees:take_smallest(B_TREE_04_32_06),
    {"k_08", "v_08", B_TREE_04_32_08} = b_trees:take_smallest(B_TREE_04_32_07),
    {"k_09", "v_09", B_TREE_04_32_09} = b_trees:take_smallest(B_TREE_04_32_08),
    {"k_10", "v_10", B_TREE_04_32_10} = b_trees:take_smallest(B_TREE_04_32_09),
    {"k_11", "v_11", B_TREE_04_32_11} = b_trees:take_smallest(B_TREE_04_32_10),
    {"k_12", "v_12", B_TREE_04_32_12} = b_trees:take_smallest(B_TREE_04_32_11),
    {"k_13", "v_13", B_TREE_04_32_13} = b_trees:take_smallest(B_TREE_04_32_12),
    {"k_14", "v_14", B_TREE_04_32_14} = b_trees:take_smallest(B_TREE_04_32_13),
    {"k_15", "v_15", B_TREE_04_32_15} = b_trees:take_smallest(B_TREE_04_32_14),
    {"k_16", "v_16", B_TREE_04_32_16} = b_trees:take_smallest(B_TREE_04_32_15),
    {"k_17", "v_17", B_TREE_04_32_17} = b_trees:take_smallest(B_TREE_04_32_16),
    {"k_18", "v_18", B_TREE_04_32_18} = b_trees:take_smallest(B_TREE_04_32_17),
    {"k_19", "v_19", B_TREE_04_32_19} = b_trees:take_smallest(B_TREE_04_32_18),
    {"k_20", "v_20", B_TREE_04_32_20} = b_trees:take_smallest(B_TREE_04_32_19),
    {"k_21", "v_21", B_TREE_04_32_21} = b_trees:take_smallest(B_TREE_04_32_20),
    {"k_22", "v_22", B_TREE_04_32_22} = b_trees:take_smallest(B_TREE_04_32_21),
    {"k_23", "v_23", B_TREE_04_32_23} = b_trees:take_smallest(B_TREE_04_32_22),
    {"k_24", "v_24", B_TREE_04_32_24} = b_trees:take_smallest(B_TREE_04_32_23),
    {"k_25", "v_25", B_TREE_04_32_25} = b_trees:take_smallest(B_TREE_04_32_24),
    {"k_26", "v_26", B_TREE_04_32_26} = b_trees:take_smallest(B_TREE_04_32_25),
    {"k_27", "v_27", B_TREE_04_32_27} = b_trees:take_smallest(B_TREE_04_32_26),
    {"k_28", "v_28", B_TREE_04_32_28} = b_trees:take_smallest(B_TREE_04_32_27),
    {"k_29", "v_29", B_TREE_04_32_29} = b_trees:take_smallest(B_TREE_04_32_28),
    {"k_30", "v_30", B_TREE_04_32_30} = b_trees:take_smallest(B_TREE_04_32_29),
    {"k_31", "v_31", B_TREE_04_32_31} = b_trees:take_smallest(B_TREE_04_32_30),
    {"k_32", "v_32", B_TREE_04_32_00} = b_trees:take_smallest(B_TREE_04_32_31),

    ?assertEqual(0, b_trees:number_key_values(B_TREE_04_32_00)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: take_smallest
%%--------------------------------------------------------------------

take_smallest_test() ->
    ?assertException(error, {empty_tree, _}, b_trees:take_smallest(?B_TREE_06_00)),

    test_generator:check_equal(?B_TREE_04_00, test_generator:take_smallest_b_tree(4, 3, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_smallest_b_tree(4, 6, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_smallest_b_tree(4, 9, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_smallest_b_tree(4, 12, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_smallest_b_tree(4, 15, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_smallest_b_tree(4, 18, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_smallest_b_tree(4, 21, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_smallest_b_tree(4, 24, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_smallest_b_tree(4, 27, 2)),
    test_generator:check_equal(?B_TREE_04_00, test_generator:take_smallest_b_tree(4, 30, 2)),

    test_generator:check_equal(?B_TREE_06_00, test_generator:take_smallest_b_tree(6, 4, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_smallest_b_tree(6, 8, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_smallest_b_tree(6, 12, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_smallest_b_tree(6, 16, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_smallest_b_tree(6, 20, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_smallest_b_tree(6, 24, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_smallest_b_tree(6, 28, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_smallest_b_tree(6, 32, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_smallest_b_tree(6, 32, 2, fun b_trees:sort_descending/2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_smallest_b_tree(6, 36, 2)),
    test_generator:check_equal(?B_TREE_06_00, test_generator:take_smallest_b_tree(6, 40, 2)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list - persistence by ets
%%--------------------------------------------------------------------

to_list_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(4, length(ets:tab2list(StateTarget))),

    _KeyValues_06_32 = b_trees:to_list(B_TREE_06_32),
    ?assertEqual(test_generator:generate_key_values_from(32, 2), _KeyValues_06_32),
    ?assertEqual(32, length(_KeyValues_06_32)),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: to_list
%%--------------------------------------------------------------------

to_list_test() ->
    ?assertException(error, {empty_tree, _}, b_trees:to_list(test_generator:prepare_template_asc(?B_TREE_06_00))),

    ?assertEqual([{"k_01", "v_01"}], b_trees:to_list(test_generator:prepare_template_asc(?B_TREE_06_01))),

    _KeyValues_06_02 = b_trees:to_list(test_generator:prepare_template_asc(?B_TREE_06_02)),
    ?assertEqual(test_generator:generate_key_values_from(2, 2), _KeyValues_06_02),
    ?assertEqual(2, length(_KeyValues_06_02)),

    _KeyValues_06_05 = b_trees:to_list(test_generator:prepare_template_asc(?B_TREE_06_05)),
    ?assertEqual(test_generator:generate_key_values_from(5, 2), _KeyValues_06_05),
    ?assertEqual(5, length(_KeyValues_06_05)),

    _KeyValues_06_09 = b_trees:to_list(test_generator:prepare_template_asc(?B_TREE_06_09)),
    ?assertEqual(test_generator:generate_key_values_from(9, 2), _KeyValues_06_09),
    ?assertEqual(9, length(_KeyValues_06_09)),

    _KeyValues_06_16 = b_trees:to_list(test_generator:prepare_template_asc(?B_TREE_06_16)),
    ?assertEqual(test_generator:generate_key_values_from(16, 2), _KeyValues_06_16),
    ?assertEqual(16, length(_KeyValues_06_16)),

    _KeyValues_06_32 = b_trees:to_list(test_generator:prepare_template_desc(?B_TREE_06_32_DESC)),
    ?assertEqual(test_generator:generate_key_values_till(32, 2), _KeyValues_06_32),
    ?assertEqual(32, length(_KeyValues_06_32)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update
%%--------------------------------------------------------------------

update_b_tree_test() ->
    BTree_04_15_K_01 = b_trees:update("k_01", "v_01_new", test_generator:prepare_template_asc(?B_TREE_04_15)),
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
    _BTree_04_15_K_14 = b_trees:update("k_14", "v_14_new", BTree_04_15_K_13),
    test_generator:check_equal(?B_TREE_04_15_UPDATE, b_trees:update("k_15", "v_15_new", _BTree_04_15_K_14)),

    BTree_06_32_DESC_K_01 = b_trees:update("k_01", "v_01_new", test_generator:prepare_template_desc(?B_TREE_06_32_DESC)),
    BTree_06_32_DESC_K_02 = b_trees:update("k_02", "v_02_new", BTree_06_32_DESC_K_01),
    BTree_06_32_DESC_K_03 = b_trees:update("k_03", "v_03_new", BTree_06_32_DESC_K_02),
    BTree_06_32_DESC_K_04 = b_trees:update("k_04", "v_04_new", BTree_06_32_DESC_K_03),
    BTree_06_32_DESC_K_05 = b_trees:update("k_05", "v_05_new", BTree_06_32_DESC_K_04),
    BTree_06_32_DESC_K_06 = b_trees:update("k_06", "v_06_new", BTree_06_32_DESC_K_05),
    BTree_06_32_DESC_K_07 = b_trees:update("k_07", "v_07_new", BTree_06_32_DESC_K_06),
    BTree_06_32_DESC_K_08 = b_trees:update("k_08", "v_08_new", BTree_06_32_DESC_K_07),
    BTree_06_32_DESC_K_09 = b_trees:update("k_09", "v_09_new", BTree_06_32_DESC_K_08),
    BTree_06_32_DESC_K_10 = b_trees:update("k_10", "v_10_new", BTree_06_32_DESC_K_09),
    BTree_06_32_DESC_K_11 = b_trees:update("k_11", "v_11_new", BTree_06_32_DESC_K_10),
    BTree_06_32_DESC_K_12 = b_trees:update("k_12", "v_12_new", BTree_06_32_DESC_K_11),
    BTree_06_32_DESC_K_13 = b_trees:update("k_13", "v_13_new", BTree_06_32_DESC_K_12),
    BTree_06_32_DESC_K_14 = b_trees:update("k_14", "v_14_new", BTree_06_32_DESC_K_13),
    BTree_06_32_DESC_K_15 = b_trees:update("k_15", "v_15_new", BTree_06_32_DESC_K_14),
    BTree_06_32_DESC_K_16 = b_trees:update("k_16", "v_16_new", BTree_06_32_DESC_K_15),
    BTree_06_32_DESC_K_17 = b_trees:update("k_17", "v_17_new", BTree_06_32_DESC_K_16),
    BTree_06_32_DESC_K_18 = b_trees:update("k_18", "v_18_new", BTree_06_32_DESC_K_17),
    BTree_06_32_DESC_K_19 = b_trees:update("k_19", "v_19_new", BTree_06_32_DESC_K_18),
    BTree_06_32_DESC_K_20 = b_trees:update("k_20", "v_20_new", BTree_06_32_DESC_K_19),
    BTree_06_32_DESC_K_21 = b_trees:update("k_21", "v_21_new", BTree_06_32_DESC_K_20),
    BTree_06_32_DESC_K_22 = b_trees:update("k_22", "v_22_new", BTree_06_32_DESC_K_21),
    BTree_06_32_DESC_K_23 = b_trees:update("k_23", "v_23_new", BTree_06_32_DESC_K_22),
    BTree_06_32_DESC_K_24 = b_trees:update("k_24", "v_24_new", BTree_06_32_DESC_K_23),
    BTree_06_32_DESC_K_25 = b_trees:update("k_25", "v_25_new", BTree_06_32_DESC_K_24),
    BTree_06_32_DESC_K_26 = b_trees:update("k_26", "v_26_new", BTree_06_32_DESC_K_25),
    BTree_06_32_DESC_K_27 = b_trees:update("k_27", "v_27_new", BTree_06_32_DESC_K_26),
    BTree_06_32_DESC_K_28 = b_trees:update("k_28", "v_28_new", BTree_06_32_DESC_K_27),
    BTree_06_32_DESC_K_29 = b_trees:update("k_29", "v_29_new", BTree_06_32_DESC_K_28),
    BTree_06_32_DESC_K_30 = b_trees:update("k_30", "v_30_new", BTree_06_32_DESC_K_29),
    _BTree_06_32_DESC_K_31 = b_trees:update("k_31", "v_31_new", BTree_06_32_DESC_K_30),
    test_generator:check_equal(?B_TREE_06_32_DESC_UPDATE, b_trees:update("k_32", "v_32_new", _BTree_06_32_DESC_K_31)),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update - error
%%--------------------------------------------------------------------

update_error_test() ->
    ?assertException(error, {key_not_found, "k_00"}, b_trees:update("k_00", "v_00_new", test_generator:prepare_template_asc(?B_TREE_06_00))),
    ?assertException(error, {key_not_found, "k_00"}, b_trees:update("k_00", "v_00_new", test_generator:prepare_template_asc(?B_TREE_06_02))),
    ?assertException(error, {key_not_found, "k_00"}, b_trees:update("k_00", "v_00_new", test_generator:prepare_template_asc(?B_TREE_06_29))),
    ?assertException(error, {key_not_found, "k_30"}, b_trees:update("k_30", "v_30_new", test_generator:prepare_template_asc(?B_TREE_06_29))),
    ?assertException(error, {key_not_found, "k_00"}, b_trees:update("k_00", "v_00_new", test_generator:prepare_template_asc(?B_TREE_08_64))),
    ?assertException(error, {key_not_found, "k_65"}, b_trees:update("k_65", "v_65_new", test_generator:prepare_template_asc(?B_TREE_08_64))),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: update - persistence by ets
%%--------------------------------------------------------------------

update_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_04_32 = test_generator:generate_b_tree_from_number_ets(4, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    B_TREE_04_32_K_01 = b_trees:update("k_01", "v_01_new", B_TREE_04_32),
    B_TREE_04_32_K_02 = b_trees:update("k_02", "v_02_new", B_TREE_04_32_K_01),
    B_TREE_04_32_K_03 = b_trees:update("k_03", "v_03_new", B_TREE_04_32_K_02),
    B_TREE_04_32_K_04 = b_trees:update("k_04", "v_04_new", B_TREE_04_32_K_03),
    B_TREE_04_32_K_05 = b_trees:update("k_05", "v_05_new", B_TREE_04_32_K_04),
    B_TREE_04_32_K_06 = b_trees:update("k_06", "v_06_new", B_TREE_04_32_K_05),
    B_TREE_04_32_K_07 = b_trees:update("k_07", "v_07_new", B_TREE_04_32_K_06),
    B_TREE_04_32_K_08 = b_trees:update("k_08", "v_08_new", B_TREE_04_32_K_07),
    B_TREE_04_32_K_09 = b_trees:update("k_09", "v_09_new", B_TREE_04_32_K_08),
    B_TREE_04_32_K_10 = b_trees:update("k_10", "v_10_new", B_TREE_04_32_K_09),
    B_TREE_04_32_K_11 = b_trees:update("k_11", "v_11_new", B_TREE_04_32_K_10),
    B_TREE_04_32_K_12 = b_trees:update("k_12", "v_12_new", B_TREE_04_32_K_11),
    B_TREE_04_32_K_13 = b_trees:update("k_13", "v_13_new", B_TREE_04_32_K_12),
    B_TREE_04_32_K_14 = b_trees:update("k_14", "v_14_new", B_TREE_04_32_K_13),
    B_TREE_04_32_K_15 = b_trees:update("k_15", "v_15_new", B_TREE_04_32_K_14),
    B_TREE_04_32_K_16 = b_trees:update("k_16", "v_16_new", B_TREE_04_32_K_15),
    B_TREE_04_32_K_17 = b_trees:update("k_17", "v_17_new", B_TREE_04_32_K_16),
    B_TREE_04_32_K_18 = b_trees:update("k_18", "v_18_new", B_TREE_04_32_K_17),
    B_TREE_04_32_K_19 = b_trees:update("k_19", "v_19_new", B_TREE_04_32_K_18),
    B_TREE_04_32_K_20 = b_trees:update("k_20", "v_20_new", B_TREE_04_32_K_19),
    B_TREE_04_32_K_21 = b_trees:update("k_21", "v_21_new", B_TREE_04_32_K_20),
    B_TREE_04_32_K_22 = b_trees:update("k_22", "v_22_new", B_TREE_04_32_K_21),
    B_TREE_04_32_K_23 = b_trees:update("k_23", "v_23_new", B_TREE_04_32_K_22),
    B_TREE_04_32_K_24 = b_trees:update("k_24", "v_24_new", B_TREE_04_32_K_23),
    B_TREE_04_32_K_25 = b_trees:update("k_25", "v_25_new", B_TREE_04_32_K_24),
    B_TREE_04_32_K_26 = b_trees:update("k_26", "v_26_new", B_TREE_04_32_K_25),
    B_TREE_04_32_K_27 = b_trees:update("k_27", "v_27_new", B_TREE_04_32_K_26),
    B_TREE_04_32_K_28 = b_trees:update("k_28", "v_28_new", B_TREE_04_32_K_27),
    B_TREE_04_32_K_29 = b_trees:update("k_29", "v_29_new", B_TREE_04_32_K_28),
    B_TREE_04_32_K_30 = b_trees:update("k_30", "v_30_new", B_TREE_04_32_K_29),
    B_TREE_04_32_K_31 = b_trees:update("k_31", "v_31_new", B_TREE_04_32_K_30),
    B_TREE_04_32_K_32 = b_trees:update("k_32", "v_32_new", B_TREE_04_32_K_31),

    B_TREE_04_32_K_32_VALUES = b_trees:to_list(B_TREE_04_32_K_32),

    ?assertEqual(B_TREE_04_32_K_32_VALUES, test_generator:generate_key_values_from_update(32, 2)),

    ?assertEqual(11, length(ets:tab2list(StateTarget))),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values - persistence by ets
%%--------------------------------------------------------------------

values_persistence_by_ets_test() ->
    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2, b_trees_test, spawn(fun test_generator:ets_owner/0)),

    ?assertEqual(4, length(ets:tab2list(StateTarget))),

    ?assertEqual(32, length(b_trees:values(B_TREE_06_32))),

    true = ets:delete(StateTarget),

    ok.

%%--------------------------------------------------------------------
%% TEST CASES: values
%%--------------------------------------------------------------------

values_test() ->
    ?assertEqual([], b_trees:values(test_generator:prepare_template_asc(?B_TREE_06_00))),

    ?assertEqual(["v_01"], b_trees:values(test_generator:prepare_template_asc(?B_TREE_06_01))),

    ?assertEqual(2, length(b_trees:values(test_generator:prepare_template_asc(?B_TREE_06_02)))),
    ?assertEqual(5, length(b_trees:values(test_generator:prepare_template_asc(?B_TREE_06_05)))),
    ?assertEqual(9, length(b_trees:values(test_generator:prepare_template_asc(?B_TREE_06_09)))),
    ?assertEqual(16, length(b_trees:values(test_generator:prepare_template_asc(?B_TREE_06_16)))),

    ?assertEqual(32, length(b_trees:values(test_generator:prepare_template_desc(?B_TREE_06_32_DESC)))),

    ok.

