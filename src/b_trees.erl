% -define(NODEBUG, true).

-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/b_trees_templates.hrl").

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2015. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%% =============================================================================
%% B Trees - balanced n-ary trees (B tree & B* tree).
%%
%% Copyright (C) 2016 Walter Weinmann
%%
%% An efficient implementation of Prof. Arne Andersson's General ???
%% Balanced Trees. These have no storage overhead compared to plain
%% unbalanced binary trees, and their performance is in general better
%% than AVL trees.
%% -----------------------------------------------------------------------------
%% Operations:
%%
%% - empty(O): returns empty B tree of order O. Order is defined as 
%%   the maximum number of children nodes a non-leaf node may hold.
%%   The minimum value is 4.
%%
%% - empty(O, b_star): returns empty B* tree of order O. Order is 
%%   defined as the maximum number of children nodes a non-leaf node 
%%   may hold. The minimum value is 4.
%%
%% - height(T): returns the height of the B tree / B* tree T as an integer.
%%   Returns 0 (zero) if the B tree / B* tree T is empty.
%%
%% - insert(X, V, T): inserts key X with value V into B tree / B* tree T; 
%%   returns the new B tree / B* tree. Assumes that the key is *not* 
%%   present in the B tree / B* tree T.
%%
%% - is_empty(T): returns 'true' if T is an empty B tree / B* tree, 
%%   and 'false' otherwise.
%%
%% - lookup(X, T): looks up key X in B tree / B* tree T; returns {value, V}, 
%%   or `none' if the key is not present.
%%
%% - number_key_values(T): returns the number of key / value pairs in the 
%%   B tree / B* tree T as an integer. Returns 0 (zero) if the 
%%   B tree / B* tree T is empty.
%%
%% - size(T): returns the number of nodes in the B tree / B* tree T 
%%   as an integer. Returns 0 (zero) if the B tree / B* tree T is empty.
%%


%% - is_defined(X, T): returns `true' if key X is present in tree T, and
%%   `false' otherwise.
%%
%% - keys(T): returns an ordered list of all keys in tree T.
%%


%% - enter(X, V, T): inserts key X with value V into tree T if the key
%%   is not present in the tree, otherwise updates key X to value V in
%%   T. Returns the new tree.
%%
%% - from_orddict(L): turns an ordered list L of {Key, Value} pairs into
%%   a tree. The list must not contain duplicate keys.
%%
%% - get(X, T): retreives the value stored with key X in tree T. Assumes
%%   that the key is present in the tree.
%%
%% - iterator(T): returns an iterator that can be used for traversing
%%   the entries of tree T; see `next'. The implementation of this is
%%   very efficient; traversing the whole tree using `next' is only
%%   slightly slower than getting the list of all elements using
%%   `to_list' and traversing that. The main advantage of the iterator
%%   approach is that it does not require the complete list of all
%%   elements to be built in memory at one time.
%%
%% - iterator_from(K, T): returns an iterator that can be used for
%%   traversing the entries of tree T with key greater than or
%%   equal to K; see `next'.
%%
%% - largest(T): returns {X, V}, where X is the largest key in tree T,
%%   and V is the value associated with X in T. Assumes that the tree T
%%   is nonempty.
%%
%% - next(S): returns {X, V, S1} where X is the smallest key referred to
%%   by the iterator S, and S1 is the new iterator to be used for
%%   traversing the remaining entries, or the atom `none' if no entries
%%   remain.
%%
%% - smallest(T): returns {X, V}, where X is the smallest key in tree T,
%%   and V is the value associated with X in T. Assumes that the tree T
%%   is nonempty.
%%
%% - to_list(T): returns an ordered list of {Key, Value} pairs for all
%%   keys in tree T.
%%
%% - values(T): returns the list of values for all keys in tree T,
%%   sorted by their corresponding keys. Duplicates are not removed.
%%
%% - update(X, V, T): updates key X to value V in tree T; returns the
%%   new tree. Assumes that the key is present in the tree.
%%


%% - balance(T): rebalances tree T. Note that this is rarely necessary,
%%   but may be motivated when a large number of entries have been
%%   deleted from the tree without further insertions. Rebalancing could
%%   then be forced in order to minimise lookup times, since deletion
%%   only does not rebalance the tree.
%%
%% - delete(X, T): removes key X from tree T; returns new tree. Assumes
%%   that the key is present in the tree.
%%
%% - delete_any(X, T): removes key X from tree T if the key is present
%%   in the tree, otherwise does nothing; returns new tree.
%%
%% - map(F, T): maps the function F(K, V) -> V' to all key-value pairs
%%   of the tree T and returns a new tree T' with the same set of keys
%%   as T and the new set of values V'.
%%
%% - take_largest(T): returns {X, V, T1}, where X is the largest key
%%   in tree T, V is the value associated with X in T, and T1 is the
%%   tree T with key X deleted. Assumes that the tree T is nonempty.
%%
%% - take_smallest(T): returns {X, V, T1}, where X is the smallest key
%%   in tree T, V is the value associated with X in T, and T1 is the
%%   tree T with key X deleted. Assumes that the tree T is nonempty.
%%

-module(b_trees).

-export([
    empty/1,
    empty/2,
    height/1,
    insert/3,
    is_empty/1,
    lookup/2,
    number_key_values/1,
    size/1
]).

-define(BINARY_SEARCH_INSERT, 4).

-ifdef(EUNIT).
-compile([export_all]).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data structure:
%% - {Minimum, Maximum, NumberKeyValues, Tree}, where `Tree' is composed of :
%%   - {KeyNo, IsLeaf, [{Key, Value}], [Tree]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

-type key() :: any().
-type value() :: any().

-type key_value() :: {key(), value()}.
-type key_values() :: [key_value()].

-type tree() :: 'nil' | {pos_integer(), boolean(), key_values(), trees()}.
-type trees() :: [tree()].

-type b_tree() :: {pos_integer(), pos_integer(), non_neg_integer(), tree()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec empty(pos_integer()) -> b_tree().
-spec empty(pos_integer(), atom()) -> b_tree().

empty(Order) when Order > 3 ->
    {Order div 2, Order - 1, 0, nil}.
empty(Order, b_star) when Order > 3 ->
    {Order * 2 div 3, Order - 1, 0, nil}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec height(b_tree()) -> non_neg_integer().

height({_, _, 0, nil}) ->
    0;
height({_, _, _, Tree}) ->
    height_1(Tree, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert(key(), value(), b_tree()) -> b_tree().

insert(Key, Value, {KeyNoMin, KeyNoMax, 0, nil}) ->
    {KeyNoMin, KeyNoMax, 1, {1, true, [{Key, Value}], [nil, nil]}};
insert(Key, Value, {KeyNoMin, KeyNoMax, NumberKeyValues, {KeyNo, _, KeyValues, _} = BTree}) ->
    TreeRepl = case KeyNo == KeyNoMax of
                   true ->
                       KeyNoSplit = case KeyNoMax rem 2 of
                                        1 ->
                                            KeyNoMin + 1;
                                        _ ->
                                            {KeyKeyNoMin, _} = lists:nth(KeyNoMin, KeyValues),
                                            case Key < KeyKeyNoMin of
                                                true ->
                                                    KeyNoMin;
                                                _ ->
                                                    KeyNoMin + 1
                                            end
                                    end,
                       split_tree_root(BTree, KeyNoSplit);
                   _ ->
                       BTree
               end,
    {KeyNoMin, KeyNoMax, NumberKeyValues + 1, insert_into_tree({Key, Value}, TreeRepl, KeyNoMin, KeyNoMax)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_empty(b_tree()) -> boolean().

is_empty({_, _, 0, nil}) ->
    true;
is_empty(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec lookup(key(), b_tree()) -> 'none' | {'value', value()}.

lookup(_Key, {_, _, 0, nil}) ->
    none;
lookup(Key, {_, _, _, Tree}) ->
    lookup_1(Key, Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec number_key_values(b_tree()) -> non_neg_integer().

number_key_values({_, _, NumberKeyValues, _}) ->
    NumberKeyValues.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec size(b_tree()) -> non_neg_integer().

size({_, _, 0, nil}) ->
    0;
size({_, _, _, Tree} = _X) ->
    size_tree(Tree, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec binary_search(key(), key_values(), pos_integer(), pos_integer()) -> {none, pos_integer()} | {any(), pos_integer()}.

binary_search(Key, KeyValues, Lower, Upper) when Lower > Upper ->
    % ?debugFmt("wwe debugging binary_search/4 ===> Start ~n Key: ~p~n KeyValues: ~p~n Lower: ~p~n Upper: ~p~n", [Key, KeyValues, Lower, Upper]),
    TreeNo = case Lower > length(KeyValues) of
                 true -> Upper;
                 _ -> Lower
             end,
    {KeyLast, _} = lists:nth(TreeNo, KeyValues),
    case Key < KeyLast of
        true -> {none, TreeNo};
        _ -> {none, TreeNo + 1}
    end;
binary_search(Key, KeyValues, Lower, Upper) ->
    % ?debugFmt("wwe debugging binary_search/4 ===> Start ~n Key: ~p~n KeyValues: ~p~n Lower: ~p~n Upper: ~p~n", [Key, KeyValues, Lower, Upper]),
    Mid = (Upper + Lower) div 2,
    {MidKey, MidValue} = lists:nth(Mid, KeyValues),
    if
        Key > MidKey ->
            binary_search(Key, KeyValues, Mid + 1, Upper);
        Key < MidKey ->
            binary_search(Key, KeyValues, Lower, Mid - 1);
        true ->
            {MidValue, Mid}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec height_1(tree(), pos_integer()) -> pos_integer().

height_1({_, true, _, _}, Number) ->
    Number;
height_1({_, _, _, [Tree | _]}, Number) ->
    height_1(Tree, Number + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert_into_key_values(key_value(), key_values(), key_values()) -> key_values().

insert_into_key_values(KeyValue, [] = _KeyValues, KeyValuesAcc) ->
    KeyValuesAcc ++ [KeyValue];
insert_into_key_values({Key, _} = KeyValue, [{KeyCurr, _} = KeyValueCurr | Tail] = _KeyValues, KeyValuesAcc) when Key > KeyCurr ->
    insert_into_key_values(KeyValue, Tail, KeyValuesAcc ++ [KeyValueCurr]);
insert_into_key_values({Key, _} = KeyValue, [{KeyCurr, _} | _] = KeyValues, KeyValuesAcc) when Key < KeyCurr ->
    KeyValuesAcc ++ [KeyValue] ++ KeyValues;
insert_into_key_values({Key, _Value} = _KeyValue, _KeyValues, _KeyValuesAcc) ->
    erlang:error({key_exists, Key}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert_into_tree(key_value(), tree(), pos_integer(), pos_integer()) -> tree().

insert_into_tree(KeyValue, {KeyNo, true = IsLeaf, KeyValues, Trees} = _Tree, _KeyNoMin, _KeyNoMax) ->
    % ?debugFmt("wwe debugging insert_into_tree/4 ===> Start ~n KeyValue: ~p~n Tree: ~p~n KeyNoMin: ~p~n KeyNoMax: ~p~n", [KeyValue, _Tree, _KeyNoMin, _KeyNoMax]),
    TreeOut = {KeyNo + 1, IsLeaf, insert_into_key_values(KeyValue, KeyValues, []), Trees ++ [nil]},
    TreeOut;
insert_into_tree({Key, _} = KeyValue, {KeyNo, false = IsLeaf, KeyValues, Trees} = _Tree, KeyNoMin, KeyNoMax) ->
    % ?debugFmt("wwe debugging insert_into_tree/4 ===> Start ~n KeyValue: ~p~n Tree: ~p~n KeyNoMin: ~p~n KeyNoMax: ~p~n", [KeyValue, _Tree, KeyNoMin, KeyNoMax]),
    {ValueFound, TreeUpperPos} = search(Key, KeyValues, KeyNo, ?BINARY_SEARCH_INSERT),
    case ValueFound of
        none ->
            TreeUpper = lists:nth(TreeUpperPos, Trees),
            {KeyNoUpper, _, KeyValuesUpper, _} = TreeUpper,
            {TreeUpperRepl, KeyValuesRepl, TreesRepl, TreeUpperPosRepl} = case KeyNoUpper == KeyNoMax of
                                                                              true ->
                                                                                  KeyNoSplit = case KeyNoMax rem 2 of
                                                                                                   1 ->
                                                                                                       KeyNoMin + 1;
                                                                                                   _ ->
                                                                                                       {KeyKeyNoMin, _} = lists:nth(KeyNoMin, KeyValuesUpper),
                                                                                                       case Key < KeyKeyNoMin of
                                                                                                           true ->
                                                                                                               KeyNoMin;
                                                                                                           _ ->
                                                                                                               KeyNoMin + 1
                                                                                                       end
                                                                                               end,
                                                                                  {KeyValuesSplit, TreesSplit} = split_tree_non_root(TreeUpper, KeyNoSplit, KeyValues, Trees, TreeUpperPos),
                                                                                  {none, TreeUpperPosSplit} = search(Key, KeyValuesSplit, length(KeyValuesSplit), ?BINARY_SEARCH_INSERT),
                                                                                  {lists:nth(TreeUpperPosSplit, TreesSplit), KeyValuesSplit, TreesSplit, TreeUpperPosSplit};
                                                                              _ ->
                                                                                  {TreeUpper, KeyValues, Trees, TreeUpperPos}
                                                                          end,
            {
                length(KeyValuesRepl),
                IsLeaf,
                KeyValuesRepl,
                    lists:sublist(TreesRepl, 1, TreeUpperPosRepl - 1) ++
                    [insert_into_tree(KeyValue, TreeUpperRepl, KeyNoMin, KeyNoMax)] ++
                    lists:sublist(TreesRepl, TreeUpperPosRepl + 1, length(TreesRepl))
            };
        _ ->
            erlang:error({key_exists, Key})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The term order is an arithmetic total order, so we should not
%% test exact equality for the keys. (If we do, then it becomes
%% possible that neither `>', `<', nor `=:=' matches.) Testing '<'
%% and '>' first is statistically better than testing for
%% equality, and also allows us to skip the test completely in the
%% remaining case.

-spec lookup_1(key(), b_tree()) -> 'none' | {'value', value()}.

lookup_1(Key, {_, IsLeaf, KeyValues, ChildTrees}) ->
    {Value, Pos} = sequential_search(Key, KeyValues, 0),
    case Value == none of
        true -> case IsLeaf of
                    true -> none;
                    _ -> ChildTree = lists:nth(Pos, ChildTrees),
                        lookup_1(Key, ChildTree)
                end;
        _ -> {value, Value}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec search(key(), key_values(), pos_integer(), pos_integer()) -> {'none', pos_integer()} | {value(), pos_integer()}.

search(Key, KeyValues, Upper, Limit) when Upper =< Limit ->
    sequential_search(Key, KeyValues, 0);
search(Key, KeyValues, Upper, _Limit) ->
    binary_search(Key, KeyValues, 1, Upper).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sequential_search(key(), key_values(), non_neg_integer()) -> {'none', pos_integer()} | {value(), pos_integer()}.

sequential_search(_, [], Pos) ->
    {none, Pos + 1};
sequential_search(Key, [{KeyLast, ValueLast} | Tail], Pos) ->
    PosNew = Pos + 1,
    case Key < KeyLast of
        true -> {none, PosNew};
        _ -> case Key > KeyLast of
                 true -> sequential_search(Key, Tail, PosNew);
                 _ -> {ValueLast, PosNew}
             end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec size_tree(tree(), pos_integer()) -> pos_integer().

size_tree({_, true, _, _} = _X, Number) ->
    % ?debugFmt("wwe debugging size_tree/2 ===> Start ~n Tree: ~p~n Number: ~p~n", [_X, Number]),
    Number + 1;
size_tree({_, _, _, Trees} = _X, Number) ->
    % ?debugFmt("wwe debugging size_tree/2 ===> Start ~n Tree: ~p~n Number: ~p~n", [_X, Number]),
    size_trees(Trees, Number + 1).

-spec size_trees([tree(), ...], pos_integer()) -> pos_integer().

%%size_trees([] = _X, Number) ->
%%    ?debugFmt("wwe debugging size_trees/2 ===> Start ~n Trees: ~p~n Number: ~p~n", [_X, Number]),
%%    Number;
size_trees([] = _X, Number) ->
    % ?debugFmt("wwe debugging size_trees/2 ===> Start ~n Trees: ~p~n Number: ~p~n", [_X, Number]),
    Number;
size_trees([Tree | Tail] = _X, Number) ->
    % ?debugFmt("wwe debugging size_trees/2 ===> Start ~n Trees: ~p~n Number: ~p~n", [_X, Number]),
    size_trees(Tail, size_tree(Tree, Number)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec split_tree_non_root(tree(), pos_integer(), key_values(), trees(), pos_integer()) -> {key_values(), trees()}.

split_tree_non_root({KeyNo, IsLeaf, KeyValues, Trees} = _Tree, KeyNoSplit, KeyValuesLower, TreesLower, TreeNo) ->
    % ?debugFmt("wwe debugging split_tree_non_root/5 ===> Start ~n Tree: ~p~n KeyNoSplit: ~p~n KeyValuesLower: ~p~n TreesLower: ~p~n TreeNo: ~p~n", [_Tree, KeyNoSplit, KeyValuesLower, TreesLower, TreeNo]),
    {insert_into_key_values(lists:nth(KeyNoSplit, KeyValues), KeyValuesLower, []),
            lists:sublist(TreesLower, 1, TreeNo - 1) ++
            [
                {
                    KeyNoSplit - 1, IsLeaf,
                    lists:sublist(KeyValues, 1, KeyNoSplit - 1),
                    lists:sublist(Trees, 1, KeyNoSplit)
                },
                {
                    KeyNo - KeyNoSplit, IsLeaf,
                    lists:sublist(KeyValues, KeyNoSplit + 1, KeyNo),
                    lists:sublist(Trees, KeyNoSplit + 1, KeyNo)}
            ] ++ lists:sublist(TreesLower, TreeNo + 1, length(Trees))}.

-spec split_tree_root(tree(), pos_integer()) -> tree().

split_tree_root({KeyNo, IsLeaf, KeyValues, Trees} = _Tree, KeyNoSplit) ->
    % ?debugFmt("wwe debugging split_tree_root/2 ===> Start ~n Tree: ~p~n KeyNoSplit: ~p~n", [_Tree, KeyNoSplit]),
    {
        1, false,
        [lists:nth(KeyNoSplit, KeyValues)],
        [
            {
                KeyNoSplit - 1, IsLeaf,
                lists:sublist(KeyValues, 1, KeyNoSplit - 1),
                lists:sublist(Trees, 1, KeyNoSplit)
            },
            {
                KeyNo - KeyNoSplit, IsLeaf,
                lists:sublist(KeyValues, KeyNoSplit + 1, KeyNo),
                lists:sublist(Trees, KeyNoSplit + 1, KeyNo)
            }
        ]
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%direct_test() ->
%%    ?debugFmt("wwe debugging insert_insert_simple_split_test/0 =====================================> B-tree_04_00~n ~p~n", [b_trees:empty(4)]),
%%
%%    ?debugFmt("wwe debugging insert_insert_simple_split_test/0 =====================================> B-tree_05_01~n ~p~n", [test_generator:generate_b_tree_from_number(5, 01, 2)]),
%%
%%    ok.
