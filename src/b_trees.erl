%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% B-trees - balanced n-ary trees.
%%
%% Copyright (C) 2016 Walter Weinmann
%%
%% An efficient implementation of B-trees. The definition of B-trees and its
%% components corresponds to Knuth's definitions in Volume 3 of "The Art of
%% Computer Programming": The order O of a b-tree is an integer expressing
%% the maximum number of subtrees (sons) in a node and the height of a b-tree is
%% its maximum level, i.e. the length of the longest path from the root node to
%% a leaf node. The minimum order of a b-tree is 4.
%% -----------------------------------------------------------------------------
%% Operations:
%%
%% - copy(T1, T2): copies tree T1 to an empty tree T2. Both trees may be either
%%   of type b-tree or binary tree (gb_trees). Returns a new tree of the same
%%   type as tree T2.
%%
%% - delete(K, B): removes key K from b-tree B; returns a new b-tree B'. Assumes
%%   that the key is present in the b-tree.
%%
%% - delete_any(K, B): removes key K from b-tree B if the key is present in the
%%   b-tree, otherwise does nothing; returns a new b-tree B'.
%%
%% - empty(O): returns a new empty b-tree of order O. Order is defined as the
%%   maximum number of children nodes a non-leaf node may hold. The minimum
%%   value is 4. The sort order of the key values is ascending.
%%
%% - enter(K, V, B): inserts key K with value V into b-tree B if the key is not
%%   present in the b-tree, otherwise updates key K to value V in B. Returns a
%%   new b-tree.
%%
%% - from_dict(B, L): turns a list L of {Key, Value} pairs into a b-tree. B must
%%   be an empty b-tree. The list must not contain duplicate keys.
%%
%% - get(K, B): retrieves the value stored with key K in b-tree B. Assumes that
%%   the key is present in the b-tree.
%%
%% - height(B): returns the height of the b-tree B as an integer. Assumes that
%%   the b-tree B is non-empty.
%%
%% - insert(K, V, B): inserts key K with value V into b-tree B; returns a new
%%   b-tree. Assumes that the key K is *not* present in the b-tree B.
%%
%% - is_defined(K, B): returns `true' if key K is present in b-tree B, and
%%   `false' otherwise.
%%
%% - is_empty(B): returns 'true' if B is an empty b-tree, and 'false' otherwise.
%%
%% - iterator(B): returns an iterator that can be used for traversing the
%%   entries of b-tree B; see `next'. The implementation of this is very
%%   efficient; traversing the whole b-tree using `next' is only slightly slower
%%   than getting the list of all key-value pairs using `to_list' and traversing 
%%   that. The main advantage of the iterator approach is that it does not require 
%%   the complete list of all key-value pairs to be built in memory at one time.
%%
%% - iterator_from(K, B): Returns an iterator that can be used for traversing the
%%   entries of b-tree B; see next/1. The difference, as compared to the iterator
%%   returned by iterator/1, is that the first key greater than or equal to Key K
%%   is returned.
%%
%% - keys(B): returns an ordered list of all keys in b-tree B.
%%
%% - largest(B): returns tuple {K, V}, where K is the largest key in b-tree B,
%%   and V is the value associated with K in B. Assumes that the b-tree B is
%%   non-empty.
%%
%% - lookup(K, B): looks up key K in b-tree B; returns {value, V}, or `none' if
%%   the key K is not present.
%%
%% - map(F, B): maps the function F(K, V) -> V' to all key-value pairs of the
%%   b-tree B and returns a new b-tree B' with the same set of keys as B and
%%   the new set of values V'.
%%
%% - next(I): returns {K, V, I1} where K is the smallest key referred to by
%%   the iterator I, and I1 is the new iterator to be used for traversing the
%%   remaining entries, or the atom `none' if no entries remain.
%%
%% - set_parameter(B, P, V): sets the parameter P to value V in b-tree B;
%%   returns a new b-tree B'.
%%
%% - size_key_values(B): returns the number of key-value pairs in the
%%   b-tree B as an integer. Returns 0 (zero) if the b-tree B is empty.
%%
%% - size_nodes(B): returns the number of total nodes and the number of 
%%   leaf nodes in the b-tree B as a tuple of two integers. Returns {0, 0} 
%%   (zero) if b-tree B is empty.
%%
%% - smallest(B): returns tuple {K, V}, where K is the smallest key in b-tree B,
%%   and V is the value associated with K in B. Assumes that the b-tree B is
%%   non-empty.
%%
%% - sort_ascending(K1, K2): returns the atom 'greater' if K1 > K2, the atom 
%%   'less' if K1 < K2 and otherwise the atom 'equal'.
%%
%% - sort_descending(K1, K2): returns the atom 'greater' if K1 < K2, the atom 
%%   'less' if K1 > K2 and otherwise the atom 'equal'.
%%
%% - take(K, B): removes the key-value pair with key K from b-tree B; returns a
%%   new b-tree B' without the removed key-value pair . Assumes that the key is 
%%   present in the b-tree B.
%%
%% - take_any(K, B): removes the key-value pair with key K from b-tree B and 
%%   returns a new b-tree B' if the key is present; otherwise does nothing and 
%%   returns the atom 'error'.
%%
%% - take_largest(B): returns {K, V, B'}, where K is the largest key in b-tree
%%   B, V is the value associated with K in B, and B' is the b-tree B with key
%%   K deleted. Assumes that b-tree B is non-empty.
%%
%% - take_smallest(B): returns {K, V, B'}, where K is the smallest key in
%%   b-tree B, V is the value associated with K in B, and B' is the b-tree B
%%   with key K deleted. Assumes that b-tree B is non-empty.
%%
%% - to_list(B): returns an ordered list of {Key, Value} pairs for all keys in
%%   b-tree B.
%%
%% - update(K, V, B): updates key K to value V in b-tree B; returns a new
%%   b-tree B'. Assumes that the key is present in the b-tree.
%%
%% - values(B): returns the list of values for all keys in b-tree B, sorted by
%%   their corresponding keys. Duplicates are not removed.

-module(b_trees).

-export([
    copy/2,
    delete/2,
    delete_any/2,
    empty/1,
    enter/3,
    from_dict/2,
    get/2,
    height/1,
    insert/3,
    is_defined/2,
    is_empty/1,
    iterator/1,
    iterator_from/2,
    keys/1,
    largest/1,
    lookup/2,
    map/2,
    next/1,
    size_key_values/1,
    size_nodes/1,
    set_parameter/3,
    smallest/1,
    sort_ascending/2,
    sort_descending/2,
    take/2,
    take_any/2,
    take_smallest/1,
    take_largest/1,
    to_list/1,
    update/3,
    values/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data structure:
%% - {MinimumSubtrees, MaximumKeys, SizeKeyValues, SortFunction, State, Tree},
%%   where `Tree' is composed of :
%%   - {KeyNo, SubtreeNo, [{Key, Value}], [Tree]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

-export_type([b_tree/0, iter/0]).

-opaque b_tree() :: {MinimumSubtrees :: pos_integer(), MaximumKeys :: pos_integer(), SizeKeyValues :: non_neg_integer(), sort_function(), state(), subtree()}.

-type delete_function() :: fun((state_target(), 'delete', subtrees_key()) -> 'ok').

-type insert_function() :: fun((state_target(), 'insert', subtrees()) -> subtrees_key()).

-opaque iter() :: [{key_values(), subtrees(), state()}].

-type key() :: any().
-type keys() :: [key()].

-type key_value() :: {key(), value()}.
-type key_values() :: [key_value()].

-type lookup_function() :: fun((state_target(), 'lookup', subtrees_key()) -> subtrees()).

-type map_function() :: fun((key(), value()) -> value()).

-type sort_function() :: fun((key(), key()) -> sort_result()).
-type sort_result() :: 'less' | 'equal' | 'greater'.

-type state() :: 'nil'
| {state_target(), delete_function(), insert_function(), lookup_function()}.
-type state_target() :: any().

-type subtree() :: 'nil'
| {KeyNo :: pos_integer(), SubtreeNo :: pos_integer(), key_values(), []}
| {KeyNo :: pos_integer(), SubtreeNo :: pos_integer(), key_values(), subtrees()}.
-type subtrees() :: subtrees_key()
| [subtree()].
-type subtrees_key() :: integer().

-type value() :: any().
-type values() :: [value()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec copy(b_tree(), b_tree()) -> b_tree();
    (b_tree(), gb_trees:tree()) -> gb_trees:tree();
    (gb_trees:tree(), b_tree()) -> b_tree().

% Copy tree to b-tree.
copy(Tree1, {MinimumSubtrees2, _, 0, _, State2, nil} = Tree2) ->
    case Tree1 of
        {_, _, 0, _, _, nil} ->
            Tree2;
        {0, nil} ->
            Tree2;
        {MinimumSubtrees1, _, _, _, State1, _} ->
            case MinimumSubtrees1 == MinimumSubtrees2 andalso State1 == State2 of
                true ->
                    Tree1;
                _ ->
                    copy_b_tree_to_b_tree(Tree1, Tree2)
            end;
        _ ->
            copy_binary_tree_to_b_tree(Tree1, Tree2)
    end;
% Copy tree to binary tree.
copy(Tree1, Tree2) ->
    case Tree1 of
        {_, _, 0, _, _, nil} ->
            Tree2;
        {_, _, _, _, _, _} ->
            copy_b_tree_to_binary_tree(Tree1, Tree2);
        _ ->
            Tree1
    end.

-spec copy_b_tree_to_b_tree(b_tree(), b_tree()) -> b_tree().

% Copy b-tree to b-tree.
copy_b_tree_to_b_tree(BTree1, BTree2) ->
    copy_b_tree_to_b_tree_iterator(next(iterator(BTree1)), BTree2).

-spec copy_b_tree_to_b_tree_iterator('none' | iter(), b_tree()) -> b_tree().

copy_b_tree_to_b_tree_iterator(none, BTree) ->
    BTree;
copy_b_tree_to_b_tree_iterator({Key, Value, Iterator}, BTree) ->
    copy_b_tree_to_b_tree_iterator(next(Iterator), insert(Key, Value, BTree)).

-spec copy_b_tree_to_binary_tree(b_tree(), gb_trees:tree()) -> gb_trees:tree().

% Copy b-tree to binary tree.
copy_b_tree_to_binary_tree(BTree, GBTree) ->
    copy_b_tree_to_binary_tree_iterator(next(iterator(BTree)), GBTree).

-spec copy_b_tree_to_binary_tree_iterator('none' | {key(), value(), iter()}, gb_trees:tree()) -> gb_trees:tree().

copy_b_tree_to_binary_tree_iterator(none, GBTree) ->
    GBTree;
copy_b_tree_to_binary_tree_iterator({Key, Value, Iterator}, GBTree) ->
    copy_b_tree_to_binary_tree_iterator(next(Iterator), gb_trees:insert(Key, Value, GBTree)).

-spec copy_binary_tree_to_b_tree(gb_trees:tree(), b_tree()) -> b_tree().

% Copy binary tree to b-tree.
copy_binary_tree_to_b_tree(GBTree, BTree) ->
    copy_binary_tree_to_b_tree_iterator(gb_trees:next(gb_trees:iterator(GBTree)), BTree).

-spec copy_binary_tree_to_b_tree_iterator('none' | {key(), value(), gb_trees:iter()}, b_tree()) -> b_tree().

copy_binary_tree_to_b_tree_iterator(none, BTree) ->
    BTree;
copy_binary_tree_to_b_tree_iterator({Key, Value, Iterator}, BTree) ->
    copy_binary_tree_to_b_tree_iterator(gb_trees:next(Iterator), insert(Key, Value, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% delete. Assumes that key is present.

-spec delete(key(), b_tree()) -> b_tree().

% Empty tree.
delete(Key, {_, _, 0, _, _, nil}) ->
    erlang:error({key_not_found, Key});
% Root node is leaf node.
delete(Key, {SubtreeNoMin, KeyNoMax, SizeKeyValues, SortFunction, State, {KeyNo, 0, KeyValues, []}}) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            erlang:error({key_not_found, Key});
        _ ->
            {SubtreeNoMin, KeyNoMax, SizeKeyValues - 1, SortFunction, State, case KeyNo of
                                                                                 1 ->
                                                                                     nil;
                                                                                 _ ->
                                                                                     {
                                                                                         KeyNo - 1,
                                                                                         0,
                                                                                         lists:append([
                                                                                             lists:sublist(KeyValues, 1, KeyPos - 1),
                                                                                             lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                                                                                         ]),
                                                                                         []
                                                                                     }
                                                                             end}
    end;
delete(Key, {SubtreeNoMin, KeyNoMax, SizeKeyValues, SortFunction, State, Tree}) ->
    {SubtreeNoMin, KeyNoMax, SizeKeyValues - 1, SortFunction, State, delete_1(Key, Tree, SubtreeNoMin, KeyNoMax, SortFunction, State)}.

-spec combine(subtree(), subtree(), state()) -> subtree().

combine({LeftKeyNo, 0, LeftKeyValues, []}, {RightKeyNo, 0, RightKeyValues, []}, _) ->
    {
        LeftKeyNo + RightKeyNo,
        0,
        lists:append([
            LeftKeyValues,
            RightKeyValues
        ]),
        []
    };
combine({LeftKeyNo, LeftSubtreeNo, LeftKeyValues, LeftSubtrees}, {RightKeyNo, RightSubtreeNo, RightKeyValues, RightSubtrees}, nil) ->
    {
        LeftKeyNo + RightKeyNo,
        LeftSubtreeNo + RightSubtreeNo - 1,
        lists:append([
            LeftKeyValues,
            RightKeyValues
        ]),
        lists:append([
            lists:sublist(LeftSubtrees, 1, LeftKeyNo),
            [combine(lists:nth(LeftSubtreeNo, LeftSubtrees), lists:nth(1, RightSubtrees), nil)],
            lists:sublist(RightSubtrees, 2, RightSubtreeNo)
        ])
    };
combine({LeftKeyNo, LeftSubtreeNo, LeftKeyValues, LeftSubtreesKey} = _TreeLeft, {RightKeyNo, RightSubtreeNo, RightKeyValues, RightSubtreesKey} = _TreeRight, {StateTarget, DeleteFunction, InsertFunction, LookupFunction} = State) ->
    LeftSubtrees = LookupFunction(StateTarget, lookup, LeftSubtreesKey),
    true = DeleteFunction(StateTarget, delete, LeftSubtreesKey),
    RightSubtrees = LookupFunction(StateTarget, lookup, RightSubtreesKey),
    true = DeleteFunction(StateTarget, delete, RightSubtreesKey),
    {
        LeftKeyNo + RightKeyNo,
        LeftSubtreeNo + RightSubtreeNo - 1,
        lists:append([
            LeftKeyValues,
            RightKeyValues
        ]),
        InsertFunction(
            StateTarget,
            insert,
            lists:append([
                lists:sublist(LeftSubtrees, 1, LeftKeyNo),
                [combine(lists:nth(LeftSubtreeNo, LeftSubtrees), lists:nth(1, RightSubtrees), State)],
                lists:sublist(RightSubtrees, 2, RightSubtreeNo)
            ]))
    }.

-spec delete_1(key(), subtree(), pos_integer(), pos_integer(), sort_function(), state()) -> subtree().

% Leaf node.
delete_1(Key, {KeyNo, 0, KeyValues, []}, _, _, SortFunction, _) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            erlang:error({key_not_found, Key});
        _ ->
            % CLRS: case 1 leaf node
            {
                KeyNo - 1,
                0,
                lists:append([
                    lists:sublist(KeyValues, 1, KeyPos - 1),
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                ]),
                []
            }
    end;
delete_1(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees} = _Tree, SubtreeNoMin, KeyNoMax, SortFunction, State) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            delete_1_3(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, KeyNoMax, KeyPos, SortFunction, State);
        _ ->
            delete_1_2(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, KeyNoMax, KeyPos, State)
    end.

-spec delete_1_2(key(), subtree(), pos_integer(), pos_integer(), pos_integer(), state()) -> subtree().

delete_1_2(_, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, _, KeyPos, nil) ->
    {KeyNoXC, SubtreeNoXC, KeyValuesXC, SubtreesXC} = lists:nth(KeyPos, Subtrees),
    {KeyNoXCRight, SubtreeNoXCRight, KeyValuesXCRight, SubtreesXCRight} = lists:nth(KeyPos + 1, Subtrees),
    case KeyNoXC >= SubtreeNoMin of
        true ->
            % CLRS: case 2a
            {
                KeyNo,
                SubtreeNo,
                lists:append([
                    lists:sublist(KeyValues, 1, KeyPos - 1),
                    [lists:nth(KeyNoXC, KeyValuesXC)],
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                ]),
                lists:append([
                    lists:sublist(Subtrees, 1, KeyPos - 1),
                    [
                        {
                            KeyNoXC - 1, case SubtreeNoXC of
                                             0 ->
                                                 0;
                                             _ ->
                                                 SubtreeNoXC - 1
                                         end, lists:sublist(KeyValuesXC, 1, KeyNoXC - 1), case SubtreeNoXC of
                                                                                              0 ->
                                                                                                  [];
                                                                                              _ ->
                                                                                                  lists:sublist(SubtreesXC, 1, KeyNoXC)
                                                                                          end
                        },
                        {
                            KeyNoXCRight,
                            SubtreeNoXCRight,
                            KeyValuesXCRight,
                            case SubtreeNoXC of
                                0 ->
                                    [];
                                _ ->
                                    lists:append([
                                        [combine(lists:nth(SubtreeNoXC, SubtreesXC), lists:nth(1, SubtreesXCRight), nil)],
                                        lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                    ])
                            end
                        }
                    ],
                    lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                ])
            };
        _ ->
            case KeyNoXCRight >= SubtreeNoMin of
                % CLRS: case 2b
                true ->
                    {
                        KeyNo,
                        SubtreeNo,
                        lists:append([
                            lists:sublist(KeyValues, 1, KeyPos - 1),
                            [lists:nth(1, KeyValuesXCRight)],
                            lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                        ]),
                        lists:append([
                            lists:sublist(Subtrees, 1, KeyPos - 1),
                            [
                                {
                                    KeyNoXC,
                                    SubtreeNoXC,
                                    KeyValuesXC,
                                    case SubtreeNoXC of
                                        0 ->
                                            [];
                                        _ ->
                                            lists:append([
                                                lists:sublist(SubtreesXC, 1, KeyNoXC),
                                                [combine(lists:nth(SubtreeNoXC, SubtreesXC), lists:nth(1, SubtreesXCRight), nil)]
                                            ])
                                    end
                                },
                                {
                                    KeyNoXCRight - 1, case SubtreeNoXCRight of
                                                          0 ->
                                                              0;
                                                          _ ->
                                                              SubtreeNoXCRight - 1
                                                      end, lists:sublist(KeyValuesXCRight, 2, KeyNoXCRight), case SubtreeNoXCRight of
                                                                                                                 0 ->
                                                                                                                     [];
                                                                                                                 _ ->
                                                                                                                     lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                                                                                             end
                                }
                            ],
                            lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                        ])
                    };
                % CLRS: case 2c
                _ ->
                    {
                        LeftTree,
                        RightTree
                    } = case SubtreeNoXC of
                            0 ->
                                {
                                    {0, 0, [], []},
                                    {0, 0, [], []}
                                };
                            _ ->
                                {
                                    lists:nth(SubtreeNoXC, SubtreesXC),
                                    lists:nth(1, SubtreesXCRight)
                                }
                        end,
                    case KeyNo of
                        1 ->
                            {
                                KeyNoXC + KeyNoXCRight,
                                case SubtreeNoXCRight of
                                    0 ->
                                        0;
                                    _ ->
                                        SubtreeNoXC + SubtreeNoXCRight - 1
                                end,
                                lists:append([
                                    KeyValuesXC,
                                    KeyValuesXCRight
                                ]),
                                case SubtreeNoXCRight of
                                    0 ->
                                        [];
                                    _ ->
                                        lists:append([
                                            lists:sublist(SubtreesXC, 1, KeyNoXC),
                                            [combine(LeftTree, RightTree, nil)],
                                            lists:sublist(SubtreesXCRight, 2, KeyNoXCRight)
                                        ])
                                end
                            };
                        _ ->
                            {
                                KeyNo - 1,
                                SubtreeNo - 1,
                                lists:append([
                                    lists:sublist(KeyValues, 1, KeyPos - 1),
                                    lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                                ]),
                                lists:append([
                                    lists:sublist(Subtrees, 1, KeyPos - 1),
                                    [{
                                        KeyNoXC + KeyNoXCRight, case SubtreeNoXCRight of
                                                                    0 ->
                                                                        0;
                                                                    _ ->
                                                                        SubtreeNoXC + SubtreeNoXCRight - 1
                                                                end, lists:append([
                                            KeyValuesXC,
                                            KeyValuesXCRight
                                        ]),
                                        case SubtreeNoXCRight of
                                            0 ->
                                                [];
                                            _ ->
                                                lists:append([
                                                    lists:sublist(SubtreesXC, 1, KeyNoXC),
                                                    [combine(LeftTree, RightTree, nil)],
                                                    lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                                ])
                                        end
                                    }],
                                    lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                                ])
                            }
                    end
            end
    end;
delete_1_2(_Key, {KeyNo, SubtreeNo, KeyValues, SubtreesKey} = _Tree, SubtreeNoMin, _, KeyPos, {StateTarget, DeleteFunction, InsertFunction, LookupFunction} = State) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    true = DeleteFunction(StateTarget, delete, SubtreesKey),
    {KeyNoXC, SubtreeNoXC, KeyValuesXC, SubtreesKeyXC} = lists:nth(KeyPos, Subtrees),
    SubtreesXC = LookupFunction(StateTarget, lookup, SubtreesKeyXC),
    true = DeleteFunction(StateTarget, delete, SubtreesKeyXC),
    {KeyNoXCRight, SubtreeNoXCRight, KeyValuesXCRight, SubtreesKeyXCRight} = lists:nth(KeyPos + 1, Subtrees),
    SubtreesXCRight = LookupFunction(StateTarget, lookup, SubtreesKeyXCRight),
    true = DeleteFunction(StateTarget, delete, SubtreesKeyXCRight),
    case KeyNoXC >= SubtreeNoMin of
        true ->
            % CLRS: case 2a
            {
                KeyNo,
                SubtreeNo,
                lists:append([
                    lists:sublist(KeyValues, 1, KeyPos - 1),
                    [lists:nth(KeyNoXC, KeyValuesXC)],
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                ]),
                InsertFunction(
                    StateTarget,
                    insert,
                    lists:append([
                        lists:sublist(Subtrees, 1, KeyPos - 1),
                        [{
                            KeyNoXC - 1, case SubtreeNoXC of
                                             0 ->
                                                 0;
                                             _ ->
                                                 SubtreeNoXC - 1
                                         end, lists:sublist(KeyValuesXC, 1, KeyNoXC - 1), case SubtreeNoXC of
                                                                                              0 ->
                                                                                                  [];
                                                                                              _ ->
                                                                                                  lists:sublist(SubtreesXC, 1, KeyNoXC)
                                                                                          end
                        },
                            {
                                KeyNoXCRight,
                                SubtreeNoXCRight,
                                KeyValuesXCRight,
                                case SubtreeNoXC of
                                    0 ->
                                        [];
                                    _ ->
                                        InsertFunction(
                                            StateTarget,
                                            insert,
                                            lists:append([
                                                [combine(lists:nth(SubtreeNoXC, SubtreesXC), lists:nth(1, SubtreesXCRight), State)],
                                                lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                            ]))
                                end
                            }],
                        lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                    ]))
            };
        _ ->
            case KeyNoXCRight >= SubtreeNoMin of
                % CLRS: case 2b
                true ->
                    {
                        KeyNo,
                        SubtreeNo,
                        lists:append([
                            lists:sublist(KeyValues, 1, KeyPos - 1),
                            [lists:nth(1, KeyValuesXCRight)],
                            lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                        ]),
                        InsertFunction(
                            StateTarget,
                            insert,
                            lists:append([
                                lists:sublist(Subtrees, 1, KeyPos - 1),
                                [{
                                    KeyNoXC,
                                    SubtreeNoXC,
                                    KeyValuesXC,
                                    case SubtreeNoXC of
                                        0 ->
                                            [];
                                        _ ->
                                            InsertFunction(
                                                StateTarget,
                                                insert,
                                                lists:append([
                                                    lists:sublist(SubtreesXC, 1, KeyNoXC),
                                                    [combine(lists:nth(SubtreeNoXC, SubtreesXC), lists:nth(1, SubtreesXCRight), State)]
                                                ]))
                                    end
                                },
                                    {
                                        KeyNoXCRight - 1, case SubtreeNoXCRight of
                                                              0 ->
                                                                  0;
                                                              _ ->
                                                                  SubtreeNoXCRight - 1
                                                          end, lists:sublist(KeyValuesXCRight, 2, KeyNoXCRight), case SubtreeNoXCRight of
                                                                                                                     0 ->
                                                                                                                         [];
                                                                                                                     _ ->
                                                                                                                         InsertFunction(
                                                                                                                             StateTarget,
                                                                                                                             insert,
                                                                                                                             lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight))
                                                                                                                 end
                                    }
                                ],
                                lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                            ]))
                    };
                % CLRS: case 2c
                _ ->
                    {
                        LeftTree,
                        RightTree
                    } = case SubtreeNoXC of
                            0 ->
                                {
                                    {0, 0, [], []},
                                    {0, 0, [], []}
                                };
                            _ ->
                                {
                                    lists:nth(SubtreeNoXC, SubtreesXC),
                                    lists:nth(1, SubtreesXCRight)
                                }
                        end,
                    case KeyNo of
                        1 ->
                            {
                                KeyNoXC + KeyNoXCRight,
                                case SubtreeNoXCRight of
                                    0 ->
                                        0;
                                    _ ->
                                        SubtreeNoXC + SubtreeNoXCRight - 1
                                end,
                                lists:append([
                                    KeyValuesXC,
                                    KeyValuesXCRight
                                ]),
                                case SubtreeNoXCRight of
                                    0 ->
                                        [];
                                    _ ->
                                        InsertFunction(
                                            StateTarget,
                                            insert,
                                            lists:append([
                                                lists:sublist(SubtreesXC, 1, KeyNoXC),
                                                [combine(LeftTree, RightTree, State)],
                                                lists:sublist(SubtreesXCRight, 2, KeyNoXCRight)
                                            ]))
                                end
                            };
                        _ ->
                            {
                                KeyNo - 1,
                                SubtreeNo - 1,
                                lists:append([
                                    lists:sublist(KeyValues, 1, KeyPos - 1),
                                    lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                                ]),
                                InsertFunction(
                                    StateTarget,
                                    insert,
                                    lists:append([
                                        lists:sublist(Subtrees, 1, KeyPos - 1),
                                        [{
                                            KeyNoXC + KeyNoXCRight, case SubtreeNoXCRight of
                                                                        0 ->
                                                                            0;
                                                                        _ ->
                                                                            SubtreeNoXC + SubtreeNoXCRight - 1
                                                                    end, lists:append([KeyValuesXC,
                                                KeyValuesXCRight
                                            ]), case SubtreeNoXCRight of
                                                    0 ->
                                                        [];
                                                    _ ->
                                                        InsertFunction(
                                                            StateTarget,
                                                            insert,
                                                            lists:append([
                                                                lists:sublist(SubtreesXC, 1, KeyNoXC),
                                                                [combine(LeftTree, RightTree, State)],
                                                                lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                                            ]))
                                                end
                                        }],
                                        lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                                    ]))
                            }
                    end
            end
    end.

-spec delete_1_3(key(), subtree(), pos_integer(), pos_integer(), pos_integer(), sort_function(), state()) -> subtree().

delete_1_3(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, KeyNoMax, KeyPos, SortFunction, nil) ->
    {KeyNoXCLeft, SubtreeNoXCLeft, KeyValuesXCLeft, SubtreesXCLeft} = case KeyPos of
                                                                          1 ->
                                                                              {0, 0, [], []};
                                                                          _ ->
                                                                              lists:nth(KeyPos - 1, Subtrees)
                                                                      end,
    {KeyNoXC, SubtreeNoXC, KeyValuesXC, SubtreesXC} = TreeXC = lists:nth(KeyPos, Subtrees),
    {KeyNoXCRight, SubtreeNoXCRight, KeyValuesXCRight, SubtreesXCRight} = case KeyPos of
                                                                              SubtreeNo ->
                                                                                  {0, 0, [], []};
                                                                              _ ->
                                                                                  lists:nth(KeyPos + 1, Subtrees)
                                                                          end,
    case KeyNoXC < SubtreeNoMin of
        true ->
            if
                KeyNoXCLeft >= SubtreeNoMin ->
                    % CLRS: case 3a - left
                    {
                        KeyNo,
                        SubtreeNo,
                        lists:append([
                            lists:sublist(KeyValues, 1, KeyPos - 2),
                            [lists:nth(KeyNoXCLeft, KeyValuesXCLeft)],
                            lists:sublist(KeyValues, KeyPos, KeyNo)
                        ]),
                        lists:append([
                            lists:sublist(Subtrees, 1, KeyPos - 2),
                            [{KeyNoXCLeft - 1, case SubtreeNoXCLeft of
                                                   0 ->
                                                       0;
                                                   _ ->
                                                       SubtreeNoXCLeft - 1
                                               end, lists:sublist(KeyValuesXCLeft, 1, KeyNoXCLeft - 1), case SubtreeNoXCLeft of
                                                                                                            0 ->
                                                                                                                [];
                                                                                                            _ ->
                                                                                                                lists:sublist(SubtreesXCLeft, 1, KeyNoXCLeft)
                                                                                                        end
                            },
                                delete_1(
                                    Key,
                                    {
                                        KeyNoXC + 1,
                                        case SubtreeNoXC of
                                            0 ->
                                                0;
                                            _ ->
                                                SubtreeNoXC + 1
                                        end,
                                        lists:append([
                                            [lists:nth(KeyPos - 1, KeyValues)],
                                            lists:sublist(KeyValuesXC, 1, KeyNoXC)
                                        ]),
                                        lists:append([
                                            case SubtreeNoXCLeft of
                                                0 ->
                                                    [];
                                                _ ->
                                                    [lists:nth(SubtreeNoXCLeft, SubtreesXCLeft)]
                                            end,
                                            SubtreesXC
                                        ])
                                    },
                                    SubtreeNoMin,
                                    KeyNoMax,
                                    SortFunction,
                                    nil)
                            ],
                            lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                        ])
                    };
                KeyNoXCRight >= SubtreeNoMin ->
% CLRS: case 3a - right
                    {
                        KeyNo,
                        SubtreeNo,
                        lists:append([
                            lists:sublist(KeyValues, 1, KeyPos - 1),
                            [lists:nth(1, KeyValuesXCRight)],
                            lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                        ]),
                        lists:append([
                            lists:sublist(Subtrees, 1, KeyPos - 1),
                            [delete_1(
                                Key,
                                {
                                    KeyNoXC + 1,
                                    case SubtreeNoXC of
                                        0 ->
                                            0;
                                        _ ->
                                            SubtreeNoXC + 1
                                    end,
                                    lists:append([
                                        lists:sublist(KeyValuesXC, 1, KeyNoXC),
                                        [lists:nth(KeyPos, KeyValues)]
                                    ]),
                                    lists:append([
                                        SubtreesXC,
                                        case SubtreeNoXCRight of
                                            0 ->
                                                [];
                                            _ ->
                                                [lists:nth(1, SubtreesXCRight)]
                                        end
                                    ])
                                },
                                SubtreeNoMin,
                                KeyNoMax,
                                SortFunction,
                                nil),
                                {KeyNoXCRight - 1, case SubtreeNoXCRight of
                                                       0 ->
                                                           0;
                                                       _ ->
                                                           SubtreeNoXCRight - 1
                                                   end, lists:sublist(KeyValuesXCRight, 2, KeyNoXCRight), case SubtreeNoXCRight of
                                                                                                              0 ->
                                                                                                                  [];
                                                                                                              _ ->
                                                                                                                  lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                                                                                          end
                                }
                            ],
                            lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                        ])
                    };
                KeyNoXCRight == 0 ->
% CLRS: case 3b left
                    case KeyNo of
                        1 ->
                            delete_1(
                                Key,
                                {
                                    KeyNoXCLeft + KeyNoXC + 1, SubtreeNoXCLeft + SubtreeNoXC, lists:append([
                                    KeyValuesXCLeft,
                                    KeyValues,
                                    KeyValuesXC
                                ]),
                                    lists:append([
                                        SubtreesXCLeft,
                                        SubtreesXC
                                    ])
                                },
                                SubtreeNoMin,
                                KeyNoMax,
                                SortFunction,
                                nil
                            );
                        _ ->
                            {
                                KeyNo - 1,
                                SubtreeNo - 1,
                                lists:append([
                                    lists:sublist(KeyValues, 1, KeyPos - 2),
                                    lists:sublist(KeyValues, KeyPos, KeyNo)
                                ]),
                                lists:append([
                                    lists:sublist(Subtrees, 1, KeyPos - 2),
                                    [
                                        delete_1(
                                            Key,
                                            {
                                                KeyNoXCLeft + KeyNoXC + 1,
                                                case SubtreeNoXC of
                                                    0 ->
                                                        0;
                                                    _ ->
                                                        SubtreeNoXCLeft + SubtreeNoXC
                                                end,
                                                lists:append([
                                                    KeyValuesXCLeft, [lists:nth(KeyPos - 1, KeyValues)], KeyValuesXC
                                                ]),
                                                lists:append([
                                                    SubtreesXCLeft,
                                                    SubtreesXC
                                                ])},
                                            SubtreeNoMin,
                                            KeyNoMax,
                                            SortFunction,
                                            nil)
                                    ],
                                    lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                                ])
                            }
                    end;
                true ->
% CLRS: case 3b right
                    case KeyNo of
                        1 ->
                            delete_1(
                                Key,
                                {
                                    KeyNoXC + KeyNoXCRight + 1,
                                    SubtreeNoXC + SubtreeNoXCRight,
                                    lists:append([
                                        KeyValuesXC,
                                        KeyValues,
                                        KeyValuesXCRight
                                    ]),
                                    lists:append([
                                        SubtreesXC,
                                        SubtreesXCRight
                                    ])
                                },
                                SubtreeNoMin,
                                KeyNoMax,
                                SortFunction,
                                nil
                            );
                        _ ->
                            {
                                KeyNo - 1,
                                SubtreeNo - 1,
                                lists:append([
                                    lists:sublist(KeyValues, 1, KeyPos - 1),
                                    lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                                ]),
                                lists:append([
                                    lists:sublist(Subtrees, 1, KeyPos - 1),
                                    [
                                        delete_1(
                                            Key,
                                            {
                                                KeyNoXC + KeyNoXCRight + 1,
                                                case SubtreeNoXC of
                                                    0 ->
                                                        0;
                                                    _ ->
                                                        SubtreeNoXC + SubtreeNoXCRight
                                                end,
                                                lists:append([
                                                    KeyValuesXC,
                                                    [lists:nth(KeyPos, KeyValues)],
                                                    KeyValuesXCRight
                                                ]),
                                                lists:append([
                                                    SubtreesXC,
                                                    SubtreesXCRight
                                                ])
                                            },
                                            SubtreeNoMin,
                                            KeyNoMax,
                                            SortFunction,
                                            nil)
                                    ],
                                    lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                                ])
                            }
                    end
            end;
        _ ->
% CLRS: case 3 else
            {
                KeyNo,
                SubtreeNo,
                KeyValues,
                lists:append([
                    lists:sublist(Subtrees, 1, KeyPos - 1),
                    [delete_1(Key, TreeXC, SubtreeNoMin, KeyNoMax, SortFunction, nil)],
                    lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                ])
            }
    end;
delete_1_3(Key, {KeyNo, SubtreeNo, KeyValues, SubtreesKey} = _Tree, SubtreeNoMin, KeyNoMax, KeyPos, SortFunction, {StateTarget, DeleteFunction, InsertFunction, LookupFunction} = State) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    true = DeleteFunction(StateTarget, delete, SubtreesKey),
    {KeyNoXCLeft, SubtreeNoXCLeft, KeyValuesXCLeft, SubtreesKeyXCLeft} = case KeyPos of
                                                                             1 ->
                                                                                 {0, 0, [], []};
                                                                             _ ->
                                                                                 lists:nth(KeyPos - 1, Subtrees)
                                                                         end,
    SubtreesXCLeft = LookupFunction(StateTarget, lookup, SubtreesKeyXCLeft),
    {KeyNoXC, SubtreeNoXC, KeyValuesXC, SubtreesKeyXC} = TreeXC = lists:nth(KeyPos, Subtrees),
    SubtreesXC = LookupFunction(StateTarget, lookup, SubtreesKeyXC),
    {KeyNoXCRight, SubtreeNoXCRight, KeyValuesXCRight, SubtreesKeyXCRight} = case KeyPos of
                                                                                 SubtreeNo ->
                                                                                     {0, 0, [], []};
                                                                                 _ ->
                                                                                     lists:nth(KeyPos + 1, Subtrees)
                                                                             end,
    SubtreesXCRight = LookupFunction(StateTarget, lookup, SubtreesKeyXCRight),
    case KeyNoXC < SubtreeNoMin of
        true ->
            if
                KeyNoXCLeft >= SubtreeNoMin ->
% CLRS: case 3a - left
                    true = DeleteFunction(StateTarget, delete, SubtreesKeyXCLeft),
                    true = DeleteFunction(StateTarget, delete, SubtreesKeyXC),
                    {
                        KeyNo,
                        SubtreeNo,
                        lists:append([
                            lists:sublist(KeyValues, 1, KeyPos - 2),
                            [lists:nth(KeyNoXCLeft, KeyValuesXCLeft)],
                            lists:sublist(KeyValues, KeyPos, KeyNo)
                        ]),
                        InsertFunction(
                            StateTarget,
                            insert,
                            lists:append([
                                lists:sublist(Subtrees, 1, KeyPos - 2),
                                [{KeyNoXCLeft - 1, case SubtreeNoXCLeft of
                                                       0 ->
                                                           0;
                                                       _ ->
                                                           SubtreeNoXCLeft - 1
                                                   end, lists:sublist(KeyValuesXCLeft, 1, KeyNoXCLeft - 1),
                                    InsertFunction(
                                        StateTarget,
                                        insert,
                                        case SubtreeNoXCLeft of
                                            0 ->
                                                [];
                                            _ ->
                                                lists:sublist(SubtreesXCLeft, 1, KeyNoXCLeft)
                                        end)
                                },
                                    delete_1(
                                        Key,
                                        {
                                            KeyNoXC + 1,
                                            case SubtreeNoXC of
                                                0 ->
                                                    0;
                                                _ ->
                                                    SubtreeNoXC + 1
                                            end,
                                            lists:append([
                                                [lists:nth(KeyPos - 1, KeyValues)],
                                                lists:sublist(KeyValuesXC, 1, KeyNoXC)
                                            ]),
                                            InsertFunction(
                                                StateTarget,
                                                insert,
                                                lists:append([
                                                    case SubtreeNoXCLeft of
                                                        0 ->
                                                            [];
                                                        _ ->
                                                            [lists:nth(SubtreeNoXCLeft, SubtreesXCLeft)]
                                                    end,
                                                    SubtreesXC
                                                ]))
                                        }, SubtreeNoMin, KeyNoMax, SortFunction, State)
                                ],
                                lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                            ]))
                    };
                KeyNoXCRight >= SubtreeNoMin ->
% CLRS: case 3a - right
                    true = DeleteFunction(StateTarget, delete, SubtreesKeyXC),
                    true = DeleteFunction(StateTarget, delete, SubtreesKeyXCRight),
                    {
                        KeyNo,
                        SubtreeNo,
                        lists:append([
                            lists:sublist(KeyValues, 1, KeyPos - 1),
                            [lists:nth(1, KeyValuesXCRight)],
                            lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                        ]),
                        InsertFunction(
                            StateTarget,
                            insert,
                            lists:append([
                                lists:sublist(Subtrees, 1, KeyPos - 1),
                                [delete_1(
                                    Key,
                                    {
                                        KeyNoXC + 1,
                                        case SubtreeNoXC of
                                            0 ->
                                                0;
                                            _ ->
                                                SubtreeNoXC + 1
                                        end,
                                        lists:append([
                                            lists:sublist(KeyValuesXC, 1, KeyNoXC),
                                            [lists:nth(KeyPos, KeyValues)]
                                        ]),
                                        InsertFunction(
                                            StateTarget,
                                            insert,
                                            lists:append([
                                                SubtreesXC,
                                                case SubtreeNoXCRight of
                                                    0 ->
                                                        [];
                                                    _ ->
                                                        [lists:nth(1, SubtreesXCRight)]
                                                end
                                            ]))
                                    }, SubtreeNoMin, KeyNoMax, SortFunction, State),
                                    {KeyNoXCRight - 1, case SubtreeNoXCRight of
                                                           0 ->
                                                               0;
                                                           _ ->
                                                               SubtreeNoXCRight - 1
                                                       end, lists:sublist(KeyValuesXCRight, 2, KeyNoXCRight),
                                        InsertFunction(
                                            StateTarget,
                                            insert,
                                            case SubtreeNoXCRight of
                                                0 ->
                                                    [];
                                                _ ->
                                                    lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                            end)
                                    }
                                ],
                                lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                            ]))
                    };
                KeyNoXCRight == 0 ->
% CLRS: case 3b left
                    true = DeleteFunction(StateTarget, delete, SubtreesKeyXCLeft),
                    true = DeleteFunction(StateTarget, delete, SubtreesKeyXC),
                    case KeyNo of
                        1 ->
                            delete_1(
                                Key,
                                {
                                    KeyNoXCLeft + KeyNoXC + 1,
                                    SubtreeNoXCLeft + SubtreeNoXC,
                                    lists:append([
                                        KeyValuesXCLeft,
                                        KeyValues,
                                        KeyValuesXC
                                    ]),
                                    lists:append([
                                        SubtreesXCLeft,
                                        SubtreesXC
                                    ])
                                },
                                SubtreeNoMin,
                                KeyNoMax,
                                SortFunction,
                                State);
                        _ ->
                            {
                                KeyNo - 1,
                                SubtreeNo - 1,
                                lists:append([
                                    lists:sublist(KeyValues, 1, KeyPos - 2),
                                    lists:sublist(KeyValues, KeyPos, KeyNo)
                                ]),
                                InsertFunction(
                                    StateTarget,
                                    insert,
                                    lists:append([
                                        lists:sublist(Subtrees, 1, KeyPos - 2),
                                        [
                                            delete_1(
                                                Key,
                                                {
                                                    KeyNoXCLeft + KeyNoXC + 1,
                                                    case SubtreeNoXC of
                                                        0 ->
                                                            0;
                                                        _ ->
                                                            SubtreeNoXCLeft + SubtreeNoXC
                                                    end,
                                                    lists:append([
                                                        KeyValuesXCLeft,
                                                        [lists:nth(KeyPos - 1, KeyValues)],
                                                        KeyValuesXC
                                                    ]),
                                                    InsertFunction(
                                                        StateTarget,
                                                        insert,
                                                        lists:append([
                                                            SubtreesXCLeft,
                                                            SubtreesXC
                                                        ]))
                                                }, SubtreeNoMin, KeyNoMax, SortFunction, State)
                                        ],
                                        lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                                    ]))
                            }
                    end;
                true ->
% CLRS: case 3b right
                    true = DeleteFunction(StateTarget, delete, SubtreesKeyXC),
                    true = DeleteFunction(StateTarget, delete, SubtreesKeyXCRight),
                    case KeyNo of
                        1 ->
                            delete_1(
                                Key,
                                {
                                    KeyNoXC + KeyNoXCRight + 1,
                                    SubtreeNoXC + SubtreeNoXCRight,
                                    lists:append([
                                        KeyValuesXC,
                                        KeyValues,
                                        KeyValuesXCRight
                                    ]),
                                    lists:append([
                                        SubtreesXC,
                                        SubtreesXCRight
                                    ])
                                },
                                SubtreeNoMin,
                                KeyNoMax,
                                SortFunction,
                                State
                            );
                        _ ->
                            {
                                KeyNo - 1,
                                SubtreeNo - 1,
                                lists:append([
                                    lists:sublist(KeyValues, 1, KeyPos - 1),
                                    lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                                ]),
                                InsertFunction(
                                    StateTarget,
                                    insert,
                                    lists:append([
                                        lists:sublist(Subtrees, 1, KeyPos - 1),
                                        [
                                            delete_1(
                                                Key,
                                                {
                                                    KeyNoXC + KeyNoXCRight + 1,
                                                    case SubtreeNoXC of
                                                        0 ->
                                                            0;
                                                        _ ->
                                                            SubtreeNoXC + SubtreeNoXCRight
                                                    end,
                                                    lists:append([
                                                        KeyValuesXC,
                                                        [lists:nth(KeyPos, KeyValues)],
                                                        KeyValuesXCRight
                                                    ]),
                                                    InsertFunction(
                                                        StateTarget,
                                                        insert,
                                                        lists:append([
                                                            SubtreesXC,
                                                            SubtreesXCRight
                                                        ]))
                                                },
                                                SubtreeNoMin,
                                                KeyNoMax,
                                                SortFunction,
                                                State)
                                        ],
                                        lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                                    ]))
                            }
                    end
            end;
        _ ->
% CLRS: case 3 else
            {
                KeyNo,
                SubtreeNo,
                KeyValues,
                InsertFunction(
                    StateTarget,
                    insert,
                    lists:append([
                        lists:sublist(Subtrees, 1, KeyPos - 1),
                        [delete_1(
                            Key,
                            TreeXC,
                            SubtreeNoMin,
                            KeyNoMax,
                            SortFunction,
                            State)],
                        lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                    ]))
            }
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_any(key(), b_tree()) -> b_tree().

delete_any(Key, BTree) ->
    case is_defined(Key, BTree) of
        true ->
            delete(Key, BTree);
        false ->
            BTree
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec empty(pos_integer()) -> b_tree().

empty(Order) when Order > 3 ->
    {Order div 2, Order - 1, 0, fun sort_ascending/2, nil, nil}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec enter(key(), value(), b_tree()) -> b_tree().

enter(Key, Value, {_, _, _, _, nil, _} = BTree) ->
    try
        update(Key, Value, BTree)
    catch
        error:{key_not_found, _} ->
            insert(Key, Value, BTree)
    end;
enter(Key, Value, BTree) ->
    case is_defined(Key, BTree) of
        true ->
            update(Key, Value, BTree);
        _ ->
            insert(Key, Value, BTree)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec from_dict(b_tree(), key_values()) -> b_tree().

from_dict(BTree, KeyValues) when length(KeyValues) > 0 ->
    case is_empty(BTree) of
        true ->
            from_dict_1(KeyValues, BTree);
        _ ->
            erlang:error({tree_not_empty, BTree})
    end.

-spec from_dict_1(key_values(), b_tree()) -> b_tree().

from_dict_1([], BTree) ->
    BTree;
from_dict_1([{Key, Value} | Tail], BTree) ->
    from_dict_1(Tail, insert(Key, Value, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a specialized version of `lookup'.

-spec get(key(), b_tree()) -> value().

get(Key, {_, _, 0, _, _, nil}) ->
    erlang:error({key_not_found, Key});
get(Key, {_, _, _, SortFunction, State, Tree}) ->
    case lookup_1(Key, Tree, SortFunction, State) of
        {value, Value} ->
            Value;
        _ ->
            erlang:error({key_not_found, Key})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec height(b_tree()) -> non_neg_integer().

height({_, _, 0, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
height({_, _, _, _, nil, Tree}) ->
    height_1(Tree, nil, 0);
height({_, _, _, _, {StateTarget, _, _, LookupFunction} = State, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}}) ->
    height_1({KeyNo, SubtreeNo, KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey)}, State, 0).

-spec height_1(subtree(), state(), non_neg_integer()) -> non_neg_integer().

% Leaf node.
height_1({_, _, _, []}, _, Height) ->
    Height;
% The most left subtree.
height_1({_, _, _, [Tree | _]}, nil, Height) ->
    height_1(Tree, nil, Height + 1);
height_1({_, _, _, [{KeyNo, SubtreeNo, KeyValues, SubtreesKey} | _]}, {StateTarget, _, _, LookupFunction} = State, Height) ->
    height_1({KeyNo, SubtreeNo, KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey)}, State, Height + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert(key(), value(), b_tree()) -> b_tree().

% First entry.
insert(Key, Value, {SubtreeNoMin, KeyNoMax, 0, SortFunction, State, nil}) ->
    {SubtreeNoMin, KeyNoMax, 1, SortFunction, State, {1, 0, [{Key, Value}], []}};
% Split root node.
insert(Key, Value, {SubtreeNoMin, KeyNoMax, SizeKeyValues, SortFunction, State, {KeyNo, _, _, _} = Tree}) when KeyNo == KeyNoMax ->
    {SubtreeNoMin, KeyNoMax, SizeKeyValues + 1, SortFunction, State, insert_1({Key, Value}, split_node_root(Tree, SubtreeNoMin, State), SubtreeNoMin, KeyNoMax, SortFunction, State)};
% Normal.
insert(Key, Value, {SubtreeNoMin, KeyNoMax, SizeKeyValues, SortFunction, State, Tree}) ->
    {SubtreeNoMin, KeyNoMax, SizeKeyValues + 1, SortFunction, State, insert_1({Key, Value}, Tree, SubtreeNoMin, KeyNoMax, SortFunction, State)}.

-spec insert_1(key_value(), subtree(), pos_integer(), pos_integer(), sort_function(), state()) -> subtree().

% Leaf node.
insert_1(KeyValue, {KeyNo, 0, KeyValues, []}, _, _, SortFunction, _) ->
    {KeyNo + 1, 0, insert_key_value(KeyValue, KeyValues, KeyNo, SortFunction), []};
% Non-Leaf node.
insert_1({Key, _} = KeyValue, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, KeyNoMax, SortFunction, nil) ->
    {ValueFound, SubtreePos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            % Look ahead.
            {SubtreeKeyNo, _, SubtreeKeyValues, _} = Subtree = lists:nth(SubtreePos, Subtrees),
            case SubtreeKeyNo of
                % Split node.
                KeyNoMax ->
                    {SplitKeyValues, SplitSubtrees1, SplitTree1, SplitTree2, SplitSubtrees2} = split_node_non_root(KeyNo, KeyValues, Subtrees, Subtree, SubtreePos, SubtreeNoMin, SortFunction, nil),
                    {
                        KeyNo + 1,
                        SubtreeNo + 1,
                        SplitKeyValues,
                        lists:append([
                            SplitSubtrees1,
                            case SortFunction(KeyValue, lists:nth(SubtreeNoMin, SubtreeKeyValues)) of
                                less ->
                                    [
                                        insert_1(KeyValue, SplitTree1, SubtreeNoMin, KeyNoMax, SortFunction, nil),
                                        SplitTree2
                                    ];
                                _ ->
                                    [
                                        SplitTree1,
                                        insert_1(KeyValue, SplitTree2, SubtreeNoMin, KeyNoMax, SortFunction, nil)
                                    ]
                            end,
                            SplitSubtrees2
                        ])
                    };
                _ ->
                    {
                        KeyNo,
                        SubtreeNo,
                        KeyValues,
                        lists:append([
                            lists:sublist(Subtrees, 1, SubtreePos - 1),
                            [insert_1(KeyValue, Subtree, SubtreeNoMin, KeyNoMax, SortFunction, nil)],
                            lists:sublist(Subtrees, SubtreePos + 1, KeyNo + 1)
                        ])
                    }
            end;
        _ ->
            erlang:error({key_exists, Key})
    end;
% Non-Leaf node.
insert_1({Key, _} = KeyValue, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}, SubtreeNoMin, KeyNoMax, SortFunction, {StateTarget, DeleteFunction, InsertFunction, LookupFunction} = State) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    true = DeleteFunction(StateTarget, delete, SubtreesKey),
    {ValueFound, SubtreePos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            % Look ahead.
            {SubtreeKeyNo, _, SubtreeKeyValues, _} = Subtree = lists:nth(SubtreePos, Subtrees),
            case SubtreeKeyNo of
                % Split node.
                KeyNoMax ->
                    {SplitKeyValues, SplitSubtrees1, SplitTree1, SplitTree2, SplitSubtrees2} = split_node_non_root(KeyNo, KeyValues, Subtrees, Subtree, SubtreePos, SubtreeNoMin, SortFunction, State),
                    {
                        KeyNo + 1,
                        SubtreeNo + 1,
                        SplitKeyValues,
                        InsertFunction(
                            StateTarget,
                            insert,
                            lists:append([
                                SplitSubtrees1,
                                case SortFunction(KeyValue, lists:nth(SubtreeNoMin, SubtreeKeyValues)) of
                                    less ->
                                        [
                                            insert_1(KeyValue, SplitTree1, SubtreeNoMin, KeyNoMax, SortFunction, State),
                                            SplitTree2
                                        ];
                                    _ ->
                                        [
                                            SplitTree1,
                                            insert_1(KeyValue, SplitTree2, SubtreeNoMin, KeyNoMax, SortFunction, State)
                                        ]
                                end,
                                SplitSubtrees2
                            ]))
                    };
                _ ->
                    {
                        KeyNo,
                        SubtreeNo,
                        KeyValues,
                        InsertFunction(
                            StateTarget,
                            insert,
                            lists:append([
                                lists:sublist(Subtrees, 1, SubtreePos - 1),
                                [insert_1(KeyValue, Subtree, SubtreeNoMin, KeyNoMax, SortFunction, State)],
                                lists:sublist(Subtrees, SubtreePos + 1, KeyNo + 1)
                            ]))
                    }
            end;
        _ ->
            erlang:error({key_exists, Key})
    end.

-spec insert_key_value(key_value(), key_values(), pos_integer(), sort_function()) -> key_values().

insert_key_value({Key, _} = KeyValue, KeyValues, KeyNo, SortFunction) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            lists:append([
                lists:sublist(KeyValues, 1, KeyPos - 1),
                [KeyValue],
                lists:sublist(KeyValues, KeyPos, KeyNo)
            ]);
        _ ->
            erlang:error({key_exists, Key})
    end.

-spec split_node_non_root(pos_integer(), key_values(), subtrees(), subtree(), pos_integer(), pos_integer(), sort_function(), state()) -> {key_values(), subtrees(), subtree(), subtree(), subtrees()}.

split_node_non_root(KeyNo, KeyValues, Subtrees, {TreeKeyNo, TreeSubtreeNo, TreeKeyValues, TreeSubtrees}, SubtreePos, SubtreeNoMin, SortFunction, nil) ->
    {
        % SplitKeyValues .......................................................
        insert_key_value(lists:nth(SubtreeNoMin, TreeKeyValues), KeyValues, KeyNo, SortFunction),
        % SplitSubtrees1 .......................................................
        lists:sublist(Subtrees, 1, SubtreePos - 1),
        % SplitTree 1 ..........................................................
        {
            SubtreeNoMin - 1,
            case TreeSubtreeNo of
                0 ->
                    0;
                _ ->
                    SubtreeNoMin
            end,
            lists:sublist(TreeKeyValues, 1, SubtreeNoMin - 1),
            % Leaf node to be splitted ?
            case TreeSubtreeNo of
                0 ->
                    [];
                _ ->
                    lists:sublist(TreeSubtrees, 1, SubtreeNoMin)
            end
        },
        % SplitTree 2 ..........................................................
        {
            TreeKeyNo - SubtreeNoMin,
            case TreeSubtreeNo of
                0 ->
                    0;
                _ ->
                    TreeKeyNo - SubtreeNoMin + 1
            end,
            lists:sublist(TreeKeyValues, SubtreeNoMin + 1, TreeKeyNo),
            % Leaf node to be splitted ?
            case TreeSubtreeNo of
                0 ->
                    [];
                _ ->
                    lists:sublist(TreeSubtrees, SubtreeNoMin + 1, TreeKeyNo + 1)
            end
        },
        % SplitSubtrees2 .......................................................
        lists:sublist(Subtrees, SubtreePos + 1, KeyNo + 1)
    };
split_node_non_root(KeyNo, KeyValues, Subtrees, {TreeKeyNo, TreeSubtreeNo, TreeKeyValues, TreeSubtreesKey}, SubtreePos, SubtreeNoMin, SortFunction, {StateTarget, DeleteFunction, InsertFunction, LookupFunction}) ->
    TreeSubtrees = LookupFunction(StateTarget, lookup, TreeSubtreesKey),
    true = DeleteFunction(StateTarget, delete, TreeSubtreesKey),
    {
        % SplitKeyValues .......................................................
        insert_key_value(lists:nth(SubtreeNoMin, TreeKeyValues), KeyValues, KeyNo, SortFunction),
        % SplitSubtrees1 .......................................................
        lists:sublist(Subtrees, 1, SubtreePos - 1),
        % SplitTree 1 ..........................................................
        {
            SubtreeNoMin - 1,
            case TreeSubtreeNo of
                0 ->
                    0;
                _ ->
                    SubtreeNoMin
            end,
            lists:sublist(TreeKeyValues, 1, SubtreeNoMin - 1),
            % Leaf node to be splitted ?
            case TreeSubtreeNo of
                0 ->
                    [];
                _ ->
                    InsertFunction(
                        StateTarget,
                        insert,
                        lists:sublist(TreeSubtrees, 1, SubtreeNoMin))
            end
        },
        % SplitTree 2 ..........................................................
        {
            TreeKeyNo - SubtreeNoMin,
            case TreeSubtreeNo of
                0 ->
                    0;
                _ ->
                    TreeKeyNo - SubtreeNoMin + 1
            end,
            lists:sublist(TreeKeyValues, SubtreeNoMin + 1, TreeKeyNo),
            % Leaf node to be splitted ?
            case TreeSubtreeNo of
                0 ->
                    [];
                _ ->
                    InsertFunction(
                        StateTarget,
                        insert,
                        lists:sublist(TreeSubtrees, SubtreeNoMin + 1, TreeKeyNo + 1))
            end
        },
        % SplitSubtrees2 .......................................................
        lists:sublist(Subtrees, SubtreePos + 1, KeyNo + 1)
    }.

-spec split_node_root(subtree(), pos_integer(), state()) -> subtree().

% Leaf node.
split_node_root({KeyNo, 0, KeyValues, []}, SubtreeNoMin, nil) ->
    {
        1,
        2,
        [lists:nth(SubtreeNoMin, KeyValues)],
        [
            {
                SubtreeNoMin - 1,
                0,
                lists:sublist(KeyValues, 1, SubtreeNoMin - 1),
                []
            },
            {
                KeyNo - SubtreeNoMin,
                0,
                lists:sublist(KeyValues, SubtreeNoMin + 1, KeyNo),
                []
            }
        ]
    };
split_node_root({KeyNo, 0, KeyValues, []}, SubtreeNoMin, {StateTarget, _, InsertFunction, _}) ->
    {
        1,
        2,
        [lists:nth(SubtreeNoMin, KeyValues)],
        InsertFunction(
            StateTarget,
            insert,
            [
                {
                    SubtreeNoMin - 1,
                    0,
                    lists:sublist(KeyValues, 1, SubtreeNoMin - 1),
                    []
                },
                {
                    KeyNo - SubtreeNoMin,
                    0,
                    lists:sublist(KeyValues, SubtreeNoMin + 1, KeyNo),
                    []
                }
            ])
    };
split_node_root({KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, nil) ->
    {
        1,
        2,
        [lists:nth(SubtreeNoMin, KeyValues)],
        [
            {
                SubtreeNoMin - 1,
                SubtreeNoMin,
                lists:sublist(KeyValues, 1, SubtreeNoMin - 1),
                lists:sublist(Subtrees, 1, SubtreeNoMin)
            },
            {
                KeyNo - SubtreeNoMin,
                KeyNo - SubtreeNoMin + 1,
                lists:sublist(KeyValues, SubtreeNoMin + 1, KeyNo),
                lists:sublist(Subtrees, SubtreeNoMin + 1, SubtreeNo)
            }
        ]
    };
split_node_root({KeyNo, SubtreeNo, KeyValues, SubtreesKey}, SubtreeNoMin, {StateTarget, DeleteFunction, InsertFunction, LookupFunction}) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    DeleteFunction(StateTarget, delete, SubtreesKey),
    {
        1,
        2,
        [lists:nth(SubtreeNoMin, KeyValues)],
        InsertFunction(
            StateTarget,
            insert,
            [
                {
                    SubtreeNoMin - 1,
                    SubtreeNoMin,
                    lists:sublist(KeyValues, 1, SubtreeNoMin - 1),
                    InsertFunction(
                        StateTarget,
                        insert,
                        lists:sublist(Subtrees, 1, SubtreeNoMin))
                },
                {
                    KeyNo - SubtreeNoMin,
                    KeyNo - SubtreeNoMin + 1,
                    lists:sublist(KeyValues, SubtreeNoMin + 1, KeyNo),
                    InsertFunction(
                        StateTarget,
                        insert,
                        lists:sublist(Subtrees, SubtreeNoMin + 1, SubtreeNo))
                }
            ])
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is a specialized version of `lookup'.

-spec is_defined(key(), b_tree()) -> boolean().

is_defined(_, {_, _, 0, _, _, nil}) ->
    false;
is_defined(Key, {_, _, _, SortFunction, State, Tree}) ->
    case lookup_1(Key, Tree, SortFunction, State) of
        none ->
            false;
        _ ->
            true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_empty(b_tree()) -> boolean().

is_empty({_, _, 0, _, _, nil}) ->
    true;
is_empty(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec iterator(b_tree()) -> iter().

iterator({_, _, 0, _, _, nil}) ->
    [];
iterator({_, _, _, _, State, {_, _, KeyValues, Subtrees}}) ->
    iterator_1({KeyValues, Subtrees, State}, []).

% The iterator structure is really just a list corresponding to
% the call stack of an in-order traversal. This is quite fast.

-spec iterator_1({key_values(), subtrees(), state()}, iter()) -> iter().

% The most left key / value.
iterator_1({KeyValues, [], State}, Iterator) ->
    [{KeyValues, [], State} | Iterator];
% The most left subtree.
iterator_1({KeyValues, Subtrees, nil}, Iterator) ->
    {_, _, KeyValues_1, Subtrees_1} = lists:nth(1, Subtrees),
    iterator_1({KeyValues_1, Subtrees_1, nil}, [{KeyValues, Subtrees, nil} | Iterator]);
iterator_1({KeyValues, SubtreesKey, {StateTarget, _, _, LookupFunction} = State}, Iterator) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    {_, _, KeyValues_1, Subtrees_1Key} = lists:nth(1, Subtrees),
    Subtrees_1 = LookupFunction(StateTarget, lookup, Subtrees_1Key),
    iterator_1({KeyValues_1, Subtrees_1, State}, [{KeyValues, Subtrees, State} | Iterator]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec iterator_from(key(), b_tree()) -> iter().

iterator_from(_, {_, _, 0, _, _, nil}) ->
    [];
iterator_from(Key, {_, _, _, SortFunction, State, Tree}) ->
    iterator_from_1(Key, Tree, [], SortFunction, State).

-spec iterator_from_1(key(), subtree(), iter(), sort_function(), state()) -> iter().

% The most left key / value.
iterator_from_1(Key, {KeyNo, 0, KeyValues, []}, Iterator, SortFunction, State) ->
    {_, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    [{lists:sublist(KeyValues, Pos, KeyNo), [], State} | Iterator];
% The most left subtree.
iterator_from_1(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, Iterator, SortFunction, nil) ->
    {_, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    {KeyNo_1, SubtreeNo_1, KeyValues_1, Subtrees_1} = lists:nth(Pos, Subtrees),
    iterator_from_1(Key, {KeyNo_1, SubtreeNo_1, KeyValues_1, Subtrees_1}, [{lists:sublist(KeyValues, Pos, KeyNo), lists:sublist(Subtrees, Pos, SubtreeNo), nil} | Iterator], SortFunction, nil);
iterator_from_1(Key, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}, Iterator, SortFunction, {StateTarget, _, _, LookupFunction} = State) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    {_, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    {KeyNo_1, SubtreeNo_1, KeyValues_1, Subtrees_1Key} = lists:nth(Pos, Subtrees),
    Subtrees_1 = LookupFunction(StateTarget, lookup, Subtrees_1Key),
    iterator_from_1(Key, {KeyNo_1, SubtreeNo_1, KeyValues_1, Subtrees_1}, [{lists:sublist(KeyValues, Pos, KeyNo), lists:sublist(Subtrees, Pos, SubtreeNo), State} | Iterator], SortFunction, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec keys(b_tree()) -> keys().

keys({_, _, 0, _, _, nil}) ->
    [];
keys({_, _, _, _, nil, {_, _, KeyValues, Subtrees}}) ->
    keys_1(KeyValues, Subtrees, nil, []);
keys({_, _, _, _, {StateTarget, _, _, LookupFunction} = State, {_, _, KeyValues, SubtreesKey}}) ->
    keys_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []).

-spec keys_1(key_values(), subtrees(), state(), keys()) -> keys().

keys_1([], [], _, Keys) ->
    Keys;
% Leaf node.
keys_1([{Key, _} | Tail], [], State, Keys) ->
    keys_1(Tail, [], State, lists:append([
        Keys,
        [Key]
    ]));
% The most right subtree.
keys_1([], [{_, _, KeyValues, Subtrees}], nil, Keys) ->
    lists:append([
        Keys,
        keys_1(KeyValues, Subtrees, nil, [])
    ]);
keys_1([], [{_, _, KeyValues, SubtreesKey}], {StateTarget, _, _, LookupFunction} = State, Keys) ->
    lists:append([
        Keys,
        keys_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, [])
    ]);
% Left subtree and current key.
keys_1([{Key, _} | TailKeyValues], [{_, _, KeyValues, Subtrees} | TailSubtrees], nil, Keys) ->
    keys_1(
        TailKeyValues,
        TailSubtrees,
        nil,
        lists:append([
            Keys,
            keys_1(KeyValues, Subtrees, nil, []),
            [Key]
        ]));
keys_1([{Key, _} | TailKeyValues], [{_, _, KeyValues, SubtreesKey} | TailSubtrees], {StateTarget, _, _, LookupFunction} = State, Keys) ->
    keys_1(
        TailKeyValues,
        TailSubtrees,
        State,
        lists:append([
            Keys,
            keys_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []),
            [Key]
        ])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec largest(b_tree()) -> key_value().

largest({_, _, 0, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
largest({_, _, _, _, nil, Tree}) ->
    largest_1(Tree, nil);
largest({_, _, _, _, {StateTarget, _, _, LookupFunction} = State, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}}) ->
    largest_1({KeyNo, SubtreeNo, KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey)}, State).

-spec largest_1(subtree(), state()) -> key_value().

% The most right key / value.
largest_1({KeyNo, 0, KeyValues, []}, _) ->
    lists:nth(KeyNo, KeyValues);
% The most right subtree.
largest_1({_, SubtreeNo, _, Subtrees}, nil) ->
    largest_1(lists:nth(SubtreeNo, Subtrees), nil);
largest_1({_, SubtreeNo, _, Subtrees}, {StateTarget, _, _, LookupFunction} = State) ->
    {NextKeyNo, NextSubtreeNo, NextKeyValues, NextSubtreesKey} = lists:nth(SubtreeNo, Subtrees),
    largest_1({NextKeyNo, NextSubtreeNo, NextKeyValues, LookupFunction(StateTarget, lookup, NextSubtreesKey)}, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec lookup(key(), b_tree()) -> 'none' | {'value', value()}.

lookup(_, {_, _, 0, _, _, nil}) ->
    none;
lookup(Key, {_, _, _, SortFunction, State, Tree}) ->
    lookup_1(Key, Tree, SortFunction, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec map(map_function(), b_tree()) -> b_tree().

map(_, {_, _, 0, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
map(Function, {SubtreeNoMin, KeyNoMax, SizeKeyValues, SortFunction, State, Tree}) when is_function(Function, 2) ->
    {SubtreeNoMin, KeyNoMax, SizeKeyValues, SortFunction, State, map_subtree(Function, State, Tree)}.

-spec map_key_values(map_function(), key_values(), key_values()) -> key_values().

map_key_values(_, [], KeyValuesMapped) ->
    KeyValuesMapped;
map_key_values(Function, [{Key, Value} | Tail], KeyValuesMapped) ->
    map_key_values(
        Function,
        Tail,
        lists:append([
            KeyValuesMapped,
            [{Key, Function(Key, Value)}]
        ])).

-spec map_subtree(map_function(), state(), subtree()) -> subtree().

% Leaf node.
map_subtree(Function, _, {KeyNo, 0, KeyValues, []}) ->
    {KeyNo, 0, map_key_values(Function, KeyValues, []), []};
map_subtree(Function, nil, {KeyNo, SubtreeNo, KeyValues, Subtrees}) ->
    {KeyNo, SubtreeNo, map_key_values(Function, KeyValues, []), map_subtrees(Function, nil, Subtrees, [])};
map_subtree(Function, {StateTarget, DeleteFunction, InsertFunction, LookupFunction} = State, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    true = DeleteFunction(StateTarget, delete, SubtreesKey),
    {KeyNo, SubtreeNo, map_key_values(Function, KeyValues, []), InsertFunction(StateTarget, insert, map_subtrees(Function, State, Subtrees, []))}.

-spec map_subtrees(map_function(), state(), subtrees(), subtrees()) -> subtrees().

map_subtrees(_, _, [], TreesMapped) ->
    TreesMapped;
map_subtrees(Function, State, [Tree | Tail], TreesMapped) ->
    map_subtrees(
        Function,
        State,
        Tail,
        lists:append([
            TreesMapped,
            [map_subtree(Function, State, Tree)]
        ])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec next(iter()) -> 'none' | {key(), value(), iter()}.

% One level up.
next([{[], _, _}, {[], _, _} = Iterator | TailIterator]) ->
    next(lists:append([
        [Iterator],
        TailIterator
    ]));
% End of leaf node.
next([{[], _, _}, {[{Key, Value} | TailKeyValues], [_ | TailSubtrees], State} | TailIterator]) ->
    {Key, Value, iterator_1({TailKeyValues, TailSubtrees, State}, TailIterator)};
% Processing a leaf node.
next([{[{Key, Value} | KeyValues], [], State} | Iterator]) ->
    {Key, Value, [{KeyValues, [], State} | Iterator]};
% End of iterator.
next([{[], _, _}]) ->
    none;
% Empty iterator.
next([]) ->
    none.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec set_parameter(b_tree(), atom(), any()) -> b_tree().

set_parameter({SubtreeNoMin, KeyNoMax, 0, _, State, nil}, sort, SortFunction)
    when is_function(SortFunction, 2) ->
    {SubtreeNoMin, KeyNoMax, 0, SortFunction, State, nil};
set_parameter({SubtreeNoMin, KeyNoMax, 0, SortFunction, _, nil}, state, {_, DeleteFunction, InsertFunction, LookupFunction} = State)
    when is_function(DeleteFunction, 3),
    is_function(InsertFunction, 3),
    is_function(LookupFunction, 3) ->
    {SubtreeNoMin, KeyNoMax, 0, SortFunction, State, nil}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec size_key_values(b_tree()) -> non_neg_integer().

size_key_values({_, _, SizeKeyValues, _, _, _}) ->
    SizeKeyValues.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec size_nodes(b_tree()) -> {non_neg_integer(), non_neg_integer()}.

size_nodes({_, _, 0, _, _, nil}) ->
    {0, 0};
size_nodes({_, _, _, _, nil, Tree}) ->
    size_nodes_tree(Tree, nil, {0, 0});
size_nodes({_, _, _, _, {StateTarget, _, _, LookupFunction} = State, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}}) ->
    size_nodes_tree({KeyNo, SubtreeNo, KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey)}, State, {0, 0}).

-spec size_nodes_tree(subtree(), state(), {non_neg_integer(), non_neg_integer()}) -> {non_neg_integer(), non_neg_integer()}.

% Leaf node.
size_nodes_tree({_, 0, _, []}, _, {SizeTotal, SizeLeaves}) ->
    {SizeTotal + 1, SizeLeaves + 1};
size_nodes_tree({_, _, _, Subtrees}, State, {SizeTotal, SizeLeaves}) ->
    size_nodes_subtrees(Subtrees, State, {SizeTotal + 1, SizeLeaves}).

-spec size_nodes_subtrees(subtrees(), state(), {non_neg_integer(), non_neg_integer()}) -> {non_neg_integer(), non_neg_integer()}.

size_nodes_subtrees([], _, {SizeTotal, SizeLeaves}) ->
    {SizeTotal, SizeLeaves};
size_nodes_subtrees([Tree | Tail], nil, {SizeTotal, SizeLeaves}) ->
    size_nodes_subtrees(Tail, nil, size_nodes_tree(Tree, nil, {SizeTotal, SizeLeaves}));
size_nodes_subtrees([{KeyNo, SubtreeNo, KeyValues, SubtreesKey} | Tail], {StateTarget, _, _, LookupFunction} = State, {SizeTotal, SizeLeaves}) ->
    size_nodes_subtrees(Tail, State, size_nodes_tree({KeyNo, SubtreeNo, KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey)}, State, {SizeTotal, SizeLeaves})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec smallest(b_tree()) -> key_value().

smallest({_, _, 0, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
smallest({_, _, _, _, nil, Tree}) ->
    smallest_1(Tree, nil);
smallest({_, _, _, _, {StateTarget, _, _, LookupFunction} = State, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}}) ->
    smallest_1({KeyNo, SubtreeNo, KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey)}, State).

-spec smallest_1(subtree(), state()) -> key_value().

% The most left key / value.
smallest_1({_, 0, KeyValues, []}, _) ->
    lists:nth(1, KeyValues);
% The most left subtree.
smallest_1({_, _, _, Subtrees}, nil) ->
    smallest_1(lists:nth(1, Subtrees), nil);
smallest_1({_, _, _, Subtrees}, {StateTarget, _, _, LookupFunction} = State) ->
    {NextKeyNo, NextSubtreeNo, NextKeyValues, NextSubtreesKey} = lists:nth(1, Subtrees),
    smallest_1({NextKeyNo, NextSubtreeNo, NextKeyValues, LookupFunction(StateTarget, lookup, NextSubtreesKey)}, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take(key(), b_tree()) -> {value(), b_tree()}.

take(Key, BTree) ->
    {value, Value} = lookup(Key, BTree),
    {Value, delete(Key, BTree)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_any(key(), b_tree()) -> 'error' | {value(), b_tree()}.

take_any(Key, BTree) ->
    case is_defined(Key, BTree) of
        true ->
            take(Key, BTree);
        false ->
            error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_largest(b_tree()) -> {key(), value(), b_tree()}.

take_largest(BTree) ->
    {Key, Value} = largest(BTree),
    {Key, Value, delete(Key, BTree)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_smallest(b_tree()) -> {key(), value(), b_tree()}.

take_smallest(BTree) ->
    {Key, Value} = smallest(BTree),
    {Key, Value, delete(Key, BTree)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec to_list(b_tree()) -> key_values().

to_list({_, _, 0, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
to_list({_, _, _, _, nil, {_, _, KeyValues, Subtrees}}) ->
    to_list_1(KeyValues, Subtrees, nil, []);
to_list({_, _, _, _, {StateTarget, _, _, LookupFunction} = State, {_, _, KeyValues, SubtreesKey}}) ->
    to_list_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []).

-spec to_list_1(key_values(), subtrees(), state(), key_values()) -> key_values().

to_list_1([], [], _, KeyValueList) ->
    KeyValueList;
% Leaf node.
to_list_1([KeyValue | Tail], [], State, KeyValueList) ->
    to_list_1(
        Tail,
        [],
        State,
        lists:append([
            KeyValueList,
            [KeyValue]
        ]));
% The most right subtree.
to_list_1([], [{_, _, KeyValues, Subtrees}], nil, KeyValueList) ->
    lists:append([
        KeyValueList,
        to_list_1(KeyValues, Subtrees, nil, [])
    ]);
to_list_1([], [{_, _, KeyValues, SubtreesKey}], {StateTarget, _, _, LookupFunction} = State, KeyValueList) ->
    lists:append([
        KeyValueList,
        to_list_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, [])
    ]);
% Left subtree and key / value from current key.
to_list_1([KeyValue | TailKeyValues], [{_, _, KeyValues, Subtrees} | TailSubtrees], nil, KeyValueList) ->
    to_list_1(
        TailKeyValues,
        TailSubtrees,
        nil,
        lists:append([
            KeyValueList,
            to_list_1(KeyValues, Subtrees, nil, []),
            [KeyValue]
        ]));
to_list_1([KeyValue | TailKeyValues], [{_, _, KeyValues, SubtreesKey} | TailSubtrees], {StateTarget, _, _, LookupFunction} = State, KeyValueList) ->
    to_list_1(
        TailKeyValues,
        TailSubtrees,
        State,
        lists:append([
            KeyValueList,
            to_list_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []),
            [KeyValue]
        ])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec update(key(), value(), b_tree()) -> b_tree().

update(Key, _, {_, _, 0, _, _, nil}) ->
    erlang:error({key_not_found, Key});
update(Key, Value, {SubtreeNoMin, KeyNoMax, SizeKeyValues, SortFunction, State, Tree}) ->
    {SubtreeNoMin, KeyNoMax, SizeKeyValues, SortFunction, State, update_1({Key, Value}, Tree, SortFunction, State)}.

-spec update_1(key_value(), subtree(), sort_function(), state()) -> subtree().

% Leaf node.
update_1({Key, _} = KeyValue, {KeyNo, 0, KeyValues, []}, SortFunction, _) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            erlang:error({key_not_found, Key});
        _ ->
            {
                KeyNo,
                0,
                lists:append([
                    lists:sublist(KeyValues, 1, KeyPos - 1),
                    [KeyValue],
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                ]),
                []
            }
    end;
update_1({Key, _} = KeyValue, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SortFunction, nil) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            {
                KeyNo,
                SubtreeNo,
                KeyValues,
                lists:append([
                    lists:sublist(Subtrees, 1, KeyPos - 1),
                    [update_1(KeyValue, lists:nth(KeyPos, Subtrees), SortFunction, nil)],
                    lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                ])
            };
        _ ->
            {
                KeyNo,
                SubtreeNo,
                lists:append([
                    lists:sublist(KeyValues, 1, KeyPos - 1),
                    [KeyValue],
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                ]),
                Subtrees
            }
    end;
update_1({Key, _} = KeyValue, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}, SortFunction, {StateTarget, DeleteFunction, InsertFunction, LookupFunction} = State) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            true = DeleteFunction(StateTarget, delete, SubtreesKey),
            {
                KeyNo,
                SubtreeNo,
                KeyValues,
                InsertFunction(
                    StateTarget,
                    insert,
                    lists:append([
                        lists:sublist(Subtrees, 1, KeyPos - 1),
                        [update_1(KeyValue, lists:nth(KeyPos, Subtrees), SortFunction, State)],
                        lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                    ]))
            };
        _ ->
            {
                KeyNo,
                SubtreeNo,
                lists:append([
                    lists:sublist(KeyValues, 1, KeyPos - 1),
                    [KeyValue],
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo)
                ]),
                SubtreesKey
            }
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec values(b_tree()) -> values().

values({_, _, 0, _, _, nil}) ->
    [];
values({_, _, _, _, nil, {_, _, KeyValues, Subtrees}}) ->
    values_1(KeyValues, Subtrees, nil, []);
values({_, _, _, _, {StateTarget, _, _, LookupFunction} = State, {_, _, KeyValues, SubtreesKey}}) ->
    values_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []).

-spec values_1(key_values(), subtrees(), state(), values()) -> values().

values_1([], [], _, Values) ->
    Values;
% Leaf node.
values_1([{_, Value} | Tail], [], State, Values) ->
    values_1(
        Tail,
        [],
        State,
        lists:append([
            Values,
            [Value]
        ]));
% The most right subtree.
values_1([], [{_, _, KeyValues, Subtrees}], nil, Values) ->
    lists:append([
        Values,
        values_1(KeyValues, Subtrees, nil, [])
    ]);
values_1([], [{_, _, KeyValues, SubtreesKey}], {StateTarget, _, _, LookupFunction} = State, Values) ->
    lists:append([
        Values,
        values_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, [])
    ]);
% Left subtree and value from current key.
values_1([{_, Value} | TailKeyValues], [{_, _, KeyValues, Subtrees} | TailSubtrees], nil, Values) ->
    values_1(
        TailKeyValues,
        TailSubtrees,
        nil,
        lists:append([
            Values,
            values_1(KeyValues, Subtrees, nil, []),
            [Value]
        ]));
values_1([{_, Value} | TailKeyValues], [{_, _, KeyValues, SubtreesKey} | TailSubtrees], {StateTarget, _, _, LookupFunction} = State, Values) ->
    values_1(
        TailKeyValues,
        TailSubtrees,
        State,
        lists:append([
            Values,
            values_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []),
            [Value]
        ])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec binary_search(key(), key_values(), pos_integer(), pos_integer(), pos_integer(), sort_function()) -> {'none', pos_integer()} | {any(), pos_integer()}.

binary_search(Key, KeyValues, KeyNo, Lower, Upper, SortFunction) when Lower > Upper ->
    TreeNo = case Lower > KeyNo of
                 true ->
                     Upper;
                 _ ->
                     Lower
             end,
    {KeyLast, _} = lists:nth(TreeNo, KeyValues),
    case SortFunction(Key, KeyLast) of
        less ->
            {none, TreeNo};
        _ ->
            {none, TreeNo + 1}
    end;
binary_search(Key, KeyValues, KeyNo, Lower, Upper, SortFunction) ->
    Mid = (Upper + Lower) div 2,
    {MidKey, MidValue} = lists:nth(Mid, KeyValues),
    case SortFunction(Key, MidKey) of
        greater ->
            binary_search(Key, KeyValues, KeyNo, Mid + 1, Upper, SortFunction);
        less ->
            binary_search(Key, KeyValues, KeyNo, Lower, Mid - 1, SortFunction);
        _ ->
            {MidValue, Mid}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The term order is an arithmetic total order, so we should not
%% test exact equality for the keys. (If we do, then it becomes
%% possible that neither `>', `<', nor `=:=' matches.) Testing '<'
%% and '>' first is statistically better than testing for
%% equality, and also allows us to skip the test completely in the
%% remaining case.

-spec lookup_1(key(), subtree(), sort_function(), state()) -> 'none' |  {'value', value()}.

% Leaf node.
lookup_1(Key, {KeyNo, 0, KeyValues, []}, SortFunction, _) ->
    {Value, _} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case Value of
        none ->
            Value;
        _ ->
            {value, Value}
    end;
% Non-Leaf node.
lookup_1(Key, {KeyNo, _, KeyValues, Subtrees}, SortFunction, nil) ->
    {Value, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case Value of
        none ->
            lookup_1(Key, lists:nth(Pos, Subtrees), SortFunction, nil);
        _ ->
            {value, Value}
    end;
lookup_1(Key, {KeyNo, _, KeyValues, SubtreesKey}, SortFunction, {StateTarget, _, _, LookupFunction} = State) ->
    {Value, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case Value of
        none ->
            lookup_1(Key, lists:nth(Pos, LookupFunction(StateTarget, lookup, SubtreesKey)), SortFunction, State);
        _ ->
            {value, Value}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sort_ascending(key(), key()) -> sort_result().

sort_ascending(Key_1, Key_2) ->
    if
        Key_1 < Key_2 -> less;
        Key_1 > Key_2 -> greater;
        true -> equal
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sort_descending(key(), key()) -> sort_result().

sort_descending(Key_1, Key_2) ->
    if
        Key_1 < Key_2 -> greater;
        Key_1 > Key_2 -> less;
        true -> equal
    end.
