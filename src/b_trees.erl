% -define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/b_trees_templates.hrl").

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
%% Computer Programming": The order O of a B-Tree is an integer expressing
%% the maximum number of subtrees (sons) in a node and the height of a B-Tree is
%% its maximum level, i.e. the length of the longest path from the root node to
%% a leaf node. The minimum order of a B-Tree is 4.
%% -----------------------------------------------------------------------------
%% Operations:
%%
%% - delete(K, B): removes key K from B-Tree B; returns a new B-Tree B'. Assumes
%%   that the key is present in the tree.
%%
%% - delete_any(K, B): removes key K from B-Tree B if the key is present in the
%%   tree, otherwise does nothing; returns a new B-Tree B'.
%%
%% - empty(O): returns a new empty B-Tree of order O. Order is defined as the
%%   maximum number of children nodes a non-leaf node may hold. The minimum
%%   value is 4. The sort order of the key values is ascending.
%%
%% - empty(O, F): returns a new empty B-Tree of order O. Order is defined as
%%   the maximum number of children nodes a non-leaf node may hold. The
%%   minimum value is 4. The sort order of the key values is defined in
%%   function F(K, K) -> equal | greater | less.
%%
%% - enter(K, V, B): inserts key K with value V into B-Tree B if the key is not
%%   present in the tree, otherwise updates key K to value V in B. Returns the
%%   new tree.
%%
%% - from_dict(B, L): turns a list L of {Key, Value} pairs into a B-Tree. B must
%%   be an empty B-Tree. The list must not contain duplicate keys.
%%
%% - get(K, B): retrieves the value stored with key K in B-Tree B. Assumes that
%%   the key is present in the tree.
%%
%% - height(B): returns the height of the B-Tree B as an integer. Assumes that
%%   the B-Tree B is non-empty.
%%
%% - insert(K, V, B): inserts key K with value V into B-Tree B; returns a new
%%   B-Tree. Assumes that the key K is *not* present in the B-Tree B.
%%
%% - is_defined(K, B): returns `true' if key K is present in B-Tree B, and
%%   `false' otherwise.
%%
%% - is_empty(B): returns 'true' if B is an empty B-Tree, and 'false' otherwise.
%%
%% - iterator(B): returns an iterator that can be used for traversing the
%%   entries of B-Tree B; see `next'. The implementation of this is very
%%   efficient; traversing the whole tree using `next' is only slightly slower
%%   than getting the list of all elements using `to_list' and traversing that.
%%   The main advantage of the iterator approach is that it does not require the
%%   complete list of all elements to be built in memory at one time.
%%
%% - iterator_from(K, B): Returns an iterator that can be used for traversing the
%%   entries of B-Tree B; see next/1. The difference as compared to the iterator
%%   returned by iterator/1 is that the first key greater than or equal to Key K
%%   is returned.
%%
%% - keys(B): returns an ordered list of all keys in B-Tree B.
%%
%% - largest(B): returns tuple {K, V}, where K is the largest key in B-Tree B,
%%   and V is the value associated with K in B. Assumes that the B-Tree B is
%%   non-empty.
%%
%% - lookup(K, B): looks up key K in B-Tree B; returns {value, V}, or `none' if
%%   the key K is not present.
%%
%% - map(F, B): maps the function F(K, V) -> V' to all key-value pairs of the
%%   B-Tree B and returns a new B-Tree B' with the same set of keys as B and
%%   the new set of values V'.
%%
%% - next(I): returns {K, V, I1} where K is the smallest key referred to by
%%   the iterator I, and I1 is the new iterator to be used for traversing the
%%   remaining entries, or the atom `none' if no entries remain.
%%
%% - number_key_values(B): returns the number of key / value pairs in the
%%   B-Tree B as an integer. Returns 0 (zero) if the B-Tree B is empty.
%%
%% - set_parameter(B, P, V): sets in B-Tree B the parameter P to value V;
%%   returns a new B-Tree B'.
%%
%% - size(B): returns the number of nodes in the B-Tree B as an integer.
%%   Returns 0 (zero) if the B-Tree B is empty.
%%
%% - smallest(B): returns tuple {K, V}, where K is the smallest key in B-Tree B,
%%   and V is the value associated with K in B. Assumes that the B-Tree B is
%%   non-empty.
%%
%% - sort_ascending(K1, K2): returns the atom greater if K1 > K2, the atom less
%%   if K1 < K2 and the atom equal else-wise.
%%
%% - sort_descending(K1, K2): returns the atom greater if K1 < K2, the atom less
%%   if K1 > K2 and the atom equal else-wise.
%%
%% - take_largest(B): returns {K, V, B'}, where K is the largest key in B-Tree
%%   B, V is the value associated with K in B, and B' is the B-Tree B with key
%%   K deleted. Assumes that B-Tree B is non-empty.
%%
%% - take_smallest(B): returns {K, V, B'}, where K is the smallest key in
%%   B-Tree B, V is the value associated with K in B, and B' is the B-Tree B
%%   with key K deleted. Assumes that B-Tree B is non-empty.
%%
%% - to_list(B): returns an ordered list of {Key, Value} pairs for all keys in
%%   B-Tree B.
%%
%% - update(K, V, B): updates key K to value V in B-Tree B; returns a new
%%   B-Tree B'. Assumes that the key is present in the tree.
%%
%% - values(B): returns the list of values for all keys in B-Tree B, sorted by
%%   their corresponding keys. Duplicates are not removed.

-module(b_trees).

-export([
    delete/2,
    delete_any/2,
    empty/1,
    empty/2,
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
    number_key_values/1,
    set_parameter/3,
    size/1,
    smallest/1,
    sort_ascending/2,
    sort_descending/2,
    take_smallest/1,
    take_largest/1,
    to_list/1,
    update/3,
    values/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data structure:
%% - {MinimumSubtrees, MaximumKeys, NumberKeyValues, SortFunction, State, Tree},
%%   where `Tree' is composed of :
%%   - {KeyNo, SubtreeNo, [{Key, Value}], [Tree]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

-export_type([b_tree/0]).
-type b_tree() :: {pos_integer(), pos_integer(), non_neg_integer(), sort_function(), any(), tree()}.

-export_type([delete_function/0]).
-type delete_function() :: fun((state_target(), 'delete', subtrees_key()) -> 'ok').

-export_type([insert_function/0]).
-type insert_function() :: fun((state_target(), 'insert', subtrees()) -> subtrees_key()).

-export_type([iterator/0]).
-type iterator() :: [{key_values(), subtrees(), state()}].

-type key() :: any().
-type keys() :: [key()].

-type key_value() :: {key(), value()}.
-type key_values() :: [key_value()].

-export_type([lookup_function/0]).
-type lookup_function() :: fun((state_target(), 'lookup', subtrees_key()) -> subtrees()).

-export_type([map_function/0]).
-type map_function() :: fun((key(), value()) -> value()).

-export_type([sort_function/0]).
-type sort_function() :: fun((key(), key()) -> sort_result()).
-type sort_result() :: 'less' | 'equal' | 'greater'.

-type state() :: 'nil'
| {state_target(), delete_function(), insert_function(), lookup_function()}.
-type state_target() :: any().

-type subtrees() :: subtrees_key()
| [tree()].
-type subtrees_key() :: integer().

-type tree() :: 'nil'
| {pos_integer(), pos_integer(), key_values(), []}
| {pos_integer(), pos_integer(), key_values(), subtrees()}.

-type value() :: any().
-type values() :: [value()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% delete. Assumes that key is present.

-spec delete(key(), b_tree()) -> b_tree().

% Empty tree.
delete(Key, {_, _, 0, _, _, nil}) ->
    erlang:error({key_not_found, Key});
% Root node is leaf node.
delete(Key, {SubtreeNoMin, KeyNoMax, NumberKeyValues, SortFunction, StateTarget, {KeyNo, 0, KeyValues, []}}) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            erlang:error({key_not_found, Key});
        _ ->
            {SubtreeNoMin, KeyNoMax, NumberKeyValues - 1, SortFunction, StateTarget, case KeyNo == 1 of
                                                                                         true ->
                                                                                             nil;
                                                                                         _ ->
                                                                                             {
                                                                                                 KeyNo - 1,
                                                                                                 0,
                                                                                                     lists:sublist(KeyValues, 1, KeyPos - 1) ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                                                                                                 []
                                                                                             }
                                                                                     end}
    end;
delete(Key, {SubtreeNoMin, KeyNoMax, NumberKeyValues, SortFunction, StateTarget, Tree}) ->
    {SubtreeNoMin, KeyNoMax, NumberKeyValues - 1, SortFunction, StateTarget, delete_1(Key, Tree, SubtreeNoMin, KeyNoMax, SortFunction)}.

-spec combine(tree(), tree()) -> tree().

combine({LeftKeyNo, 0, LeftKeyValues, []}, {RightKeyNo, 0, RightKeyValues, []}) ->
    {
        LeftKeyNo + RightKeyNo,
        0,
            LeftKeyValues ++ RightKeyValues,
        []
    };
combine({LeftKeyNo, LeftSubtreeNo, LeftKeyValues, LeftSubtrees}, {RightKeyNo, RightSubtreeNo, RightKeyValues, RightSubtrees}) ->
    {
        LeftKeyNo + RightKeyNo,
        LeftSubtreeNo + RightSubtreeNo - 1,
            LeftKeyValues ++ RightKeyValues,
            lists:sublist(LeftSubtrees, 1, LeftKeyNo) ++ [combine(lists:nth(LeftSubtreeNo, LeftSubtrees), lists:nth(1, RightSubtrees))] ++ lists:sublist(RightSubtrees, 2, RightSubtreeNo)
    }.

-spec delete_1(key(), tree(), pos_integer(), pos_integer(), sort_function()) -> tree().

% Leaf node.
delete_1(Key, {KeyNo, 0, KeyValues, []}, _, _, SortFunction) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            erlang:error({key_not_found, Key});
        _ ->
            % CLRS: case 1 leaf node
            {
                KeyNo - 1,
                0,
                    lists:sublist(KeyValues, 1, KeyPos - 1) ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                []
            }
    end;
delete_1(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, KeyNoMax, SortFunction) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            delete_1_3(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, KeyNoMax, KeyPos, SortFunction);
        _ ->
            delete_1_2(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, KeyNoMax, KeyPos)
    end.

-spec delete_1_2(key(), tree(), pos_integer(), pos_integer(), pos_integer()) -> tree().

delete_1_2(_, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, _, KeyPos) ->
    {KeyNoXC, SubtreeNoXC, KeyValuesXC, SubtreesXC} = lists:nth(KeyPos, Subtrees),
    {KeyNoXCRight, SubtreeNoXCRight, KeyValuesXCRight, SubtreesXCRight} = lists:nth(KeyPos + 1, Subtrees),
    case KeyNoXC >= SubtreeNoMin of
        true ->
            % CLRS: case 2a
            {
                KeyNo,
                SubtreeNo,
                    lists:sublist(KeyValues, 1, KeyPos - 1) ++ [lists:nth(KeyNoXC, KeyValuesXC)] ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                    lists:sublist(Subtrees, 1, KeyPos - 1) ++
                    [
                        {
                            KeyNoXC - 1, case SubtreeNoXC == 0 of
                                             true ->
                                                 0;
                                             _ ->
                                                 SubtreeNoXC - 1
                                         end, lists:sublist(KeyValuesXC, 1, KeyNoXC - 1), case SubtreeNoXC == 0 of
                                                                                              true ->
                                                                                                  [];
                                                                                              _ ->
                                                                                                  lists:sublist(SubtreesXC, 1, KeyNoXC)
                                                                                          end
                        },
                        {
                            KeyNoXCRight,
                            SubtreeNoXCRight,
                            KeyValuesXCRight,
                            case SubtreeNoXC == 0 of
                                true ->
                                    [];
                                _ ->
                                    [combine(lists:nth(SubtreeNoXC, SubtreesXC), lists:nth(1, SubtreesXCRight))] ++ lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                            end
                        }
                    ] ++ lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
            };
        _ ->
            case KeyNoXCRight >= SubtreeNoMin of
                % CLRS: case 2b
                true ->
                    {
                        KeyNo,
                        SubtreeNo,
                            lists:sublist(KeyValues, 1, KeyPos - 1) ++ [lists:nth(1, KeyValuesXCRight)] ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                            lists:sublist(Subtrees, 1, KeyPos - 1) ++
                            [
                                {
                                    KeyNoXC,
                                    SubtreeNoXC,
                                    KeyValuesXC,
                                    case SubtreeNoXC == 0 of
                                        true ->
                                            [];
                                        _ ->
                                            lists:sublist(SubtreesXC, 1, KeyNoXC) ++ [combine(lists:nth(SubtreeNoXC, SubtreesXC), lists:nth(1, SubtreesXCRight))]
                                    end
                                },
                                {
                                    KeyNoXCRight - 1, case SubtreeNoXCRight == 0 of
                                                          true ->
                                                              0;
                                                          _ ->
                                                              SubtreeNoXCRight - 1
                                                      end, lists:sublist(KeyValuesXCRight, 2, KeyNoXCRight), case SubtreeNoXCRight == 0 of
                                                                                                                 true ->
                                                                                                                     [];
                                                                                                                 _ ->
                                                                                                                     lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                                                                                             end
                                }
                            ] ++
                            lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                    };
                % CLRS: case 2c
                _ ->
                    {
                        LeftTree,
                        RightTree
                    } = case SubtreeNoXC == 0 of
                            true ->
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
                    case KeyNo == 1 of
                        true ->
                            {
                                KeyNoXC + KeyNoXCRight,
                                case SubtreeNoXCRight == 0 of
                                    true ->
                                        0;
                                    _ ->
                                        SubtreeNoXC + SubtreeNoXCRight - 1
                                end,
                                    KeyValuesXC ++ KeyValuesXCRight,
                                case SubtreeNoXCRight == 0 of
                                    true ->
                                        [];
                                    _ ->
                                        lists:sublist(SubtreesXC, 1, KeyNoXC) ++ [combine(LeftTree, RightTree)] ++ lists:sublist(SubtreesXCRight, 2, KeyNoXCRight)
                                end
                            };
                        _ ->
                            {
                                KeyNo - 1,
                                SubtreeNo - 1,
                                    lists:sublist(KeyValues, 1, KeyPos - 1) ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                                    lists:sublist(Subtrees, 1, KeyPos - 1) ++
                                    [
                                        {
                                            KeyNoXC + KeyNoXCRight, case SubtreeNoXCRight == 0 of
                                                                        true ->
                                                                            0;
                                                                        _ ->
                                                                            SubtreeNoXC + SubtreeNoXCRight - 1
                                                                    end, KeyValuesXC ++ KeyValuesXCRight, case SubtreeNoXCRight == 0 of
                                                                                                              true ->
                                                                                                                  [];
                                                                                                              _ ->
                                                                                                                  lists:sublist(SubtreesXC, 1, KeyNoXC) ++ [combine(LeftTree, RightTree)] ++ lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                                                                                          end
                                        }
                                    ] ++ lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                            }
                    end
            end
    end.

-spec delete_1_3(key(), tree(), pos_integer(), pos_integer(), pos_integer(), sort_function()) -> tree().

delete_1_3(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, KeyNoMax, KeyPos, SortFunction) ->
    {KeyNoXCLeft, SubtreeNoXCLeft, KeyValuesXCLeft, SubtreesXCLeft} = case KeyPos == 1 of
                                                                          true ->
                                                                              {0, 0, [], []};
                                                                          _ ->
                                                                              lists:nth(KeyPos - 1, Subtrees)
                                                                      end,
    {KeyNoXC, SubtreeNoXC, KeyValuesXC, SubtreesXC} = TreeXC = lists:nth(KeyPos, Subtrees),
    {KeyNoXCRight, SubtreeNoXCRight, KeyValuesXCRight, SubtreesXCRight} = case KeyPos == SubtreeNo of
                                                                              true ->
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
                            lists:sublist(KeyValues, 1, KeyPos - 2) ++ [lists:nth(KeyNoXCLeft, KeyValuesXCLeft)] ++ lists:sublist(KeyValues, KeyPos, KeyNo),
                            lists:sublist(Subtrees, 1, KeyPos - 2) ++
                            [{KeyNoXCLeft - 1, case SubtreeNoXCLeft == 0 of
                                                   true ->
                                                       0;
                                                   _ ->
                                                       SubtreeNoXCLeft - 1
                                               end, lists:sublist(KeyValuesXCLeft, 1, KeyNoXCLeft - 1), case SubtreeNoXCLeft == 0 of
                                                                                                            true ->
                                                                                                                [];
                                                                                                            _ ->
                                                                                                                lists:sublist(SubtreesXCLeft, 1, KeyNoXCLeft)
                                                                                                        end
                            },
                                delete_1(Key, {KeyNoXC + 1, case SubtreeNoXC == 0 of
                                                                true ->
                                                                    0;
                                                                _ ->
                                                                    SubtreeNoXC + 1
                                                            end, [lists:nth(KeyPos - 1, KeyValues)] ++ lists:sublist(KeyValuesXC, 1, KeyNoXC), case SubtreeNoXCLeft == 0 of
                                                                                                                                                   true ->
                                                                                                                                                       [];
                                                                                                                                                   _ ->
                                                                                                                                                       [lists:nth(SubtreeNoXCLeft, SubtreesXCLeft)]
                                                                                                                                               end ++ SubtreesXC}, SubtreeNoMin, KeyNoMax, SortFunction)
                            ] ++ lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                    };
                KeyNoXCRight >= SubtreeNoMin ->
                    % CLRS: case 3a - right
                    {
                        KeyNo,
                        SubtreeNo,
                            lists:sublist(KeyValues, 1, KeyPos - 1) ++ [lists:nth(1, KeyValuesXCRight)] ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                            lists:sublist(Subtrees, 1, KeyPos - 1) ++
                            [delete_1(Key, {KeyNoXC + 1, case SubtreeNoXC == 0 of
                                                             true ->
                                                                 0;
                                                             _ ->
                                                                 SubtreeNoXC + 1
                                                         end, lists:sublist(KeyValuesXC, 1, KeyNoXC) ++ [lists:nth(KeyPos, KeyValues)], SubtreesXC ++ case SubtreeNoXCRight == 0 of
                                                                                                                                                          true ->
                                                                                                                                                              [];
                                                                                                                                                          _ ->
                                                                                                                                                              [lists:nth(1, SubtreesXCRight)]
                                                                                                                                                      end}, SubtreeNoMin, KeyNoMax, SortFunction),
                                {KeyNoXCRight - 1, case SubtreeNoXCRight == 0 of
                                                       true ->
                                                           0;
                                                       _ ->
                                                           SubtreeNoXCRight - 1
                                                   end, lists:sublist(KeyValuesXCRight, 2, KeyNoXCRight), case SubtreeNoXCRight == 0 of
                                                                                                              true ->
                                                                                                                  [];
                                                                                                              _ ->
                                                                                                                  lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                                                                                          end
                                }
                            ] ++ lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                    };
                KeyNoXCRight == 0 ->
                    % CLRS: case 3b left
                    case KeyNo == 1 of
                        true ->
                            delete_1(Key, {KeyNoXCLeft + KeyNoXC + 1, SubtreeNoXCLeft + SubtreeNoXC, KeyValuesXCLeft ++ KeyValues ++ KeyValuesXC, SubtreesXCLeft ++ SubtreesXC}, SubtreeNoMin, KeyNoMax, SortFunction);
                        _ ->
                            {
                                KeyNo - 1,
                                SubtreeNo - 1,
                                    lists:sublist(KeyValues, 1, KeyPos - 2) ++ lists:sublist(KeyValues, KeyPos, KeyNo),
                                    lists:sublist(Subtrees, 1, KeyPos - 2) ++
                                    [
                                        delete_1(Key, {KeyNoXCLeft + KeyNoXC + 1, case SubtreeNoXC == 0 of
                                                                                      true ->
                                                                                          0;
                                                                                      _ ->
                                                                                          SubtreeNoXCLeft + SubtreeNoXC
                                                                                  end, KeyValuesXCLeft ++ [lists:nth(KeyPos - 1, KeyValues)] ++ KeyValuesXC, SubtreesXCLeft ++ SubtreesXC}, SubtreeNoMin, KeyNoMax, SortFunction)
                                    ] ++ lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                            }
                    end;
                true ->
                    % CLRS: case 3b right
                    case KeyNo == 1 of
                        true ->
                            delete_1(Key, {KeyNoXC + KeyNoXCRight + 1, SubtreeNoXC + SubtreeNoXCRight, KeyValuesXC ++ KeyValues ++ KeyValuesXCRight, SubtreesXC ++ SubtreesXCRight}, SubtreeNoMin, KeyNoMax, SortFunction);
                        _ ->
                            {
                                KeyNo - 1,
                                SubtreeNo - 1,
                                    lists:sublist(KeyValues, 1, KeyPos - 1) ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                                    lists:sublist(Subtrees, 1, KeyPos - 1) ++
                                    [
                                        delete_1(Key, {KeyNoXC + KeyNoXCRight + 1, case SubtreeNoXC == 0 of
                                                                                       true ->
                                                                                           0;
                                                                                       _ ->
                                                                                           SubtreeNoXC + SubtreeNoXCRight
                                                                                   end, KeyValuesXC ++ [lists:nth(KeyPos, KeyValues)] ++ KeyValuesXCRight, SubtreesXC ++ SubtreesXCRight}, SubtreeNoMin, KeyNoMax, SortFunction)
                                    ] ++ lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                            }
                    end
            end;
        _ ->
            % CLRS: case 3 else
            {
                KeyNo,
                SubtreeNo,
                KeyValues,
                    lists:sublist(Subtrees, 1, KeyPos - 1) ++ [delete_1(Key, TreeXC, SubtreeNoMin, KeyNoMax, SortFunction)] ++ lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
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
    empty(Order, fun sort_ascending/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec empty(pos_integer(), sort_function()) -> b_tree().

empty(Order, Function) when Order > 3, is_function(Function, 2) ->
    {Order div 2, Order - 1, 0, Function, nil, nil}.

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
    case b_trees:is_empty(BTree) of
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
    case lookup_1(Key, Tree, SortFunction, State)
    of
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

-spec height_1(tree(), state(), non_neg_integer()) -> non_neg_integer().

% Leaf node.
height_1({_, _, _, []}, _, Number) ->
    Number;
% The most left subtree.
height_1({_, _, _, [Tree | _]}, nil, Number) ->
    height_1(Tree, nil, Number + 1);
height_1({_, _, _, [{KeyNo, SubtreeNo, KeyValues, SubtreesKey} | _]}, {StateTarget, _, _, LookupFunction} = State, Number) ->
    height_1({KeyNo, SubtreeNo, KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey)}, State, Number + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert(key(), value(), b_tree()) -> b_tree().

% First entry.
insert(Key, Value, {SubtreeNoMin, KeyNoMax, 0, SortFunction, State, nil}) ->
    {SubtreeNoMin, KeyNoMax, 1, SortFunction, State, {1, 0, [{Key, Value}], []}};
% Split root node.
insert(Key, Value, {SubtreeNoMin, KeyNoMax, NumberKeyValues, SortFunction, State, {KeyNo, _, _, _} = Tree}) when KeyNo == KeyNoMax ->
    {SubtreeNoMin, KeyNoMax, NumberKeyValues + 1, SortFunction, State, insert_1({Key, Value}, split_node_root(Tree, SubtreeNoMin, State), SubtreeNoMin, KeyNoMax, SortFunction, State)};
% Normal.
insert(Key, Value, {SubtreeNoMin, KeyNoMax, NumberKeyValues, SortFunction, State, Tree}) ->
    {SubtreeNoMin, KeyNoMax, NumberKeyValues + 1, SortFunction, State, insert_1({Key, Value}, Tree, SubtreeNoMin, KeyNoMax, SortFunction, State)}.

-spec insert_1(key_value(), tree(), pos_integer(), pos_integer(), sort_function(), state()) -> tree().

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
            case SubtreeKeyNo == KeyNoMax of
                % Split node.
                true ->
                    {SplitKeyValues, SplitSubtrees1, SplitTree1, SplitTree2, SplitSubtrees2} = split_node_non_root(KeyNo, KeyValues, Subtrees, Subtree, SubtreePos, SubtreeNoMin, SortFunction, nil),
                    {
                        KeyNo + 1,
                        SubtreeNo + 1,
                        SplitKeyValues,
                            SplitSubtrees1 ++
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
                            end ++
                            SplitSubtrees2
                    };
                _ ->
                    {
                        KeyNo,
                        SubtreeNo,
                        KeyValues,
                            lists:sublist(Subtrees, 1, SubtreePos - 1) ++
                            [insert_1(KeyValue, Subtree, SubtreeNoMin, KeyNoMax, SortFunction, nil)] ++
                            lists:sublist(Subtrees, SubtreePos + 1, KeyNo + 1)
                    }
            end;
        _ ->
            erlang:error({key_exists, Key})
    end;
% Non-Leaf node.
insert_1({Key, _} = KeyValue, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}, SubtreeNoMin, KeyNoMax, SortFunction, {StateTarget, DeleteFunction, InsertFunction, LookupFunction} = State) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    ok = DeleteFunction(StateTarget, delete, SubtreesKey),
    {ValueFound, SubtreePos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            % Look ahead.
            {SubtreeKeyNo, _, SubtreeKeyValues, _} = Subtree = lists:nth(SubtreePos, Subtrees),
            case SubtreeKeyNo == KeyNoMax of
                % Split node.
                true ->
                    {SplitKeyValues, SplitSubtrees1, SplitTree1, SplitTree2, SplitSubtrees2} = split_node_non_root(KeyNo, KeyValues, Subtrees, Subtree, SubtreePos, SubtreeNoMin, SortFunction, State),
                    {
                        KeyNo + 1,
                        SubtreeNo + 1,
                        SplitKeyValues,
                        InsertFunction(StateTarget,
                            insert,
                            SplitSubtrees1 ++
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
                                end ++
                                SplitSubtrees2
                        )

                    };
                _ ->
                    {
                        KeyNo,
                        SubtreeNo,
                        KeyValues,
                        InsertFunction(StateTarget,
                            insert,
                            lists:sublist(Subtrees, 1, SubtreePos - 1) ++
                                [insert_1(KeyValue, Subtree, SubtreeNoMin, KeyNoMax, SortFunction, State)] ++
                                lists:sublist(Subtrees, SubtreePos + 1, KeyNo + 1)
                        )
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
            lists:sublist(KeyValues, 1, KeyPos - 1) ++ [KeyValue] ++ lists:sublist(KeyValues, KeyPos, KeyNo);
        _ ->
            erlang:error({key_exists, Key})
    end.

-spec split_node_non_root(pos_integer(), key_values(), subtrees(), tree(), pos_integer(), pos_integer(), sort_function(), state()) -> {key_values(), subtrees(), tree(), tree(), subtrees()}.

split_node_non_root(KeyNo, KeyValues, Subtrees, {TreeKeyNo, TreeSubtreeNo, TreeKeyValues, TreeSubtrees}, SubtreePos, SubtreeNoMin, SortFunction, nil) ->
    {
        % SplitKeyValues .......................................................
        insert_key_value(lists:nth(SubtreeNoMin, TreeKeyValues), KeyValues, KeyNo, SortFunction),
        % SplitSubtrees1 .......................................................
        lists:sublist(Subtrees, 1, SubtreePos - 1),
        % SplitTree 1 ..........................................................
        {
            SubtreeNoMin - 1,
            case TreeSubtreeNo == 0 of
                true ->
                    0;
                _ ->
                    SubtreeNoMin
            end,
            lists:sublist(TreeKeyValues, 1, SubtreeNoMin - 1),
            % Leaf node to be splitted ?
            case TreeSubtreeNo == 0 of
                true ->
                    [];
                _ ->
                    lists:sublist(TreeSubtrees, 1, SubtreeNoMin)
            end
        },
        % SplitTree 2 ..........................................................
        {
            TreeKeyNo - SubtreeNoMin,
            case TreeSubtreeNo == 0 of
                true ->
                    0;
                _ ->
                    TreeKeyNo - SubtreeNoMin + 1
            end,
            lists:sublist(TreeKeyValues, SubtreeNoMin + 1, TreeKeyNo),
            % Leaf node to be splitted ?
            case TreeSubtreeNo == 0 of
                true ->
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
    ok = DeleteFunction(StateTarget, delete, TreeSubtreesKey),
    {
        % SplitKeyValues .......................................................
        insert_key_value(lists:nth(SubtreeNoMin, TreeKeyValues), KeyValues, KeyNo, SortFunction),
        % SplitSubtrees1 .......................................................
        lists:sublist(Subtrees, 1, SubtreePos - 1),
        % SplitTree 1 ..........................................................
        {
            SubtreeNoMin - 1,
            case TreeSubtreeNo == 0 of
                true ->
                    0;
                _ ->
                    SubtreeNoMin
            end,
            lists:sublist(TreeKeyValues, 1, SubtreeNoMin - 1),
            % Leaf node to be splitted ?
            case TreeSubtreeNo == 0 of
                true ->
                    [];
                _ ->
                    InsertFunction(StateTarget,
                        insert,
                        lists:sublist(TreeSubtrees, 1, SubtreeNoMin)
                    )
            end
        },
        % SplitTree 2 ..........................................................
        {
            TreeKeyNo - SubtreeNoMin,
            case TreeSubtreeNo == 0 of
                true ->
                    0;
                _ ->
                    TreeKeyNo - SubtreeNoMin + 1
            end,
            lists:sublist(TreeKeyValues, SubtreeNoMin + 1, TreeKeyNo),
            % Leaf node to be splitted ?
            case TreeSubtreeNo == 0 of
                true ->
                    [];
                _ ->
                    InsertFunction(StateTarget,
                        insert,
                        lists:sublist(TreeSubtrees, SubtreeNoMin + 1, TreeKeyNo + 1)
                    )
            end
        },
        % SplitSubtrees2 .......................................................
        lists:sublist(Subtrees, SubtreePos + 1, KeyNo + 1)
    }.

-spec split_node_root(tree(), pos_integer(), state()) -> tree().

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
        InsertFunction(StateTarget,
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
            ]
        )
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
        InsertFunction(StateTarget,
            insert,
            [
                {
                    SubtreeNoMin - 1,
                    SubtreeNoMin,
                    lists:sublist(KeyValues, 1, SubtreeNoMin - 1),
                    InsertFunction(StateTarget,
                        insert,
                        lists:sublist(Subtrees, 1, SubtreeNoMin)
                    )
                },
                {
                    KeyNo - SubtreeNoMin,
                    KeyNo - SubtreeNoMin + 1,
                    lists:sublist(KeyValues, SubtreeNoMin + 1, KeyNo),
                    InsertFunction(StateTarget,
                        insert,
                        lists:sublist(Subtrees, SubtreeNoMin + 1, SubtreeNo)
                    )
                }
            ]
        )
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is a specialized version of `lookup'.

-spec is_defined(key(), b_tree()) -> boolean().

is_defined(_, {_, _, 0, _, _, nil}) ->
    false;
is_defined(Key, {_, _, _, SortFunction, State, Tree}) ->
    case lookup_1(Key, Tree, SortFunction, State) == none of
        true ->
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

-spec iterator(b_tree()) -> iterator().

iterator({_, _, 0, _, _, nil}) ->
    [];
iterator({_, _, _, _, State, {_, _, KeyValues, Subtrees}}) ->
    iterator_1({KeyValues, Subtrees, State}, []).

% The iterator structure is really just a list corresponding to
% the call stack of an in-order traversal. This is quite fast.

-spec iterator_1({key_values(), subtrees(), state()}, iterator()) -> iterator().

% The most left key / value.
iterator_1({KeyValues, [], State}, Iterator) ->
    [{KeyValues, [], State} | Iterator];
% The most left subtree.
iterator_1({KeyValues, Subtrees, nil}, Iterator) ->
    {_, _, KeyValues_1, Subtrees_1} = lists:nth(1, Subtrees),
    iterator_1({KeyValues_1, Subtrees_1, nil}, [{KeyValues, Subtrees, nil} | Iterator]);
iterator_1({KeyValues, Subtrees, State}, Iterator) when is_list(Subtrees) ->
    {_, _, KeyValues_1, Subtrees_1} = lists:nth(1, Subtrees),
    iterator_1({KeyValues_1, Subtrees_1, State}, [{KeyValues, Subtrees, State} | Iterator]);
iterator_1({KeyValues, SubtreesKey, {StateTarget, _, _, LookupFunction} = State}, Iterator) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    {_, _, KeyValues_1, Subtrees_1Key} = lists:nth(1, Subtrees),
    Subtrees_1 = LookupFunction(StateTarget, lookup, Subtrees_1Key),
    iterator_1({KeyValues_1, Subtrees_1, State}, [{KeyValues, Subtrees, State} | Iterator]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec iterator_from(key(), b_tree()) -> iterator().

iterator_from(_, {_, _, 0, _, _, nil}) ->
    [];
iterator_from(Key, {_, _, _, SortFunction, State, Tree}) ->
    iterator_from_1(Key, Tree, [], SortFunction, State).

-spec iterator_from_1(key(), tree(), iterator(), sort_function(), state()) -> iterator().

% The most left key / value.
iterator_from_1(Key, {KeyNo, 0, KeyValues, []}, Iterator, SortFunction, State) ->
    {_, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    [{lists:sublist(KeyValues, Pos, KeyNo), [], State} | Iterator];
% The most left subtree.
iterator_from_1(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, Iterator, SortFunction, nil) ->
    {_, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    {KeyNo_1, SubtreeNo_1, KeyValues_1, Subtrees_1} = lists:nth(Pos, Subtrees),
    iterator_from_1(Key, {KeyNo_1, SubtreeNo_1, KeyValues_1, Subtrees_1}, [{lists:sublist(KeyValues, Pos, KeyNo), lists:sublist(Subtrees, Pos, SubtreeNo), nil} | Iterator], SortFunction, nil);
iterator_from_1(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, Iterator, SortFunction, State) when is_list(Subtrees) ->
    {_, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    {KeyNo_1, SubtreeNo_1, KeyValues_1, Subtrees_1} = lists:nth(Pos, Subtrees),
    iterator_from_1(Key, {KeyNo_1, SubtreeNo_1, KeyValues_1, Subtrees_1}, [{lists:sublist(KeyValues, Pos, KeyNo), lists:sublist(Subtrees, Pos, SubtreeNo), State} | Iterator], SortFunction, State);
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
    keys_1(Tail, [], State, Keys ++ [Key]);
% The most right subtree.
keys_1([], [{_, _, KeyValues, Subtrees}], nil, Keys) ->
    Keys ++ keys_1(KeyValues, Subtrees, nil, []);
keys_1([], [{_, _, KeyValues, SubtreesKey}], {StateTarget, _, _, LookupFunction} = State, Keys) ->
    Keys ++ keys_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []);
% Left subtree and current key.
keys_1([{Key, _} | TailKeyValues], [{_, _, KeyValues, Subtrees} | TailSubtrees], nil, Keys) ->
    keys_1(TailKeyValues, TailSubtrees, nil, Keys ++ keys_1(KeyValues, Subtrees, nil, []) ++ [Key]);
keys_1([{Key, _} | TailKeyValues], [{_, _, KeyValues, SubtreesKey} | TailSubtrees], {StateTarget, _, _, LookupFunction} = State, Keys) ->
    keys_1(TailKeyValues, TailSubtrees, State, Keys ++ keys_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []) ++ [Key]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec largest(b_tree()) -> key_value().

largest({_, _, 0, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
largest({_, _, _, _, nil, Tree}) ->
    largest_1(Tree, nil);
largest({_, _, _, _, {StateTarget, _, _, LookupFunction} = State, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}}) ->
    largest_1({KeyNo, SubtreeNo, KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey)}, State).

-spec largest_1(tree(), state()) -> key_value().

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
map(Function, {SubtreeNoMin, KeyNoMax, NumberKeyValues, SortFunction, State, Tree}) when is_function(Function, 2) ->
    {SubtreeNoMin, KeyNoMax, NumberKeyValues, SortFunction, State, map_tree(Function, State, Tree)}.

-spec map_key_values(map_function(), key_values(), key_values()) -> key_values().

map_key_values(_, [], KeyValuesMapped) ->
    KeyValuesMapped;
map_key_values(Function, [{Key, Value} | Tail], KeyValuesMapped) ->
    map_key_values(Function, Tail, KeyValuesMapped ++ [{Key, Function(Key, Value)}]).

-spec map_tree(map_function(), state(), tree()) -> tree().

% Leaf node.
map_tree(Function, _, {KeyNo, 0, KeyValues, []}) ->
    {KeyNo, 0, map_key_values(Function, KeyValues, []), []};
map_tree(Function, nil, {KeyNo, SubtreeNo, KeyValues, Subtrees}) ->
    {KeyNo, SubtreeNo, map_key_values(Function, KeyValues, []), map_subtrees(Function, nil, Subtrees, [])};
map_tree(Function, {StateTarget, DeleteFunction, InsertFunction, LookupFunction} = State, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    ok = DeleteFunction(StateTarget, delete, SubtreesKey),
    {KeyNo, SubtreeNo, map_key_values(Function, KeyValues, []), InsertFunction(StateTarget, insert, map_subtrees(Function, State, Subtrees, []))}.

-spec map_subtrees(map_function(), state(), subtrees(), subtrees()) -> subtrees().

map_subtrees(_, _, [], TreesMapped) ->
    TreesMapped;
map_subtrees(Function, State, [Tree | Tail], TreesMapped) ->
    map_subtrees(Function, State, Tail, TreesMapped ++ [map_tree(Function, State, Tree)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec next(iterator()) -> 'none' | {key(), value(), iterator()}.

% One level up.
next([{[], _, _}, {[], _, _} = Iterator | TailIterator]) ->
    next([Iterator] ++ TailIterator);
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

-spec number_key_values(b_tree()) -> non_neg_integer().

number_key_values({_, _, NumberKeyValues, _, _, _}) ->
    NumberKeyValues.

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

-spec size(b_tree()) -> non_neg_integer().

size({_, _, 0, _, _, nil}) ->
    0;
size({_, _, _, _, nil, Tree}) ->
    size_tree(Tree, nil, 0);
size({_, _, _, _, {StateTarget, _, _, LookupFunction} = State, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}}) ->
    size_tree({KeyNo, SubtreeNo, KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey)}, State, 0).

-spec size_tree(tree(), state(), non_neg_integer()) -> non_neg_integer().

% Leaf node.
size_tree({_, 0, _, []}, _, Number) ->
    Number + 1;
size_tree({_, _, _, Subtrees}, State, Number) ->
    size_subtrees(Subtrees, State, Number + 1).

-spec size_subtrees(subtrees(), state(), non_neg_integer()) -> non_neg_integer().

size_subtrees([], _, Number) ->
    Number;
size_subtrees([Tree | Tail], nil, Number) ->
    size_subtrees(Tail, nil, size_tree(Tree, nil, Number));
size_subtrees([{KeyNo, SubtreeNo, KeyValues, SubtreesKey} | Tail], {StateTarget, _, _, LookupFunction} = State, Number) ->
    size_subtrees(Tail, State, size_tree({KeyNo, SubtreeNo, KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey)}, State, Number)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec smallest(b_tree()) -> key_value().

smallest({_, _, 0, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
smallest({_, _, _, _, nil, Tree}) ->
    smallest_1(Tree, nil);
smallest({_, _, _, _, {StateTarget, _, _, LookupFunction} = State, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}}) ->
    smallest_1({KeyNo, SubtreeNo, KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey)}, State).

-spec smallest_1(tree(), state()) -> key_value().

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
    to_list_1(Tail, [], State, KeyValueList ++ [KeyValue]);
% The most right subtree.
to_list_1([], [{_, _, KeyValues, Subtrees}], nil, KeyValueList) ->
    KeyValueList ++ to_list_1(KeyValues, Subtrees, nil, []);
to_list_1([], [{_, _, KeyValues, SubtreesKey}], {StateTarget, _, _, LookupFunction} = State, KeyValueList) ->
    KeyValueList ++ to_list_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []);
% Left subtree and key / value from current key.
to_list_1([KeyValue | TailKeyValues], [{_, _, KeyValues, Subtrees} | TailSubtrees], nil, KeyValueList) ->
    to_list_1(TailKeyValues, TailSubtrees, nil, KeyValueList ++ to_list_1(KeyValues, Subtrees, nil, []) ++ [KeyValue]);
to_list_1([KeyValue | TailKeyValues], [{_, _, KeyValues, SubtreesKey} | TailSubtrees], {StateTarget, _, _, LookupFunction} = State, KeyValueList) ->
    to_list_1(TailKeyValues, TailSubtrees, State, KeyValueList ++ to_list_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []) ++ [KeyValue]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec update(key(), value(), b_tree()) -> b_tree().

update(Key, _, {_, _, 0, _, _, nil}) ->
    erlang:error({key_not_found, Key});
update(Key, Value, {SubtreeNoMin, KeyNoMax, NumberKeyValues, SortFunction, State, Tree}) ->
    {SubtreeNoMin, KeyNoMax, NumberKeyValues, SortFunction, State, update_1({Key, Value}, Tree, SortFunction, State)}.

-spec update_1(key_value(), tree(), sort_function(), state()) -> tree().

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
                    lists:sublist(KeyValues, 1, KeyPos - 1) ++
                    [KeyValue] ++
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo),
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
                    lists:sublist(Subtrees, 1, KeyPos - 1) ++
                    [update_1(KeyValue, lists:nth(KeyPos, Subtrees), SortFunction, nil)] ++
                    lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
            };
        _ ->
            {
                KeyNo,
                SubtreeNo,
                    lists:sublist(KeyValues, 1, KeyPos - 1) ++
                    [KeyValue] ++
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                Subtrees
            }
    end;
update_1({Key, _} = KeyValue, {KeyNo, SubtreeNo, KeyValues, SubtreesKey}, SortFunction, {StateTarget, DeleteFunction, InsertFunction, LookupFunction} = State) ->
    Subtrees = LookupFunction(StateTarget, lookup, SubtreesKey),
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case ValueFound of
        none ->
            ok = DeleteFunction(StateTarget, delete, SubtreesKey),
            {
                KeyNo,
                SubtreeNo,
                KeyValues,
                InsertFunction(StateTarget,
                    insert,
                    lists:sublist(Subtrees, 1, KeyPos - 1) ++
                        [update_1(KeyValue, lists:nth(KeyPos, Subtrees), SortFunction, State)] ++
                        lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                )
            };
        _ ->
            {
                KeyNo,
                SubtreeNo,
                    lists:sublist(KeyValues, 1, KeyPos - 1) ++
                    [KeyValue] ++
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo),
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
    values_1(Tail, [], State, Values ++ [Value]);
% The most right subtree.
values_1([], [{_, _, KeyValues, Subtrees}], nil, Values) ->
    Values ++ values_1(KeyValues, Subtrees, nil, []);
values_1([], [{_, _, KeyValues, SubtreesKey}], {StateTarget, _, _, LookupFunction} = State, Values) ->
    Values ++ values_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []);
% Left subtree and value from current key.
values_1([{_, Value} | TailKeyValues], [{_, _, KeyValues, Subtrees} | TailSubtrees], nil, Values) ->
    values_1(TailKeyValues, TailSubtrees, nil, Values ++ values_1(KeyValues, Subtrees, nil, []) ++ [Value]);
values_1([{_, Value} | TailKeyValues], [{_, _, KeyValues, SubtreesKey} | TailSubtrees], {StateTarget, _, _, LookupFunction} = State, Values) ->
    values_1(TailKeyValues, TailSubtrees, State, Values ++ values_1(KeyValues, LookupFunction(StateTarget, lookup, SubtreesKey), State, []) ++ [Value]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec binary_search(key(), key_values(), pos_integer(), pos_integer(), pos_integer(), sort_function()) -> {none, pos_integer()} | {any(), pos_integer()}.

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

-spec lookup_1(key(), tree(), sort_function(), state()) -> 'none' | {'value', value()}.

% Leaf node.
lookup_1(Key, {KeyNo, 0, KeyValues, []}, SortFunction, _) ->
    {Value, _} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case Value == none of
        true ->
            Value;
        _ ->
            {value, Value}
    end;
% Non-Leaf node.
lookup_1(Key, {KeyNo, _, KeyValues, Subtrees}, SortFunction, nil) ->
    {Value, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case Value == none of
        true ->
            lookup_1(Key, lists:nth(Pos, Subtrees), SortFunction, nil);
        _ ->
            {value, Value}
    end;
lookup_1(Key, {KeyNo, _, KeyValues, SubtreesKey}, SortFunction, {StateTarget, _, _, LookupFunction} = State) ->
    {Value, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo, SortFunction),
    case Value == none of
        true ->
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

direct_test() ->
    %% ?debugFmt("wwe debugging direct_test ===> ~n b_trees:empty(6): ~p~n", [b_trees:empty(6, fun sort_ascending/2)]),

%%    B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2),
%%
%%    ?assertEqual(2, b_trees:height(test_generator:prepare_template_desc(B_TREE_06_32))),


%%    StateTarget = ets:new(b_trees, [ordered_set, {keypos, 1}]),
%%
%%    B_TREE_06_00 = b_trees:set_parameter(b_trees:empty(6), state, {StateTarget, fun test_generator:persistance_by_ets/3, fun test_generator:persistance_by_ets/3, fun test_generator:persistance_by_ets/3}),
%%
%%    ?assertEqual(32, b_trees:number_key_values(b_trees:from_dict(B_TREE_06_00, test_generator:generate_key_values_from(32, 2)))),


%%    StateTarget = ets:new(b_trees, [ordered_set, {keypos, 1}]),
%%
%%    B_TREE_06_00 = b_trees:set_parameter(b_trees:empty(6), state, {StateTarget, fun test_generator:persistance_by_ets/3, fun test_generator:persistance_by_ets/3, fun test_generator:persistance_by_ets/3}),
%%
%%    ?assertEqual(true, b_trees:is_empty(test_generator:prepare_template_asc(B_TREE_06_00))),
%%
%%    ets:delete(StateTarget),

%%    B_TREE_06_32 = test_generator:generate_b_tree_from_number_ets(6, 32, 2),
%%
%%    ?assertEqual(14, b_trees:size(test_generator:prepare_template_desc(B_TREE_06_32))),

%%    {_, _, _, _, {StateTarget, _, _, _}, _} = B_TREE_04_32_30 = test_generator:generate_b_tree_from_number_ets(4, 30, 2),
%%
%%    B_TREE_04_32_31 = b_trees:enter("k_31", "v_31", B_TREE_04_32_30),
%%    B_TREE_04_32_32 = b_trees:enter("k_32", "v_32", B_TREE_04_32_31),
%%
%%    B_TREE_04_32_01_NEW = b_trees:enter("k_01", "v_01_new", B_TREE_04_32_32),
%%    B_TREE_04_32_02_NEW = b_trees:enter("k_02", "v_02_new", B_TREE_04_32_01_NEW),
%%    B_TREE_04_32_03_NEW = b_trees:enter("k_03", "v_03_new", B_TREE_04_32_02_NEW),
%%    B_TREE_04_32_04_NEW = b_trees:enter("k_04", "v_04_new", B_TREE_04_32_03_NEW),
%%    B_TREE_04_32_05_NEW = b_trees:enter("k_05", "v_05_new", B_TREE_04_32_04_NEW),
%%    B_TREE_04_32_06_NEW = b_trees:enter("k_06", "v_06_new", B_TREE_04_32_05_NEW),
%%    B_TREE_04_32_07_NEW = b_trees:enter("k_07", "v_07_new", B_TREE_04_32_06_NEW),
%%    B_TREE_04_32_08_NEW = b_trees:enter("k_08", "v_08_new", B_TREE_04_32_07_NEW),
%%    B_TREE_04_32_09_NEW = b_trees:enter("k_09", "v_09_new", B_TREE_04_32_08_NEW),
%%    B_TREE_04_32_10_NEW = b_trees:enter("k_10", "v_10_new", B_TREE_04_32_09_NEW),
%%    B_TREE_04_32_11_NEW = b_trees:enter("k_11", "v_11_new", B_TREE_04_32_10_NEW),
%%    B_TREE_04_32_12_NEW = b_trees:enter("k_12", "v_12_new", B_TREE_04_32_11_NEW),
%%    B_TREE_04_32_13_NEW = b_trees:enter("k_13", "v_13_new", B_TREE_04_32_12_NEW),
%%    B_TREE_04_32_14_NEW = b_trees:enter("k_14", "v_14_new", B_TREE_04_32_13_NEW),
%%    B_TREE_04_32_15_NEW = b_trees:enter("k_15", "v_15_new", B_TREE_04_32_14_NEW),
%%    B_TREE_04_32_16_NEW = b_trees:enter("k_16", "v_16_new", B_TREE_04_32_15_NEW),
%%    B_TREE_04_32_17_NEW = b_trees:enter("k_17", "v_17_new", B_TREE_04_32_16_NEW),
%%    B_TREE_04_32_18_NEW = b_trees:enter("k_18", "v_18_new", B_TREE_04_32_17_NEW),
%%    B_TREE_04_32_19_NEW = b_trees:enter("k_19", "v_19_new", B_TREE_04_32_18_NEW),
%%    B_TREE_04_32_20_NEW = b_trees:enter("k_20", "v_20_new", B_TREE_04_32_19_NEW),
%%    B_TREE_04_32_21_NEW = b_trees:enter("k_21", "v_21_new", B_TREE_04_32_20_NEW),
%%    B_TREE_04_32_22_NEW = b_trees:enter("k_22", "v_22_new", B_TREE_04_32_21_NEW),
%%    B_TREE_04_32_23_NEW = b_trees:enter("k_23", "v_23_new", B_TREE_04_32_22_NEW),
%%    B_TREE_04_32_24_NEW = b_trees:enter("k_24", "v_24_new", B_TREE_04_32_23_NEW),
%%    B_TREE_04_32_25_NEW = b_trees:enter("k_25", "v_25_new", B_TREE_04_32_24_NEW),
%%    B_TREE_04_32_26_NEW = b_trees:enter("k_26", "v_26_new", B_TREE_04_32_25_NEW),
%%    B_TREE_04_32_27_NEW = b_trees:enter("k_27", "v_27_new", B_TREE_04_32_26_NEW),
%%    B_TREE_04_32_28_NEW = b_trees:enter("k_28", "v_28_new", B_TREE_04_32_27_NEW),
%%    B_TREE_04_32_29_NEW = b_trees:enter("k_29", "v_29_new", B_TREE_04_32_28_NEW),
%%    B_TREE_04_32_30_NEW = b_trees:enter("k_30", "v_30_new", B_TREE_04_32_29_NEW),
%%    B_TREE_04_32_31_NEW = b_trees:enter("k_31", "v_31_new", B_TREE_04_32_30_NEW),
%%    B_TREE_04_32_32_NEW = b_trees:enter("k_32", "v_32_new", B_TREE_04_32_31_NEW),
%%
%%    B_TREE_04_32_MAPPED_VALUES = b_trees:to_list(B_TREE_04_32_32_NEW),
%%
%%    ?assertEqual(B_TREE_04_32_MAPPED_VALUES, test_generator:generate_key_values_from_update(32, 2)),
%%
%%    ?assertEqual(11, length(ets:tab2list(StateTarget))),
%%
%%    ets:delete(StateTarget),

%%    ?debugFmt("wwe debugging direct_test ===> ~n ets:tab2list(StateTarget): ~p~n length: ~p~n", [ets:tab2list(StateTarget), length(ets:tab2list(StateTarget))]),
%%    test_generator:check_equal(?B_TREE_06_00, b_trees:update("k_32", "v_32_new", _B_TREE_06_32_K_31)),




    B_TREE_04_32 = test_generator:generate_b_tree_from_number(4, 32, 2),

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

    ok.

