-define(NODEBUG, true).

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
%% Computer Programming": The order O of a B-tree is an even number expressing
%% the maximum number of subtrees (sons) in a node and the height of a B-tree is
%% its maximum level, i.e. the length of the longest path from the root node to
%% a leaf node. The minimum order of a B-tree is 4.
%% -----------------------------------------------------------------------------
%% Operations:
%%
%% - delete(K, B): removes key K from B-tree B; returns new tree. Assumes that
%%   the key is present in the tree.
%%
%% - delete_any(K, T): removes key K from B-tree B if the key is present in the
%%   tree, otherwise does nothing; returns new tree.
%%
%% - empty(O): returns empty B-tree of order O. Order is defined as the maximum
%%   number of children nodes a non-leaf node may hold. The minimum value is 4.
%%
%% - enter(K, V, B): inserts key K with value V into B-tree B if the key is not
%%   present in the tree, otherwise updates key K to value V in B. Returns the
%%   new tree.
%%
%% - from_dict(O, L): turns a list L of {Key, Value} pairs into a B-tree of
%%   order O. The list must not contain duplicate keys.
%%
%% - get(K, B): retrieves the value stored with key K in B-tree B. Assumes that
%%   the key is present in the tree.
%%
%% - height(B): returns the height of the B-tree B as an integer. Assumes that
%%   the B-tree B is nonempty.
%%
%% - insert(K, V, B): inserts key K with value V into B-tree B; returns the new
%%   B-tree. Assumes that the key K is *not* present in the B-tree B.
%%
%% - is_defined(K, B): returns `true' if key K is present in B-tree B, and
%%   `false' otherwise.
%%
%% - is_empty(B): returns 'true' if B is an empty B-tree, and 'false' otherwise.
%%
%% - iterator(B): returns an iterator that can be used for traversing the
%%   entries of B-tree B; see `next'. The implementation of this is very
%%   efficient; traversing the whole tree using `next' is only slightly slower
%%   than getting the list of all elements using `to_list' and traversing that.
%%   The main advantage of the iterator approach is that it does not require the
%%   complete list of all elements to be built in memory at one time.
%%
%% - keys(B): returns an ordered list of all keys in B-tree B.
%%
%% - largest(B): returns {K, V}, where K is the largest key in B-tree B, and V
%%   is the value associated with K in B. Assumes that the B-tree B is nonempty.
%%
%% - lookup(K, B): looks up key K in B-tree B; returns {value, V}, or `none' if
%%   the key K is not present.
%%
%% - map(F, B): maps the function F(K, V) -> V' to all key-value pairs of the
%%   B-tree B and returns a new B-tree B' with the same set of keys as B and
%%   the new set of values V'.
%%
%% - next(I): returns {K, V, I1} where K is the smallest key referred to by
%%   the iterator I, and I1 is the new iterator to be used for traversing the
%%   remaining entries, or the atom `none' if no entries remain.
%%
%% - number_key_values(B): returns the number of key / value pairs in the
%%   B-tree B as an integer. Returns 0 (zero) if the B-tree B is empty.
%%
%% - size(B): returns the number of nodes in the B-tree B as an integer.
%%   Returns 0 (zero) if the B-tree B is empty.
%%
%% - smallest(B): returns {K, V}, where K is the smallest key in B-tree B, and V
%%   is the value associated with K in B. Assumes that the B-tree B is nonempty.
%%
%% - take_largest(B): returns {K, V, B1}, where K is the largest key in B-tree
%%   B, V is the value associated with K in B, and B1 is the tree B with key K
%%   deleted. Assumes that the tree B is nonempty.
%%
%% - take_smallest(B): returns {K, V, B1}, where K is the smallest key in
%%   B-tree B, V is the value associated with K in B, and B1 is the tree B with
%%   key K deleted. Assumes that the tree B is nonempty.
%%
%% - to_list(B): returns an ordered list of {Key, Value} pairs for all keys in
%%   B-tree B.
%%
%% - update(K, V, B): updates key K to value V in B-tree B; returns the new
%%   tree. Assumes that the key is present in the tree.
%%
%% - values(B): returns the list of values for all keys in B-tree B, sorted by
%%   their corresponding keys. Duplicates are not removed.
%%

-module(b_trees).

-export([
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
    keys/1,
    largest/1,
    lookup/2,
    map/2,
    next/1,
    number_key_values/1,
    size/1,
    smallest/1,
    take_smallest/1,
    take_largest/1,
    to_list/1,
    update/3,
    values/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data structure:
%% - {BTreeType, MinimumSubtrees, MaximumKeys, NumberKeyValues, Tree},
%%   where `Tree' is composed of :
%%   - {KeyNo, SubtreeNo, [{Key, Value}], [Tree]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

-export_type([b_tree/0]).

-type b_tree() :: {pos_integer(), pos_integer(), non_neg_integer(), tree()}.

-type iterator() :: [{key_values(), subtrees()}].

-type key() :: any().
-type keys() :: [any()].

-type key_value() :: {key(), value()}.
-type key_values() :: [key_value()].

-type map_function() :: fun((key(), value()) -> value()).

-type subtrees() :: [tree()].

-type tree() :: 'nil'
| {pos_integer(), pos_integer(), key_values(), []}
| {pos_integer(), pos_integer(), key_values(), subtrees()}.

-type value() :: any().
-type values() :: [any()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% delete. Assumes that key is present.

-spec delete(key(), b_tree()) -> b_tree().

% Empty tree.
delete(Key, {_, _, 0, nil} = _BTree) ->
    ?debugFmt("wwe debugging delete ===> Start ~n Key: ~p~n BTree: ~p~n", [Key, _BTree]),
    erlang:error({key_not_found, Key});
% Root node is leaf node.
delete(Key, {SubtreeNoMin, KeyNoMax, NumberKeyValues, {KeyNo, 0, KeyValues, []}} = _BTree) ->
    ?debugFmt("wwe debugging delete ===> Start ~n Key: ~p~n BTree: ~p~n", [Key, _BTree]),
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case ValueFound of
        none ->
            erlang:error({key_not_found, Key});
        _ ->
            {SubtreeNoMin, KeyNoMax, NumberKeyValues - 1, case KeyNo == 1 of
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
delete(Key, {SubtreeNoMin, KeyNoMax, NumberKeyValues, Tree} = _BTree) ->
    ?debugFmt("wwe debugging delete ===> Start ~n Key: ~p~n BTree: ~p~n", [Key, _BTree]),
    {SubtreeNoMin, KeyNoMax, NumberKeyValues - 1, delete_1(Key, Tree, SubtreeNoMin, KeyNoMax)}.

-spec combine(tree(), tree()) -> tree().

combine({LeftKeyNo, 0, LeftKeyValues, []} = _LeftTree, {RightKeyNo, 0, RightKeyValues, []} = _RightTree) ->
    ?debugFmt("wwe debugging combine ===> Start ~n LeftTree: ~p~n RightTree: ~p~n", [_LeftTree, _RightTree]),
    {
        LeftKeyNo + RightKeyNo,
        0,
            LeftKeyValues ++ RightKeyValues,
        []
    };
combine({LeftKeyNo, LeftSubtreeNo, LeftKeyValues, LeftSubtrees} = _LeftTree, {RightKeyNo, RightSubtreeNo, RightKeyValues, RightSubtrees} = _RightTree) ->
    ?debugFmt("wwe debugging combine ===> Start ~n LeftTree: ~p~n RightTree: ~p~n", [_LeftTree, _RightTree]),
    {
        LeftKeyNo + RightKeyNo,
        LeftSubtreeNo + RightSubtreeNo - 1,
            LeftKeyValues ++ RightKeyValues,
            lists:sublist(LeftSubtrees, 1, LeftKeyNo) ++ [combine(lists:nth(LeftSubtreeNo, LeftSubtrees), lists:nth(1, RightSubtrees))] ++ lists:sublist(RightSubtrees, 2, RightSubtreeNo)
    }.

-spec delete_1(key(), tree(), pos_integer(), pos_integer()) -> tree().

% Leaf node.
delete_1(Key, {KeyNo, 0, KeyValues, []} = _Tree, _, _) ->
    ?debugFmt("wwe debugging delete_1 ===> Start ~n Key: ~p~n Tree: ~p~n", [Key, _Tree]),
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    Out = case ValueFound of
              none ->
                  erlang:error({key_not_found, Key});
              _ ->
                  % CLRS: case 1 leaf node
                  ?debugFmt("wwe debugging delete_1 ===> ~n CLRS: case 1 leaf node ~n", []),
                  {
                      KeyNo - 1,
                      0,
                          lists:sublist(KeyValues, 1, KeyPos - 1) ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                      []
                  }
          end,
    ?debugFmt("wwe debugging delete_1 ===> Start ~n Out: ~p~n", [Out]),
    Out;
delete_1(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees} = _Tree, SubtreeNoMin, KeyNoMax) ->
    ?debugFmt("wwe debugging delete_1 ===> Start ~n Key: ~p~n Tree: ~p~n", [Key, _Tree]),
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    ?debugFmt("wwe debugging delete_1 ===> ~n KeyPos: ~p~n", [KeyPos]),
    Out = case ValueFound of
              none ->
                  delete_1_3(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, KeyNoMax, KeyPos);
              _ ->
                  delete_1_2(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, KeyNoMax, KeyPos)
          end,
    ?debugFmt("wwe debugging delete_1 ===> Start ~n Out: ~p~n", [Out]),
    Out.

-spec delete_1_2(key(), tree(), pos_integer(), pos_integer(), pos_integer()) -> tree().

delete_1_2(_Key, {KeyNo, SubtreeNo, KeyValues, Subtrees} = _Tree, SubtreeNoMin, _KeyNoMax, KeyPos) ->
    ?debugFmt("wwe debugging delete_1_2 ===> Start ~n Key: ~p~n Tree: ~p~n KeyPos: ~p~n", [_Key, _Tree, KeyPos]),
    {_KeyNoXCLeft, _SubtreeNoXCLeft, _KeyValuesXCLeft, _SubtreesXCLeft} = _TreeXCLeft = case KeyPos == 1 of
                                                                                            true ->
                                                                                                {0, 0, [], []};
                                                                                            _ ->
                                                                                                lists:nth(KeyPos - 1, Subtrees)
                                                                                        end,
    {KeyNoXC, SubtreeNoXC, KeyValuesXC, SubtreesXC} = _TreeXC = lists:nth(KeyPos, Subtrees),
    ?debugFmt("wwe debugging delete_1_2 ===> ~n KeyPos: ~p~n SubtreeNo: ~p~n", [KeyPos, SubtreeNo]),
    {KeyNoXCRight, SubtreeNoXCRight, KeyValuesXCRight, SubtreesXCRight} = _TreeXCRight = lists:nth(KeyPos + 1, Subtrees),
    ?debugFmt("wwe debugging delete_1_2 ===> ~n TreeXCLeft: ~p~n TreeX: ~p~n TreeXCRight: ~p~n", [_TreeXCLeft, _TreeXC, _TreeXCRight]),
    Out = case KeyNoXC >= SubtreeNoMin of
              true ->
                  % CLRS: case 2a
                  ?debugFmt("wwe debugging delete_1_2 ===> ~n CLRS: case 2a ~n", []),
                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Outer KeyValues: ~p~n", [lists:sublist(KeyValues, 1, KeyPos - 1) ++ [lists:nth(KeyNoXC, KeyValuesXC)] ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo)]),
                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Outer Subtrees Part 1: ~p~n", [lists:sublist(Subtrees, 1, KeyPos - 1)]),
                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Outer Subtrees Part 3: ~p~n", [lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)]),
                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Inner KeyValues: ~p~n", [KeyValuesXC]),
                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Inner Subtrees Part 1: ~p~n", [_SubtreesXCLeft]),
                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Inner Subtrees Part 2: ~p~n", [SubtreesXC]),
                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Inner Subtrees Part 3: ~p~n", [SubtreesXCRight]),
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
                          ?debugFmt("wwe debugging delete_1_2 ===> ~n CLRS: case 2b ~n", []),
                          ?debugFmt("wwe debugging delete_1_2 ===> ~n New Outer KeyValues: ~p~n", [lists:sublist(KeyValues, 1, KeyPos - 1) ++ [lists:nth(1, KeyValuesXCRight)]]),
                          ?debugFmt("wwe debugging delete_1_2 ===> ~n New Outer Subtrees Part 1: ~p~n", [lists:sublist(Subtrees, 1, KeyPos)]),
                          ?debugFmt("wwe debugging delete_1_2 ===> ~n New Outer Subtrees Part 3: ~p~n", [lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)]),
                          ?debugFmt("wwe debugging delete_1_2 ===> ~n New Inner KeyValues: ~p~n", [lists:sublist(KeyValuesXCRight, 2, KeyNoXCRight)]),
                          ?debugFmt("wwe debugging delete_1_2 ===> ~n New Inner Subtrees Combine Part 1: ~p~n", [case SubtreeNoXC == 0 of
                                                                                                                     true ->
                                                                                                                         [];
                                                                                                                     _ ->
                                                                                                                         lists:nth(SubtreeNoXC, SubtreesXC)
                                                                                                                 end]),
                          ?debugFmt("wwe debugging delete_1_2 ===> ~n New Inner Subtrees Combine Part 2: ~p~n", [case SubtreeNoXCRight == 0 of
                                                                                                                     true ->
                                                                                                                         [];
                                                                                                                     _ ->
                                                                                                                         lists:nth(1, SubtreesXCRight)
                                                                                                                 end]),
                          ?debugFmt("wwe debugging delete_1_2 ===> ~n New Inner Subtrees Part 3: ~p~n", [case SubtreeNoXCRight == 0 of
                                                                                                             true ->
                                                                                                                 [];
                                                                                                             _ ->
                                                                                                                 lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                                                                                         end]),
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
                          ?debugFmt("wwe debugging delete_1_2 ===> ~n CLRS: case 2c ~n", []),
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
                          ?debugFmt("wwe debugging delete_1_2 ===> ~n LeftTree: ~p~n RightTree: ~p~n", [LeftTree, RightTree]),
                          case KeyNo == 1 of
                              true ->
                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n CLRS: case 2c KeyNo == 1 ~n", []),
%%                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New KeyValues: ~p~n", [KeyValuesXC ++ KeyValuesXCRight]),
%%                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Subtrees Part 1: ~p~n", [lists:sublist(SubtreesXC, 1, KeyNoXC)]),
%%                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Subtrees Part 3: ~p~n", [lists:sublist(SubtreesXCRight, 2, KeyNoXCRight)]),
%%                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Subtrees Part x: ~p~n", [lists:sublist(SubtreesXCRight, 1, SubtreeNoXCRight)]),
%%                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Subtrees Part y: ~p~n", [lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)]),
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
                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n CLRS: case 2c KeyNo != 1 ~n", []),
                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Outer KeyValues: ~p~n", [lists:sublist(KeyValues, 1, KeyPos - 1) ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo)]),
                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Outer Subtrees Part 1: ~p~n", [lists:sublist(Subtrees, 1, KeyPos - 1)]),
                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Outer Subtrees Part 3: ~p~n", [lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)]),
                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Inner KeyValues: ~p~n", [KeyValuesXC ++ KeyValuesXCRight]),
                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Inner Subtrees Part 1: ~p~n", [lists:sublist(SubtreesXC, 1, KeyNoXC)]),
                                  ?debugFmt("wwe debugging delete_1_2 ===> ~n New Inner Subtrees Part 3: ~p~n", [lists:sublist(SubtreesXCRight, 1, SubtreeNoXCRight)]),
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
          end,
    ?debugFmt("wwe debugging delete_1_2 ===> Start ~n Out: ~p~n", [Out]),
    Out.

-spec delete_1_3(key(), tree(), pos_integer(), pos_integer(), pos_integer()) -> tree().

delete_1_3(Key, {KeyNo, SubtreeNo, KeyValues, Subtrees} = _Tree, SubtreeNoMin, KeyNoMax, KeyPos) ->
    ?debugFmt("wwe debugging delete_1_3 ===> Start ~n Key: ~p~n Tree: ~p~n KeyPos: ~p~n", [Key, _Tree, KeyPos]),
    {KeyNoXCLeft, SubtreeNoXCLeft, KeyValuesXCLeft, SubtreesXCLeft} = _TreeXCLeft = case KeyPos == 1 of
                                                                                        true ->
                                                                                            {0, 0, [], []};
                                                                                        _ ->
                                                                                            lists:nth(KeyPos - 1, Subtrees)
                                                                                    end,
    {KeyNoXC, SubtreeNoXC, KeyValuesXC, SubtreesXC} = TreeXC = lists:nth(KeyPos, Subtrees),
    {KeyNoXCRight, SubtreeNoXCRight, KeyValuesXCRight, SubtreesXCRight} = _TreeXCRight = case KeyPos == SubtreeNo of
                                                                                             true ->
                                                                                                 {0, 0, [], []};
                                                                                             _ ->
                                                                                                 lists:nth(KeyPos + 1, Subtrees)
                                                                                         end,
    ?debugFmt("wwe debugging delete_1_3 ===> ~n TreeXCLeft: ~p~n TreeX: ~p~n TreeXCRight: ~p~n", [_TreeXCLeft, TreeXC, _TreeXCRight]),
    Out = case KeyNoXC < SubtreeNoMin of
              true ->
                  if
                      KeyNoXCLeft >= SubtreeNoMin ->
                          % CLRS: case 3a - left
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n CLRS: case 3a - left  ~n", []),

                          ?debugFmt("wwe debugging delete_1_3 ===> ~n New KeyValues 1: ~p~n", [lists:sublist(KeyValues, 1, KeyPos - 2)]),

                          ?debugFmt("wwe debugging delete_1_3 ===> ~n New KeyValues 2.1: ~p~n", [KeyNoXCLeft]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n New KeyValues 2.2: ~p~n", [KeyValuesXCLeft]),

                          ?debugFmt("wwe debugging delete_1_3 ===> ~n New KeyValues 2: ~p~n", [[lists:nth(KeyNoXCLeft, KeyValuesXCLeft)]]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n New KeyValues 3: ~p~n", [lists:sublist(KeyValues, KeyPos, KeyNo)]),

                          ?debugFmt("wwe debugging delete_1_3 ===> ~n New KeyValues: ~p~n", [lists:sublist(KeyValues, 1, KeyPos - 2) ++ [lists:nth(KeyNoXCLeft, KeyValuesXCLeft)] ++ lists:sublist(KeyValues, KeyPos, KeyNo)]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n New Subtrees Part 1: ~p~n", [case SubtreeNo == 0 of
                                                                                                       true ->
                                                                                                           [];
                                                                                                       _ ->
                                                                                                           lists:sublist(Subtrees, 1, KeyPos - 2)
                                                                                                   end]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n New Subtrees Part 3: ~p~n", [case SubtreeNo == 0 of
                                                                                                       true ->
                                                                                                           [];
                                                                                                       _ ->
                                                                                                           lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                                                                                                   end]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n Nxt KeyValues: ~p~n", [lists:sublist(KeyValuesXCLeft, 1, KeyNoXCLeft - 1)]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n Nxt Subtreest: ~p~n", [lists:sublist(SubtreesXCLeft, 1, KeyNoXCLeft)]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n Del KeyValues: ~p~n", [[lists:nth(KeyPos - 1, KeyValues)] ++ lists:sublist(KeyValuesXC, 1, KeyNoXC)]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n Del SubtreesXCLeft: ~p~n", [case SubtreeNoXCLeft == 0 of
                                                                                                      true ->
                                                                                                          [];
                                                                                                      _ ->
                                                                                                          lists:nth(SubtreeNoXCLeft, SubtreesXCLeft)
                                                                                                  end]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n Del SubtreesXC: ~p~n", [SubtreesXC]),
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
                                                                                                                                                     end ++ SubtreesXC}, SubtreeNoMin, KeyNoMax)
                                  ] ++ lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                          };
                      KeyNoXCRight >= SubtreeNoMin ->
                          % CLRS: case 3a - right
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n CLRS: case 3a - right ~n", []),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n New KeyValues: ~p~n", [lists:sublist(KeyValues, 1, KeyPos - 1) ++ [lists:nth(1, KeyValuesXCRight)] ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo)]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n New Subtrees Part 1: ~p~n", [lists:sublist(Subtrees, 1, KeyPos - 1)]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n New Subtrees Part 3: ~p~n", [lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n Del KeyValues: ~p~n", [lists:sublist(KeyValuesXC, 1, KeyNoXC) ++ [lists:nth(KeyPos, KeyValues)]]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n Del SubtreesXC: ~p~n", [SubtreesXC]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n Del SubtreesXCRight: ~p~n", [case SubtreeNoXCRight == 0 of
                                                                                                       true ->
                                                                                                           [];
                                                                                                       _ ->
                                                                                                           lists:nth(1, SubtreesXCRight)
                                                                                                   end]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n Nxt KeyValues: ~p~n", [lists:sublist(KeyValuesXCRight, 2, KeyNoXCRight)]),
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n Nxt Subtreest: ~p~n", [case SubtreeNoXCRight == 0 of
                                                                                                 true ->
                                                                                                     [];
                                                                                                 _ ->
                                                                                                     lists:sublist(SubtreesXCRight, 2, SubtreeNoXCRight)
                                                                                             end]),
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
                                                                                                                                                            end}, SubtreeNoMin, KeyNoMax),
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
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n CLRS: case 3b left  ~n", []),
                          case KeyNo == 1 of
                              true ->
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New KeyValues: ~p~n", [KeyValuesXCLeft ++ KeyValues ++ KeyValuesXC]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Subtrees Part 1: ~p~n", [SubtreesXCLeft]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Subtrees Part 3: ~p~n", [SubtreesXC]),
                                  delete_1(Key, {KeyNoXCLeft + KeyNoXC + 1, SubtreeNoXCLeft + SubtreeNoXC, KeyValuesXCLeft ++ KeyValues ++ KeyValuesXC, SubtreesXCLeft ++ SubtreesXC}, SubtreeNoMin, KeyNoMax);
                              _ ->
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Outer KeyValues: ~p~n", [lists:sublist(KeyValues, 1, KeyPos - 2) ++ lists:sublist(KeyValues, KeyPos, KeyNo)]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Outer Subtrees Part 1: ~p~n", [lists:sublist(Subtrees, 1, KeyPos - 2)]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Outer Subtrees Part 3: ~p~n", [lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Inner KeyValues: ~p~n", [KeyValuesXCLeft ++ [lists:nth(KeyPos - 1, KeyValues)] ++ KeyValuesXC]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Inner Subtrees Part 1: ~p~n", [SubtreesXCLeft]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Inner Subtrees Part 3: ~p~n", [SubtreesXC]),
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
                                                                                        end, KeyValuesXCLeft ++ [lists:nth(KeyPos - 1, KeyValues)] ++ KeyValuesXC, SubtreesXCLeft ++ SubtreesXC}, SubtreeNoMin, KeyNoMax)
                                          ] ++ lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                                  }
                          end;
                      true ->
                          % CLRS: case 3b right
                          ?debugFmt("wwe debugging delete_1_3 ===> ~n CLRS: case 3b right ~n", []),
                          case KeyNo == 1 of
                              true ->
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New KeyValues: ~p~n", [KeyValuesXC ++ KeyValues ++ KeyValuesXCRight]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Subtrees Part 1: ~p~n", [SubtreesXC]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Subtrees Part 3: ~p~n", [SubtreesXCRight]),
                                  delete_1(Key, {KeyNoXC + KeyNoXCRight + 1, SubtreeNoXC + SubtreeNoXCRight, KeyValuesXC ++ KeyValues ++ KeyValuesXCRight, SubtreesXC ++ SubtreesXCRight}, SubtreeNoMin, KeyNoMax);
                              _ ->
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Outer KeyValues: ~p~n", [lists:sublist(KeyValues, 1, KeyPos - 1) ++ lists:sublist(KeyValues, KeyPos + 1, KeyNo)]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Outer Subtrees Part 1: ~p~n", [lists:sublist(Subtrees, 1, KeyPos - 1)]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Outer Subtrees Part 3: ~p~n", [lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Inner KeyValues: ~p~n", [KeyValuesXC ++ [lists:nth(KeyPos, KeyValues)] ++ KeyValuesXCRight]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Inner Subtrees Part 1: ~p~n", [SubtreesXC]),
                                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Inner Subtrees Part 3: ~p~n", [SubtreesXCRight]),
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
                                                                                         end, KeyValuesXC ++ [lists:nth(KeyPos, KeyValues)] ++ KeyValuesXCRight, SubtreesXC ++ SubtreesXCRight}, SubtreeNoMin, KeyNoMax)
                                          ] ++ lists:sublist(Subtrees, KeyPos + 2, SubtreeNo)
                                  }
                          end
                  end;
              _ ->
                  % CLRS: case 3 else
                  ?debugFmt("wwe debugging delete_1_3 ===> ~n CLRS: case 3 else ~n", []),
                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New KeyValues: ~p~n", [KeyValues]),
                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Subtrees Part 1: ~p~n", [lists:sublist(Subtrees, 1, KeyPos - 1)]),
                  ?debugFmt("wwe debugging delete_1_3 ===> ~n New Subtrees Part 3: ~p~n", [lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)]),
                  {
                      KeyNo,
                      SubtreeNo,
                      KeyValues,
                          lists:sublist(Subtrees, 1, KeyPos - 1) ++ [delete_1(Key, TreeXC, SubtreeNoMin, KeyNoMax)] ++ lists:sublist(Subtrees, KeyPos + 1, SubtreeNo)
                  }
          end,
    ?debugFmt("wwe debugging delete_1_3 ===> Start ~n Out: ~p~n", [Out]),
    Out.

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

empty(Order) when Order > 3, Order rem 2 == 0 ->
    {Order div 2, Order - 1, 0, nil}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec enter(key(), value(), b_tree()) -> b_tree().

enter(Key, Value, BTree) ->
    try
        update(Key, Value, BTree)
    catch
        error:{key_not_found, _} ->
            insert(Key, Value, BTree)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec from_dict(pos_integer(), key_values()) -> b_tree().

from_dict(Order, KeyValues) when Order > 3, Order rem 2 == 0, length(KeyValues) > 0 ->
    from_dict_1(KeyValues, empty(Order)).

-spec from_dict_1(key_values(), b_tree()) -> b_tree().

from_dict_1([], BTree) ->
    BTree;
from_dict_1([{Key, Value} | Tail], BTree) ->
    from_dict_1(Tail, insert(Key, Value, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a specialized version of `lookup'.

-spec get(key(), b_tree()) -> value().

get(Key, {_, _, 0, nil}) ->
    erlang:error({key_not_found, Key});
get(Key, {_, _, _, Tree}) ->
    case lookup_1(Key, Tree) of
        {value, Value} ->
            Value;
        _ ->
            erlang:error({key_not_found, Key})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec height(b_tree()) -> non_neg_integer().

height({_, _, 0, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
height({_, _, _, Tree}) ->
    height_1(Tree, 0).

-spec height_1(tree(), non_neg_integer()) -> non_neg_integer().

% Leaf node.
height_1({_, _, _, []}, Number) ->
    Number;
% The most left subtree.
height_1({_, _, _, [Tree | _]}, Number) ->
    height_1(Tree, Number + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert(key(), value(), b_tree()) -> b_tree().

% First entry.
insert(Key, Value, {SubtreeNoMin, KeyNoMax, 0, nil}) ->
    {SubtreeNoMin, KeyNoMax, 1, {1, 0, [{Key, Value}], []}};
% Split root node.
insert(Key, Value, {SubtreeNoMin, KeyNoMax, NumberKeyValues, {KeyNo, _, _, _} = Tree}) when KeyNo == KeyNoMax ->
    {SubtreeNoMin, KeyNoMax, NumberKeyValues + 1, insert_1({Key, Value}, split_node_root(Tree, SubtreeNoMin), SubtreeNoMin, KeyNoMax)};
insert(Key, Value, {SubtreeNoMin, KeyNoMax, NumberKeyValues, Tree}) ->
    {SubtreeNoMin, KeyNoMax, NumberKeyValues + 1, insert_1({Key, Value}, Tree, SubtreeNoMin, KeyNoMax)}.

-spec insert_1(key_value(), tree(), pos_integer(), pos_integer()) -> tree().

% Leaf node.
insert_1(KeyValue, {KeyNo, 0, KeyValues, []}, _, _) ->
    {KeyNo + 1, 0, insert_key_value(KeyValue, KeyValues, []), []};
insert_1({Key, _} = KeyValue, {KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin, KeyNoMax) ->
    {ValueFound, SubtreePos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case ValueFound of
        none ->
            % Look ahead.
            {SubtreeKeyNo, _, SubtreeKeyValues, _} = Subtree = lists:nth(SubtreePos, Subtrees),
            case SubtreeKeyNo == KeyNoMax of
                % Split node.
                true ->
                    {SplitKeyValues, SplitSubtrees1, SplitTree1, SplitTree2, SplitSubtrees2} = split_node_non_root(KeyNo, KeyValues, Subtrees, Subtree, SubtreePos, SubtreeNoMin),
                    {
                        KeyNo + 1,
                        SubtreeNo + 1,
                        SplitKeyValues,
                            SplitSubtrees1 ++
                            case KeyValue < lists:nth(SubtreeNoMin, SubtreeKeyValues) of
                                true ->
                                    [
                                        insert_1(KeyValue, SplitTree1, SubtreeNoMin, KeyNoMax),
                                        SplitTree2
                                    ];
                                _ ->
                                    [
                                        SplitTree1,
                                        insert_1(KeyValue, SplitTree2, SubtreeNoMin, KeyNoMax)
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
                            [insert_1(KeyValue, Subtree, SubtreeNoMin, KeyNoMax)] ++
                            lists:sublist(Subtrees, SubtreePos + 1, KeyNo + 1)
                    }
            end;
        _ ->
            erlang:error({key_exists, Key})
    end.

-spec insert_key_value(key_value(), key_values(), key_values()) -> key_values().

insert_key_value(KeyValue, [], KeyValuesAcc) ->
    KeyValuesAcc ++ [KeyValue];
insert_key_value({Key, _} = KeyValue, [{KeyCurr, _} = KeyValueCurr | Tail], KeyValuesAcc) when Key > KeyCurr ->
    insert_key_value(KeyValue, Tail, KeyValuesAcc ++ [KeyValueCurr]);
insert_key_value({Key, _} = KeyValue, [{KeyCurr, _} | _] = KeyValues, KeyValuesAcc) when Key < KeyCurr ->
    KeyValuesAcc ++ [KeyValue] ++ KeyValues;
insert_key_value({Key, _}, _, _) ->
    erlang:error({key_exists, Key}).

-spec split_node_non_root(pos_integer(), key_values(), subtrees(), tree(), pos_integer(), pos_integer()) -> {key_values(), subtrees(), tree(), tree(), subtrees()}.

split_node_non_root(KeyNo, KeyValues, Subtrees, {TreeKeyNo, TreeSubtreeNo, TreeKeyValues, TreeSubtrees}, SubtreePos, SubtreeNoMin) ->
    {
        % SplitKeyValues .......................................................
        insert_key_value(lists:nth(SubtreeNoMin, TreeKeyValues), KeyValues, []),
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
    }.

-spec split_node_root(tree(), pos_integer()) -> tree().

% Leaf node.
split_node_root({KeyNo, 0, KeyValues, []}, SubtreeNoMin) ->
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
split_node_root({KeyNo, SubtreeNo, KeyValues, Subtrees}, SubtreeNoMin) ->
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
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is a specialized version of `lookup'.

-spec is_defined(key(), b_tree()) -> boolean().

is_defined(_, {_, _, 0, nil}) ->
    false;
is_defined(Key, {_, _, _, Tree}) ->
    case lookup_1(Key, Tree) == none of
        true ->
            false;
        _ ->
            true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_empty(b_tree()) -> boolean().

is_empty({_, _, 0, nil}) ->
    true;
is_empty(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec iterator(b_tree()) -> iterator().

iterator({_, _, 0, nil}) ->
    [];
iterator({_, _, _, {_, _, KeyValues, Subtrees}}) ->
    iterator_1({KeyValues, Subtrees}, []).

% The iterator structure is really just a list corresponding to
% the call stack of an in-order traversal. This is quite fast.

-spec iterator_1({key_values(), subtrees()}, iterator()) -> iterator().

% The most left key / value.
iterator_1({KeyValues, []}, Iterator) ->
    [{KeyValues, []} | Iterator];
% The most left subtree.
iterator_1({KeyValues, Subtrees}, Iterator) ->
    {_, _, KeyValues_1, Subtrees_1} = lists:nth(1, Subtrees),
    iterator_1({KeyValues_1, Subtrees_1}, [{KeyValues, Subtrees} | Iterator]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec keys(b_tree()) -> keys().

keys({_, _, 0, nil}) ->
    [];
keys({_, _, _, {_, _, KeyValues, Subtrees}}) ->
    keys_1(KeyValues, Subtrees, []).

-spec keys_1(key_values(), subtrees(), keys()) -> keys().

keys_1([], [], Keys) ->
    Keys;
% Leaf node.
keys_1([{Key, _} | Tail], [], Keys) ->
    keys_1(Tail, [], Keys ++ [Key]);
% The most right subtree.
keys_1([], [{_, _, KeyValues, Subtrees}], Keys) ->
    Keys ++ keys_1(KeyValues, Subtrees, []);
% Left subtree and current key.
keys_1([{Key, _} | TailKeyValues], [{_, _, KeyValues, Subtrees} | TailTrees], Keys) ->
    keys_1(TailKeyValues, TailTrees, Keys ++ keys_1(KeyValues, Subtrees, []) ++ [Key]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec largest(b_tree()) -> key_value().

largest({_, _, 0, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
largest({_, _, _, Tree}) ->
    largest_1(Tree).

-spec largest_1(tree()) -> key_value().

% The most right key / value.
largest_1({KeyNo, 0, KeyValues, []}) ->
    lists:nth(KeyNo, KeyValues);
% The most right subtree.
largest_1({_, SubtreeNo, _, Subtrees}) ->
    largest_1(lists:nth(SubtreeNo, Subtrees)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec lookup(key(), b_tree()) -> 'none' | {'value', value()}.

lookup(_, {_, _, 0, nil}) ->
    none;
lookup(Key, {_, _, _, Tree}) ->
    lookup_1(Key, Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec map(map_function(), b_tree()) -> b_tree().

map(_, {_, _, 0, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
map(Function, {SubtreeNoMin, KeyNoMax, NumberKeyValues, Tree}) when is_function(Function, 2) ->
    {SubtreeNoMin, KeyNoMax, NumberKeyValues, map_tree(Function, Tree)}.

-spec map_key_values(map_function(), key_values(), key_values()) -> key_values().

map_key_values(_, [], KeyValuesMapped) ->
    KeyValuesMapped;
map_key_values(Function, [{Key, Value} | Tail], KeyValuesMapped) ->
    map_key_values(Function, Tail, KeyValuesMapped ++ [{Key, Function(Key, Value)}]).

-spec map_tree(map_function(), tree()) -> tree().

% Leaf node.
map_tree(Function, {KeyNo, 0, KeyValues, []}) ->
    {KeyNo, 0, map_key_values(Function, KeyValues, []), []};
map_tree(Function, {KeyNo, SubtreeNo, KeyValues, Subtrees}) ->
    {KeyNo, SubtreeNo, map_key_values(Function, KeyValues, []), map_subtrees(Function, Subtrees, [])}.

-spec map_subtrees(map_function(), subtrees(), subtrees()) -> subtrees().

map_subtrees(_, [], TreesMapped) ->
    TreesMapped;
map_subtrees(Function, [Tree | Tail], TreesMapped) ->
    map_subtrees(Function, Tail, TreesMapped ++ [map_tree(Function, Tree)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec next(iterator()) -> 'none' | {key(), value(), iterator()}.

% One level up.
next([{[], _}, {[], _} = Iterator | TailIterator]) ->
    next([Iterator] ++ TailIterator);
% End of leaf node.
next([{[], _}, {[{Key, Value} | TailKeyValues], [_ | TailSubtrees]} | TailIterator]) ->
    {Key, Value, iterator_1({TailKeyValues, TailSubtrees}, TailIterator)};
% Processing a leaf node..
next([{[{Key, Value} | KeyValues], []} | Iterator]) ->
    {Key, Value, [{KeyValues, []} | Iterator]};
% End of iterator.
next([{[], _}]) ->
    none;
% Empty iterator.
next([]) ->
    none.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec number_key_values(b_tree()) -> non_neg_integer().

number_key_values({_, _, NumberKeyValues, _}) ->
    NumberKeyValues.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec size(b_tree()) -> non_neg_integer().

size({_, _, 0, nil}) ->
    0;
size({_, _, _, Tree}) ->
    size_tree(Tree, 0).

-spec size_tree(tree(), non_neg_integer()) -> non_neg_integer().

% Leaf node.
size_tree({_, 0, _, []}, Number) ->
    Number + 1;
size_tree({_, _, _, Subtrees}, Number) ->
    size_subtrees(Subtrees, Number + 1).

-spec size_subtrees(subtrees(), non_neg_integer()) -> non_neg_integer().

size_subtrees([], Number) ->
    Number;
size_subtrees([Tree | Tail], Number) ->
    size_subtrees(Tail, size_tree(Tree, Number)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec smallest(b_tree()) -> key_value().

smallest({_, _, 0, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
smallest({_, _, _, Tree}) ->
    smallest_1(Tree).

-spec smallest_1(tree()) -> key_value().

% The most left key / value.
smallest_1({_, 0, KeyValues, []}) ->
    lists:nth(1, KeyValues);
% The most left subtree.
smallest_1({_, _, _, Subtrees}) ->
    smallest_1(lists:nth(1, Subtrees)).

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

to_list({_, _, 0, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
to_list({_, _, _, {_, _, KeyValues, Subtrees}}) ->
    to_list_1(KeyValues, Subtrees, []).

-spec to_list_1(key_values(), subtrees(), key_values()) -> key_values().

to_list_1([], [], KeyValueList) ->
    KeyValueList;
% Leaf node.
to_list_1([KeyValue | Tail], [], KeyValueList) ->
    to_list_1(Tail, [], KeyValueList ++ [KeyValue]);
% The most right subtree.
to_list_1([], [{_, _, KeyValues, Subtrees}], KeyValueList) ->
    KeyValueList ++ to_list_1(KeyValues, Subtrees, []);
% Left subtree and key / value from current key.
to_list_1([KeyValue | TailKeyValues], [{_, _, KeyValues, Subtrees} | TailTrees], KeyValueList) ->
    to_list_1(TailKeyValues, TailTrees, KeyValueList ++ to_list_1(KeyValues, Subtrees, []) ++ [KeyValue]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec update(key(), value(), b_tree()) -> b_tree().

update(Key, _, {_, _, 0, nil}) ->
    erlang:error({key_not_found, Key});
update(Key, Value, {SubtreeNoMin, KeyNoMax, NumberKeyValues, Tree}) ->
    {SubtreeNoMin, KeyNoMax, NumberKeyValues, update_1({Key, Value}, Tree)}.

-spec update_1(key_value(), tree()) -> tree().

% Leaf node.
update_1({Key, _} = KeyValue, {KeyNo, 0, KeyValues, []}) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
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
update_1({Key, _} = KeyValue, {KeyNo, SubtreeNo, KeyValues, Subtrees}) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case ValueFound of
        none ->
            {
                KeyNo,
                SubtreeNo,
                KeyValues,
                    lists:sublist(Subtrees, 1, KeyPos - 1) ++
                    [update_1(KeyValue, lists:nth(KeyPos, Subtrees))] ++
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
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec values(b_tree()) -> values().

values({_, _, 0, nil}) ->
    [];
values({_, _, _, {_, _, KeyValues, Subtrees}}) ->
    values_1(KeyValues, Subtrees, []).

-spec values_1(key_values(), subtrees(), values()) -> values().

values_1([], [], Values) ->
    Values;
% Leaf node.
values_1([{_, Value} | Tail], [], Values) ->
    values_1(Tail, [], Values ++ [Value]);
% The most right subtree.
values_1([], [{_, _, KeyValues, Subtrees}], Values) ->
    Values ++ values_1(KeyValues, Subtrees, []);
% Left subtree and value from current key.
values_1([{_, Value} | TailKeyValues], [{_, _, KeyValues, Subtrees} | TailTrees], Values) ->
    values_1(TailKeyValues, TailTrees, Values ++ values_1(KeyValues, Subtrees, []) ++ [Value]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec binary_search(key(), key_values(), pos_integer(), pos_integer(), pos_integer()) -> {none, pos_integer()} | {any(), pos_integer()}.

binary_search(Key, KeyValues, KeyNo, Lower, Upper) when Lower > Upper ->
    TreeNo = case Lower > KeyNo of
                 true ->
                     Upper;
                 _ ->
                     Lower
             end,
    {KeyLast, _} = lists:nth(TreeNo, KeyValues),
    case Key < KeyLast of
        true ->
            {none, TreeNo};
        _ ->
            {none, TreeNo + 1}
    end;
binary_search(Key, KeyValues, KeyNo, Lower, Upper) ->
    Mid = (Upper + Lower) div 2,
    {MidKey, MidValue} = lists:nth(Mid, KeyValues),
    if
        Key > MidKey ->
            binary_search(Key, KeyValues, KeyNo, Mid + 1, Upper);
        Key < MidKey ->
            binary_search(Key, KeyValues, KeyNo, Lower, Mid - 1);
        true ->
            {MidValue, Mid}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The term order is an arithmetic total order, so we should not
%% test exact equality for the keys. (If we do, then it becomes
%% possible that neither `>', `<', nor `=:=' matches.) Testing '<'
%% and '>' first is statistically better than testing for
%% equality, and also allows us to skip the test completely in the
%% remaining case.

-spec lookup_1(key(), tree()) -> 'none' | {'value', value()}.

% Leaf node.
lookup_1(Key, {KeyNo, 0, KeyValues, []}) ->
    {Value, _} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case Value == none of
        true ->
            Value;
        _ ->
            {value, Value}
    end;
lookup_1(Key, {KeyNo, _, KeyValues, ChildTrees}) ->
    {Value, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case Value == none of
        true ->
            ChildTree = lists:nth(Pos, ChildTrees),
            lookup_1(Key, ChildTree);
        _ ->
            {value, Value}
    end.

%%direct_test() ->
%%
%%%%    ?assertEqual(?B_TREE_04_64_MINUS_32, b_trees:delete("k_32", ?B_TREE_04_64)),
%%
%%%%    ?debugFmt("wwe debugging direct_test ===> ~n Result: ~p~n", [b_trees:delete("k_22", ?B_TREE_WWE)]),
%%
%%%%    ?assertEqual(?B_TREE_04_32_MINUS_07_01_05, b_trees:delete("k_05", ?B_TREE_04_32_MINUS_07_01)),
%%
%%%%    Number = 64,
%%%%
%%%%    _Keys = test_generator:generate_keys_random(Number, 2),
%%%%
%%%%    _Btree = test_generator:generate_b_tree_from_number(4, Number, 2),
%%%%
%%%%    ?assertEqual(b_trees:empty(4), test_generator:delete_b_tree_list(_Keys, _Btree)),
%%
%%    ok.
