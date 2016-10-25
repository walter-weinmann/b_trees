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
%% B-trees - balanced n-ary trees (B-tree & B*-tree).
%%
%% Copyright (C) 2016 Walter Weinmann
%%
%% An efficient implementation of B-trees and B*-trees. The definition of B-trees
%% and its components corresponds to Knuth's definitions in Volume 3 of "The Art
%% of Computer Programming": The order O of a B-tree is the maximum number of
%% subtrees (sons) in a node and the height of a B-tree is its maximum level, i.e.
%% the length of the longest path from the root node to a leaf node. The difference
%% between a B-tree and a B*-tree is the minimum number of subtrees in a non-root
%% node: O/2 subtrees with a B-tree and 2/3*O subtrees with a B*-tree. The minimum
%% order of a B-tree or B*-tree is 4.
%% -----------------------------------------------------------------------------
%% Operations:
%%
%% - empty(O): returns empty B-tree of order O. Order is defined as
%%   the maximum number of children nodes a non-leaf node may hold.
%%   The minimum value is 4.
%%
%% - empty(O, b_star): returns empty B*-tree of order O. Order is
%%   defined as the maximum number of children nodes a non-leaf node
%%   may hold. The minimum value is 4.
%%
%% - enter(K, V, B): inserts key K with value V into B-tree / B*-tree B
%%   if the key is not present in the tree, otherwise updates key K to
%%   value V in B. Returns the new tree.
%%
%% - from_dict(O, L): turns a list L of {Key, Value} pairs into
%%   a B-tree of order O. The list must not contain duplicate keys.
%%
%% - from_dict(O, b_star, L): turns a list L of {Key, Value} pairs into
%%   a B*-tree of order O. The list must not contain duplicate keys.
%%
%% - get(K, B): retrieves the value stored with key K in
%%   B-tree / B*-tree T. Assumes that the key is present in the tree.
%%
%% - height(B): returns the height of the B-tree / B*-tree B as an integer.
%%   Assumes that the B-tree / B*-tree B is nonempty.
%%
%% - insert(K, V, B): inserts key K with value V into B-tree / B*-tree B;
%%   returns the new B-tree / B*-tree. Assumes that the key K is *not*
%%   present in the B-tree / B*-tree B.
%%
%% - is_defined(K, B): returns `true' if key K is present in
%%   B-tree / B*-tree B, and `false' otherwise.
%%
%% - is_empty(B): returns 'true' if B is an empty B-tree / B*-tree,
%%   and 'false' otherwise.
%%
%% - iterator(B): returns an iterator that can be used for traversing
%%   the entries of B-tree / B*-tree B; see `next'. The implementation
%%   of this is very efficient; traversing the whole tree using `next'
%%   is only slightly slower than getting the list of all elements using
%%   `to_list' and traversing that. The main advantage of the iterator
%%   approach is that it does not require the complete list of all
%%   elements to be built in memory at one time.
%%
%% - iterator_from(K, B): returns an iterator that can be used for
%%   traversing the entries of B-tree / B*-tree B with key greater
%%   than or equal to K; see `next'.
%%
%% - keys(B): returns an ordered list of all keys in B-tree / B*-tree B.
%%
%% - largest(B): returns {K, V}, where K is the largest key in
%%   B-tree / B*-tree B, and V is the value associated with K in B.
%%   Assumes that the B-tree / B*-tree B is nonempty.
%%
%% - lookup(K, B): looks up key K in B-tree / B*-tree B; returns {value, V},
%%   or `none' if the key K is not present.
%%
%% - map(F, B): maps the function F(K, V) -> V' to all key-value pairs
%%   of the B-tree / B*-tree B and returns a new B-tree / B*-tree B'
%%   with the same set of keys as B and the new set of values V'.
%%
%% - next(I): returns {K, V, I1} where K is the smallest key referred to
%%   by the iterator I, and I1 is the new iterator to be used for
%%   traversing the remaining entries, or the atom `none' if no entries
%%   remain.
%%
%% - number_key_values(B): returns the number of key / value pairs in the
%%   B-tree / B*-tree B as an integer. Returns 0 (zero) if the
%%   B-tree / B*-tree B is empty.
%%
%% - size(B): returns the number of nodes in the B-tree / B*-tree B
%%   as an integer. Returns 0 (zero) if the B-tree / B*-tree B is empty.
%%
%% - smallest(B): returns {K, V}, where K is the smallest key in
%%   B-tree / B*-tree B, and V is the value associated with K in B.
%%   Assumes that the B-tree / B*-tree B is nonempty.
%%
%% - to_list(B): returns an ordered list of {Key, Value} pairs
%%   for all keys in B-tree / B*-tree B.

%% - update(K, V, B): updates key K to value V in B-tree / B*-tree B;
%%   returns the new tree. Assumes that the key is present in the tree.
%%
%% - values(B): returns the list of values for all keys in
%%   B-tree / B*-tree B, sorted by their corresponding keys.
%%   Duplicates are not removed.
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
    enter/3,
    from_dict/2,
    from_dict/3,
    get/2,
    height/1,
    insert/3,
    is_defined/2,
    is_empty/1,
%%    iterator/1,
%%    iterator_from/2,
    keys/1,
    largest/1,
    lookup/2,
    map/2,
%%    next/1,
    number_key_values/1,
    size/1,
    smallest/1,
    to_list/1,
    update/3,
    values/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data structure:
%% - {BTreeType, MinimumKeys, MaximumKeys, NumberKeyValues, Tree},
%%   where `Tree' is composed of :
%%   - {KeyNo, [{Key, Value}], [Tree]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

-type b_tree() :: {b_tree_type(), pos_integer(), pos_integer(), non_neg_integer(), tree()}.
-type b_tree_type() :: 'b' | 'b_star'.

% -type iterator() :: none | {key(), value(), tree()}.

-type key() :: any().
-type keys() :: [any()].

-type key_value() :: {key(), value()}.
-type key_values() :: [key_value()].

-type map_function() :: fun((key(), value()) -> value()).

-type subtrees() :: [tree()].

-type tree() :: 'nil'
| {pos_integer(), key_values(), []}
| {pos_integer(), key_values(), subtrees()}.

-type value() :: any().
-type values() :: [any()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec empty(pos_integer()) -> b_tree().
-spec empty(pos_integer(), atom()) -> b_tree().

empty(Order) when Order > 3 ->
    {b, Order div 2, Order - 1, 0, nil}.
empty(Order, b_star) when Order > 3 ->
    {b_star, Order * 2 div 3, Order - 1, 0, nil}.

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
-spec from_dict(pos_integer(), atom(), key_values()) -> b_tree().

from_dict(Order, KeyValues) when Order > 3, length(KeyValues) > 0 ->
    from_dict_1(KeyValues, empty(Order)).
from_dict(Order, b_star, KeyValues) when Order > 3, length(KeyValues) > 0 ->
    from_dict_1(KeyValues, empty(Order, b_star)).

-spec from_dict_1(key_values(), b_tree()) -> b_tree().

from_dict_1([], BTree) ->
    BTree;
from_dict_1([{Key, Value} | Tail], BTree) ->
    from_dict_1(Tail, insert(Key, Value, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a specialized version of `lookup'.

-spec get(key(), b_tree()) -> value().

get(Key, {_, _, _, _, nil}) ->
    erlang:error({key_not_found, Key});
get(Key, {_, _, _, _, Tree}) ->
    case lookup_1(Key, Tree) of
        {value, Value} ->
            Value;
        _ ->
            erlang:error({key_not_found, Key})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec height(b_tree()) -> non_neg_integer().

height({_, _, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
height({_, _, _, _, Tree}) ->
    height(Tree, 0).

-spec height(tree(), non_neg_integer()) -> non_neg_integer().

% Leaf node.
height({_, _, []}, Number) ->
    Number;
% The most left subtree.
height({_, _, [Tree | _]}, Number) ->
    height(Tree, Number + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert(key(), value(), b_tree()) -> b_tree().

% First entry.
insert(Key, Value, {BTreeType, KeyNoMin, KeyNoMax, 0, nil} = _BTree) ->
    {BTreeType, KeyNoMin, KeyNoMax, 1, {1, [{Key, Value}], []}};
% Split root node.
insert(Key, Value, {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, {KeyNo, KeyValues, _} = Tree} = _BTree) when KeyNo == KeyNoMax ->
    KeyNoSplit = case KeyNoMax rem 2 of
                     1 ->
                         KeyNoMin;
                     _ ->
                         {KeyNoMinKey, _} = lists:nth(KeyNoMin, KeyValues),
                         case Key < KeyNoMinKey of
                             true ->
                                 KeyNoMin;
                             _ ->
                                 KeyNoMin + 1
                         end
                 end,
    {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues + 1, insert({Key, Value}, split_node_root(Tree, KeyNoSplit), KeyNoMin, KeyNoMax)};
insert(Key, Value, {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, Tree} = _BTree) ->
    {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues + 1, insert({Key, Value}, Tree, KeyNoMin, KeyNoMax)}.

-spec insert(key_value(), tree(), pos_integer(), pos_integer()) -> tree().

% Leaf node.
insert(KeyValue, {KeyNo, KeyValues, []} = _Tree, _, _) ->
    {KeyNo + 1, insert_key_values(KeyValue, KeyValues, []), []};
insert({Key, _} = KeyValue, {KeyNo, KeyValues, Subtrees} = _Tree, KeyNoMin, KeyNoMax) ->
    {ValueFound, SubtreePos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case ValueFound of
        none ->
            % Look ahead.
            {SubtreeKeyNo, _, _} = Subtree = lists:nth(SubtreePos, Subtrees),
            case SubtreeKeyNo == KeyNoMax of
                % Split node.
                true ->
                    {SplitKeyValues, SplitSubtrees1, SplitTree, SplitSubtrees2} = split_node_non_root(KeyValue, KeyNo, KeyValues, Subtrees, Subtree, SubtreePos, KeyNoMin, KeyNoMax),
                    {
                        KeyNo + 1,
                        SplitKeyValues,
                            SplitSubtrees1 ++
                            [insert(KeyValue, SplitTree, KeyNoMin, KeyNoMax)] ++
                            SplitSubtrees2
                    };
                _ ->
                    {
                        KeyNo,
                        KeyValues,
                            lists:sublist(Subtrees, 1, SubtreePos - 1) ++
                            [insert(KeyValue, Subtree, KeyNoMin, KeyNoMax)] ++
                            lists:sublist(Subtrees, SubtreePos + 1, KeyNo + 1)
                    }
            end;
        _ ->
            erlang:error({key_exists, Key})
    end.

-spec insert_key_values(key_value(), key_values(), key_values()) -> key_values().

insert_key_values(KeyValue, [], KeyValuesAcc) ->
    KeyValuesAcc ++ [KeyValue];
insert_key_values({Key, _} = KeyValue, [{KeyCurr, _} = KeyValueCurr | Tail], KeyValuesAcc) when Key > KeyCurr ->
    insert_key_values(KeyValue, Tail, KeyValuesAcc ++ [KeyValueCurr]);
insert_key_values({Key, _} = KeyValue, [{KeyCurr, _} | _] = KeyValues, KeyValuesAcc) when Key < KeyCurr ->
    KeyValuesAcc ++ [KeyValue] ++ KeyValues;
insert_key_values({Key, _}, _, _) ->
    erlang:error({key_exists, Key}).

-spec split_node_non_root(key_value(), pos_integer(), key_values(), subtrees(), tree(), pos_integer(), pos_integer(), pos_integer()) -> {key_values(), subtrees(), tree(), subtrees()}.

split_node_non_root({Key, _} = _KeyValue, KeyNo, KeyValues, Subtrees, {TreeKeyNo, TreeKeyValues, TreeSubtrees}, SubtreePos, KeyNoMin, KeyNoMax) ->
    KeyNoSplit = case KeyNoMax rem 2 of
                     1 ->
                         KeyNoMin;
                     _ ->
                         {KeyNoMinKey, _} = lists:nth(KeyNoMin, TreeKeyValues),
                         case Key < KeyNoMinKey of
                             true ->
                                 KeyNoMin;
                             _ ->
                                 KeyNoMin + 1
                         end
                 end,
    % {SplitKeyValues, SplitSubtrees1, SplitTree, SplitSubtrees2}
    {
        % SplitKeyValues .......................................................
        insert_key_values(lists:nth(KeyNoSplit, TreeKeyValues), KeyValues, []),
        % SplitSubtrees1 .......................................................
            lists:sublist(Subtrees, 1, SubtreePos - 1) ++
            [
                {
                    KeyNoSplit - 1,
                    lists:sublist(TreeKeyValues, 1, KeyNoSplit - 1),
                    % Leaf node to be splitted ?
                    case length(TreeSubtrees) == 0 of
                        true ->
                            [];
                        _ ->
                            lists:sublist(TreeSubtrees, 1, KeyNoSplit)
                    end
                }
            ],
        % SplitTree ............................................................
        {
            TreeKeyNo - KeyNoSplit,
            lists:sublist(TreeKeyValues, KeyNoSplit + 1, TreeKeyNo),
            % Leaf node to be splitted ?
            case length(TreeSubtrees) == 0 of
                true ->
                    [];
                _ ->
                    lists:sublist(TreeSubtrees, KeyNoSplit + 1, TreeKeyNo + 1)
            end
        },
        % SplitSubtrees2 .......................................................
        lists:sublist(Subtrees, SubtreePos + 1, KeyNo + 1)
    }.

-spec split_node_root(tree(), pos_integer()) -> tree().

% Leaf node.
split_node_root({KeyNo, KeyValues, []} = _Tree, KeyNoSplit) ->
    {
        1,
        [lists:nth(KeyNoSplit, KeyValues)],
        [
            {
                KeyNoSplit - 1,
                lists:sublist(KeyValues, 1, KeyNoSplit - 1),
                []
            },
            {
                KeyNo - KeyNoSplit,
                lists:sublist(KeyValues, KeyNoSplit + 1, KeyNo),
                []
            }
        ]
    };
split_node_root({KeyNo, KeyValues, Subtrees} = _Tree, KeyNoSplit) ->
    {
        1,
        [lists:nth(KeyNoSplit, KeyValues)],
        [
            {
                KeyNoSplit - 1,
                lists:sublist(KeyValues, 1, KeyNoSplit - 1),
                lists:sublist(Subtrees, 1, KeyNoSplit)
            },
            {
                KeyNo - KeyNoSplit,
                lists:sublist(KeyValues, KeyNoSplit + 1, KeyNo),
                lists:sublist(Subtrees, KeyNoSplit + 1, KeyNo)
            }
        ]
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a specialized version of `lookup'.

-spec is_defined(key(), b_tree()) -> boolean().

is_defined(_, {_, _, _, _, nil}) ->
    false;
is_defined(Key, {_, _, _, _, Tree}) ->
    case lookup_1(Key, Tree) == none of
        true ->
            false;
        _ ->
            true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_empty(b_tree()) -> boolean().

is_empty({_, _, _, _, nil}) ->
    true;
is_empty(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-spec iterator(b_tree()) -> [key_value(),...].
%%
%%iterator({_, _, _, _, nil}) ->
%%    [].
%%iterator({_, _, _, _, Tree}) ->
%%    iterator_1(Tree,[]).
%%
%%%% The iterator structure is really just a list corresponding to
%%%% the call stack of an in-order traversal. This is quite fast.
%%
%%iterator({_, _, nil, _} = T, As) ->
%%    [T | As];
%%iterator({_, _, L, _} = T, As) ->
%%    iterator(L, [T | As]);
%%iterator(nil, As) ->
%%    As.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec keys(b_tree()) -> keys().

keys({_, _, _, _, nil}) ->
    [];
keys({_, _, _, _, {_, KeyValues, Subtrees}}) ->
    keys(KeyValues, Subtrees, []).

-spec keys(key_values(), subtrees(), keys()) -> keys().

keys([], [], Keys) ->
    Keys;
% Leaf node.
keys([{Key, _} | Tail], [], Keys) ->
    keys(Tail, [], Keys ++ [Key]);
% The most right subtree.
keys([], [{_, KeyValues, Subtrees}], Keys) ->
    Keys ++ keys(KeyValues, Subtrees, []);
% Left subtree and current key.
keys([{Key, _} | TailKeyValues], [{_, KeyValues, Subtrees} | TailTrees], Keys) ->
    keys(TailKeyValues, TailTrees, Keys ++ keys(KeyValues, Subtrees, []) ++ [Key]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec largest(b_tree()) -> key_value().

largest({_, _, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
largest({_, _, _, _, Tree}) ->
    largest_1(Tree).

-spec largest_1(tree()) -> key_value().

% The most right key / value.
largest_1({KeyNo, KeyValues, []}) ->
    lists:nth(KeyNo, KeyValues);
% The most right subtree.
largest_1({KeyNo, _, Subtrees}) ->
    largest_1(lists:nth(KeyNo + 1, Subtrees)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec lookup(key(), b_tree()) -> 'none' | {'value', value()}.

lookup(_Key, {_, _, _, _, nil}) ->
    none;
lookup(Key, {_, _, _, _, Tree}) ->
    lookup_1(Key, Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec map(map_function(), b_tree()) -> b_tree().

map(_, {_, _, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
map(Function, {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, Tree}) when is_function(Function, 2) ->
    {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, map_tree(Function, Tree)}.

-spec map_key_values(map_function(), key_values(), key_values()) -> key_values().

map_key_values(_, [], KeyValuesMapped) ->
    KeyValuesMapped;
map_key_values(Function, [{Key, Value} | Tail], KeyValuesMapped) ->
    map_key_values(Function, Tail, KeyValuesMapped ++ [{Key, Function(Key, Value)}]).

-spec map_tree(map_function(), tree()) -> tree().

% Leaf node.
map_tree(Function, {KeyNo, KeyValues, []}) ->
    {KeyNo, map_key_values(Function, KeyValues, []), []};
map_tree(Function, {KeyNo, KeyValues, Subtrees}) ->
    {KeyNo, map_key_values(Function, KeyValues, []), map_subtrees(Function, Subtrees, [])}.

-spec map_subtrees(map_function(), subtrees(), subtrees()) -> subtrees().

map_subtrees(_, [], TreesMapped) ->
    TreesMapped;
map_subtrees(Function, [Tree | Tail], TreesMapped) ->
    map_subtrees(Function, Tail, TreesMapped ++ [map_tree(Function, Tree)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec number_key_values(b_tree()) -> non_neg_integer().

number_key_values({_, _, _, NumberKeyValues, _}) ->
    NumberKeyValues.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec size(b_tree()) -> non_neg_integer().

size({_, _, _, _, nil}) ->
    0;
size({_, _, _, _, Tree}) ->
    size_tree(Tree, 0).

-spec size_tree(tree(), non_neg_integer()) -> non_neg_integer().

% Leaf node.
size_tree({_, _, []}, Number) ->
    Number + 1;
size_tree({_, _, Subtrees}, Number) ->
    size_subtrees(Subtrees, Number + 1).

-spec size_subtrees(subtrees(), non_neg_integer()) -> non_neg_integer().

size_subtrees([], Number) ->
    Number;
size_subtrees([Tree | Tail], Number) ->
    size_subtrees(Tail, size_tree(Tree, Number)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec smallest(b_tree()) -> key_value().

smallest({_, _, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
smallest({_, _, _, _, Tree}) ->
    smallest_1(Tree).

-spec smallest_1(tree()) -> key_value().

% The most left key / value.
smallest_1({_, KeyValues, []}) ->
    lists:nth(1, KeyValues);
% The most left subtree.
smallest_1({_, _, Subtrees}) ->
    smallest_1(lists:nth(1, Subtrees)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec to_list(b_tree()) -> key_values().

to_list({_, _, _, _, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
to_list({_, _, _, _, {_, KeyValues, Subtrees}}) ->
    to_list(KeyValues, Subtrees, []).

-spec to_list(key_values(), subtrees(), key_values()) -> key_values().

to_list([], [], KeyValueList) ->
    KeyValueList;
% Leaf node.
to_list([KeyValue | Tail], [], KeyValueList) ->
    to_list(Tail, [], KeyValueList ++ [KeyValue]);
% The most right subtree.
to_list([], [{_, KeyValues, Subtrees}], KeyValueList) ->
    KeyValueList ++ to_list(KeyValues, Subtrees, []);
% Left subtree and key / value from current key.
to_list([KeyValue | TailKeyValues], [{_, KeyValues, Subtrees} | TailTrees], KeyValueList) ->
    to_list(TailKeyValues, TailTrees, KeyValueList ++ to_list(KeyValues, Subtrees, []) ++ [KeyValue]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec update(key(), value(), b_tree()) -> b_tree().

update(Key, _, {_, _, _, _, nil}) ->
    erlang:error({key_not_found, Key});
update(Key, Value, {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, Tree}) ->
    {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, update_1({Key, Value}, Tree)}.

-spec update_1(key_value(), tree()) -> tree().

% Leaf node.
update_1({Key, _} = KeyValue, {KeyNo, KeyValues, []}) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case ValueFound of
        none ->
            erlang:error({key_not_found, Key});
        _ ->
            {
                KeyNo,
                    lists:sublist(KeyValues, 1, KeyPos - 1) ++
                    [KeyValue] ++
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                []
            }
    end;
update_1({Key, _} = KeyValue, {KeyNo, KeyValues, Subtrees}) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case ValueFound of
        none ->
            {
                KeyNo,
                KeyValues,
                    lists:sublist(Subtrees, 1, KeyPos - 1) ++
                    [update_1(KeyValue, lists:nth(KeyPos, Subtrees))] ++
                    lists:sublist(Subtrees, KeyPos + 1, KeyNo + 1)
            };
        _ ->
            {
                KeyNo,
                    lists:sublist(KeyValues, 1, KeyPos - 1) ++
                    [KeyValue] ++
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                Subtrees
            }
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec values(b_tree()) -> values().

values({_, _, _, _, nil}) ->
    [];
values({_, _, _, _, {_, KeyValues, Subtrees}}) ->
    values(KeyValues, Subtrees, []).

-spec values(key_values(), subtrees(), values()) -> values().

values([], [], Values) ->
    Values;
% Leaf node.
values([{_, Value} | Tail], [], Values) ->
    values(Tail, [], Values ++ [Value]);
% The most right subtree.
values([], [{_, KeyValues, Subtrees}], Values) ->
    Values ++ values(KeyValues, Subtrees, []);
% Left subtree and value from current key.
values([{_, Value} | TailKeyValues], [{_, KeyValues, Subtrees} | TailTrees], Values) ->
    values(TailKeyValues, TailTrees, Values ++ values(KeyValues, Subtrees, []) ++ [Value]).

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
lookup_1(Key, {KeyNo, KeyValues, []}) ->
    {Value, _} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case Value == none of
        true ->
            Value;
        _ ->
            {value, Value}
    end;
lookup_1(Key, {KeyNo, KeyValues, ChildTrees}) ->
    {Value, Pos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case Value == none of
        true ->
            ChildTree = lists:nth(Pos, ChildTrees),
            lookup_1(Key, ChildTree);
        _ ->
            {value, Value}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

direct_test() ->

%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n B-tree: ~p~n", [test_generator:generate_b_tree_from_number(16, 16, 2)]),

%%    BTREE_I = b_trees:insert("k_06", "v_06", ?B_TREE_03_05),
%%    BTREE_P = ?B_TREE_03_06,
%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n P: ~p~n I: ~p~n", [BTREE_P, BTREE_I]),
%%    ?assertEqual(BTREE_P, BTREE_I),

%%%%%%    GBTree = test_generator:generate_gb_tree_from_number(10, 2),
%%%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n GBTree: ~p~n", [GBTree]),
%%%%%%    Iterator_01 = gb_trees:iterator(GBTree),
%%%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n Iterator_01: ~p~n", [Iterator_01]),
%%%%%%    {Key_01, Value_01, Iterator_02} = gb_trees:next(Iterator_01),
%%%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n Key: ~p~n Value: ~p~n Iterator_02: ~p~n", [Key_01, Value_01, Iterator_02]),
%%%%%%    {Key_02, Value_02, Iterator_03} = gb_trees:next(Iterator_02),
%%%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n Key: ~p~n Value: ~p~n Iterator_03: ~p~n", [Key_02, Value_02, Iterator_03]),
%%%%%%    {Key_03, Value_03, Iterator_04} = gb_trees:next(Iterator_03),
%%%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n Key: ~p~n Value: ~p~n Iterator_04: ~p~n", [Key_03, Value_03, Iterator_04]),
%%%%%%    {_Key_04, _Value_04, _Iterator_05} = gb_trees:next(Iterator_04),
%%%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n Key: ~p~n Value: ~p~n Iterator_05: ~p~n", [_Key_04, _Value_04, _Iterator_05]),
%%%%
%%%%%%    ?debugFmt("wwe debugging direct_test/0 ===> Start ~n Key: ~p~n KeyValues: ~p~n Lower: ~p~n Upper: ~p~n", [Key, KeyValues, Lower, Upper]),
%%%%%%
%%%%%%    ?debugFmt("wwe debugging insert_insert_simple_split_test/0 =====================================> B-tree_04_00~n ~p~n", [b_trees:empty(4)]),
%%%%%%
%%%%%%    ?debugFmt("wwe debugging insert_insert_simple_split_test/0 =====================================> B-tree_05_01~n ~p~n", [test_generator:generate_b_tree_from_number(5, 01, 2)]),

    ok.
