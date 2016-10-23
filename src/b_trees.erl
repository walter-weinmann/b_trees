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
%% An efficient implementation of Prof. Arne Andersson's General ???
%% Balanced Trees. These have no storage overhead compared to plain
%% unbalanced binary trees, and their performance is in general better
%% than AVL trees.
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
%%   Returns 0 (zero) if the B-tree / B*-tree B is empty.
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
%% - {BTreeType, Minimum, Maximum, NumberKeyValues, Tree},
%%   where `Tree' is composed of :
%%   - {KeyNo, IsLeaf, [{Key, Value}], [Tree]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some macros.

-define(B_TREE_EMPTY, {_, _, _, _, nil}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

% -type iterator() :: none | {key(), value(), tree()}.

-type key() :: any().
-type keys() :: [any(), ...].
-type value() :: any().
-type values() :: [any(), ...].

-type key_value() :: {key(), value()}.
-type key_values() :: [key_value(), ...].

-type map_function() :: fun((key(), value()) -> value()).

-type tree() :: 'nil' | {pos_integer(), boolean(), key_values(), trees()}.
-type trees() :: [tree(), ...].

-type b_tree() :: {atom(), pos_integer(), pos_integer(), non_neg_integer(), tree()}.

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

get(Key, ?B_TREE_EMPTY) ->
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

height(?B_TREE_EMPTY) ->
    0;
height({_, _, _, _, Tree}) ->
    height(Tree, 1).

-spec height(tree(), pos_integer()) -> pos_integer().

height({_, true, _, _}, Number) ->
    Number;
height({_, _, _, [Tree | _]}, Number) ->
    height(Tree, Number + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert(key(), value(), b_tree()) -> b_tree().

insert(Key, Value, {BTreeType, KeyNoMin, KeyNoMax, 0, nil}) ->
    {BTreeType, KeyNoMin, KeyNoMax, 1, {1, true, [{Key, Value}], []}};
insert(Key, Value, {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, {KeyNo, _IsLeaf, KeyValues, _} = BTree}) ->
    {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues + 1, insert_into_tree({Key, Value}, case KeyNo == KeyNoMax of
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
                                                                                        end, KeyNoMin, KeyNoMax)}.

-spec insert_into_key_values(key_value(), key_values(), key_values()) -> key_values().

insert_into_key_values(KeyValue, [], KeyValuesAcc) ->
    KeyValuesAcc ++ [KeyValue];
insert_into_key_values({Key, _} = KeyValue, [{KeyCurr, _} = KeyValueCurr | Tail], KeyValuesAcc) when Key > KeyCurr ->
    insert_into_key_values(KeyValue, Tail, KeyValuesAcc ++ [KeyValueCurr]);
insert_into_key_values({Key, _} = KeyValue, [{KeyCurr, _} | _] = KeyValues, KeyValuesAcc) when Key < KeyCurr ->
    KeyValuesAcc ++ [KeyValue] ++ KeyValues;
insert_into_key_values({Key, _Value}, _KeyValues, _KeyValuesAcc) ->
    erlang:error({key_exists, Key}).

-spec insert_into_tree(key_value(), tree(), pos_integer(), pos_integer()) -> tree().

insert_into_tree(KeyValue, {KeyNo, IsLeaf, KeyValues, []}, _KeyNoMin, _KeyNoMax) ->
    {KeyNo + 1, IsLeaf, insert_into_key_values(KeyValue, KeyValues, []), []};
insert_into_tree({Key, _} = KeyValue, {KeyNo, false = IsLeaf, KeyValues, Trees}, KeyNoMin, KeyNoMax) ->
    {ValueFound, TreeUpperPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
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
                                                                                  KeyNoEff = length(KeyValuesSplit),
                                                                                  {none, TreeUpperPosSplit} = binary_search(Key, KeyValuesSplit, KeyNoEff, 1, KeyNoEff),
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

-spec split_tree_non_root(tree(), pos_integer(), key_values(), trees(), pos_integer()) -> {key_values(), trees()}.

split_tree_non_root({KeyNo, IsLeaf, KeyValues, []}, KeyNoSplit, KeyValuesLower, TreesLower, TreeNo) ->
    {insert_into_key_values(lists:nth(KeyNoSplit, KeyValues), KeyValuesLower, []),
            lists:sublist(TreesLower, 1, TreeNo - 1) ++
            [
                {
                    KeyNoSplit - 1, IsLeaf,
                    lists:sublist(KeyValues, 1, KeyNoSplit - 1),
                    []
                },
                {
                    KeyNo - KeyNoSplit, IsLeaf,
                    lists:sublist(KeyValues, KeyNoSplit + 1, KeyNo),
                    []
                }
            ] ++ lists:sublist(TreesLower, TreeNo + 1, length(TreesLower))};
split_tree_non_root({KeyNo, IsLeaf, KeyValues, Trees}, KeyNoSplit, KeyValuesLower, TreesLower, TreeNo) ->
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
                    lists:sublist(Trees, KeyNoSplit + 1, KeyNo)
                }
            ] ++ lists:sublist(TreesLower, TreeNo + 1, KeyNo + 1)}.

-spec split_tree_root(tree(), pos_integer()) -> tree().

split_tree_root({KeyNo, IsLeaf, KeyValues, []}, KeyNoSplit) ->
    {
        1, false,
        [lists:nth(KeyNoSplit, KeyValues)],
        [
            {
                KeyNoSplit - 1,
                IsLeaf,
                lists:sublist(KeyValues, 1, KeyNoSplit - 1),
                []
            },
            {
                KeyNo - KeyNoSplit,
                IsLeaf,
                lists:sublist(KeyValues, KeyNoSplit + 1, KeyNo),
                []
            }
        ]
    };
split_tree_root({KeyNo, IsLeaf, KeyValues, Trees}, KeyNoSplit) ->
    {
        1, false,
        [lists:nth(KeyNoSplit, KeyValues)],
        [
            {
                KeyNoSplit - 1,
                IsLeaf,
                lists:sublist(KeyValues, 1, KeyNoSplit - 1),
                lists:sublist(Trees, 1, KeyNoSplit)
            },
            {
                KeyNo - KeyNoSplit,
                IsLeaf,
                lists:sublist(KeyValues, KeyNoSplit + 1, KeyNo),
                lists:sublist(Trees, KeyNoSplit + 1, KeyNo)
            }
        ]
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a specialized version of `lookup'.

-spec is_defined(key(), b_tree()) -> boolean().

is_defined(_, ?B_TREE_EMPTY) ->
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

is_empty(?B_TREE_EMPTY) ->
    true;
is_empty(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-spec iterator(b_tree()) -> [key_value(),...].
%%
%%iterator({_, _, _, 0, nil}) ->
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

keys(?B_TREE_EMPTY) ->
    [];
keys({_, _, _, _, {_, _, KeyValues, Trees}}) ->
    keys(KeyValues, Trees, []).

-spec keys(key_values(), trees(), keys()) -> keys().

keys([], [], Keys) ->
    Keys;
keys([], [{_, _, KeyValues, Trees}], Keys) ->
    Keys ++ keys(KeyValues, Trees, []);
keys([{Key, _} | Tail], [], Keys) ->
    keys(Tail, [], Keys ++ [Key]);
keys([{Key, _} | TailKeyValues], [{_, _, KeyValues, Trees} | TailTrees], Keys) ->
    keys(TailKeyValues, TailTrees, Keys ++ keys(KeyValues, Trees, []) ++ [Key]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec largest(b_tree()) -> key_value().

largest(?B_TREE_EMPTY = BTree) ->
    erlang:error({empty_tree, BTree});
largest({_, _, _, _, Tree}) ->
    largest_1(Tree).

-spec largest_1(tree()) -> key_value().

largest_1({_, true, KeyValues, _}) ->
    lists:nth(length(KeyValues), KeyValues);
largest_1({KeyNo, _IsLeaf, _, Trees}) ->
    largest_1(lists:nth(KeyNo + 1, Trees)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec lookup(key(), b_tree()) -> 'none' | {'value', value()}.

lookup(_Key, ?B_TREE_EMPTY) ->
    none;
lookup(Key, {_, _, _, _, Tree}) ->
    lookup_1(Key, Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec map(map_function(), b_tree()) -> b_tree().

map(_, ?B_TREE_EMPTY = BTree) ->
    erlang:error({empty_tree, BTree});
map(Function, {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, Tree}) when is_function(Function, 2) ->
    {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, map_tree(Function, Tree)}.

-spec map_key_values(map_function(), [key_values(), ...], [key_values(), ...]) -> [key_values(), ...].

map_key_values(_, [], KeyValuesMapped) ->
    KeyValuesMapped;
map_key_values(Function, [{Key, Value} | Tail], KeyValuesMapped) ->
    map_key_values(Function, Tail, KeyValuesMapped ++ [{Key, Function(Key, Value)}]).

-spec map_tree(map_function(), tree()) -> tree().

map_tree(Function, {KeyNo, true = IsLeaf, KeyValues, Trees}) ->
    {KeyNo, IsLeaf, map_key_values(Function, KeyValues, []), Trees};
map_tree(Function, {KeyNo, IsLeaf, KeyValues, Trees}) ->
    {KeyNo, IsLeaf, map_key_values(Function, KeyValues, []), map_trees(Function, Trees, [])}.

-spec map_trees(map_function(), [tree(), ...], [tree(), ...]) -> [tree(), ...].

map_trees(_, [], TreesMapped) ->
    TreesMapped;
map_trees(Function, [Tree | Tail], TreesMapped) ->
    map_trees(Function, Tail, TreesMapped ++ [map_tree(Function, Tree)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec number_key_values(b_tree()) -> non_neg_integer().

number_key_values({_, _, _, NumberKeyValues, _}) ->
    NumberKeyValues.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec size(b_tree()) -> non_neg_integer().

size(?B_TREE_EMPTY) ->
    0;
size({_, _, _, _, Tree}) ->
    size_tree(Tree, 0).

-spec size_tree(tree(), pos_integer()) -> pos_integer().

size_tree({_, true, _, _}, Number) ->
    Number + 1;
size_tree({_, _, _, Trees}, Number) ->
    size_trees(Trees, Number + 1).

-spec size_trees([tree(), ...], pos_integer()) -> pos_integer().

size_trees([], Number) ->
    Number;
size_trees([Tree | Tail], Number) ->
    size_trees(Tail, size_tree(Tree, Number)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec smallest(b_tree()) -> key_value().

smallest(?B_TREE_EMPTY = BTree) ->
    erlang:error({empty_tree, BTree});
smallest({_, _, _, _, Tree}) ->
    smallest_1(Tree).

-spec smallest_1(tree()) -> key_value().

smallest_1({_, true, KeyValues, _}) ->
    lists:nth(1, KeyValues);
smallest_1({_, _, _, Trees}) ->
    smallest_1(lists:nth(1, Trees)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec to_list(b_tree()) -> key_values().

to_list(?B_TREE_EMPTY = BTree) ->
    erlang:error({empty_tree, BTree});
to_list({_, _, _, _, {_, _, KeyValues, Trees}}) ->
    to_list(KeyValues, Trees, []).

-spec to_list(key_values(), trees(), key_values()) -> key_values().

to_list([], [], KeyValueList) ->
    KeyValueList;
to_list([], [{_, _, KeyValues, Trees}], KeyValueList) ->
    KeyValueList ++ to_list(KeyValues, Trees, []);
to_list([KeyValue | Tail], [], KeyValueList) ->
    to_list(Tail, [], KeyValueList ++ [KeyValue]);
to_list([KeyValue | TailKeyValues], [{_, _, KeyValues, Trees} | TailTrees], KeyValueList) ->
    to_list(TailKeyValues, TailTrees, KeyValueList ++ to_list(KeyValues, Trees, []) ++ [KeyValue]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec update(key(), value(), b_tree()) -> b_tree().

update(Key, _, ?B_TREE_EMPTY) ->
    erlang:error({key_not_found, Key});
update(Key, Value, {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, Tree}) ->
    {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, update_1({Key, Value}, Tree)}.

-spec update_1(key_value(), tree()) -> {tree(), boolean()}.

update_1({Key, _} = KeyValue, {KeyNo, IsLeaf, KeyValues, []}) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case ValueFound of
        none ->
            erlang:error({key_not_found, Key});
        _ ->
            {
                KeyNo,
                IsLeaf,
                    lists:sublist(KeyValues, 1, KeyPos - 1) ++
                    [KeyValue] ++
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                []
            }
    end;
update_1({Key, _} = KeyValue, {KeyNo, IsLeaf, KeyValues, Trees}) ->
    {ValueFound, KeyPos} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case ValueFound of
        none ->
            {
                KeyNo,
                IsLeaf,
                KeyValues,
                    lists:sublist(Trees, 1, KeyPos - 1) ++
                    [update_1(KeyValue, lists:nth(KeyPos, Trees))] ++
                    lists:sublist(Trees, KeyPos + 1, KeyNo + 1)
            };
        _ ->
            {
                KeyNo,
                IsLeaf,
                    lists:sublist(KeyValues, 1, KeyPos - 1) ++
                    [KeyValue] ++
                    lists:sublist(KeyValues, KeyPos + 1, KeyNo),
                Trees
            }
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec values(b_tree()) -> values().

values(?B_TREE_EMPTY) ->
    [];
values({_, _, _, _, {_, _, KeyValues, Trees}}) ->
    values(KeyValues, Trees, []).

-spec values(key_values(), trees(), values()) -> values().

values([], [], Values) ->
    Values;
values([], [{_, _, KeyValues, Trees}], Values) ->
    Values ++ values(KeyValues, Trees, []);
values([{_, Value} | Tail], [], Values) ->
    values(Tail, [], Values ++ [Value]);
values([{_, Value} | TailKeyValues], [{_, _, KeyValues, Trees} | TailTrees], Values) ->
    values(TailKeyValues, TailTrees, Values ++ values(KeyValues, Trees, []) ++ [Value]).

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

lookup_1(Key, {KeyNo, _IsLeaf, KeyValues, []}) ->
    {Value, _} = binary_search(Key, KeyValues, KeyNo, 1, KeyNo),
    case Value == none of
        true ->
            none;
        _ ->
            {value, Value}
    end;
lookup_1(Key, {KeyNo, _IsLeaf, KeyValues, ChildTrees}) ->
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

%%direct_test() ->
%%
%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n b_trees:keys(?B_TREE_05_16): ~p~n", [b_trees:keys(?B_TREE_05_16)]),
%%
%%%%    GBTree = test_generator:generate_gb_tree_from_number(10, 2),
%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n GBTree: ~p~n", [GBTree]),
%%%%    Iterator_01 = gb_trees:iterator(GBTree),
%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n Iterator_01: ~p~n", [Iterator_01]),
%%%%    {Key_01, Value_01, Iterator_02} = gb_trees:next(Iterator_01),
%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n Key: ~p~n Value: ~p~n Iterator_02: ~p~n", [Key_01, Value_01, Iterator_02]),
%%%%    {Key_02, Value_02, Iterator_03} = gb_trees:next(Iterator_02),
%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n Key: ~p~n Value: ~p~n Iterator_03: ~p~n", [Key_02, Value_02, Iterator_03]),
%%%%    {Key_03, Value_03, Iterator_04} = gb_trees:next(Iterator_03),
%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n Key: ~p~n Value: ~p~n Iterator_04: ~p~n", [Key_03, Value_03, Iterator_04]),
%%%%    {_Key_04, _Value_04, _Iterator_05} = gb_trees:next(Iterator_04),
%%%%    ?debugFmt("wwe debugging direct_test/0 ===> ~n Key: ~p~n Value: ~p~n Iterator_05: ~p~n", [_Key_04, _Value_04, _Iterator_05]),
%%
%%%%    ?debugFmt("wwe debugging direct_test/0 ===> Start ~n Key: ~p~n KeyValues: ~p~n Lower: ~p~n Upper: ~p~n", [Key, KeyValues, Lower, Upper]),
%%%%
%%%%    ?debugFmt("wwe debugging insert_insert_simple_split_test/0 =====================================> B-tree_04_00~n ~p~n", [b_trees:empty(4)]),
%%%%
%%%%    ?debugFmt("wwe debugging insert_insert_simple_split_test/0 =====================================> B-tree_05_01~n ~p~n", [test_generator:generate_b_tree_from_number(5, 01, 2)]),
%%
%%    ok.
