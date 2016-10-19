-define(NODEBUG, true).

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
%% - get(K, B): retreives the value stored with key K in 
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
%% - keys(B): returns a list of all keys in B-tree / B*-tree B.
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
%% - to_list(B): returns a list of {Key, Value} pairs for all keys
%%   in B-tree / B*-tree B.
%%
%% - update(K, V, B): updates key K to value V in B-tree / B*-tree B; 
%%   returns the new tree. Assumes that the key is present in the tree.
%%
%% - values(B): returns the list of values for all keys in 
%%   B-tree / B*-tree B. Duplicates are not removed.
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
%% - next(S): returns {X, V, S1} where X is the smallest key referred to
%%   by the iterator S, and S1 is the new iterator to be used for
%%   traversing the remaining entries, or the atom `none' if no entries
%%   remain.
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
%%    map/1,
%%    next/1,
    number_key_values/1,
    size/1,
    smallest/1,
    to_list/1,
    update/3,
    values/1
]).

-define(BINARY_SEARCH_FROM_LENGTH, 4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data structure:
%% - {BTreeType, Minimum, Maximum, NumberKeyValues, Tree}, 
%%   where `Tree' is composed of :
%%   - {KeyNo, IsLeaf, [{Key, Value}], [Tree]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

-type key() :: any().
-type value() :: any().

-type key_value() :: {key(), value()}.
-type key_values() :: [key_value()].

-type tree() :: 'nil' | {pos_integer(), boolean(), key_values(), trees()}.
-type trees() :: [tree()].

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
    case is_defined(Key, BTree) of
        true ->
            update(Key, Value, BTree);
        false ->
            insert(Key, Value, BTree)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec from_dict(pos_integer(), [key_value()]) -> b_tree().
-spec from_dict(pos_integer(), atom(), [key_value()]) -> b_tree().

from_dict(Order, KeyValues) when Order > 3, length(KeyValues) > 0 ->
    BTree = {b, Order div 2, Order - 1, 0, nil},
    from_dict_insert(KeyValues, BTree).
from_dict(Order, b_star, KeyValues) when Order > 3, length(KeyValues) > 0 ->
    BTree = {b_star, Order * 2 div 3, Order - 1, 0, nil},
    from_dict_insert(KeyValues, BTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a specialized version of `lookup'.

-spec get(key(), b_tree()) -> value().

get(Key, {_, _, _, 0, nil}) ->
    erlang:error({key_not_found, Key});
get(Key, {_, _, _, _, Tree} = _X) ->
    case lookup_1(Key, Tree) of
        {value, Value} ->
            Value;
        _ ->
            erlang:error({key_not_found, Key})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec height(b_tree()) -> non_neg_integer().

height({_, _, _, 0, nil}) ->
    0;
height({_, _, _, _, Tree}) ->
    height_1(Tree, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert(key(), value(), b_tree()) -> b_tree().

insert(Key, Value, {BTreeType, KeyNoMin, KeyNoMax, 0, nil}) ->
    {BTreeType, KeyNoMin, KeyNoMax, 1, {1, true, [{Key, Value}], [nil, nil]}};
insert(Key, Value, {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, {KeyNo, _, KeyValues, _} = BTree}) ->
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
    {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues + 1, insert_into_tree({Key, Value}, TreeRepl, KeyNoMin, KeyNoMax)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a specialized version of `lookup'.

-spec is_defined(key(), b_tree()) -> boolean().

is_defined(_Key, {_, _, _, 0, nil}) ->
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

is_empty({_, _, _, 0, nil}) ->
    true;
is_empty(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec keys(b_tree()) -> [key()].

keys({_, _, _, 0, nil}) ->
    [];
keys({_, _, _, _, Tree}) ->
    keys_tree(Tree, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec largest(b_tree()) -> key_value().

largest({_, _, _, 0, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
largest({_, _, _, _, Tree}) ->
    largest_tree(Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec lookup(key(), b_tree()) -> 'none' | {'value', value()}.

lookup(_Key, {_, _, _, 0, nil}) ->
    none;
lookup(Key, {_, _, _, _, Tree}) ->
    lookup_1(Key, Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec number_key_values(b_tree()) -> non_neg_integer().

number_key_values({_, _, _, NumberKeyValues, _}) ->
    NumberKeyValues.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec size(b_tree()) -> non_neg_integer().

size({_, _, _, 0, nil}) ->
    0;
size({_, _, _, _, Tree}) ->
    size_tree(Tree, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec smallest(b_tree()) -> key_value().

smallest({_, _, _, 0, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
smallest({_, _, _, _, Tree}) ->
    smallest_tree(Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec to_list(b_tree()) -> [key_value()].

to_list({_, _, _, 0, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
to_list({_, _, _, _, Tree}) ->
    to_list_tree(Tree, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec update(key(), value(), b_tree()) -> b_tree().

update(_, _, {_, _, _, 0, nil} = BTree) ->
    erlang:error({empty_tree, BTree});
update(Key, Value, {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, Tree}) ->
    {BTreeType, KeyNoMin, KeyNoMax, NumberKeyValues, update_into_tree({Key, Value}, Tree)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec values(b_tree()) -> [value()].

values({_, _, _, 0, nil}) ->
    [];
values({_, _, _, _, Tree}) ->
    values_tree(Tree, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec binary_search(key(), key_values(), pos_integer(), pos_integer()) -> {none, pos_integer()} | {any(), pos_integer()}.

binary_search(Key, KeyValues, Lower, Upper) when Lower > Upper ->
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

-spec from_dict_insert([key_value(), ...], b_tree()) -> b_tree().

from_dict_insert([], BTree) ->
    BTree;
from_dict_insert([{Key, Value} | Tail], BTree) ->
    from_dict_insert(Tail, insert(Key, Value, BTree)).

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
    TreeOut = {KeyNo + 1, IsLeaf, insert_into_key_values(KeyValue, KeyValues, []), Trees ++ [nil]},
    TreeOut;
insert_into_tree({Key, _} = KeyValue, {KeyNo, false = IsLeaf, KeyValues, Trees} = _Tree, KeyNoMin, KeyNoMax) ->
    {ValueFound, TreeUpperPos} = search(Key, KeyValues, KeyNo, ?BINARY_SEARCH_FROM_LENGTH),
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
                                                                                  {none, TreeUpperPosSplit} = search(Key, KeyValuesSplit, length(KeyValuesSplit), ?BINARY_SEARCH_FROM_LENGTH),
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

-spec keys([key_value(), ...], [key()]) -> [key()].

keys([], Keys) ->
    Keys;
keys([{Key, _} | Tail], Keys) ->
    keys(Tail, Keys ++ [Key]).

-spec keys_tree(tree(), [key()]) -> [key()].

keys_tree({_, true, KeyValues, _}, Keys) ->
    keys(KeyValues, Keys);
keys_tree({_, _, KeyValues, Trees}, Keys) ->
    keys_trees(Trees, keys(KeyValues, Keys)).

-spec keys_trees([tree(), ...], [key()]) -> [key()].

keys_trees([], Keys) ->
    Keys;
keys_trees([Tree | Tail], Keys) ->
    keys_trees(Tail, keys_tree(Tree, Keys)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec largest_tree(tree()) -> key_value().

largest_tree({_, true, KeyValues, _}) ->
    lists:nth(length(KeyValues), KeyValues);
largest_tree({_, _, _, Trees}) ->
    largest_tree(lists:nth(length(Trees), Trees)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The term order is an arithmetic total order, so we should not
%% test exact equality for the keys. (If we do, then it becomes
%% possible that neither `>', `<', nor `=:=' matches.) Testing '<'
%% and '>' first is statistically better than testing for
%% equality, and also allows us to skip the test completely in the
%% remaining case.

-spec lookup_1(key(), tree()) -> 'none' | {'value', value()}.

lookup_1(Key, {_, IsLeaf, KeyValues, ChildTrees}) ->
    {Value, Pos} = sequential_search(Key, KeyValues, 0),
    case Value == none of
        true -> case IsLeaf of
                    true ->
                        none;
                    _ ->
                        ChildTree = lists:nth(Pos, ChildTrees),
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
        true ->
            {none, PosNew};
        _ ->
            case Key > KeyLast of
                true ->
                    sequential_search(Key, Tail, PosNew);
                _ ->
                    {ValueLast, PosNew}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

-spec smallest_tree(tree()) -> key_value().

smallest_tree({_, true, KeyValues, _}) ->
    lists:nth(1, KeyValues);
smallest_tree({_, _, _, Trees}) ->
    smallest_tree(lists:nth(1, Trees)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec split_tree_non_root(tree(), pos_integer(), key_values(), trees(), pos_integer()) -> {key_values(), trees()}.

split_tree_non_root({KeyNo, IsLeaf, KeyValues, Trees} = _Tree, KeyNoSplit, KeyValuesLower, TreesLower, TreeNo) ->
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

-spec to_list([key_value(), ...], [value()]) -> [key_value()].

to_list([], KeyValueList) ->
    KeyValueList;
to_list([{Key, Value} | Tail], KeyValueList) ->
    to_list(Tail, KeyValueList ++ [{Key, Value}]).

-spec to_list_tree(tree(), [key_value()]) -> [key_value()].

to_list_tree({_, true, KeyValues, _}, KeyValueList) ->
    to_list(KeyValues, KeyValueList);
to_list_tree({_, _, KeyValues, Trees}, KeyValueList) ->
    to_list_trees(Trees, to_list(KeyValues, KeyValueList)).

-spec to_list_trees([tree(), ...], [key_value()]) -> [key_value()].

to_list_trees([], KeyValueList) ->
    KeyValueList;
to_list_trees([Tree | Tail], KeyValueList) ->
    to_list_trees(Tail, to_list_tree(Tree, KeyValueList)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec update_into_tree(key_value(), tree()) -> {tree(), boolean()}.

update_into_tree({Key, _} = KeyValue, {KeyNo, IsLeaf, KeyValues, Trees}) ->
    {ValueFound, KeyPos} = search(Key, KeyValues, KeyNo, ?BINARY_SEARCH_FROM_LENGTH),
    case ValueFound of
        none ->
            case IsLeaf of
                true ->
                    erlang:error({key_not_found, Key});
                _ ->
                    {
                        KeyNo,
                        IsLeaf,
                        KeyValues,
                            lists:sublist(Trees, 1, KeyPos - 1) ++
                            [update_into_tree(KeyValue, lists:nth(KeyPos, Trees))] ++
                            lists:sublist(Trees, KeyPos + 1, length(Trees))
                    }
            end;
        _ ->
            {
                KeyNo,
                IsLeaf,
                    lists:sublist(KeyValues, 1, KeyPos - 1) ++
                    [KeyValue] ++
                    lists:sublist(KeyValues, KeyPos + 1, length(Trees)),
                Trees
            }
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec values([key_value(), ...], [value()]) -> [value()].

values([], Values) ->
    Values;
values([{_, Value} | Tail], Values) ->
    values(Tail, Values ++ [Value]).

-spec values_tree(tree(), [value()]) -> [value()].

values_tree({_, true, KeyValues, _}, Values) ->
    values(KeyValues, Values);
values_tree({_, _, KeyValues, Trees}, Values) ->
    values_trees(Trees, values(KeyValues, Values)).

-spec values_trees([tree(), ...], [value()]) -> [value()].

values_trees([], Values) ->
    Values;
values_trees([Tree | Tail], Values) ->
    values_trees(Tail, values_tree(Tree, Values)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%direct_test() ->
%%
%%%%    ?debugFmt("wwe debugging binary_search/4 ===> Start ~n Key: ~p~n KeyValues: ~p~n Lower: ~p~n Upper: ~p~n", [Key, KeyValues, Lower, Upper]),
%%%%
%%%%    ?debugFmt("wwe debugging insert_insert_simple_split_test/0 =====================================> B-tree_04_00~n ~p~n", [b_trees:empty(4)]),
%%%%
%%%%    ?debugFmt("wwe debugging insert_insert_simple_split_test/0 =====================================> B-tree_05_01~n ~p~n", [test_generator:generate_b_tree_from_number(5, 01, 2)]),
%%
%%    ok.
