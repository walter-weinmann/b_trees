% -define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

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
%% Copyright (C) 2016 SWalter Weinmann
%%
%% An efficient implementation of Prof. Arne Andersson's General ???
%% Balanced Trees. These have no storage overhead compared to plain
%% unbalanced binary trees, and their performance is in general better
%% than AVL trees.
%% -----------------------------------------------------------------------------
%% Operations:
%%
%% - empty(O): returns empty B tree of order O.
%%
%% - empty(O, b_star): returns empty B* tree of order O.
%%
%% - insert(X, V, T): inserts key X with value V into tree T; returns
%%   the new tree. Assumes that the key is *not* present in the tree.
%%
%% - is_defined(X, T): returns `true' if key X is present in tree T, and
%%   `false' otherwise.
%%
%% - is_empty(T): returns 'true' if T is an empty tree, and 'false'
%%   otherwise.
%%
%% - lookup(X, T): looks up key X in tree T; returns {value, V}, or
%%   `none' if the key is not present.
%%
%% - size(T): returns the number of nodes in the tree as an integer.
%%   Returns 0 (zero) if the tree is empty.
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
%% - keys(T): returns an ordered list of all keys in tree T.
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

-export([empty/1, empty/2, insert/3, is_empty/1, lookup/2, size/1]).

-ifdef(EUNIT).
-compile([export_all]).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data structure:
%% - {Tree, Minimum, Maximum}, where `Tree' is composed of nodes of the form:
%%   - [{NodeId, Level, Parent, Children, KeyValues}.
%%
%% Performance is comparable to the AVL trees in the Erlang book (and
%% faster in general due to less overhead); the difference is that
%% deletion works for my trees, but not for the book's trees. Behaviour
%% is logaritmic (as it should be).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some macros.

%%-define(p, 2). % It seems that p = 2 is optimal for sorted keys
%%
%%-define(pow(A, _), A * A). % correct with exponent as defined above.
%%
%%-define(div2(X), X bsr 1).
%%
%%-define(mul2(X), X bsl 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

%%-export_type([tree/0, tree/2, iter/0, iter/2]).
%%
%%-type b_tree_node(K, V) :: {non_neg_integer(),non_neg_integer(),non_neg_integer(),[],[]}
%%| {non_neg_integer(), non_neg_integer(),non_neg_integer(), [{K, V}, ...], [{'nil', 'nil'} | b_tree_node(K, V)]}.
%%
%%-opaque tree(Key, Value) :: {non_neg_integer(), non_neg_integer(), non_neg_integer(), b_tree_node(Key, Value)}.
%%-type tree() :: tree(b_tree_node(Key, Value), _, {non_neg_integer(), non_neg_integer()}).
%%
%%-opaque iter(Key, Value) :: [b_tree_node(Key, Value)].
%%-type iter() :: iter(_, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-spec empty(pos_integer()) -> tree().
%%-spec empty(pos_integer(), atom()) -> tree().

empty(Order) when Order > 2 ->
    {[], Order div 2, Order - 1}.
empty(Order, b_star) when Order > 2 ->
    {[], Order * 2 div 3, Order - 1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-spec insert(Key, Value, Tree1) -> Tree2 when
%%    Tree1 :: tree(Key, Value),
%%    Tree2 :: tree(Key, Value).

insert(Key, Value, {[], Min, Max} = _BTree) ->
    % ?debugFmt("wwe debugging insert/3 ===> Start ~n Key: ~p~n Value: ~p~n BTree: ~p~n", [Key, Value, _BTree]),
    {[{1, 1, 0, [], [{Key, Value}]}], Min, Max};
insert(Key, Value, BTree) ->
    % ?debugFmt("wwe debugging insert/3 ===> Start ~n Key: ~p~n Value: ~p~n BTree: ~p~n", [Key, Value, BTree]),
    insert_key_value_1(1, Key, Value, BTree).

insert_key_value_1(NodeId, Key, Value, {Nodes, Min, Max} = BTree) ->
    % ?debugFmt("wwe debugging insert_key_value_1/4 ===> Start ~n NodeId: ~p~n Key: ~p~n Value: ~p~n BTree: ~p~n", [NodeId, Key, Value, BTree]),
    {NodeId, Level, Parent, Children, KeyValues} = lists:nth(NodeId, Nodes),
    {ValueCurr, ChildNo} = binary_search(KeyValues, Key, 1, length(KeyValues)),
    case ValueCurr of
        none ->
            case length(Children) of
                0 ->
                    KeyValuesNew = insert_key_value(KeyValues, [{Key, Value}], []),
                    NodesNew = update_nodes(Nodes, {NodeId, Level, Parent, Children, KeyValuesNew}),
                    BTreeNew = {NodesNew, Min, Max},
                    case length(KeyValuesNew) > Max of
                        true -> node_split(BTreeNew, NodeId);
                        _ -> BTreeNew
                    end;
                _ ->
                    insert_key_value_1(lists:nth(ChildNo, Children), Key, Value, BTree)
            end;
        _ ->
            erlang:error({key_exists, Key})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-spec is_empty(Tree) -> boolean() when
%%    Tree :: tree().

is_empty({[], _, _}) ->
    true;
is_empty(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-spec lookup(Key, Tree) -> 'none' | {'value', Value} when
%%    Tree :: tree(Key, Value).

lookup(_, {[], _, _}) ->
    none;
lookup(Key, BTree) ->
    lookup_nodes(1, Key, BTree).

%% The term order is an arithmetic total order, so we should not
%% test exact equality for the keys. (If we do, then it becomes
%% possible that neither `>', `<', nor `=:=' matches.) Testing '<'
%% and '>' first is statistically better than testing for
%% equality, and also allows us to skip the test completely in the
%% remaining case.

lookup_nodes(NodeId, Key, {Nodes, _, _} = BTree) ->
    % ?debugFmt("wwe debugging lookup_nodes/3 ===> Start ~n NodeId: ~p~n Key: ~p~n BTree: ~p~n Nodes: ~p~n", [NodeId, Key, BTree, Nodes]),
    {_, _, _, Children, KeyValues} = lists:nth(NodeId, Nodes),
    % ?debugFmt("wwe debugging lookup_nodes/3 ===> ~n Children: ~p~n KeyValues: ~p~n", [Children, KeyValues]),
    {Value, ChildNo} = binary_search(KeyValues, Key, 1, length(KeyValues)),
    % ?debugFmt("wwe debugging lookup_nodes/3 ===> ~n Value: ~p~n Index: ~p~n", [Value, ChildNo]),
    case Value of
        none ->
            case length(Children) of
                0 ->
                    none;
                _ ->
                    lookup_nodes(lists:nth(ChildNo, Children), Key, BTree)
            end;
        _ ->
            Value
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-spec size(Tree) -> non_neg_integer() when
%%    Tree :: tree().

size({Nodes, _, _}) ->
    length(Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

binary_search(Node, Key, Lower, Upper) when Lower > Upper ->
    % ?debugFmt("wwe debugging binary_search/4 ===> Start ~n Node: ~p~n Key: ~p~n Lower: ~p~n Upper: ~p~n", [Node, Key, Lower, Upper]),
    ChildNo = case Lower > length(Node) of
                  true -> Upper;
                  _ -> Lower
              end,
    {KeyLast, _} = lists:nth(ChildNo, Node),
    case Key < KeyLast of
        true -> {none, ChildNo};
        _ -> {none, ChildNo + 1}
    end;
binary_search(Node, Key, Lower, Upper) ->
    % ?debugFmt("wwe debugging binary_search/4 ===> Start ~n Node: ~p~n Key: ~p~n Lower: ~p~n Upper: ~p~n", [Node, Key, Lower, Upper]),
    Mid = (Upper + Lower) div 2,
    {MidKey, MidValue} = lists:nth(Mid, Node),
    if
        Key > MidKey ->
            binary_search(Node, Key, Mid + 1, Upper);
        Key < MidKey ->
            binary_search(Node, Key, Lower, Mid - 1);
        true ->
            {MidValue, Mid}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_key_value([], [], KeyValuesAcc) ->
    KeyValuesAcc;
insert_key_value([], KeyValueNew, KeyValuesAcc) ->
    KeyValuesAcc ++ KeyValueNew;
insert_key_value([{KeyCurr, _} = KeyValueCurr | Tail] = KeyValuesCurr, [{KeyNew, _}] = KeyValueNew, KeyValuesAcc) ->
    case KeyCurr < KeyNew of
        true -> insert_key_value(Tail, KeyValueNew, KeyValuesAcc ++ [KeyValueCurr]);
        _ -> insert_key_value([], [], KeyValuesAcc ++ KeyValueNew ++ KeyValuesCurr)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

node_split({Nodes, Min, Max} = _BTree, NodeId) ->
    % ?debugFmt("wwe debugging node_split/2 ===> Start ~n BTree: ~p~n NodeId: ~p~n", [_BTree, NodeId]),
    {_, _, ParentSplit, _, KeyValuesSplit} = lists:nth(NodeId, Nodes),
    {KeyMiddle, ValueMiddle} = lists:nth(Min + 1, KeyValuesSplit),
    % ?debugFmt("wwe debugging node_split/2 ===> ~n KeyMiddle: ~p~n ValueMiddle: ~p~n", [KeyMiddle, ValueMiddle]),
    {{_, LevelSplit, _, _, _} = NodeSplit, ToBeSplitted, IsNewRoot} = case ParentSplit of
                                                                          0 ->
                                                                              {
                                                                                  {1, 1, 0, [], [{KeyMiddle, ValueMiddle}]},
                                                                                  0,
                                                                                  true};
                                                                          _ ->
                                                                              {NodeIdParent, LevelParent, _, _, KeyValuesParent} = lists:nth(ParentSplit, Nodes),
                                                                              KeyValuesParentNew = insert_key_value(KeyValuesParent, [{KeyMiddle, ValueMiddle}], []),
                                                                              {
                                                                                  {NodeIdParent, LevelParent, 0, [], KeyValuesParentNew},
                                                                                  case length(KeyValuesParentNew) > Max of
                                                                                      true ->
                                                                                          NodeIdParent;
                                                                                      _ ->
                                                                                          0
                                                                                  end,
                                                                                  false}
                                                                      end,
    % ?debugFmt("wwe debugging node_split/2 ===> ~n NodeSplit: ~p~n", [NodeSplit]),
    KeyValuesParentSmaller = lists:sublist(KeyValuesSplit, Min),
    NodeSmaller = {case ParentSplit of
                       0 -> 2;
                       _ -> NodeId
                   end, LevelSplit + 1, 0, [], KeyValuesParentSmaller},
    % ?debugFmt("wwe debugging node_split/2 ===> ~n NodeSmaller: ~p~n", [NodeSmaller]),
    KeyValuesParentBigger = lists:sublist(KeyValuesSplit, Min + 2, Max - Min),
    NodeBigger = {case ParentSplit of
                      0 -> 3;
                      _ -> NodeId + 1
                  end, LevelSplit + 1, 0, [], KeyValuesParentBigger},
    % ?debugFmt("wwe debugging node_split/2 ===> ~n NodeBigger: ~p~n", [NodeBigger]),

    NodesNew = update_nodes_split(Nodes, NodeSplit, NodeSmaller, NodeBigger, [], case IsNewRoot of
                                                                                     true ->
                                                                                         2;
                                                                                     _ ->
                                                                                         1
                                                                                 end, case IsNewRoot of
                                                                                          true ->
                                                                                              1;
                                                                                          _ ->
                                                                                              0
                                                                                      end, []),
    BTreeNew = {NodesNew, Min, Max},
    case ToBeSplitted of
        0 -> BTreeNew;
        _ -> node_split(BTreeNew, ToBeSplitted)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_nodes(Nodes, {NodeIdNew, _, _, _, _} = NodeNew) ->
    [case NodeIdCurr == NodeIdNew of
         true -> NodeNew;
         _ -> Node
     end || {NodeIdCurr, _, _, _, _} = Node <- Nodes].

update_nodes_children(ChildNext, ChildrenNo, NodeId, ChildrenMap) ->
    % ?debugFmt("wwe debugging update_nodes_children/4 ===> Start~n ChildNext: ~p~n ChildrenNo: ~p~n NodeId: ~p~n ChildrenMap: ~p~n", [ChildNext, ChildrenNo, NodeId, ChildrenMap]),
    Children = [Child || Child <- lists:seq(ChildNext, ChildNext + ChildrenNo)],
    {Children, update_nodes_children(Children, NodeId, ChildrenMap)}.

update_nodes_children([], _, ChildrenMap) ->
    % ?debugFmt("wwe debugging update_nodes_children/3 ===> Start~n ChildrenMap: ~p~n", [ChildrenMap]),
    ChildrenMap;
update_nodes_children([Child | Tail], NodeId, ChildrenMap) ->
    % ?debugFmt("wwe debugging update_nodes_children/3 ===> Start~n Child: ~p~n NodeId: ~p~n ChildrenMap: ~p~n", [Child, NodeId, ChildrenMap]),
    update_nodes_children(Tail, NodeId, maps:put(Child, NodeId, ChildrenMap)).

update_nodes_children_parents(Nodes) ->
    % ?debugFmt("wwe debugging update_nodes_children_parents/1 ===> Start~n Nodes: ~p~n", [Nodes]),
    {_, LevelLeaves, _, _, _} = lists:nth(length(Nodes), Nodes),
    update_nodes_children_parents(Nodes, 2, maps:new(), LevelLeaves, []).

update_nodes_children_parents([], _, _, _, NodesNew) ->
    % ?debugFmt("wwe debugging update_nodes_children_parents/5 ===> Start~n NodesNew: ~p~n", [NodesNew]),
    NodesNew;
update_nodes_children_parents([{NodeId, Level, Parent, _, KeyValues} = _Node | Tail], ChildNext, ChildrenMap, LevelLeaves, NodesNew) when NodeId == 1 ->
    % ?debugFmt("wwe debugging update_nodes_children_parents/5 ===> Start ~n Node: ~p~n ChildNext: ~p~n ChildrenMap: ~p~n LevelLeaves: ~p~n NodesNew: ~p~n", [Node, ChildNext, ChildrenMap, LevelLeaves, NodesNew]),
    ChildrenNo = length(KeyValues),
    {ChildrenNew, ChildrenMapNew} = update_nodes_children(ChildNext, ChildrenNo, NodeId, ChildrenMap),
    update_nodes_children_parents(Tail, ChildNext + ChildrenNo + 1, ChildrenMapNew, LevelLeaves, NodesNew ++ [{NodeId, Level, Parent, ChildrenNew, KeyValues}]);
update_nodes_children_parents([{NodeId, Level, _, Children, KeyValues} | Tail] = _Node, ChildNext, ChildrenMap, LevelLeaves, NodesNew) when Level == LevelLeaves ->
    % ?debugFmt("wwe debugging update_nodes_children_parents/5 ===> Start ~n Node: ~p~n ChildNext: ~p~n ChildrenMap: ~p~n LevelLeaves: ~p~n NodesNew: ~p~n", [Node, ChildNext, ChildrenMap, LevelLeaves, NodesNew]),
    update_nodes_children_parents(Tail, ChildNext, ChildrenMap, LevelLeaves, NodesNew ++ [{NodeId, Level, maps:get(NodeId, ChildrenMap), Children, KeyValues}]);
update_nodes_children_parents([{NodeId, Level, _, _, KeyValues} = _Node | Tail], ChildNext, ChildrenMap, LevelLeaves, NodesNew) ->
    % ?debugFmt("wwe debugging update_nodes_children_parents/5 ===> Start ~n Node: ~p~n ChildNext: ~p~n ChildrenMap: ~p~n LevelLeaves: ~p~n NodesNew: ~p~n", [Node, ChildNext, ChildrenMap, LevelLeaves, NodesNew]),
    ChildrenNo = length(KeyValues),
    {ChildrenNew, ChildrenMapNew} = update_nodes_children(ChildNext, ChildrenNo, NodeId, ChildrenMap),
    update_nodes_children_parents(Tail, ChildNext + ChildrenNo + 1, ChildrenMapNew, LevelLeaves, NodesNew ++ [{NodeId, Level, maps:get(NodeId, ChildrenMapNew), ChildrenNew, KeyValues}]).

update_nodes_split([], _, {NodeIdSmaller, _, _, _, _} = NodeSmaller, {NodeIdBigger, _, _, _, _} = NodeBigger, NodesDelayed, _, _, NodesNew) ->
    % ?debugFmt("wwe debugging update_nodes_split/8 ===> Start ~n NodeSmaller: ~p~n NodeBigger: ~p~n NodesNew: ~p~n", [NodeSmaller, NodeBigger, NodesNew]),
    NodesNew2 = case NodeIdSmaller > length(NodesNew) of
                    true -> NodesNew ++ [NodeSmaller];
                    _ -> NodesNew
                end ++ NodesDelayed,
    update_nodes_children_parents(case NodeIdBigger > length(NodesNew2) of
                                      true -> NodesNew2 ++ [NodeBigger];
                                      _ -> NodesNew2
                                  end);

update_nodes_split([{NodeIdCurr, LevelCurr, ParentCurr, ChildrenCurr, KeyValuesCurr} = NodeCurr | Tail] = _Nodes,
    {NodeIdSplit, _, _, _, _} = NodeSplit,
    {NodeIdSmaller, _, _, _, _} = NodeSmaller,
    {NodeIdBigger, _, _, _, _} = NodeBigger,
    NodesDelayed, NoNewNodes, NoNewLevel, NodesNew) ->
    % ?debugFmt("wwe debugging update_nodes_split/8 ===> Start ~n Nodes: ~p~n NodeSplit: ~p~n NodeSmaller: ~p~n NodeBigger: ~p~n NodesDelayed: ~p~n NoNewNodes: ~p~n NoNewLevel: ~p~n NodesNew: ~p~n", [_Nodes, NodeSplit, NodeSmaller, NodeBigger, NodesDelayed, NoNewNodes, NoNewLevel, NodesNew]),
    {NodeOut, NodesDelayedNew} = if
                                     NodeIdCurr < NodeIdSplit ->
                                         {[NodeCurr], NodesDelayed};
                                     NodeIdCurr == NodeIdSplit ->
                                         {[NodeSplit], NodesDelayed};
                                     NodeIdCurr < NodeIdSmaller ->
                                         {[NodeCurr], NodesDelayed};
                                     NodeIdCurr == NodeIdSmaller ->
                                         case NoNewLevel == 1 of
                                             true ->
                                                 {[NodeSmaller], NodesDelayed ++ [{NodeIdCurr + NoNewNodes, LevelCurr + NoNewLevel, ParentCurr, ChildrenCurr, KeyValuesCurr}]};
                                             _ ->
                                                 {[NodeSmaller], NodesDelayed}
                                         end;
                                     NodeIdCurr == NodeIdBigger ->
                                         {[NodeBigger], NodesDelayed ++ [{NodeIdCurr + NoNewNodes, LevelCurr + NoNewLevel, ParentCurr, ChildrenCurr, KeyValuesCurr}]};
                                     true ->
                                         {NodesDelayed ++ [{NodeIdCurr + NoNewNodes, LevelCurr + NoNewLevel, ParentCurr, ChildrenCurr, KeyValuesCurr}], []}
                                 end,
    update_nodes_split(Tail, NodeSplit, NodeSmaller, NodeBigger, NodesDelayedNew, NoNewNodes, NoNewLevel, NodesNew ++ NodeOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
