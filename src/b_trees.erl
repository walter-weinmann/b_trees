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
%% - is_empty(T): returns 'true' if T is an empty tree, and 'false'
%%   otherwise.
%%
%% - lookup(X, T): looks up key X in tree T; returns {value, V}, or
%%   `none' if the key is not present.
%%
%% - size(T): returns the number of nodes in the tree as an integer.
%%   Returns 0 (zero) if the tree is empty.

-module(b_trees).

-export([empty/1, empty/2, insert/3, is_empty/1, lookup/2, size/1]).

-ifdef(EUNIT).
-compile([export_all]).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data structure:
%% - {Minimum, Order, Size, Tree}, where `Tree' is composed of nodes of the form:
%%   - [{IsRoot, IsLeaf, Key, Value, Smaller, Bigger}, and the "empty tree" node:
%%   - nil.
%%
%% I make no attempt to balance trees after deletions. Since deletions
%% don't increase the height of a tree, I figure this is OK.
%%
%% Original balance condition h(T) <= ceil(c * log(|T|)) has been
%% changed to the similar (but not quite equivalent) condition 2 ^ h(T)
%% <= |T| ^ c. I figure this should also be OK.
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

generate_b_tree_from_number(Order, Number, Width) when Order > 2, Number >= 0 ->
    % ?debugFmt("wwe debugging generate_b_tree_from_number/3 ===> Start B Tree ~n Order : ~p~n Number: ~p~n Width : ~p~n", [Order, Number, Width]),
    generate_b_tree_1(lists:seq(1, Number), Width, empty(Order)).
generate_b_tree_from_number(Order, b_star, Number, Width) when Order > 2, Number >= 0 ->
    % ?debugFmt("wwe debugging generate_b_tree_from_number/3 ===> Start B* Tree ~n Order : ~p~n Number: ~p~n Width : ~p~n", [Order, Number, Width]),
    generate_b_tree_1(lists:seq(1, Number), Width, empty(Order, b_star)).

generate_b_tree_till_number(Order, Number, Width) when Order > 2, Number >= 0 ->
    % ?debugFmt("wwe debugging generate_b_tree_from_number/3 ===> Start B Tree ~n Order : ~p~n Number: ~p~n Width : ~p~n", [Order, Number, Width]),
    generate_b_tree_1(lists:seq(Number, 1, -1), Width, empty(Order)).
generate_b_tree_till_number(Order, b_star, Number, Width) when Order > 2, Number >= 0 ->
    % ?debugFmt("wwe debugging generate_b_tree_from_number/3 ===> Start B* Tree ~n Order : ~p~n Number: ~p~n Width : ~p~n", [Order, Number, Width]),
    generate_b_tree_1(lists:seq(Number, 1, -1), Width, empty(Order, b_star)).

generate_b_tree_1(Nodes, Width, BTree) ->
    % ?debugFmt("wwe debugging generate_b_tree_1/3 ===> Start ~n Nodes : ~p~n", [Nodes]),
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_b_tree_2(Nodes, Format, BTree).

generate_b_tree_2([], _, BTree) ->
    BTree;
generate_b_tree_2([Node | Tail], Format, BTree) ->
    LastString = lists:flatten(io_lib:format(Format, [Node])),
    generate_b_tree_2(Tail, Format, insert("k_" ++ LastString, "v_" ++ LastString, BTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_gb_tree_from_number(Number, Width) when Number >= 0 ->
    % ?debugFmt("wwe debugging generate_gb_tree_from_number/3 ===> Start GB Tree ~n Order : ~p~n Number: ~p~n Width : ~p~n", [Order, Number, Width]),
    generate_gb_tree_1(lists:seq(1, Number), Width, gb_trees:empty()).

generate_gb_tree_1(Nodes, Width, GBTree) ->
    % ?debugFmt("wwe debugging generate_gb_tree_1/3 ===> Start ~n Nodes : ~p~n", [Nodes]),
    Format = "~" ++ integer_to_list(Width) ++ "..0B",
    generate_gb_tree_2(Nodes, Format, GBTree).

generate_gb_tree_2([], _, GBTree) ->
    GBTree;
generate_gb_tree_2([Node | Tail], Format, GBTree) ->
    LastString = lists:flatten(io_lib:format(Format, [Node])),
    generate_gb_tree_2(Tail, Format, gb_trees:insert("k_" ++ LastString, "v_" ++ LastString, GBTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_key_value_1(NodeId, Key, Value, {Nodes, Min, Max} = BTree) ->
    % ?debugFmt("wwe debugging insert_key_value_1/4 ===> Start ~n NodeId: ~p~n Key: ~p~n Value: ~p~n BTree: ~p~n", [NodeId, Key, Value, BTree]),
    {NodeId, Level, Parent, Children, KeyValues} = lists:nth(NodeId, Nodes),
    {ValueCurr, ChildNo} = binary_search(KeyValues, Key, 1, length(KeyValues)),
    case ValueCurr of
        none ->
            case length(Children) of
                0 ->
                    KeyValuesNew = insert_new_key_value(KeyValues, {Key, Value}),
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

insert_new_key_value(KeyValues, KeyValueNew) ->
    lists:sort(fun({Key1, _}, {Key2, _}) ->
        Key1 < Key2
               end, [KeyValueNew] ++ KeyValues).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
                                                                              KeyValuesParentNew = insert_new_key_value(KeyValuesParent, {KeyMiddle, ValueMiddle}),
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
