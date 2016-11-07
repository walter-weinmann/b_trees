<center> <h1> b_trees </h1> </center>

## MODULE ##

b_trees

## MODULE SUMMARY ##

B-trees: balanced search trees of order m in which each non-leaf node has up to m children.

## DESCRIPTION ##

A B-tree is a self-balancing tree data structure that keeps data sorted and allows searches, sequential access, insertions, and deletions in logarithmic  time. The B-tree is a generalization of a binary search tree in that a node can have more than two children. Unlike self-balancing binary search trees, the  B-tree is optimized for systems that read and write large blocks of data.

This module considers two keys as different if and only if they do not compare equal (==).

### Data Structure ###

    {MinimumSubtrees, MaximumKeys, NumberKeyValues, Tree}

Tree is composed of nodes of the form 

    {KeyNumber, SubtreeNumber, [{Key, Value}], [Tree]} 

and the "empty tree" node 

    nil

Since the trees are always balanced, there is no need for a balance operation.


## DATA TYPES ##

    b_tree() = {pos_integer(), pos_integer(), non_neg_integer(), tree()}

A general balanced tree.

    iterator() = [{key_values(), subtrees()}]

A general balanced tree iterator.


## EXPORTS ##

### delete(Key, B-Tree1) -> B-Tree2 ###

Types:

    Key = any()
    B-Tree1 = B-Tree2 = b_tree()

Removes the node with key Key from B-Tree1 and returns the new B-Tree2. Assumes that the key is present in B-Tree1, crashes otherwise.

### delete_any (Key, B-Tree1) -> B-Tree2 ###

Types:

    Key = any()
    B-Tree1 = B-Tree2 = b_tree()

Removes the node with key Key from B-Tree1 if the key is present in B-Tree1, otherwise does nothing. Returns the new B-Tree2.

### empty (Order) -> B-Tree ###

Types:

    Order = pos_integer()
    B-Tree = b_tree()

Returns a new empty b-tree. Order is defined as the maximum number of children nodes a non-leaf node may hold. The minimum value is 4.

### enter (Key, Value, B-Tree1) -> B-Tree2 ###

Types:

    Key = any()
    Value = any()
    B-Tree1 = B-Tree2 = b_tree()

Inserts key Key with value Value into B-Tree1 if the key is not present in B-Tree1, otherwise updates key Key to value Value in B-Tree1. Returns the new B-Tree2.

### from_orddict (Order, List) -> B-Tree ###

Types:

List = [{Key, Value}]
Tree = tree(Key, Value)
Turns an ordered list List of key-value tuples into a tree. The list must not contain duplicate keys.

get(Key, Tree) -> Value

Types:

Tree = tree(Key, Value)
Retrieves the value stored with Key in Tree. Assumes that the key is present in the tree, crashes otherwise.

insert(Key, Value, Tree1) -> Tree2

Types:

Tree1 = Tree2 = tree(Key, Value)
Inserts Key with value Value into Tree1 and returns the new tree. Assumes that the key is not present in the tree, crashes otherwise.

is_defined(Key, Tree) -> boolean()

Types:

Tree = tree(Key, Value :: term())
Returns true if Key is present in Tree, otherwise false.

is_empty(Tree) -> boolean()

Types:

Tree = tree()
Returns true if Tree is an empty tree, othwewise false.

iterator(Tree) -> Iter

Types:

Tree = tree(Key, Value)
Iter = iter(Key, Value)
Returns an iterator that can be used for traversing the entries of Tree; see next/1. The implementation of this is very efficient; traversing the whole tree using next/1 is only slightly slower than getting the list of all elements using to_list/1 and traversing that. The main advantage of the iterator approach is that it does not require the complete list of all elements to be built in memory at one time.

iterator_from(Key, Tree) -> Iter

Types:

Tree = tree(Key, Value)
Iter = iter(Key, Value)
Returns an iterator that can be used for traversing the entries of Tree; see next/1. The difference as compared to the iterator returned by iterator/1 is that the first key greater than or equal to Key is returned.

keys(Tree) -> [Key]

Types:

Tree = tree(Key, Value :: term())
Returns the keys in Tree as an ordered list.

largest(Tree) -> {Key, Value}

Types:

Tree = tree(Key, Value)
Returns {Key, Value}, where Key is the largest key in Tree, and Value is the value associated with this key. Assumes that the tree is not empty.

lookup(Key, Tree) -> none | {value, Value}

Types:

Tree = tree(Key, Value)
Looks up Key in Tree. Returns {value, Value}, or none if Key is not present.

map(Function, Tree1) -> Tree2

Types:

Function = fun((K :: Key, V1 :: Value1) -> V2 :: Value2)
Tree1 = tree(Key, Value1)
Tree2 = tree(Key, Value2)
Maps function F(K, V1) -> V2 to all key-value pairs of tree Tree1. Returns a new tree Tree2 with the same set of keys as Tree1 and the new set of values V2.

next(Iter1) -> none | {Key, Value, Iter2}

Types:

Iter1 = Iter2 = iter(Key, Value)
Returns {Key, Value, Iter2}, where Key is the smallest key referred to by iterator Iter1, and Iter2 is the new iterator to be used for traversing the remaining nodes, or the atom none if no nodes remain.

size(Tree) -> integer() >= 0

Types:

Tree = tree()
Returns the number of nodes in Tree.

smallest(Tree) -> {Key, Value}

Types:

Tree = tree(Key, Value)
Returns {Key, Value}, where Key is the smallest key in Tree, and Value is the value associated with this key. Assumes that the tree is not empty.

take_largest(Tree1) -> {Key, Value, Tree2}

Types:

Tree1 = Tree2 = tree(Key, Value)
Returns {Key, Value, Tree2}, where Key is the largest key in Tree1, Value is the value associated with this key, and Tree2 is this tree with the corresponding node deleted. Assumes that the tree is not empty.

take_smallest(Tree1) -> {Key, Value, Tree2}

Types:

Tree1 = Tree2 = tree(Key, Value)
Returns {Key, Value, Tree2}, where Key is the smallest key in Tree1, Value is the value associated with this key, and Tree2 is this tree with the corresponding node deleted. Assumes that the tree is not empty.

to_list(Tree) -> [{Key, Value}]

Types:

Tree = tree(Key, Value)
Converts a tree into an ordered list of key-value tuples.

update(Key, Value, Tree1) -> Tree2

Types:

Tree1 = Tree2 = tree(Key, Value)
Updates Key to value Value in Tree1 and returns the new tree. Assumes that the key is present in the tree.

values(Tree) -> [Value]

Types:

Tree = tree(Key :: term(), Value)
Returns the values in Tree as an ordered list, sorted by their corresponding keys. Duplicates are not removed.

## See Also ##

[gb_trees](http://erlang.org/doc/man/gb_trees.html)

