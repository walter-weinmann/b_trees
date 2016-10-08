<center> <h1> b_trees </h1> </center>

## MODULE ##

b_trees

## MODULE SUMMARY ##

B trees.

## DESCRIPTION ##

This module provides Prof. Arne Andersson's General Balanced Trees. These have no storage overhead compared to unbalanced binary trees, and their performance is better than AVL trees.

This module considers two keys as different if and only if they do not compare equal (==).

### Data Structure ###

    {Size, Tree}

Tree is composed of nodes of the form {Key, Value, Smaller, Bigger} and the "empty tree" node nil.

There is no attempt to balance trees after deletions. As deletions do not increase the height of a tree, this should be OK.

The original balance condition h(T) <= ceil(c * log(|T|)) has been changed to the similar (but not quite equivalent) condition 2 ^ h(T) <= |T| ^ c. This should also be OK.

## DATA TYPES ##

    tree(Key, Value)

A general balanced tree.

    tree() = tree(term(), term())
    iter(Key, Value)

A general balanced tree iterator.

    iter() = iter(term(), term())

## EXPORTS ##

balance(Tree1) -> Tree2

Types:

Tree1 = Tree2 = tree(Key, Value)
Rebalances Tree1. Notice that this is rarely necessary, but can be motivated when many nodes have been deleted from the tree without further insertions. Rebalancing can then be forced to minimize lookup times, as deletion does not rebalance the tree.

delete(Key, Tree1) -> Tree2

Types:

Tree1 = Tree2 = tree(Key, Value)
Removes the node with key Key from Tree1 and returns the new tree. Assumes that the key is present in the tree, crashes otherwise.

delete_any(Key, Tree1) -> Tree2

Types:

Tree1 = Tree2 = tree(Key, Value)
Removes the node with key Key from Tree1 if the key is present in the tree, otherwise does nothing. Returns the new tree.

empty() -> tree()

Returns a new empty tree.

enter(Key, Value, Tree1) -> Tree2

Types:

Tree1 = Tree2 = tree(Key, Value)
Inserts Key with value Value into Tree1 if the key is not present in the tree, otherwise updates Key to value Value in Tree1. Returns the new tree.

from_orddict(List) -> Tree

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

### See Also ###

dict(3), gb_sets(3)

