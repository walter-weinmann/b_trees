<center> <h1> b_trees </h1> </center>

[![Build Status](https://travis-ci.org/walter-weinmann/b_trees.svg?branch=master)](https://travis-ci.org/walter-weinmann/b_trees)


## MODULE ##

**b_trees**

## MODULE SUMMARY ##

A module for balanced n-ary search trees of order `n` in which each non-leaf node has up to `n` children.

## DESCRIPTION ##

A B-Tree is a self-balancing tree data structure that keeps data sorted and allows searches, sequential access, insertions, and deletions in logarithmic  time. The B-Tree is a generalization of a binary search tree in that a node can have more than two children. Unlike self-balancing binary search trees, the  B-tree is optimized for systems that read and write large blocks of data.

This module considers two keys as different if and only if they do not compare equal (==).

### Data Structure ###

    {MinimumSubtrees, MaximumKeys, NumberKeyValues, SortFunction, Tree}

Tree is composed of nodes of the form 

    {KeyNumber, SubtreeNumber, [{Key, Value}], [Tree]} 

and the "empty tree" node 

    nil

Since the trees are always balanced, there is no need for a balance operation.


## DATA TYPES ##

    b_tree() = {pos_integer(), pos_integer(), non_neg_integer(), sort_function(), tree()}

A general balanced tree.

    iterator() = [{key_values(), subtrees()}]

A general balanced tree iterator.


## EXPORTS ##

### delete(Key, B-Tree1) -> B-Tree2 ###

Types:

    Key = any()
    B-Tree1 = B-Tree2 = b_tree()

Removes the node with key Key from b-tree B-Tree1 and returns the new b-tree B-Tree2. Assumes that Key is present in B-Tree1, crashes otherwise.

### delete_any (Key, B-Tree1) -> B-Tree2 ###

Types:

    Key = any()
    B-Tree1 = B-Tree2 = b_tree()

Removes the node with key Key from b-tree B-Tree1 if Key is present in B-Tree1, otherwise does nothing. Returns the new b-tree B-Tree2.

### empty (Order) -> B-Tree ###

Types:

    Order = pos_integer()
    B-Tree = b_tree()

Returns a new empty b-tree. The order is defined as the maximum number of children nodes a non-leaf node may hold. The minimum value is 4. The sort order of the key values is ascending.

### empty (Order, Function) -> B-Tree ###

Types:

    Order = pos_integer()
    Function = fun((Key1, Key2) -> equal | greater | less)
    B-Tree = b_tree()

Returns a new empty b-tree. The order is defined as the maximum number of children nodes a non-leaf node may hold. The minimum value is 4. The sort order of the key values is defined by the given function, which takes two key values and returns one of the atoms **equal**, **greater** or **less**.

### enter (Key, Value, B-Tree1) -> B-Tree2 ###

Types:

    Key = any()
    Value = any()
    B-Tree1 = B-Tree2 = b_tree()

Inserts key Key with value Value into b-tree B-Tree1 if Key is not present in B-Tree1, otherwise updates the current value of Key to Value in B-Tree1. Returns a new b-tree B-Tree2.

### from_dict (B-Tree1, List) -> B-Tree2 ###

Types:

    B-Tree1 = B-Tree2 = b_tree()
    List = [{Key, Value}]


Turns an ordered list List of key value tuples into a b-tree. The given b-tree B-Tree1 must be empty. The list must not contain duplicate keys.

### get (Key, B-Tree) -> Value ###

Types:

    Key = any()
    B-Tree = b_tree()
    Value = any()
    
Retrieves the value stored with key Key in b-tree B-Tree. Assumes that Key is present in B-Tree, crashes otherwise.

### height (B-Tree) -> integer() >= 0 ###

Types:

    B-Tree = b_tree()
    
Returns the height of b-tree B-Tree as an integer. Assumes that B-Tree is non-empty.

### insert (Key, Value, B-Tree1) -> B-Tree2 ###

Types:

    Key = any()
    Value = any()
    B-Tree1 = B-Tree2 = b_tree()
    
Inserts key Key with value Value into b-trere B-Tree1 and returns the new b-tree B-Tree2. Assumes that Key is **not** present in B-Tree1, crashes otherwise.

### is_defined (Key, B-Tree) -> boolean() ###

Types:

    Key = any()
    B-Tree = b_tree()

Returns `true` if key Key is present in b-tree B-Tree, otherwise `false`.

### is_empty (B-Tree) -> boolean() ###

Types:

    B-Tree = b_tree()
    
Returns `true` if b-tree B-Tree is an empty tree, otherwise `false`.

### iterator (B-Tree) -> Iterator ###

Types:

    B-Tree = b_tree()
    Iterator = iterator()
    
Returns iterator Iterator that can be used for traversing the entries of b-tree B-Tree; see `next/1`. The implementation of this iterator is very efficient; traversing the whole b-tree using `next/1` is only slightly slower than getting the list of all elements using `to_list/1` and traversing that. The main advantage of the iterator approach is that it does not require the complete list of all elements to be built in memory at one time.

### iterator_from (Key, B-Tree) -> Iterator ###

Types:

    Key = any(9
    B-Tree = b_tree()
    Iterator = iterator()
    
Returns iterator Iterator that can be used for traversing the entries of b-tree B-Tree; see `next/1`. The difference as compared to the iterator returned by iterator/1 is that the first key greater than or equal to Key is returned.

### keys (B-Tree) -> [Key] ###

Types:

    B-Tree = b_tree()
    Key = any()
    
Returns the keys in b-tree B-Tree as an ordered list.

### largest (B-Tree) -> {Key, Value} ###

Types:

    B-Tree = b_tree()
    Key = any()
    Value = any()
    
Returns a tuple {Key, Value}, where Key is the largest key in b-tree B-Tree, and Value is the value associated with this key. Assumes that B-Tree is not empty.

### lookup (Key, B-Tree) -> none | {value, Value} ###

Types:

    Key = any()
    B-Tree = b_tree()
    Value = any()
    
Looks up key Key in b-tree B-Tree. Returns {value, Value}, or none if Key is not present.

### map (Function, B-Tree1) -> B-Tree2 ###

Types:

    Function = fun((Key, Value1) -> Value2)
    B-Tree1 = B-Tree2 = b_tree()
    Key = any()
    Value1 = Value2 = any()
    
Maps function Function(Key, Value1) -> Value2 to all key value pairs of b-tree B-Tree1. Returns the new b-tree B-Tree2 with the same set of keys as B-Tree1 and the new set of values.

### next (Iterator1) -> none | {Key, Value, Iterator2} ###

Types:

    Iterator1 = Iterator2 = iterator()
    Key = any()
    Value = any()
    

Returns the tuple {Key, Value, Iterator2}, where Key is the smallest key referred to by iterator Iterator1, and iterator Iterator2 is the new iterator to be used for traversing the remaining nodes, or the atom none if no nodes remain.

### number_key_values (B-Tree) -> integer() >= 0 ###

Types:

    B-Tree = b_tree()

Returns the number of key value pairs in b-tree B-tree as an integer. Returns 0 (zero) if B-Tree is empty.

### set_parameter (B-Tree1, Name, Value) -> B-Tree2 ###

Types:

    B-Tree1 = B-Tree2 = b_tree()
    Name : Value = sort  : Function = fun((Key1, Key2) -> equal | greater | less)
                 | state : {StateTarget, Function = fun(StateTarget, delete, Key) -> ok,
                                         Function = fun(StateTarget, insert, Subtrees) -> Key,
                                         Function = fun(StateTarget, lookup, Key) -> Subtrees}
    
Sets in the empty b-tree B-Tree1 the parameter Name to value Value and returns the new b-tree B-Tree2.

### size (B-Tree) -> integer() >= 0 ###

Types:

    B-Tree = b_tree()

Returns the number of nodes in b-tree B-Tree.

### smallest (B-Tree) -> {Key, Value} ###

Types:

    B-Tree = b_tree()
    Key = any()
    Value = any()
    
Returns tuple {Key, Value}, where Key is the smallest key in b-tree B-Tree, and Value is the value associated with this key. Assumes that B-Tree is not empty.

### sort_ascending (Key1, Key2) -> equal | greater | less ###

Types:

    Key1 = Key2  = any()
    equal = greater = less = atom()
    
Returns the atom **greater** if Key1 > Key2, the atom **less** if Key1 < Key2 and the atom **equal** else-wise.

### sort_descending (Key1, Key2) -> equal | greater | less ###

Types:

    Key1 = Key2  = any()
    equal = greater = less = atom()
    
Returns the atom **less** if Key1 > Key2, the atom **greater** if Key1 < Key2 and the atom **equal** else-wise.

### take_largest (B-Tree1) -> {Key, Value, B-Tree2} ###

Types:

    B-Tree1 = B-Tree2 = b_tree()
    Key = any()
    Value = any()
    
Returns tuple {Key, Value, B-Tree2}, where Key is the largest key in b-tree B-Tree1, Value is the value associated with this key, and b-tree B-Tree2 is this b-tree with the corresponding key value pair deleted. Assumes that B-Tree1 is not empty.

### take_smallest (B-Tree1) -> {Key, Value, B-Tree2} ###

Types:

    B-Tree1 = B-Tree2 = b_tree()
    Key = any()
    Value = any()
    
Returns tuple {Key, Value, B-Tree2}, where Key is the smallest key in b-tree B-Tree1, Value is the value associated with this key, and b-tree B-Tree2 is this b-tree with the corresponding key value pair deleted. Assumes that B-Tree1 is not empty.

### to_list (B-Tree) -> [{Key, Value}] ###

Types:

    B-Tree = b_tree()
    Key = any()
    Value = any()

Converts b-tree B-Tree into an ordered list of key value tuples.

### update (Key, Value, B-Tree1) -> B-Tree2 ###

Types:

    Key = any()
    Value = any()
    B-Tree1 = B-Tree2 = b_tree()


Updates key Key to value Value in b-tree B-Tree1 and returns the new b-tree B-Tree2. Assumes that Key is present in B-Tree1.

### values (B-Tree) -> [Value] ###

Types:

    B-Tree = b_tree()
    Value = any()

Returns the values in b-tree B-Tree as an ordered list, sorted by their corresponding keys. Duplicates are not removed.

## See Also ##

Additional documentation for **b_trees** is available here: [Wiki](https://github.com/walter-weinmann/b_trees/wiki).
