/**
This module implements a red-black tree container.

This module is a submodule of $(MREF std, container).

Source: $(PHOBOSSRC std/container/_rbtree.d)

Copyright: Red-black tree code copyright (C) 2008- by Steven Schveighoffer. Other code
copyright 2010- Andrei Alexandrescu. All rights reserved by the respective holders.

License: Distributed under the Boost Software License, Version 1.0.
(See accompanying file LICENSE_1_0.txt or copy at $(HTTP
boost.org/LICENSE_1_0.txt)).

Authors: Steven Schveighoffer, $(HTTP erdani.com, Andrei Alexandrescu)
*/
module std.container.rbtree;

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.container.rbtree;

    auto rbt = redBlackTree(3, 1, 4, 2, 5);
    assert(rbt.front == 1);
    assert(equal(rbt[], [1, 2, 3, 4, 5]));

    rbt.removeKey(1, 4);
    assert(equal(rbt[], [2, 3, 5]));

    rbt.removeFront();
    assert(equal(rbt[], [3, 5]));

    rbt.insert([1, 2, 4]);
    assert(equal(rbt[], [1, 2, 3, 4, 5]));

    // Query bounds in O(log(n))
    assert(rbt.lowerBound(3).equal([1, 2]));
    assert(rbt.equalRange(3).equal([3]));
    assert(rbt.upperBound(3).equal([4, 5]));

    // A Red Black tree with the highest element at front:
    import std.range : iota;
    auto maxTree = redBlackTree!"a > b"(iota(5));
    assert(equal(maxTree[], [4, 3, 2, 1, 0]));

    // adding duplicates will not add them, but return 0
    auto rbt2 = redBlackTree(1, 3);
    assert(rbt2.insert(1) == 0);
    assert(equal(rbt2[], [1, 3]));
    assert(rbt2.insert(2) == 1);

    // however you can allow duplicates
    auto ubt = redBlackTree!true([0, 1, 0, 1]);
    assert(equal(ubt[], [0, 0, 1, 1]));
}

import std.format;
import std.functional : binaryFun;

public import std.container.util;

version (unittest) debug = RBDoChecks;

//debug = RBDoChecks;

/*
 * Implementation for a Red Black node for use in a Red Black Tree (see below)
 *
 * this implementation assumes we have a marker Node that is the parent of the
 * root Node.  This marker Node is not a valid Node, but marks the end of the
 * collection.  The root is the left child of the marker Node, so it is always
 * last in the collection.  The marker Node is passed in to the setColor
 * function, and the Node which has this Node as its parent is assumed to be
 * the root Node.
 *
 * A Red Black tree should have O(lg(n)) insertion, removal, and search time.
 */
struct RBNode(V)
{
    /*
     * Convenience alias
     */
    alias Node = RBNode*;

    private Node _left;
    private Node _right;
    private Node _parent;

    /**
     * The value held by this node
     */
    V value;

    /**
     * Enumeration determining what color the node is.  Null nodes are assumed
     * to be black.
     */
    enum Color : byte
    {
        Red,
        Black
    }

    /**
     * The color of the node.
     */
    Color color;

    /**
     * Get the left child
     */
    @property inout(RBNode)* left() inout
    {
        return _left;
    }

    /**
     * Get the right child
     */
    @property inout(RBNode)* right() inout
    {
        return _right;
    }

    /**
     * Get the parent
     */
    @property inout(RBNode)* parent() inout
    {
        return _parent;
    }

    /**
     * Set the left child.  Also updates the new child's parent node.  This
     * does not update the previous child.
     *
     * Returns newNode
     */
    @property Node left(Node newNode)
    {
        _left = newNode;
        if (newNode !is null)
            newNode._parent = &this;
        return newNode;
    }

    /**
     * Set the right child.  Also updates the new child's parent node.  This
     * does not update the previous child.
     *
     * Returns newNode
     */
    @property Node right(Node newNode)
    {
        _right = newNode;
        if (newNode !is null)
            newNode._parent = &this;
        return newNode;
    }

    // assume _left is not null
    //
    // performs rotate-right operation, where this is T, _right is R, _left is
    // L, _parent is P:
    //
    //      P         P
    //      |   ->    |
    //      T         L
    //     / \       / \
    //    L   R     a   T
    //   / \           / \
    //  a   b         b   R
    //
    /**
     * Rotate right.  This performs the following operations:
     *  - The left child becomes the parent of this node.
     *  - This node becomes the new parent's right child.
     *  - The old right child of the new parent becomes the left child of this
     *    node.
     */
    Node rotateR()
    in
    {
        assert(_left !is null);
    }
    body
    {
        // sets _left._parent also
        if (isLeftNode)
            parent.left = _left;
        else
            parent.right = _left;
        Node tmp = _left._right;

        // sets _parent also
        _left.right = &this;

        // sets tmp._parent also
        left = tmp;

        return &this;
    }

    // assumes _right is non null
    //
    // performs rotate-left operation, where this is T, _right is R, _left is
    // L, _parent is P:
    //
    //      P           P
    //      |    ->     |
    //      T           R
    //     / \         / \
    //    L   R       T   b
    //       / \     / \
    //      a   b   L   a
    //
    /**
     * Rotate left.  This performs the following operations:
     *  - The right child becomes the parent of this node.
     *  - This node becomes the new parent's left child.
     *  - The old left child of the new parent becomes the right child of this
     *    node.
     */
    Node rotateL()
    in
    {
        assert(_right !is null);
    }
    body
    {
        // sets _right._parent also
        if (isLeftNode)
            parent.left = _right;
        else
            parent.right = _right;
        Node tmp = _right._left;

        // sets _parent also
        _right.left = &this;

        // sets tmp._parent also
        right = tmp;
        return &this;
    }


    /**
     * Returns true if this node is a left child.
     *
     * Note that this should always return a value because the root has a
     * parent which is the marker node.
     */
    @property bool isLeftNode() const
    in
    {
        assert(_parent !is null);
    }
    body
    {
        return _parent._left is &this;
    }

    /**
     * Set the color of the node after it is inserted.  This performs an
     * update to the whole tree, possibly rotating nodes to keep the Red-Black
     * properties correct.  This is an O(lg(n)) operation, where n is the
     * number of nodes in the tree.
     *
     * end is the marker node, which is the parent of the topmost valid node.
     */
    void setColor(Node end)
    {
        // test against the marker node
        if (_parent !is end)
        {
            if (_parent.color == Color.Red)
            {
                Node cur = &this;
                while (true)
                {
                    // because root is always black, _parent._parent always exists
                    if (cur._parent.isLeftNode)
                    {
                        // parent is left node, y is 'uncle', could be null
                        Node y = cur._parent._parent._right;
                        if (y !is null && y.color == Color.Red)
                        {
                            cur._parent.color = Color.Black;
                            y.color = Color.Black;
                            cur = cur._parent._parent;
                            if (cur._parent is end)
                            {
                                // root node
                                cur.color = Color.Black;
                                break;
                            }
                            else
                            {
                                // not root node
                                cur.color = Color.Red;
                                if (cur._parent.color == Color.Black)
                                    // satisfied, exit the loop
                                    break;
                            }
                        }
                        else
                        {
                            if (!cur.isLeftNode)
                                cur = cur._parent.rotateL();
                            cur._parent.color = Color.Black;
                            cur = cur._parent._parent.rotateR();
                            cur.color = Color.Red;
                            // tree should be satisfied now
                            break;
                        }
                    }
                    else
                    {
                        // parent is right node, y is 'uncle'
                        Node y = cur._parent._parent._left;
                        if (y !is null && y.color == Color.Red)
                        {
                            cur._parent.color = Color.Black;
                            y.color = Color.Black;
                            cur = cur._parent._parent;
                            if (cur._parent is end)
                            {
                                // root node
                                cur.color = Color.Black;
                                break;
                            }
                            else
                            {
                                // not root node
                                cur.color = Color.Red;
                                if (cur._parent.color == Color.Black)
                                    // satisfied, exit the loop
                                    break;
                            }
                        }
                        else
                        {
                            if (cur.isLeftNode)
                                cur = cur._parent.rotateR();
                            cur._parent.color = Color.Black;
                            cur = cur._parent._parent.rotateL();
                            cur.color = Color.Red;
                            // tree should be satisfied now
                            break;
                        }
                    }
                }

            }
        }
        else
        {
            //
            // this is the root node, color it black
            //
            color = Color.Black;
        }
    }

    /**
     * Remove this node from the tree.  The 'end' node is used as the marker
     * which is root's parent.  Note that this cannot be null!
     *
     * Returns the next highest valued node in the tree after this one, or end
     * if this was the highest-valued node.
     */
    Node remove(Node end)
    {
        //
        // remove this node from the tree, fixing the color if necessary.
        //
        Node x;
        Node ret = next;

        // if this node has 2 children
        if (_left !is null && _right !is null)
        {
            //
            // normally, we can just swap this node's and y's value, but
            // because an iterator could be pointing to y and we don't want to
            // disturb it, we swap this node and y's structure instead.  This
            // can also be a benefit if the value of the tree is a large
            // struct, which takes a long time to copy.
            //
            Node yp, yl, yr;
            Node y = ret; // y = next
            yp = y._parent;
            yl = y._left;
            yr = y._right;
            auto yc = y.color;
            auto isyleft = y.isLeftNode;

            //
            // replace y's structure with structure of this node.
            //
            if (isLeftNode)
                _parent.left = y;
            else
                _parent.right = y;
            //
            // need special case so y doesn't point back to itself
            //
            y.left = _left;
            if (_right is y)
                y.right = &this;
            else
                y.right = _right;
            y.color = color;

            //
            // replace this node's structure with structure of y.
            //
            left = yl;
            right = yr;
            if (_parent !is y)
            {
                if (isyleft)
                    yp.left = &this;
                else
                    yp.right = &this;
            }
            color = yc;
        }

        // if this has less than 2 children, remove it
        if (_left !is null)
            x = _left;
        else
            x = _right;

        bool deferedUnlink = false;
        if (x is null)
        {
            // pretend this is a null node, defer unlinking the node
            x = &this;
            deferedUnlink = true;
        }
        else if (isLeftNode)
            _parent.left = x;
        else
            _parent.right = x;

        // if the color of this is black, then it needs to be fixed
        if (color == color.Black)
        {
            // need to recolor the tree.
            while (x._parent !is end && x.color == Node.Color.Black)
            {
                if (x.isLeftNode)
                {
                    // left node
                    Node w = x._parent._right;
                    if (w.color == Node.Color.Red)
                    {
                        w.color = Node.Color.Black;
                        x._parent.color = Node.Color.Red;
                        x._parent.rotateL();
                        w = x._parent._right;
                    }
                    Node wl = w.left;
                    Node wr = w.right;
                    if ((wl is null || wl.color == Node.Color.Black) &&
                            (wr is null || wr.color == Node.Color.Black))
                    {
                        w.color = Node.Color.Red;
                        x = x._parent;
                    }
                    else
                    {
                        if (wr is null || wr.color == Node.Color.Black)
                        {
                            // wl cannot be null here
                            wl.color = Node.Color.Black;
                            w.color = Node.Color.Red;
                            w.rotateR();
                            w = x._parent._right;
                        }

                        w.color = x._parent.color;
                        x._parent.color = Node.Color.Black;
                        w._right.color = Node.Color.Black;
                        x._parent.rotateL();
                        x = end.left; // x = root
                    }
                }
                else
                {
                    // right node
                    Node w = x._parent._left;
                    if (w.color == Node.Color.Red)
                    {
                        w.color = Node.Color.Black;
                        x._parent.color = Node.Color.Red;
                        x._parent.rotateR();
                        w = x._parent._left;
                    }
                    Node wl = w.left;
                    Node wr = w.right;
                    if ((wl is null || wl.color == Node.Color.Black) &&
                            (wr is null || wr.color == Node.Color.Black))
                    {
                        w.color = Node.Color.Red;
                        x = x._parent;
                    }
                    else
                    {
                        if (wl is null || wl.color == Node.Color.Black)
                        {
                            // wr cannot be null here
                            wr.color = Node.Color.Black;
                            w.color = Node.Color.Red;
                            w.rotateL();
                            w = x._parent._left;
                        }

                        w.color = x._parent.color;
                        x._parent.color = Node.Color.Black;
                        w._left.color = Node.Color.Black;
                        x._parent.rotateR();
                        x = end.left; // x = root
                    }
                }
            }
            x.color = Node.Color.Black;
        }

        if (deferedUnlink)
        {
            //
            // unlink this node from the tree
            //
            if (isLeftNode)
                _parent.left = null;
            else
                _parent.right = null;
        }

        // clean references to help GC - Bugzilla 12915
        _left = _right = _parent = null;

        return ret;
    }

    /**
     * Return the leftmost descendant of this node.
     */
    @property inout(RBNode)* leftmost() inout
    {
        inout(RBNode)* result = &this;
        while (result._left !is null)
            result = result._left;
        return result;
    }

    /**
     * Return the rightmost descendant of this node
     */
    @property inout(RBNode)* rightmost() inout
    {
        inout(RBNode)* result = &this;
        while (result._right !is null)
            result = result._right;
        return result;
    }

    /**
     * Returns the next valued node in the tree.
     *
     * You should never call this on the marker node, as it is assumed that
     * there is a valid next node.
     */
    @property inout(RBNode)* next() inout
    {
        inout(RBNode)* n = &this;
        if (n.right is null)
        {
            while (!n.isLeftNode)
                n = n._parent;
            return n._parent;
        }
        else
            return n.right.leftmost;
    }

    /**
     * Returns the previous valued node in the tree.
     *
     * You should never call this on the leftmost node of the tree as it is
     * assumed that there is a valid previous node.
     */
    @property inout(RBNode)* prev() inout
    {
        inout(RBNode)* n = &this;
        if (n.left is null)
        {
            while (n.isLeftNode)
                n = n._parent;
            return n._parent;
        }
        else
            return n.left.rightmost;
    }

    Node dup(scope Node delegate(V v) alloc)
    {
        //
        // duplicate this and all child nodes
        //
        // The recursion should be lg(n), so we shouldn't have to worry about
        // stack size.
        //
        Node copy = alloc(value);
        copy.color = color;
        if (_left !is null)
            copy.left = _left.dup(alloc);
        if (_right !is null)
            copy.right = _right.dup(alloc);
        return copy;
    }

    Node dup()
    {
        Node copy = new RBNode!V(null, null, null, value);
        copy.color = color;
        if (_left !is null)
            copy.left = _left.dup();
        if (_right !is null)
            copy.right = _right.dup();
        return copy;
    }
}

//constness checks
@safe pure unittest
{
    const RBNode!int n;
    static assert(is(typeof(n.leftmost)));
    static assert(is(typeof(n.rightmost)));
    static assert(is(typeof(n.next)));
    static assert(is(typeof(n.prev)));
}

private struct RBRange(N)
{
    alias Node = N;
    alias Elem = typeof(Node.value);

    private Node _begin;
    private Node _end;

    private this(Node b, Node e)
    {
        _begin = b;
        _end = e;
    }

    /**
     * Returns $(D true) if the range is _empty
     */
    @property bool empty() const
    {
        return _begin is _end;
    }

    /**
     * Returns the first element in the range
     */
    @property Elem front()
    {
        return _begin.value;
    }

    /**
     * Returns the last element in the range
     */
    @property Elem back()
    {
        return _end.prev.value;
    }

    /**
     * pop the front element from the range
     *
     * Complexity: amortized $(BIGOH 1)
     */
    void popFront()
    {
        _begin = _begin.next;
    }

    /**
     * pop the back element from the range
     *
     * Complexity: amortized $(BIGOH 1)
     */
    void popBack()
    {
        _end = _end.prev;
    }

    /**
     * Trivial _save implementation, needed for $(D isForwardRange).
     */
    @property RBRange save()
    {
        return this;
    }
}

/**
 * Implementation of a $(LINK2 https://en.wikipedia.org/wiki/Red%E2%80%93black_tree,
 * red-black tree) container.
 *
 * All inserts, removes, searches, and any function in general has complexity
 * of $(BIGOH lg(n)).
 *
 * To use a different comparison than $(D "a < b"), pass a different operator string
 * that can be used by $(REF binaryFun, std,functional), or pass in a
 * function, delegate, functor, or any type where $(D less(a, b)) results in a $(D bool)
 * value.
 *
 * Note that less should produce a strict ordering.  That is, for two unequal
 * elements $(D a) and $(D b), $(D less(a, b) == !less(b, a)). $(D less(a, a)) should
 * always equal $(D false).
 *
 * If $(D allowDuplicates) is set to $(D true), then inserting the same element more than
 * once continues to add more elements.  If it is $(D false), duplicate elements are
 * ignored on insertion.  If duplicates are allowed, then new elements are
 * inserted after all existing duplicate elements.
 */
final class RedBlackTree(T, alias less = "a < b", bool allowDuplicates = false)
if (is(typeof(binaryFun!less(T.init, T.init))))
{
    import std.meta : allSatisfy;
    import std.range : Take;
    import std.range.primitives : isInputRange, walkLength;
    import std.traits : isIntegral, isDynamicArray, isImplicitlyConvertible;

    alias _less = binaryFun!less;

    version (unittest)
    {
        static if (is(typeof(less) == string))
        {
            private enum doUnittest = isIntegral!T && (less == "a < b" || less == "a > b");
        }
        else
            enum doUnittest = false;

        // note, this must be final so it does not affect the vtable layout
        bool arrayEqual(T[] arr)
        {
            if (walkLength(this[]) == arr.length)
            {
                foreach (v; arr)
                {
                    if (!(v in this))
                        return false;
                }
                return true;
            }
            return false;
        }
    }
    else
    {
        private enum doUnittest = false;
    }

    /**
      * Element type for the tree
      */
    alias Elem = T;

    // used for convenience
    private alias RBNode = .RBNode!Elem;
    private alias Node = RBNode.Node;

    private Node   _end;
    private Node   _begin;
    private size_t _length;

    private void _setup()
    {
        assert(!_end); //Make sure that _setup isn't run more than once.
        _begin = _end = allocate();
    }

    static private Node allocate()
    {
        return new RBNode;
    }

    static private Node allocate(Elem v)
    {
        return new RBNode(null, null, null, v);
    }

    /**
     * The range types for $(D RedBlackTree)
     */
    alias Range = RBRange!(RBNode*);
    alias ConstRange = RBRange!(const(RBNode)*); /// Ditto
    alias ImmutableRange = RBRange!(immutable(RBNode)*); /// Ditto

    static if (doUnittest) @safe pure unittest
    {
        import std.algorithm.comparison : equal;
        import std.range.primitives;
        auto ts = new RedBlackTree(1, 2, 3, 4, 5);
        assert(ts.length == 5);
        auto r = ts[];

        static if (less == "a < b")
            auto vals = [1, 2, 3, 4, 5];
        else
            auto vals = [5, 4, 3, 2, 1];
        assert(equal(r, vals));

        assert(r.front == vals.front);
        assert(r.back != r.front);
        auto oldfront = r.front;
        auto oldback = r.back;
        r.popFront();
        r.popBack();
        assert(r.front != r.back);
        assert(r.front != oldfront);
        assert(r.back != oldback);
        assert(ts.length == 5);
    }

    // find a node based on an element value
    private inout(RBNode)* _find(Elem e) inout
    {
        static if (allowDuplicates)
        {
            inout(RBNode)* cur = _end.left;
            inout(RBNode)* result = null;
            while (cur)
            {
                if (_less(cur.value, e))
                    cur = cur.right;
                else if (_less(e, cur.value))
                    cur = cur.left;
                else
                {
                    // want to find the left-most element
                    result = cur;
                    cur = cur.left;
                }
            }
            return result;
        }
        else
        {
            inout(RBNode)* cur = _end.left;
            while (cur)
            {
                if (_less(cur.value, e))
                    cur = cur.right;
                else if (_less(e, cur.value))
                    cur = cur.left;
                else
                    return cur;
            }
            return null;
        }
    }

    // add an element to the tree, returns the node added, or the existing node
    // if it has already been added and allowDuplicates is false
    private auto _add(Elem n)
    {
        Node result;
        static if (!allowDuplicates)
            bool added = true;

        if (!_end.left)
        {
            _end.left = _begin = result = allocate(n);
        }
        else
        {
            Node newParent = _end.left;
            Node nxt;
            while (true)
            {
                if (_less(n, newParent.value))
                {
                    nxt = newParent.left;
                    if (nxt is null)
                    {
                        //
                        // add to right of new parent
                        //
                        newParent.left = result = allocate(n);
                        break;
                    }
                }
                else
                {
                    static if (!allowDuplicates)
                    {
                        if (!_less(newParent.value, n))
                        {
                            result = newParent;
                            added = false;
                            break;
                        }
                    }
                    nxt = newParent.right;
                    if (nxt is null)
                    {
                        //
                        // add to right of new parent
                        //
                        newParent.right = result = allocate(n);
                        break;
                    }
                }
                newParent = nxt;
            }
            if (_begin.left)
                _begin = _begin.left;
        }

        static if (allowDuplicates)
        {
            result.setColor(_end);
            debug(RBDoChecks)
                check();
            ++_length;
            return result;
        }
        else
        {
            import std.typecons : Tuple;

            if (added)
            {
                ++_length;
                result.setColor(_end);
            }
            debug(RBDoChecks)
                check();
            return Tuple!(bool, "added", Node, "n")(added, result);
        }
    }


    /**
     * Check if any elements exist in the container.  Returns $(D false) if at least
     * one element exists.
     */
    @property bool empty()
    {
        return _end.left is null;
    }

    /++
        Returns the number of elements in the container.

        Complexity: $(BIGOH 1).
    +/
    @property size_t length() const
    {
        return _length;
    }

    /**
     * Duplicate this container.  The resulting container contains a shallow
     * copy of the elements.
     *
     * Complexity: $(BIGOH n)
     */
    @property RedBlackTree dup()
    {
        return new RedBlackTree(_end.dup(), _length);
    }

    static if (doUnittest) @safe pure unittest
    {
        import std.algorithm.comparison : equal;
        auto ts = new RedBlackTree(1, 2, 3, 4, 5);
        assert(ts.length == 5);
        auto ts2 = ts.dup;
        assert(ts2.length == 5);
        assert(equal(ts[], ts2[]));
        ts2.insert(cast(Elem) 6);
        assert(!equal(ts[], ts2[]));
        assert(ts.length == 5 && ts2.length == 6);
    }

    /**
     * Fetch a range that spans all the elements in the container.
     *
     * Complexity: $(BIGOH 1)
     */
    Range opSlice()
    {
        return Range(_begin, _end);
    }

    /// Ditto
    ConstRange opSlice() const
    {
        return ConstRange(_begin, _end);
    }

    /// Ditto
    ImmutableRange opSlice() immutable
    {
        return ImmutableRange(_begin, _end);
    }

    /**
     * The front element in the container
     *
     * Complexity: $(BIGOH 1)
     */
    Elem front()
    {
        return _begin.value;
    }

    /**
     * The last element in the container
     *
     * Complexity: $(BIGOH log(n))
     */
    Elem back()
    {
        return _end.prev.value;
    }

    /++
        $(D in) operator. Check to see if the given element exists in the
        container.

       Complexity: $(BIGOH log(n))
     +/
    bool opBinaryRight(string op)(Elem e) const if (op == "in")
    {
        return _find(e) !is null;
    }

    static if (doUnittest) @safe pure unittest
    {
        auto ts = new RedBlackTree(1, 2, 3, 4, 5);
        assert(cast(Elem) 3 in ts);
        assert(cast(Elem) 6 !in ts);
    }

    /**
     * Compares two trees for equality.
     *
     * Complexity: $(BIGOH n)
     */
    override bool opEquals(Object rhs)
    {
        import std.algorithm.comparison : equal;

        RedBlackTree that = cast(RedBlackTree) rhs;
        if (that is null) return false;

        // If there aren't the same number of nodes, we can't be equal.
        if (this._length != that._length) return false;

        auto thisRange = this[];
        auto thatRange = that[];
        return equal!(function(Elem a, Elem b) => !_less(a,b) && !_less(b,a))
                     (thisRange, thatRange);
    }

    static if (doUnittest) @system unittest
    {
        auto t1 = new RedBlackTree(1,2,3,4);
        auto t2 = new RedBlackTree(1,2,3,4);
        auto t3 = new RedBlackTree(1,2,3,5);
        auto t4 = new RedBlackTree(1,2,3,4,5);
        auto o = new Object();

        assert(t1 == t1);
        assert(t1 == t2);
        assert(t1 != t3);
        assert(t1 != t4);
        assert(t1 != o);  // pathological case, must not crash
    }

    /**
     * Removes all elements from the container.
     *
     * Complexity: $(BIGOH 1)
     */
    void clear()
    {
        _end.left = null;
        _begin = _end;
        _length = 0;
    }

    static if (doUnittest) @safe pure unittest
    {
        auto ts = new RedBlackTree(1,2,3,4,5);
        assert(ts.length == 5);
        ts.clear();
        assert(ts.empty && ts.length == 0);
    }

    /**
     * Insert a single element in the container.  Note that this does not
     * invalidate any ranges currently iterating the container.
     *
     * Returns: The number of elements inserted.
     *
     * Complexity: $(BIGOH log(n))
     */
    size_t stableInsert(Stuff)(Stuff stuff) if (isImplicitlyConvertible!(Stuff, Elem))
    {
        static if (allowDuplicates)
        {
            _add(stuff);
            return 1;
        }
        else
        {
            return(_add(stuff).added ? 1 : 0);
        }
    }

    /**
     * Insert a range of elements in the container.  Note that this does not
     * invalidate any ranges currently iterating the container.
     *
     * Returns: The number of elements inserted.
     *
     * Complexity: $(BIGOH m * log(n))
     */
    size_t stableInsert(Stuff)(Stuff stuff) if (isInputRange!Stuff && isImplicitlyConvertible!(ElementType!Stuff, Elem))
    {
        size_t result = 0;
        static if (allowDuplicates)
        {
            foreach (e; stuff)
            {
                ++result;
                _add(e);
            }
        }
        else
        {
            foreach (e; stuff)
            {
                if (_add(e).added)
                    ++result;
            }
        }
        return result;
    }

    /// ditto
    alias insert = stableInsert;

    static if (doUnittest) @safe pure unittest
    {
        auto ts = new RedBlackTree(2,1,3,4,5,2,5);
        static if (allowDuplicates)
        {
            assert(ts.length == 7);
            assert(ts.stableInsert(cast(Elem[])[7, 8, 6, 9, 10, 8]) == 6);
            assert(ts.length == 13);
            assert(ts.stableInsert(cast(Elem) 11) == 1 && ts.length == 14);
            assert(ts.stableInsert(cast(Elem) 7) == 1 && ts.length == 15);

            static if (less == "a < b")
                assert(ts.arrayEqual([1,2,2,3,4,5,5,6,7,7,8,8,9,10,11]));
            else
                assert(ts.arrayEqual([11,10,9,8,8,7,7,6,5,5,4,3,2,2,1]));
        }
        else
        {
            assert(ts.length == 5);
            Elem[] elems = [7, 8, 6, 9, 10, 8];
            assert(ts.stableInsert(elems) == 5);
            assert(ts.length == 10);
            assert(ts.stableInsert(cast(Elem) 11) == 1 && ts.length == 11);
            assert(ts.stableInsert(cast(Elem) 7) == 0 && ts.length == 11);

            static if (less == "a < b")
                assert(ts.arrayEqual([1,2,3,4,5,6,7,8,9,10,11]));
            else
                assert(ts.arrayEqual([11,10,9,8,7,6,5,4,3,2,1]));
        }
    }

    /**
     * Remove an element from the container and return its value.
     *
     * Complexity: $(BIGOH log(n))
     */
    Elem removeAny()
    {
        scope(success)
            --_length;
        auto n = _begin;
        auto result = n.value;
        _begin = n.remove(_end);
        debug(RBDoChecks)
            check();
        return result;
    }

    static if (doUnittest) @safe pure unittest
    {
        auto ts = new RedBlackTree(1,2,3,4,5);
        assert(ts.length == 5);
        auto x = ts.removeAny();
        assert(ts.length == 4);
        Elem[] arr;
        foreach (Elem i; 1 .. 6)
            if (i != x) arr ~= i;
        assert(ts.arrayEqual(arr));
    }

    /**
     * Remove the front element from the container.
     *
     * Complexity: $(BIGOH log(n))
     */
    void removeFront()
    {
        scope(success)
            --_length;
        _begin = _begin.remove(_end);
        debug(RBDoChecks)
            check();
    }

    /**
     * Remove the back element from the container.
     *
     * Complexity: $(BIGOH log(n))
     */
    void removeBack()
    {
        scope(success)
            --_length;
        auto lastnode = _end.prev;
        if (lastnode is _begin)
            _begin = _begin.remove(_end);
        else
            lastnode.remove(_end);
        debug(RBDoChecks)
            check();
    }

    static if (doUnittest) @safe pure unittest
    {
        auto ts = new RedBlackTree(1,2,3,4,5);
        assert(ts.length == 5);
        ts.removeBack();
        assert(ts.length == 4);

        static if (less == "a < b")
            assert(ts.arrayEqual([1,2,3,4]));
        else
            assert(ts.arrayEqual([2,3,4,5]));

        ts.removeFront();
        assert(ts.arrayEqual([2,3,4]) && ts.length == 3);
    }

    /++
        Removes the given range from the container.

        Returns: A range containing all of the elements that were after the
                 given range.

        Complexity: $(BIGOH m * log(n)) (where m is the number of elements in
                    the range)
     +/
    Range remove(Range r)
    {
        auto b = r._begin;
        auto e = r._end;
        if (_begin is b)
            _begin = e;
        while (b !is e)
        {
            b = b.remove(_end);
            --_length;
        }
        debug(RBDoChecks)
            check();
        return Range(e, _end);
    }

    static if (doUnittest) @safe pure unittest
    {
        import std.algorithm.comparison : equal;
        auto ts = new RedBlackTree(1,2,3,4,5);
        assert(ts.length == 5);
        auto r = ts[];
        r.popFront();
        r.popBack();
        assert(ts.length == 5);
        auto r2 = ts.remove(r);
        assert(ts.length == 2);
        assert(ts.arrayEqual([1,5]));

        static if (less == "a < b")
            assert(equal(r2, [5]));
        else
            assert(equal(r2, [1]));
    }

    /++
        Removes the given $(D Take!Range) from the container

        Returns: A range containing all of the elements that were after the
                 given range.

        Complexity: $(BIGOH m * log(n)) (where m is the number of elements in
                    the range)
     +/
    Range remove(Take!Range r)
    {
        immutable isBegin = (r.source._begin is _begin);
        auto b = r.source._begin;

        while (!r.empty)
        {
            r.popFront();
            b = b.remove(_end);
            --_length;
        }

        if (isBegin)
            _begin = b;

        return Range(b, _end);
    }

    static if (doUnittest) @safe pure unittest
    {
        import std.algorithm.comparison : equal;
        import std.range : take;
        auto ts = new RedBlackTree(1,2,3,4,5);
        auto r = ts[];
        r.popFront();
        assert(ts.length == 5);
        auto r2 = ts.remove(take(r, 0));

        static if (less == "a < b")
        {
            assert(equal(r2, [2,3,4,5]));
            auto r3 = ts.remove(take(r, 2));
            assert(ts.arrayEqual([1,4,5]) && ts.length == 3);
            assert(equal(r3, [4,5]));
        }
        else
        {
            assert(equal(r2, [4,3,2,1]));
            auto r3 = ts.remove(take(r, 2));
            assert(ts.arrayEqual([5,2,1]) && ts.length == 3);
            assert(equal(r3, [2,1]));
        }
    }

    /++
       Removes elements from the container that are equal to the given values
       according to the less comparator. One element is removed for each value
       given which is in the container. If $(D allowDuplicates) is true,
       duplicates are removed only if duplicate values are given.

       Returns: The number of elements removed.

       Complexity: $(BIGOH m log(n)) (where m is the number of elements to remove)

       Example:
--------------------
auto rbt = redBlackTree!true(0, 1, 1, 1, 4, 5, 7);
rbt.removeKey(1, 4, 7);
assert(equal(rbt[], [0, 1, 1, 5]));
rbt.removeKey(1, 1, 0);
assert(equal(rbt[], [5]));
--------------------
      +/
    size_t removeKey(U...)(U elems)
        if (allSatisfy!(isImplicitlyConvertibleToElem, U))
    {
        Elem[U.length] toRemove = [elems];
        return removeKey(toRemove[]);
    }

    /++ Ditto +/
    size_t removeKey(U)(U[] elems)
        if (isImplicitlyConvertible!(U, Elem))
    {
        immutable lenBefore = length;

        foreach (e; elems)
        {
            auto beg = _firstGreaterEqual(e);
            if (beg is _end || _less(e, beg.value))
                // no values are equal
                continue;
            immutable isBegin = (beg is _begin);
            beg = beg.remove(_end);
            if (isBegin)
                _begin = beg;
            --_length;
        }

        return lenBefore - length;
    }

    /++ Ditto +/
    size_t removeKey(Stuff)(Stuff stuff)
        if (isInputRange!Stuff &&
           isImplicitlyConvertible!(ElementType!Stuff, Elem) &&
           !isDynamicArray!Stuff)
    {
        import std.array : array;
        //We use array in case stuff is a Range from this RedBlackTree - either
        //directly or indirectly.
        return removeKey(array(stuff));
    }

    //Helper for removeKey.
    private template isImplicitlyConvertibleToElem(U)
    {
        enum isImplicitlyConvertibleToElem = isImplicitlyConvertible!(U, Elem);
    }

    static if (doUnittest) @safe pure unittest
    {
        import std.algorithm.comparison : equal;
        import std.range : take;
        auto rbt = new RedBlackTree(5, 4, 3, 7, 2, 1, 7, 6, 2, 19, 45);

        //The cast(Elem) is because these tests are instantiated with a variety
        //of numeric types, and the literals are all int, which is not always
        //implicitly convertible to Elem (e.g. short).
        static if (allowDuplicates)
        {
            assert(rbt.length == 11);
            assert(rbt.removeKey(cast(Elem) 4) == 1 && rbt.length == 10);
            assert(rbt.arrayEqual([1,2,2,3,5,6,7,7,19,45]) && rbt.length == 10);

            assert(rbt.removeKey(cast(Elem) 6, cast(Elem) 2, cast(Elem) 1) == 3);
            assert(rbt.arrayEqual([2,3,5,7,7,19,45]) && rbt.length == 7);

            assert(rbt.removeKey(cast(Elem)(42)) == 0 && rbt.length == 7);
            assert(rbt.removeKey(take(rbt[], 3)) == 3 && rbt.length == 4);

            static if (less == "a < b")
                assert(equal(rbt[], [7,7,19,45]));
            else
                assert(equal(rbt[], [7,5,3,2]));
        }
        else
        {
            assert(rbt.length == 9);
            assert(rbt.removeKey(cast(Elem) 4) == 1 && rbt.length == 8);
            assert(rbt.arrayEqual([1,2,3,5,6,7,19,45]));

            assert(rbt.removeKey(cast(Elem) 6, cast(Elem) 2, cast(Elem) 1) == 3);
            assert(rbt.arrayEqual([3,5,7,19,45]) && rbt.length == 5);

            assert(rbt.removeKey(cast(Elem)(42)) == 0 && rbt.length == 5);
            assert(rbt.removeKey(take(rbt[], 3)) == 3 && rbt.length == 2);

            static if (less == "a < b")
                assert(equal(rbt[], [19,45]));
            else
                assert(equal(rbt[], [5,3]));
        }
    }

    // find the first node where the value is > e
    private inout(RBNode)* _firstGreater(Elem e) inout
    {
        // can't use _find, because we cannot return null
        auto cur = _end.left;
        inout(RBNode)* result = _end;
        while (cur)
        {
            if (_less(e, cur.value))
            {
                result = cur;
                cur = cur.left;
            }
            else
                cur = cur.right;
        }
        return result;
    }

    // find the first node where the value is >= e
    private inout(RBNode)* _firstGreaterEqual(Elem e) inout
    {
        // can't use _find, because we cannot return null.
        auto cur = _end.left;
        inout(RBNode)* result = _end;
        while (cur)
        {
            if (_less(cur.value, e))
                cur = cur.right;
            else
            {
                result = cur;
                cur = cur.left;
            }

        }
        return result;
    }

    /**
     * Get a range from the container with all elements that are > e according
     * to the less comparator
     *
     * Complexity: $(BIGOH log(n))
     */
    Range upperBound(Elem e)
    {
        return Range(_firstGreater(e), _end);
    }

    /// Ditto
    ConstRange upperBound(Elem e) const
    {
        return ConstRange(_firstGreater(e), _end);
    }

    /// Ditto
    ImmutableRange upperBound(Elem e) immutable
    {
        return ImmutableRange(_firstGreater(e), _end);
    }

    /**
     * Get a range from the container with all elements that are < e according
     * to the less comparator
     *
     * Complexity: $(BIGOH log(n))
     */
    Range lowerBound(Elem e)
    {
        return Range(_begin, _firstGreaterEqual(e));
    }

    /// Ditto
    ConstRange lowerBound(Elem e) const
    {
        return ConstRange(_begin, _firstGreaterEqual(e));
    }

    /// Ditto
    ImmutableRange lowerBound(Elem e) immutable
    {
        return ImmutableRange(_begin, _firstGreaterEqual(e));
    }

    /**
     * Get a range from the container with all elements that are == e according
     * to the less comparator
     *
     * Complexity: $(BIGOH log(n))
     */
    auto equalRange(this This)(Elem e)
    {
        auto beg = _firstGreaterEqual(e);
        alias RangeType = RBRange!(typeof(beg));
        if (beg is _end || _less(e, beg.value))
            // no values are equal
            return RangeType(beg, beg);
        static if (allowDuplicates)
        {
            return RangeType(beg, _firstGreater(e));
        }
        else
        {
            // no sense in doing a full search, no duplicates are allowed,
            // so we just get the next node.
            return RangeType(beg, beg.next);
        }
    }

    static if (doUnittest) @safe pure unittest
    {
        import std.algorithm.comparison : equal;
        auto ts = new RedBlackTree(1, 2, 3, 4, 5);
        auto rl = ts.lowerBound(3);
        auto ru = ts.upperBound(3);
        auto re = ts.equalRange(3);

        static if (less == "a < b")
        {
            assert(equal(rl, [1,2]));
            assert(equal(ru, [4,5]));
        }
        else
        {
            assert(equal(rl, [5,4]));
            assert(equal(ru, [2,1]));
        }

        assert(equal(re, [3]));
    }

    debug(RBDoChecks)
    {
        /*
         * Print the tree.  This prints a sideways view of the tree in ASCII form,
         * with the number of indentations representing the level of the nodes.
         * It does not print values, only the tree structure and color of nodes.
         */
        void printTree(Node n, int indent = 0)
        {
            import std.stdio : write, writeln;
            if (n !is null)
            {
                printTree(n.right, indent + 2);
                for (int i = 0; i < indent; i++)
                    write(".");
                writeln(n.color == n.color.Black ? "B" : "R");
                printTree(n.left, indent + 2);
            }
            else
            {
                for (int i = 0; i < indent; i++)
                    write(".");
                writeln("N");
            }
            if (indent is 0)
                writeln();
        }

        /*
         * Check the tree for validity.  This is called after every add or remove.
         * This should only be enabled to debug the implementation of the RB Tree.
         */
        void check()
        {
            //
            // check implementation of the tree
            //
            int recurse(Node n, string path)
            {
                import std.stdio : writeln;
                if (n is null)
                    return 1;
                if (n.parent.left !is n && n.parent.right !is n)
                    throw new Exception("Node at path " ~ path ~ " has inconsistent pointers");
                Node next = n.next;
                static if (allowDuplicates)
                {
                    if (next !is _end && _less(next.value, n.value))
                        throw new Exception("ordering invalid at path " ~ path);
                }
                else
                {
                    if (next !is _end && !_less(n.value, next.value))
                        throw new Exception("ordering invalid at path " ~ path);
                }
                if (n.color == n.color.Red)
                {
                    if ((n.left !is null && n.left.color == n.color.Red) ||
                            (n.right !is null && n.right.color == n.color.Red))
                        throw new Exception("Node at path " ~ path ~ " is red with a red child");
                }

                int l = recurse(n.left, path ~ "L");
                int r = recurse(n.right, path ~ "R");
                if (l != r)
                {
                    writeln("bad tree at:");
                    debug printTree(n);
                    throw new Exception(
                        "Node at path " ~ path ~ " has different number of black nodes on left and right paths"
                    );
                }
                return l + (n.color == n.color.Black ? 1 : 0);
            }

            try
            {
                recurse(_end.left, "");
            }
            catch (Exception e)
            {
                debug printTree(_end.left, 0);
                throw e;
            }
        }
    }

    /**
      Formats the RedBlackTree into a sink function. For more info see $(D
      std.format.formatValue). Note that this only is available when the
      element type can be formatted. Otherwise, the default toString from
      Object is used.
     */
    static if (is(typeof((){FormatSpec!(char) fmt; formatValue((const(char)[]) {}, ConstRange.init, fmt);})))
    {
        void toString(scope void delegate(const(char)[]) sink, FormatSpec!char fmt) const {
            sink("RedBlackTree(");
            sink.formatValue(this[], fmt);
            sink(")");
        }
    }

    /**
     * Constructor. Pass in an array of elements, or individual elements to
     * initialize the tree with.
     */
    this(Elem[] elems...)
    {
        _setup();
        stableInsert(elems);
    }

    /**
     * Constructor. Pass in a range of elements to initialize the tree with.
     */
    this(Stuff)(Stuff stuff) if (isInputRange!Stuff && isImplicitlyConvertible!(ElementType!Stuff, Elem))
    {
        _setup();
        stableInsert(stuff);
    }

    ///
    this()
    {
        _setup();
    }

    private this(Node end, size_t length)
    {
        _end = end;
        _begin = end.leftmost;
        _length = length;
    }
}

//Verify Example for removeKey.
@safe pure unittest
{
    import std.algorithm.comparison : equal;
    auto rbt = redBlackTree!true(0, 1, 1, 1, 4, 5, 7);
    rbt.removeKey(1, 4, 7);
    assert(equal(rbt[], [0, 1, 1, 5]));
    rbt.removeKey(1, 1, 0);
    assert(equal(rbt[], [5]));
}

//Tests for removeKey
@safe pure unittest
{
    import std.algorithm.comparison : equal;
    {
        auto rbt = redBlackTree(["hello", "world", "foo", "bar"]);
        assert(equal(rbt[], ["bar", "foo", "hello", "world"]));
        assert(rbt.removeKey("hello") == 1);
        assert(equal(rbt[], ["bar", "foo", "world"]));
        assert(rbt.removeKey("hello") == 0);
        assert(equal(rbt[], ["bar", "foo", "world"]));
        assert(rbt.removeKey("hello", "foo", "bar") == 2);
        assert(equal(rbt[], ["world"]));
        assert(rbt.removeKey(["", "world", "hello"]) == 1);
        assert(rbt.empty);
    }

    {
        auto rbt = redBlackTree([1, 2, 12, 27, 4, 500]);
        assert(equal(rbt[], [1, 2, 4, 12, 27, 500]));
        assert(rbt.removeKey(1u) == 1);
        assert(equal(rbt[], [2, 4, 12, 27, 500]));
        assert(rbt.removeKey(cast(byte) 1) == 0);
        assert(equal(rbt[], [2, 4, 12, 27, 500]));
        assert(rbt.removeKey(1, 12u, cast(byte) 27) == 2);
        assert(equal(rbt[], [2, 4, 500]));
        assert(rbt.removeKey([cast(short) 0, cast(short) 500, cast(short) 1]) == 1);
        assert(equal(rbt[], [2, 4]));
    }
}

@safe pure unittest
{
    void test(T)()
    {
        auto rt1 = new RedBlackTree!(T, "a < b", false)();
        auto rt2 = new RedBlackTree!(T, "a < b", true)();
        auto rt3 = new RedBlackTree!(T, "a > b", false)();
        auto rt4 = new RedBlackTree!(T, "a > b", true)();
    }

    test!long();
    test!ulong();
    test!int();
    test!uint();
    test!short();
    test!ushort();
    test!byte();
    test!byte();
}

import std.range.primitives : isInputRange, ElementType;
import std.traits : isArray, isSomeString;

/++
    Convenience function for creating a $(D RedBlackTree!E) from a list of
    values.

    Params:
        allowDuplicates =  Whether duplicates should be allowed (optional, default: false)
        less = predicate to sort by (optional)
        elems = elements to insert into the rbtree (variadic arguments)
        range = range elements to insert into the rbtree (alternative to elems)
  +/
auto redBlackTree(E)(E[] elems...)
{
    return new RedBlackTree!E(elems);
}

/++ Ditto +/
auto redBlackTree(bool allowDuplicates, E)(E[] elems...)
{
    return new RedBlackTree!(E, "a < b", allowDuplicates)(elems);
}

/++ Ditto +/
auto redBlackTree(alias less, E)(E[] elems...)
if (is(typeof(binaryFun!less(E.init, E.init))))
{
    return new RedBlackTree!(E, less)(elems);
}

/++ Ditto +/
auto redBlackTree(alias less, bool allowDuplicates, E)(E[] elems...)
if (is(typeof(binaryFun!less(E.init, E.init))))
{
    //We shouldn't need to instantiate less here, but for some reason,
    //dmd can't handle it if we don't (even though the template which
    //takes less but not allowDuplicates works just fine).
    return new RedBlackTree!(E, binaryFun!less, allowDuplicates)(elems);
}

/++ Ditto +/
auto redBlackTree(Stuff)(Stuff range)
if (isInputRange!Stuff && !isArray!(Stuff))
{
    return new RedBlackTree!(ElementType!Stuff)(range);
}

/++ Ditto +/
auto redBlackTree(bool allowDuplicates, Stuff)(Stuff range)
if (isInputRange!Stuff && !isArray!(Stuff))
{
    return new RedBlackTree!(ElementType!Stuff, "a < b", allowDuplicates)(range);
}

/++ Ditto +/
auto redBlackTree(alias less, Stuff)(Stuff range)
if ( is(typeof(binaryFun!less((ElementType!Stuff).init, (ElementType!Stuff).init)))
    && isInputRange!Stuff && !isArray!(Stuff))
{
    return new RedBlackTree!(ElementType!Stuff, less)(range);
}

/++ Ditto +/
auto redBlackTree(alias less, bool allowDuplicates, Stuff)(Stuff range)
if ( is(typeof(binaryFun!less((ElementType!Stuff).init, (ElementType!Stuff).init)))
    && isInputRange!Stuff && !isArray!(Stuff))
{
    //We shouldn't need to instantiate less here, but for some reason,
    //dmd can't handle it if we don't (even though the template which
    //takes less but not allowDuplicates works just fine).
    return new RedBlackTree!(ElementType!Stuff, binaryFun!less, allowDuplicates)(range);
}

///
@safe pure unittest
{
    import std.range : iota;

    auto rbt1 = redBlackTree(0, 1, 5, 7);
    auto rbt2 = redBlackTree!string("hello", "world");
    auto rbt3 = redBlackTree!true(0, 1, 5, 7, 5);
    auto rbt4 = redBlackTree!"a > b"(0, 1, 5, 7);
    auto rbt5 = redBlackTree!("a > b", true)(0.1, 1.3, 5.9, 7.2, 5.9);

    // also works with ranges
    auto rbt6 = redBlackTree(iota(3));
    auto rbt7 = redBlackTree!true(iota(3));
    auto rbt8 = redBlackTree!"a > b"(iota(3));
    auto rbt9 = redBlackTree!("a > b", true)(iota(3));
}

//Combinations not in examples.
@safe pure unittest
{
    auto rbt1 = redBlackTree!(true, string)("hello", "hello");
    auto rbt2 = redBlackTree!((a, b){return a < b;}, double)(5.1, 2.3);
    auto rbt3 = redBlackTree!("a > b", true, string)("hello", "world");
}

//Range construction.
@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota;
    auto rbt = new RedBlackTree!(int, "a > b")(iota(5));
    assert(equal(rbt[], [4, 3, 2, 1, 0]));
}


// construction with arrays
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    auto rbt = redBlackTree!"a > b"([0, 1, 2, 3, 4]);
    assert(equal(rbt[], [4, 3, 2, 1, 0]));

    auto rbt2 = redBlackTree!"a > b"(["a", "b"]);
    assert(equal(rbt2[], ["b", "a"]));

    auto rbt3 = redBlackTree!"a > b"([1, 2]);
    assert(equal(rbt3[], [2, 1]));

    auto rbt4 = redBlackTree([0, 1, 7, 5]);
    assert(equal(rbt4[], [0, 1, 5, 7]));

    auto rbt5 = redBlackTree(["hello", "world"]);
    assert(equal(rbt5[], ["hello", "world"]));

    auto rbt6 = redBlackTree!true([0, 1, 5, 7, 5]);
    assert(equal(rbt6[], [0, 1, 5, 5, 7]));

    auto rbt7 = redBlackTree!"a > b"([0, 1, 5, 7]);
    assert(equal(rbt7[], [7, 5, 1, 0]));

    auto rbt8 = redBlackTree!("a > b", true)([0.1, 1.3, 5.9, 7.2, 5.9]);
    assert(equal(rbt8[], [7.2, 5.9, 5.9, 1.3, 0.1]));
}

// convenience wrapper range construction
@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.range : chain, iota;

    auto rbt = redBlackTree(iota(3));
    assert(equal(rbt[], [0, 1, 2]));

    auto rbt2 = redBlackTree!"a > b"(iota(2));
    assert(equal(rbt2[], [1, 0]));

    auto rbt3 = redBlackTree(chain([0, 1], [7, 5]));
    assert(equal(rbt3[], [0, 1, 5, 7]));

    auto rbt4 = redBlackTree(chain(["hello"], ["world"]));
    assert(equal(rbt4[], ["hello", "world"]));

    auto rbt5 = redBlackTree!true(chain([0, 1], [5, 7, 5]));
    assert(equal(rbt5[], [0, 1, 5, 5, 7]));

    auto rbt6 = redBlackTree!("a > b", true)(chain([0.1, 1.3], [5.9, 7.2, 5.9]));
    assert(equal(rbt6[], [7.2, 5.9, 5.9, 1.3, 0.1]));
}

@safe pure unittest
{
    import std.array : array;

    auto rt1 = redBlackTree(5, 4, 3, 2, 1);
    assert(rt1.length == 5);
    assert(array(rt1[]) == [1, 2, 3, 4, 5]);

    auto rt2 = redBlackTree!"a > b"(1.1, 2.1);
    assert(rt2.length == 2);
    assert(array(rt2[]) == [2.1, 1.1]);

    auto rt3 = redBlackTree!true(5, 5, 4);
    assert(rt3.length == 3);
    assert(array(rt3[]) == [4, 5, 5]);

    auto rt4 = redBlackTree!string("hello", "hello");
    assert(rt4.length == 1);
    assert(array(rt4[]) == ["hello"]);
}

@system unittest
{
    import std.conv : to;

    auto rt1 = redBlackTree!string();
    assert(rt1.to!string == "RedBlackTree([])");

    auto rt2 = redBlackTree!string("hello");
    assert(rt2.to!string == "RedBlackTree([\"hello\"])");

    auto rt3 = redBlackTree!string("hello", "world", "!");
    assert(rt3.to!string == "RedBlackTree([\"!\", \"hello\", \"world\"])");

    // type deduction can be done automatically
    auto rt4 = redBlackTree(["hello"]);
    assert(rt4.to!string == "RedBlackTree([\"hello\"])");
}

//constness checks
@safe pure unittest
{
    const rt1 = redBlackTree(5,4,3,2,1);
    static assert(is(typeof(rt1.length)));
    static assert(is(typeof(5 in rt1)));

    static assert(is(typeof(rt1.upperBound(3).front) == const(int)));
    import std.algorithm.comparison : equal;
    assert(rt1.upperBound(3).equal([4, 5]));
    assert(rt1.lowerBound(3).equal([1, 2]));
    assert(rt1.equalRange(3).equal([3]));
    assert(rt1[].equal([1, 2, 3, 4, 5]));
}

//immutable checks
@safe pure unittest
{
    immutable rt1 = redBlackTree(5,4,3,2,1);
    static assert(is(typeof(rt1.length)));

    static assert(is(typeof(rt1.upperBound(3).front) == immutable(int)));
    import std.algorithm.comparison : equal;
    assert(rt1.upperBound(2).equal([3, 4, 5]));
}

// issue 15941
@safe pure unittest
{
    class C {}
    RedBlackTree!(C, "cast(void*)a < cast(void*) b") tree;
}

@safe pure unittest // const/immutable elements (issue 17519)
{
    RedBlackTree!(immutable int) t1;
    RedBlackTree!(const int) t2;

    import std.algorithm.iteration : map;
    static struct S { int* p; }
    auto t3 = new RedBlackTree!(immutable S, (a, b) => *a.p < *b.p);
    t3.insert([1, 2, 3].map!(x => immutable S(new int(x))));
    static assert(!__traits(compiles, *t3.front.p = 4));
    assert(*t3.front.p == 1);
}
