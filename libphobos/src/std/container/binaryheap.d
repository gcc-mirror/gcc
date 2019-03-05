/**
This module provides a $(D BinaryHeap) (aka priority queue)
adaptor that makes a binary heap out of any user-provided random-access range.

This module is a submodule of $(MREF std, container).

Source: $(PHOBOSSRC std/container/_binaryheap.d)

Copyright: 2010- Andrei Alexandrescu. All rights reserved by the respective holders.

License: Distributed under the Boost Software License, Version 1.0.
(See accompanying file LICENSE_1_0.txt or copy at $(HTTP
boost.org/LICENSE_1_0.txt)).

Authors: $(HTTP erdani.com, Andrei Alexandrescu)
*/
module std.container.binaryheap;

import std.range.primitives;
import std.traits;

public import std.container.util;

///
@system unittest
{
    import std.algorithm.comparison : equal;
    import std.range : take;
    auto maxHeap = heapify([4, 7, 3, 1, 5]);
    assert(maxHeap.take(3).equal([7, 5, 4]));

    auto minHeap = heapify!"a > b"([4, 7, 3, 1, 5]);
    assert(minHeap.take(3).equal([1, 3, 4]));
}

// BinaryHeap
/**
Implements a $(HTTP en.wikipedia.org/wiki/Binary_heap, binary heap)
container on top of a given random-access range type (usually $(D
T[])) or a random-access container type (usually $(D Array!T)). The
documentation of $(D BinaryHeap) will refer to the underlying range or
container as the $(I store) of the heap.

The binary heap induces structure over the underlying store such that
accessing the largest element (by using the $(D front) property) is a
$(BIGOH 1) operation and extracting it (by using the $(D
removeFront()) method) is done fast in $(BIGOH log n) time.

If $(D less) is the less-than operator, which is the default option,
then $(D BinaryHeap) defines a so-called max-heap that optimizes
extraction of the $(I largest) elements. To define a min-heap,
instantiate BinaryHeap with $(D "a > b") as its predicate.

Simply extracting elements from a $(D BinaryHeap) container is
tantamount to lazily fetching elements of $(D Store) in descending
order. Extracting elements from the $(D BinaryHeap) to completion
leaves the underlying store sorted in ascending order but, again,
yields elements in descending order.

If $(D Store) is a range, the $(D BinaryHeap) cannot grow beyond the
size of that range. If $(D Store) is a container that supports $(D
insertBack), the $(D BinaryHeap) may grow by adding elements to the
container.
     */
struct BinaryHeap(Store, alias less = "a < b")
if (isRandomAccessRange!(Store) || isRandomAccessRange!(typeof(Store.init[])))
{
    import std.algorithm.comparison : min;
    import std.algorithm.mutation : move, swapAt;
    import std.algorithm.sorting : HeapOps;
    import std.exception : enforce;
    import std.functional : binaryFun;
    import std.typecons : RefCounted, RefCountedAutoInitialize;

    static if (isRandomAccessRange!Store)
        alias Range = Store;
    else
        alias Range = typeof(Store.init[]);
    alias percolate = HeapOps!(less, Range).percolate;
    alias buildHeap = HeapOps!(less, Range).buildHeap;

// Really weird @@BUG@@: if you comment out the "private:" label below,
// std.algorithm can't unittest anymore
//private:

    // The payload includes the support store and the effective length
    private static struct Data
    {
        Store _store;
        size_t _length;
    }
    private RefCounted!(Data, RefCountedAutoInitialize.no) _payload;
    // Comparison predicate
    private alias comp = binaryFun!(less);
    // Convenience accessors
    private @property ref Store _store()
    {
        assert(_payload.refCountedStore.isInitialized);
        return _payload._store;
    }
    private @property ref size_t _length()
    {
        assert(_payload.refCountedStore.isInitialized);
        return _payload._length;
    }

    // Asserts that the heap property is respected.
    private void assertValid()
    {
        debug
        {
            import std.conv : to;
            if (!_payload.refCountedStore.isInitialized) return;
            if (_length < 2) return;
            for (size_t n = _length - 1; n >= 1; --n)
            {
                auto parentIdx = (n - 1) / 2;
                assert(!comp(_store[parentIdx], _store[n]), to!string(n));
            }
        }
    }

    // @@@BUG@@@: add private here, std.algorithm doesn't unittest anymore
    /*private*/ void pop(Store store)
    {
        assert(!store.empty, "Cannot pop an empty store.");
        if (store.length == 1) return;
        auto t1 = store[].moveFront();
        auto t2 = store[].moveBack();
        store.front = move(t2);
        store.back = move(t1);
        percolate(store[], 0, store.length - 1);
    }

public:

    /**
       Converts the store $(D s) into a heap. If $(D initialSize) is
       specified, only the first $(D initialSize) elements in $(D s)
       are transformed into a heap, after which the heap can grow up
       to $(D r.length) (if $(D Store) is a range) or indefinitely (if
       $(D Store) is a container with $(D insertBack)). Performs
       $(BIGOH min(r.length, initialSize)) evaluations of $(D less).
     */
    this(Store s, size_t initialSize = size_t.max)
    {
        acquire(s, initialSize);
    }

/**
Takes ownership of a store. After this, manipulating $(D s) may make
the heap work incorrectly.
     */
    void acquire(Store s, size_t initialSize = size_t.max)
    {
        _payload.refCountedStore.ensureInitialized();
        _store = move(s);
        _length = min(_store.length, initialSize);
        if (_length < 2) return;
        buildHeap(_store[]);
        assertValid();
    }

/**
Takes ownership of a store assuming it already was organized as a
heap.
     */
    void assume(Store s, size_t initialSize = size_t.max)
    {
        _payload.refCountedStore.ensureInitialized();
        _store = s;
        _length = min(_store.length, initialSize);
        assertValid();
    }

/**
Clears the heap. Returns the portion of the store from $(D 0) up to
$(D length), which satisfies the $(LINK2 https://en.wikipedia.org/wiki/Heap_(data_structure),
heap property).
     */
    auto release()
    {
        if (!_payload.refCountedStore.isInitialized)
        {
            return typeof(_store[0 .. _length]).init;
        }
        assertValid();
        auto result = _store[0 .. _length];
        _payload = _payload.init;
        return result;
    }

/**
Returns $(D true) if the heap is _empty, $(D false) otherwise.
     */
    @property bool empty()
    {
        return !length;
    }

/**
Returns a duplicate of the heap. The $(D dup) method is available only if the
underlying store supports it.
     */
    static if (is(typeof((Store s) { return s.dup; }(Store.init)) == Store))
    {
        @property BinaryHeap dup()
        {
            BinaryHeap result;
            if (!_payload.refCountedStore.isInitialized) return result;
            result.assume(_store.dup, length);
            return result;
        }
    }

/**
Returns the _length of the heap.
     */
    @property size_t length()
    {
        return _payload.refCountedStore.isInitialized ? _length : 0;
    }

/**
Returns the _capacity of the heap, which is the length of the
underlying store (if the store is a range) or the _capacity of the
underlying store (if the store is a container).
     */
    @property size_t capacity()
    {
        if (!_payload.refCountedStore.isInitialized) return 0;
        static if (is(typeof(_store.capacity) : size_t))
        {
            return _store.capacity;
        }
        else
        {
            return _store.length;
        }
    }

/**
Returns a copy of the _front of the heap, which is the largest element
according to $(D less).
     */
    @property ElementType!Store front()
    {
        enforce(!empty, "Cannot call front on an empty heap.");
        return _store.front;
    }

/**
Clears the heap by detaching it from the underlying store.
     */
    void clear()
    {
        _payload = _payload.init;
    }

/**
Inserts $(D value) into the store. If the underlying store is a range
and $(D length == capacity), throws an exception.
     */
    size_t insert(ElementType!Store value)
    {
        static if (is(typeof(_store.insertBack(value))))
        {
            _payload.refCountedStore.ensureInitialized();
            if (length == _store.length)
            {
                // reallocate
                _store.insertBack(value);
            }
            else
            {
                // no reallocation
                _store[_length] = value;
            }
        }
        else
        {
            import std.traits : isDynamicArray;
            static if (isDynamicArray!Store)
            {
                if (length == _store.length)
                    _store.length = (length < 6 ? 8 : length * 3 / 2);
                _store[_length] = value;
            }
            else
            {
                // can't grow
                enforce(length < _store.length,
                        "Cannot grow a heap created over a range");
            }
        }

        // sink down the element
        for (size_t n = _length; n; )
        {
            auto parentIdx = (n - 1) / 2;
            if (!comp(_store[parentIdx], _store[n])) break; // done!
            // must swap and continue
            _store.swapAt(parentIdx, n);
            n = parentIdx;
        }
        ++_length;
        debug(BinaryHeap) assertValid();
        return 1;
    }

/**
Removes the largest element from the heap.
     */
    void removeFront()
    {
        enforce(!empty, "Cannot call removeFront on an empty heap.");
        if (_length > 1)
        {
            auto t1 = _store[].moveFront();
            auto t2 = _store[].moveAt(_length - 1);
            _store.front = move(t2);
            _store[_length - 1] = move(t1);
        }
        --_length;
        percolate(_store[], 0, _length);
    }

    /// ditto
    alias popFront = removeFront;

/**
Removes the largest element from the heap and returns a copy of
it. The element still resides in the heap's store. For performance
reasons you may want to use $(D removeFront) with heaps of objects
that are expensive to copy.
     */
    ElementType!Store removeAny()
    {
        removeFront();
        return _store[_length];
    }

/**
Replaces the largest element in the store with $(D value).
     */
    void replaceFront(ElementType!Store value)
    {
        // must replace the top
        assert(!empty, "Cannot call replaceFront on an empty heap.");
        _store.front = value;
        percolate(_store[], 0, _length);
        debug(BinaryHeap) assertValid();
    }

/**
If the heap has room to grow, inserts $(D value) into the store and
returns $(D true). Otherwise, if $(D less(value, front)), calls $(D
replaceFront(value)) and returns again $(D true). Otherwise, leaves
the heap unaffected and returns $(D false). This method is useful in
scenarios where the smallest $(D k) elements of a set of candidates
must be collected.
     */
    bool conditionalInsert(ElementType!Store value)
    {
        _payload.refCountedStore.ensureInitialized();
        if (_length < _store.length)
        {
            insert(value);
            return true;
        }

        assert(!_store.empty, "Cannot replace front of an empty heap.");
        if (!comp(value, _store.front)) return false; // value >= largest
        _store.front = value;

        percolate(_store[], 0, _length);
        debug(BinaryHeap) assertValid();
        return true;
    }

/**
Swapping is allowed if the heap is full. If $(D less(value, front)), the
method exchanges store.front and value and returns $(D true). Otherwise, it
leaves the heap unaffected and returns $(D false).
     */
    bool conditionalSwap(ref ElementType!Store value)
    {
        _payload.refCountedStore.ensureInitialized();
        assert(_length == _store.length);
        assert(!_store.empty, "Cannot swap front of an empty heap.");

        if (!comp(value, _store.front)) return false; // value >= largest

        import std.algorithm.mutation : swap;
        swap(_store.front, value);

        percolate(_store[], 0, _length);
        debug(BinaryHeap) assertValid();

        return true;
    }
}

/// Example from "Introduction to Algorithms" Cormen et al, p 146
@system unittest
{
    import std.algorithm.comparison : equal;
    int[] a = [ 4, 1, 3, 2, 16, 9, 10, 14, 8, 7 ];
    auto h = heapify(a);
    // largest element
    assert(h.front == 16);
    // a has the heap property
    assert(equal(a, [ 16, 14, 10, 8, 7, 9, 3, 2, 4, 1 ]));
}

/// $(D BinaryHeap) implements the standard input range interface, allowing
/// lazy iteration of the underlying range in descending order.
@system unittest
{
    import std.algorithm.comparison : equal;
    import std.range : take;
    int[] a = [4, 1, 3, 2, 16, 9, 10, 14, 8, 7];
    auto top5 = heapify(a).take(5);
    assert(top5.equal([16, 14, 10, 9, 8]));
}

/**
Convenience function that returns a $(D BinaryHeap!Store) object
initialized with $(D s) and $(D initialSize).
 */
BinaryHeap!(Store, less) heapify(alias less = "a < b", Store)(Store s,
        size_t initialSize = size_t.max)
{

    return BinaryHeap!(Store, less)(s, initialSize);
}

///
@system unittest
{
    import std.conv : to;
    import std.range.primitives;
    {
        // example from "Introduction to Algorithms" Cormen et al., p 146
        int[] a = [ 4, 1, 3, 2, 16, 9, 10, 14, 8, 7 ];
        auto h = heapify(a);
        h = heapify!"a < b"(a);
        assert(h.front == 16);
        assert(a == [ 16, 14, 10, 8, 7, 9, 3, 2, 4, 1 ]);
        auto witness = [ 16, 14, 10, 9, 8, 7, 4, 3, 2, 1 ];
        for (; !h.empty; h.removeFront(), witness.popFront())
        {
            assert(!witness.empty);
            assert(witness.front == h.front);
        }
        assert(witness.empty);
    }
    {
        int[] a = [ 4, 1, 3, 2, 16, 9, 10, 14, 8, 7 ];
        int[] b = new int[a.length];
        BinaryHeap!(int[]) h = BinaryHeap!(int[])(b, 0);
        foreach (e; a)
        {
            h.insert(e);
        }
        assert(b == [ 16, 14, 10, 8, 7, 3, 9, 1, 4, 2 ], to!string(b));
    }
}

@system unittest
{
    // Test range interface.
    import std.algorithm.comparison : equal;
    int[] a = [4, 1, 3, 2, 16, 9, 10, 14, 8, 7];
    auto h = heapify(a);
    static assert(isInputRange!(typeof(h)));
    assert(h.equal([16, 14, 10, 9, 8, 7, 4, 3, 2, 1]));
}

@system unittest // 15675
{
    import std.container.array : Array;

    Array!int elements = [1, 2, 10, 12];
    auto heap = heapify(elements);
    assert(heap.front == 12);
}

@system unittest // 16072
{
    auto q = heapify!"a > b"([2, 4, 5]);
    q.insert(1);
    q.insert(6);
    assert(q.front == 1);

    // test more multiple grows
    int[] arr;
    auto r = heapify!"a < b"(arr);
    foreach (i; 0 .. 100)
        r.insert(i);

    assert(r.front == 99);
}

@system unittest
{
    import std.algorithm.comparison : equal;
    int[] a = [4, 1, 3, 2, 16, 9, 10, 14, 8, 7];
    auto heap = heapify(a);
    auto dup = heap.dup();
    assert(dup.equal([16, 14, 10, 9, 8, 7, 4, 3, 2, 1]));
}

@safe unittest
{
    static struct StructWithoutDup
    {
        int[] a;
        @disable StructWithoutDup dup()
        {
            StructWithoutDup d;
            return d;
        }
        alias a this;
    }

    // Assert Binary heap can be created when Store doesn't have dup
    // if dup is not used.
    assert(__traits(compiles, ()
        {
            auto s = StructWithoutDup([1,2]);
            auto h = heapify(s);
        }));

    // Assert dup can't be used on BinaryHeaps when Store doesn't have dup
    assert(!__traits(compiles, ()
        {
            auto s = StructWithoutDup([1,2]);
            auto h = heapify(s);
            h.dup();
        }));
}

@safe unittest
{
    static struct StructWithDup
    {
        int[] a;
        StructWithDup dup()
        {
            StructWithDup d;
            return d;
        }
        alias a this;
    }

    // Assert dup can be used on BinaryHeaps when Store has dup
    assert(__traits(compiles, ()
        {
            auto s = StructWithDup([1, 2]);
            auto h = heapify(s);
            h.dup();
        }));
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange;

    alias RefRange = DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random);

    RefRange a;
    RefRange b;
    a.reinit();
    b.reinit();

    auto heap = heapify(a);
    foreach (ref elem; b)
    {
        heap.conditionalSwap(elem);
    }

    assert(equal(heap, [ 5, 5, 4, 4, 3, 3, 2, 2, 1, 1]));
    assert(equal(b, [10, 9, 8, 7, 6, 6, 7, 8, 9, 10]));
}

@system unittest // Issue 17314
{
    import std.algorithm.comparison : equal;
    int[] a = [5];
    auto heap = heapify(a);
    heap.insert(6);
    assert(equal(heap, [6, 5]));
}
