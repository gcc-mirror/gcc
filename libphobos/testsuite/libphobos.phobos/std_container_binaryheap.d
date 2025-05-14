@system unittest
{
    import std.container.binaryheap;

    import std.algorithm.comparison : equal;
    import std.range : take;
    auto maxHeap = heapify([4, 7, 3, 1, 5]);
    assert(maxHeap.take(3).equal([7, 5, 4]));

    auto minHeap = heapify!"a > b"([4, 7, 3, 1, 5]);
    assert(minHeap.take(3).equal([1, 3, 4]));
}

@system unittest
{
    import std.container.binaryheap;

    import std.algorithm.comparison : equal;
    int[] a = [ 4, 1, 3, 2, 16, 9, 10, 14, 8, 7 ];
    auto h = heapify(a);
    // largest element
    assert(h.front == 16);
    // a has the heap property
    assert(equal(a, [ 16, 14, 10, 8, 7, 9, 3, 2, 4, 1 ]));
}

@system unittest
{
    import std.container.binaryheap;

    import std.algorithm.comparison : equal;
    import std.range : take;
    int[] a = [4, 1, 3, 2, 16, 9, 10, 14, 8, 7];
    auto top5 = heapify(a).take(5);
    assert(top5.equal([16, 14, 10, 9, 8]));
}

@system unittest
{
    import std.container.binaryheap;

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
    import std.container.binaryheap;

    import std.stdio;
    import std.algorithm.comparison : equal;
    import std.container.binaryheap;

    int[] a = [ 4, 1, 3, 2, 16, 9, 10, 14, 8, 7 ];
    auto h = heapify(a);

    // Internal representation of Binary Heap tree
    assert(a.equal([16, 14, 10, 8, 7, 9, 3, 2, 4, 1]));

    h.replaceFront(30);
    // Value 16 was replaced by 30
    assert(a.equal([30, 14, 10, 8, 7, 9, 3, 2, 4, 1]));

    // Making changes to the Store will be seen in the Heap
    a[0] = 40;
    assert(h.front() == 40);

    // Inserting a new element will reallocate the Store, leaving
    // the original Store unchanged.
    h.insert(20);
    assert(a.equal([40, 14, 10, 8, 7, 9, 3, 2, 4, 1]));

    // Making changes to the original Store will not affect the Heap anymore
    a[0] = 60;
    assert(h.front() == 40);
}

