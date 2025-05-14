@system unittest
{
    import std.container.util;

    import std.algorithm.comparison : equal;
    import std.container;

    auto arr = make!(Array!int)([4, 2, 3, 1]);
    assert(equal(arr[], [4, 2, 3, 1]));

    auto rbt = make!(RedBlackTree!(int, "a > b"))([4, 2, 3, 1]);
    assert(equal(rbt[], [4, 3, 2, 1]));

    alias makeList = make!(SList!int);
    auto slist = makeList(1, 2, 3);
    assert(equal(slist[], [1, 2, 3]));
}

@safe unittest
{
    import std.container.util;

    import std.container.array : Array;
    import std.range : only, repeat;
    import std.range.primitives : isInfinite;
    static assert(__traits(compiles, { auto arr = make!Array(only(5)); }));
    static assert(!__traits(compiles, { auto arr = make!Array(repeat(5)); }));
}

@system unittest
{
    import std.container.util;

    import std.algorithm.comparison : equal;
    import std.container.array, std.container.rbtree, std.container.slist;
    import std.range : iota;

    auto arr = make!Array(iota(5));
    assert(equal(arr[], [0, 1, 2, 3, 4]));

    auto rbtmax = make!(RedBlackTree, "a > b")(iota(5));
    assert(equal(rbtmax[], [4, 3, 2, 1, 0]));

    auto rbtmin = make!RedBlackTree(4, 1, 3, 2);
    assert(equal(rbtmin[], [1, 2, 3, 4]));

    alias makeList = make!SList;
    auto list = makeList(1, 7, 42);
    assert(equal(list[], [1, 7, 42]));
}

