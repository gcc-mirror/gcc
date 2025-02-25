@safe pure unittest
{
    import std.container.rbtree;

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

@safe pure unittest
{
    import std.container.rbtree;

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

