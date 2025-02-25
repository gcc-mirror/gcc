@safe unittest
{
    import std.container.slist;

    import std.algorithm.comparison : equal;
    import std.container : SList;

    auto s = SList!int(1, 2, 3);
    assert(equal(s[], [1, 2, 3]));

    s.removeFront();
    assert(equal(s[], [2, 3]));

    s.insertFront([5, 6]);
    assert(equal(s[], [5, 6, 2, 3]));

    // If you want to apply range operations, simply slice it.
    import std.algorithm.searching : countUntil;
    import std.range : popFrontN, walkLength;

    auto sl = SList!int(1, 2, 3, 4, 5);
    assert(countUntil(sl[], 2) == 1);

    auto r = sl[];
    popFrontN(r, 2);
    assert(walkLength(r) == 3);
}

