@safe unittest
{
    import std.container.dlist;

    import std.algorithm.comparison : equal;
    import std.container : DList;

    auto s = DList!int(1, 2, 3);
    assert(equal(s[], [1, 2, 3]));

    s.removeFront();
    assert(equal(s[], [2, 3]));
    s.removeBack();
    assert(equal(s[], [2]));

    s.insertFront([4, 5]);
    assert(equal(s[], [4, 5, 2]));
    s.insertBack([6, 7]);
    assert(equal(s[], [4, 5, 2, 6, 7]));

    // If you want to apply range operations, simply slice it.
    import std.algorithm.searching : countUntil;
    import std.range : popFrontN, popBackN, walkLength;

    auto sl = DList!int([1, 2, 3, 4, 5]);
    assert(countUntil(sl[], 2) == 1);

    auto r = sl[];
    popFrontN(r, 2);
    popBackN(r, 2);
    assert(r.equal([3]));
    assert(walkLength(r) == 1);

    // DList.Range can be used to remove elements from the list it spans
    auto nl = DList!int([1, 2, 3, 4, 5]);
    for (auto rn = nl[]; !rn.empty;)
        if (rn.front % 2 == 0)
            nl.popFirstOf(rn);
        else
            rn.popFront();
    assert(equal(nl[], [1, 3, 5]));
    auto rs = nl[];
    rs.popFront();
    nl.remove(rs);
    assert(equal(nl[], [1]));
}

