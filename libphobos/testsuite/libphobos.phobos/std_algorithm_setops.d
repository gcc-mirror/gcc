@safe unittest
{
    import std.algorithm.setops;

    import std.algorithm.searching : canFind;
    import std.range;
    import std.typecons : tuple;

    auto N = sequence!"n"(0);         // the range of natural numbers
    auto N2 = cartesianProduct(N, N); // the range of all pairs of natural numbers

    // Various arbitrary number pairs can be found in the range in finite time.
    assert(canFind(N2, tuple(0, 0)));
    assert(canFind(N2, tuple(123, 321)));
    assert(canFind(N2, tuple(11, 35)));
    assert(canFind(N2, tuple(279, 172)));
}

@safe unittest
{
    import std.algorithm.setops;

    import std.algorithm.searching : canFind;
    import std.typecons : tuple;

    auto B = [ 1, 2, 3 ];
    auto C = [ 4, 5, 6 ];
    auto BC = cartesianProduct(B, C);

    foreach (n; [[1, 4], [2, 4], [3, 4], [1, 5], [2, 5], [3, 5], [1, 6],
                 [2, 6], [3, 6]])
    {
        assert(canFind(BC, tuple(n[0], n[1])));
    }
}

@safe unittest
{
    import std.algorithm.setops;

    import std.algorithm.comparison : equal;
    import std.typecons : tuple;

    auto A = [ 1, 2, 3 ];
    auto B = [ 'a', 'b', 'c' ];
    auto C = [ "x", "y", "z" ];
    auto ABC = cartesianProduct(A, B, C);

    assert(ABC.equal([
        tuple(1, 'a', "x"), tuple(1, 'a', "y"), tuple(1, 'a', "z"),
        tuple(1, 'b', "x"), tuple(1, 'b', "y"), tuple(1, 'b', "z"),
        tuple(1, 'c', "x"), tuple(1, 'c', "y"), tuple(1, 'c', "z"),
        tuple(2, 'a', "x"), tuple(2, 'a', "y"), tuple(2, 'a', "z"),
        tuple(2, 'b', "x"), tuple(2, 'b', "y"), tuple(2, 'b', "z"),
        tuple(2, 'c', "x"), tuple(2, 'c', "y"), tuple(2, 'c', "z"),
        tuple(3, 'a', "x"), tuple(3, 'a', "y"), tuple(3, 'a', "z"),
        tuple(3, 'b', "x"), tuple(3, 'b', "y"), tuple(3, 'b', "z"),
        tuple(3, 'c', "x"), tuple(3, 'c', "y"), tuple(3, 'c', "z")
    ]));
}

@system unittest
{
    import std.algorithm.setops;

    import std.typecons : tuple, Tuple;

    // Figure which number can be found in most arrays of the set of
    // arrays below.
    double[][] a =
    [
        [ 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    auto b = new Tuple!(double, uint)[1];
    // it will modify the input range, hence we need to create a duplicate
    largestPartialIntersection(a.dup, b);
    // First member is the item, second is the occurrence count
    assert(b[0] == tuple(7.0, 4u));
    // 7.0 occurs in 4 out of 5 inputs, more than any other number

    // If more of the top-frequent numbers are needed, just create a larger
    // tgt range
    auto c = new Tuple!(double, uint)[2];
    largestPartialIntersection(a, c);
    assert(c[0] == tuple(1.0, 3u));
    // 1.0 occurs in 3 inputs

    // multiset
    double[][] x =
    [
        [1, 1, 1, 1, 4, 7, 8],
        [1, 7],
        [1, 7, 8],
        [4, 7],
        [7]
    ];
    auto y = new Tuple!(double, uint)[2];
    largestPartialIntersection(x.dup, y);
    // 7.0 occurs 5 times
    assert(y[0] == tuple(7.0, 5u));
    // 1.0 occurs 6 times
    assert(y[1] == tuple(1.0, 6u));
}

@system unittest
{
    import std.algorithm.setops;

    import std.typecons : tuple, Tuple;

    // Figure which number can be found in most arrays of the set of
    // arrays below, with specific per-element weights
    double[][] a =
    [
        [ 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    auto b = new Tuple!(double, uint)[1];
    double[double] weights = [ 1:1.2, 4:2.3, 7:1.1, 8:1.1 ];
    largestPartialIntersectionWeighted(a, b, weights);
    // First member is the item, second is the occurrence count
    assert(b[0] == tuple(4.0, 2u));
    // 4.0 occurs 2 times -> 4.6 (2 * 2.3)
    // 7.0 occurs 3 times -> 4.4 (3 * 1.1)

   // multiset
    double[][] x =
    [
        [ 1, 1, 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    auto y = new Tuple!(double, uint)[1];
    largestPartialIntersectionWeighted(x, y, weights);
    assert(y[0] == tuple(1.0, 5u));
    // 1.0 occurs 5 times -> 1.2 * 5 = 6
}

@system unittest
{
    import std.algorithm.setops;

    import std.algorithm.comparison : equal;

    double[][] a =
    [
        [ 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    auto witness = [
        1, 1, 1, 4, 4, 7, 7, 7, 7, 8, 8
    ];
    assert(equal(multiwayMerge(a), witness));

    double[][] b =
    [
        // range with duplicates
        [ 1, 1, 4, 7, 8 ],
        [ 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    // duplicates are propagated to the resulting range
    assert(equal(multiwayMerge(b), witness));
}

@system unittest
{
    import std.algorithm.setops;

    import std.algorithm.comparison : equal;

    // sets
    double[][] a =
    [
        [ 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];

    auto witness = [1, 4, 7, 8];
    assert(equal(multiwayUnion(a), witness));

    // multisets
    double[][] b =
    [
        [ 1, 1, 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    assert(equal(multiwayUnion(b), witness));

    double[][] c =
    [
        [9, 8, 8, 8, 7, 6],
        [9, 8, 6],
        [9, 8, 5]
    ];
    auto witness2 = [9, 8, 7, 6, 5];
    assert(equal(multiwayUnion!"a > b"(c), witness2));
}

@safe unittest
{
    import std.algorithm.setops;

    import std.algorithm.comparison : equal;
    import std.range.primitives : isForwardRange;

    //sets
    int[] a = [ 1, 2, 4, 5, 7, 9 ];
    int[] b = [ 0, 1, 2, 4, 7, 8 ];
    assert(equal(setDifference(a, b), [5, 9]));
    static assert(isForwardRange!(typeof(setDifference(a, b))));

    // multisets
    int[] x = [1, 1, 1, 2, 3];
    int[] y = [1, 1, 2, 4, 5];
    auto r = setDifference(x, y);
    assert(equal(r, [1, 3]));
    assert(setDifference(r, x).empty);
}

@safe unittest
{
    import std.algorithm.setops;

    import std.algorithm.comparison : equal;

    // sets
    int[] a = [ 1, 2, 4, 5, 7, 9 ];
    int[] b = [ 0, 1, 2, 4, 7, 8 ];
    int[] c = [ 0, 1, 4, 5, 7, 8 ];
    assert(equal(setIntersection(a, a), a));
    assert(equal(setIntersection(a, b), [1, 2, 4, 7]));
    assert(equal(setIntersection(a, b, c), [1, 4, 7]));

    // multisets
    int[] d = [ 1, 1, 2, 2, 7, 7 ];
    int[] e = [ 1, 1, 1, 7];
    assert(equal(setIntersection(a, d), [1, 2, 7]));
    assert(equal(setIntersection(d, e), [1, 1, 7]));
}

@safe unittest
{
    import std.algorithm.setops;

    import std.algorithm.comparison : equal;
    import std.range.primitives : isForwardRange;

    // sets
    int[] a = [ 1, 2, 4, 5, 7, 9 ];
    int[] b = [ 0, 1, 2, 4, 7, 8 ];
    assert(equal(setSymmetricDifference(a, b), [0, 5, 8, 9][]));
    static assert(isForwardRange!(typeof(setSymmetricDifference(a, b))));

    //mutisets
    int[] c = [1, 1, 1, 1, 2, 2, 2, 4, 5, 6];
    int[] d = [1, 1, 2, 2, 2, 2, 4, 7, 9];
    assert(equal(setSymmetricDifference(c, d), setSymmetricDifference(d, c)));
    assert(equal(setSymmetricDifference(c, d), [1, 1, 2, 5, 6, 7, 9]));
}

