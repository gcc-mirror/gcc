@safe @nogc unittest
{
    import std.algorithm.comparison;

    assert(3.among(1, 42, 24, 3, 2));

    if (auto pos = "bar".among("foo", "bar", "baz"))
        assert(pos == 2);
    else
        assert(false);

    // 42 is larger than 24
    assert(42.among!((lhs, rhs) => lhs > rhs)(43, 24, 100) == 2);
}

@safe @nogc unittest
{
    import std.algorithm.comparison;

    assert(3.among!(2, 3, 4));
    assert("bar".among!("foo", "bar", "baz") == 2);
}

@system unittest
{
    import std.algorithm.comparison;

    import std.algorithm.iteration : map;
    import std.format : format;

    class A
    {
        int a;
        this(int a) {this.a = a;}
        @property int i() { return a; }
    }
    interface I { }
    class B : I { }

    Object[] arr = [new A(1), new B(), null];

    auto results = arr.map!(castSwitch!(
                                (A a) => "A with a value of %d".format(a.a),
                                (I i) => "derived from I",
                                ()    => "null reference",
                            ))();

    // A is handled directly:
    assert(results[0] == "A with a value of 1");
    // B has no handler - it is handled by the handler of I:
    assert(results[1] == "derived from I");
    // null is handled by the null handler:
    assert(results[2] == "null reference");
}

@system unittest
{
    import std.algorithm.comparison;

    import std.exception : assertThrown;

    class A { }
    class B { }
    // Void handlers are allowed if they throw:
    assertThrown!Exception(
        new B().castSwitch!(
            (A a) => 1,
            (B d)    { throw new Exception("B is not allowed!"); }
        )()
    );

    // Void handlers are also allowed if all the handlers are void:
    new A().castSwitch!(
        (A a) { },
        (B b) { assert(false); },
    )();
}

@safe @nogc unittest
{
    import std.algorithm.comparison;

    assert(clamp(2, 1, 3) == 2);
    assert(clamp(0, 1, 3) == 1);
    assert(clamp(4, 1, 3) == 3);

    assert(clamp(1, 1, 1) == 1);

    assert(clamp(5, -1, 2u) == 2);

    auto x = clamp(42, uint.max, uint.max);
    static assert(is(typeof(x) == int));
    assert(x == -1);
}

pure @safe unittest
{
    import std.algorithm.comparison;

    int result;

    result = cmp("abc", "abc");
    assert(result == 0);
    result = cmp("", "");
    assert(result == 0);
    result = cmp("abc", "abcd");
    assert(result < 0);
    result = cmp("abcd", "abc");
    assert(result > 0);
    result = cmp("abc"d, "abd");
    assert(result < 0);
    result = cmp("bbc", "abc"w);
    assert(result > 0);
    result = cmp("aaa", "aaaa"d);
    assert(result < 0);
    result = cmp("aaaa", "aaa"d);
    assert(result > 0);
    result = cmp("aaa", "aaa"d);
    assert(result == 0);
    result = cmp("aaa"d, "aaa"d);
    assert(result == 0);
    result = cmp(cast(int[])[], cast(int[])[]);
    assert(result == 0);
    result = cmp([1, 2, 3], [1, 2, 3]);
    assert(result == 0);
    result = cmp([1, 3, 2], [1, 2, 3]);
    assert(result > 0);
    result = cmp([1, 2, 3], [1L, 2, 3, 4]);
    assert(result < 0);
    result = cmp([1L, 2, 3], [1, 2]);
    assert(result > 0);
}

pure @safe unittest
{
    import std.algorithm.comparison;

    int result;

    result = cmp!"a > b"("abc", "abc");
    assert(result == 0);
    result = cmp!"a > b"("", "");
    assert(result == 0);
    result = cmp!"a > b"("abc", "abcd");
    assert(result < 0);
    result = cmp!"a > b"("abcd", "abc");
    assert(result > 0);
    result = cmp!"a > b"("abc"d, "abd");
    assert(result > 0);
    result = cmp!"a > b"("bbc", "abc"w);
    assert(result < 0);
    result = cmp!"a > b"("aaa", "aaaa"d);
    assert(result < 0);
    result = cmp!"a > b"("aaaa", "aaa"d);
    assert(result > 0);
    result = cmp!"a > b"("aaa", "aaa"d);
    assert(result == 0);
    result = cmp("aaa"d, "aaa"d);
    assert(result == 0);
    result = cmp!"a > b"(cast(int[])[], cast(int[])[]);
    assert(result == 0);
    result = cmp!"a > b"([1, 2, 3], [1, 2, 3]);
    assert(result == 0);
    result = cmp!"a > b"([1, 3, 2], [1, 2, 3]);
    assert(result < 0);
    result = cmp!"a > b"([1, 2, 3], [1L, 2, 3, 4]);
    assert(result < 0);
    result = cmp!"a > b"([1L, 2, 3], [1, 2]);
    assert(result > 0);
}

@safe @nogc unittest
{
    import std.algorithm.comparison;

    import std.algorithm.comparison : equal;
    import std.math.operations : isClose;

    int[4] a = [ 1, 2, 4, 3 ];
    assert(!equal(a[], a[1..$]));
    assert(equal(a[], a[]));
    assert(equal!((a, b) => a == b)(a[], a[]));

    // different types
    double[4] b = [ 1.0, 2, 4, 3];
    assert(!equal(a[], b[1..$]));
    assert(equal(a[], b[]));

    // predicated: ensure that two vectors are approximately equal
    double[4] c = [ 1.0000000005, 2, 4, 3];
    assert(equal!isClose(b[], c[]));
}

@safe unittest
{
    import std.algorithm.comparison;

    import std.algorithm.comparison : equal;
    import std.range : iota, chunks;
    assert(equal!(equal!equal)(
        [[[0, 1], [2, 3]], [[4, 5], [6, 7]]],
        iota(0, 8).chunks(2).chunks(2)
    ));
}

@safe unittest
{
    import std.algorithm.comparison;

    with(EditOp)
    {
        assert(levenshteinDistanceAndPath("foo", "foobar")[1] == [none, none, none, insert, insert, insert]);
        assert(levenshteinDistanceAndPath("banana", "fazan")[1] == [substitute, none, substitute, none, none, remove]);
    }
}

@safe unittest
{
    import std.algorithm.comparison;

    import std.algorithm.iteration : filter;
    import std.uni : toUpper;

    assert(levenshteinDistance("cat", "rat") == 1);
    assert(levenshteinDistance("parks", "spark") == 2);
    assert(levenshteinDistance("abcde", "abcde") == 0);
    assert(levenshteinDistance("abcde", "abCde") == 1);
    assert(levenshteinDistance("kitten", "sitting") == 3);
    assert(levenshteinDistance!((a, b) => toUpper(a) == toUpper(b))
        ("parks", "SPARK") == 2);
    assert(levenshteinDistance("parks".filter!"true", "spark".filter!"true") == 2);
    assert(levenshteinDistance("ID", "I♥D") == 1);
}

@safe unittest
{
    import std.algorithm.comparison;

    string a = "Saturday", b = "Sundays";
    auto p = levenshteinDistanceAndPath(a, b);
    assert(p[0] == 4);
    assert(equal(p[1], "nrrnsnnni"));
}

@safe @nogc unittest
{
    import std.algorithm.comparison;

    int a = 5;
    short b = 6;
    double c = 2;
    auto d = max(a, b);
    assert(is(typeof(d) == int));
    assert(d == 6);
    auto e = min(a, b, c);
    assert(is(typeof(e) == double));
    assert(e == 2);
}

@safe @nogc unittest
{
    import std.algorithm.comparison;

    int a = 5;
    short b = 6;
    double c = 2;
    auto d = min(a, b);
    static assert(is(typeof(d) == int));
    assert(d == 5);
    auto e = min(a, b, c);
    static assert(is(typeof(e) == double));
    assert(e == 2);
    ulong f = 0xffff_ffff_ffff;
    const uint g = min(f, 0xffff_0000);
    assert(g == 0xffff_0000);
    dchar h = 100;
    uint i = 101;
    static assert(is(typeof(min(h, i)) == dchar));
    static assert(is(typeof(min(i, h)) == uint));
    assert(min(h, i) == 100);
}

@safe @nogc unittest
{
    import std.algorithm.comparison;

    int a = -10;
    uint f = 10;
    static assert(is(typeof(min(a, f)) == int));
    assert(min(a, f) == -10);
}

@safe unittest
{
    import std.algorithm.comparison;

    import std.datetime;
    assert(min(Date(2012, 12, 21), Date(1982, 1, 4)) == Date(1982, 1, 4));
    assert(min(Date(1982, 1, 4), Date(2012, 12, 21)) == Date(1982, 1, 4));
    assert(min(Date(1982, 1, 4), Date.min) == Date.min);
    assert(min(Date.min, Date(1982, 1, 4)) == Date.min);
    assert(min(Date(1982, 1, 4), Date.max) == Date(1982, 1, 4));
    assert(min(Date.max, Date(1982, 1, 4)) == Date(1982, 1, 4));
    assert(min(Date.min, Date.max) == Date.min);
    assert(min(Date.max, Date.min) == Date.min);
}

@safe @nogc unittest
{
    import std.algorithm.comparison;

    int[6] x = [ 1,   5, 2, 7,   4, 3 ];
    double[6] y = [ 1.0, 5, 2, 7.3, 4, 8 ];
    auto m = mismatch(x[], y[]);
    assert(m[0] == x[3 .. $]);
    assert(m[1] == y[3 .. $]);

    auto m2 = mismatch(x[], y[], x[], y[]);
    assert(m2[0] == x[3 .. $]);
    assert(m2[1] == y[3 .. $]);
    assert(m2[2] == x[3 .. $]);
    assert(m2[3] == y[3 .. $]);
}

@safe unittest
{
    import std.algorithm.comparison;

    string res = 2.predSwitch!"a < b"(
        1, "less than 1",
        5, "less than 5",
        10, "less than 10",
        "greater or equal to 10");

    assert(res == "less than 5");

    //The arguments are lazy, which allows us to use predSwitch to create
    //recursive functions:
    int factorial(int n)
    {
        return n.predSwitch!"a <= b"(
            -1, {throw new Exception("Can not calculate n! for n < 0");}(),
            0, 1, // 0! = 1
            n * factorial(n - 1) // n! = n * (n - 1)! for n >= 0
            );
    }
    assert(factorial(3) == 6);

    //Void return expressions are allowed if they always throw:
    import std.exception : assertThrown;
    assertThrown!Exception(factorial(-9));
}

@safe nothrow pure unittest
{
    import std.algorithm.comparison;

    assert(isSameLength([1, 2, 3], [4, 5, 6]));
    assert(isSameLength([1, 2, 3], [4, 5, 6], [7, 8, 9]));
    assert(isSameLength([0.3, 90.4, 23.7, 119.2], [42.6, 23.6, 95.5, 6.3]));
    assert(isSameLength("abc", "xyz"));
    assert(isSameLength("abc", "xyz", [1, 2, 3]));

    int[] a;
    int[] b;
    assert(isSameLength(a, b));
    assert(isSameLength(a, b, a, a, b, b, b));

    assert(!isSameLength([1, 2, 3], [4, 5]));
    assert(!isSameLength([1, 2, 3], [4, 5, 6], [7, 8]));
    assert(!isSameLength([0.3, 90.4, 23.7], [42.6, 23.6, 95.5, 6.3]));
    assert(!isSameLength("abcd", "xyz"));
    assert(!isSameLength("abcd", "xyz", "123"));
    assert(!isSameLength("abcd", "xyz", "1234"));
}

@safe pure unittest
{
    import std.algorithm.comparison;

    import std.typecons : Yes;

    assert(isPermutation([1, 2, 3], [3, 2, 1]));
    assert(isPermutation([1.1, 2.3, 3.5], [2.3, 3.5, 1.1]));
    assert(isPermutation("abc", "bca"));

    assert(!isPermutation([1, 2], [3, 4]));
    assert(!isPermutation([1, 1, 2, 3], [1, 2, 2, 3]));
    assert(!isPermutation([1, 1], [1, 1, 1]));

    // Faster, but allocates GC handled memory
    assert(isPermutation!(Yes.allocateGC)([1.1, 2.3, 3.5], [2.3, 3.5, 1.1]));
    assert(!isPermutation!(Yes.allocateGC)([1, 2], [3, 4]));
}

@safe pure unittest
{
    import std.algorithm.comparison;

    const a = 1;
    const b = 2;
    auto ab = either(a, b);
    static assert(is(typeof(ab) == const(int)));
    assert(ab == a);

    auto c = 2;
    const d = 3;
    auto cd = either!(a => a == 3)(c, d); // use predicate
    static assert(is(typeof(cd) == int));
    assert(cd == d);

    auto e = 0;
    const f = 2;
    auto ef = either(e, f);
    static assert(is(typeof(ef) == int));
    assert(ef == f);
}

@safe pure unittest
{
    import std.algorithm.comparison;

    immutable p = 1;
    immutable q = 2;
    auto pq = either(p, q);
    static assert(is(typeof(pq) == immutable(int)));
    assert(pq == p);

    assert(either(3, 4) == 3);
    assert(either(0, 4) == 4);
    assert(either(0, 0) == 0);
    assert(either("", "a") == "");
}

@safe pure unittest
{
    import std.algorithm.comparison;

    string r = null;
    assert(either(r, "a") == "a");
    assert(either("a", "") == "a");

    immutable s = [1, 2];
    assert(either(s, s) == s);

    assert(either([0, 1], [1, 2]) == [0, 1]);
    assert(either([0, 1], [1]) == [0, 1]);
    assert(either("a", "b") == "a");

    static assert(!__traits(compiles, either(1, "a")));
    static assert(!__traits(compiles, either(1.0, "a")));
    static assert(!__traits(compiles, either('a', "a")));
}

