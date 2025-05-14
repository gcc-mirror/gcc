pure @safe nothrow @nogc unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    int[5] a = [ 1, 2, 3, 4, 5 ];
    int[5] b = [ 5, 4, 3, 2, 1 ];
    assert(equal(retro(a[]), b[]));
    assert(retro(a[]).source is a[]);
    assert(retro(retro(a[])) is a[]);
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    int[] a = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ];
    assert(equal(stride(a, 3), [ 1, 4, 7, 10 ][]));
    assert(stride(stride(a, 2), 3) == stride(a, 6));
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    int[] arr1 = [ 1, 2, 3, 4 ];
    int[] arr2 = [ 5, 6 ];
    int[] arr3 = [ 7 ];
    auto s = chain(arr1, arr2, arr3);
    assert(s.length == 7);
    assert(s[5] == 6);
    assert(equal(s, [1, 2, 3, 4, 5, 6, 7][]));
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.algorithm.sorting : sort;

    int[] arr1 = [5, 2, 8];
    int[] arr2 = [3, 7, 9];
    int[] arr3 = [1, 4, 6];

    // in-place sorting across all of the arrays
    auto s = arr1.chain(arr2, arr3).sort;

    assert(s.equal([1, 2, 3, 4, 5, 6, 7, 8, 9]));
    assert(arr1.equal([1, 2, 3]));
    assert(arr2.equal([4, 5, 6]));
    assert(arr3.equal([7, 8, 9]));
}

pure @safe nothrow unittest
{
    import std.range;

    import std.utf : byChar, byCodeUnit;

    auto s1 = "string one";
    auto s2 = "string two";
    // s1 and s2 front is dchar because of auto-decoding
    static assert(is(typeof(s1.front) == dchar) && is(typeof(s2.front) == dchar));

    auto r1 = s1.chain(s2);
    // chains of ranges of the same character type give that same type
    static assert(is(typeof(r1.front) == dchar));

    auto s3 = "string three".byCodeUnit;
    static assert(is(typeof(s3.front) == immutable char));
    auto r2 = s1.chain(s3);
    // chaining ranges of mixed character types gives `dchar`
    static assert(is(typeof(r2.front) == dchar));

    // use byChar on character ranges to correctly convert them to UTF-8
    auto r3 = s1.byChar.chain(s3);
    static assert(is(typeof(r3.front) == immutable char));
}

@safe nothrow pure @nogc unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter, map;

    auto data1 = only(1, 2, 3, 4).filter!(a => a != 3);
    auto data2 = only(5, 6, 7, 8).map!(a => a + 1);

    // choose() is primarily useful when you need to select one of two ranges
    // with different types at runtime.
    static assert(!is(typeof(data1) == typeof(data2)));

    auto chooseRange(bool pickFirst)
    {
        // The returned range is a common wrapper type that can be used for
        // returning or storing either range without running into a type error.
        return choose(pickFirst, data1, data2);

        // Simply returning the chosen range without using choose() does not
        // work, because map() and filter() return different types.
        //return pickFirst ? data1 : data2; // does not compile
    }

    auto result = chooseRange(true);
    assert(result.equal(only(1, 2, 4)));

    result = chooseRange(false);
    assert(result.equal(only(6, 7, 8, 9)));
}

@safe nothrow pure @nogc unittest
{
    import std.range;

    auto test()
    {
        import std.algorithm.comparison : equal;

        int[4] sarr1 = [1, 2, 3, 4];
        int[2] sarr2 = [5, 6];
        int[1] sarr3 = [7];
        auto arr1 = sarr1[];
        auto arr2 = sarr2[];
        auto arr3 = sarr3[];

        {
            auto s = chooseAmong(0, arr1, arr2, arr3);
            auto t = s.save;
            assert(s.length == 4);
            assert(s[2] == 3);
            s.popFront();
            assert(equal(t, only(1, 2, 3, 4)));
        }
        {
            auto s = chooseAmong(1, arr1, arr2, arr3);
            assert(s.length == 2);
            s.front = 8;
            assert(equal(s, only(8, 6)));
        }
        {
            auto s = chooseAmong(1, arr1, arr2, arr3);
            assert(s.length == 2);
            s[1] = 9;
            assert(equal(s, only(8, 9)));
        }
        {
            auto s = chooseAmong(1, arr2, arr1, arr3)[1 .. 3];
            assert(s.length == 2);
            assert(equal(s, only(2, 3)));
        }
        {
            auto s = chooseAmong(0, arr1, arr2, arr3);
            assert(s.length == 4);
            assert(s.back == 4);
            s.popBack();
            s.back = 5;
            assert(equal(s, only(1, 2, 5)));
            s.back = 3;
            assert(equal(s, only(1, 2, 3)));
        }
        {
            uint[5] foo = [1, 2, 3, 4, 5];
            uint[5] bar = [6, 7, 8, 9, 10];
            auto c = chooseAmong(1, foo[], bar[]);
            assert(c[3] == 9);
            c[3] = 42;
            assert(c[3] == 42);
            assert(c.moveFront() == 6);
            assert(c.moveBack() == 10);
            assert(c.moveAt(4) == 10);
        }
        {
            import std.range : cycle;
            auto s = chooseAmong(0, cycle(arr2), cycle(arr3));
            assert(isInfinite!(typeof(s)));
            assert(!s.empty);
            assert(s[100] == 8);
            assert(s[101] == 9);
            assert(s[0 .. 3].equal(only(8, 9, 8)));
        }
        return 0;
    }
    // works at runtime
    auto a = test();
    // and at compile time
    static b = test();
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    int[] a = [ 1, 2, 3 ];
    int[] b = [ 10, 20, 30, 40 ];
    auto r = roundRobin(a, b);
    assert(equal(r, [ 1, 10, 2, 20, 3, 30, 40 ]));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    auto interleave(R, E)(R range, E element)
    if ((isInputRange!R && hasLength!R) || isForwardRange!R)
    {
        static if (hasLength!R)
            immutable len = range.length;
        else
            immutable len = range.save.walkLength;

        return roundRobin(
            range,
            element.repeat(len - 1)
        );
    }

    assert(interleave([1, 2, 3], 0).equal([1, 0, 2, 0, 3]));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    int[] a = [ 1, 2, 3, 4, 5 ];
    assert(equal(radial(a), [ 3, 4, 2, 5, 1 ]));
    a = [ 1, 2, 3, 4 ];
    assert(equal(radial(a), [ 2, 3, 1, 4 ]));

    // If the left end is reached first, the remaining elements on the right
    // are concatenated in order:
    a = [ 0, 1, 2, 3, 4, 5 ];
    assert(equal(radial(a, 1), [ 1, 2, 0, 3, 4, 5 ]));

    // If the right end is reached first, the remaining elements on the left
    // are concatenated in reverse order:
    assert(equal(radial(a, 4), [ 4, 5, 3, 2, 1, 0 ]));
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    int[] arr1 = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    auto s = take(arr1, 5);
    assert(s.length == 5);
    assert(s[4] == 5);
    assert(equal(s, [ 1, 2, 3, 4, 5 ][]));
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    int[] arr2 = [ 1, 2, 3 ];
    auto t = take(arr2, 5);
    assert(t.length == 3);
    assert(equal(t, [ 1, 2, 3 ]));
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    auto a = [ 1, 2, 3, 4, 5 ];

    auto b = takeExactly(a, 3);
    assert(equal(b, [1, 2, 3]));
    static assert(is(typeof(b.length) == size_t));
    assert(b.length == 3);
    assert(b.front == 1);
    assert(b.back == 3);
}

pure @safe nothrow unittest
{
    import std.range;

    auto s = takeOne([42, 43, 44]);
    static assert(isRandomAccessRange!(typeof(s)));
    assert(s.length == 1);
    assert(!s.empty);
    assert(s.front == 42);
    s.front = 43;
    assert(s.front == 43);
    assert(s.back == 43);
    assert(s[0] == 43);
    s.popFront();
    assert(s.length == 0);
    assert(s.empty);
}

pure @safe nothrow @nogc unittest
{
    import std.range;

    auto range = takeNone!(int[])();
    assert(range.length == 0);
    assert(range.empty);
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.iteration : filter;
    assert(takeNone([42, 27, 19]).empty);
    assert(takeNone("dlang.org").empty);
    assert(takeNone(filter!"true"([42, 27, 19])).empty);
}

pure @safe nothrow unittest
{
    import std.range;

    // tail -c n
    assert([1, 2, 3].tail(1) == [3]);
    assert([1, 2, 3].tail(2) == [2, 3]);
    assert([1, 2, 3].tail(3) == [1, 2, 3]);
    assert([1, 2, 3].tail(4) == [1, 2, 3]);
    assert([1, 2, 3].tail(0).length == 0);

    // tail --lines=n
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : joiner;
    import std.exception : assumeWontThrow;
    import std.string : lineSplitter;
    assert("one\ntwo\nthree"
        .lineSplitter
        .tail(2)
        .joiner("\n")
        .equal("two\nthree")
        .assumeWontThrow);
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    assert([0, 2, 1, 5, 0, 3].drop(3) == [5, 0, 3]);
    assert("hello world".drop(6) == "world");
    assert("hello world".drop(50).empty);
    assert("hello world".take(6).drop(3).equal("lo "));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    assert([0, 2, 1, 5, 0, 3].dropBack(3) == [0, 2, 1]);
    assert("hello world".dropBack(6) == "hello");
    assert("hello world".dropBack(50).empty);
    assert("hello world".drop(4).dropBack(4).equal("o w"));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filterBidirectional;

    auto a = [1, 2, 3];
    assert(a.dropExactly(2) == [3]);
    assert(a.dropBackExactly(2) == [1]);

    string s = "日本語";
    assert(s.dropExactly(2) == "語");
    assert(s.dropBackExactly(2) == "日");

    auto bd = filterBidirectional!"true"([1, 2, 3]);
    assert(bd.dropExactly(2).equal([3]));
    assert(bd.dropBackExactly(2).equal([1]));
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filterBidirectional;
    import std.container.dlist : DList;

    auto dl = DList!int(9, 1, 2, 3, 9);
    assert(dl[].dropOne().dropBackOne().equal([1, 2, 3]));

    auto a = [1, 2, 3];
    assert(a.dropOne() == [2, 3]);
    assert(a.dropBackOne() == [1, 2]);

    string s = "日本語";
    import std.exception : assumeWontThrow;
    assert(assumeWontThrow(s.dropOne() == "本語"));
    assert(assumeWontThrow(s.dropBackOne() == "日本"));

    auto bd = filterBidirectional!"true"([1, 2, 3]);
    assert(bd.dropOne().equal([2, 3]));
    assert(bd.dropBackOne().equal([1, 2]));
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    assert(5.repeat().take(4).equal([5, 5, 5, 5]));
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    assert(5.repeat(4).equal([5, 5, 5, 5]));
}

@safe pure nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;

    int i = 1;
    auto powersOfTwo = generate!(() => i *= 2)().take(10);
    assert(equal(powersOfTwo, iota(1, 11).map!"2^^a"()));
}

@safe pure nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    //Returns a run-time delegate
    auto infiniteIota(T)(T low, T high)
    {
        T i = high;
        return (){if (i == high) i = low; return i++;};
    }
    //adapted as a range.
    assert(equal(generate(infiniteIota(1, 4)).take(10), [1, 2, 3, 1, 2, 3, 1, 2, 3, 1]));
}

@safe unittest
{
    import std.range;

    import std.format : format;
    import std.random : uniform;

    auto r = generate!(() => uniform(0, 6)).take(10);
    format("%(%s %)", r);
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.range : cycle, take;

    // Here we create an infinitive cyclic sequence from [1, 2]
    // (i.e. get here [1, 2, 1, 2, 1, 2 and so on]) then
    // take 5 elements of this sequence (so we have [1, 2, 1, 2, 1])
    // and compare them with the expected values for equality.
    assert(cycle([1, 2]).take(5).equal([ 1, 2, 1, 2, 1 ]));
}

@nogc nothrow pure @safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;

    // pairwise sum
    auto arr = only(0, 1, 2);
    auto part1 = zip(arr, arr.dropOne).map!"a[0] + a[1]";
    assert(part1.equal(only(1, 3)));
}

nothrow pure @safe unittest
{
    import std.range;

    import std.conv : to;

    int[] a = [ 1, 2, 3 ];
    string[] b = [ "a", "b", "c" ];
    string[] result;

    foreach (tup; zip(a, b))
    {
        result ~= tup[0].to!string ~ tup[1];
    }

    assert(result == [ "1a", "2b", "3c" ]);

    size_t idx = 0;
    // unpacking tuple elements with foreach
    foreach (e1, e2; zip(a, b))
    {
        assert(e1 == a[idx]);
        assert(e2 == b[idx]);
        ++idx;
    }
}

nothrow pure @safe unittest
{
    import std.range;

    import std.algorithm.sorting : sort;

    int[] a = [ 1, 2, 3 ];
    string[] b = [ "a", "c", "b" ];
    zip(a, b).sort!((t1, t2) => t1[0] > t2[0]);

    assert(a == [ 3, 2, 1 ]);
    // b is sorted according to a's sorting
    assert(b == [ "b", "c", "a" ]);
}

pure @safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;
    import std.range.primitives;
    import std.typecons : tuple;

    auto a = [1, 2, 3];
    auto b = [4, 5, 6, 7];

    auto shortest = zip(StoppingPolicy.shortest, a, b);
    assert(shortest.equal([
        tuple(1, 4),
        tuple(2, 5),
        tuple(3, 6)
    ]));

    auto longest = zip(StoppingPolicy.longest, a, b);
    assert(longest.equal([
        tuple(1, 4),
        tuple(2, 5),
        tuple(3, 6),
        tuple(0, 7)
    ]));

    auto same = zip(StoppingPolicy.requireSameLength, a, b);
    same.popFrontN(3);
    assertThrown!Exception(same.popFront);
}

pure @safe unittest
{
    import std.range;

    int[6] arr1 = [1,2,3,4,5,100];
    int[5] arr2 = [6,7,8,9,10];

    foreach (ref a, b; lockstep(arr1[], arr2[]))
    {
        a += b;
    }

    assert(arr1 == [7,9,11,13,15,100]);
}

pure @safe unittest
{
    import std.range;

    int[3] arr1 = [1,2,3];
    int[3] arr2 = [4,5,6];

    foreach (index, a, b; lockstep(arr1[], arr2[]))
    {
        assert(arr1[index] == a);
        assert(arr2[index] == b);
    }
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    // The Fibonacci numbers, using function in string form:
    // a[0] = 1, a[1] = 1, and compute a[n+1] = a[n-1] + a[n]
    auto fib = recurrence!("a[n-1] + a[n-2]")(1, 1);
    assert(fib.take(10).equal([1, 1, 2, 3, 5, 8, 13, 21, 34, 55]));

    // The factorials, using function in lambda form:
    auto fac = recurrence!((a,n) => a[n-1] * n)(1);
    assert(take(fac, 10).equal([
        1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880
    ]));

    // The triangular numbers, using function in explicit form:
    static size_t genTriangular(R)(R state, size_t n)
    {
        return state[n-1] + n;
    }
    auto tri = recurrence!genTriangular(0);
    assert(take(tri, 10).equal([0, 1, 3, 6, 10, 15, 21, 28, 36, 45]));
}

pure @safe nothrow @nogc unittest
{
    import std.range;

    auto odds = sequence!("a[0] + n * a[1]")(1, 2);
    assert(odds.front == 1);
    odds.popFront();
    assert(odds.front == 3);
    odds.popFront();
    assert(odds.front == 5);
}

pure @safe nothrow @nogc unittest
{
    import std.range;

    auto tri = sequence!((a,n) => n*(n+1)/2)();

    // Note random access
    assert(tri[0] == 0);
    assert(tri[3] == 6);
    assert(tri[1] == 1);
    assert(tri[4] == 10);
    assert(tri[2] == 3);
}

@safe nothrow @nogc unittest
{
    import std.range;

    import std.math.exponential : pow;
    import std.math.rounding : round;
    import std.math.algebraic : sqrt;
    static ulong computeFib(S)(S state, size_t n)
    {
        // Binet's formula
        return cast(ulong)(round((pow(state[0], n+1) - pow(state[1], n+1)) /
                                 state[2]));
    }
    auto fib = sequence!computeFib(
        (1.0 + sqrt(5.0)) / 2.0,    // Golden Ratio
        (1.0 - sqrt(5.0)) / 2.0,    // Conjugate of Golden Ratio
        sqrt(5.0));

    // Note random access with [] operator
    assert(fib[1] == 1);
    assert(fib[4] == 5);
    assert(fib[3] == 3);
    assert(fib[2] == 2);
    assert(fib[9] == 55);
}

pure @safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.math.operations : isClose;

    auto r = iota(0, 10, 1);
    assert(equal(r, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    assert(equal(r, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    assert(3 in r);
    assert(r.contains(3)); //Same as above
    assert(!(10 in r));
    assert(!(-8 in r));
    r = iota(0, 11, 3);
    assert(equal(r, [0, 3, 6, 9]));
    assert(r[2] == 6);
    assert(!(2 in r));
    auto rf = iota(0.0, 0.5, 0.1);
    assert(isClose(rf, [0.0, 0.1, 0.2, 0.3, 0.4]));
}

@safe pure unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;

    auto arr = [[1, 2], [3, 4, 5]];

    auto r1 = arr.frontTransversal!(TransverseOptions.assumeJagged);
    assert(r1.equal([1, 3]));

    // throws on construction
    assertThrown!Exception(arr.frontTransversal!(TransverseOptions.enforceNotJagged));

    auto r2 = arr.frontTransversal!(TransverseOptions.assumeNotJagged);
    assert(r2.equal([1, 3]));

    // either assuming or checking for equal lengths makes
    // the result a random access range
    assert(r2[0] == 1);
    static assert(!__traits(compiles, r1[0]));
}

pure @safe nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    int[][] x = new int[][2];
    x[0] = [1, 2];
    x[1] = [3, 4];
    auto ror = frontTransversal(x);
    assert(equal(ror, [ 1, 3 ][]));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    int[][] x = new int[][2];
    x[0] = [1, 2];
    x[1] = [3, 4];
    auto ror = transversal(x, 1);
    assert(equal(ror, [ 2, 4 ]));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    int[][] y = [[1, 2, 3], [4, 5, 6]];
    auto z = y.front.walkLength.iota.map!(i => transversal(y, i));
    assert(equal!equal(z, [[1, 4], [2, 5], [3, 6]]));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    int[][] ror = [
        [1, 2, 3],
        [4, 5, 6]
    ];
    auto xp = transposed(ror);
    assert(equal!"a.equal(b)"(xp, [
        [1, 4],
        [2, 5],
        [3, 6]
    ]));
}

@safe unittest
{
    import std.range;

    int[][] x = new int[][2];
    x[0] = [1, 2];
    x[1] = [3, 4];
    auto tr = transposed(x);
    int[][] witness = [ [ 1, 3 ], [ 2, 4 ] ];
    uint i;

    foreach (e; tr)
    {
        assert(array(e) == witness[i++]);
    }
}

@safe unittest
{
    import std.range;

            auto ind = indexed([1, 2, 3, 4, 5], [1, 3, 4]);
            assert(ind.physicalIndex(0) == 1);
        
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    auto source = [1, 2, 3, 4, 5];
    auto indices = [4, 3, 1, 2, 0, 4];
    auto ind = indexed(source, indices);
    assert(equal(ind, [5, 4, 2, 3, 1, 5]));
    assert(equal(retro(ind), [5, 1, 3, 2, 4, 5]));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    auto source = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto chunks = chunks(source, 4);
    assert(chunks[0] == [1, 2, 3, 4]);
    assert(chunks[1] == [5, 6, 7, 8]);
    assert(chunks[2] == [9, 10]);
    assert(chunks.back == chunks[2]);
    assert(chunks.front == chunks[0]);
    assert(chunks.length == 3);
    assert(equal(retro(array(chunks)), array(retro(chunks))));
}

@system unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    int i;

    // The generator doesn't save state, so it cannot be a forward range.
    auto inputRange = generate!(() => ++i).take(10);

    // We can still process it in chunks, but it will be single-pass only.
    auto chunked = inputRange.chunks(2);

    assert(chunked.front.equal([1, 2]));
    assert(chunked.front.empty); // Iterating the chunk has consumed it
    chunked.popFront;
    assert(chunked.front.equal([3, 4]));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    auto source = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto chunks = evenChunks(source, 3);
    assert(chunks[0] == [1, 2, 3, 4]);
    assert(chunks[1] == [5, 6, 7]);
    assert(chunks[2] == [8, 9, 10]);
}

@safe pure nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    assert([0, 1, 2, 3].slide(2).equal!equal(
        [[0, 1], [1, 2], [2, 3]]
    ));

    assert(5.iota.slide(3).equal!equal(
        [[0, 1, 2], [1, 2, 3], [2, 3, 4]]
    ));
}

@safe pure nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    assert(6.iota.slide(1, 2).equal!equal(
        [[0], [2], [4]]
    ));

    assert(6.iota.slide(2, 4).equal!equal(
        [[0, 1], [4, 5]]
    ));

    assert(iota(7).slide(2, 2).equal!equal(
        [[0, 1], [2, 3], [4, 5], [6]]
    ));

    assert(iota(12).slide(2, 4).equal!equal(
        [[0, 1], [4, 5], [8, 9]]
    ));
}

@safe pure nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    assert(3.iota.slide!(No.withPartial)(4).empty);
    assert(3.iota.slide!(Yes.withPartial)(4).equal!equal(
        [[0, 1, 2]]
    ));
}

@safe pure nothrow unittest
{
    import std.range;

    import std.algorithm.iteration : each;

    int[dstring] d;
    "AGAGA"d.slide!(Yes.withPartial)(2).each!(a => d[a]++);
    assert(d == ["AG"d: 2, "GA"d: 2]);
}

@safe pure nothrow unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    assert(5.iota.slide!(Yes.withPartial)(3, 4).equal!equal([[0, 1, 2], [4]]));
    assert(6.iota.slide!(Yes.withPartial)(3, 4).equal!equal([[0, 1, 2], [4, 5]]));
    assert(7.iota.slide!(Yes.withPartial)(3, 4).equal!equal([[0, 1, 2], [4, 5, 6]]));

    assert(5.iota.slide!(No.withPartial)(3, 4).equal!equal([[0, 1, 2]]));
    assert(6.iota.slide!(No.withPartial)(3, 4).equal!equal([[0, 1, 2]]));
    assert(7.iota.slide!(No.withPartial)(3, 4).equal!equal([[0, 1, 2], [4, 5, 6]]));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter, joiner, map;
    import std.algorithm.searching : findSplitBefore;
    import std.uni : isUpper;

    assert(equal(only('♡'), "♡"));
    assert([1, 2, 3, 4].findSplitBefore(only(3))[0] == [1, 2]);

    assert(only("one", "two", "three").joiner(" ").equal("one two three"));

    string title = "The D Programming Language";
    assert(title
        .filter!isUpper // take the upper case letters
        .map!only       // make each letter its own range
        .joiner(".")    // join the ranges together lazily
        .equal("T.D.P.L"));
}

pure @safe nothrow unittest
{
    import std.range;

    import std.array : assocArray;
    import std.range : enumerate;

    bool[int] aa = true.repeat(3).enumerate(-1).assocArray();
    assert(aa[-1]);
    assert(aa[0]);
    assert(aa[1]);
}

@safe unittest
{
    import std.range;

    void func1(int a, int b);
    void func2(int a, float b);

    static assert(isTwoWayCompatible!(func1, int, int));
    static assert(isTwoWayCompatible!(func1, short, int));
    static assert(!isTwoWayCompatible!(func2, int, float));

    void func3(ref int a, ref int b);
    static assert( isTwoWayCompatible!(func3, int, int));
    static assert(!isTwoWayCompatible!(func3, short, int));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    auto a = assumeSorted([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
    auto p1 = a.upperBound!(SearchPolicy.binarySearch)(3);
    assert(p1.equal([4, 5, 6, 7, 8, 9]));

    auto p2 = a.lowerBound!(SearchPolicy.gallop)(4);
    assert(p2.equal([0, 1, 2, 3]));
}

@safe pure unittest
{
    import std.range;

    // create a SortedRange, that's checked strictly
    SortedRange!(int[],"a < b", SortedRangeOptions.checkStrictly)([ 1, 3, 5, 7, 9 ]);
}

@safe unittest
{
    import std.range;

        import std.algorithm.sorting : sort;
        int[3] data = [ 1, 2, 3 ];
        auto a = assumeSorted(data[]);
        assert(a == sort!"a < b"(data[]));
        int[] p = a.release();
        assert(p == [ 1, 2, 3 ]);
    
}

@safe unittest
{
    import std.range;

        import std.algorithm.comparison : equal;
        auto a = assumeSorted([ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]);
        auto p = a.lowerBound(4);
        assert(equal(p, [ 0, 1, 2, 3 ]));
    
}

@safe unittest
{
    import std.range;

        import std.algorithm.comparison : equal;
        auto a = assumeSorted([ 1, 2, 3, 3, 3, 4, 4, 5, 6 ]);
        auto p = a.upperBound(3);
        assert(equal(p, [4, 4, 5, 6]));
    
}

@safe unittest
{
    import std.range;

        import std.algorithm.comparison : equal;
        auto a = [ 1, 2, 3, 3, 3, 4, 4, 5, 6 ];
        auto r = a.assumeSorted.equalRange(3);
        assert(equal(r, [ 3, 3, 3 ]));
    
}

@safe unittest
{
    import std.range;

        import std.algorithm.comparison : equal;
        auto a = [ 1, 2, 3, 3, 3, 4, 4, 5, 6 ];
        auto r = assumeSorted(a).trisect(3);
        assert(equal(r[0], [ 1, 2 ]));
        assert(equal(r[1], [ 3, 3, 3 ]));
        assert(equal(r[2], [ 4, 4, 5, 6 ]));
    
}

@safe unittest
{
    import std.range;

    import std.algorithm.sorting : sort;
    auto a = [ 1, 2, 3, 42, 52, 64 ];
    auto r = assumeSorted(a);
    assert(r.contains(3));
    assert(!(32 in r));
    auto r1 = sort!"a > b"(a);
    assert(3 in r1);
    assert(!r1.contains(32));
    assert(r1.release() == [ 64, 52, 42, 3, 2, 1 ]);
}

@safe unittest
{
    import std.range;

    import std.algorithm.mutation : swap;
    auto a = [ 1, 2, 3, 42, 52, 64 ];
    auto r = assumeSorted(a);
    assert(r.contains(42));
    swap(a[3], a[5]);         // illegal to break sortedness of original range
    assert(!r.contains(42));  // passes although it shouldn't
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    static struct S { int i; }
    static bool byI(A, B)(A a, B b)
    {
        static if (is(A == S))
            return a.i < b;
        else
            return a < b.i;
    }
    auto r = assumeSorted!byI([S(1), S(2), S(3)]);
    auto lessThanTwo = r.lowerBound(2);
    assert(equal(lessThanTwo, [S(1)]));
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    int[] a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    auto p = assumeSorted(a);

    assert(equal(p.lowerBound(4), [0, 1, 2, 3]));
    assert(equal(p.lowerBound(5), [0, 1, 2, 3, 4]));
    assert(equal(p.lowerBound(6), [0, 1, 2, 3, 4, 5]));
    assert(equal(p.lowerBound(6.9), [0, 1, 2, 3, 4, 5, 6]));
}

@system unittest
{
    import std.range;

    import std.algorithm.searching : find;
    ubyte[] buffer = [1, 9, 45, 12, 22];
    auto found1 = find(buffer, 45);
    assert(found1 == [45, 12, 22]);
    assert(buffer == [1, 9, 45, 12, 22]);

    auto wrapped1 = refRange(&buffer);
    auto found2 = find(wrapped1, 45);
    assert(*found2.ptr == [45, 12, 22]);
    assert(buffer == [45, 12, 22]);

    auto found3 = find(wrapped1.save, 22);
    assert(*found3.ptr == [22]);
    assert(buffer == [45, 12, 22]);

    string str = "hello world";
    auto wrappedStr = refRange(&str);
    assert(str.front == 'h');
    str.popFrontN(5);
    assert(str == " world");
    assert(wrappedStr.front == ' ');
    assert(*wrappedStr.ptr == " world");
}

@system unittest
{
    import std.range;

    ubyte[] buffer1 = [1, 2, 3, 4, 5];
    ubyte[] buffer2 = [6, 7, 8, 9, 10];
    auto wrapped1 = refRange(&buffer1);
    auto wrapped2 = refRange(&buffer2);
    assert(wrapped1.ptr is &buffer1);
    assert(wrapped2.ptr is &buffer2);
    assert(wrapped1.ptr !is wrapped2.ptr);
    assert(buffer1 != buffer2);

    wrapped1 = wrapped2;

    //Everything points to the same stuff as before.
    assert(wrapped1.ptr is &buffer1);
    assert(wrapped2.ptr is &buffer2);
    assert(wrapped1.ptr !is wrapped2.ptr);

    //But buffer1 has changed due to the assignment.
    assert(buffer1 == [6, 7, 8, 9, 10]);
    assert(buffer2 == [6, 7, 8, 9, 10]);

    buffer2 = [11, 12, 13, 14, 15];

    //Everything points to the same stuff as before.
    assert(wrapped1.ptr is &buffer1);
    assert(wrapped2.ptr is &buffer2);
    assert(wrapped1.ptr !is wrapped2.ptr);

    //But buffer2 has changed due to the assignment.
    assert(buffer1 == [6, 7, 8, 9, 10]);
    assert(buffer2 == [11, 12, 13, 14, 15]);

    wrapped2 = null;

    //The pointer changed for wrapped2 but not wrapped1.
    assert(wrapped1.ptr is &buffer1);
    assert(wrapped2.ptr is null);
    assert(wrapped1.ptr !is wrapped2.ptr);

    //buffer2 is not affected by the assignment.
    assert(buffer1 == [6, 7, 8, 9, 10]);
    assert(buffer2 == [11, 12, 13, 14, 15]);
}

@safe pure unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.format : format;

    // 00000011 00001001
    ubyte[] arr = [3, 9];
    auto r = arr.bitwise;

    // iterate through it as with any other range
    assert(format("%(%d%)", r) == "1100000010010000");
    assert(format("%(%d%)", r.retro).equal("1100000010010000".retro));

    auto r2 = r[5 .. $];
    // set a bit
    r[2] = 1;
    assert(arr[0] == 7);
    assert(r[5] == r2[0]);
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.random : rndGen;

    auto rb = rndGen.bitwise;
    static assert(isInfinite!(typeof(rb)));

    auto rb2 = rndGen.bitwise;
    // Don't forget that structs are passed by value
    assert(rb.take(10).equal(rb2.take(10)));
}

@safe nothrow unittest
{
    import std.range;

    import std.algorithm.iteration : map;
    import std.algorithm.mutation : copy;
    [4, 5, 6].map!(x => x * 2).copy(nullSink); // data is discarded
}

@safe unittest
{
    import std.range;

    import std.csv : csvNextToken;

    string line = "a,b,c";

    // ignore the first column
    line.csvNextToken(nullSink, ',', '"');
    line.popFront;

    // look at the second column
    Appender!string app;
    line.csvNextToken(app, ',', '"');
    assert(app.data == "b");
}

@safe unittest
{
    import std.range;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter, map;

    // Sum values while copying
    int[] values = [1, 4, 9, 16, 25];
    int sum = 0;
    auto newValues = values.tee!(a => sum += a).array;
    assert(equal(newValues, values));
    assert(sum == 1 + 4 + 9 + 16 + 25);

    // Count values that pass the first filter
    int count = 0;
    auto newValues4 = values.filter!(a => a < 10)
                            .tee!(a => count++)
                            .map!(a => a + 1)
                            .filter!(a => a < 10);

    //Fine, equal also evaluates any lazy ranges passed to it.
    //count is not 3 until equal evaluates newValues4
    assert(equal(newValues4, [2, 5]));
    assert(count == 3);
}

@safe pure unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    assert([1, 2, 3, 4].padLeft(0, 6).equal([0, 0, 1, 2, 3, 4]));
    assert([1, 2, 3, 4].padLeft(0, 3).equal([1, 2, 3, 4]));

    assert("abc".padLeft('_', 6).equal("___abc"));
}

@safe pure unittest
{
    import std.range;

    import std.algorithm.comparison : equal;

    assert([1, 2, 3, 4].padRight(0, 6).equal([1, 2, 3, 4, 0, 0]));
    assert([1, 2, 3, 4].padRight(0, 4).equal([1, 2, 3, 4]));

    assert("abc".padRight('_', 6).equal("abc___"));
}

@safe unittest
{
    import std.range;

    import std.path : chainPath;
    import std.range : chain;

    void someLibraryMethod(R)(R argument)
    if (isSomeFiniteCharInputRange!R)
    {
        // implementation detail, would iterate over each character of argument
    }

    someLibraryMethod("simple strings work");
    someLibraryMethod(chain("chained", " ", "strings", " ", "work"));
    someLibraryMethod(chainPath("chained", "paths", "work"));
    // you can also use custom structs implementing a char range
}

