@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range, std.stdio;
    import std.typecons : tuple;

    ulong counter = 0;
    double fun(int x)
    {
        ++counter;
        // http://en.wikipedia.org/wiki/Quartic_function
        return ( (x + 4.0) * (x + 1.0) * (x - 1.0) * (x - 3.0) ) / 14.0 + 0.5;
    }
    // Without cache, with array (greedy)
    auto result1 = iota(-4, 5).map!(a =>tuple(a, fun(a)))()
                             .filter!(a => a[1] < 0)()
                             .map!(a => a[0])()
                             .array();

    // the values of x that have a negative y are:
    assert(equal(result1, [-3, -2, 2]));

    // Check how many times fun was evaluated.
    // As many times as the number of items in both source and result.
    assert(counter == iota(-4, 5).length + result1.length);

    counter = 0;
    // Without array, with cache (lazy)
    auto result2 = iota(-4, 5).map!(a =>tuple(a, fun(a)))()
                             .cache()
                             .filter!(a => a[1] < 0)()
                             .map!(a => a[0])();

    // the values of x that have a negative y are:
    assert(equal(result2, [-3, -2, 2]));

    // Check how many times fun was evaluated.
    // Only as many times as the number of items in source.
    assert(counter == iota(-4, 5).length);
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range;
    int i = 0;

    auto r = iota(0, 4).tee!((a){i = a;}, No.pipeOnPop);
    auto r1 = r.take(3).cache();
    auto r2 = r.cache().take(3);

    assert(equal(r1, [0, 1, 2]));
    assert(i == 2); //The last "seen" element was 2. The data in cache has been cleared.

    assert(equal(r2, [0, 1, 2]));
    assert(i == 3); //cache has accessed 3. It is still stored internally by cache.
}

@safe @nogc unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range : chain, only;
    auto squares =
        chain(only(1, 2, 3, 4), only(5, 6)).map!(a => a * a);
    assert(equal(squares, only(1, 4, 9, 16, 25, 36)));
}

@safe unittest
{
    import std.algorithm.iteration;

    auto sums = [2, 4, 6, 8];
    auto products = [1, 4, 9, 16];

    size_t i = 0;
    foreach (result; [ 1, 2, 3, 4 ].map!("a + a", "a * a"))
    {
        assert(result[0] == sums[i]);
        assert(result[1] == products[i]);
        ++i;
    }
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.conv : to;

    alias stringize = map!(to!string);
    assert(equal(stringize([ 1, 2, 3, 4 ]), [ "1", "2", "3", "4" ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.range : iota;
    import std.typecons : No;

    int[] arr;
    iota(5).each!(n => arr ~= n);
    assert(arr == [0, 1, 2, 3, 4]);

    // stop iterating early
    iota(5).each!((n) { arr ~= n; return No.each; });
    assert(arr == [0, 1, 2, 3, 4, 0]);

    // If the range supports it, the value can be mutated in place
    arr.each!((ref n) => n++);
    assert(arr == [1, 2, 3, 4, 5, 1]);

    arr.each!"a++";
    assert(arr == [2, 3, 4, 5, 6, 2]);

    auto m = arr.map!(n => n);
    // by-ref lambdas are not allowed for non-ref ranges
    static assert(!__traits(compiles, m.each!((ref n) => n++)));

    // The default predicate consumes the range
    (&m).each();
    assert(m.empty);
}

@safe unittest
{
    import std.algorithm.iteration;

    auto arr = new size_t[4];

    arr.each!"a=i"();
    assert(arr == [0, 1, 2, 3]);

    arr.each!((i, ref e) => e = i * 2);
    assert(arr == [0, 2, 4, 6]);
}

@system unittest
{
    import std.algorithm.iteration;

    static class S
    {
        int x;
        int opApply(scope int delegate(ref int _x) dg) { return dg(x); }
    }

    auto s = new S;
    s.each!"a++";
    assert(s.x == 1);
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.math.operations : isClose;
    import std.range;

    int[] arr = [ 1, 2, 3, 4, 5 ];

    // Filter below 3
    auto small = filter!(a => a < 3)(arr);
    assert(equal(small, [ 1, 2 ]));

    // Filter again, but with Uniform Function Call Syntax (UFCS)
    auto sum = arr.filter!(a => a < 3);
    assert(equal(sum, [ 1, 2 ]));

    // In combination with chain() to span multiple ranges
    int[] a = [ 3, -2, 400 ];
    int[] b = [ 100, -101, 102 ];
    auto r = chain(a, b).filter!(a => a > 0);
    assert(equal(r, [ 3, 400, 100, 102 ]));

    // Mixing convertible types is fair game, too
    double[] c = [ 2.5, 3.0 ];
    auto r1 = chain(c, a, b).filter!(a => cast(int) a != a);
    assert(isClose(r1, [ 2.5 ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range;

    int[] arr = [ 1, 2, 3, 4, 5 ];
    auto small = filterBidirectional!("a < 3")(arr);
    static assert(isBidirectionalRange!(typeof(small)));
    assert(small.back == 2);
    assert(equal(small, [ 1, 2 ]));
    assert(equal(retro(small), [ 2, 1 ]));
    // In combination with chain() to span multiple ranges
    int[] a = [ 3, -2, 400 ];
    int[] b = [ 100, -101, 102 ];
    auto r = filterBidirectional!("a > 0")(chain(a, b));
    assert(r.back == 102);
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.typecons : tuple, Tuple;

    int[] arr = [ 1, 2, 2, 2, 2, 3, 4, 4, 4, 5 ];
    assert(equal(group(arr), [ tuple(1, 1u), tuple(2, 4u), tuple(3, 1u),
        tuple(4, 3u), tuple(5, 1u) ][]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.sorting : sort;
    import std.array : assocArray;

    uint[string] result;
    auto range = ["a", "b", "a", "c", "b", "c", "c", "d", "e"];
    result = range.sort!((a, b) => a < b)
        .group
        .assocArray;

    assert(result == ["a": 2U, "b": 2U, "c": 3U, "d": 1U, "e": 1U]);
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;

    // Grouping by particular attribute of each element:
    auto data = [
        [1, 1],
        [1, 2],
        [2, 2],
        [2, 3]
    ];

    auto r1 = data.chunkBy!((a,b) => a[0] == b[0]);
    assert(r1.equal!equal([
        [[1, 1], [1, 2]],
        [[2, 2], [2, 3]]
    ]));

    auto r2 = data.chunkBy!((a,b) => a[1] == b[1]);
    assert(r2.equal!equal([
        [[1, 1]],
        [[1, 2], [2, 2]],
        [[2, 3]]
    ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range.primitives;
    import std.typecons : tuple;

    // Grouping by particular attribute of each element:
    auto range =
    [
        [1, 1],
        [1, 1],
        [1, 2],
        [2, 2],
        [2, 3],
        [2, 3],
        [3, 3]
    ];

    auto byX = chunkBy!(a => a[0])(range);
    auto expected1 =
    [
        tuple(1, [[1, 1], [1, 1], [1, 2]]),
        tuple(2, [[2, 2], [2, 3], [2, 3]]),
        tuple(3, [[3, 3]])
    ];
    foreach (e; byX)
    {
        assert(!expected1.empty);
        assert(e[0] == expected1.front[0]);
        assert(e[1].equal(expected1.front[1]));
        expected1.popFront();
    }

    auto byY = chunkBy!(a => a[1])(range);
    auto expected2 =
    [
        tuple(1, [[1, 1], [1, 1]]),
        tuple(2, [[1, 2], [2, 2]]),
        tuple(3, [[2, 3], [2, 3], [3, 3]])
    ];
    foreach (e; byY)
    {
        assert(!expected2.empty);
        assert(e[0] == expected2.front[0]);
        assert(e[1].equal(expected2.front[1]));
        expected2.popFront();
    }
}

nothrow pure @safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range : dropExactly;
    auto source = [4, 3, 2, 11, 0, -3, -3, 5, 3, 0];

    auto result1 = source.splitWhen!((a,b) => a <= b);
    assert(result1.save.equal!equal([
        [4, 3, 2],
        [11, 0, -3],
        [-3],
        [5, 3, 0]
    ]));

    //splitWhen, like chunkBy, is currently a reference range (this may change
    //in future). Remember to call `save` when appropriate.
    auto result2 = result1.dropExactly(2);
    assert(result1.save.equal!equal([
        [-3],
        [5, 3, 0]
    ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.conv : text;

    assert(["abc", "def"].joiner.equal("abcdef"));
    assert(["Mary", "has", "a", "little", "lamb"]
        .joiner("...")
        .equal("Mary...has...a...little...lamb"));
    assert(["", "abc"].joiner("xyz").equal("xyzabc"));
    assert([""].joiner("xyz").equal(""));
    assert(["", ""].joiner("xyz").equal("xyz"));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range : repeat;

    assert([""].joiner.equal(""));
    assert(["", ""].joiner.equal(""));
    assert(["", "abc"].joiner.equal("abc"));
    assert(["abc", ""].joiner.equal("abc"));
    assert(["abc", "def"].joiner.equal("abcdef"));
    assert(["Mary", "has", "a", "little", "lamb"].joiner.equal("Maryhasalittlelamb"));
    assert("abc".repeat(3).joiner.equal("abcabcabc"));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    auto a = [ [1, 2, 3], [42, 43] ];
    auto j = joiner(a);
    j.front = 44;
    assert(a == [ [44, 2, 3], [42, 43] ]);
    assert(equal(j, [44, 2, 3, 42, 43]));
}

@safe pure unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range : chain, cycle, iota, only, retro, take, zip;
    import std.format : format;

    static immutable number = "12345678";
    static immutable delimiter = ",";
    auto formatted = number.retro
        .zip(3.iota.cycle.take(number.length))
        .map!(z => chain(z[0].only, z[1] == 2 ? delimiter : null))
        .joiner
        .retro;
    static immutable expected = "12,345,678";
    assert(formatted.equal(expected));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range : retro;

    auto a = [[1, 2, 3], [4, 5]];
    auto j = a.joiner;
    j.back = 44;
    assert(a == [[1, 2, 3], [4, 44]]);
    assert(equal(j.retro, [44, 4, 3, 2, 1]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : max, min;
    import std.math.operations : isClose;
    import std.range;

    int[] arr = [ 1, 2, 3, 4, 5 ];
    // Sum all elements
    auto sum = reduce!((a,b) => a + b)(0, arr);
    assert(sum == 15);

    // Sum again, using a string predicate with "a" and "b"
    sum = reduce!"a + b"(0, arr);
    assert(sum == 15);

    // Compute the maximum of all elements
    auto largest = reduce!(max)(arr);
    assert(largest == 5);

    // Max again, but with Uniform Function Call Syntax (UFCS)
    largest = arr.reduce!(max);
    assert(largest == 5);

    // Compute the number of odd elements
    auto odds = reduce!((a,b) => a + (b & 1))(0, arr);
    assert(odds == 3);

    // Compute the sum of squares
    auto ssquares = reduce!((a,b) => a + b * b)(0, arr);
    assert(ssquares == 55);

    // Chain multiple ranges into seed
    int[] a = [ 3, 4 ];
    int[] b = [ 100 ];
    auto r = reduce!("a + b")(chain(a, b));
    assert(r == 107);

    // Mixing convertible types is fair game, too
    double[] c = [ 2.5, 3.0 ];
    auto r1 = reduce!("a + b")(chain(a, b, c));
    assert(isClose(r1, 112.5));

    // To minimize nesting of parentheses, Uniform Function Call Syntax can be used
    auto r2 = chain(a, b, c).reduce!("a + b");
    assert(isClose(r2, 112.5));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : max, min;
    import std.math.operations : isClose;
    import std.math.algebraic : sqrt;
    import std.typecons : tuple, Tuple;

    double[] a = [ 3.0, 4, 7, 11, 3, 2, 5 ];
    // Compute minimum and maximum in one pass
    auto r = reduce!(min, max)(a);
    // The type of r is Tuple!(int, int)
    assert(isClose(r[0], 2));  // minimum
    assert(isClose(r[1], 11)); // maximum

    // Compute sum and sum of squares in one pass
    r = reduce!("a + b", "a + b * b")(tuple(0.0, 0.0), a);
    assert(isClose(r[0], 35));  // sum
    assert(isClose(r[1], 233)); // sum of squares
    // Compute average and standard deviation from the above
    auto avg = r[0] / a.length;
    assert(avg == 5);
    auto stdev = sqrt(r[1] / a.length - avg * avg);
    assert(cast(int) stdev == 2);
}

@safe pure unittest
{
    import std.algorithm.iteration;

    immutable arr = [1, 2, 3, 4, 5];

    // Sum all elements
    assert(arr.fold!((a, e) => a + e) == 15);

    // Sum all elements with explicit seed
    assert(arr.fold!((a, e) => a + e)(6) == 21);

    import std.algorithm.comparison : min, max;
    import std.typecons : tuple;

    // Compute minimum and maximum at the same time
    assert(arr.fold!(min, max) == tuple(1, 5));

    // Compute minimum and maximum at the same time with seeds
    assert(arr.fold!(min, max)(0, 7) == tuple(0, 7));

    // Can be used in a UFCS chain
    assert(arr.map!(a => a + 1).fold!((a, e) => a + e) == 20);

    // Return the last element of any range
    assert(arr.fold!((a, e) => e) == 5);
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : max, min;
    import std.array : array;
    import std.math.operations : isClose;
    import std.range : chain;

    int[] arr = [1, 2, 3, 4, 5];
    // Partial sum of all elements
    auto sum = cumulativeFold!((a, b) => a + b)(arr, 0);
    assert(sum.array == [1, 3, 6, 10, 15]);

    // Partial sum again, using a string predicate with "a" and "b"
    auto sum2 = cumulativeFold!"a + b"(arr, 0);
    assert(sum2.array == [1, 3, 6, 10, 15]);

    // Compute the partial maximum of all elements
    auto largest = cumulativeFold!max(arr);
    assert(largest.array == [1, 2, 3, 4, 5]);

    // Partial max again, but with Uniform Function Call Syntax (UFCS)
    largest = arr.cumulativeFold!max;
    assert(largest.array == [1, 2, 3, 4, 5]);

    // Partial count of odd elements
    auto odds = arr.cumulativeFold!((a, b) => a + (b & 1))(0);
    assert(odds.array == [1, 1, 2, 2, 3]);

    // Compute the partial sum of squares
    auto ssquares = arr.cumulativeFold!((a, b) => a + b * b)(0);
    assert(ssquares.array == [1, 5, 14, 30, 55]);

    // Chain multiple ranges into seed
    int[] a = [3, 4];
    int[] b = [100];
    auto r = cumulativeFold!"a + b"(chain(a, b));
    assert(r.array == [3, 7, 107]);

    // Mixing convertible types is fair game, too
    double[] c = [2.5, 3.0];
    auto r1 = cumulativeFold!"a + b"(chain(a, b, c));
    assert(isClose(r1, [3, 7, 107, 109.5, 112.5]));

    // To minimize nesting of parentheses, Uniform Function Call Syntax can be used
    auto r2 = chain(a, b, c).cumulativeFold!"a + b";
    assert(isClose(r2, [3, 7, 107, 109.5, 112.5]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : max, min;
    import std.algorithm.iteration : map;
    import std.math.operations : isClose;
    import std.typecons : tuple;

    double[] a = [3.0, 4, 7, 11, 3, 2, 5];
    // Compute minimum and maximum in one pass
    auto r = a.cumulativeFold!(min, max);
    // The type of r is Tuple!(int, int)
    assert(isClose(r.map!"a[0]", [3, 3, 3, 3, 3, 2, 2]));     // minimum
    assert(isClose(r.map!"a[1]", [3, 4, 7, 11, 11, 11, 11])); // maximum

    // Compute sum and sum of squares in one pass
    auto r2 = a.cumulativeFold!("a + b", "a + b * b")(tuple(0.0, 0.0));
    assert(isClose(r2.map!"a[0]", [3, 7, 14, 25, 28, 30, 35]));      // sum
    assert(isClose(r2.map!"a[1]", [9, 25, 74, 195, 204, 208, 233])); // sum of squares
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;

    assert("a|bc|def".splitter('|').equal([ "a", "bc", "def" ]));

    int[] a = [1, 0, 2, 3, 0, 4, 5, 6];
    int[][] w = [ [1], [2, 3], [4, 5, 6] ];
    assert(a.splitter(0).equal(w));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.typecons : Yes;

    assert("a|bc|def".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "a", "|", "bc", "|", "def" ]));

    int[] a = [1, 0, 2, 3, 0, 4, 5, 6];
    int[][] w = [ [1], [0], [2, 3], [0], [4, 5, 6] ];
    assert(a.splitter!("a == b", Yes.keepSeparators)(0).equal(w));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;

    assert("|ab|".splitter('|').equal([ "", "ab", "" ]));
    assert("ab".splitter('|').equal([ "ab" ]));

    assert("a|b||c".splitter('|').equal([ "a", "b", "", "c" ]));
    assert("hello  world".splitter(' ').equal([ "hello", "", "world" ]));

    auto a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    auto w = [ [1, 2], [], [3], [4, 5], [] ];
    assert(a.splitter(0).equal(w));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.typecons : Yes;

    assert("|ab|".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "", "|", "ab", "|", "" ]));
    assert("ab".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "ab" ]));

    assert("a|b||c".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "a", "|", "b", "|", "", "|", "c" ]));
    assert("hello  world".splitter!("a == b", Yes.keepSeparators)(' ')
        .equal([ "hello", " ", "", " ", "world" ]));

    auto a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    auto w = [ [1, 2], [0], [], [0], [3], [0], [4, 5], [0], [] ];
    assert(a.splitter!("a == b", Yes.keepSeparators)(0).equal(w));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range : empty;

    assert("".splitter('|').empty);
    assert("|".splitter('|').equal([ "", "" ]));
    assert("||".splitter('|').equal([ "", "", "" ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.typecons : Yes;
    import std.range : empty;

    assert("".splitter!("a == b", Yes.keepSeparators)('|').empty);
    assert("|".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "", "|", "" ]));
    assert("||".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "", "|", "", "|", "" ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;

    assert("a=>bc=>def".splitter("=>").equal([ "a", "bc", "def" ]));
    assert("a|b||c".splitter("||").equal([ "a|b", "c" ]));
    assert("hello  world".splitter("  ").equal([ "hello", "world" ]));

    int[] a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    int[][] w = [ [1, 2], [3, 0, 4, 5, 0] ];
    assert(a.splitter([0, 0]).equal(w));

    a = [ 0, 0 ];
    assert(a.splitter([0, 0]).equal([ (int[]).init, (int[]).init ]));

    a = [ 0, 0, 1 ];
    assert(a.splitter([0, 0]).equal([ [], [1] ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.typecons : Yes;

    assert("a=>bc=>def".splitter!("a == b", Yes.keepSeparators)("=>")
        .equal([ "a", "=>", "bc", "=>", "def" ]));
    assert("a|b||c".splitter!("a == b", Yes.keepSeparators)("||")
        .equal([ "a|b", "||", "c" ]));
    assert("hello  world".splitter!("a == b", Yes.keepSeparators)("  ")
        .equal([ "hello", "  ",  "world" ]));

    int[] a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    int[][] w = [ [1, 2], [0, 0], [3, 0, 4, 5, 0] ];
    assert(a.splitter!("a == b", Yes.keepSeparators)([0, 0]).equal(w));

    a = [ 0, 0 ];
    assert(a.splitter!("a == b", Yes.keepSeparators)([0, 0])
        .equal([ (int[]).init, [0, 0], (int[]).init ]));

    a = [ 0, 0, 1 ];
    assert(a.splitter!("a == b", Yes.keepSeparators)([0, 0])
        .equal([ [], [0, 0], [1] ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.ascii : toLower;

    assert("abXcdxef".splitter!"a.toLower == b"('x').equal(
                 [ "ab", "cd", "ef" ]));

    auto w = [ [0], [1], [2] ];
    assert(w.splitter!"a.front == b"(1).equal([ [[0]], [[2]] ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.typecons : Yes;
    import std.ascii : toLower;

    assert("abXcdxef".splitter!("a.toLower == b", Yes.keepSeparators)('x')
        .equal([ "ab", "X", "cd", "x", "ef" ]));

    auto w = [ [0], [1], [2] ];
    assert(w.splitter!("a.front == b", Yes.keepSeparators)(1)
        .equal([ [[0]], [[1]], [[2]] ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range.primitives : front;

    assert(equal(splitter!(a => a == '|')("a|bc|def"), [ "a", "bc", "def" ]));
    assert(equal(splitter!(a => a == ' ')("hello  world"), [ "hello", "", "world" ]));

    int[] a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    int[][] w = [ [1, 2], [], [3], [4, 5], [] ];
    assert(equal(splitter!(a => a == 0)(a), w));

    a = [ 0 ];
    assert(equal(splitter!(a => a == 0)(a), [ (int[]).init, (int[]).init ]));

    a = [ 0, 1 ];
    assert(equal(splitter!(a => a == 0)(a), [ [], [1] ]));

    w = [ [0], [1], [2] ];
    assert(equal(splitter!(a => a.front == 1)(w), [ [[0]], [[2]] ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;

    assert("|ab|".splitter('|').equal([ "", "ab", "" ]));
    assert("ab".splitter('|').equal([ "ab" ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.typecons : Yes;

    assert("|ab|".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "", "|", "ab", "|", "" ]));
    assert("ab".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "ab" ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range : retro;
    assert("a|bc|def".splitter('|').retro.equal([ "def", "bc", "a" ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.typecons : Yes;
    import std.range : retro;
    assert("a|bc|def".splitter!("a == b", Yes.keepSeparators)('|')
        .retro.equal([ "def", "|", "bc", "|", "a" ]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.ascii : isWhite;
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : splitter;

    string str = "Hello World!";
    assert(str.splitter!(isWhite).equal(["Hello", "World!"]));
}

@safe pure unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    auto a = " a     bcd   ef gh ";
    assert(equal(splitter(a), ["a", "bcd", "ef", "gh"][]));
}

@safe pure unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;

    // substitute single elements
    assert("do_it".substitute('_', ' ').equal("do it"));

    // substitute multiple, single elements
    assert("do_it".substitute('_', ' ',
                               'd', 'g',
                               'i', 't',
                               't', 'o')
                  .equal("go to"));

    // substitute subranges
    assert("do_it".substitute("_", " ",
                              "do", "done")
                  .equal("done it"));

    // substitution works for any ElementType
    int[] x = [1, 2, 3];
    auto y = x.substitute(1, 0.1);
    assert(y.equal([0.1, 2, 3]));
    static assert(is(typeof(y.front) == double));

    import std.range : retro;
    assert([1, 2, 3].substitute(1, 0.1).retro.equal([3, 2, 0.1]));
}

@safe pure unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;

    // substitute subranges of a range
    assert("apple_tree".substitute!("apple", "banana",
                                    "tree", "shrub").equal("banana_shrub"));

    // substitute subranges of a range
    assert("apple_tree".substitute!('a', 'b',
                                    't', 'f').equal("bpple_free"));

    // substitute values
    assert('a'.substitute!('a', 'b', 't', 'f') == 'b');
}

@safe pure unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range.primitives : ElementType;

    int[3] x = [1, 2, 3];
    auto y = x[].substitute(1, 0.1)
                .substitute(0.1, 0.2);
    static assert(is(typeof(y.front) == double));
    assert(y.equal([0.2, 2, 3]));

    auto z = "42".substitute('2', '3')
                 .substitute('3', '1');
    static assert(is(ElementType!(typeof(z)) == dchar));
    assert(equal(z, "41"));
}

@safe pure nothrow unittest
{
    import std.algorithm.iteration;

    import std.range;

    //simple integral sumation
    assert(sum([ 1, 2, 3, 4]) == 10);

    //with integral promotion
    assert(sum([false, true, true, false, true]) == 3);
    assert(sum(ubyte.max.repeat(100)) == 25500);

    //The result may overflow
    assert(uint.max.repeat(3).sum()           ==  4294967293U );
    //But a seed can be used to change the sumation primitive
    assert(uint.max.repeat(3).sum(ulong.init) == 12884901885UL);

    //Floating point sumation
    assert(sum([1.0, 2.0, 3.0, 4.0]) == 10);

    //Floating point operations have double precision minimum
    static assert(is(typeof(sum([1F, 2F, 3F, 4F])) == double));
    assert(sum([1F, 2, 3, 4]) == 10);

    //Force pair-wise floating point sumation on large integers
    import std.math.operations : isClose;
    assert(iota(ulong.max / 2, ulong.max / 2 + 4096).sum(0.0)
               .isClose((ulong.max / 2) * 4096.0 + 4096^^2 / 2));
}

@safe @nogc pure nothrow unittest
{
    import std.algorithm.iteration;

    import std.math.operations : isClose;
    import std.math.traits : isNaN;

    static immutable arr1 = [1, 2, 3];
    static immutable arr2 = [1.5, 2.5, 12.5];

    assert(arr1.mean.isClose(2));
    assert(arr2.mean.isClose(5.5));

    assert(arr1[0 .. 0].mean.isNaN);
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.algorithm.mutation : copy;

    int[] arr = [ 1, 2, 2, 2, 2, 3, 4, 4, 4, 5 ];
    assert(equal(uniq(arr), [ 1, 2, 3, 4, 5 ][]));

    // Filter duplicates in-place using copy
    arr.length -= arr.uniq().copy(arr).length;
    assert(arr == [ 1, 2, 3, 4, 5 ]);

    // Note that uniqueness is only determined consecutively; duplicated
    // elements separated by an intervening different element will not be
    // eliminated:
    assert(equal(uniq([ 1, 1, 2, 1, 1, 3, 1]), [1, 2, 1, 3, 1]));
}

@safe unittest
{
    import std.algorithm.iteration;

    import std.algorithm.comparison : equal;
    import std.range : iota;
    assert(equal!equal(iota(3).permutations,
        [[0, 1, 2],
         [1, 0, 2],
         [2, 0, 1],
         [0, 2, 1],
         [1, 2, 0],
         [2, 1, 0]]));
}

