@safe pure nothrow unittest
{
    import std.array;

    auto a = array([1, 2, 3, 4, 5][]);
    assert(a == [ 1, 2, 3, 4, 5 ]);
}

@safe pure nothrow unittest
{
    import std.array;

    import std.range.primitives : isRandomAccessRange;
    import std.traits : isAutodecodableString;

    // note that if autodecoding is turned off, `array` will not transcode these.
    static if (isAutodecodableString!string)
        assert("Hello D".array == "Hello D"d);
    else
        assert("Hello D".array == "Hello D");

    static if (isAutodecodableString!wstring)
        assert("Hello D"w.array == "Hello D"d);
    else
        assert("Hello D"w.array == "Hello D"w);

    static assert(isRandomAccessRange!dstring == true);
}

@safe pure unittest
{
    import std.array;

    import std.range : repeat, zip;
    import std.typecons : tuple;
    import std.range.primitives : autodecodeStrings;
    auto a = assocArray(zip([0, 1, 2], ["a", "b", "c"])); // aka zipMap
    static assert(is(typeof(a) == string[int]));
    assert(a == [0:"a", 1:"b", 2:"c"]);

    auto b = assocArray([ tuple("foo", "bar"), tuple("baz", "quux") ]);
    static assert(is(typeof(b) == string[string]));
    assert(b == ["foo":"bar", "baz":"quux"]);

    static if (autodecodeStrings)
        alias achar = dchar;
    else
        alias achar = immutable(char);
    auto c = assocArray("ABCD", true.repeat);
    static assert(is(typeof(c) == bool[achar]));
    bool[achar] expected = ['D':true, 'A':true, 'B':true, 'C':true];
    assert(c == expected);
}

@safe pure nothrow unittest
{
    import std.array;

    import std.algorithm.sorting : sort;
    import std.typecons : tuple, Tuple;

    auto aa = ["a": 1, "b": 2, "c": 3];
    Tuple!(string, int)[] pairs;

    // Iteration over key/value pairs.
    foreach (pair; aa.byPair)
    {
        if (pair.key == "b")
            pairs ~= tuple("B", pair.value);
        else
            pairs ~= pair;
    }

    // Iteration order is implementation-dependent, so we should sort it to get
    // a fixed order.
    pairs.sort();
    assert(pairs == [
        tuple("B", 2),
        tuple("a", 1),
        tuple("c", 3)
    ]);
}

@system nothrow pure unittest
{
    import std.array;

    double[] arr = uninitializedArray!(double[])(100);
    assert(arr.length == 100);

    double[][] matrix = uninitializedArray!(double[][])(42, 31);
    assert(matrix.length == 42);
    assert(matrix[0].length == 31);

    char*[] ptrs = uninitializedArray!(char*[])(100);
    assert(ptrs.length == 100);
}

@safe pure nothrow unittest
{
    import std.array;

    import std.algorithm.comparison : equal;
    import std.range : repeat;

    auto arr = minimallyInitializedArray!(int[])(42);
    assert(arr.length == 42);

    // Elements aren't necessarily initialized to 0, so don't do this:
    // assert(arr.equal(0.repeat(42)));
    // If that is needed, initialize the array normally instead:
    auto arr2 = new int[42];
    assert(arr2.equal(0.repeat(42)));
}

@safe pure nothrow unittest
{
    import std.array;

    int[] a = [ 10, 11, 12, 13, 14 ];
    int[] b = a[1 .. 3];
    assert(overlap(a, b) == [ 11, 12 ]);
    b = b.dup;
    // overlap disappears even though the content is the same
    assert(overlap(a, b).empty);

    static test()() @nogc
    {
        auto a = "It's three o'clock"d;
        auto b = a[5 .. 10];
        return b.overlap(a);
    }

    //works at compile-time
    static assert(test == "three"d);
}

@safe pure nothrow unittest
{
    import std.array;

    import std.meta : AliasSeq;

    // can be used as an alternative implementation of overlap that returns
    // `true` or `false` instead of a slice of the overlap
    bool isSliceOf(T)(const scope T[] part, const scope T[] whole)
    {
        return part.overlap(whole) is part;
    }

    auto x = [1, 2, 3, 4, 5];

    assert(isSliceOf(x[3..$], x));
    assert(isSliceOf(x[], x));
    assert(!isSliceOf(x, x[3..$]));
    assert(!isSliceOf([7, 8], x));
    assert(isSliceOf(null, x));

    // null is a slice of itself
    assert(isSliceOf(null, null));

    foreach (T; AliasSeq!(int[], const(int)[], immutable(int)[], const int[], immutable int[]))
    {
        T a = [1, 2, 3, 4, 5];
        T b = a;
        T c = a[1 .. $];
        T d = a[0 .. 1];
        T e = null;

        assert(isSliceOf(a, a));
        assert(isSliceOf(b, a));
        assert(isSliceOf(a, b));

        assert(isSliceOf(c, a));
        assert(isSliceOf(c, b));
        assert(!isSliceOf(a, c));
        assert(!isSliceOf(b, c));

        assert(isSliceOf(d, a));
        assert(isSliceOf(d, b));
        assert(!isSliceOf(a, d));
        assert(!isSliceOf(b, d));

        assert(isSliceOf(e, a));
        assert(isSliceOf(e, b));
        assert(isSliceOf(e, c));
        assert(isSliceOf(e, d));

        //verifies R-value compatibilty
        assert(!isSliceOf(a[$ .. $], a));
        assert(isSliceOf(a[0 .. 0], a));
        assert(isSliceOf(a, a[0.. $]));
        assert(isSliceOf(a[0 .. $], a));
    }
}

@safe pure unittest
{
    import std.array;

    int[] a = [ 1, 2, 3, 4 ];
    a.insertInPlace(2, [ 1, 2 ]);
    assert(a == [ 1, 2, 1, 2, 3, 4 ]);
    a.insertInPlace(3, 10u, 11);
    assert(a == [ 1, 2, 1, 10, 11, 2, 3, 4]);

    union U
    {
        float a = 3.0;
        int b;
    }

    U u1 = { b : 3 };
    U u2 = { b : 4 };
    U u3 = { b : 5 };
    U[] unionArr = [u2, u3];
    unionArr.insertInPlace(2, [u1]);
    assert(unionArr == [u2, u3, u1]);
    unionArr.insertInPlace(0, [u3, u2]);
    assert(unionArr == [u3, u2, u2, u3, u1]);

    static class C
    {
        int a;
        float b;

        this(int a, float b) { this.a = a; this.b = b; }
    }

    C c1 = new C(42, 1.0);
    C c2 = new C(0, 0.0);
    C c3 = new C(int.max, float.init);

    C[] classArr = [c1, c2, c3];
    insertInPlace(classArr, 3, [c2, c3]);
    C[5] classArr1 = classArr;
    assert(classArr1 == [c1, c2, c3, c2, c3]);
    insertInPlace(classArr, 0, c3, c1);
    C[7] classArr2 = classArr;
    assert(classArr2 == [c3, c1, c1, c2, c3, c2, c3]);
}

@safe pure nothrow unittest
{
    import std.array;

    auto a = [1, 2, 3, 4, 5];
    auto b = a[0 .. 2];

    assert(a.sameHead(b));
}

@safe pure nothrow unittest
{
    import std.array;

    auto a = [1, 2, 3, 4, 5];
    auto b = a[3..$];

    assert(a.sameTail(b));
}

@safe unittest
{
    import std.array;

    auto a = "abc";
    auto s = replicate(a, 3);

    assert(s == "abcabcabc");

    auto b = [1, 2, 3];
    auto c = replicate(b, 3);

    assert(c == [1, 2, 3, 1, 2, 3, 1, 2, 3]);

    auto d = replicate(b, 0);

    assert(d == []);
}

@safe unittest
{
    import std.array;

    import std.uni : isWhite;
    assert("Learning,D,is,fun".split(",") == ["Learning", "D", "is", "fun"]);
    assert("Learning D is fun".split!isWhite == ["Learning", "D", "is", "fun"]);
    assert("Learning D is fun".split(" D ") == ["Learning", "is fun"]);
}

@safe unittest
{
    import std.array;

    string str = "Hello World!";
    assert(str.split == ["Hello", "World!"]);

    string str2 = "Hello\t\tWorld\t!";
    assert(str2.split == ["Hello", "World", "!"]);
}

@safe unittest
{
    import std.array;

    assert(split("hello world") == ["hello","world"]);
    assert(split("192.168.0.1", ".") == ["192", "168", "0", "1"]);

    auto a = split([1, 2, 3, 4, 5, 1, 2, 3, 4, 5], [2, 3]);
    assert(a == [[1], [4, 5, 1], [4, 5]]);
}

@safe pure nothrow unittest
{
    import std.array;

    assert(join(["hello", "silly", "world"], " ") == "hello silly world");
    assert(join(["hello", "silly", "world"]) == "hellosillyworld");

    assert(join([[1, 2, 3], [4, 5]], [72, 73]) == [1, 2, 3, 72, 73, 4, 5]);
    assert(join([[1, 2, 3], [4, 5]]) == [1, 2, 3, 4, 5]);

    const string[] arr = ["apple", "banana"];
    assert(arr.join(",") == "apple,banana");
    assert(arr.join() == "applebanana");
}

@safe unittest
{
    import std.array;

    assert("Hello Wörld".replace("o Wö", "o Wo") == "Hello World");
    assert("Hello Wörld".replace("l", "h") == "Hehho Wörhd");
}

@safe unittest
{
    import std.array;

    size_t changed = 0;
    assert("Hello Wörld".replace("o Wö", "o Wo", changed) == "Hello World");
    assert(changed == 1);

    changed = 0;
    assert("Hello Wörld".replace("l", "h", changed) == "Hehho Wörhd");
    import std.stdio : writeln;
    writeln(changed);
    assert(changed == 3);
}

@safe unittest
{
    import std.array;

    auto arr = [1, 2, 3, 4, 5];
    auto from = [2, 3];
    auto to = [4, 6];
    auto sink = appender!(int[])();

    replaceInto(sink, arr, from, to);

    assert(sink.data == [1, 4, 6, 4, 5]);
}

@safe unittest
{
    import std.array;

    auto arr = [1, 2, 3, 4, 5];
    auto from = [2, 3];
    auto to = [4, 6];
    auto sink = appender!(int[])();

    size_t changed = 0;
    replaceInto(sink, arr, from, to, changed);

    assert(sink.data == [1, 4, 6, 4, 5]);
    assert(changed == 1);
}

@safe unittest
{
    import std.array;

    auto a = [ 1, 2, 3, 4 ];
    auto b = a.replace(1, 3, [ 9, 9, 9 ]);
    assert(a == [ 1, 2, 3, 4 ]);
    assert(b == [ 1, 9, 9, 9, 4 ]);
}

@safe unittest
{
    import std.array;

    int[] a = [1, 4, 5];
    replaceInPlace(a, 1u, 2u, [2, 3, 4]);
    assert(a == [1, 2, 3, 4, 5]);
    replaceInPlace(a, 1u, 2u, cast(int[])[]);
    assert(a == [1, 3, 4, 5]);
    replaceInPlace(a, 1u, 3u, a[2 .. 4]);
    assert(a == [1, 4, 5, 5]);
}

@safe unittest
{
    import std.array;

    auto a = [1, 2, 2, 3, 4, 5];
    auto b = a.replaceFirst([2], [1337]);
    assert(b == [1, 1337, 2, 3, 4, 5]);

    auto s = "This is a foo foo list";
    auto r = s.replaceFirst("foo", "silly");
    assert(r == "This is a silly foo list");
}

@safe unittest
{
    import std.array;

    auto a = [1, 2, 2, 3, 4, 5];
    auto b = a.replaceLast([2], [1337]);
    assert(b == [1, 2, 1337, 3, 4, 5]);

    auto s = "This is a foo foo list";
    auto r = s.replaceLast("foo", "silly");
    assert(r == "This is a foo silly list", r);
}

@safe unittest
{
    import std.array;

    auto a = [1, 2, 3, 4, 5];
    auto b = replaceSlice(a, a[1 .. 4], [0, 0, 0]);

    assert(b == [1, 0, 0, 0, 5]);
}

@safe pure nothrow unittest
{
    import std.array;

    auto app = appender!string();
    string b = "abcdefg";
    foreach (char c; b)
        app.put(c);
    assert(app[] == "abcdefg");

    int[] a = [ 1, 2 ];
    auto app2 = appender(a);
    app2.put(3);
    assert(app2.length == 3);
    app2.put([ 4, 5, 6 ]);
    assert(app2[] == [ 1, 2, 3, 4, 5, 6 ]);
}

@safe pure nothrow unittest
{
    import std.array;

    int[] a = [1, 2];
    auto app2 = appender(&a);
    assert(app2[] == [1, 2]);
    assert(a == [1, 2]);
    app2 ~= 3;
    assert(app2.length == 3);
    app2 ~= [4, 5, 6];
    assert(app2[] == [1, 2, 3, 4, 5, 6]);
    assert(a == [1, 2, 3, 4, 5, 6]);

    app2.reserve(5);
    assert(app2.capacity >= 5);
}

@safe pure nothrow unittest
{
    import std.array;

    auto w = appender!string;
    // pre-allocate space for at least 10 elements (this avoids costly reallocations)
    w.reserve(10);
    assert(w.capacity >= 10);

    w.put('a'); // single elements
    w.put("bc"); // multiple elements

    // use the append syntax
    w ~= 'd';
    w ~= "ef";

    assert(w[] == "abcdef");
}

@safe pure nothrow unittest
{
    import std.array;

    int[] a = [1, 2];
    auto app2 = appender(&a);
    assert(app2[] == [1, 2]);
    assert(a == [1, 2]);
    app2 ~= 3;
    app2 ~= [4, 5, 6];
    assert(app2[] == [1, 2, 3, 4, 5, 6]);
    assert(a == [1, 2, 3, 4, 5, 6]);

    app2.reserve(5);
    assert(app2.capacity >= 5);
}

nothrow pure @safe @nogc unittest
{
    import std.array;

    auto a = [0, 1].staticArray;
    static assert(is(typeof(a) == int[2]));
    assert(a == [0, 1]);
}

nothrow pure @safe @nogc unittest
{
    import std.array;

    auto b = [0, 1].staticArray!long;
    static assert(is(typeof(b) == long[2]));
    assert(b == [0, 1]);
}

nothrow pure @safe @nogc unittest
{
    import std.array;

    import std.range : iota;

    auto input = 3.iota;
    auto a = input.staticArray!2;
    static assert(is(typeof(a) == int[2]));
    assert(a == [0, 1]);
    auto b = input.staticArray!(long[4]);
    static assert(is(typeof(b) == long[4]));
    assert(b == [0, 1, 2, 0]);
}

nothrow pure @safe @nogc unittest
{
    import std.array;

    import std.range : iota;

    enum a = staticArray!(2.iota);
    static assert(is(typeof(a) == int[2]));
    assert(a == [0, 1]);

    enum b = staticArray!(long, 2.iota);
    static assert(is(typeof(b) == long[2]));
    assert(b == [0, 1]);
}

