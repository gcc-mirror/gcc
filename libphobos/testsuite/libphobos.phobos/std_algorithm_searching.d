@safe unittest
{
    import std.algorithm.searching;

    assert( all!"a & 1"([1, 3, 5, 7, 9]));
    assert(!all!"a & 1"([1, 2, 3, 5, 7, 9]));
}

@safe unittest
{
    import std.algorithm.searching;

    int[3] vals = [5, 3, 18];
    assert( all(vals[]));
}

@safe unittest
{
    import std.algorithm.searching;

    import std.ascii : isWhite;
    assert( all!(any!isWhite)(["a a", "b b"]));
    assert(!any!(all!isWhite)(["a a", "b b"]));
}

@safe unittest
{
    import std.algorithm.searching;

    int[3] vals1 = [0, 0, 0];
    assert(!any(vals1[])); //none of vals1 evaluate to true

    int[3] vals2 = [2, 0, 2];
    assert( any(vals2[]));
    assert(!all(vals2[]));

    int[3] vals3 = [3, 3, 3];
    assert( any(vals3[]));
    assert( all(vals3[]));
}

@safe pure unittest
{
    import std.algorithm.searching;

    auto s = "1 + (2 * (3 + 1 / 2)";
    assert(!balancedParens(s, '(', ')'));
    s = "1 + (2 * (3 + 1) / 2)";
    assert(balancedParens(s, '(', ')'));
    s = "1 + (2 * (3 + 1) / 2)";
    assert(!balancedParens(s, '(', ')', 0));
    s = "1 + (2 * 3 + 1) / (2 - 5)";
    assert(balancedParens(s, '(', ')', 0));
    s = "f(x) = ⌈x⌉";
    assert(balancedParens(s, '⌈', '⌉'));
}

@safe pure nothrow unittest
{
    import std.algorithm.searching;

    auto bmFinder = boyerMooreFinder("TG");

    string r = "TAGTGCCTGA";
    // search for the first match in the haystack r
    r = bmFinder.beFound(r);
    assert(r == "TGCCTGA");

    // continue search in haystack
    r = bmFinder.beFound(r[2 .. $]);
    assert(r == "TGA");
}

@safe unittest
{
    import std.algorithm.searching;

    assert(commonPrefix("hello, world", "hello, there") == "hello, ");
}

@safe unittest
{
    import std.algorithm.searching;

    // count elements in range
    int[] a = [ 1, 2, 4, 3, 2, 5, 3, 2, 4 ];
    assert(count(a, 2) == 3);
    assert(count!("a > b")(a, 2) == 5);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.uni : toLower;
    // count range in range
    assert(count("abcadfabf", "ab") == 2);
    assert(count("ababab", "abab") == 1);
    assert(count("ababab", "abx") == 0);
    // fuzzy count range in range
    assert(count!((a, b) => toLower(a) == toLower(b))("AbcAdFaBf", "ab") == 2);
}

@safe unittest
{
    import std.algorithm.searching;

    // count elements in range
    int[] a = [ 1, 2, 4, 3, 2, 5, 3, 2, 4 ];
    assert(count(a) == 9);
    // count predicate in range
    assert(count!("a > 2")(a) == 5);
}

@safe unittest
{
    import std.algorithm.searching;

    assert(countUntil("hello world", "world") == 6);
    assert(countUntil("hello world", 'r') == 8);
    assert(countUntil("hello world", "programming") == -1);
    assert(countUntil("日本語", "本語") == 1);
    assert(countUntil("日本語", '語')   == 2);
    assert(countUntil("日本語", "五") == -1);
    assert(countUntil("日本語", '五') == -1);
    assert(countUntil([0, 7, 12, 22, 9], [12, 22]) == 2);
    assert(countUntil([0, 7, 12, 22, 9], 9) == 4);
    assert(countUntil!"a > b"([0, 7, 12, 22, 9], 20) == 3);

    // supports multiple needles
    auto res = "...hello".countUntil("ha", "he");
    assert(res.steps == 3);
    assert(res.needle == 1);

    // returns -1 if no needle was found
    res = "hello".countUntil("ha", "hu");
    assert(res.steps == -1);
    assert(res.needle == -1);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.ascii : isDigit;
    import std.uni : isWhite;

    assert(countUntil!(isWhite)("hello world") == 5);
    assert(countUntil!(isDigit)("hello world") == -1);
    assert(countUntil!"a > 20"([0, 7, 12, 22, 9]) == 3);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.ascii : isAlpha;
    assert("abc".endsWith!(a => a.isAlpha));
    assert("abc".endsWith!isAlpha);

    assert(!"ab1".endsWith!(a => a.isAlpha));

    assert(!"ab1".endsWith!isAlpha);
    assert(!"".endsWith!(a => a.isAlpha));

    import std.algorithm.comparison : among;
    assert("abc".endsWith!(a => a.among('c', 'd') != 0));
    assert(!"abc".endsWith!(a => a.among('a', 'b') != 0));

    assert(endsWith("abc", ""));
    assert(!endsWith("abc", "b"));
    assert(endsWith("abc", "a", 'c') == 2);
    assert(endsWith("abc", "c", "a") == 1);
    assert(endsWith("abc", "c", "c") == 1);
    assert(endsWith("abc", "bc", "c") == 2);
    assert(endsWith("abc", "x", "c", "b") == 2);
    assert(endsWith("abc", "x", "aa", "bc") == 3);
    assert(endsWith("abc", "x", "aaa", "sab") == 0);
    assert(endsWith("abc", "x", "aaa", 'c', "sab") == 3);
}

@safe unittest
{
    import std.algorithm.searching;

    auto arr = [ 1, 2, 3, 4, 1 ];
    assert(find!("a > 2")(arr) == [ 3, 4, 1 ]);

    // with predicate alias
    bool pred(int e) => e + 1 > 1.5;
    assert(find!(pred)(arr) == arr);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.range.primitives;

    auto arr = [1, 2, 4, 4, 4, 4, 5, 6, 9];
    assert(arr.find(4) == [4, 4, 4, 4, 5, 6, 9]);
    assert(arr.find(1) == arr);
    assert(arr.find(9) == [9]);
    assert(arr.find!((e, n) => e > n)(4) == [5, 6, 9]);
    assert(arr.find!((e, n) => e < n)(4) == arr);
    assert(arr.find(0).empty);
    assert(arr.find(10).empty);
    assert(arr.find(8).empty);

    assert(find("hello, world", ',') == ", world");
}

@safe unittest
{
    import std.algorithm.searching;

    import std.range.primitives;
    import std.uni : toLower;

    string[] s = ["Hello", "world", "!"];
    assert(s.find!((e, n) => toLower(e) == n)("hello") == s);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.container : SList;
    import std.range.primitives : empty;
    import std.typecons : Tuple;

    assert(find("hello, world", "World").empty);
    assert(find("hello, world", "wo") == "world");
    assert([1, 2, 3, 4].find(SList!int(2, 3)[]) == [2, 3, 4]);
    alias C = Tuple!(int, "x", int, "y");
    auto a = [C(1,0), C(2,0), C(3,1), C(4,0)];
    assert(a.find!"a.x == b"([2, 3]) == [C(2,0), C(3,1), C(4,0)]);
    assert(a[1 .. $].find!"a.x == b"([2, 3]) == [C(2,0), C(3,1), C(4,0)]);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.typecons : tuple;
    int[] a = [ 1, 4, 2, 3 ];
    assert(find(a, 4) == [ 4, 2, 3 ]);
    assert(find(a, [ 1, 4 ]) == [ 1, 4, 2, 3 ]);
    assert(find(a, [ 1, 3 ], 4) == tuple([ 4, 2, 3 ], 2));
    // Mixed types allowed if comparable
    assert(find(a, 5, [ 1.2, 3.5 ], 2.0) == tuple([ 2, 3 ], 3));
}

@safe unittest
{
    import std.algorithm.searching;

    import std.range.primitives : empty;
    int[] a = [ -1, 0, 1, 2, 3, 4, 5 ];
    int[] b = [ 1, 2, 3 ];

    assert(find(a, boyerMooreFinder(b)) == [ 1, 2, 3, 4, 5 ]);
    assert(find(b, boyerMooreFinder(a)).empty);
}

@safe unittest
{
    import std.algorithm.searching;

    const arr = [0, 1, 2, 3];
    assert(canFind(arr, 2));
    assert(!canFind(arr, 4));

    // find one of several needles
    assert(arr.canFind(3, 2));
    assert(arr.canFind(3, 2) == 2); // second needle found
    assert(arr.canFind([1, 3], 2) == 2);

    assert(canFind(arr, [1, 2], [2, 3]));
    assert(canFind(arr, [1, 2], [2, 3]) == 1);
    assert(canFind(arr, [1, 7], [2, 3]));
    assert(canFind(arr, [1, 7], [2, 3]) == 2);
    assert(!canFind(arr, [1, 3], [2, 4]));
    assert(canFind(arr, [1, 3], [2, 4]) == 0);
}

@safe unittest
{
    import std.algorithm.searching;

    auto words = [
        "apple",
        "beeswax",
        "cardboard"
    ];
    assert(!canFind(words, "bees"));
    assert( canFind!((string elem, string needle) => elem.startsWith(needle))(words, "bees"));
}

@safe unittest
{
    import std.algorithm.searching;

    string s1 = "aaa111aaa";
    string s2 = "aaa222aaa";
    string s3 = "aaa333aaa";
    string s4 = "aaa444aaa";
    const hay = [s1, s2, s3, s4];
    assert(hay.canFind!(e => e.canFind("111", "222")));
}

@safe unittest
{
    import std.algorithm.searching;

    int[] a = [ 11, 10, 10, 9, 8, 8, 7, 8, 9 ];
    auto r = findAdjacent(a);
    assert(r == [ 10, 10, 9, 8, 8, 7, 8, 9 ]);
    auto p = findAdjacent!("a < b")(a);
    assert(p == [ 7, 8, 9 ]);

}

@safe unittest
{
    import std.algorithm.searching;

    int[] a = [ -1, 0, 1, 2, 3, 4, 5 ];
    int[] b = [ 3, 1, 2 ];
    assert(findAmong(a, b) == a[2 .. $]);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.range.primitives : empty;
    // Needle is found; s is replaced by the substring following the first
    // occurrence of the needle.
    string s = "abcdef";
    assert(findSkip(s, "cd") && s == "ef");

    // Needle is not found; s is left untouched.
    s = "abcdef";
    assert(!findSkip(s, "cxd") && s == "abcdef");

    // If the needle occurs at the end of the range, the range is left empty.
    s = "abcdef";
    assert(findSkip(s, "def") && s.empty);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.ascii : isWhite;
    string s = "    abc";
    assert(findSkip!isWhite(s) && s == "abc");
    assert(!findSkip!isWhite(s) && s == "abc");

    s = "  ";
    assert(findSkip!isWhite(s) == 2);
}

@safe pure nothrow unittest
{
    import std.algorithm.searching;

    // findSplit returns a triplet
    if (auto split = "dlang-rocks".findSplit("-"))
    {
        assert(split[0] == "dlang");
        assert(split[1] == "-");
        assert(split[2] == "rocks");
    }
    else assert(0);

    // findSplitBefore returns 2 ranges
    if (const split = [2, 3, 2, 3, 4, 1].findSplitBefore!"a > b"([2, 2]))
    {
        assert(split[0] == [2, 3, 2]);
        // [3, 4] each greater than [2, 2]
        assert(split[1] == [3, 4, 1]);
    }
    else assert(0);
}

@safe pure nothrow unittest
{
    import std.algorithm.searching;

    import std.range.primitives : empty;

    auto a = "Carl Sagan Memorial Station";
    auto r = findSplit(a, "Velikovsky");
    import std.typecons : isTuple;
    static assert(isTuple!(typeof(r.asTuple)));
    static assert(isTuple!(typeof(r)));
    assert(!r);
    assert(r[0] == a);
    assert(r[1].empty);
    assert(r[2].empty);
    r = findSplit(a, " ");
    assert(r[0] == "Carl");
    assert(r[1] == " ");
    assert(r[2] == "Sagan Memorial Station");
    if (const r1 = findSplitBefore(a, "Sagan"))
    {
        assert(r1);
        assert(r1[0] == "Carl ");
        assert(r1[1] == "Sagan Memorial Station");
    }
    if (const r2 = findSplitAfter(a, "Sagan"))
    {
        assert(r2);
        assert(r2[0] == "Carl Sagan");
        assert(r2[1] == " Memorial Station");
    }
}

@safe pure nothrow unittest
{
    import std.algorithm.searching;

    import std.range : only;
    assert([1, 2, 3, 4].findSplitBefore(only(3))[0] == [1, 2]);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.conv : text;
    import std.typecons : tuple;

    int[] a = [ 2, 3, 4, 1, 2, 4, 1, 1, 2 ];
    // Minimum is 1 and occurs 3 times
    assert(a.minCount == tuple(1, 3));
    // Maximum is 4 and occurs 2 times
    assert(a.maxCount == tuple(4, 2));
}

@safe pure unittest
{
    import std.algorithm.searching;

    import std.range : enumerate;
    import std.typecons : tuple;

    assert([2, 7, 1, 3].minElement == 1);

    // allows to get the index of an element too
    assert([5, 3, 7, 9].enumerate.minElement!"a.value" == tuple(1, 3));

    // any custom accessor can be passed
    assert([[0, 4], [1, 2]].minElement!"a[1]" == [1, 2]);

    // can be seeded
    int[] arr;
    assert(arr.minElement(1) == 1);
}

@safe pure unittest
{
    import std.algorithm.searching;

    import std.range : enumerate;
    import std.typecons : tuple;
    assert([2, 1, 4, 3].maxElement == 4);

    // allows to get the index of an element too
    assert([2, 1, 4, 3].enumerate.maxElement!"a.value" == tuple(2, 4));

    // any custom accessor can be passed
    assert([[0, 4], [1, 2]].maxElement!"a[1]" == [0, 4]);

    // can be seeded
    int[] arr;
    assert(arr.minElement(1) == 1);
}

@safe unittest
{
    import std.algorithm.searching;

    assert(extrema([5,2,9,4,1]) == [1, 9]);
}

@safe unittest
{
    import std.algorithm.searching;

    int[] a = [ 2, 3, 4, 1, 2, 4, 1, 1, 2 ];
    // Minimum is 1 and first occurs in position 3
    assert(a.minPos == [ 1, 2, 4, 1, 1, 2 ]);
    // Maximum is 4 and first occurs in position 2
    assert(a.maxPos == [ 4, 1, 2, 4, 1, 1, 2 ]);
}

@safe pure nothrow unittest
{
    import std.algorithm.searching;

    int[] a = [2, 3, 4, 1, 2, 4, 1, 1, 2];

    // Minimum is 1 and first occurs in position 3
    assert(a.minIndex == 3);
    // Get maximum index with minIndex
    assert(a.minIndex!"a > b" == 2);

    // Range is empty, so return value is -1
    int[] b;
    assert(b.minIndex == -1);

    // Works with more custom types
    struct Dog { int age; }
    Dog[] dogs = [Dog(10), Dog(5), Dog(15)];
    assert(dogs.minIndex!"a.age < b.age" == 1);
}

@safe pure nothrow unittest
{
    import std.algorithm.searching;

    // Maximum is 4 and first occurs in position 2
    int[] a = [2, 3, 4, 1, 2, 4, 1, 1, 2];
    assert(a.maxIndex == 2);

    // Empty range
    int[] b;
    assert(b.maxIndex == -1);

    // Works with more custom types
    struct Dog { int age; }
    Dog[] dogs = [Dog(10), Dog(15), Dog(5)];
    assert(dogs.maxIndex!"a.age < b.age" == 1);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.algorithm.comparison : equal;

    auto s1 = "Hello world";
    assert(!skipOver(s1, "Ha"));
    assert(s1 == "Hello world");
    assert(skipOver(s1, "Hell") && s1 == "o world", s1);

    string[]  r1 = ["abc", "def", "hij"];
    dstring[] r2 = ["abc"d];
    assert(!skipOver!((a, b) => a.equal(b))(r1, ["def"d]), r1[0]);
    assert(r1 == ["abc", "def", "hij"]);
    assert(skipOver!((a, b) => a.equal(b))(r1, r2));
    assert(r1 == ["def", "hij"]);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.ascii : isWhite;
    import std.range.primitives : empty;

    auto s2 = "\t\tvalue";
    auto s3 = "";
    auto s4 = "\t\t\t";
    assert(s2.skipOver!isWhite && s2 == "value");
    assert(!s3.skipOver!isWhite);
    assert(s4.skipOver!isWhite && s3.empty);
}

@safe unittest
{
    import std.algorithm.searching;

    auto s = "Hello world";
    assert(!skipOver(s, "hello", "HellO"));
    assert(s == "Hello world");

    // the range is skipped over the longest matching needle is skipped
    assert(skipOver(s, "foo", "hell", "Hello "));
    assert(s == "world");
}

@safe unittest
{
    import std.algorithm.searching;

    import std.algorithm.comparison : equal;

    auto s1 = "Hello world";
    assert(!skipOver(s1, 'a'));
    assert(s1 == "Hello world");
    assert(skipOver(s1, 'H') && s1 == "ello world");

    string[] r = ["abc", "def", "hij"];
    dstring e = "abc"d;
    assert(!skipOver!((a, b) => a.equal(b))(r, "def"d));
    assert(r == ["abc", "def", "hij"]);
    assert(skipOver!((a, b) => a.equal(b))(r, e));
    assert(r == ["def", "hij"]);

    auto s2 = "";
    assert(!s2.skipOver('a'));
}

@safe unittest
{
    import std.algorithm.searching;

    import std.ascii : isWhite;
    import std.range.primitives : empty;

    alias whitespaceSkiper = skipOver!isWhite;

    auto s2 = "\t\tvalue";
    auto s3 = "";
    auto s4 = "\t\t\t";
    assert(whitespaceSkiper(s2) && s2 == "value");
    assert(!whitespaceSkiper(s2));
    assert(whitespaceSkiper(s4) && s3.empty);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.ascii : isAlpha;

    assert("abc".startsWith!(a => a.isAlpha));
    assert("abc".startsWith!isAlpha);
    assert(!"1ab".startsWith!(a => a.isAlpha));
    assert(!"".startsWith!(a => a.isAlpha));

    import std.algorithm.comparison : among;
    assert("abc".startsWith!(a => a.among('a', 'b') != 0));
    assert(!"abc".startsWith!(a => a.among('b', 'c') != 0));

    assert(startsWith("abc", ""));
    assert(startsWith("abc", "a"));
    assert(!startsWith("abc", "b"));
    assert(startsWith("abc", 'a', "b") == 1);
    assert(startsWith("abc", "b", "a") == 2);
    assert(startsWith("abc", "a", "a") == 1);
    assert(startsWith("abc", "ab", "a") == 2);
    assert(startsWith("abc", "x", "a", "b") == 2);
    assert(startsWith("abc", "x", "aa", "ab") == 3);
    assert(startsWith("abc", "x", "aaa", "sab") == 0);
    assert(startsWith("abc", "x", "aaa", "a", "sab") == 3);

    import std.typecons : Tuple;
    alias C = Tuple!(int, "x", int, "y");
    assert(startsWith!"a.x == b"([ C(1,1), C(1,2), C(2,2) ], [1, 1]));
    assert(startsWith!"a.x == b"([ C(1,1), C(2,1), C(2,2) ], [1, 1], [1, 2], [1, 3]) == 2);
}

@safe unittest
{
    import std.algorithm.searching;

    import std.algorithm.comparison : equal;
    import std.typecons : No;
    int[] a = [ 1, 2, 4, 7, 7, 2, 4, 7, 3, 5];
    assert(equal(a.until(7), [1, 2, 4]));
    assert(equal(a.until(7, No.openRight), [1, 2, 4, 7]));
}

