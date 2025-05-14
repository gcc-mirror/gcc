@safe unittest
{
    import std.range.primitives;

    struct A {}
    struct B
    {
        void popFront();
        @property bool empty();
        @property int front();
    }
    static assert(!isInputRange!A);
    static assert( isInputRange!B);
    static assert( isInputRange!(int[]));
    static assert( isInputRange!(char[]));
    static assert(!isInputRange!(char[4]));
    static assert( isInputRange!(inout(int)[]));
    static assert(!isInputRange!(int[], string));
    static assert( isInputRange!(int[], int));
    static assert( isInputRange!(int[], const int));
    static assert(!isInputRange!(int[], immutable int));

    static assert(!isInputRange!(const(int)[], int));
    static assert( isInputRange!(const(int)[], const int));
    static assert(!isInputRange!(const(int)[], immutable int));

    static assert(!isInputRange!(immutable(int)[], int));
    static assert( isInputRange!(immutable(int)[], const int));
    static assert( isInputRange!(immutable(int)[], immutable int));

    static struct NotDefaultConstructible
    {
        @disable this();
        void popFront();
        @property bool empty();
        @property int front();
    }
    static assert( isInputRange!NotDefaultConstructible);

    static struct NotDefaultConstructibleOrCopyable
    {
        @disable this();
        @disable this(this);
        void popFront();
        @property bool empty();
        @property int front();
    }
    static assert(isInputRange!NotDefaultConstructibleOrCopyable);

    static struct Frontless
    {
        void popFront();
        @property bool empty();
    }
    static assert(!isInputRange!Frontless);

    static struct VoidFront
    {
        void popFront();
        @property bool empty();
        void front();
    }
    static assert(!isInputRange!VoidFront);
}

@safe pure unittest
{
    import std.range.primitives;

    import std.traits : isSomeChar;

    static struct A
    {
        string data;

        void put(C)(C c)
        if (isSomeChar!C)
        {
            data ~= c;
        }
    }
    static assert(isOutputRange!(A, char));

    auto a = A();
    put(a, "Hello");
    assert(a.data == "Hello");
}

@safe pure nothrow unittest
{
    import std.range.primitives;

    int[] a = [1, 2, 3], b = [10, 20];
    auto c = a;
    put(a, b);
    assert(c == [10, 20, 3]);
    // at this point, a was advanced twice, so it only contains
    // its last element while c represents the whole array
    assert(a == [3]);
}

@safe pure unittest
{
    import std.range.primitives;

    // the elements must be mutable, so using string or const(char)[]
    // won't compile
    char[] s1 = new char[13];
    auto r1 = s1;
    put(r1, "Hello, World!"w);
    assert(s1 == "Hello, World!");
}

@safe unittest
{
    import std.range.primitives;

    void myprint(scope const(char)[] s) { }
    static assert(isOutputRange!(typeof(&myprint), char));

    static assert( isOutputRange!(char[], char));
    static assert( isOutputRange!(dchar[], wchar));
    static assert( isOutputRange!(dchar[], dchar));
}

@safe unittest
{
    import std.range.primitives;

    static assert(!isForwardRange!(int));
    static assert( isForwardRange!(int[]));
    static assert( isForwardRange!(inout(int)[]));

    static assert( isForwardRange!(int[], const int));
    static assert(!isForwardRange!(int[], immutable int));

    static assert(!isForwardRange!(const(int)[], int));
    static assert( isForwardRange!(const(int)[], const int));
    static assert(!isForwardRange!(const(int)[], immutable int));

    static assert(!isForwardRange!(immutable(int)[], int));
    static assert( isForwardRange!(immutable(int)[], const int));
    static assert( isForwardRange!(immutable(int)[], immutable int));
}

@safe unittest
{
    import std.range.primitives;

    alias R = int[];
    R r = [0,1];
    static assert(isForwardRange!R);           // is forward range
    r.popBack();                               // can invoke popBack
    auto t = r.back;                           // can get the back of the range
    auto w = r.front;
    static assert(is(typeof(t) == typeof(w))); // same type for front and back

    // Checking the element type
    static assert( isBidirectionalRange!(int[], const int));
    static assert(!isBidirectionalRange!(int[], immutable int));

    static assert(!isBidirectionalRange!(const(int)[], int));
    static assert( isBidirectionalRange!(const(int)[], const int));
    static assert(!isBidirectionalRange!(const(int)[], immutable int));

    static assert(!isBidirectionalRange!(immutable(int)[], int));
    static assert( isBidirectionalRange!(immutable(int)[], const int));
    static assert( isBidirectionalRange!(immutable(int)[], immutable int));
}

@safe unittest
{
    import std.range.primitives;

    import std.traits : isAggregateType, isAutodecodableString;

    alias R = int[];

    // range is finite and bidirectional or infinite and forward.
    static assert(isBidirectionalRange!R ||
                  isForwardRange!R && isInfinite!R);

    R r = [0,1];
    auto e = r[1]; // can index
    auto f = r.front;
    static assert(is(typeof(e) == typeof(f))); // same type for indexed and front
    static assert(!(isAutodecodableString!R && !isAggregateType!R)); // narrow strings cannot be indexed as ranges
    static assert(hasLength!R || isInfinite!R); // must have length or be infinite

    // $ must work as it does with arrays if opIndex works with $
    static if (is(typeof(r[$])))
    {
        static assert(is(typeof(f) == typeof(r[$])));

        // $ - 1 doesn't make sense with infinite ranges but needs to work
        // with finite ones.
        static if (!isInfinite!R)
            static assert(is(typeof(f) == typeof(r[$ - 1])));
    }

    // Checking the element type
    static assert( isRandomAccessRange!(int[], const int));
    static assert(!isRandomAccessRange!(int[], immutable int));

    static assert(!isRandomAccessRange!(const(int)[], int));
    static assert( isRandomAccessRange!(const(int)[], const int));
    static assert(!isRandomAccessRange!(const(int)[], immutable int));

    static assert(!isRandomAccessRange!(immutable(int)[], int));
    static assert( isRandomAccessRange!(immutable(int)[], const int));
    static assert( isRandomAccessRange!(immutable(int)[], immutable int));
}

@safe unittest
{
    import std.range.primitives;

    import std.algorithm.iteration : map;
    import std.range : iota, repeat;

    static struct HasPostblit
    {
        this(this) {}
    }

    auto nonMobile = map!"a"(repeat(HasPostblit.init));
    static assert(!hasMobileElements!(typeof(nonMobile)));
    static assert( hasMobileElements!(int[]));
    static assert( hasMobileElements!(inout(int)[]));
    static assert( hasMobileElements!(typeof(iota(1000))));

    static assert( hasMobileElements!( string));
    static assert( hasMobileElements!(dstring));
    static assert( hasMobileElements!( char[]));
    static assert( hasMobileElements!(dchar[]));
}

@safe unittest
{
    import std.range.primitives;

    import std.range : iota;

    // Standard arrays: returns the type of the elements of the array
    static assert(is(ElementType!(int[]) == int));

    // Accessing .front retrieves the decoded dchar
    static assert(is(ElementType!(char[])  == dchar)); // rvalue
    static assert(is(ElementType!(dchar[]) == dchar)); // lvalue

    // Ditto
    static assert(is(ElementType!(string) == dchar));
    static assert(is(ElementType!(dstring) == immutable(dchar)));

    // For ranges it gets the type of .front.
    auto range = iota(0, 10);
    static assert(is(ElementType!(typeof(range)) == int));
}

@safe unittest
{
    import std.range.primitives;

    import std.range : iota;
    // internally the range stores the encoded type
    static assert(is(ElementEncodingType!(char[])  == char));

    static assert(is(ElementEncodingType!(wstring) == immutable(wchar)));

    static assert(is(ElementEncodingType!(byte[]) == byte));

    auto range = iota(0, 10);
    static assert(is(ElementEncodingType!(typeof(range)) == int));
}

@safe unittest
{
    import std.range.primitives;

    static assert(!hasSwappableElements!(const int[]));
    static assert(!hasSwappableElements!(const(int)[]));
    static assert(!hasSwappableElements!(inout(int)[]));
    static assert( hasSwappableElements!(int[]));

    static assert(!hasSwappableElements!( string));
    static assert(!hasSwappableElements!(dstring));
    static assert(!hasSwappableElements!( char[]));
    static assert( hasSwappableElements!(dchar[]));
}

@safe unittest
{
    import std.range.primitives;

    static assert(!hasAssignableElements!(const int[]));
    static assert(!hasAssignableElements!(const(int)[]));
    static assert( hasAssignableElements!(int[]));
    static assert(!hasAssignableElements!(inout(int)[]));

    static assert(!hasAssignableElements!( string));
    static assert(!hasAssignableElements!(dstring));
    static assert(!hasAssignableElements!( char[]));
    static assert( hasAssignableElements!(dchar[]));
}

@safe unittest
{
    import std.range.primitives;

    import std.range : iota, chain;

    static assert( hasLvalueElements!(int[]));
    static assert( hasLvalueElements!(const(int)[]));
    static assert( hasLvalueElements!(inout(int)[]));
    static assert( hasLvalueElements!(immutable(int)[]));
    static assert(!hasLvalueElements!(typeof(iota(3))));

    static assert(!hasLvalueElements!( string));
    static assert( hasLvalueElements!(dstring));
    static assert(!hasLvalueElements!( char[]));
    static assert( hasLvalueElements!(dchar[]));

    auto c = chain([1, 2, 3], [4, 5, 6]);
    static assert( hasLvalueElements!(typeof(c)));
}

@safe unittest
{
    import std.range.primitives;

    static assert(!hasLength!(char[]));
    static assert( hasLength!(int[]));
    static assert( hasLength!(inout(int)[]));

    struct A { size_t length() { return 0; } }
    struct B { @property size_t length() { return 0; } }
    static assert( hasLength!(A));
    static assert( hasLength!(B));
}

@safe unittest
{
    import std.range.primitives;

    import std.range : Repeat;
    static assert(!isInfinite!(int[]));
    static assert( isInfinite!(Repeat!(int)));
}

@safe unittest
{
    import std.range.primitives;

    import std.range : takeExactly;
    static assert( hasSlicing!(int[]));
    static assert( hasSlicing!(const(int)[]));
    static assert(!hasSlicing!(const int[]));
    static assert( hasSlicing!(inout(int)[]));
    static assert(!hasSlicing!(inout int []));
    static assert( hasSlicing!(immutable(int)[]));
    static assert(!hasSlicing!(immutable int[]));
    static assert(!hasSlicing!string);
    static assert( hasSlicing!dstring);

    enum rangeFuncs = "@property int front();" ~
                      "void popFront();" ~
                      "@property bool empty();" ~
                      "@property auto save() { return this; }" ~
                      "@property size_t length();";

    struct A { mixin(rangeFuncs); int opSlice(size_t, size_t); }
    struct B { mixin(rangeFuncs); B opSlice(size_t, size_t); }
    struct C { mixin(rangeFuncs); @disable this(); C opSlice(size_t, size_t); }
    struct D { mixin(rangeFuncs); int[] opSlice(size_t, size_t); }
    static assert(!hasSlicing!(A));
    static assert( hasSlicing!(B));
    static assert( hasSlicing!(C));
    static assert(!hasSlicing!(D));

    struct InfOnes
    {
        enum empty = false;
        void popFront() {}
        @property int front() { return 1; }
        @property InfOnes save() { return this; }
        auto opSlice(size_t i, size_t j) { return takeExactly(this, j - i); }
        auto opSlice(size_t i, Dollar d) { return this; }

        struct Dollar {}
        Dollar opDollar() const { return Dollar.init; }
    }

    static assert(hasSlicing!InfOnes);
}

@safe unittest
{
    import std.range.primitives;

    import std.range : iota;

    assert(10.iota.walkLength == 10);
    // iota has a length function, and therefore the
    // doesn't have to be walked, and the upTo
    // parameter is ignored
    assert(10.iota.walkLength(5) == 10);
}

@safe unittest
{
    import std.range.primitives;

    int[] a = [ 1, 2, 3, 4, 5 ];
    a.popFrontN(2);
    assert(a == [ 3, 4, 5 ]);
    a.popFrontN(7);
    assert(a == [ ]);
}

@safe unittest
{
    import std.range.primitives;

    import std.algorithm.comparison : equal;
    import std.range : iota;
    auto LL = iota(1L, 7L);
    auto r = popFrontN(LL, 2);
    assert(equal(LL, [3L, 4L, 5L, 6L]));
    assert(r == 2);
}

@safe unittest
{
    import std.range.primitives;

    int[] a = [ 1, 2, 3, 4, 5 ];
    a.popBackN(2);
    assert(a == [ 1, 2, 3 ]);
    a.popBackN(7);
    assert(a == [ ]);
}

@safe unittest
{
    import std.range.primitives;

    import std.algorithm.comparison : equal;
    import std.range : iota;
    auto LL = iota(1L, 7L);
    auto r = popBackN(LL, 2);
    assert(equal(LL, [1L, 2L, 3L, 4L]));
    assert(r == 2);
}

@safe unittest
{
    import std.range.primitives;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filterBidirectional;

    auto a = [1, 2, 3];
    a.popFrontExactly(1);
    assert(a == [2, 3]);
    a.popBackExactly(1);
    assert(a == [2]);

    string s = "日本語";
    s.popFrontExactly(1);
    assert(s == "本語");
    s.popBackExactly(1);
    assert(s == "本");

    auto bd = filterBidirectional!"true"([1, 2, 3]);
    bd.popFrontExactly(1);
    assert(bd.equal([2, 3]));
    bd.popBackExactly(1);
    assert(bd.equal([2]));
}

@safe unittest
{
    import std.range.primitives;

    auto a = [ 1, 2, 3 ];
    assert(moveFront(a) == 1);
    assert(a.length == 3);

    // define a perfunctory input range
    struct InputRange
    {
        enum bool empty = false;
        enum int front = 7;
        void popFront() {}
        int moveFront() { return 43; }
    }
    InputRange r;
    // calls r.moveFront
    assert(moveFront(r) == 43);
}

@safe unittest
{
    import std.range.primitives;

    struct TestRange
    {
        int payload = 5;
        @property bool empty() { return false; }
        @property TestRange save() { return this; }
        @property ref int front() return { return payload; }
        @property ref int back() return { return payload; }
        void popFront() { }
        void popBack() { }
    }
    static assert(isBidirectionalRange!TestRange);
    TestRange r;
    auto x = moveBack(r);
    assert(x == 5);
}

@safe unittest
{
    import std.range.primitives;

    auto a = [1,2,3,4];
    foreach (idx, it; a)
    {
        assert(it == moveAt(a, idx));
    }
}

@safe pure nothrow unittest
{
    import std.range.primitives;

    auto a = [ 1, 2, 3 ];
    assert(!a.empty);
    assert(a[3 .. $].empty);

    int[string] b;
    assert(b.empty);
    b["zero"] = 0;
    assert(!b.empty);
}

@safe pure nothrow unittest
{
    import std.range.primitives;

    auto a = [ 1, 2, 3 ];
    auto b = a.save;
    assert(b is a);
}

@safe pure nothrow unittest
{
    import std.range.primitives;

    auto a = [ 1, 2, 3 ];
    a.popFront();
    assert(a == [ 2, 3 ]);
}

@safe pure nothrow unittest
{
    import std.range.primitives;

    auto a = [ 1, 2, 3 ];
    a.popBack();
    assert(a == [ 1, 2 ]);
}

@safe pure nothrow unittest
{
    import std.range.primitives;

    int[] a = [ 1, 2, 3 ];
    assert(a.front == 1);
}

@safe pure nothrow unittest
{
    import std.range.primitives;

    int[] a = [ 1, 2, 3 ];
    assert(a.back == 3);
    a.back += 4;
    assert(a.back == 7);
}

