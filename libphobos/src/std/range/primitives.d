/**
This module is a submodule of $(MREF std, range).

It defines the bidirectional and forward range primitives for arrays:
$(LREF empty), $(LREF front), $(LREF back), $(LREF popFront), $(LREF popBack) and $(LREF save).

It provides basic range functionality by defining several templates for testing
whether a given object is a range, and what kind of range it is:

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE ,
    $(TR $(TD $(LREF isInputRange))
        $(TD Tests if something is an $(I input range), defined to be
        something from which one can sequentially read data using the
        primitives `front`, `popFront`, and `empty`.
    ))
    $(TR $(TD $(LREF isOutputRange))
        $(TD Tests if something is an $(I output range), defined to be
        something to which one can sequentially write data using the
        $(LREF put) primitive.
    ))
    $(TR $(TD $(LREF isForwardRange))
        $(TD Tests if something is a $(I forward range), defined to be an
        input range with the additional capability that one can save one's
        current position with the `save` primitive, thus allowing one to
        iterate over the same range multiple times.
    ))
    $(TR $(TD $(LREF isBidirectionalRange))
        $(TD Tests if something is a $(I bidirectional range), that is, a
        forward range that allows reverse traversal using the primitives $(D
        back) and `popBack`.
    ))
    $(TR $(TD $(LREF isRandomAccessRange))
        $(TD Tests if something is a $(I random access range), which is a
        bidirectional range that also supports the array subscripting
        operation via the primitive `opIndex`.
    ))
))

It also provides number of templates that test for various range capabilities:

$(BOOKTABLE ,
    $(TR $(TD $(LREF hasMobileElements))
        $(TD Tests if a given range's elements can be moved around using the
        primitives `moveFront`, `moveBack`, or `moveAt`.
    ))
    $(TR $(TD $(LREF ElementType))
        $(TD Returns the element type of a given range.
    ))
    $(TR $(TD $(LREF ElementEncodingType))
        $(TD Returns the encoding element type of a given range.
    ))
    $(TR $(TD $(LREF hasSwappableElements))
        $(TD Tests if a range is a forward range with swappable elements.
    ))
    $(TR $(TD $(LREF hasAssignableElements))
        $(TD Tests if a range is a forward range with mutable elements.
    ))
    $(TR $(TD $(LREF hasLvalueElements))
        $(TD Tests if a range is a forward range with elements that can be
        passed by reference and have their address taken.
    ))
    $(TR $(TD $(LREF hasLength))
        $(TD Tests if a given range has the `length` attribute.
    ))
    $(TR $(TD $(LREF isInfinite))
        $(TD Tests if a given range is an $(I infinite range).
    ))
    $(TR $(TD $(LREF hasSlicing))
        $(TD Tests if a given range supports the array slicing operation $(D
        R[x .. y]).
    ))
)

Finally, it includes some convenience functions for manipulating ranges:

$(BOOKTABLE ,
    $(TR $(TD $(LREF popFrontN))
        $(TD Advances a given range by up to $(I n) elements.
    ))
    $(TR $(TD $(LREF popBackN))
        $(TD Advances a given bidirectional range from the right by up to
        $(I n) elements.
    ))
    $(TR $(TD $(LREF popFrontExactly))
        $(TD Advances a given range by up exactly $(I n) elements.
    ))
    $(TR $(TD $(LREF popBackExactly))
        $(TD Advances a given bidirectional range from the right by exactly
        $(I n) elements.
    ))
    $(TR $(TD $(LREF moveFront))
        $(TD Removes the front element of a range.
    ))
    $(TR $(TD $(LREF moveBack))
        $(TD Removes the back element of a bidirectional range.
    ))
    $(TR $(TD $(LREF moveAt))
        $(TD Removes the $(I i)'th element of a random-access range.
    ))
    $(TR $(TD $(LREF walkLength))
        $(TD Computes the length of any range in O(n) time.
    ))
    $(TR $(TD $(LREF put))
        $(TD Outputs element `e` to a range.
    ))
)

Source: $(PHOBOSSRC std/range/primitives.d)

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu), David Simcha, and
         $(HTTP jmdavisprog.com, Jonathan M Davis). Credit for some of the ideas
         in building this module goes to
         $(HTTP fantascienza.net/leonardo/so/, Leonardo Maffi).
*/
module std.range.primitives;

import std.traits;

/**
Returns `true` if `R` is an input range. An input range must
define the primitives `empty`, `popFront`, and `front`. The
following code should compile for any input range.

----
R r;              // can define a range object
if (r.empty) {}   // can test for empty
r.popFront();     // can invoke popFront()
auto h = r.front; // can get the front of the range of non-void type
----

The following are rules of input ranges are assumed to hold true in all
Phobos code. These rules are not checkable at compile-time, so not conforming
to these rules when writing ranges or range based code will result in
undefined behavior.

$(UL
    $(LI `r.empty` returns `false` if and only if there is more data
    available in the range.)
    $(LI `r.empty` evaluated multiple times, without calling
    `r.popFront`, or otherwise mutating the range object or the
    underlying data, yields the same result for every evaluation.)
    $(LI `r.front` returns the current element in the range.
    It may return by value or by reference.)
    $(LI `r.front` can be legally evaluated if and only if evaluating
    `r.empty` has, or would have, equaled `false`.)
    $(LI `r.front` evaluated multiple times, without calling
    `r.popFront`, or otherwise mutating the range object or the
    underlying data, yields the same result for every evaluation.)
    $(LI `r.popFront` advances to the next element in the range.)
    $(LI `r.popFront` can be called if and only if evaluating `r.empty`
    has, or would have, equaled `false`.)
)

Also, note that Phobos code assumes that the primitives `r.front` and
`r.empty` are $(BIGOH 1) time complexity wise or "cheap" in terms of
running time. $(BIGOH) statements in the documentation of range functions
are made with this assumption.

See_Also:
    The header of $(MREF std,range) for tutorials on ranges.

Params:
    R = type to be tested
    E = if present, the elements of the range must be
        $(DDSUBLINK spec/const3, implicit_qualifier_conversions, qualifier-convertible)
        to this type

Returns:
    `true` if R is an input range (possibly with element type `E`), `false` if not
 */
enum bool isInputRange(R) =
    is(typeof(R.init) == R)
    && is(typeof((R r) { return r.empty; } (R.init)) == bool)
    && (is(typeof((return ref R r) => r.front)) || is(typeof(ref (return ref R r) => r.front)))
    && !is(typeof((R r) { return r.front; } (R.init)) == void)
    && is(typeof((R r) => r.popFront));

/// ditto
enum bool isInputRange(R, E) =
    .isInputRange!R && isQualifierConvertible!(ElementType!R, E);

///
@safe unittest
{
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
// https://issues.dlang.org/show_bug.cgi?id=16034
@safe unittest
{
    struct One
    {
        int entry = 1;
        @disable this(this);
    }

    assert(isInputRange!(One[]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    static struct R
    {
        static struct Front
        {
            R* impl;
            @property int value() { return impl._front; }
            alias value this;
        }

        int _front;

        @property bool empty() { return _front >= 3; }
        @property auto front() { return Front(&this); }
        void popFront() { _front++; }
    }
    R r;

    static assert(isInputRange!R);
    assert(r.equal([ 0, 1, 2 ]));
}

/+
puts the whole raw element `e` into `r`. doPut will not attempt to
iterate, slice or transcode `e` in any way shape or form. It will $(B only)
call the correct primitive (`r.put(e)`,  $(D r.front = e) or
`r(e)` once.

This can be important when `e` needs to be placed in `r` unchanged.
Furthermore, it can be useful when working with `InputRange`s, as doPut
guarantees that no more than a single element will be placed.
+/
private void doPut(R, E)(ref R r, auto ref E e)
{
    static if (is(PointerTarget!R == struct))
        enum usingPut = hasMember!(PointerTarget!R, "put");
    else
        enum usingPut = hasMember!(R, "put");

    static if (usingPut)
    {
        static assert(is(typeof(r.put(e))),
            "Cannot put a " ~ E.stringof ~ " into a " ~ R.stringof ~ ".");
        r.put(e);
    }
    else static if (isNarrowString!R && is(const(E) == const(typeof(r[0]))))
    {
        // one character, we can put it
        r[0] = e;
        r = r[1 .. $];
    }
    else static if (isNarrowString!R && isNarrowString!E && is(typeof(r[] = e)))
    {
        // slice assign. Note that this is a duplicate from put, but because
        // putChar uses doPut exclusively, we have to copy it here.
        immutable len = e.length;
        r[0 .. len] = e;
        r = r[len .. $];
    }
    else static if (isInputRange!R)
    {
        static assert(is(typeof(r.front = e)),
            "Cannot put a " ~ E.stringof ~ " into a " ~ R.stringof ~ ".");
        r.front = e;
        r.popFront();
    }
    else static if (is(typeof(r(e))))
    {
        r(e);
    }
    else
    {
        static assert(false,
            "Cannot put a " ~ E.stringof ~ " into a " ~ R.stringof ~ ".");
    }
}

@safe unittest
{
    static assert(!isNativeOutputRange!(int,     int));
    static assert( isNativeOutputRange!(int[],   int));
    static assert(!isNativeOutputRange!(int[][], int));

    static assert(!isNativeOutputRange!(int,     int[]));
    static assert(!isNativeOutputRange!(int[],   int[]));
    static assert( isNativeOutputRange!(int[][], int[]));

    static assert(!isNativeOutputRange!(int,     int[][]));
    static assert(!isNativeOutputRange!(int[],   int[][]));
    static assert(!isNativeOutputRange!(int[][], int[][]));

    static assert(!isNativeOutputRange!(int[4],   int));
    static assert( isNativeOutputRange!(int[4][], int)); //Scary!
    static assert( isNativeOutputRange!(int[4][], int[4]));

    static assert( isNativeOutputRange!( char[],   char));
    static assert(!isNativeOutputRange!( char[],  dchar));
    static assert( isNativeOutputRange!(dchar[],   char));
    static assert( isNativeOutputRange!(dchar[],  dchar));

}

/++
Outputs `e` to `r`. The exact effect is dependent upon the two
types. Several cases are accepted, as described below. The code snippets
are attempted in order, and the first to compile "wins" and gets
evaluated.

In this table "doPut" is a method that places `e` into `r`, using the
correct primitive: `r.put(e)` if `R` defines `put`, $(D r.front = e)
if `r` is an input range (followed by `r.popFront()`), or `r(e)`
otherwise.

$(BOOKTABLE ,
    $(TR
        $(TH Code Snippet)
        $(TH Scenario)
    )
    $(TR
        $(TD `r.doPut(e);`)
        $(TD `R` specifically accepts an `E`.)
    )
    $(TR
        $(TD $(D r.doPut([ e ]);))
        $(TD `R` specifically accepts an `E[]`.)
    )
    $(TR
        $(TD `r.putChar(e);`)
        $(TD `R` accepts some form of string or character. put will
            transcode the character `e` accordingly.)
    )
    $(TR
        $(TD $(D for (; !e.empty; e.popFront()) put(r, e.front);))
        $(TD Copying range `E` into `R`.)
    )
)

Tip: `put` should $(I not) be used "UFCS-style", e.g. `r.put(e)`.
Doing this may call `R.put` directly, by-passing any transformation
feature provided by `Range.put`. $(D put(r, e)) is prefered.
 +/
void put(R, E)(ref R r, E e)
{
    //First level: simply straight up put.
    static if (is(typeof(doPut(r, e))))
    {
        doPut(r, e);
    }
    //Optional optimization block for straight up array to array copy.
    else static if (isDynamicArray!R &&
                    !isAutodecodableString!R &&
                    isDynamicArray!E &&
                    is(typeof(r[] = e[])))
    {
        immutable len = e.length;
        r[0 .. len] = e[];
        r = r[len .. $];
    }
    //Accepts E[] ?
    else static if (is(typeof(doPut(r, [e]))) && !isDynamicArray!R)
    {
        if (__ctfe)
        {
            E[1] arr = [e];
            doPut(r, arr[]);
        }
        else
            doPut(r, (ref e) @trusted { return (&e)[0 .. 1]; }(e));
    }
    //special case for char to string.
    else static if (isSomeChar!E && is(typeof(putChar(r, e))))
    {
        putChar(r, e);
    }
    //Extract each element from the range
    //We can use "put" here, so we can recursively test a RoR of E.
    else static if (isInputRange!E && is(typeof(put(r, e.front))))
    {
        //Special optimization: If E is a narrow string, and r accepts characters no-wider than the string's
        //Then simply feed the characters 1 by 1.
        static if (isAutodecodableString!E && !isAggregateType!E && (
            (is(E : const  char[]) && is(typeof(doPut(r,  char.max))) && !is(typeof(doPut(r, dchar.max))) &&
                !is(typeof(doPut(r, wchar.max)))) ||
            (is(E : const wchar[]) && is(typeof(doPut(r, wchar.max))) && !is(typeof(doPut(r, dchar.max)))) ) )
        {
            foreach (c; e)
                doPut(r, c);
        }
        else
        {
            for (; !e.empty; e.popFront())
                put(r, e.front);
        }
    }
    else
    {
        static assert(false, "Cannot put a " ~ E.stringof ~ " into a " ~ R.stringof ~ ".");
    }
}

/**
 * When an output range's `put` method only accepts elements of type
 * `T`, use the global `put` to handle outputting a `T[]` to the range
 * or vice-versa.
 */
@safe pure unittest
{
    import std.traits : isSomeChar;

    static struct A
    {
        string data;

        void put(C)(C c) if (isSomeChar!C)
        {
            data ~= c;
        }
    }
    static assert(isOutputRange!(A, char));

    auto a = A();
    put(a, "Hello");
    assert(a.data == "Hello");
}

/**
 * `put` treats dynamic arrays as array slices, and will call `popFront`
 * on the slice after an element has been copied.
 *
 * Be sure to save the position of the array before calling `put`.
 */
@safe pure nothrow unittest
{
    int[] a = [1, 2, 3], b = [10, 20];
    auto c = a;
    put(a, b);
    assert(c == [10, 20, 3]);
    // at this point, a was advanced twice, so it only contains
    // its last element while c represents the whole array
    assert(a == [3]);
}

/**
 * It's also possible to `put` any width strings or characters into narrow
 * strings -- put does the conversion for you.
 *
 * Note that putting the same width character as the target buffer type is
 * `nothrow`, but transcoding can throw a $(REF UTFException, std, utf).
 */
@safe pure unittest
{
    // the elements must be mutable, so using string or const(char)[]
    // won't compile
    char[] s1 = new char[13];
    auto r1 = s1;
    put(r1, "Hello, World!"w);
    assert(s1 == "Hello, World!");
}

@safe pure nothrow unittest
{
    // same thing, just using same character width.
    char[] s1 = new char[13];
    auto r1 = s1;
    put(r1, "Hello, World!");
    assert(s1 == "Hello, World!");
}


@safe pure nothrow @nogc unittest
{
    static struct R() { void put(scope const(char)[]) {} }
    R!() r;
    put(r, 'a');
}

//Helper function to handle chars as quickly and as elegantly as possible
//Assumes r.put(e)/r(e) has already been tested
private void putChar(R, E)(ref R r, E e)
if (isSomeChar!E)
{
    // https://issues.dlang.org/show_bug.cgi?id=9186: Can't use (E[]).init
    enum csCond = is(typeof(doPut(r, (){ ref const( char)[] cstringInit(); return cstringInit(); }())));
    enum wsCond = is(typeof(doPut(r, (){ ref const(wchar)[] wstringInit(); return wstringInit(); }())));
    enum dsCond = is(typeof(doPut(r, (){ ref const(dchar)[] dstringInit(); return dstringInit(); }())));

    //Use "max" to avoid static type demotion
    enum ccCond = is(typeof(doPut(r,  char.max)));
    enum wcCond = is(typeof(doPut(r, wchar.max)));
    //enum dcCond = is(typeof(doPut(r, dchar.max)));

    //Fast transform a narrow char into a wider string
    static if ((wsCond && E.sizeof < wchar.sizeof) || (dsCond && E.sizeof < dchar.sizeof))
    {
        enum w = wsCond && E.sizeof < wchar.sizeof;
        Select!(w, wchar, dchar) c = e;
        typeof(c)[1] arr = [c];
        doPut(r, arr[]);
    }
    //Encode a wide char into a narrower string
    else static if (wsCond || csCond)
    {
        import std.utf : encode;
        /+static+/ Select!(wsCond, wchar[2], char[4]) buf; //static prevents purity.
        doPut(r, buf[0 .. encode(buf, e)]);
    }
    //Slowly encode a wide char into a series of narrower chars
    else static if (wcCond || ccCond)
    {
        import std.encoding : encode;
        alias C = Select!(wcCond, wchar, char);
        encode!(C, R)(e, r);
    }
    else
    {
        static assert(false, "Cannot put a " ~ E.stringof ~ " into a " ~ R.stringof ~ ".");
    }
}

pure @safe unittest
{
    auto f = delegate (const(char)[]) {};
    putChar(f, cast(dchar)'a');
}


@safe pure unittest
{
    static struct R() { void put(scope const(char)[]) {} }
    R!() r;
    putChar(r, 'a');
}

@safe unittest
{
    struct A {}
    static assert(!isInputRange!(A));
    struct B
    {
        void put(int) {}
    }
    B b;
    put(b, 5);
}

@safe unittest
{
    int[] a = new int[10];
    int b;
    static assert(isInputRange!(typeof(a)));
    put(a, b);
}

@safe unittest
{
    void myprint(scope const(char)[] s) { }
    auto r = &myprint;
    put(r, 'a');
}

@safe unittest
{
    int[] a = new int[10];
    static assert(!__traits(compiles, put(a, 1.0L)));
    put(a, 1);
    assert(a.length == 9);
    /*
     * a[0] = 65;       // OK
     * a[0] = 'A';      // OK
     * a[0] = "ABC"[0]; // OK
     * put(a, "ABC");   // OK
     */
    put(a, "ABC");
    assert(a.length == 6);
}

@safe unittest
{
    char[] a = new char[10];
    static assert(!__traits(compiles, put(a, 1.0L)));
    static assert(!__traits(compiles, put(a, 1)));
    //char[] is now an output range for char, wchar, dchar, and ranges of such.
    static assert(__traits(compiles, putChar(a, 'a')));
    static assert(__traits(compiles, put(a, wchar('a'))));
    static assert(__traits(compiles, put(a, dchar('a'))));
    static assert(__traits(compiles, put(a, "ABC")));
    static assert(__traits(compiles, put(a, "ABC"w)));
    static assert(__traits(compiles, put(a, "ABC"d)));
}

@safe unittest
{
    // attempt putting into narrow strings by transcoding
    char[] a = new char[10];
    auto b = a;
    put(a, "ABC"w);
    assert(b[0 .. 3] == "ABC");
    assert(a.length == 7);

    a = b; // reset
    put(a, 'λ');
    assert(b[0 .. 2] == "λ");
    assert(a.length == 8);

    a = b; // reset
    put(a, "ABC"d);
    assert(b[0 .. 3] == "ABC");
    assert(a.length == 7);

    a = b; // reset
    put(a, '𐐷');
    assert(b[0 .. 4] == "𐐷");
    assert(a.length == 6);

    wchar[] aw = new wchar[10];
    auto bw = aw;
    put(aw, "ABC");
    assert(bw[0 .. 3] == "ABC"w);
    assert(aw.length == 7);

    aw = bw; // reset
    put(aw, 'λ');
    assert(bw[0 .. 1] == "λ"w);
    assert(aw.length == 9);

    aw = bw; // reset
    put(aw, "ABC"d);
    assert(bw[0 .. 3] == "ABC"w);
    assert(aw.length == 7);

    aw = bw; // reset
    put(aw, '𐐷');
    assert(bw[0 .. 2] == "𐐷"w);
    assert(aw.length == 8);

    aw = bw; // reset
    put(aw, "𐐷"); // try transcoding from char[]
    assert(bw[0 .. 2] == "𐐷"w);
    assert(aw.length == 8);
}

@safe unittest
{
    int[][] a = new int[][10];
    int[]   b = new int[10];
    int     c;
    put(b, c);
    assert(b.length == 9);
    put(a, b);
    assert(a.length == 9);
    static assert(!__traits(compiles, put(a, c)));
}

@safe unittest
{
    int[][] a = new int[][](3);
    int[]   b = [1];
    auto aa = a;
    put(aa, b);
    assert(aa == [[], []]);
    assert(a  == [[1], [], []]);
    int[][3] c = [2];
    aa = a;
    put(aa, c[]);
    assert(aa.empty);
    assert(a == [[2], [2], [2]]);
}

@safe unittest
{
    // Test fix for bug 7476.
    struct LockingTextWriter
    {
        void put(dchar c){}
    }
    struct RetroResult
    {
        bool end = false;
        @property bool empty() const { return end; }
        @property dchar front(){ return 'a'; }
        void popFront(){ end = true; }
    }
    LockingTextWriter w;
    RetroResult re;
    put(w, re);
}

@system unittest
{
    import std.conv : to;
    import std.meta : AliasSeq;
    import std.typecons : tuple;

    static struct PutC(C)
    {
        string result;
        void put(const(C) c) { result ~= to!string((&c)[0 .. 1]); }
    }
    static struct PutS(C)
    {
        string result;
        void put(const(C)[] s) { result ~= to!string(s); }
    }
    static struct PutSS(C)
    {
        string result;
        void put(const(C)[][] ss)
        {
            foreach (s; ss)
                result ~= to!string(s);
        }
    }

    PutS!char p;
    putChar(p, cast(dchar)'a');

    //Source Char
    static foreach (SC; AliasSeq!(char, wchar, dchar))
    {{
        SC ch = 'I';
        dchar dh = '♥';
        immutable(SC)[] s = "日本語！";
        immutable(SC)[][] ss = ["日本語", "が", "好き", "ですか", "？"];

        //Target Char
        static foreach (TC; AliasSeq!(char, wchar, dchar))
        {
            //Testing PutC and PutS
            static foreach (Type; AliasSeq!(PutC!TC, PutS!TC))
            {{
                Type type;
                auto sink = new Type();

                //Testing put and sink
                foreach (value ; tuple(type, sink))
                {
                    put(value, ch);
                    assert(value.result == "I");
                    put(value, dh);
                    assert(value.result == "I♥");
                    put(value, s);
                    assert(value.result == "I♥日本語！");
                    put(value, ss);
                    assert(value.result == "I♥日本語！日本語が好きですか？");
                }
            }}
        }
    }}
}

@safe unittest
{
    static struct CharRange
    {
        char c;
        enum empty = false;
        void popFront(){}
        ref char front() return @property
        {
            return c;
        }
    }
    CharRange c;
    put(c, cast(dchar)'H');
    put(c, "hello"d);
}

// https://issues.dlang.org/show_bug.cgi?id=9823
@system unittest
{
    const(char)[] r;
    void delegate(const(char)[]) dg = (s) { r = s; };
    put(dg, ["ABC"]);
    assert(r == "ABC");
}

// https://issues.dlang.org/show_bug.cgi?id=10571
@safe unittest
{
    import std.format.write : formattedWrite;
    string buf;
    formattedWrite((scope const(char)[] s) { buf ~= s; }, "%s", "hello");
    assert(buf == "hello");
}

@safe unittest
{
    import std.format.write : formattedWrite;
    import std.meta : AliasSeq;
    struct PutC(C)
    {
        void put(C){}
    }
    struct PutS(C)
    {
        void put(const(C)[]){}
    }
    struct CallC(C)
    {
        void opCall(C){}
    }
    struct CallS(C)
    {
        void opCall(const(C)[]){}
    }
    struct FrontC(C)
    {
        enum empty = false;
        auto front()@property{return C.init;}
        void front(C)@property{}
        void popFront(){}
    }
    struct FrontS(C)
    {
        enum empty = false;
        auto front()@property{return C[].init;}
        void front(const(C)[])@property{}
        void popFront(){}
    }
    void foo()
    {
        static foreach (C; AliasSeq!(char, wchar, dchar))
        {{
            formattedWrite((C c){},        "", 1, 'a', cast(wchar)'a', cast(dchar)'a', "a"c, "a"w, "a"d);
            formattedWrite((const(C)[]){}, "", 1, 'a', cast(wchar)'a', cast(dchar)'a', "a"c, "a"w, "a"d);
            formattedWrite(PutC!C(),       "", 1, 'a', cast(wchar)'a', cast(dchar)'a', "a"c, "a"w, "a"d);
            formattedWrite(PutS!C(),       "", 1, 'a', cast(wchar)'a', cast(dchar)'a', "a"c, "a"w, "a"d);
            CallC!C callC;
            CallS!C callS;
            formattedWrite(callC,          "", 1, 'a', cast(wchar)'a', cast(dchar)'a', "a"c, "a"w, "a"d);
            formattedWrite(callS,          "", 1, 'a', cast(wchar)'a', cast(dchar)'a', "a"c, "a"w, "a"d);
            formattedWrite(FrontC!C(),     "", 1, 'a', cast(wchar)'a', cast(dchar)'a', "a"c, "a"w, "a"d);
            formattedWrite(FrontS!C(),     "", 1, 'a', cast(wchar)'a', cast(dchar)'a', "a"c, "a"w, "a"d);
        }}
        formattedWrite((dchar[]).init,     "", 1, 'a', cast(wchar)'a', cast(dchar)'a', "a"c, "a"w, "a"d);
    }
}

/+
Returns `true` if `R` is a native output range for elements of type
`E`. An output range is defined functionally as a range that
supports the operation $(D doPut(r, e)) as defined above. if $(D doPut(r, e))
is valid, then `put(r,e)` will have the same behavior.

The two guarantees isNativeOutputRange gives over the larger `isOutputRange`
are:
1: `e` is $(B exactly) what will be placed (not `[e]`, for example).
2: if `E` is a non $(empty) `InputRange`, then placing `e` is
guaranteed to not overflow the range.
 +/
package(std) enum bool isNativeOutputRange(R, E) =
    is(typeof(doPut(lvalueOf!R, lvalueOf!E)));

@safe unittest
{
    int[] r = new int[](4);
    static assert(isInputRange!(int[]));
    static assert( isNativeOutputRange!(int[], int));
    static assert(!isNativeOutputRange!(int[], int[]));
    static assert( isOutputRange!(int[], int[]));

    if (!r.empty)
        put(r, 1); //guaranteed to succeed
    if (!r.empty)
        put(r, [1, 2]); //May actually error out.
}

/++
Returns `true` if `R` is an output range for elements of type
`E`. An output range is defined functionally as a range that
supports the operation $(D put(r, e)) as defined above.

See_Also:
    The header of $(MREF std,range) for tutorials on ranges.
 +/
enum bool isOutputRange(R, E) =
    is(typeof(put(lvalueOf!R, lvalueOf!E)));

///
@safe unittest
{
    void myprint(scope const(char)[] s) { }
    static assert(isOutputRange!(typeof(&myprint), char));

    static assert( isOutputRange!(char[], char));
    static assert( isOutputRange!(dchar[], wchar));
    static assert( isOutputRange!(dchar[], dchar));
}

@safe unittest
{
    import std.array;
    import std.stdio : writeln;

    auto app = appender!string();
    string s;
    static assert( isOutputRange!(Appender!string, string));
    static assert( isOutputRange!(Appender!string*, string));
    static assert(!isOutputRange!(Appender!string, int));
    static assert( isOutputRange!(wchar[], wchar));
    static assert( isOutputRange!(dchar[], char));
    static assert( isOutputRange!(dchar[], string));
    static assert( isOutputRange!(dchar[], wstring));
    static assert( isOutputRange!(dchar[], dstring));

    static assert(!isOutputRange!(const(int)[], int));
    static assert(!isOutputRange!(inout(int)[], int));
}


/**
Returns `true` if `R` is a forward range. A forward range is an
input range `r` that can save "checkpoints" by saving `r.save`
to another value of type `R`. Notable examples of input ranges that
are $(I not) forward ranges are file/socket ranges; copying such a
range will not save the position in the stream, and they most likely
reuse an internal buffer as the entire stream does not sit in
memory. Subsequently, advancing either the original or the copy will
advance the stream, so the copies are not independent.

The following code should compile for any forward range.

----
static assert(isInputRange!R);
R r1;
auto s1 = r1.save;
static assert(is(typeof(s1) == R));
----

Saving a range is not duplicating it; in the example above, `r1`
and `r2` still refer to the same underlying data. They just
navigate that data independently.

The semantics of a forward range (not checkable during compilation)
are the same as for an input range, with the additional requirement
that backtracking must be possible by saving a copy of the range
object with `save` and using it later.

`save` behaves in many ways like a copy constructor, and its
implementation typically is done using copy construction.

The existence of a copy constructor, however, does not imply
the range is a forward range. For example, a range that reads
from a TTY consumes its input and cannot save its place and
read it again, and so cannot be a forward range and cannot
have a `save` function.


See_Also:
    The header of $(MREF std,range) for tutorials on ranges.

Params:
    R = type to be tested
    E = if present, the elements of the range must be
        $(DDSUBLINK spec/const3, implicit_qualifier_conversions, qualifier-convertible)
        to this type

Returns:
    `true` if R is a forward range (possibly with element type `E`), `false` if not
 */
enum bool isForwardRange(R) = isInputRange!R
    && is(typeof((R r) { return r.save; } (R.init)) == R);

/// ditto
enum bool isForwardRange(R, E) =
    .isForwardRange!R && isQualifierConvertible!(ElementType!R, E);

///
@safe unittest
{
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
    // BUG 14544
    struct R14544
    {
        int front() { return 0;}
        void popFront() {}
        bool empty() { return false; }
        R14544 save() {return this;}
    }

    static assert( isForwardRange!R14544 );
}

/**
Returns `true` if `R` is a bidirectional range. A bidirectional
range is a forward range that also offers the primitives `back` and
`popBack`. The following code should compile for any bidirectional
range.

The semantics of a bidirectional range (not checkable during
compilation) are assumed to be the following (`r` is an object of
type `R`):

$(UL $(LI `r.back` returns (possibly a reference to) the last
element in the range. Calling `r.back` is allowed only if calling
`r.empty` has, or would have, returned `false`.))

See_Also:
    The header of $(MREF std,range) for tutorials on ranges.

Params:
    R = type to be tested
    E = if present, the elements of the range must be
        $(DDSUBLINK spec/const3, implicit_qualifier_conversions, qualifier-convertible)
        to this type

Returns:
    `true` if R is a bidirectional range (possibly with element type `E`), `false` if not
 */
enum bool isBidirectionalRange(R) = isForwardRange!R
    && is(typeof((R r) => r.popBack))
    && (is(typeof((return ref R r) => r.back)) || is(typeof(ref (return ref R r) => r.back)))
    && is(typeof(R.init.back.init) == ElementType!R);

/// ditto
enum bool isBidirectionalRange(R, E) =
    .isBidirectionalRange!R && isQualifierConvertible!(ElementType!R, E);

///
@safe unittest
{
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
    struct A {}
    struct B
    {
        void popFront();
        @property bool empty();
        @property int front();
    }
    struct C
    {
        @property bool empty();
        @property C save();
        void popFront();
        @property int front();
        void popBack();
        @property int back();
    }
    static assert(!isBidirectionalRange!(A));
    static assert(!isBidirectionalRange!(B));
    static assert( isBidirectionalRange!(C));
    static assert( isBidirectionalRange!(int[]));
    static assert( isBidirectionalRange!(char[]));
    static assert( isBidirectionalRange!(inout(int)[]));
}

/**
Returns `true` if `R` is a random-access range. A random-access
range is a bidirectional range that also offers the primitive $(D
opIndex), OR an infinite forward range that offers `opIndex`. In
either case, the range must either offer `length` or be
infinite. The following code should compile for any random-access
range.

The semantics of a random-access range (not checkable during
compilation) are assumed to be the following (`r` is an object of
type `R`): $(UL $(LI `r.opIndex(n)` returns a reference to the
`n`th element in the range.))

Although `char[]` and `wchar[]` (as well as their qualified
versions including `string` and `wstring`) are arrays, $(D
isRandomAccessRange) yields `false` for them because they use
variable-length encodings (UTF-8 and UTF-16 respectively). These types
are bidirectional ranges only.

See_Also:
    The header of $(MREF std,range) for tutorials on ranges.

Params:
    R = type to be tested
    E = if present, the elements of the range must be
        $(DDSUBLINK spec/const3, implicit_qualifier_conversions, qualifier-convertible)
        to this type

Returns:
    `true` if R is a random-access range (possibly with element type `E`), `false` if not
 */
enum bool isRandomAccessRange(R) =
    is(typeof(lvalueOf!R[1]) == ElementType!R)
    && !(isAutodecodableString!R && !isAggregateType!R)
    && isForwardRange!R
    && (isBidirectionalRange!R || isInfinite!R)
    && (hasLength!R || isInfinite!R)
    && (isInfinite!R || !is(typeof(lvalueOf!R[$ - 1]))
        || is(typeof(lvalueOf!R[$ - 1]) == ElementType!R));

/// ditto
enum bool isRandomAccessRange(R, E) =
    .isRandomAccessRange!R && isQualifierConvertible!(ElementType!R, E);

///
@safe unittest
{
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
    struct A {}
    struct B
    {
        void popFront();
        @property bool empty();
        @property int front();
    }
    struct C
    {
        void popFront();
        @property bool empty();
        @property int front();
        void popBack();
        @property int back();
    }
    struct D
    {
        @property bool empty();
        @property D save();
        @property int front();
        void popFront();
        @property int back();
        void popBack();
        ref int opIndex(uint);
        @property size_t length();
        alias opDollar = length;
        //int opSlice(uint, uint);
    }
    struct E
    {
        bool empty();
        E save();
        int front();
        void popFront();
        int back();
        void popBack();
        ref int opIndex(uint);
        size_t length();
        alias opDollar = length;
        //int opSlice(uint, uint);
    }
    static assert(!isRandomAccessRange!(A));
    static assert(!isRandomAccessRange!(B));
    static assert(!isRandomAccessRange!(C));
    static assert( isRandomAccessRange!(D));
    static assert( isRandomAccessRange!(E));
    static assert( isRandomAccessRange!(int[]));
    static assert( isRandomAccessRange!(inout(int)[]));
}

@safe unittest
{
    // Test fix for bug 6935.
    struct R
    {
        @disable this();

        @property bool empty() const { return false; }
        @property int front() const { return 0; }
        void popFront() {}

        @property R save() { return this; }

        @property int back() const { return 0; }
        void popBack(){}

        int opIndex(size_t n) const { return 0; }
        @property size_t length() const { return 0; }
        alias opDollar = length;

        void put(int e){  }
    }
    static assert(isInputRange!R);
    static assert(isForwardRange!R);
    static assert(isBidirectionalRange!R);
    static assert(isRandomAccessRange!R);
    static assert(isOutputRange!(R, int));
}

/**
Returns `true` iff `R` is an input range that supports the
`moveFront` primitive, as well as `moveBack` and `moveAt` if it's a
bidirectional or random access range. These may be explicitly implemented, or
may work via the default behavior of the module level functions `moveFront`
and friends. The following code should compile for any range
with mobile elements.

----
alias E = ElementType!R;
R r;
static assert(isInputRange!R);
static assert(is(typeof(moveFront(r)) == E));
static if (isBidirectionalRange!R)
    static assert(is(typeof(moveBack(r)) == E));
static if (isRandomAccessRange!R)
    static assert(is(typeof(moveAt(r, 0)) == E));
----
 */
enum bool hasMobileElements(R) =
    isInputRange!R
    && is(typeof(moveFront(lvalueOf!R)) == ElementType!R)
    && (!isBidirectionalRange!R
        || is(typeof(moveBack(lvalueOf!R)) == ElementType!R))
    && (!isRandomAccessRange!R
        || is(typeof(moveAt(lvalueOf!R, 0)) == ElementType!R));

///
@safe unittest
{
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

/**
The element type of `R`. `R` does not have to be a range. The
element type is determined as the type yielded by `r.front` for an
object `r` of type `R`. For example, `ElementType!(T[])` is
`T` if `T[]` isn't a narrow string; if it is, the element type is
`dchar`. If `R` doesn't have `front`, `ElementType!R` is
`void`.
 */
template ElementType(R)
{
    static if (is(typeof(R.init.front.init) T))
        alias ElementType = T;
    else
        alias ElementType = void;
}

///
@safe unittest
{
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
    static assert(is(ElementType!(byte[]) == byte));
    static assert(is(ElementType!(wchar[]) == dchar)); // rvalue
    static assert(is(ElementType!(wstring) == dchar));
}

@safe unittest
{
    enum XYZ : string { a = "foo" }
    auto x = XYZ.a.front;
    immutable char[3] a = "abc";
    int[] i;
    void[] buf;
    static assert(is(ElementType!(XYZ) == dchar));
    static assert(is(ElementType!(typeof(a)) == dchar));
    static assert(is(ElementType!(typeof(i)) == int));
    static assert(is(ElementType!(typeof(buf)) == void));
    static assert(is(ElementType!(inout(int)[]) == inout(int)));
    static assert(is(ElementType!(inout(int[])) == inout(int)));
}

@safe unittest
{
    static assert(is(ElementType!(int[5]) == int));
    static assert(is(ElementType!(int[0]) == int));
    static assert(is(ElementType!(char[5]) == dchar));
    static assert(is(ElementType!(char[0]) == dchar));
}

// https://issues.dlang.org/show_bug.cgi?id=11336
@safe unittest
{
    static struct S
    {
        this(this) @disable;
    }
    static assert(is(ElementType!(S[]) == S));
}

// https://issues.dlang.org/show_bug.cgi?id=11401
@safe unittest
{
    // ElementType should also work for non-@propety 'front'
    struct E { ushort id; }
    struct R
    {
        E front() { return E.init; }
    }
    static assert(is(ElementType!R == E));
}

/**
The encoding element type of `R`. For narrow strings (`char[]`,
`wchar[]` and their qualified variants including `string` and
`wstring`), `ElementEncodingType` is the character type of the
string. For all other types, `ElementEncodingType` is the same as
`ElementType`.
 */
template ElementEncodingType(R)
{
    static if (is(StringTypeOf!R) && is(R : E[], E))
        alias ElementEncodingType = E;
    else
        alias ElementEncodingType = ElementType!R;
}

///
@safe unittest
{
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
    static assert(is(ElementEncodingType!(wchar[]) == wchar));
    static assert(is(ElementEncodingType!(dchar[]) == dchar));
    static assert(is(ElementEncodingType!(string)  == immutable(char)));
    static assert(is(ElementEncodingType!(dstring) == immutable(dchar)));
    static assert(is(ElementEncodingType!(int[])  == int));
}

@safe unittest
{
    enum XYZ : string { a = "foo" }
    auto x = XYZ.a.front;
    immutable char[3] a = "abc";
    int[] i;
    void[] buf;
    static assert(is(ElementType!(XYZ) : dchar));
    static assert(is(ElementEncodingType!(char[]) == char));
    static assert(is(ElementEncodingType!(string) == immutable char));
    static assert(is(ElementType!(typeof(a)) : dchar));
    static assert(is(ElementType!(typeof(i)) == int));
    static assert(is(ElementEncodingType!(typeof(i)) == int));
    static assert(is(ElementType!(typeof(buf)) : void));

    static assert(is(ElementEncodingType!(inout char[]) : inout(char)));
}

@safe unittest
{
    static assert(is(ElementEncodingType!(int[5]) == int));
    static assert(is(ElementEncodingType!(int[0]) == int));
    static assert(is(ElementEncodingType!(char[5]) == char));
    static assert(is(ElementEncodingType!(char[0]) == char));
}

/**
Returns `true` if `R` is an input range and has swappable
elements. The following code should compile for any range
with swappable elements.

----
R r;
static assert(isInputRange!R);
swap(r.front, r.front);
static if (isBidirectionalRange!R) swap(r.back, r.front);
static if (isRandomAccessRange!R) swap(r[0], r.front);
----
 */
template hasSwappableElements(R)
{
    import std.algorithm.mutation : swap;
    enum bool hasSwappableElements = isInputRange!R
        && is(typeof((ref R r) => swap(r.front, r.front)))
        && (!isBidirectionalRange!R
            || is(typeof((ref R r) => swap(r.back, r.front))))
        && (!isRandomAccessRange!R
            || is(typeof((ref R r) => swap(r[0], r.front))));
}

///
@safe unittest
{
    static assert(!hasSwappableElements!(const int[]));
    static assert(!hasSwappableElements!(const(int)[]));
    static assert(!hasSwappableElements!(inout(int)[]));
    static assert( hasSwappableElements!(int[]));

    static assert(!hasSwappableElements!( string));
    static assert(!hasSwappableElements!(dstring));
    static assert(!hasSwappableElements!( char[]));
    static assert( hasSwappableElements!(dchar[]));
}

/**
Returns `true` if `R` is an input range and has mutable
elements. The following code should compile for any range
with assignable elements.

----
R r;
static assert(isInputRange!R);
r.front = r.front;
static if (isBidirectionalRange!R) r.back = r.front;
static if (isRandomAccessRange!R) r[0] = r.front;
----
 */
enum bool hasAssignableElements(R) = isInputRange!R
    && is(typeof(lvalueOf!R.front = lvalueOf!R.front))
    && (!isBidirectionalRange!R
        || is(typeof(lvalueOf!R.back = lvalueOf!R.back)))
    && (!isRandomAccessRange!R
        || is(typeof(lvalueOf!R[0] = lvalueOf!R.front)));

///
@safe unittest
{
    static assert(!hasAssignableElements!(const int[]));
    static assert(!hasAssignableElements!(const(int)[]));
    static assert( hasAssignableElements!(int[]));
    static assert(!hasAssignableElements!(inout(int)[]));

    static assert(!hasAssignableElements!( string));
    static assert(!hasAssignableElements!(dstring));
    static assert(!hasAssignableElements!( char[]));
    static assert( hasAssignableElements!(dchar[]));
}

/**
Tests whether the range `R` has lvalue elements. These are defined as
elements that can be passed by reference and have their address taken.
The following code should compile for any range with lvalue elements.
----
void passByRef(ref ElementType!R stuff);
...
static assert(isInputRange!R);
passByRef(r.front);
static if (isBidirectionalRange!R) passByRef(r.back);
static if (isRandomAccessRange!R) passByRef(r[0]);
----
*/
enum bool hasLvalueElements(R) = isInputRange!R
    && is(typeof(isLvalue(lvalueOf!R.front)))
    && (!isBidirectionalRange!R
        || is(typeof(isLvalue(lvalueOf!R.back))))
    && (!isRandomAccessRange!R
        || is(typeof(isLvalue(lvalueOf!R[0]))));

/* Compile successfully if argument of type T is an lvalue
 */
private void isLvalue(T)(T)
if (0);

private void isLvalue(T)(ref T)
if (1);

///
@safe unittest
{
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
    // bugfix 6336
    struct S { immutable int value; }
    static assert( isInputRange!(S[]));
    static assert( hasLvalueElements!(S[]));
}

/**
Yields `true` if `R` has a `length` member that returns a value of `size_t`
type. `R` does not have to be a range. If `R` is a range, algorithms in the
standard library are only guaranteed to support `length` with type `size_t`.

Note that `length` is an optional primitive as no range must implement it. Some
ranges do not store their length explicitly, some cannot compute it without
actually exhausting the range (e.g. socket streams), and some other ranges may
be infinite.

Although narrow string types (`char[]`, `wchar[]`, and their qualified
derivatives) do define a `length` property, `hasLength` yields `false` for them.
This is because a narrow string's length does not reflect the number of
characters, but instead the number of encoding units, and as such is not useful
with range-oriented algorithms. To use strings as random-access ranges with
length, use $(REF representation, std, string) or $(REF byCodeUnit, std, utf).
*/
template hasLength(R)
{
    static if (is(typeof(((R* r) => r.length)(null)) Length))
        enum bool hasLength = is(Length == size_t) &&
                              !(isAutodecodableString!R && !isAggregateType!R);
    else
        enum bool hasLength = false;
}

///
@safe unittest
{
    static assert(!hasLength!(char[]));
    static assert( hasLength!(int[]));
    static assert( hasLength!(inout(int)[]));

    struct A { size_t length() { return 0; } }
    struct B { @property size_t length() { return 0; } }
    static assert( hasLength!(A));
    static assert( hasLength!(B));
}

// test combinations which are invalid on some platforms
@safe unittest
{
    struct A { ulong length; }
    struct B { @property uint length() { return 0; } }

    static if (is(size_t == uint))
    {
        static assert(!hasLength!(A));
        static assert(hasLength!(B));
    }
    else static if (is(size_t == ulong))
    {
        static assert(hasLength!(A));
        static assert(!hasLength!(B));
    }
}

// test combinations which are invalid on all platforms
@safe unittest
{
    struct A { long length; }
    struct B { int length; }
    struct C { ubyte length; }
    struct D { char length; }
    static assert(!hasLength!(A));
    static assert(!hasLength!(B));
    static assert(!hasLength!(C));
    static assert(!hasLength!(D));
}

/**
Returns `true` if `R` is an infinite input range. An
infinite input range is an input range that has a statically-defined
enumerated member called `empty` that is always `false`,
for example:

----
struct MyInfiniteRange
{
    enum bool empty = false;
    ...
}
----
 */

template isInfinite(R)
{
    static if (isInputRange!R && __traits(compiles, { enum e = R.empty; }))
        enum bool isInfinite = !R.empty;
    else
        enum bool isInfinite = false;
}

///
@safe unittest
{
    import std.range : Repeat;
    static assert(!isInfinite!(int[]));
    static assert( isInfinite!(Repeat!(int)));
}

/**
Returns `true` if `R` offers a slicing operator with integral boundaries
that returns a forward range type.

For finite ranges, the result of `opSlice` must be of the same type as the
original range type. If the range defines `opDollar`, then it must support
subtraction.

For infinite ranges, when $(I not) using `opDollar`, the result of `opSlice`
may be a forward range of any type. However, when using `opDollar`, the result
of `opSlice` must be of the same type as the original range type.

The following expression must be true for `hasSlicing` to be `true`:

----
    isForwardRange!R
    && !(isAutodecodableString!R && !isAggregateType!R)
    && is(typeof((R r) { return r[1 .. 1].length; } (R.init)) == size_t)
    && (is(typeof(lvalueOf!R[1 .. 1]) == R) || isInfinite!R)
    && (!is(typeof(lvalueOf!R[0 .. $])) || is(typeof(lvalueOf!R[0 .. $]) == R))
    && (!is(typeof(lvalueOf!R[0 .. $])) || isInfinite!R
        || is(typeof(lvalueOf!R[0 .. $ - 1]) == R))
    && is(typeof((ref R r)
    {
        static assert(isForwardRange!(typeof(r[1 .. 2])));
    }));
----
 */
enum bool hasSlicing(R) = isForwardRange!R
    && !(isAutodecodableString!R && !isAggregateType!R)
    && is(typeof((R r) { return r[1 .. 1].length; } (R.init)) == size_t)
    && (is(typeof(lvalueOf!R[1 .. 1]) == R) || isInfinite!R)
    && (!is(typeof(lvalueOf!R[0 .. $])) || is(typeof(lvalueOf!R[0 .. $]) == R))
    && (!is(typeof(lvalueOf!R[0 .. $])) || isInfinite!R
        || is(typeof(lvalueOf!R[0 .. $ - 1]) == R))
    && is(typeof((ref R r)
    {
        static assert(isForwardRange!(typeof(r[1 .. 2])));
    }));

///
@safe unittest
{
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

// https://issues.dlang.org/show_bug.cgi?id=24348
@safe unittest
{
    static struct Slice
    {
        size_t length;
        bool empty() => length == 0;
        int front() => 0;
        void popFront() { --length; }
        Slice save() => this;
    }

    static struct InfZeros
    {
        enum empty = false;
        int front() => 0;
        void popFront() {}
        InfZeros save() => this;

        Slice opIndex(size_t[2] bounds)
        {
            size_t i = bounds[0], j = bounds[1];
            size_t length = i <= j ? j - i : 0;
            return Slice(length);
        }

        size_t[2] opSlice(size_t dim : 0)(size_t i, size_t j) => [i, j];
    }

    static assert(hasSlicing!InfZeros);
}

/**
This is a best-effort implementation of `length` for any kind of
range.

If `hasLength!Range`, simply returns `range.length` without
checking `upTo` (when specified).

Otherwise, walks the range through its length and returns the number
of elements seen. Performes $(BIGOH n) evaluations of `range.empty`
and `range.popFront()`, where `n` is the effective length of $(D
range).

The `upTo` parameter is useful to "cut the losses" in case
the interest is in seeing whether the range has at least some number
of elements. If the parameter `upTo` is specified, stops if $(D
upTo) steps have been taken and returns `upTo`.

Infinite ranges are compatible, provided the parameter `upTo` is
specified, in which case the implementation simply returns upTo.
 */
auto walkLength(Range)(Range range)
if (isInputRange!Range && !isInfinite!Range)
{
    static if (hasLength!Range)
        return range.length;
    else
    {
        size_t result;
        static if (autodecodeStrings && isNarrowString!Range)
        {
            import std.utf : codeUnitLimit;
            result = range.length;
            foreach (const i, const c; range)
            {
                if (c >= codeUnitLimit!Range)
                {
                    result = i;
                    break;
                }
            }
            range = range[result .. $];
        }
        for ( ; !range.empty ; range.popFront() )
            ++result;
        return result;
    }
}
/// ditto
auto walkLength(Range)(Range range, const size_t upTo)
if (isInputRange!Range)
{
    static if (hasLength!Range)
        return range.length;
    else static if (isInfinite!Range)
        return upTo;
    else
    {
        size_t result;
        static if (autodecodeStrings && isNarrowString!Range)
        {
            import std.utf : codeUnitLimit;
            result = upTo > range.length ? range.length : upTo;
            foreach (const i, const c; range[0 .. result])
            {
                if (c >= codeUnitLimit!Range)
                {
                    result = i;
                    break;
                }
            }
            range = range[result .. $];
        }
        for ( ; result < upTo && !range.empty ; range.popFront() )
            ++result;
        return result;
    }
}

///
@safe unittest
{
    import std.range : iota;

    assert(10.iota.walkLength == 10);
    // iota has a length function, and therefore the
    // doesn't have to be walked, and the upTo
    // parameter is ignored
    assert(10.iota.walkLength(5) == 10);
}

@safe unittest
{
    import std.algorithm.iteration : filter;
    import std.range : recurrence, take;

    //hasLength Range
    int[] a = [ 1, 2, 3 ];
    assert(walkLength(a) == 3);
    assert(walkLength(a, 0) == 3);
    assert(walkLength(a, 2) == 3);
    assert(walkLength(a, 4) == 3);

    //Forward Range
    auto b = filter!"true"([1, 2, 3, 4]);
    assert(b.walkLength() == 4);
    assert(b.walkLength(0) == 0);
    assert(b.walkLength(2) == 2);
    assert(b.walkLength(4) == 4);
    assert(b.walkLength(6) == 4);

    //Infinite Range
    auto fibs = recurrence!"a[n-1] + a[n-2]"(1, 1);
    assert(!__traits(compiles, fibs.walkLength()));
    assert(fibs.take(10).walkLength() == 10);
    assert(fibs.walkLength(55) == 55);
}

/**
    `popFrontN` eagerly advances `r` itself (not a copy) up to `n` times
    (by calling `r.popFront`). `popFrontN` takes `r` by `ref`,
    so it mutates the original range. Completes in $(BIGOH 1) steps for ranges
    that support slicing and have length.
    Completes in $(BIGOH n) time for all other ranges.

    `popBackN` behaves the same as `popFrontN` but instead removes
    elements from the back of the (bidirectional) range instead of the front.

    Returns:
    How much `r` was actually advanced, which may be less than `n` if
    `r` did not have at least `n` elements.

    See_Also: $(REF drop, std, range), $(REF dropBack, std, range)
*/
size_t popFrontN(Range)(ref Range r, size_t n)
if (isInputRange!Range)
{
    static if (hasLength!Range)
    {
        n = cast(size_t) (n < r.length ? n : r.length);
    }

    static if (hasSlicing!Range && is(typeof(r = r[n .. $])))
    {
        r = r[n .. $];
    }
    else static if (hasSlicing!Range && hasLength!Range) //TODO: Remove once hasSlicing forces opDollar.
    {
        r = r[n .. r.length];
    }
    else
    {
        static if (hasLength!Range)
        {
            foreach (i; 0 .. n)
                r.popFront();
        }
        else
        {
            foreach (i; 0 .. n)
            {
                if (r.empty) return i;
                r.popFront();
            }
        }
    }
    return n;
}

/// ditto
size_t popBackN(Range)(ref Range r, size_t n)
if (isBidirectionalRange!Range)
{
    static if (hasLength!Range)
    {
        n = cast(size_t) (n < r.length ? n : r.length);
    }

    static if (hasSlicing!Range && is(typeof(r = r[0 .. $ - n])))
    {
        r = r[0 .. $ - n];
    }
    else static if (hasSlicing!Range && hasLength!Range) //TODO: Remove once hasSlicing forces opDollar.
    {
        r = r[0 .. r.length - n];
    }
    else
    {
        static if (hasLength!Range)
        {
            foreach (i; 0 .. n)
                r.popBack();
        }
        else
        {
            foreach (i; 0 .. n)
            {
                if (r.empty) return i;
                r.popBack();
            }
        }
    }
    return n;
}

///
@safe unittest
{
    int[] a = [ 1, 2, 3, 4, 5 ];
    a.popFrontN(2);
    assert(a == [ 3, 4, 5 ]);
    a.popFrontN(7);
    assert(a == [ ]);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota;
    auto LL = iota(1L, 7L);
    auto r = popFrontN(LL, 2);
    assert(equal(LL, [3L, 4L, 5L, 6L]));
    assert(r == 2);
}

///
@safe unittest
{
    int[] a = [ 1, 2, 3, 4, 5 ];
    a.popBackN(2);
    assert(a == [ 1, 2, 3 ]);
    a.popBackN(7);
    assert(a == [ ]);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota;
    auto LL = iota(1L, 7L);
    auto r = popBackN(LL, 2);
    assert(equal(LL, [1L, 2L, 3L, 4L]));
    assert(r == 2);
}

/**
    Eagerly advances `r` itself (not a copy) exactly `n` times (by
    calling `r.popFront`). `popFrontExactly` takes `r` by `ref`,
    so it mutates the original range. Completes in $(BIGOH 1) steps for ranges
    that support slicing, and have either length or are infinite.
    Completes in $(BIGOH n) time for all other ranges.

    Note: Unlike $(LREF popFrontN), `popFrontExactly` will assume that the
    range holds at least `n` elements. This makes `popFrontExactly`
    faster than `popFrontN`, but it also means that if `range` does
    not contain at least `n` elements, it will attempt to call `popFront`
    on an empty range, which is undefined behavior. So, only use
    `popFrontExactly` when it is guaranteed that `range` holds at least
    `n` elements.

    `popBackExactly` will behave the same but instead removes elements from
    the back of the (bidirectional) range instead of the front.

    See_Also: $(REF dropExactly, std, range), $(REF dropBackExactly, std, range)
*/
void popFrontExactly(Range)(ref Range r, size_t n)
if (isInputRange!Range)
{
    static if (hasLength!Range)
        assert(n <= r.length, "range is smaller than amount of items to pop");

    static if (hasSlicing!Range && is(typeof(r = r[n .. $])))
        r = r[n .. $];
    else static if (hasSlicing!Range && hasLength!Range) //TODO: Remove once hasSlicing forces opDollar.
        r = r[n .. r.length];
    else
        foreach (i; 0 .. n)
            r.popFront();
}

/// ditto
void popBackExactly(Range)(ref Range r, size_t n)
if (isBidirectionalRange!Range)
{
    static if (hasLength!Range)
        assert(n <= r.length, "range is smaller than amount of items to pop");

    static if (hasSlicing!Range && is(typeof(r = r[0 .. $ - n])))
        r = r[0 .. $ - n];
    else static if (hasSlicing!Range && hasLength!Range) //TODO: Remove once hasSlicing forces opDollar.
        r = r[0 .. r.length - n];
    else
        foreach (i; 0 .. n)
            r.popBack();
}

///
@safe unittest
{
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

/**
   Moves the front of `r` out and returns it.

   If `r.front` is a struct with a destructor or copy constructor defined, it
   is reset to its `.init` value after its value is moved. Otherwise, it is
   left unchanged.

   In either case, `r.front` is left in a destroyable state that does not
   allocate any resources.
*/
ElementType!R moveFront(R)(R r)
{
    static if (is(typeof(&r.moveFront)))
    {
        return r.moveFront();
    }
    else static if (!hasElaborateCopyConstructor!(ElementType!R))
    {
        return r.front;
    }
    else static if (is(typeof(&(r.front())) == ElementType!R*))
    {
        import std.algorithm.mutation : move;
        return move(r.front);
    }
    else
    {
        static assert(0,
                "Cannot move front of a range with a postblit and an rvalue front.");
    }
}

///
@safe unittest
{
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
    struct R
    {
        @property ref int front() { static int x = 42; return x; }
        this(this){}
    }
    R r;
    assert(moveFront(r) == 42);
}

/**
   Moves the back of `r` out and returns it. Leaves `r.back` in a
   destroyable state that does not allocate any resources (usually equal
   to its `.init` value).
*/
ElementType!R moveBack(R)(R r)
{
    static if (is(typeof(&r.moveBack)))
    {
        return r.moveBack();
    }
    else static if (!hasElaborateCopyConstructor!(ElementType!R))
    {
        return r.back;
    }
    else static if (is(typeof(&(r.back())) == ElementType!R*))
    {
        import std.algorithm.mutation : move;
        return move(r.back);
    }
    else
    {
        static assert(0,
                "Cannot move back of a range with a postblit and an rvalue back.");
    }
}

///
@safe unittest
{
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

/**
   Moves element at index `i` of `r` out and returns it. Leaves $(D
   r[i]) in a destroyable state that does not allocate any resources
   (usually equal to its `.init` value).
*/
ElementType!R moveAt(R)(R r, size_t i)
{
    static if (is(typeof(&r.moveAt)))
    {
        return r.moveAt(i);
    }
    else static if (!hasElaborateCopyConstructor!(ElementType!(R)))
    {
        return r[i];
    }
    else static if (is(typeof(&r[i]) == ElementType!R*))
    {
        import std.algorithm.mutation : move;
        return move(r[i]);
    }
    else
    {
        static assert(0,
                "Cannot move element of a range with a postblit and rvalue elements.");
    }
}

///
@safe unittest
{
    auto a = [1,2,3,4];
    foreach (idx, it; a)
    {
        assert(it == moveAt(a, idx));
    }
}

@safe unittest
{
    import std.internal.test.dummyrange;

    foreach (DummyType; AllDummyRanges)
    {
        auto d = DummyType.init;
        assert(moveFront(d) == 1);

        static if (isBidirectionalRange!DummyType)
        {
            assert(moveBack(d) == 10);
        }

        static if (isRandomAccessRange!DummyType)
        {
            assert(moveAt(d, 2) == 3);
        }
    }
}

/**
Implements the range interface primitive `empty` for types that
obey $(LREF hasLength) property and for narrow strings. Due to the
fact that nonmember functions can be called with the first argument
using the dot notation, `a.empty` is equivalent to `empty(a)`.
 */
@property bool empty(T)(auto ref scope T a)
if (is(typeof(a.length) : size_t))
{
    return !a.length;
}

///
@safe pure nothrow unittest
{
    auto a = [ 1, 2, 3 ];
    assert(!a.empty);
    assert(a[3 .. $].empty);

    int[string] b;
    assert(b.empty);
    b["zero"] = 0;
    assert(!b.empty);
}

/**
Implements the range interface primitive `save` for built-in
arrays. Due to the fact that nonmember functions can be called with
the first argument using the dot notation, `array.save` is
equivalent to `save(array)`. The function does not duplicate the
content of the array, it simply returns its argument.
 */
@property inout(T)[] save(T)(return scope inout(T)[] a) @safe pure nothrow @nogc
{
    return a;
}

///
@safe pure nothrow unittest
{
    auto a = [ 1, 2, 3 ];
    auto b = a.save;
    assert(b is a);
}

/**
Implements the range interface primitive `popFront` for built-in
arrays. Due to the fact that nonmember functions can be called with
the first argument using the dot notation, `array.popFront` is
equivalent to `popFront(array)`. For $(GLOSSARY narrow strings),
`popFront` automatically advances to the next $(GLOSSARY code
point).
*/
void popFront(T)(scope ref inout(T)[] a) @safe pure nothrow @nogc
if (!isAutodecodableString!(T[]) && !is(T[] == void[]))
{
    assert(a.length, "Attempting to popFront() past the end of an array of " ~ T.stringof);
    a = a[1 .. $];
}

///
@safe pure nothrow unittest
{
    auto a = [ 1, 2, 3 ];
    a.popFront();
    assert(a == [ 2, 3 ]);
}

@safe unittest
{
    static assert(!is(typeof({          int[4] a; popFront(a); })));
    static assert(!is(typeof({ immutable int[] a; popFront(a); })));
    static assert(!is(typeof({          void[] a; popFront(a); })));
}

/// ditto
void popFront(C)(scope ref inout(C)[] str) @trusted pure nothrow
if (isAutodecodableString!(C[]))
{
    import std.algorithm.comparison : min;

    assert(str.length, "Attempting to popFront() past the end of an array of " ~ C.stringof);

    static if (is(immutable C == immutable char))
    {
        static immutable ubyte[] charWidthTab = [
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
            4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 1, 1
        ];

        immutable c = str[0];
        immutable charWidth = c < 192 ? 1 : charWidthTab.ptr[c - 192];
        str = str.ptr[min(str.length, charWidth) .. str.length];
    }
    else static if (is(immutable C == immutable wchar))
    {
        immutable u = str[0];
        immutable seqLen = 1 + (u >= 0xD800 && u <= 0xDBFF);
        str = str.ptr[min(seqLen, str.length) .. str.length];
    }
    else static assert(0, "Bad template constraint.");
}

@safe pure unittest
{
    import std.meta : AliasSeq;

    static foreach (S; AliasSeq!(string, wstring, dstring))
    {{
        S s = "\xC2\xA9hello";
        s.popFront();
        assert(s == "hello");

        S str = "hello\U00010143\u0100\U00010143";
        foreach (dchar c; ['h', 'e', 'l', 'l', 'o', '\U00010143', '\u0100', '\U00010143'])
        {
            assert(str.front == c);
            str.popFront();
        }
        assert(str.empty);

        static assert(!is(typeof({          immutable S a; popFront(a); })));
        static assert(!is(typeof({ typeof(S.init[0])[4] a; popFront(a); })));
    }}

    C[] _eatString(C)(C[] str)
    {
        while (!str.empty)
            str.popFront();

        return str;
    }
    enum checkCTFE = _eatString("ウェブサイト@La_Verité.com");
    static assert(checkCTFE.empty);
    enum checkCTFEW = _eatString("ウェブサイト@La_Verité.com"w);
    static assert(checkCTFEW.empty);
}

// https://issues.dlang.org/show_bug.cgi?id=16090
@safe unittest
{
    string s = "\u00E4";
    assert(s.length == 2);
    s = s[0 .. 1];
    assert(s.length == 1);
    s.popFront;
    assert(s.empty);
}

@safe unittest
{
    wstring s = "\U00010000";
    assert(s.length == 2);
    s = s[0 .. 1];
    assert(s.length == 1);
    s.popFront;
    assert(s.empty);
}

/**
Implements the range interface primitive `popBack` for built-in
arrays. Due to the fact that nonmember functions can be called with
the first argument using the dot notation, `array.popBack` is
equivalent to `popBack(array)`. For $(GLOSSARY narrow strings), $(D
popFront) automatically eliminates the last $(GLOSSARY code point).
*/
void popBack(T)(scope ref inout(T)[] a) @safe pure nothrow @nogc
if (!isAutodecodableString!(T[]) && !is(T[] == void[]))
{
    assert(a.length);
    a = a[0 .. $ - 1];
}

///
@safe pure nothrow unittest
{
    auto a = [ 1, 2, 3 ];
    a.popBack();
    assert(a == [ 1, 2 ]);
}

@safe unittest
{
    static assert(!is(typeof({ immutable int[] a; popBack(a); })));
    static assert(!is(typeof({          int[4] a; popBack(a); })));
    static assert(!is(typeof({          void[] a; popBack(a); })));
}

/// ditto
void popBack(T)(scope ref inout(T)[] a) @safe pure
if (isAutodecodableString!(T[]))
{
    import std.utf : strideBack;
    assert(a.length, "Attempting to popBack() past the front of an array of " ~ T.stringof);
    a = a[0 .. $ - strideBack(a, $)];
}

@safe pure unittest
{
    import std.meta : AliasSeq;

    static foreach (S; AliasSeq!(string, wstring, dstring))
    {{
        S s = "hello\xE2\x89\xA0";
        s.popBack();
        assert(s == "hello");
        S s3 = "\xE2\x89\xA0";
        auto c = s3.back;
        assert(c == cast(dchar)'\u2260');
        s3.popBack();
        assert(s3 == "");

        S str = "\U00010143\u0100\U00010143hello";
        foreach (dchar ch; ['o', 'l', 'l', 'e', 'h', '\U00010143', '\u0100', '\U00010143'])
        {
            assert(str.back == ch);
            str.popBack();
        }
        assert(str.empty);

        static assert(!is(typeof({          immutable S a; popBack(a); })));
        static assert(!is(typeof({ typeof(S.init[0])[4] a; popBack(a); })));
    }}
}

/**
EXPERIMENTAL: to try out removing autodecoding, set the version
`NoAutodecodeStrings`. Most things are expected to fail with this version
currently.
*/
version (NoAutodecodeStrings)
{
    enum autodecodeStrings = false;
}
else
{
    ///
    enum autodecodeStrings = true;
}

/**
Implements the range interface primitive `front` for built-in
arrays. Due to the fact that nonmember functions can be called with
the first argument using the dot notation, `array.front` is
equivalent to `front(array)`. For $(GLOSSARY narrow strings), $(D
front) automatically returns the first $(GLOSSARY code point) as _a $(D
dchar).
*/
@property ref inout(T) front(T)(return scope inout(T)[] a) @safe pure nothrow @nogc
if (!isAutodecodableString!(T[]) && !is(T[] == void[]))
{
    assert(a.length, "Attempting to fetch the front of an empty array of " ~ T.stringof);
    return a[0];
}

///
@safe pure nothrow unittest
{
    int[] a = [ 1, 2, 3 ];
    assert(a.front == 1);
}

@safe pure nothrow unittest
{
    auto a = [ 1, 2 ];
    a.front = 4;
    assert(a.front == 4);
    assert(a == [ 4, 2 ]);

    immutable b = [ 1, 2 ];
    assert(b.front == 1);

    int[2] c = [ 1, 2 ];
    assert(c.front == 1);
}

/// ditto
@property dchar front(T)(scope const(T)[] a) @safe pure
if (isAutodecodableString!(T[]))
{
    import std.utf : decode;
    assert(a.length, "Attempting to fetch the front of an empty array of " ~ T.stringof);
    size_t i = 0;
    return decode(a, i);
}

/**
Implements the range interface primitive `back` for built-in
arrays. Due to the fact that nonmember functions can be called with
the first argument using the dot notation, `array.back` is
equivalent to `back(array)`. For $(GLOSSARY narrow strings), $(D
back) automatically returns the last $(GLOSSARY code point) as _a $(D
dchar).
*/
@property ref inout(T) back(T)(return scope inout(T)[] a) @safe pure nothrow @nogc
if (!isAutodecodableString!(T[]) && !is(T[] == void[]))
{
    assert(a.length, "Attempting to fetch the back of an empty array of " ~ T.stringof);
    return a[$ - 1];
}

///
@safe pure nothrow unittest
{
    int[] a = [ 1, 2, 3 ];
    assert(a.back == 3);
    a.back += 4;
    assert(a.back == 7);
}

@safe pure nothrow unittest
{
    immutable b = [ 1, 2, 3 ];
    assert(b.back == 3);

    int[3] c = [ 1, 2, 3 ];
    assert(c.back == 3);
}

/// ditto
// Specialization for strings
@property dchar back(T)(scope const(T)[] a) @safe pure
if (isAutodecodableString!(T[]))
{
    import std.utf : decode, strideBack;
    assert(a.length, "Attempting to fetch the back of an empty array of " ~ T.stringof);
    size_t i = a.length - strideBack(a, a.length);
    return decode(a, i);
}

/*
Implements `length` for a range by forwarding it to `member`.
*/
package(std) mixin template ImplementLength(alias member)
{
    static if (hasLength!(typeof(member)))
    {
        @property auto length()
        {
            return member.length;
        }
        alias opDollar = length;
    }
}

@safe unittest
{
    import std.meta : AliasSeq;

    foreach (alias E; AliasSeq!(noreturn, const(noreturn), immutable(noreturn) ))
    {
        alias R = E[];

        static assert(isInputRange!R);
        static assert(isForwardRange!R);
        static assert(isBidirectionalRange!R);
        static assert(isRandomAccessRange!R);
    }

    static assert(isOutputRange!(noreturn[], noreturn));
}
