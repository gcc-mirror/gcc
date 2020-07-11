// Written in the D programming language.
/**
Functions and types that manipulate built-in arrays and associative arrays.

This module provides all kinds of functions to create, manipulate or convert arrays:

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE ,
$(TR $(TH Function Name) $(TH Description)
)
    $(TR $(TD $(LREF _array))
        $(TD Returns a copy of the input in a newly allocated dynamic _array.
    ))
    $(TR $(TD $(LREF appender))
        $(TD Returns a new $(LREF Appender) or $(LREF RefAppender) initialized with a given _array.
    ))
    $(TR $(TD $(LREF assocArray))
        $(TD Returns a newly allocated associative _array from a range of key/value tuples.
    ))
    $(TR $(TD $(LREF byPair))
        $(TD Construct a range iterating over an associative _array by key/value tuples.
    ))
    $(TR $(TD $(LREF insertInPlace))
        $(TD Inserts into an existing _array at a given position.
    ))
    $(TR $(TD $(LREF join))
        $(TD Concatenates a range of ranges into one _array.
    ))
    $(TR $(TD $(LREF minimallyInitializedArray))
        $(TD Returns a new _array of type $(D T).
    ))
    $(TR $(TD $(LREF replace))
        $(TD Returns a new _array with all occurrences of a certain subrange replaced.
    ))
    $(TR $(TD $(LREF replaceFirst))
        $(TD Returns a new _array with the first occurrence of a certain subrange replaced.
    ))
    $(TR $(TD $(LREF replaceInPlace))
        $(TD Replaces all occurrences of a certain subrange and puts the result into a given _array.
    ))
    $(TR $(TD $(LREF replaceInto))
        $(TD Replaces all occurrences of a certain subrange and puts the result into an output range.
    ))
    $(TR $(TD $(LREF replaceLast))
        $(TD Returns a new _array with the last occurrence of a certain subrange replaced.
    ))
    $(TR $(TD $(LREF replaceSlice))
        $(TD Returns a new _array with a given slice replaced.
    ))
    $(TR $(TD $(LREF replicate))
        $(TD Creates a new _array out of several copies of an input _array or range.
    ))
    $(TR $(TD $(LREF sameHead))
        $(TD Checks if the initial segments of two arrays refer to the same
        place in memory.
    ))
    $(TR $(TD $(LREF sameTail))
        $(TD Checks if the final segments of two arrays refer to the same place
        in memory.
    ))
    $(TR $(TD $(LREF split))
        $(TD Eagerly split a range or string into an _array.
    ))
    $(TR $(TD $(LREF uninitializedArray))
        $(TD Returns a new _array of type $(D T) without initializing its elements.
    ))
)

Copyright: Copyright Andrei Alexandrescu 2008- and Jonathan M Davis 2011-.

License:   $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors:   $(HTTP erdani.org, Andrei Alexandrescu) and Jonathan M Davis

Source: $(PHOBOSSRC std/_array.d)
*/
module std.array;

import std.functional;
import std.meta;
import std.traits;

import std.range.primitives;
public import std.range.primitives : save, empty, popFront, popBack, front, back;

/**
 * Allocates an array and initializes it with copies of the elements
 * of range $(D r).
 *
 * Narrow strings are handled as a special case in an overload.
 *
 * Params:
 *      r = range (or aggregate with $(D opApply) function) whose elements are copied into the allocated array
 * Returns:
 *      allocated and initialized array
 */
ForeachType!Range[] array(Range)(Range r)
if (isIterable!Range && !isNarrowString!Range && !isInfinite!Range)
{
    if (__ctfe)
    {
        // Compile-time version to avoid memcpy calls.
        // Also used to infer attributes of array().
        typeof(return) result;
        foreach (e; r)
            result ~= e;
        return result;
    }

    alias E = ForeachType!Range;
    static if (hasLength!Range)
    {
        auto length = r.length;
        if (length == 0)
            return null;

        import std.conv : emplaceRef;

        auto result = (() @trusted => uninitializedArray!(Unqual!E[])(length))();

        // Every element of the uninitialized array must be initialized
        size_t i;
        foreach (e; r)
        {
            emplaceRef!E(result[i], e);
            ++i;
        }
        return (() @trusted => cast(E[]) result)();
    }
    else
    {
        auto a = appender!(E[])();
        foreach (e; r)
        {
            a.put(e);
        }
        return a.data;
    }
}

///
@safe pure nothrow unittest
{
    auto a = array([1, 2, 3, 4, 5][]);
    assert(a == [ 1, 2, 3, 4, 5 ]);
}

@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    struct Foo
    {
        int a;
    }
    auto a = array([Foo(1), Foo(2), Foo(3), Foo(4), Foo(5)][]);
    assert(equal(a, [Foo(1), Foo(2), Foo(3), Foo(4), Foo(5)]));
}

@system unittest
{
    import std.algorithm.comparison : equal;
    struct Foo
    {
        int a;
        void opAssign(Foo foo)
        {
            assert(0);
        }
        auto opEquals(Foo foo)
        {
            return a == foo.a;
        }
    }
    auto a = array([Foo(1), Foo(2), Foo(3), Foo(4), Foo(5)][]);
    assert(equal(a, [Foo(1), Foo(2), Foo(3), Foo(4), Foo(5)]));
}

@safe unittest
{
    // Issue 12315
    static struct Bug12315 { immutable int i; }
    enum bug12315 = [Bug12315(123456789)].array();
    static assert(bug12315[0].i == 123456789);
}

@safe unittest
{
    import std.range;
    static struct S{int* p;}
    auto a = array(immutable(S).init.repeat(5));
}

/**
Convert a narrow string to an array type that fully supports random access.
This is handled as a special case and always returns an array of `dchar`

Params:
    str = `isNarrowString` to be converted to an array of `dchar`
Returns:
    a $(D dchar[]), $(D const(dchar)[]), or $(D immutable(dchar)[]) depending on the constness of
    the input.
*/
@trusted ElementType!String[] array(String)(scope String str)
if (isNarrowString!String)
{
    import std.utf : toUTF32;
    /* This function is @trusted because the following cast
     * is unsafe - converting a dstring[] to a dchar[], for example.
     * Our special knowledge of toUTF32 is needed to know the cast is safe.
     */
    return cast(typeof(return)) str.toUTF32;
}

///
@safe unittest
{
    import std.range.primitives : isRandomAccessRange;

    assert("Hello D".array == "Hello D"d);
    static assert(isRandomAccessRange!string == false);

    assert("Hello D"w.array == "Hello D"d);
    static assert(isRandomAccessRange!dstring == true);
}

@system unittest
{
    // @system due to array!string
    import std.conv : to;

    static struct TestArray { int x; string toString() @safe { return to!string(x); } }

    static struct OpAssign
    {
        uint num;
        this(uint num) { this.num = num; }

        // Templating opAssign to make sure the bugs with opAssign being
        // templated are fixed.
        void opAssign(T)(T rhs) { this.num = rhs.num; }
    }

    static struct OpApply
    {
        int opApply(scope int delegate(ref int) dg)
        {
            int res;
            foreach (i; 0 .. 10)
            {
                res = dg(i);
                if (res) break;
            }

            return res;
        }
    }

    auto a = array([1, 2, 3, 4, 5][]);
    //writeln(a);
    assert(a == [ 1, 2, 3, 4, 5 ]);

    auto b = array([TestArray(1), TestArray(2)][]);
    //writeln(b);

    class C
    {
        int x;
        this(int y) { x = y; }
        override string toString() const @safe { return to!string(x); }
    }
    auto c = array([new C(1), new C(2)][]);
    //writeln(c);

    auto d = array([1.0, 2.2, 3][]);
    assert(is(typeof(d) == double[]));
    //writeln(d);

    auto e = [OpAssign(1), OpAssign(2)];
    auto f = array(e);
    assert(e == f);

    assert(array(OpApply.init) == [0,1,2,3,4,5,6,7,8,9]);
    assert(array("ABC") == "ABC"d);
    assert(array("ABC".dup) == "ABC"d.dup);
}

//Bug# 8233
@safe unittest
{
    assert(array("hello world"d) == "hello world"d);
    immutable a = [1, 2, 3, 4, 5];
    assert(array(a) == a);
    const b = a;
    assert(array(b) == a);

    //To verify that the opAssign branch doesn't get screwed up by using Unqual.
    //EDIT: array no longer calls opAssign.
    struct S
    {
        ref S opAssign(S)(const ref S rhs)
        {
            assert(0);
        }

        int i;
    }

    foreach (T; AliasSeq!(S, const S, immutable S))
    {
        auto arr = [T(1), T(2), T(3), T(4)];
        assert(array(arr) == arr);
    }
}

@safe unittest
{
    //9824
    static struct S
    {
        @disable void opAssign(S);
        int i;
    }
    auto arr = [S(0), S(1), S(2)];
    arr.array();
}

// Bugzilla 10220
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.exception;
    import std.range : repeat;

    static struct S
    {
        int val;

        @disable this();
        this(int v) { val = v; }
    }
    assertCTFEable!(
    {
        auto r = S(1).repeat(2).array();
        assert(equal(r, [S(1), S(1)]));
    });
}

@safe unittest
{
    //Turn down infinity:
    static assert(!is(typeof(
        repeat(1).array()
    )));
}

/**
Returns a newly allocated associative _array from a range of key/value tuples.

Params:
    r = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    of tuples of keys and values.
Returns: A newly allocated associative array out of elements of the input
range, which must be a range of tuples (Key, Value). Returns a null associative
array reference when given an empty range.
Duplicates: Associative arrays have unique keys. If r contains duplicate keys,
then the result will contain the value of the last pair for that key in r.

See_Also: $(REF Tuple, std,typecons), $(REF zip, std,range)
 */

auto assocArray(Range)(Range r)
if (isInputRange!Range)
{
    import std.typecons : isTuple;

    alias E = ElementType!Range;
    static assert(isTuple!E, "assocArray: argument must be a range of tuples");
    static assert(E.length == 2, "assocArray: tuple dimension must be 2");
    alias KeyType = E.Types[0];
    alias ValueType = E.Types[1];
    static assert(isMutable!ValueType, "assocArray: value type must be mutable");

    ValueType[KeyType] aa;
    foreach (t; r)
        aa[t[0]] = t[1];
    return aa;
}

///
@safe pure /*nothrow*/ unittest
{
    import std.range;
    import std.typecons;
    auto a = assocArray(zip([0, 1, 2], ["a", "b", "c"])); // aka zipMap
    assert(is(typeof(a) == string[int]));
    assert(a == [0:"a", 1:"b", 2:"c"]);

    auto b = assocArray([ tuple("foo", "bar"), tuple("baz", "quux") ]);
    assert(is(typeof(b) == string[string]));
    assert(b == ["foo":"bar", "baz":"quux"]);
}

// @@@11053@@@ - Cannot be version (unittest) - recursive instantiation error
@safe unittest
{
    import std.typecons;
    static assert(!__traits(compiles, [ tuple("foo", "bar", "baz") ].assocArray()));
    static assert(!__traits(compiles, [ tuple("foo") ].assocArray()));
    assert([ tuple("foo", "bar") ].assocArray() == ["foo": "bar"]);
}

// Issue 13909
@safe unittest
{
    import std.typecons;
    auto a = [tuple!(const string, string)("foo", "bar")];
    auto b = [tuple!(string, const string)("foo", "bar")];
    assert(assocArray(a) == [cast(const(string)) "foo": "bar"]);
    static assert(!__traits(compiles, assocArray(b)));
}

/**
Construct a range iterating over an associative array by key/value tuples.

Params: aa = The associative array to iterate over.

Returns: A $(REF_ALTTEXT forward range, isForwardRange, std,_range,primitives)
of Tuple's of key and value pairs from the given associative array.
*/
auto byPair(AA : Value[Key], Value, Key)(AA aa)
{
    import std.algorithm.iteration : map;
    import std.typecons : tuple;

    return aa.byKeyValue.map!(pair => tuple(pair.key, pair.value));
}

///
@system unittest
{
    import std.algorithm.sorting : sort;
    import std.typecons : tuple, Tuple;

    auto aa = ["a": 1, "b": 2, "c": 3];
    Tuple!(string, int)[] pairs;

    // Iteration over key/value pairs.
    foreach (pair; aa.byPair)
    {
        pairs ~= pair;
    }

    // Iteration order is implementation-dependent, so we should sort it to get
    // a fixed order.
    sort(pairs);
    assert(pairs == [
        tuple("a", 1),
        tuple("b", 2),
        tuple("c", 3)
    ]);
}

@system unittest
{
    import std.typecons : tuple, Tuple;

    auto aa = ["a":2];
    auto pairs = aa.byPair();

    static assert(is(typeof(pairs.front) == Tuple!(string,int)));
    static assert(isForwardRange!(typeof(pairs)));

    assert(!pairs.empty);
    assert(pairs.front == tuple("a", 2));

    auto savedPairs = pairs.save;

    pairs.popFront();
    assert(pairs.empty);
    assert(!savedPairs.empty);
    assert(savedPairs.front == tuple("a", 2));
}

// Issue 17711
@system unittest
{
    const(int[string]) aa = [ "abc": 123 ];

    // Ensure that byKeyValue is usable with a const AA.
    auto kv = aa.byKeyValue;
    assert(!kv.empty);
    assert(kv.front.key == "abc" && kv.front.value == 123);
    kv.popFront();
    assert(kv.empty);

    // Ensure byPair is instantiable with const AA.
    auto r = aa.byPair;
    static assert(isInputRange!(typeof(r)));
    assert(!r.empty && r.front[0] == "abc" && r.front[1] == 123);
    r.popFront();
    assert(r.empty);
}

private template blockAttribute(T)
{
    import core.memory;
    static if (hasIndirections!(T) || is(T == void))
    {
        enum blockAttribute = 0;
    }
    else
    {
        enum blockAttribute = GC.BlkAttr.NO_SCAN;
    }
}
version (unittest)
{
    import core.memory : UGC = GC;
    static assert(!(blockAttribute!void & UGC.BlkAttr.NO_SCAN));
}

// Returns the number of dimensions in an array T.
private template nDimensions(T)
{
    static if (isArray!T)
    {
        enum nDimensions = 1 + nDimensions!(typeof(T.init[0]));
    }
    else
    {
        enum nDimensions = 0;
    }
}

version (unittest)
{
    static assert(nDimensions!(uint[]) == 1);
    static assert(nDimensions!(float[][]) == 2);
}

/++
Returns a new array of type $(D T) allocated on the garbage collected heap
without initializing its elements.  This can be a useful optimization if every
element will be immediately initialized.  $(D T) may be a multidimensional
array.  In this case sizes may be specified for any number of dimensions from 0
to the number in $(D T).

uninitializedArray is nothrow and weakly pure.

uninitializedArray is @system if the uninitialized element type has pointers.
+/
auto uninitializedArray(T, I...)(I sizes) nothrow @system
if (isDynamicArray!T && allSatisfy!(isIntegral, I) && hasIndirections!(ElementEncodingType!T))
{
    enum isSize_t(E) = is (E : size_t);
    alias toSize_t(E) = size_t;

    static assert(allSatisfy!(isSize_t, I),
        "Argument types in "~I.stringof~" are not all convertible to size_t: "
        ~Filter!(templateNot!(isSize_t), I).stringof);

    //Eagerlly transform non-size_t into size_t to avoid template bloat
    alias ST = staticMap!(toSize_t, I);

    return arrayAllocImpl!(false, T, ST)(sizes);
}

/// ditto
auto uninitializedArray(T, I...)(I sizes) nothrow @trusted
if (isDynamicArray!T && allSatisfy!(isIntegral, I) && !hasIndirections!(ElementEncodingType!T))
{
    enum isSize_t(E) = is (E : size_t);
    alias toSize_t(E) = size_t;

    static assert(allSatisfy!(isSize_t, I),
        "Argument types in "~I.stringof~" are not all convertible to size_t: "
        ~Filter!(templateNot!(isSize_t), I).stringof);

    //Eagerlly transform non-size_t into size_t to avoid template bloat
    alias ST = staticMap!(toSize_t, I);

    return arrayAllocImpl!(false, T, ST)(sizes);
}
///
@system nothrow pure unittest
{
    double[] arr = uninitializedArray!(double[])(100);
    assert(arr.length == 100);

    double[][] matrix = uninitializedArray!(double[][])(42, 31);
    assert(matrix.length == 42);
    assert(matrix[0].length == 31);

    char*[] ptrs = uninitializedArray!(char*[])(100);
    assert(ptrs.length == 100);
}

/++
Returns a new array of type $(D T) allocated on the garbage collected heap.

Partial initialization is done for types with indirections, for preservation
of memory safety. Note that elements will only be initialized to 0, but not
necessarily the element type's $(D .init).

minimallyInitializedArray is nothrow and weakly pure.
+/
auto minimallyInitializedArray(T, I...)(I sizes) nothrow @trusted
if (isDynamicArray!T && allSatisfy!(isIntegral, I))
{
    enum isSize_t(E) = is (E : size_t);
    alias toSize_t(E) = size_t;

    static assert(allSatisfy!(isSize_t, I),
        "Argument types in "~I.stringof~" are not all convertible to size_t: "
        ~Filter!(templateNot!(isSize_t), I).stringof);
    //Eagerlly transform non-size_t into size_t to avoid template bloat
    alias ST = staticMap!(toSize_t, I);

    return arrayAllocImpl!(true, T, ST)(sizes);
}

///
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.range : repeat;

    auto arr = minimallyInitializedArray!(int[])(42);
    assert(arr.length == 42);
    // Elements aren't necessarily initialized to 0
    assert(!arr.equal(0.repeat(42)));
}

@safe pure nothrow unittest
{
    cast(void) minimallyInitializedArray!(int[][][][][])();
    double[] arr = minimallyInitializedArray!(double[])(100);
    assert(arr.length == 100);

    double[][] matrix = minimallyInitializedArray!(double[][])(42);
    assert(matrix.length == 42);
    foreach (elem; matrix)
    {
        assert(elem.ptr is null);
    }
}

private auto arrayAllocImpl(bool minimallyInitialized, T, I...)(I sizes) nothrow
{
    static assert(I.length <= nDimensions!T,
        I.length.stringof~"dimensions specified for a "~nDimensions!T.stringof~" dimensional array.");

    alias E = ElementEncodingType!T;

    E[] ret;

    static if (I.length != 0)
    {
        static assert(is(I[0] == size_t));
        alias size = sizes[0];
    }

    static if (I.length == 1)
    {
        if (__ctfe)
        {
            static if (__traits(compiles, new E[](size)))
                ret = new E[](size);
            else static if (__traits(compiles, ret ~= E.init))
            {
                try
                {
                    //Issue: if E has an impure postblit, then all of arrayAllocImpl
                    //Will be impure, even during non CTFE.
                    foreach (i; 0 .. size)
                        ret ~= E.init;
                }
                catch (Exception e)
                    throw new Error(e.msg);
            }
            else
                assert(0, "No postblit nor default init on " ~ E.stringof ~
                    ": At least one is required for CTFE.");
        }
        else
        {
            import core.memory : GC;
            import core.stdc.string : memset;

            import core.checkedint : mulu;
            bool overflow;
            const nbytes = mulu(size, E.sizeof, overflow);
            if (overflow) assert(0);

            auto ptr = cast(E*) GC.malloc(nbytes, blockAttribute!E);
            static if (minimallyInitialized && hasIndirections!E)
                memset(ptr, 0, nbytes);
            ret = ptr[0 .. size];
        }
    }
    else static if (I.length > 1)
    {
        ret = arrayAllocImpl!(false, E[])(size);
        foreach (ref elem; ret)
            elem = arrayAllocImpl!(minimallyInitialized, E)(sizes[1..$]);
    }

    return ret;
}

@safe nothrow pure unittest
{
    auto s1 = uninitializedArray!(int[])();
    auto s2 = minimallyInitializedArray!(int[])();
    assert(s1.length == 0);
    assert(s2.length == 0);
}

@safe nothrow pure unittest //@@@9803@@@
{
    auto a = minimallyInitializedArray!(int*[])(1);
    assert(a[0] == null);
    auto b = minimallyInitializedArray!(int[][])(1);
    assert(b[0].empty);
    auto c = minimallyInitializedArray!(int*[][])(1, 1);
    assert(c[0][0] == null);
}

@safe unittest //@@@10637@@@
{
    static struct S
    {
        static struct I{int i; alias i this;}
        int* p;
        this() @disable;
        this(int i)
        {
            p = &(new I(i)).i;
        }
        this(this)
        {
            p = &(new I(*p)).i;
        }
        ~this()
        {
            assert(p != null);
        }
    }
    auto a = minimallyInitializedArray!(S[])(1);
    assert(a[0].p == null);
    enum b = minimallyInitializedArray!(S[])(1);
}

@safe nothrow unittest
{
    static struct S1
    {
        this() @disable;
        this(this) @disable;
    }
    auto a1 = minimallyInitializedArray!(S1[][])(2, 2);
    //enum b1 = minimallyInitializedArray!(S1[][])(2, 2);
    static struct S2
    {
        this() @disable;
        //this(this) @disable;
    }
    auto a2 = minimallyInitializedArray!(S2[][])(2, 2);
    enum b2 = minimallyInitializedArray!(S2[][])(2, 2);
    static struct S3
    {
        //this() @disable;
        this(this) @disable;
    }
    auto a3 = minimallyInitializedArray!(S3[][])(2, 2);
    enum b3 = minimallyInitializedArray!(S3[][])(2, 2);
}

// overlap
/*
NOTE: Undocumented for now, overlap does not yet work with ctfe.
Returns the overlapping portion, if any, of two arrays. Unlike $(D
equal), $(D overlap) only compares the pointers in the ranges, not the
values referred by them. If $(D r1) and $(D r2) have an overlapping
slice, returns that slice. Otherwise, returns the null slice.
*/
auto overlap(T, U)(T[] r1, U[] r2) @trusted pure nothrow
if (is(typeof(r1.ptr < r2.ptr) == bool))
{
    import std.algorithm.comparison : min, max;
    auto b = max(r1.ptr, r2.ptr);
    auto e = min(r1.ptr + r1.length, r2.ptr + r2.length);
    return b < e ? b[0 .. e - b] : null;
}

///
@safe pure /*nothrow*/ unittest
{
    int[] a = [ 10, 11, 12, 13, 14 ];
    int[] b = a[1 .. 3];
    assert(overlap(a, b) == [ 11, 12 ]);
    b = b.dup;
    // overlap disappears even though the content is the same
    assert(overlap(a, b).empty);
}

@safe /*nothrow*/ unittest
{
    static void test(L, R)(L l, R r)
    {
        import std.stdio;
        scope(failure) writeln("Types: L %s  R %s", L.stringof, R.stringof);

        assert(overlap(l, r) == [ 100, 12 ]);

        assert(overlap(l, l[0 .. 2]) is l[0 .. 2]);
        assert(overlap(l, l[3 .. 5]) is l[3 .. 5]);
        assert(overlap(l[0 .. 2], l) is l[0 .. 2]);
        assert(overlap(l[3 .. 5], l) is l[3 .. 5]);
    }

    int[] a = [ 10, 11, 12, 13, 14 ];
    int[] b = a[1 .. 3];
    a[1] = 100;

    immutable int[] c = a.idup;
    immutable int[] d = c[1 .. 3];

    test(a, b);
    assert(overlap(a, b.dup).empty);
    test(c, d);
    assert(overlap(c, d.idup).empty);
}

@safe pure nothrow unittest // bugzilla 9836
{
    // range primitives for array should work with alias this types
    struct Wrapper
    {
        int[] data;
        alias data this;

        @property Wrapper save() { return this; }
    }
    auto w = Wrapper([1,2,3,4]);
    std.array.popFront(w); // should work

    static assert(isInputRange!Wrapper);
    static assert(isForwardRange!Wrapper);
    static assert(isBidirectionalRange!Wrapper);
    static assert(isRandomAccessRange!Wrapper);
}

private void copyBackwards(T)(T[] src, T[] dest)
{
    import core.stdc.string : memmove;

    assert(src.length == dest.length);

    if (!__ctfe || hasElaborateCopyConstructor!T)
    {
        /* insertInPlace relies on dest being uninitialized, so no postblits allowed,
         * as this is a MOVE that overwrites the destination, not a COPY.
         * BUG: insertInPlace will not work with ctfe and postblits
         */
        memmove(dest.ptr, src.ptr, src.length * T.sizeof);
    }
    else
    {
        immutable len = src.length;
        for (size_t i = len; i-- > 0;)
        {
            dest[i] = src[i];
        }
    }
}

/++
    Inserts $(D stuff) (which must be an input range or any number of
    implicitly convertible items) in $(D array) at position $(D pos).

    Params:
        array = The array that $(D stuff) will be inserted into.
        pos   = The position in $(D array) to insert the $(D stuff).
        stuff = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives),
        or any number of implicitly convertible items to insert into $(D array).
 +/
void insertInPlace(T, U...)(ref T[] array, size_t pos, U stuff)
if (!isSomeString!(T[])
    && allSatisfy!(isInputRangeOrConvertible!T, U) && U.length > 0)
{
    static if (allSatisfy!(isInputRangeWithLengthOrConvertible!T, U))
    {
        import std.conv : emplaceRef;

        immutable oldLen = array.length;

        size_t to_insert = 0;
        foreach (i, E; U)
        {
            static if (is(E : T)) //a single convertible value, not a range
                to_insert += 1;
            else
                to_insert += stuff[i].length;
        }
        if (to_insert)
        {
            array.length += to_insert;

            // Takes arguments array, pos, stuff
            // Spread apart array[] at pos by moving elements
            (() @trusted { copyBackwards(array[pos .. oldLen], array[pos+to_insert..$]); })();

            // Initialize array[pos .. pos+to_insert] with stuff[]
            auto j = 0;
            foreach (i, E; U)
            {
                static if (is(E : T))
                {
                    emplaceRef!T(array[pos + j++], stuff[i]);
                }
                else
                {
                    foreach (v; stuff[i])
                    {
                        emplaceRef!T(array[pos + j++], v);
                    }
                }
            }
        }
    }
    else
    {
        // stuff has some InputRanges in it that don't have length
        // assume that stuff to be inserted is typically shorter
        // then the array that can be arbitrary big
        // TODO: needs a better implementation as there is no need to build an _array_
        // a singly-linked list of memory blocks (rope, etc.) will do
        auto app = appender!(T[])();
        foreach (i, E; U)
            app.put(stuff[i]);
        insertInPlace(array, pos, app.data);
    }
}

/// Ditto
void insertInPlace(T, U...)(ref T[] array, size_t pos, U stuff)
if (isSomeString!(T[]) && allSatisfy!(isCharOrStringOrDcharRange, U))
{
    static if (is(Unqual!T == T)
        && allSatisfy!(isInputRangeWithLengthOrConvertible!dchar, U))
    {
        import std.utf : codeLength;
        // mutable, can do in place
        //helper function: re-encode dchar to Ts and store at *ptr
        static T* putDChar(T* ptr, dchar ch)
        {
            static if (is(T == dchar))
            {
                *ptr++ = ch;
                return ptr;
            }
            else
            {
                import std.utf : encode;
                T[dchar.sizeof/T.sizeof] buf;
                immutable len = encode(buf, ch);
                final switch (len)
                {
                    static if (T.sizeof == char.sizeof)
                    {
                case 4:
                        ptr[3] = buf[3];
                        goto case;
                case 3:
                        ptr[2] = buf[2];
                        goto case;
                    }
                case 2:
                    ptr[1] = buf[1];
                    goto case;
                case 1:
                    ptr[0] = buf[0];
                }
                ptr += len;
                return ptr;
            }
        }
        size_t to_insert = 0;
        //count up the number of *codeunits* to insert
        foreach (i, E; U)
            to_insert += codeLength!T(stuff[i]);
        array.length += to_insert;

        @trusted static void moveToRight(T[] arr, size_t gap)
        {
            static assert(!hasElaborateCopyConstructor!T);
            import core.stdc.string : memmove;
            if (__ctfe)
            {
                for (size_t i = arr.length - gap; i; --i)
                    arr[gap + i - 1] = arr[i - 1];
            }
            else
                memmove(arr.ptr + gap, arr.ptr, (arr.length - gap) * T.sizeof);
        }
        moveToRight(array[pos .. $], to_insert);
        auto ptr = array.ptr + pos;
        foreach (i, E; U)
        {
            static if (is(E : dchar))
            {
                ptr = putDChar(ptr, stuff[i]);
            }
            else
            {
                foreach (dchar ch; stuff[i])
                    ptr = putDChar(ptr, ch);
            }
        }
        assert(ptr == array.ptr + pos + to_insert, "(ptr == array.ptr + pos + to_insert) is false");
    }
    else
    {
        // immutable/const, just construct a new array
        auto app = appender!(T[])();
        app.put(array[0 .. pos]);
        foreach (i, E; U)
            app.put(stuff[i]);
        app.put(array[pos..$]);
        array = app.data;
    }
}

///
@safe pure unittest
{
    int[] a = [ 1, 2, 3, 4 ];
    a.insertInPlace(2, [ 1, 2 ]);
    assert(a == [ 1, 2, 1, 2, 3, 4 ]);
    a.insertInPlace(3, 10u, 11);
    assert(a == [ 1, 2, 1, 10, 11, 2, 3, 4]);
}

//constraint helpers
private template isInputRangeWithLengthOrConvertible(E)
{
    template isInputRangeWithLengthOrConvertible(R)
    {
        //hasLength not defined for char[], wchar[] and dchar[]
        enum isInputRangeWithLengthOrConvertible =
            (isInputRange!R && is(typeof(R.init.length))
                && is(ElementType!R : E))  || is(R : E);
    }
}

//ditto
private template isCharOrStringOrDcharRange(T)
{
    enum isCharOrStringOrDcharRange = isSomeString!T || isSomeChar!T ||
        (isInputRange!T && is(ElementType!T : dchar));
}

//ditto
private template isInputRangeOrConvertible(E)
{
    template isInputRangeOrConvertible(R)
    {
        enum isInputRangeOrConvertible =
            (isInputRange!R && is(ElementType!R : E))  || is(R : E);
    }
}

@system unittest
{
    // @system due to insertInPlace
    import core.exception;
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter;
    import std.conv : to;
    import std.exception;


    bool test(T, U, V)(T orig, size_t pos, U toInsert, V result,
               string file = __FILE__, size_t line = __LINE__)
    {
        {
            static if (is(T == typeof(T.init.dup)))
                auto a = orig.dup;
            else
                auto a = orig.idup;

            a.insertInPlace(pos, toInsert);
            if (!equal(a, result))
                return false;
        }

        static if (isInputRange!U)
        {
            orig.insertInPlace(pos, filter!"true"(toInsert));
            return equal(orig, result);
        }
        else
            return true;
    }


    assert(test([1, 2, 3, 4], 0, [6, 7], [6, 7, 1, 2, 3, 4]));
    assert(test([1, 2, 3, 4], 2, [8, 9], [1, 2, 8, 9, 3, 4]));
    assert(test([1, 2, 3, 4], 4, [10, 11], [1, 2, 3, 4, 10, 11]));

    assert(test([1, 2, 3, 4], 0, 22, [22, 1, 2, 3, 4]));
    assert(test([1, 2, 3, 4], 2, 23, [1, 2, 23, 3, 4]));
    assert(test([1, 2, 3, 4], 4, 24, [1, 2, 3, 4, 24]));

    void testStr(T, U)(string file = __FILE__, size_t line = __LINE__)
    {

        auto l = to!T("hello");
        auto r = to!U(" વિશ્વ");

        enforce(test(l, 0, r, " વિશ્વhello"),
                new AssertError("testStr failure 1", file, line));
        enforce(test(l, 3, r, "hel વિશ્વlo"),
                new AssertError("testStr failure 2", file, line));
        enforce(test(l, l.length, r, "hello વિશ્વ"),
                new AssertError("testStr failure 3", file, line));
    }

    foreach (T; AliasSeq!(char, wchar, dchar,
        immutable(char), immutable(wchar), immutable(dchar)))
    {
        foreach (U; AliasSeq!(char, wchar, dchar,
            immutable(char), immutable(wchar), immutable(dchar)))
        {
            testStr!(T[], U[])();
        }

    }

    // variadic version
    bool testVar(T, U...)(T orig, size_t pos, U args)
    {
        static if (is(T == typeof(T.init.dup)))
            auto a = orig.dup;
        else
            auto a = orig.idup;
        auto result = args[$-1];

        a.insertInPlace(pos, args[0..$-1]);
        if (!equal(a, result))
            return false;
        return true;
    }
    assert(testVar([1, 2, 3, 4], 0, 6, 7u, [6, 7, 1, 2, 3, 4]));
    assert(testVar([1L, 2, 3, 4], 2, 8, 9L, [1, 2, 8, 9, 3, 4]));
    assert(testVar([1L, 2, 3, 4], 4, 10L, 11, [1, 2, 3, 4, 10, 11]));
    assert(testVar([1L, 2, 3, 4], 4, [10, 11], 40L, 42L,
                    [1, 2, 3, 4, 10, 11, 40, 42]));
    assert(testVar([1L, 2, 3, 4], 4, 10, 11, [40L, 42],
                    [1, 2, 3, 4, 10, 11, 40, 42]));
    assert(testVar("t".idup, 1, 'e', 's', 't', "test"));
    assert(testVar("!!"w.idup, 1, "\u00e9ll\u00f4", 'x', "TTT"w, 'y',
                    "!\u00e9ll\u00f4xTTTy!"));
    assert(testVar("flipflop"d.idup, 4, '_',
                    "xyz"w, '\U00010143', '_', "abc"d, "__",
                    "flip_xyz\U00010143_abc__flop"));
}

@system unittest
{
    import std.algorithm.comparison : equal;
    // insertInPlace interop with postblit
    static struct Int
    {
        int* payload;
        this(int k)
        {
            payload = new int;
            *payload = k;
        }
        this(this)
        {
            int* np = new int;
            *np = *payload;
            payload = np;
        }
        ~this()
        {
            if (payload)
                *payload = 0; //'destroy' it
        }
        @property int getPayload(){ return *payload; }
        alias getPayload this;
    }

    Int[] arr = [Int(1), Int(4), Int(5)];
    assert(arr[0] == 1);
    insertInPlace(arr, 1, Int(2), Int(3));
    assert(equal(arr, [1, 2, 3, 4, 5]));  //check it works with postblit

    version (none) // illustrates that insertInPlace() will not work with CTFE and postblit
    {
        static bool testctfe()
        {
            Int[] arr = [Int(1), Int(4), Int(5)];
            assert(arr[0] == 1);
            insertInPlace(arr, 1, Int(2), Int(3));
            return equal(arr, [1, 2, 3, 4, 5]);  //check it works with postblit
        }
        enum E = testctfe();
    }
}

@safe unittest
{
    import std.exception;
    assertCTFEable!(
    {
        int[] a = [1, 2];
        a.insertInPlace(2, 3);
        a.insertInPlace(0, -1, 0);
        return a == [-1, 0, 1, 2, 3];
    });
}

@system unittest // bugzilla 6874
{
    import core.memory;
    // allocate some space
    byte[] a;
    a.length = 1;

    // fill it
    a.length = a.capacity;

    // write beyond
    byte[] b = a[$ .. $];
    b.insertInPlace(0, a);

    // make sure that reallocation has happened
    assert(GC.addrOf(&b[0]) == GC.addrOf(&b[$-1]));
}


/++
    Returns whether the $(D front)s of $(D lhs) and $(D rhs) both refer to the
    same place in memory, making one of the arrays a slice of the other which
    starts at index $(D 0).
  +/
@safe
pure nothrow bool sameHead(T)(in T[] lhs, in T[] rhs)
{
    return lhs.ptr == rhs.ptr;
}

///
@safe pure nothrow unittest
{
    auto a = [1, 2, 3, 4, 5];
    auto b = a[0 .. 2];

    assert(a.sameHead(b));
}


/++
    Returns whether the $(D back)s of $(D lhs) and $(D rhs) both refer to the
    same place in memory, making one of the arrays a slice of the other which
    end at index $(D $).
  +/
@trusted
pure nothrow bool sameTail(T)(in T[] lhs, in T[] rhs)
{
    return lhs.ptr + lhs.length == rhs.ptr + rhs.length;
}

///
@safe pure nothrow unittest
{
    auto a = [1, 2, 3, 4, 5];
    auto b = a[3..$];

    assert(a.sameTail(b));
}

@safe pure nothrow unittest
{
    foreach (T; AliasSeq!(int[], const(int)[], immutable(int)[], const int[], immutable int[]))
    {
        T a = [1, 2, 3, 4, 5];
        T b = a;
        T c = a[1 .. $];
        T d = a[0 .. 1];
        T e = null;

        assert(sameHead(a, a));
        assert(sameHead(a, b));
        assert(!sameHead(a, c));
        assert(sameHead(a, d));
        assert(!sameHead(a, e));

        assert(sameTail(a, a));
        assert(sameTail(a, b));
        assert(sameTail(a, c));
        assert(!sameTail(a, d));
        assert(!sameTail(a, e));

        //verifies R-value compatibilty
        assert(a.sameHead(a[0 .. 0]));
        assert(a.sameTail(a[$ .. $]));
    }
}

/**
Params:
    s = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    or a dynamic array
    n = number of times to repeat `s`

Returns:
    An array that consists of `s` repeated `n` times. This function allocates, fills, and
    returns a new array.

See_Also:
    For a lazy version, refer to $(REF repeat, std,range).
 */
ElementEncodingType!S[] replicate(S)(S s, size_t n)
if (isDynamicArray!S)
{
    alias RetType = ElementEncodingType!S[];

    // Optimization for return join(std.range.repeat(s, n));
    if (n == 0)
        return RetType.init;
    if (n == 1)
        return cast(RetType) s;
    auto r = new Unqual!(typeof(s[0]))[n * s.length];
    if (s.length == 1)
        r[] = s[0];
    else
    {
        immutable len = s.length, nlen = n * len;
        for (size_t i = 0; i < nlen; i += len)
        {
            r[i .. i + len] = s[];
        }
    }
    return r;
}

/// ditto
ElementType!S[] replicate(S)(S s, size_t n)
if (isInputRange!S && !isDynamicArray!S)
{
    import std.range : repeat;
    return join(std.range.repeat(s, n));
}


///
@safe unittest
{
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
    import std.conv : to;

    foreach (S; AliasSeq!(string, wstring, dstring, char[], wchar[], dchar[]))
    {
        S s;
        immutable S t = "abc";

        assert(replicate(to!S("1234"), 0) is null);
        assert(replicate(to!S("1234"), 0) is null);
        assert(replicate(to!S("1234"), 1) == "1234");
        assert(replicate(to!S("1234"), 2) == "12341234");
        assert(replicate(to!S("1"), 4) == "1111");
        assert(replicate(t, 3) == "abcabcabc");
        assert(replicate(cast(S) null, 4) is null);
    }
}

/++
Eagerly split the string $(D s) into an array of words, using whitespace as
delimiter. Runs of whitespace are merged together (no empty words are produced).

$(D @safe), $(D pure) and $(D CTFE)-able.

Params:
    s = the string to split

Returns:
    An array of each word in `s`

See_Also:
$(REF splitter, std,algorithm,iteration) for a version that splits using any
separator.

$(REF splitter, std,regex) for a version that splits using a regular
expression defined separator.
+/
S[] split(S)(S s) @safe pure
if (isSomeString!S)
{
    size_t istart;
    bool inword = false;
    S[] result;

    foreach (i, dchar c ; s)
    {
        import std.uni : isWhite;
        if (isWhite(c))
        {
            if (inword)
            {
                result ~= s[istart .. i];
                inword = false;
            }
        }
        else
        {
            if (!inword)
            {
                istart = i;
                inword = true;
            }
        }
    }
    if (inword)
        result ~= s[istart .. $];
    return result;
}

///
@safe unittest
{
    string str = "Hello World!";
    assert(str.split == ["Hello", "World!"]);

    string str2 = "Hello\t\tWorld\t!";
    assert(str2.split == ["Hello", "World", "!"]);
}

/**
 * `split` allocates memory, so the same effect can be achieved lazily
 * using $(REF splitter, std,algorithm,iteration).
 */
@safe unittest
{
    import std.ascii : isWhite;
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : splitter;

    string str = "Hello World!";
    assert(str.splitter!(isWhite).equal(["Hello", "World!"]));
}

@safe unittest
{
    import std.conv : to;
    import std.format;
    import std.typecons;

    static auto makeEntry(S)(string l, string[] r)
    {return tuple(l.to!S(), r.to!(S[])());}

    foreach (S; AliasSeq!(string, wstring, dstring,))
    {
        auto entries =
        [
            makeEntry!S("", []),
            makeEntry!S(" ", []),
            makeEntry!S("hello", ["hello"]),
            makeEntry!S(" hello ", ["hello"]),
            makeEntry!S("  h  e  l  l  o ", ["h", "e", "l", "l", "o"]),
            makeEntry!S("peter\t\npaul\rjerry", ["peter", "paul", "jerry"]),
            makeEntry!S(" \t\npeter paul\tjerry \n", ["peter", "paul", "jerry"]),
            makeEntry!S("\u2000日\u202F本\u205F語\u3000", ["日", "本", "語"]),
            makeEntry!S("　　哈・郎博尔德｝　　　　___一个", ["哈・郎博尔德｝", "___一个"])
        ];
        foreach (entry; entries)
            assert(entry[0].split() == entry[1], format("got: %s, expected: %s.", entry[0].split(), entry[1]));
    }

    //Just to test that an immutable is split-able
    immutable string s = " \t\npeter paul\tjerry \n";
    assert(split(s) == ["peter", "paul", "jerry"]);
}

@safe unittest //purity, ctfe ...
{
    import std.exception;
    void dg() @safe pure {
        assert(split("hello world"c) == ["hello"c, "world"c]);
        assert(split("hello world"w) == ["hello"w, "world"w]);
        assert(split("hello world"d) == ["hello"d, "world"d]);
    }
    dg();
    assertCTFEable!dg;
}

///
@safe unittest
{
    assert(split("hello world") == ["hello","world"]);
    assert(split("192.168.0.1", ".") == ["192", "168", "0", "1"]);

    auto a = split([1, 2, 3, 4, 5, 1, 2, 3, 4, 5], [2, 3]);
    assert(a == [[1], [4, 5, 1], [4, 5]]);
}

/++
    Eagerly splits $(D range) into an array, using $(D sep) as the delimiter.

    The _range must be a
    $(REF_ALTTEXT forward _range, isForwardRange, std,_range,primitives).
    The separator can be a value of the same type as the elements in $(D range)
    or it can be another forward _range.

    Example:
        If $(D range) is a $(D string), $(D sep) can be a $(D char) or another
        $(D string). The return type will be an array of strings. If $(D range) is
        an $(D int) array, $(D sep) can be an $(D int) or another $(D int) array.
        The return type will be an array of $(D int) arrays.

    Params:
        range = a forward _range.
        sep = a value of the same type as the elements of $(D range) or another
        forward range.

    Returns:
        An array containing the divided parts of $(D range).

    See_Also:
        $(REF splitter, std,algorithm,iteration) for the lazy version of this
        function.
 +/
auto split(Range, Separator)(Range range, Separator sep)
if (isForwardRange!Range && is(typeof(ElementType!Range.init == Separator.init)))
{
    import std.algorithm.iteration : splitter;
    return range.splitter(sep).array;
}
///ditto
auto split(Range, Separator)(Range range, Separator sep)
if (
    isForwardRange!Range && isForwardRange!Separator
    && is(typeof(ElementType!Range.init == ElementType!Separator.init)))
{
    import std.algorithm.iteration : splitter;
    return range.splitter(sep).array;
}
///ditto
auto split(alias isTerminator, Range)(Range range)
if (isForwardRange!Range && is(typeof(unaryFun!isTerminator(range.front))))
{
    import std.algorithm.iteration : splitter;
    return range.splitter!isTerminator.array;
}

///
@safe unittest
{
    import std.uni : isWhite;
    assert("Learning,D,is,fun".split(",") == ["Learning", "D", "is", "fun"]);
    assert("Learning D is fun".split!isWhite == ["Learning", "D", "is", "fun"]);
    assert("Learning D is fun".split(" D ") == ["Learning", "is fun"]);
}

@safe unittest
{
    import std.algorithm.comparison : cmp;
    import std.conv;

    foreach (S; AliasSeq!(string, wstring, dstring,
                    immutable(string), immutable(wstring), immutable(dstring),
                    char[], wchar[], dchar[],
                    const(char)[], const(wchar)[], const(dchar)[],
                    const(char[]), immutable(char[])))
    {
        S s = to!S(",peter,paul,jerry,");

        auto words = split(s, ",");
        assert(words.length == 5, text(words.length));
        assert(cmp(words[0], "") == 0);
        assert(cmp(words[1], "peter") == 0);
        assert(cmp(words[2], "paul") == 0);
        assert(cmp(words[3], "jerry") == 0);
        assert(cmp(words[4], "") == 0);

        auto s1 = s[0 .. s.length - 1];   // lop off trailing ','
        words = split(s1, ",");
        assert(words.length == 4);
        assert(cmp(words[3], "jerry") == 0);

        auto s2 = s1[1 .. s1.length];   // lop off leading ','
        words = split(s2, ",");
        assert(words.length == 3);
        assert(cmp(words[0], "peter") == 0);

        auto s3 = to!S(",,peter,,paul,,jerry,,");

        words = split(s3, ",,");
        assert(words.length == 5);
        assert(cmp(words[0], "") == 0);
        assert(cmp(words[1], "peter") == 0);
        assert(cmp(words[2], "paul") == 0);
        assert(cmp(words[3], "jerry") == 0);
        assert(cmp(words[4], "") == 0);

        auto s4 = s3[0 .. s3.length - 2];    // lop off trailing ',,'
        words = split(s4, ",,");
        assert(words.length == 4);
        assert(cmp(words[3], "jerry") == 0);

        auto s5 = s4[2 .. s4.length];    // lop off leading ',,'
        words = split(s5, ",,");
        assert(words.length == 3);
        assert(cmp(words[0], "peter") == 0);
    }
}

/++
   Conservative heuristic to determine if a range can be iterated cheaply.
   Used by $(D join) in decision to do an extra iteration of the range to
   compute the resultant length. If iteration is not cheap then precomputing
   length could be more expensive than using $(D Appender).

   For now, we only assume arrays are cheap to iterate.
 +/
private enum bool hasCheapIteration(R) = isArray!R;

/++
   Eagerly concatenates all of the ranges in `ror` together (with the GC)
   into one array using `sep` as the separator if present.

   Params:
        ror = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
        of input ranges
        sep = An input range, or a single element, to join the ranges on

   Returns:
        An array of elements

   See_Also:
        For a lazy version, see $(REF joiner, std,algorithm,iteration)
  +/
ElementEncodingType!(ElementType!RoR)[] join(RoR, R)(RoR ror, scope R sep)
if (isInputRange!RoR &&
    isInputRange!(Unqual!(ElementType!RoR)) &&
    isInputRange!R &&
    is(Unqual!(ElementType!(ElementType!RoR)) == Unqual!(ElementType!R)))
{
    alias RetType = typeof(return);
    alias RetTypeElement = Unqual!(ElementEncodingType!RetType);
    alias RoRElem = ElementType!RoR;

    if (ror.empty)
        return RetType.init;

    // Constraint only requires input range for sep.
    // This converts sep to an array (forward range) if it isn't one,
    // and makes sure it has the same string encoding for string types.
    static if (isSomeString!RetType &&
               !is(RetTypeElement == Unqual!(ElementEncodingType!R)))
    {
        import std.conv : to;
        auto sepArr = to!RetType(sep);
    }
    else static if (!isArray!R)
        auto sepArr = array(sep);
    else
        alias sepArr = sep;

    static if (hasCheapIteration!RoR && (hasLength!RoRElem || isNarrowString!RoRElem))
    {
        import std.conv : emplaceRef;
        size_t length;          // length of result array
        size_t rorLength;       // length of range ror
        foreach (r; ror.save)
        {
            length += r.length;
            ++rorLength;
        }
        if (!rorLength)
            return null;
        length += (rorLength - 1) * sepArr.length;

        auto result = (() @trusted => uninitializedArray!(RetTypeElement[])(length))();
        size_t len;
        foreach (e; ror.front)
            emplaceRef(result[len++], e);
        ror.popFront();
        foreach (r; ror)
        {
            foreach (e; sepArr)
                emplaceRef(result[len++], e);
            foreach (e; r)
                emplaceRef(result[len++], e);
        }
        assert(len == result.length);
        return (() @trusted => cast(RetType) result)();
    }
    else
    {
        auto result = appender!RetType();
        put(result, ror.front);
        ror.popFront();
        for (; !ror.empty; ror.popFront())
        {
            put(result, sep);
            put(result, ror.front);
        }
        return result.data;
    }
}

@safe unittest // Issue 14230
{
   string[] ary = ["","aa","bb","cc"]; // leaded by _empty_ element
   assert(ary.join(" @") == " @aa @bb @cc"); // OK in 2.067b1 and olders
}

/// Ditto
ElementEncodingType!(ElementType!RoR)[] join(RoR, E)(RoR ror, scope E sep)
if (isInputRange!RoR &&
    isInputRange!(Unqual!(ElementType!RoR)) &&
    is(E : ElementType!(ElementType!RoR)))
{
    alias RetType = typeof(return);
    alias RetTypeElement = Unqual!(ElementEncodingType!RetType);
    alias RoRElem = ElementType!RoR;

    if (ror.empty)
        return RetType.init;

    static if (hasCheapIteration!RoR && (hasLength!RoRElem || isNarrowString!RoRElem))
    {
        static if (isSomeChar!E && isSomeChar!RetTypeElement && E.sizeof > RetTypeElement.sizeof)
        {
            import std.utf : encode;
            RetTypeElement[4 / RetTypeElement.sizeof] encodeSpace;
            immutable size_t sepArrLength = encode(encodeSpace, sep);
            return join(ror, encodeSpace[0 .. sepArrLength]);
        }
        else
        {
            import std.conv : emplaceRef;
            size_t length;
            size_t rorLength;
            foreach (r; ror.save)
            {
                length += r.length;
                ++rorLength;
            }
            if (!rorLength)
                return null;
            length += rorLength - 1;
            auto result = uninitializedArray!(RetTypeElement[])(length);


            size_t len;
            foreach (e; ror.front)
                emplaceRef(result[len++], e);
            ror.popFront();
            foreach (r; ror)
            {
                emplaceRef(result[len++], sep);
                foreach (e; r)
                    emplaceRef(result[len++], e);
            }
            assert(len == result.length);
            return (() @trusted => cast(RetType) result)();
        }
    }
    else
    {
        auto result = appender!RetType();
        put(result, ror.front);
        ror.popFront();
        for (; !ror.empty; ror.popFront())
        {
            put(result, sep);
            put(result, ror.front);
        }
        return result.data;
    }
}

@safe unittest // Issue 10895
{
    class A
    {
        string name;
        alias name this;
        this(string name) { this.name = name; }
    }
    auto a = [new A(`foo`)];
    assert(a[0].length == 3);
    auto temp = join(a, " ");
    assert(a[0].length == 3);
}

@safe unittest // Issue 14230
{
   string[] ary = ["","aa","bb","cc"];
   assert(ary.join('@') == "@aa@bb@cc");
}

/// Ditto
ElementEncodingType!(ElementType!RoR)[] join(RoR)(RoR ror)
if (isInputRange!RoR &&
    isInputRange!(Unqual!(ElementType!RoR)))
{
    alias RetType = typeof(return);
    alias RetTypeElement = Unqual!(ElementEncodingType!RetType);
    alias RoRElem = ElementType!RoR;

    if (ror.empty)
        return RetType.init;

    static if (hasCheapIteration!RoR && (hasLength!RoRElem || isNarrowString!RoRElem))
    {
        import std.conv : emplaceRef;
        size_t length;
        foreach (r; ror.save)
            length += r.length;

        auto result = (() @trusted => uninitializedArray!(RetTypeElement[])(length))();
        size_t len;
        foreach (r; ror)
            foreach (e; r)
                emplaceRef(result[len++], e);
        assert(len == result.length);
        return (() @trusted => cast(RetType) result)();
    }
    else
    {
        auto result = appender!RetType();
        for (; !ror.empty; ror.popFront())
            put(result, ror.front);
        return result.data;
    }
}

///
@safe pure nothrow unittest
{
    assert(join(["hello", "silly", "world"], " ") == "hello silly world");
    assert(join(["hello", "silly", "world"]) == "hellosillyworld");

    assert(join([[1, 2, 3], [4, 5]], [72, 73]) == [1, 2, 3, 72, 73, 4, 5]);
    assert(join([[1, 2, 3], [4, 5]]) == [1, 2, 3, 4, 5]);

    const string[] arr = ["apple", "banana"];
    assert(arr.join(",") == "apple,banana");
    assert(arr.join() == "applebanana");
}

@safe pure unittest
{
    import std.conv : to;

    foreach (T; AliasSeq!(string,wstring,dstring))
    {
        auto arr2 = "Здравствуй Мир Unicode".to!(T);
        auto arr = ["Здравствуй", "Мир", "Unicode"].to!(T[]);
        assert(join(arr) == "ЗдравствуйМирUnicode");
        foreach (S; AliasSeq!(char,wchar,dchar))
        {
            auto jarr = arr.join(to!S(' '));
            static assert(is(typeof(jarr) == T));
            assert(jarr == arr2);
        }
        foreach (S; AliasSeq!(string,wstring,dstring))
        {
            auto jarr = arr.join(to!S(" "));
            static assert(is(typeof(jarr) == T));
            assert(jarr == arr2);
        }
    }

    foreach (T; AliasSeq!(string,wstring,dstring))
    {
        auto arr2 = "Здравствуй\u047CМир\u047CUnicode".to!(T);
        auto arr = ["Здравствуй", "Мир", "Unicode"].to!(T[]);
        foreach (S; AliasSeq!(wchar,dchar))
        {
            auto jarr = arr.join(to!S('\u047C'));
            static assert(is(typeof(jarr) == T));
            assert(jarr == arr2);
        }
    }

    const string[] arr = ["apple", "banana"];
    assert(arr.join(',') == "apple,banana");
}

@system unittest
{
    import std.algorithm;
    import std.conv : to;
    import std.range;

    foreach (R; AliasSeq!(string, wstring, dstring))
    {
        R word1 = "日本語";
        R word2 = "paul";
        R word3 = "jerry";
        R[] words = [word1, word2, word3];

        auto filteredWord1    = filter!"true"(word1);
        auto filteredLenWord1 = takeExactly(filteredWord1, word1.walkLength());
        auto filteredWord2    = filter!"true"(word2);
        auto filteredLenWord2 = takeExactly(filteredWord2, word2.walkLength());
        auto filteredWord3    = filter!"true"(word3);
        auto filteredLenWord3 = takeExactly(filteredWord3, word3.walkLength());
        auto filteredWordsArr = [filteredWord1, filteredWord2, filteredWord3];
        auto filteredLenWordsArr = [filteredLenWord1, filteredLenWord2, filteredLenWord3];
        auto filteredWords    = filter!"true"(filteredWordsArr);

        foreach (S; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(join(filteredWords, to!S(", ")) == "日本語, paul, jerry");
            assert(join(filteredWords, to!(ElementType!S)(',')) == "日本語,paul,jerry");
            assert(join(filteredWordsArr, to!(ElementType!(S))(',')) == "日本語,paul,jerry");
            assert(join(filteredWordsArr, to!S(", ")) == "日本語, paul, jerry");
            assert(join(filteredWordsArr, to!(ElementType!(S))(',')) == "日本語,paul,jerry");
            assert(join(filteredLenWordsArr, to!S(", ")) == "日本語, paul, jerry");
            assert(join(filter!"true"(words), to!S(", ")) == "日本語, paul, jerry");
            assert(join(words, to!S(", ")) == "日本語, paul, jerry");

            assert(join(filteredWords, to!S("")) == "日本語pauljerry");
            assert(join(filteredWordsArr, to!S("")) == "日本語pauljerry");
            assert(join(filteredLenWordsArr, to!S("")) == "日本語pauljerry");
            assert(join(filter!"true"(words), to!S("")) == "日本語pauljerry");
            assert(join(words, to!S("")) == "日本語pauljerry");

            assert(join(filter!"true"([word1]), to!S(", ")) == "日本語");
            assert(join([filteredWord1], to!S(", ")) == "日本語");
            assert(join([filteredLenWord1], to!S(", ")) == "日本語");
            assert(join(filter!"true"([filteredWord1]), to!S(", ")) == "日本語");
            assert(join([word1], to!S(", ")) == "日本語");

            assert(join(filteredWords, to!S(word1)) == "日本語日本語paul日本語jerry");
            assert(join(filteredWordsArr, to!S(word1)) == "日本語日本語paul日本語jerry");
            assert(join(filteredLenWordsArr, to!S(word1)) == "日本語日本語paul日本語jerry");
            assert(join(filter!"true"(words), to!S(word1)) == "日本語日本語paul日本語jerry");
            assert(join(words, to!S(word1)) == "日本語日本語paul日本語jerry");

            auto filterComma = filter!"true"(to!S(", "));
            assert(join(filteredWords, filterComma) == "日本語, paul, jerry");
            assert(join(filteredWordsArr, filterComma) == "日本語, paul, jerry");
            assert(join(filteredLenWordsArr, filterComma) == "日本語, paul, jerry");
            assert(join(filter!"true"(words), filterComma) == "日本語, paul, jerry");
            assert(join(words, filterComma) == "日本語, paul, jerry");
        }();

        assert(join(filteredWords) == "日本語pauljerry");
        assert(join(filteredWordsArr) == "日本語pauljerry");
        assert(join(filteredLenWordsArr) == "日本語pauljerry");
        assert(join(filter!"true"(words)) == "日本語pauljerry");
        assert(join(words) == "日本語pauljerry");

        assert(join(filteredWords, filter!"true"(", ")) == "日本語, paul, jerry");
        assert(join(filteredWordsArr, filter!"true"(", ")) == "日本語, paul, jerry");
        assert(join(filteredLenWordsArr, filter!"true"(", ")) == "日本語, paul, jerry");
        assert(join(filter!"true"(words), filter!"true"(", ")) == "日本語, paul, jerry");
        assert(join(words, filter!"true"(", ")) == "日本語, paul, jerry");

        assert(join(filter!"true"(cast(typeof(filteredWordsArr))[]), ", ").empty);
        assert(join(cast(typeof(filteredWordsArr))[], ", ").empty);
        assert(join(cast(typeof(filteredLenWordsArr))[], ", ").empty);
        assert(join(filter!"true"(cast(R[])[]), ", ").empty);
        assert(join(cast(R[])[], ", ").empty);

        assert(join(filter!"true"(cast(typeof(filteredWordsArr))[])).empty);
        assert(join(cast(typeof(filteredWordsArr))[]).empty);
        assert(join(cast(typeof(filteredLenWordsArr))[]).empty);

        assert(join(filter!"true"(cast(R[])[])).empty);
        assert(join(cast(R[])[]).empty);
    }

    assert(join([[1, 2], [41, 42]], [5, 6]) == [1, 2, 5, 6, 41, 42]);
    assert(join([[1, 2], [41, 42]], cast(int[])[]) == [1, 2, 41, 42]);
    assert(join([[1, 2]], [5, 6]) == [1, 2]);
    assert(join(cast(int[][])[], [5, 6]).empty);

    assert(join([[1, 2], [41, 42]]) == [1, 2, 41, 42]);
    assert(join(cast(int[][])[]).empty);

    alias f = filter!"true";
    assert(join([[1, 2], [41, 42]],          [5, 6]) == [1, 2, 5, 6, 41, 42]);
    assert(join(f([[1, 2], [41, 42]]),       [5, 6]) == [1, 2, 5, 6, 41, 42]);
    assert(join([f([1, 2]), f([41, 42])],    [5, 6]) == [1, 2, 5, 6, 41, 42]);
    assert(join(f([f([1, 2]), f([41, 42])]), [5, 6]) == [1, 2, 5, 6, 41, 42]);
    assert(join([[1, 2], [41, 42]],          f([5, 6])) == [1, 2, 5, 6, 41, 42]);
    assert(join(f([[1, 2], [41, 42]]),       f([5, 6])) == [1, 2, 5, 6, 41, 42]);
    assert(join([f([1, 2]), f([41, 42])],    f([5, 6])) == [1, 2, 5, 6, 41, 42]);
    assert(join(f([f([1, 2]), f([41, 42])]), f([5, 6])) == [1, 2, 5, 6, 41, 42]);
}

// Issue 10683
@safe unittest
{
    import std.range : join;
    import std.typecons : tuple;
    assert([[tuple(1)]].join == [tuple(1)]);
    assert([[tuple("x")]].join == [tuple("x")]);
}

// Issue 13877
@safe unittest
{
    // Test that the range is iterated only once.
    import std.algorithm.iteration : map;
    int c = 0;
    auto j1 = [1, 2, 3].map!(_ => [c++]).join;
    assert(c == 3);
    assert(j1 == [0, 1, 2]);

    c = 0;
    auto j2 = [1, 2, 3].map!(_ => [c++]).join(9);
    assert(c == 3);
    assert(j2 == [0, 9, 1, 9, 2]);

    c = 0;
    auto j3 = [1, 2, 3].map!(_ => [c++]).join([9]);
    assert(c == 3);
    assert(j3 == [0, 9, 1, 9, 2]);
}


/++
    Replace occurrences of `from` with `to` in `subject` in a new
    array. If `sink` is defined, then output the new array into
    `sink`.

    Params:
        sink = an $(REF_ALTTEXT output range, isOutputRange, std,range,primitives)
        subject = the array to scan
        from = the item to replace
        to = the item to replace all instances of `from` with

    Returns:
        If `sink` isn't defined, a new array without changing the
        contents of `subject`, or the original array if no match
        is found.

    See_Also:
        $(REF map, std,algorithm,iteration) which can act as a lazy replace
 +/
E[] replace(E, R1, R2)(E[] subject, R1 from, R2 to)
if (isDynamicArray!(E[]) && isForwardRange!R1 && isForwardRange!R2
        && (hasLength!R2 || isSomeString!R2))
{
    import std.algorithm.searching : find;

    if (from.empty) return subject;

    auto balance = find(subject, from.save);
    if (balance.empty)
        return subject;

    auto app = appender!(E[])();
    app.put(subject[0 .. subject.length - balance.length]);
    app.put(to.save);
    replaceInto(app, balance[from.length .. $], from, to);

    return app.data;
}

///
@safe unittest
{
    assert("Hello Wörld".replace("o Wö", "o Wo") == "Hello World");
    assert("Hello Wörld".replace("l", "h") == "Hehho Wörhd");
}

/// ditto
void replaceInto(E, Sink, R1, R2)(Sink sink, E[] subject, R1 from, R2 to)
if (isOutputRange!(Sink, E) && isDynamicArray!(E[])
    && isForwardRange!R1 && isForwardRange!R2
    && (hasLength!R2 || isSomeString!R2))
{
    import std.algorithm.searching : find;

    if (from.empty)
    {
        sink.put(subject);
        return;
    }
    for (;;)
    {
        auto balance = find(subject, from.save);
        if (balance.empty)
        {
            sink.put(subject);
            break;
        }
        sink.put(subject[0 .. subject.length - balance.length]);
        sink.put(to.save);
        subject = balance[from.length .. $];
    }
}

///
@safe unittest
{
    auto arr = [1, 2, 3, 4, 5];
    auto from = [2, 3];
    auto to = [4, 6];
    auto sink = appender!(int[])();

    replaceInto(sink, arr, from, to);

    assert(sink.data == [1, 4, 6, 4, 5]);
}

@safe unittest
{
    import std.algorithm.comparison : cmp;
    import std.conv : to;

    foreach (S; AliasSeq!(string, wstring, dstring, char[], wchar[], dchar[]))
    {
        foreach (T; AliasSeq!(string, wstring, dstring, char[], wchar[], dchar[]))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            auto s = to!S("This is a foo foo list");
            auto from = to!T("foo");
            auto into = to!S("silly");
            S r;
            int i;

            r = replace(s, from, into);
            i = cmp(r, "This is a silly silly list");
            assert(i == 0);

            r = replace(s, to!S(""), into);
            i = cmp(r, "This is a foo foo list");
            assert(i == 0);

            assert(replace(r, to!S("won't find this"), to!S("whatever")) is r);
        }();
    }

    immutable s = "This is a foo foo list";
    assert(replace(s, "foo", "silly") == "This is a silly silly list");
}

@safe unittest
{
    import std.algorithm.searching : skipOver;
    import std.conv : to;

    struct CheckOutput(C)
    {
        C[] desired;
        this(C[] arr){ desired = arr; }
        void put(C[] part){ assert(skipOver(desired, part)); }
    }
    foreach (S; AliasSeq!(string, wstring, dstring, char[], wchar[], dchar[]))
    {
        alias Char = ElementEncodingType!S;
        S s = to!S("yet another dummy text, yet another ...");
        S from = to!S("yet another");
        S into = to!S("some");
        replaceInto(CheckOutput!(Char)(to!S("some dummy text, some ..."))
                    , s, from, into);
    }
}

/++
    Replaces elements from `array` with indices ranging from `from`
    (inclusive) to `to` (exclusive) with the range `stuff`.

    Params:
        subject = the array to scan
        from = the starting index
        to = the ending index
        stuff = the items to replace in-between `from` and `to`

    Returns:
        A new array without changing the contents of `subject`.
 +/
T[] replace(T, Range)(T[] subject, size_t from, size_t to, Range stuff)
if (isInputRange!Range &&
    (is(ElementType!Range : T) ||
    isSomeString!(T[]) && is(ElementType!Range : dchar)))
{
    static if (hasLength!Range && is(ElementEncodingType!Range : T))
    {
        import std.algorithm.mutation : copy;
        assert(from <= to);
        immutable sliceLen = to - from;
        auto retval = new Unqual!(T)[](subject.length - sliceLen + stuff.length);
        retval[0 .. from] = subject[0 .. from];

        if (!stuff.empty)
            copy(stuff, retval[from .. from + stuff.length]);

        retval[from + stuff.length .. $] = subject[to .. $];
        return cast(T[]) retval;
    }
    else
    {
        auto app = appender!(T[])();
        app.put(subject[0 .. from]);
        app.put(stuff);
        app.put(subject[to .. $]);
        return app.data;
    }
}

///
@safe unittest
{
    auto a = [ 1, 2, 3, 4 ];
    auto b = a.replace(1, 3, [ 9, 9, 9 ]);
    assert(a == [ 1, 2, 3, 4 ]);
    assert(b == [ 1, 9, 9, 9, 4 ]);
}

@system unittest
{
    import core.exception;
    import std.algorithm.iteration : filter;
    import std.conv : to;
    import std.exception;


    auto a = [ 1, 2, 3, 4 ];
    assert(replace(a, 0, 0, [5, 6, 7]) == [5, 6, 7, 1, 2, 3, 4]);
    assert(replace(a, 0, 2, cast(int[])[]) == [3, 4]);
    assert(replace(a, 0, 4, [5, 6, 7]) == [5, 6, 7]);
    assert(replace(a, 0, 2, [5, 6, 7]) == [5, 6, 7, 3, 4]);
    assert(replace(a, 2, 4, [5, 6, 7]) == [1, 2, 5, 6, 7]);

    assert(replace(a, 0, 0, filter!"true"([5, 6, 7])) == [5, 6, 7, 1, 2, 3, 4]);
    assert(replace(a, 0, 2, filter!"true"(cast(int[])[])) == [3, 4]);
    assert(replace(a, 0, 4, filter!"true"([5, 6, 7])) == [5, 6, 7]);
    assert(replace(a, 0, 2, filter!"true"([5, 6, 7])) == [5, 6, 7, 3, 4]);
    assert(replace(a, 2, 4, filter!"true"([5, 6, 7])) == [1, 2, 5, 6, 7]);
    assert(a == [ 1, 2, 3, 4 ]);

    void testStr(T, U)(string file = __FILE__, size_t line = __LINE__)
    {

        auto l = to!T("hello");
        auto r = to!U(" world");

        enforce(replace(l, 0, 0, r) == " worldhello",
                new AssertError("testStr failure 1", file, line));
        enforce(replace(l, 0, 3, r) == " worldlo",
                new AssertError("testStr failure 2", file, line));
        enforce(replace(l, 3, l.length, r) == "hel world",
                new AssertError("testStr failure 3", file, line));
        enforce(replace(l, 0, l.length, r) == " world",
                new AssertError("testStr failure 4", file, line));
        enforce(replace(l, l.length, l.length, r) == "hello world",
                new AssertError("testStr failure 5", file, line));
    }

    testStr!(string, string)();
    testStr!(string, wstring)();
    testStr!(string, dstring)();
    testStr!(wstring, string)();
    testStr!(wstring, wstring)();
    testStr!(wstring, dstring)();
    testStr!(dstring, string)();
    testStr!(dstring, wstring)();
    testStr!(dstring, dstring)();

    enum s = "0123456789";
    enum w = "⁰¹²³⁴⁵⁶⁷⁸⁹"w;
    enum d = "⁰¹²³⁴⁵⁶⁷⁸⁹"d;

    assert(replace(s, 0, 0, "***") == "***0123456789");
    assert(replace(s, 10, 10, "***") == "0123456789***");
    assert(replace(s, 3, 8, "1012") == "012101289");
    assert(replace(s, 0, 5, "43210") == "4321056789");
    assert(replace(s, 5, 10, "43210") == "0123443210");

    assert(replace(w, 0, 0, "***"w) == "***⁰¹²³⁴⁵⁶⁷⁸⁹"w);
    assert(replace(w, 10, 10, "***"w) == "⁰¹²³⁴⁵⁶⁷⁸⁹***"w);
    assert(replace(w, 3, 8, "¹⁰¹²"w) == "⁰¹²¹⁰¹²⁸⁹"w);
    assert(replace(w, 0, 5, "⁴³²¹⁰"w) == "⁴³²¹⁰⁵⁶⁷⁸⁹"w);
    assert(replace(w, 5, 10, "⁴³²¹⁰"w) == "⁰¹²³⁴⁴³²¹⁰"w);

    assert(replace(d, 0, 0, "***"d) == "***⁰¹²³⁴⁵⁶⁷⁸⁹"d);
    assert(replace(d, 10, 10, "***"d) == "⁰¹²³⁴⁵⁶⁷⁸⁹***"d);
    assert(replace(d, 3, 8, "¹⁰¹²"d) == "⁰¹²¹⁰¹²⁸⁹"d);
    assert(replace(d, 0, 5, "⁴³²¹⁰"d) == "⁴³²¹⁰⁵⁶⁷⁸⁹"d);
    assert(replace(d, 5, 10, "⁴³²¹⁰"d) == "⁰¹²³⁴⁴³²¹⁰"d);
}

/++
    Replaces elements from `array` with indices ranging from `from`
    (inclusive) to `to` (exclusive) with the range `stuff`. Expands or
    shrinks the array as needed.

    Params:
        array = the _array to scan
        from = the starting index
        to = the ending index
        stuff = the items to replace in-between `from` and `to`
 +/
void replaceInPlace(T, Range)(ref T[] array, size_t from, size_t to, Range stuff)
if (is(typeof(replace(array, from, to, stuff))))
{
    static if (isDynamicArray!Range &&
              is(Unqual!(ElementEncodingType!Range) == T) &&
              !isNarrowString!(T[]))
    {
        // optimized for homogeneous arrays that can be overwritten.
        import std.algorithm.mutation : remove;
        import std.typecons : tuple;

        if (overlap(array, stuff).length)
        {
            // use slower/conservative method
            array = array[0 .. from] ~ stuff ~ array[to .. $];
        }
        else if (stuff.length <= to - from)
        {
            // replacement reduces length
            immutable stuffEnd = from + stuff.length;
            array[from .. stuffEnd] = stuff[];
            if (stuffEnd < to)
                array = remove(array, tuple(stuffEnd, to));
        }
        else
        {
            // replacement increases length
            // @@@TODO@@@: optimize this
            immutable replaceLen = to - from;
            array[from .. to] = stuff[0 .. replaceLen];
            insertInPlace(array, to, stuff[replaceLen .. $]);
        }
    }
    else
    {
        // default implementation, just do what replace does.
        array = replace(array, from, to, stuff);
    }
}

///
@safe unittest
{
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
    // Bug# 12889
    int[1][] arr = [[0], [1], [2], [3], [4], [5], [6]];
    int[1][] stuff = [[0], [1]];
    replaceInPlace(arr, 4, 6, stuff);
    assert(arr == [[0], [1], [2], [3], [0], [1], [6]]);
}

@system unittest
{
    // Bug# 14925
    char[] a = "mon texte 1".dup;
    char[] b = "abc".dup;
    replaceInPlace(a, 4, 9, b);
    assert(a == "mon abc 1");

    // ensure we can replace in place with different encodings
    string unicoded = "\U00010437";
    string unicodedLong = "\U00010437aaaaa";
    string base = "abcXXXxyz";
    string result = "abc\U00010437xyz";
    string resultLong = "abc\U00010437aaaaaxyz";
    size_t repstart = 3;
    size_t repend = 3 + 3;

    void testStringReplaceInPlace(T, U)()
    {
        import std.algorithm.comparison : equal;
        import std.conv;
        auto a = unicoded.to!(U[]);
        auto b = unicodedLong.to!(U[]);

        auto test = base.to!(T[]);

        test.replaceInPlace(repstart, repend, a);
        assert(equal(test, result), "Failed for types " ~ T.stringof ~ " and " ~ U.stringof);

        test = base.to!(T[]);

        test.replaceInPlace(repstart, repend, b);
        assert(equal(test, resultLong), "Failed for types " ~ T.stringof ~ " and " ~ U.stringof);
    }

    import std.meta : AliasSeq;
    alias allChars = AliasSeq!(char, immutable(char), const(char),
                         wchar, immutable(wchar), const(wchar),
                         dchar, immutable(dchar), const(dchar));
    foreach (T; allChars)
        foreach (U; allChars)
            testStringReplaceInPlace!(T, U)();

    void testInout(inout(int)[] a)
    {
        // will be transferred to the 'replace' function
        replaceInPlace(a, 1, 2, [1,2,3]);
    }
}

@safe unittest
{
    // the constraint for the first overload used to match this, which wouldn't compile.
    import std.algorithm.comparison : equal;
    long[] a = [1L, 2, 3];
    int[] b = [4, 5, 6];
    a.replaceInPlace(1, 2, b);
    assert(equal(a, [1L, 4, 5, 6, 3]));
}

@system unittest
{
    import core.exception;
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter;
    import std.conv : to;
    import std.exception;


    bool test(T, U, V)(T orig, size_t from, size_t to, U toReplace, V result,
               string file = __FILE__, size_t line = __LINE__)
    {
        {
            static if (is(T == typeof(T.init.dup)))
                auto a = orig.dup;
            else
                auto a = orig.idup;

            a.replaceInPlace(from, to, toReplace);
            if (!equal(a, result))
                return false;
        }

        static if (isInputRange!U)
        {
            orig.replaceInPlace(from, to, filter!"true"(toReplace));
            return equal(orig, result);
        }
        else
            return true;
    }

    assert(test([1, 2, 3, 4], 0, 0, [5, 6, 7], [5, 6, 7, 1, 2, 3, 4]));
    assert(test([1, 2, 3, 4], 0, 2, cast(int[])[], [3, 4]));
    assert(test([1, 2, 3, 4], 0, 4, [5, 6, 7], [5, 6, 7]));
    assert(test([1, 2, 3, 4], 0, 2, [5, 6, 7], [5, 6, 7, 3, 4]));
    assert(test([1, 2, 3, 4], 2, 4, [5, 6, 7], [1, 2, 5, 6, 7]));

    assert(test([1, 2, 3, 4], 0, 0, filter!"true"([5, 6, 7]), [5, 6, 7, 1, 2, 3, 4]));
    assert(test([1, 2, 3, 4], 0, 2, filter!"true"(cast(int[])[]), [3, 4]));
    assert(test([1, 2, 3, 4], 0, 4, filter!"true"([5, 6, 7]), [5, 6, 7]));
    assert(test([1, 2, 3, 4], 0, 2, filter!"true"([5, 6, 7]), [5, 6, 7, 3, 4]));
    assert(test([1, 2, 3, 4], 2, 4, filter!"true"([5, 6, 7]), [1, 2, 5, 6, 7]));

    void testStr(T, U)(string file = __FILE__, size_t line = __LINE__)
    {

        auto l = to!T("hello");
        auto r = to!U(" world");

        enforce(test(l, 0, 0, r, " worldhello"),
                new AssertError("testStr failure 1", file, line));
        enforce(test(l, 0, 3, r, " worldlo"),
                new AssertError("testStr failure 2", file, line));
        enforce(test(l, 3, l.length, r, "hel world"),
                new AssertError("testStr failure 3", file, line));
        enforce(test(l, 0, l.length, r, " world"),
                new AssertError("testStr failure 4", file, line));
        enforce(test(l, l.length, l.length, r, "hello world"),
                new AssertError("testStr failure 5", file, line));
    }

    testStr!(string, string)();
    testStr!(string, wstring)();
    testStr!(string, dstring)();
    testStr!(wstring, string)();
    testStr!(wstring, wstring)();
    testStr!(wstring, dstring)();
    testStr!(dstring, string)();
    testStr!(dstring, wstring)();
    testStr!(dstring, dstring)();
}

/++
    Replaces the first occurrence of `from` with `to` in `subject`.

    Params:
        subject = the array to scan
        from = the item to replace
        to = the item to replace `from` with

    Returns:
        A new array without changing the contents of $(D subject), or the original
        array if no match is found.
 +/
E[] replaceFirst(E, R1, R2)(E[] subject, R1 from, R2 to)
if (isDynamicArray!(E[]) &&
    isForwardRange!R1 && is(typeof(appender!(E[])().put(from[0 .. 1]))) &&
    isForwardRange!R2 && is(typeof(appender!(E[])().put(to[0 .. 1]))))
{
    if (from.empty) return subject;
    static if (isSomeString!(E[]))
    {
        import std.string : indexOf;
        immutable idx = subject.indexOf(from);
    }
    else
    {
        import std.algorithm.searching : countUntil;
        immutable idx = subject.countUntil(from);
    }
    if (idx == -1)
        return subject;

    auto app = appender!(E[])();
    app.put(subject[0 .. idx]);
    app.put(to);

    static if (isSomeString!(E[]) && isSomeString!R1)
    {
        import std.utf : codeLength;
        immutable fromLength = codeLength!(Unqual!E, R1)(from);
    }
    else
        immutable fromLength = from.length;

    app.put(subject[idx + fromLength .. $]);

    return app.data;
}

///
@safe unittest
{
    auto a = [1, 2, 2, 3, 4, 5];
    auto b = a.replaceFirst([2], [1337]);
    assert(b == [1, 1337, 2, 3, 4, 5]);

    auto s = "This is a foo foo list";
    auto r = s.replaceFirst("foo", "silly");
    assert(r == "This is a silly foo list");
}

@safe unittest
{
    import std.algorithm.comparison : cmp;
    import std.conv : to;

    foreach (S; AliasSeq!(string, wstring, dstring, char[], wchar[], dchar[],
                          const(char[]), immutable(char[])))
    {
        foreach (T; AliasSeq!(string, wstring, dstring, char[], wchar[], dchar[],
                              const(char[]), immutable(char[])))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            auto s = to!S("This is a foo foo list");
            auto s2 = to!S("Thüs is a ßöö foo list");
            auto from = to!T("foo");
            auto from2 = to!T("ßöö");
            auto into = to!T("silly");
            auto into2 = to!T("sälly");

            S r1 = replaceFirst(s, from, into);
            assert(cmp(r1, "This is a silly foo list") == 0);

            S r11 = replaceFirst(s2, from2, into2);
            assert(cmp(r11, "Thüs is a sälly foo list") == 0,
                to!string(r11) ~ " : " ~ S.stringof ~ " " ~ T.stringof);

            S r2 = replaceFirst(r1, from, into);
            assert(cmp(r2, "This is a silly silly list") == 0);

            S r3 = replaceFirst(s, to!T(""), into);
            assert(cmp(r3, "This is a foo foo list") == 0);

            assert(replaceFirst(r3, to!T("won't find"), to!T("whatever")) is r3);
        }();
    }
}

//Bug# 8187
@safe unittest
{
    auto res = ["a", "a"];
    assert(replace(res, "a", "b") == ["b", "b"]);
    assert(replaceFirst(res, "a", "b") == ["b", "a"]);
}

/++
    Replaces the last occurrence of `from` with `to` in `subject`.

    Params:
        subject = the array to scan
        from = the item to replace
        to = the item to replace `from` with

    Returns:
        A new array without changing the contents of $(D subject), or the original
        array if no match is found.
 +/
E[] replaceLast(E, R1, R2)(E[] subject, R1 from , R2 to)
if (isDynamicArray!(E[]) &&
    isForwardRange!R1 && is(typeof(appender!(E[])().put(from[0 .. 1]))) &&
    isForwardRange!R2 && is(typeof(appender!(E[])().put(to[0 .. 1]))))
{
    import std.range : retro;
    if (from.empty) return subject;
    static if (isSomeString!(E[]))
    {
        import std.string : lastIndexOf;
        auto idx = subject.lastIndexOf(from);
    }
    else
    {
        import std.algorithm.searching : countUntil;
        auto idx = retro(subject).countUntil(retro(from));
    }

    if (idx == -1)
        return subject;

    static if (isSomeString!(E[]) && isSomeString!R1)
    {
        import std.utf : codeLength;
        auto fromLength = codeLength!(Unqual!E, R1)(from);
    }
    else
        auto fromLength = from.length;

    auto app = appender!(E[])();
    static if (isSomeString!(E[]))
        app.put(subject[0 .. idx]);
    else
        app.put(subject[0 .. $ - idx - fromLength]);

    app.put(to);

    static if (isSomeString!(E[]))
        app.put(subject[idx+fromLength .. $]);
    else
        app.put(subject[$ - idx .. $]);

    return app.data;
}

///
@safe unittest
{
    auto a = [1, 2, 2, 3, 4, 5];
    auto b = a.replaceLast([2], [1337]);
    assert(b == [1, 2, 1337, 3, 4, 5]);

    auto s = "This is a foo foo list";
    auto r = s.replaceLast("foo", "silly");
    assert(r == "This is a foo silly list", r);
}

@safe unittest
{
    import std.algorithm.comparison : cmp;
    import std.conv : to;

    foreach (S; AliasSeq!(string, wstring, dstring, char[], wchar[], dchar[],
                          const(char[]), immutable(char[])))
    {
        foreach (T; AliasSeq!(string, wstring, dstring, char[], wchar[], dchar[],
                              const(char[]), immutable(char[])))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            auto s = to!S("This is a foo foo list");
            auto s2 = to!S("Thüs is a ßöö ßöö list");
            auto from = to!T("foo");
            auto from2 = to!T("ßöö");
            auto into = to!T("silly");
            auto into2 = to!T("sälly");

            S r1 = replaceLast(s, from, into);
            assert(cmp(r1, "This is a foo silly list") == 0, to!string(r1));

            S r11 = replaceLast(s2, from2, into2);
            assert(cmp(r11, "Thüs is a ßöö sälly list") == 0,
                to!string(r11) ~ " : " ~ S.stringof ~ " " ~ T.stringof);

            S r2 = replaceLast(r1, from, into);
            assert(cmp(r2, "This is a silly silly list") == 0);

            S r3 = replaceLast(s, to!T(""), into);
            assert(cmp(r3, "This is a foo foo list") == 0);

            assert(replaceLast(r3, to!T("won't find"), to!T("whatever")) is r3);
        }();
    }
}

/++
    Creates a new array such that the items in `slice` are replaced with the
    items in `replacement`. `slice` and `replacement` do not need to be the
    same length. The result will grow or shrink based on the items given.

    Params:
        s = the base of the new array
        slice = the slice of `s` to be replaced
        replacement = the items to replace `slice` with

    Returns:
        A new array that is `s` with `slice` replaced by
        `replacement[]`.
 +/
inout(T)[] replaceSlice(T)(inout(T)[] s, in T[] slice, in T[] replacement)
in
{
    // Verify that slice[] really is a slice of s[]
    assert(overlap(s, slice) is slice);
}
body
{
    auto result = new T[s.length - slice.length + replacement.length];
    immutable so = slice.ptr - s.ptr;
    result[0 .. so] = s[0 .. so];
    result[so .. so + replacement.length] = replacement[];
    result[so + replacement.length .. result.length] =
        s[so + slice.length .. s.length];

    return cast(inout(T)[]) result;
}

///
@system unittest
{
    auto a = [1, 2, 3, 4, 5];
    auto b = replaceSlice(a, a[1 .. 4], [0, 0, 0]);

    assert(b == [1, 0, 0, 0, 5]);
}

@system unittest
{
    import std.algorithm.comparison : cmp;

    string s = "hello";
    string slice = s[2 .. 4];

    auto r = replaceSlice(s, slice, "bar");
    int i;
    i = cmp(r, "hebaro");
    assert(i == 0);
}

/**
Implements an output range that appends data to an array. This is
recommended over $(D array ~= data) when appending many elements because it is more
efficient. `Appender` maintains its own array metadata locally, so it can avoid
global locking for each append where $(LREF capacity) is non-zero.
See_Also: $(LREF appender)
 */
struct Appender(A)
if (isDynamicArray!A)
{
    import core.memory : GC;

    private alias T = ElementEncodingType!A;

    private struct Data
    {
        size_t capacity;
        Unqual!T[] arr;
        bool canExtend = false;
    }

    private Data* _data;

    /**
     * Constructs an `Appender` with a given array.  Note that this does not copy the
     * data.  If the array has a larger capacity as determined by `arr.capacity`,
     * it will be used by the appender.  After initializing an appender on an array,
     * appending to the original array will reallocate.
     */
    this(A arr) @trusted pure nothrow
    {
        // initialize to a given array.
        _data = new Data;
        _data.arr = cast(Unqual!T[]) arr; //trusted

        if (__ctfe)
            return;

        // We want to use up as much of the block the array is in as possible.
        // if we consume all the block that we can, then array appending is
        // safe WRT built-in append, and we can use the entire block.
        // We only do this for mutable types that can be extended.
        static if (isMutable!T && is(typeof(arr.length = size_t.max)))
        {
            immutable cap = arr.capacity; //trusted
            // Replace with "GC.setAttr( Not Appendable )" once pure (and fixed)
            if (cap > arr.length)
                arr.length = cap;
        }
        _data.capacity = arr.length;
    }

    /**
     * Reserve at least newCapacity elements for appending.  Note that more elements
     * may be reserved than requested. If `newCapacity <= capacity`, then nothing is
     * done.
     */
    void reserve(size_t newCapacity) @safe pure nothrow
    {
        if (_data)
        {
            if (newCapacity > _data.capacity)
                ensureAddable(newCapacity - _data.arr.length);
        }
        else
        {
            ensureAddable(newCapacity);
        }
    }

    /**
     * Returns: the capacity of the array (the maximum number of elements the
     * managed array can accommodate before triggering a reallocation). If any
     * appending will reallocate, `0` will be returned.
     */
    @property size_t capacity() const @safe pure nothrow
    {
        return _data ? _data.capacity : 0;
    }

    /**
     * Use opSlice() from now on.
     * Returns: The managed array.
     */
    @property inout(ElementEncodingType!A)[] data() inout @trusted pure nothrow
    {
        return this[];
    }

    /**
     * Returns: The managed array.
     */
    @property inout(ElementEncodingType!A)[] opSlice() inout @trusted pure nothrow
    {
        /* @trusted operation:
         * casting Unqual!T[] to inout(T)[]
         */
        return cast(typeof(return))(_data ? _data.arr : null);
    }

    // ensure we can add nelems elements, resizing as necessary
    private void ensureAddable(size_t nelems) @trusted pure nothrow
    {
        if (!_data)
            _data = new Data;
        immutable len = _data.arr.length;
        immutable reqlen = len + nelems;

        if (_data.capacity >= reqlen)
            return;

        // need to increase capacity
        if (__ctfe)
        {
            static if (__traits(compiles, new Unqual!T[1]))
            {
                _data.arr.length = reqlen;
            }
            else
            {
                // avoid restriction of @disable this()
                _data.arr = _data.arr[0 .. _data.capacity];
                foreach (i; _data.capacity .. reqlen)
                    _data.arr ~= Unqual!T.init;
            }
            _data.arr = _data.arr[0 .. len];
            _data.capacity = reqlen;
        }
        else
        {
            // Time to reallocate.
            // We need to almost duplicate what's in druntime, except we
            // have better access to the capacity field.
            auto newlen = appenderNewCapacity!(T.sizeof)(_data.capacity, reqlen);
            // first, try extending the current block
            if (_data.canExtend)
            {
                immutable u = GC.extend(_data.arr.ptr, nelems * T.sizeof, (newlen - len) * T.sizeof);
                if (u)
                {
                    // extend worked, update the capacity
                    _data.capacity = u / T.sizeof;
                    return;
                }
            }


            // didn't work, must reallocate
            import core.checkedint : mulu;
            bool overflow;
            const nbytes = mulu(newlen, T.sizeof, overflow);
            if (overflow) assert(0);

            auto bi = GC.qalloc(nbytes, blockAttribute!T);
            _data.capacity = bi.size / T.sizeof;
            import core.stdc.string : memcpy;
            if (len)
                memcpy(bi.base, _data.arr.ptr, len * T.sizeof);
            _data.arr = (cast(Unqual!T*) bi.base)[0 .. len];
            _data.canExtend = true;
            // leave the old data, for safety reasons
        }
    }

    private template canPutItem(U)
    {
        enum bool canPutItem =
            isImplicitlyConvertible!(U, T) ||
            isSomeChar!T && isSomeChar!U;
    }
    private template canPutConstRange(Range)
    {
        enum bool canPutConstRange =
            isInputRange!(Unqual!Range) &&
            !isInputRange!Range &&
            is(typeof(Appender.init.put(Range.init.front)));
    }
    private template canPutRange(Range)
    {
        enum bool canPutRange =
            isInputRange!Range &&
            is(typeof(Appender.init.put(Range.init.front)));
    }

    /**
     * Appends `item` to the managed array.
     */
    void put(U)(U item) if (canPutItem!U)
    {
        static if (isSomeChar!T && isSomeChar!U && T.sizeof < U.sizeof)
        {
            /* may throwable operation:
             * - std.utf.encode
             */
            // must do some transcoding around here
            import std.utf : encode;
            Unqual!T[T.sizeof == 1 ? 4 : 2] encoded;
            auto len = encode(encoded, item);
            put(encoded[0 .. len]);
        }
        else
        {
            import std.conv : emplaceRef;

            ensureAddable(1);
            immutable len = _data.arr.length;

            auto bigData = (() @trusted => _data.arr.ptr[0 .. len + 1])();
            emplaceRef!(Unqual!T)(bigData[len], cast(Unqual!T) item);
            //We do this at the end, in case of exceptions
            _data.arr = bigData;
        }
    }

    // Const fixing hack.
    void put(Range)(Range items) if (canPutConstRange!Range)
    {
        alias p = put!(Unqual!Range);
        p(items);
    }

    /**
     * Appends an entire range to the managed array.
     */
    void put(Range)(Range items) if (canPutRange!Range)
    {
        // note, we disable this branch for appending one type of char to
        // another because we can't trust the length portion.
        static if (!(isSomeChar!T && isSomeChar!(ElementType!Range) &&
                     !is(immutable Range == immutable T[])) &&
                    is(typeof(items.length) == size_t))
        {
            // optimization -- if this type is something other than a string,
            // and we are adding exactly one element, call the version for one
            // element.
            static if (!isSomeChar!T)
            {
                if (items.length == 1)
                {
                    put(items.front);
                    return;
                }
            }

            // make sure we have enough space, then add the items
            @trusted auto bigDataFun(size_t extra)
            {
                ensureAddable(extra);
                return _data.arr.ptr[0 .. _data.arr.length + extra];
            }
            auto bigData = bigDataFun(items.length);

            immutable len = _data.arr.length;
            immutable newlen = bigData.length;

            alias UT = Unqual!T;

            static if (is(typeof(_data.arr[] = items[])) &&
                !hasElaborateAssign!UT && isAssignable!(UT, ElementEncodingType!Range))
            {
                bigData[len .. newlen] = items[];
            }
            else
            {
                import std.conv : emplaceRef;
                foreach (ref it ; bigData[len .. newlen])
                {
                    emplaceRef!T(it, items.front);
                    items.popFront();
                }
            }

            //We do this at the end, in case of exceptions
            _data.arr = bigData;
        }
        else
        {
            //pragma(msg, Range.stringof);
            // Generic input range
            for (; !items.empty; items.popFront())
            {
                put(items.front);
            }
        }
    }

    /**
     * Appends `rhs` to the managed array.
     * Params:
     * rhs = Element or range.
     */
    void opOpAssign(string op : "~", U)(U rhs)
    if (__traits(compiles, put(rhs)))
    {
        put(rhs);
    }

    // only allow overwriting data on non-immutable and non-const data
    static if (isMutable!T)
    {
        /**
         * Clears the managed array.  This allows the elements of the array to be reused
         * for appending.
         *
         * Note: clear is disabled for immutable or const element types, due to the
         * possibility that $(D Appender) might overwrite immutable data.
         */
        void clear() @trusted pure nothrow
        {
            if (_data)
            {
                _data.arr = _data.arr.ptr[0 .. 0];
            }
        }

        /**
         * Shrinks the managed array to the given length.
         *
         * Throws: $(D Exception) if newlength is greater than the current array length.
         * Note: shrinkTo is disabled for immutable or const element types.
         */
        void shrinkTo(size_t newlength) @trusted pure
        {
            import std.exception : enforce;
            if (_data)
            {
                enforce(newlength <= _data.arr.length, "Attempting to shrink Appender with newlength > length");
                _data.arr = _data.arr.ptr[0 .. newlength];
            }
            else
                enforce(newlength == 0, "Attempting to shrink empty Appender with non-zero newlength");
        }
    }

    void toString(Writer)(scope Writer w)
    {
        import std.format : formattedWrite;
        w.formattedWrite(typeof(this).stringof ~ "(%s)", data);
    }
}

///
@safe unittest
{
    auto app = appender!string();
    string b = "abcdefg";
    foreach (char c; b)
        app.put(c);
    assert(app[] == "abcdefg");

    int[] a = [ 1, 2 ];
    auto app2 = appender(a);
    app2.put(3);
    app2.put([ 4, 5, 6 ]);
    assert(app2[] == [ 1, 2, 3, 4, 5, 6 ]);
}

@safe unittest
{
    import std.format : format;
    auto app = appender!(int[])();
    app.put(1);
    app.put(2);
    app.put(3);
    assert("%s".format(app) == "Appender!(int[])(%s)".format([1,2,3]));
}

@safe unittest // issue 17251
{
    static struct R
    {
        int front() const { return 0; }
        bool empty() const { return true; }
        void popFront() {}
    }

    auto app = appender!(R[]);
    const(R)[1] r;
    app.put(r[0]);
    app.put(r[]);
}

//Calculates an efficient growth scheme based on the old capacity
//of data, and the minimum requested capacity.
//arg curLen: The current length
//arg reqLen: The length as requested by the user
//ret sugLen: A suggested growth.
private size_t appenderNewCapacity(size_t TSizeOf)(size_t curLen, size_t reqLen) @safe pure nothrow
{
    import core.bitop : bsr;
    import std.algorithm.comparison : max;
    if (curLen == 0)
        return max(reqLen,8);
    ulong mult = 100 + (1000UL) / (bsr(curLen * TSizeOf) + 1);
    // limit to doubling the length, we don't want to grow too much
    if (mult > 200)
        mult = 200;
    auto sugLen = cast(size_t)((curLen * mult + 99) / 100);
    return max(reqLen, sugLen);
}

/**
 * A version of $(LREF Appender) that can update an array in-place.
 * It forwards all calls to an underlying appender implementation.
 * Any calls made to the appender also update the pointer to the
 * original array passed in.
 *
 * Tip: Use the `arrayPtr` overload of $(LREF appender) for construction with type-inference.
 */
struct RefAppender(A)
if (isDynamicArray!A)
{
    private
    {
        Appender!A impl;
        A* arr;
    }

    /**
     * Constructs a `RefAppender` with a given array reference.  This does not copy the
     * data.  If the array has a larger capacity as determined by `arr.capacity`, it
     * will be used by the appender.
     *
     * Note: Do not use built-in appending (i.e. `~=`) on the original array
     * until you are done with the appender, because subsequent calls to the appender
     * will reallocate the array data without those appends.
     *
     * Params:
     * arr = Pointer to an array. Must not be _null.
     */
    this(A* arr)
    {
        impl = Appender!A(*arr);
        this.arr = arr;
    }

    /** Wraps remaining `Appender` methods such as $(LREF put).
     * Params:
     * fn = Method name to call.
     * args = Arguments to pass to the method.
     */
    void opDispatch(string fn, Args...)(Args args)
    if (__traits(compiles, (Appender!A a) => mixin("a." ~ fn ~ "(args)")))
    {
        // we do it this way because we can't cache a void return
        scope(exit) *this.arr = impl[];
        mixin("return impl." ~ fn ~ "(args);");
    }

    /**
     * Appends `rhs` to the managed array.
     * Params:
     * rhs = Element or range.
     */
    void opOpAssign(string op : "~", U)(U rhs)
    if (__traits(compiles, (Appender!A a){ a.put(rhs); }))
    {
        scope(exit) *this.arr = impl[];
        impl.put(rhs);
    }

    /**
     * Returns the capacity of the array (the maximum number of elements the
     * managed array can accommodate before triggering a reallocation).  If any
     * appending will reallocate, $(D capacity) returns $(D 0).
     */
    @property size_t capacity() const
    {
        return impl.capacity;
    }

    /* Use opSlice() instead.
     * Returns: the managed array.
     */
    @property inout(ElementEncodingType!A)[] data() inout
    {
        return impl[];
    }

    /**
     * Returns: the managed array.
     */
    @property inout(ElementEncodingType!A)[] opSlice() inout
    {
        return impl[];
    }
}

///
@system pure nothrow
unittest
{
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

/++
    Convenience function that returns an $(LREF Appender) instance,
    optionally initialized with $(D array).
 +/
Appender!A appender(A)()
if (isDynamicArray!A)
{
    return Appender!A(null);
}
/// ditto
Appender!(E[]) appender(A : E[], E)(auto ref A array)
{
    static assert(!isStaticArray!A || __traits(isRef, array),
        "Cannot create Appender from an rvalue static array");

    return Appender!(E[])(array);
}

@safe pure nothrow unittest
{
    import std.exception;
    {
        auto app = appender!(char[])();
        string b = "abcdefg";
        foreach (char c; b) app.put(c);
        assert(app[] == "abcdefg");
    }
    {
        auto app = appender!(char[])();
        string b = "abcdefg";
        foreach (char c; b) app ~= c;
        assert(app[] == "abcdefg");
    }
    {
        int[] a = [ 1, 2 ];
        auto app2 = appender(a);
        assert(app2[] == [ 1, 2 ]);
        app2.put(3);
        app2.put([ 4, 5, 6 ][]);
        assert(app2[] == [ 1, 2, 3, 4, 5, 6 ]);
        app2.put([ 7 ]);
        assert(app2[] == [ 1, 2, 3, 4, 5, 6, 7 ]);
    }

    int[] a = [ 1, 2 ];
    auto app2 = appender(a);
    assert(app2[] == [ 1, 2 ]);
    app2 ~= 3;
    app2 ~= [ 4, 5, 6 ][];
    assert(app2[] == [ 1, 2, 3, 4, 5, 6 ]);
    app2 ~= [ 7 ];
    assert(app2[] == [ 1, 2, 3, 4, 5, 6, 7 ]);

    app2.reserve(5);
    assert(app2.capacity >= 5);

    try // shrinkTo may throw
    {
        app2.shrinkTo(3);
    }
    catch (Exception) assert(0);
    assert(app2[] == [ 1, 2, 3 ]);
    assertThrown(app2.shrinkTo(5));

    const app3 = app2;
    assert(app3.capacity >= 3);
    assert(app3[] == [1, 2, 3]);

    auto app4 = appender([]);
    try // shrinkTo may throw
    {
        app4.shrinkTo(0);
    }
    catch (Exception) assert(0);

    // Issue 5663 & 9725 tests
    foreach (S; AliasSeq!(char[], const(char)[], string))
    {
        {
            Appender!S app5663i;
            assertNotThrown(app5663i.put("\xE3"));
            assert(app5663i[] == "\xE3");

            Appender!S app5663c;
            assertNotThrown(app5663c.put(cast(const(char)[])"\xE3"));
            assert(app5663c[] == "\xE3");

            Appender!S app5663m;
            assertNotThrown(app5663m.put("\xE3".dup));
            assert(app5663m[] == "\xE3");
        }
        // ditto for ~=
        {
            Appender!S app5663i;
            assertNotThrown(app5663i ~= "\xE3");
            assert(app5663i[] == "\xE3");

            Appender!S app5663c;
            assertNotThrown(app5663c ~= cast(const(char)[])"\xE3");
            assert(app5663c[] == "\xE3");

            Appender!S app5663m;
            assertNotThrown(app5663m ~= "\xE3".dup);
            assert(app5663m[] == "\xE3");
        }
    }

    static struct S10122
    {
        int val;

        @disable this();
        this(int v) @safe pure nothrow { val = v; }
    }
    assertCTFEable!(
    {
        auto w = appender!(S10122[])();
        w.put(S10122(1));
        assert(w[].length == 1 && w[][0].val == 1);
    });
}

///
@safe pure nothrow
unittest
{
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
    {
        auto w = appender!string();
        w.reserve(4);
        cast(void) w.capacity;
        cast(void) w[];
        try
        {
            wchar wc = 'a';
            dchar dc = 'a';
            w.put(wc);    // decoding may throw
            w.put(dc);    // decoding may throw
        }
        catch (Exception) assert(0);
    }
    {
        auto w = appender!(int[])();
        w.reserve(4);
        cast(void) w.capacity;
        cast(void) w[];
        w.put(10);
        w.put([10]);
        w.clear();
        try
        {
            w.shrinkTo(0);
        }
        catch (Exception) assert(0);

        struct N
        {
            int payload;
            alias payload this;
        }
        w.put(N(1));
        w.put([N(2)]);

        struct S(T)
        {
            @property bool empty() { return true; }
            @property T front() { return T.init; }
            void popFront() {}
        }
        S!int r;
        w.put(r);
    }
}

@safe unittest
{
    import std.algorithm;
    import std.typecons;
    //10690
    [tuple(1)].filter!(t => true).array; // No error
    [tuple("A")].filter!(t => true).array; // error
}

@system unittest
{
    import std.range;
    //Coverage for put(Range)
    struct S1
    {
    }
    struct S2
    {
        void opAssign(S2){}
    }
    auto a1 = Appender!(S1[])();
    auto a2 = Appender!(S2[])();
    auto au1 = Appender!(const(S1)[])();
    auto au2 = Appender!(const(S2)[])();
    a1.put(S1().repeat().take(10));
    a2.put(S2().repeat().take(10));
    auto sc1 = const(S1)();
    auto sc2 = const(S2)();
    au1.put(sc1.repeat().take(10));
    au2.put(sc2.repeat().take(10));
}

@system unittest
{
    struct S
    {
        int* p;
    }

    auto a0 = Appender!(S[])();
    auto a1 = Appender!(const(S)[])();
    auto a2 = Appender!(immutable(S)[])();
    auto s0 = S(null);
    auto s1 = const(S)(null);
    auto s2 = immutable(S)(null);
    a1.put(s0);
    a1.put(s1);
    a1.put(s2);
    a1.put([s0]);
    a1.put([s1]);
    a1.put([s2]);
    a0.put(s0);
    static assert(!is(typeof(a0.put(a1))));
    static assert(!is(typeof(a0.put(a2))));
    a0.put([s0]);
    static assert(!is(typeof(a0.put([a1]))));
    static assert(!is(typeof(a0.put([a2]))));
    static assert(!is(typeof(a2.put(a0))));
    static assert(!is(typeof(a2.put(a1))));
    a2.put(s2);
    static assert(!is(typeof(a2.put([a0]))));
    static assert(!is(typeof(a2.put([a1]))));
    a2.put([s2]);
}

@safe unittest
{
    //9528
    const(E)[] fastCopy(E)(E[] src) {
            auto app = appender!(const(E)[])();
            foreach (i, e; src)
                    app.put(e);
            return app[];
    }

    class C {}
    struct S { const(C) c; }
    S[] s = [ S(new C) ];

    auto t = fastCopy(s); // Does not compile
}

@safe unittest
{
    import std.algorithm.iteration : map;
    //10753
    struct Foo {
       immutable dchar d;
    }
    struct Bar {
       immutable int x;
    }
    "12".map!Foo.array;
    [1, 2].map!Bar.array;
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    //New appender signature tests
    alias mutARR = int[];
    alias conARR = const(int)[];
    alias immARR = immutable(int)[];

    mutARR mut;
    conARR con;
    immARR imm;

    auto app1 = Appender!mutARR(mut);                //Always worked. Should work. Should not create a warning.
    app1.put(7);
    assert(equal(app1[], [7]));
    static assert(!is(typeof(Appender!mutARR(con)))); //Never worked.  Should not work.
    static assert(!is(typeof(Appender!mutARR(imm)))); //Never worked.  Should not work.

    auto app2 = Appender!conARR(mut); //Always worked. Should work. Should not create a warning.
    app2.put(7);
    assert(equal(app2[], [7]));
    auto app3 = Appender!conARR(con); //Didn't work.   Now works.   Should not create a warning.
    app3.put(7);
    assert(equal(app3[], [7]));
    auto app4 = Appender!conARR(imm); //Didn't work.   Now works.   Should not create a warning.
    app4.put(7);
    assert(equal(app4[], [7]));

    //{auto app = Appender!immARR(mut);}                //Worked. Will cease to work. Creates warning.
    //static assert(!is(typeof(Appender!immARR(mut)))); //Worked. Will cease to work. Uncomment me after full deprecation.
    static assert(!is(typeof(Appender!immARR(con))));   //Never worked. Should not work.
    auto app5 = Appender!immARR(imm);                  //Didn't work.  Now works. Should not create a warning.
    app5.put(7);
    assert(equal(app5[], [7]));

    //Deprecated. Please uncomment and make sure this doesn't work:
    //char[] cc;
    //static assert(!is(typeof(Appender!string(cc))));

    //This should always work:
    auto app6 = appender!string(null);
    assert(app6[] == null);
    auto app7 = appender!(const(char)[])(null);
    assert(app7[] == null);
    auto app8 = appender!(char[])(null);
    assert(app8[] == null);
}

@safe unittest //Test large allocations (for GC.extend)
{
    import std.algorithm.comparison : equal;
    import std.range;
    Appender!(char[]) app;
    app.reserve(1); //cover reserve on non-initialized
    foreach (_; 0 .. 100_000)
        app.put('a');
    assert(equal(app[], 'a'.repeat(100_000)));
}

@safe unittest
{
    auto reference = new ubyte[](2048 + 1); //a number big enough to have a full page (EG: the GC extends)
    auto arr = reference.dup;
    auto app = appender(arr[0 .. 0]);
    app.reserve(1); //This should not trigger a call to extend
    app.put(ubyte(1)); //Don't clobber arr
    assert(reference[] == arr[]);
}

@safe unittest // clear method is supported only for mutable element types
{
    Appender!string app;
    app.put("foo");
    static assert(!__traits(compiles, app.clear()));
    assert(app[] == "foo");
}

@safe unittest
{
    static struct D//dynamic
    {
        int[] i;
        alias i this;
    }
    static struct S//static
    {
        int[5] i;
        alias i this;
    }
    static assert(!is(Appender!(char[5])));
    static assert(!is(Appender!D));
    static assert(!is(Appender!S));

    enum int[5] a = [];
    int[5] b;
    D d;
    S s;
    int[5] foo(){return a;}

    static assert(!is(typeof(appender(a))));
    static assert( is(typeof(appender(b))));
    static assert( is(typeof(appender(d))));
    static assert( is(typeof(appender(s))));
    static assert(!is(typeof(appender(foo()))));
}

@system unittest
{
    // Issue 13077
    static class A {}

    // reduced case
    auto w = appender!(shared(A)[])();
    w.put(new shared A());

    // original case
    import std.range;
    InputRange!(shared A) foo()
    {
        return [new shared A].inputRangeObject;
    }
    auto res = foo.array;
}

/++
    Convenience function that returns a $(LREF RefAppender) instance initialized
    with `arrayPtr`. Don't use null for the array pointer, use the other
    version of $(D appender) instead.
 +/
RefAppender!(E[]) appender(P : E[]*, E)(P arrayPtr)
{
    return RefAppender!(E[])(arrayPtr);
}

///
@system pure nothrow
unittest
{
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

@system unittest
{
    import std.exception;
    {
        auto arr = new char[0];
        auto app = appender(&arr);
        string b = "abcdefg";
        foreach (char c; b) app.put(c);
        assert(app[] == "abcdefg");
        assert(arr == "abcdefg");
    }
    {
        auto arr = new char[0];
        auto app = appender(&arr);
        string b = "abcdefg";
        foreach (char c; b) app ~= c;
        assert(app[] == "abcdefg");
        assert(arr == "abcdefg");
    }
    {
        int[] a = [ 1, 2 ];
        auto app2 = appender(&a);
        assert(app2[] == [ 1, 2 ]);
        assert(a == [ 1, 2 ]);
        app2.put(3);
        app2.put([ 4, 5, 6 ][]);
        assert(app2[] == [ 1, 2, 3, 4, 5, 6 ]);
        assert(a == [ 1, 2, 3, 4, 5, 6 ]);
    }

    int[] a = [ 1, 2 ];
    auto app2 = appender(&a);
    assert(app2[] == [ 1, 2 ]);
    assert(a == [ 1, 2 ]);
    app2 ~= 3;
    app2 ~= [ 4, 5, 6 ][];
    assert(app2[] == [ 1, 2, 3, 4, 5, 6 ]);
    assert(a == [ 1, 2, 3, 4, 5, 6 ]);

    app2.reserve(5);
    assert(app2.capacity >= 5);

    try // shrinkTo may throw
    {
        app2.shrinkTo(3);
    }
    catch (Exception) assert(0);
    assert(app2[] == [ 1, 2, 3 ]);
    assertThrown(app2.shrinkTo(5));

    const app3 = app2;
    assert(app3.capacity >= 3);
    assert(app3[] == [1, 2, 3]);
}

@safe unittest // issue 14605
{
    static assert(isOutputRange!(Appender!(int[]), int));
    static assert(isOutputRange!(RefAppender!(int[]), int));
}

@safe unittest
{
    Appender!(int[]) app;
    short[] range = [1, 2, 3];
    app.put(range);
    assert(app[] == [1, 2, 3]);
}

@safe unittest
{
    string s = "hello".idup;
    char[] a = "hello".dup;
    auto appS = appender(s);
    auto appA = appender(a);
    put(appS, 'w');
    put(appA, 'w');
    s ~= 'a'; //Clobbers here?
    a ~= 'a'; //Clobbers here?
    assert(appS[] == "hellow");
    assert(appA[] == "hellow");
}
