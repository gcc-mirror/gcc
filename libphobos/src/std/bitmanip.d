// Written in the D programming language.

/**
Bit-level manipulation facilities.

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Bit constructs) $(TD
    $(LREF BitArray)
    $(LREF bitfields)
    $(LREF bitsSet)
))
$(TR $(TD Endianness conversion) $(TD
    $(LREF bigEndianToNative)
    $(LREF littleEndianToNative)
    $(LREF nativeToBigEndian)
    $(LREF nativeToLittleEndian)
    $(LREF swapEndian)
))
$(TR $(TD Integral ranges) $(TD
    $(LREF append)
    $(LREF peek)
    $(LREF read)
    $(LREF write)
))
$(TR $(TD Floating-Point manipulation) $(TD
    $(LREF DoubleRep)
    $(LREF FloatRep)
))
$(TR $(TD Tagging) $(TD
    $(LREF taggedClassRef)
    $(LREF taggedPointer)
))
))

Copyright: Copyright The D Language Foundation 2007 - 2011.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright),
           $(HTTP erdani.org, Andrei Alexandrescu),
           $(HTTP jmdavisprog.com, Jonathan M Davis),
           Alex Rønne Petersen,
           Damian Ziemba,
           Amaury SECHET
Source: $(PHOBOSSRC std/bitmanip.d)
*/
/*
         Copyright The D Language Foundation 2007 - 2012.
Distributed under the Boost Software License, Version 1.0.
   (See accompanying file LICENSE_1_0.txt or copy at
         http://www.boost.org/LICENSE_1_0.txt)
*/
module std.bitmanip;

import std.range.primitives;
public import std.system : Endian;
import std.traits;

private string myToString(ulong n) pure @safe
{
    import core.internal.string : UnsignedStringBuf, unsignedToTempString;
    UnsignedStringBuf buf;
    auto s = unsignedToTempString(n, buf);
    // pure allows implicit cast to string
    return s ~ (n > uint.max ? "UL" : "U");
}

@safe pure unittest
{
    assert(myToString(5) == "5U");
    assert(myToString(uint.max) == "4294967295U");
    assert(myToString(uint.max + 1UL) == "4294967296UL");
}


private template createAccessors(
    string store, T, string name, size_t len, size_t offset)
{
    static if (!name.length)
    {
        // No need to create any accessor
        enum createAccessors = "";
    }
    else static if (len == 0)
    {
        // Fields of length 0 are always zero
        enum createAccessors = "enum "~T.stringof~" "~name~" = 0;\n";
    }
    else
    {
        enum ulong maskAllElse = ((~0uL) >> (64 - len)) << offset;
        enum TSize = 8 * T.sizeof;
        enum SignShift = TSize - len;

        static if (T.min < 0)
        {
            enum long minVal = -(1uL << (len - 1));
            enum ulong maxVal = (1uL << (len - 1)) - 1;
            enum RightShiftOp = ">>=";
        }
        else
        {
            enum ulong minVal = 0;
            enum ulong maxVal = (~0uL) >> (64 - len);
            enum RightShiftOp = ">>>=";
        }

        static if (is(T == bool))
        {
            enum createAccessors =
            // getter
                "@property bool " ~ name ~ "() @safe pure nothrow @nogc const { return "
                ~"("~store~" & "~myToString(maskAllElse)~") != 0;}\n"
            // setter
                ~"@property void " ~ name ~ "(bool v) @safe pure nothrow @nogc { "
                ~"if (v) "~store~" |= "~myToString(maskAllElse)~";"
                ~"else "~store~" &= cast(typeof("~store~"))(-1-cast(typeof("~store~"))"~myToString(maskAllElse)~");}\n";
        }
        else
        {
            // getter
            enum createAccessors = "@property "~T.stringof~" "~name~"() @safe pure nothrow @nogc const {"
                ~ "auto result = cast("~T.stringof~") (" ~ store ~ " >>" ~ myToString(offset) ~ ");"
                ~ "result <<= " ~ myToString(SignShift) ~ ";"
                ~ "result " ~ RightShiftOp ~ myToString(SignShift) ~ ";"
                ~ " return result;}\n"
            // setter
                ~"@property void "~name~"("~T.stringof~" v) @safe pure nothrow @nogc { "
                ~"assert(v >= "~name~`_min, "Value is smaller than the minimum value of bitfield '`~name~`'"); `
                ~"assert(v <= "~name~`_max, "Value is greater than the maximum value of bitfield '`~name~`'"); `
                ~store~" = cast(typeof("~store~"))"
                ~" (("~store~" & (-1-cast(typeof("~store~"))"~myToString(maskAllElse)~"))"
                ~" | ((cast(typeof("~store~")) v << "~myToString(offset)~")"
                ~" & "~myToString(maskAllElse)~"));}\n"
            // constants
                ~"enum "~T.stringof~" "~name~"_min = cast("~T.stringof~")"
                ~myToString(minVal)~"; "
                ~" enum "~T.stringof~" "~name~"_max = cast("~T.stringof~")"
                ~myToString(maxVal)~"; ";
        }
    }
}

private template createStoreName(Ts...)
{
    static if (Ts.length < 2)
        enum createStoreName = "_bf";
    else
        enum createStoreName = "_" ~ Ts[1] ~ createStoreName!(Ts[3 .. $]);
}

private template createStorageAndFields(Ts...)
{
    enum Name = createStoreName!Ts;
    enum Size = sizeOfBitField!Ts;
    static if (Size == ubyte.sizeof * 8)
        alias StoreType = ubyte;
    else static if (Size == ushort.sizeof * 8)
        alias StoreType = ushort;
    else static if (Size == uint.sizeof * 8)
        alias StoreType = uint;
    else static if (Size == ulong.sizeof * 8)
        alias StoreType = ulong;
    else
    {
        import std.conv : to;
        static assert(false, "Field widths must sum to 8, 16, 32, or 64, not " ~ to!string(Size));
        alias StoreType = ulong; // just to avoid another error msg
    }

    enum createStorageAndFields
        = "private " ~ StoreType.stringof ~ " " ~ Name ~ ";"
        ~ createFields!(Name, 0, Ts);
}

private template createFields(string store, size_t offset, Ts...)
{
    static if (Ts.length > 0)
        enum createFields
            = createAccessors!(store, Ts[0], Ts[1], Ts[2], offset)
            ~ createFields!(store, offset + Ts[2], Ts[3 .. $]);
    else
        enum createFields = "";
}

private ulong getBitsForAlign(ulong a)
{
    ulong bits = 0;
    while ((a & 0x01) == 0)
    {
        bits++;
        a >>= 1;
    }

    assert(a == 1, "alignment is not a power of 2");
    return bits;
}

private template createReferenceAccessor(string store, T, ulong bits, string name)
{
    enum storage = "private void* " ~ store ~ "_ptr;\n";
    enum storage_accessor = "@property ref size_t " ~ store ~ "() return @trusted pure nothrow @nogc const { "
        ~ "return *cast(size_t*) &" ~ store ~ "_ptr;}\n"
        ~ "@property void " ~ store ~ "(size_t v) @trusted pure nothrow @nogc { "
        ~ "" ~ store ~ "_ptr = cast(void*) v;}\n";

    enum mask = (1UL << bits) - 1;
    // getter
    enum ref_accessor = "@property "~T.stringof~" "~name~"() @trusted pure nothrow @nogc const { auto result = "
        ~ "("~store~" & "~myToString(~mask)~"); "
        ~ "return cast("~T.stringof~") cast(void*) result;}\n"
    // setter
        ~"@property void "~name~"("~T.stringof~" v) @trusted pure nothrow @nogc { "
        ~"assert(((cast(typeof("~store~")) cast(void*) v) & "~myToString(mask)
        ~`) == 0, "Value not properly aligned for '`~name~`'"); `
        ~store~" = cast(typeof("~store~"))"
        ~" (("~store~" & (cast(typeof("~store~")) "~myToString(mask)~"))"
        ~" | ((cast(typeof("~store~")) cast(void*) v) & (cast(typeof("~store~")) "~myToString(~mask)~")));}\n";

    enum createReferenceAccessor = storage ~ storage_accessor ~ ref_accessor;
}

private template sizeOfBitField(T...)
{
    static if (T.length < 2)
        enum sizeOfBitField = 0;
    else
        enum sizeOfBitField = T[2] + sizeOfBitField!(T[3 .. $]);
}

private template createTaggedReference(T, ulong a, string name, Ts...)
{
    static assert(
        sizeOfBitField!Ts <= getBitsForAlign(a),
        "Fields must fit in the bits know to be zero because of alignment."
    );
    enum StoreName = createStoreName!(T, name, 0, Ts);
    enum createTaggedReference
        = createReferenceAccessor!(StoreName, T, sizeOfBitField!Ts, name)
        ~ createFields!(StoreName, 0, Ts, size_t, "", T.sizeof * 8 - sizeOfBitField!Ts);
}

/**
Allows creating `bitfields` inside `structs`, `classes` and `unions`.

A `bitfield` consists of one or more entries with a fixed number of
bits reserved for each of the entries. The types of the entries can
be `bool`s, integral types or enumerated types, arbitrarily mixed.
The most efficient type to store in `bitfields` is `bool`, followed
by unsigned types, followed by signed types.

Each non-`bool` entry of the `bitfield` will be represented by the
number of bits specified by the user. The minimum and the maximum
numbers that represent this domain can be queried by using the name
of the variable followed by `_min` or `_max`.

Limitation: The number of bits in a `bitfield` is limited to 8, 16,
32 or 64. If padding is needed, an entry should be explicitly
allocated with an empty name.

Implementation_details: `Bitfields` are internally stored in an
`ubyte`, `ushort`, `uint` or `ulong` depending on the number of bits
used. The bits are filled in the order given by the parameters,
starting with the lowest significant bit. The name of the (private)
variable used for saving the `bitfield` is created by concatenating
all of the variable names, each preceded by an underscore, and
a suffix `_bf`.

Params: T = A list of template parameters divided into chunks of 3
            items. Each chunk consists (in this order) of a type, a
            name and a number. Together they define an entry
            of the `bitfield`: a variable of the given type and name,
            which can hold as many bits as the number denotes.

Returns: A string that can be used in a `mixin` to add the `bitfield`.

See_Also: $(REF BitFlags, std,typecons)
*/
string bitfields(T...)()
{
    static assert(T.length % 3 == 0,
                  "Wrong number of arguments (" ~ T.length.stringof ~ "): Must be a multiple of 3");

    static foreach (i, ARG; T)
    {
        static if (i % 3 == 0)
            static assert(is (typeof({ARG tmp = cast (ARG)0; if (ARG.min < 0) {} }())),
                          "Integral type or `bool` expected, found " ~ ARG.stringof);

        // would be nice to check for valid variable names too
        static if (i % 3 == 1)
            static assert(is (typeof(ARG) : string),
                          "Variable name expected, found " ~ ARG.stringof);

        static if (i % 3 == 2)
        {
            static assert(is (typeof({ulong tmp = ARG;}())),
                          "Integral value expected, found " ~ ARG.stringof);

            static if (T[i-1] != "")
            {
                static assert(!is (T[i-2] : bool) || ARG <= 1,
                              "Type `bool` is only allowed for single-bit fields");

                static assert(ARG >= 0 && ARG <= T[i-2].sizeof * 8,
                              "Wrong size of bitfield: " ~ ARG.stringof);
            }
        }
    }

    return createStorageAndFields!T;
}

/**
Create a `bitfield` pack of eight bits, which fit in
one `ubyte`. The `bitfields` are allocated starting from the
least significant bit, i.e. `x` occupies the two least significant bits
of the `bitfields` storage.
*/
@safe unittest
{
    struct A
    {
        int a;
        mixin(bitfields!(
            uint, "x",    2,
            int,  "y",    3,
            uint, "z",    2,
            bool, "flag", 1));
    }

    A obj;
    obj.x = 2;
    obj.z = obj.x;

    assert(obj.x == 2);
    assert(obj.y == 0);
    assert(obj.z == 2);
    assert(obj.flag == false);
}

/**
The sum of all bit lengths in one `bitfield` instantiation
must be exactly 8, 16, 32, or 64. If padding is needed, just allocate
one bitfield with an empty name.
*/
@safe unittest
{
    struct A
    {
        mixin(bitfields!(
            bool, "flag1",    1,
            bool, "flag2",    1,
            uint, "",         6));
    }

    A a;
    assert(a.flag1 == 0);
    a.flag1 = 1;
    assert(a.flag1 == 1);
    a.flag1 = 0;
    assert(a.flag1 == 0);
}

/// enums can be used too
@safe unittest
{
    enum ABC { A, B, C }
    struct EnumTest
    {
        mixin(bitfields!(
                  ABC, "x", 2,
                  bool, "y", 1,
                  ubyte, "z", 5));
    }
}

@safe pure nothrow @nogc
unittest
{
    // Degenerate bitfields tests mixed with range tests
    // https://issues.dlang.org/show_bug.cgi?id=8474
    // https://issues.dlang.org/show_bug.cgi?id=11160
    struct Test1
    {
        mixin(bitfields!(uint, "a", 32,
                        uint, "b", 4,
                        uint, "c", 4,
                        uint, "d", 8,
                        uint, "e", 16,));

        static assert(Test1.b_min == 0);
        static assert(Test1.b_max == 15);
    }

    struct Test2
    {
        mixin(bitfields!(bool, "a", 0,
                        ulong, "b", 64));

        static assert(Test2.b_min == ulong.min);
        static assert(Test2.b_max == ulong.max);
    }

    struct Test1b
    {
        mixin(bitfields!(bool, "a", 0,
                        int, "b", 8));
    }

    struct Test2b
    {
        mixin(bitfields!(int, "a", 32,
                        int, "b", 4,
                        int, "c", 4,
                        int, "d", 8,
                        int, "e", 16,));

        static assert(Test2b.b_min == -8);
        static assert(Test2b.b_max == 7);
    }

    struct Test3b
    {
        mixin(bitfields!(bool, "a", 0,
                        long, "b", 64));

        static assert(Test3b.b_min == long.min);
        static assert(Test3b.b_max == long.max);
    }

    struct Test4b
    {
        mixin(bitfields!(long, "a", 32,
                        int, "b", 32));
    }

    // Sign extension tests
    Test2b t2b;
    Test4b t4b;
    t2b.b = -5; assert(t2b.b == -5);
    t2b.d = -5; assert(t2b.d == -5);
    t2b.e = -5; assert(t2b.e == -5);
    t4b.a = -5; assert(t4b.a == -5L);
}

// https://issues.dlang.org/show_bug.cgi?id=6686
@safe unittest
{
    union  S {
        ulong bits = ulong.max;
        mixin (bitfields!(
            ulong, "back",  31,
            ulong, "front", 33)
        );
    }
    S num;

    num.bits = ulong.max;
    num.back = 1;
    assert(num.bits == 0xFFFF_FFFF_8000_0001uL);
}

// https://issues.dlang.org/show_bug.cgi?id=5942
@safe unittest
{
    struct S
    {
        mixin(bitfields!(
            int, "a" , 32,
            int, "b" , 32
        ));
    }

    S data;
    data.b = 42;
    data.a = 1;
    assert(data.b == 42);
}

@safe unittest
{
    struct Test
    {
        mixin(bitfields!(bool, "a", 1,
                         uint, "b", 3,
                         short, "c", 4));
    }

    @safe void test() pure nothrow
    {
        Test t;

        t.a = true;
        t.b = 5;
        t.c = 2;

        assert(t.a);
        assert(t.b == 5);
        assert(t.c == 2);
    }

    test();
}

@safe unittest
{
    {
        static struct Integrals {
            bool checkExpectations(bool eb, int ei, short es) { return b == eb && i == ei && s == es; }

            mixin(bitfields!(
                      bool, "b", 1,
                      uint, "i", 3,
                      short, "s", 4));
        }
        Integrals i;
        assert(i.checkExpectations(false, 0, 0));
        i.b = true;
        assert(i.checkExpectations(true, 0, 0));
        i.i = 7;
        assert(i.checkExpectations(true, 7, 0));
        i.s = -8;
        assert(i.checkExpectations(true, 7, -8));
        i.s = 7;
        assert(i.checkExpectations(true, 7, 7));
    }

    //https://issues.dlang.org/show_bug.cgi?id=8876
    {
        struct MoreIntegrals {
            bool checkExpectations(uint eu, ushort es, uint ei) { return u == eu && s == es && i == ei; }

            mixin(bitfields!(
                  uint, "u", 24,
                  short, "s", 16,
                  int, "i", 24));
        }

        MoreIntegrals i;
        assert(i.checkExpectations(0, 0, 0));
        i.s = 20;
        assert(i.checkExpectations(0, 20, 0));
        i.i = 72;
        assert(i.checkExpectations(0, 20, 72));
        i.u = 8;
        assert(i.checkExpectations(8, 20, 72));
        i.s = 7;
        assert(i.checkExpectations(8, 7, 72));
    }

    enum A { True, False }
    enum B { One, Two, Three, Four }
    static struct Enums {
        bool checkExpectations(A ea, B eb) { return a == ea && b == eb; }

        mixin(bitfields!(
                  A, "a", 1,
                  B, "b", 2,
                  uint, "", 5));
    }
    Enums e;
    assert(e.checkExpectations(A.True, B.One));
    e.a = A.False;
    assert(e.checkExpectations(A.False, B.One));
    e.b = B.Three;
    assert(e.checkExpectations(A.False, B.Three));

    static struct SingleMember {
        bool checkExpectations(bool eb) { return b == eb; }

        mixin(bitfields!(
                  bool, "b", 1,
                  uint, "", 7));
    }
    SingleMember f;
    assert(f.checkExpectations(false));
    f.b = true;
    assert(f.checkExpectations(true));
}

// https://issues.dlang.org/show_bug.cgi?id=12477
@system unittest
{
    import core.exception : AssertError;
    import std.algorithm.searching : canFind;

    static struct S
    {
        mixin(bitfields!(
            uint, "a", 6,
            int, "b", 2));
    }

    S s;

    try { s.a = uint.max; assert(0); }
    catch (AssertError ae)
    { assert(ae.msg.canFind("Value is greater than the maximum value of bitfield 'a'"), ae.msg); }

    try { s.b = int.min;  assert(0); }
    catch (AssertError ae)
    { assert(ae.msg.canFind("Value is smaller than the minimum value of bitfield 'b'"), ae.msg); }
}

// https://issues.dlang.org/show_bug.cgi?id=15305
@safe unittest
{
    struct S {
            mixin(bitfields!(
                    bool, "alice", 1,
                    ulong, "bob", 63,
            ));
    }

    S s;
    s.bob = long.max - 1;
    s.alice = false;
    assert(s.bob == long.max - 1);
}

// https://issues.dlang.org/show_bug.cgi?id=21634
@safe unittest
{
    struct A
    {
        mixin(bitfields!(int, "", 1,
                         int, "gshared", 7));
    }
}

// https://issues.dlang.org/show_bug.cgi?id=21725
@safe unittest
{
    struct S
    {
        mixin(bitfields!(
            uint, q{foo}, 4,
            uint, null, 4,
        ));
    }
}

/**
This string mixin generator allows one to create tagged pointers inside $(D_PARAM struct)s and $(D_PARAM class)es.

A tagged pointer uses the bits known to be zero in a normal pointer or class reference to store extra information.
For example, a pointer to an integer must be 4-byte aligned, so there are 2 bits that are always known to be zero.
One can store a 2-bit integer there.

The example above creates a tagged pointer in the struct A. The pointer is of type
`uint*` as specified by the first argument, and is named x, as specified by the second
argument.

Following arguments works the same way as `bitfield`'s. The bitfield must fit into the
bits known to be zero because of the pointer alignment.
*/

template taggedPointer(T : T*, string name, Ts...) {
    enum taggedPointer = createTaggedReference!(T*, T.alignof, name, Ts);
}

///
@safe unittest
{
    struct A
    {
        int a;
        mixin(taggedPointer!(
            uint*, "x",
            bool, "b1", 1,
            bool, "b2", 1));
    }
    A obj;
    obj.x = new uint;
    obj.b1 = true;
    obj.b2 = false;
}

@system unittest
{
    struct Test5
    {
        mixin(taggedPointer!(
            int*, "a",
            uint, "b", 2));
    }

    Test5 t5;
    t5.a = null;
    t5.b = 3;
    assert(t5.a is null);
    assert(t5.b == 3);

    int myint = 42;
    t5.a = &myint;
    assert(t5.a is &myint);
    assert(t5.b == 3);
}

/**
This string mixin generator allows one to create tagged class reference inside $(D_PARAM struct)s and $(D_PARAM class)es.

A tagged class reference uses the bits known to be zero in a normal class reference to store extra information.
For example, a pointer to an integer must be 4-byte aligned, so there are 2 bits that are always known to be zero.
One can store a 2-bit integer there.

The example above creates a tagged reference to an Object in the struct A. This expects the same parameters
as `taggedPointer`, except the first argument which must be a class type instead of a pointer type.
*/

template taggedClassRef(T, string name, Ts...)
if (is(T == class))
{
    enum taggedClassRef = createTaggedReference!(T, 8, name, Ts);
}

///
@safe unittest
{
    struct A
    {
        int a;
        mixin(taggedClassRef!(
            Object, "o",
            uint, "i", 2));
    }
    A obj;
    obj.o = new Object();
    obj.i = 3;
}

@system unittest
{
    struct Test6
    {
        mixin(taggedClassRef!(
            Object, "o",
            bool, "b", 1));
    }

    Test6 t6;
    t6.o = null;
    t6.b = false;
    assert(t6.o is null);
    assert(t6.b == false);

    auto o = new Object();
    t6.o = o;
    t6.b = true;
    assert(t6.o is o);
    assert(t6.b == true);
}

@safe unittest
{
    static assert(!__traits(compiles,
        taggedPointer!(
            int*, "a",
            uint, "b", 3)));

    static assert(!__traits(compiles,
        taggedClassRef!(
            Object, "a",
            uint, "b", 4)));

    struct S {
        mixin(taggedClassRef!(
            Object, "a",
            bool, "b", 1));
    }

    const S s;
    void bar(S s) {}

    static assert(!__traits(compiles, bar(s)));
}

private struct FloatingPointRepresentation(T)
{
    static if (is(T == float))
    {
        enum uint bias = 127, fractionBits = 23, exponentBits = 8, signBits = 1;
        alias FractionType = uint;
        alias ExponentType = ubyte;
    }
    else
    {
        enum uint bias = 1023, fractionBits = 52, exponentBits = 11, signBits = 1;
        alias FractionType = ulong;
        alias ExponentType = ushort;
    }

    union
    {
        T value;
        mixin(bitfields!(
                  FractionType, "fraction", fractionBits,
                  ExponentType, "exponent", exponentBits,
                  bool,  "sign",     signBits));
    }
}

/**
   Allows manipulating the fraction, exponent, and sign parts of a
   `float` separately. The definition is:

----
struct FloatRep
{
    union
    {
        float value;
        mixin(bitfields!(
                  uint,  "fraction", 23,
                  ubyte, "exponent",  8,
                  bool,  "sign",      1));
    }
    enum uint bias = 127, fractionBits = 23, exponentBits = 8, signBits = 1;
}
----
*/
alias FloatRep = FloatingPointRepresentation!float;

///
@safe unittest
{
    FloatRep rep = {value: 0};
    assert(rep.fraction == 0);
    assert(rep.exponent == 0);
    assert(!rep.sign);

    rep.value = 42;
    assert(rep.fraction == 2621440);
    assert(rep.exponent == 132);
    assert(!rep.sign);

    rep.value = 10;
    assert(rep.fraction == 2097152);
    assert(rep.exponent == 130);
}

///
@safe unittest
{
    FloatRep rep = {value: 1};
    assert(rep.fraction == 0);
    assert(rep.exponent == 127);
    assert(!rep.sign);

    rep.exponent = 126;
    assert(rep.value == 0.5);

    rep.exponent = 130;
    assert(rep.value == 8);
}

///
@safe unittest
{
    FloatRep rep = {value: 1};
    rep.value = -0.5;
    assert(rep.fraction == 0);
    assert(rep.exponent == 126);
    assert(rep.sign);

    rep.value = -1. / 3;
    assert(rep.fraction == 2796203);
    assert(rep.exponent == 125);
    assert(rep.sign);
}

/**
   Allows manipulating the fraction, exponent, and sign parts of a
   `double` separately. The definition is:

----
struct DoubleRep
{
    union
    {
        double value;
        mixin(bitfields!(
                  ulong,   "fraction", 52,
                  ushort,  "exponent", 11,
                  bool,    "sign",      1));
    }
    enum uint bias = 1023, signBits = 1, fractionBits = 52, exponentBits = 11;
}
----
*/
alias DoubleRep = FloatingPointRepresentation!double;

///
@safe unittest
{
    DoubleRep rep = {value: 0};
    assert(rep.fraction == 0);
    assert(rep.exponent == 0);
    assert(!rep.sign);

    rep.value = 42;
    assert(rep.fraction == 1407374883553280);
    assert(rep.exponent == 1028);
    assert(!rep.sign);

    rep.value = 10;
    assert(rep.fraction == 1125899906842624);
    assert(rep.exponent == 1026);
}

///
@safe unittest
{
    DoubleRep rep = {value: 1};
    assert(rep.fraction == 0);
    assert(rep.exponent == 1023);
    assert(!rep.sign);

    rep.exponent = 1022;
    assert(rep.value == 0.5);

    rep.exponent = 1026;
    assert(rep.value == 8);
}

///
@safe unittest
{
    DoubleRep rep = {value: 1};
    rep.value = -0.5;
    assert(rep.fraction == 0);
    assert(rep.exponent == 1022);
    assert(rep.sign);

    rep.value = -1. / 3;
    assert(rep.fraction == 1501199875790165);
    assert(rep.exponent == 1021);
    assert(rep.sign);
}

/// Reading
@safe unittest
{
    DoubleRep x;
    x.value = 1.0;
    assert(x.fraction == 0 && x.exponent == 1023 && !x.sign);
    x.value = -0.5;
    assert(x.fraction == 0 && x.exponent == 1022 && x.sign);
    x.value = 0.5;
    assert(x.fraction == 0 && x.exponent == 1022 && !x.sign);
}

/// Writing
@safe unittest
{
    DoubleRep x;
    x.fraction = 1125899906842624;
    x.exponent = 1025;
    x.sign = true;
    assert(x.value == -5.0);
}

/**
A dynamic array of bits. Each bit in a `BitArray` can be manipulated individually
or by the standard bitwise operators `&`, `|`, `^`, `~`, `>>`, `<<` and also by
other effective member functions; most of them work relative to the `BitArray`'s
dimension (see $(LREF dim)), instead of its $(LREF length).
*/
struct BitArray
{
private:

    import core.bitop : btc, bts, btr, bsf, bt;
    import std.format.spec : FormatSpec;

    size_t _len;
    size_t* _ptr;
    enum bitsPerSizeT = size_t.sizeof * 8;

    @property size_t fullWords() const scope @safe @nogc pure nothrow
    {
        return _len / bitsPerSizeT;
    }
    // Number of bits after the last full word
    @property size_t endBits() const scope @safe @nogc pure nothrow
    {
        return _len % bitsPerSizeT;
    }
    // Bit mask to extract the bits after the last full word
    @property size_t endMask() const scope @safe @nogc pure nothrow
    {
        return (size_t(1) << endBits) - 1;
    }
    static size_t lenToDim(size_t len) @nogc pure nothrow @safe
    {
        return (len + (bitsPerSizeT-1)) / bitsPerSizeT;
    }

public:
    /**
    Creates a `BitArray` from a `bool` array, such that `bool` values read
    from left to right correspond to subsequent bits in the `BitArray`.

    Params: ba = Source array of `bool` values.
    */
    this(in bool[] ba) nothrow pure
    {
        length = ba.length;
        foreach (i, b; ba)
        {
            this[i] = b;
        }
    }

    ///
    @system unittest
    {
        import std.algorithm.comparison : equal;

        bool[] input = [true, false, false, true, true];
        auto a = BitArray(input);
        assert(a.length == 5);
        assert(a.bitsSet.equal([0, 3, 4]));

        // This also works because an implicit cast to bool[] occurs for this array.
        auto b = BitArray([0, 0, 1]);
        assert(b.length == 3);
        assert(b.bitsSet.equal([2]));
    }

    ///
    @system unittest
    {
        import std.algorithm.comparison : equal;
        import std.array : array;
        import std.range : iota, repeat;

        BitArray a = true.repeat(70).array;
        assert(a.length == 70);
        assert(a.bitsSet.equal(iota(0, 70)));
    }

    /**
    Creates a `BitArray` from the raw contents of the source array. The
    source array is not copied but simply acts as the underlying array
    of bits, which stores data as `size_t` units.

    That means a particular care should be taken when passing an array
    of a type different than `size_t`, firstly because its length should
    be a multiple of `size_t.sizeof`, and secondly because how the bits
    are mapped:
    ---
    size_t[] source = [1, 2, 3, 3424234, 724398, 230947, 389492];
    enum sbits = size_t.sizeof * 8;
    auto ba = BitArray(source, source.length * sbits);
    foreach (n; 0 .. source.length * sbits)
    {
        auto nth_bit = cast(bool) (source[n / sbits] & (1L << (n % sbits)));
        assert(ba[n] == nth_bit);
    }
    ---
    The least significant bit in any `size_t` unit is the starting bit of this
    unit, and the most significant bit is the last bit of this unit. Therefore,
    passing e.g. an array of `int`s may result in a different `BitArray`
    depending on the processor's endianness.

    This constructor is the inverse of $(LREF opCast).

    Params:
        v = Source array. `v.length` must be a multple of `size_t.sizeof`.
        numbits = Number of bits to be mapped from the source array, i.e.
                  length of the created `BitArray`.
    */
    this(void[] v, size_t numbits) @nogc nothrow pure
    in
    {
        assert(numbits <= v.length * 8,
                "numbits must be less than or equal to v.length * 8");
        assert(v.length % size_t.sizeof == 0,
                "v.length must be a multiple of the size of size_t");
    }
    do
    {
        _ptr = cast(size_t*) v.ptr;
        _len = numbits;
    }

    ///
    @system unittest
    {
        import std.algorithm.comparison : equal;

        auto a = BitArray([1, 0, 0, 1, 1]);

        // Inverse of the cast.
        auto v = cast(void[]) a;
        auto b = BitArray(v, a.length);

        assert(b.length == 5);
        assert(b.bitsSet.equal([0, 3, 4]));

        // a and b share the underlying data.
        a[0] = 0;
        assert(b[0] == 0);
        assert(a == b);
    }

    ///
    @system unittest
    {
        import std.algorithm.comparison : equal;

        size_t[] source = [0b1100, 0b0011];
        enum sbits = size_t.sizeof * 8;
        auto ba = BitArray(source, source.length * sbits);
        // The least significant bit in each unit is this unit's starting bit.
        assert(ba.bitsSet.equal([2, 3, sbits, sbits + 1]));
    }

    ///
    @system unittest
    {
        // Example from the doc for this constructor.
        static immutable size_t[] sourceData = [1, 0b101, 3, 3424234, 724398, 230947, 389492];
        size_t[] source = sourceData.dup;
        enum sbits = size_t.sizeof * 8;
        auto ba = BitArray(source, source.length * sbits);
        foreach (n; 0 .. source.length * sbits)
        {
            auto nth_bit = cast(bool) (source[n / sbits] & (1L << (n % sbits)));
            assert(ba[n] == nth_bit);
        }

        // Example of mapping only part of the array.
        import std.algorithm.comparison : equal;

        auto bc = BitArray(source, sbits + 1);
        assert(bc.bitsSet.equal([0, sbits]));
        // Source array has not been modified.
        assert(source == sourceData);
    }

    // Deliberately undocumented: raw initialization of bit array.
    this(size_t len, size_t* ptr) @nogc nothrow pure
    {
        _len = len;
        _ptr = ptr;
    }

    /**
    Returns: Dimension i.e. the number of native words backing this `BitArray`.

    Technically, this is the length of the underlying array storing bits, which
    is equal to `ceil(length / (size_t.sizeof * 8))`, as bits are packed into
    `size_t` units.
    */
    @property size_t dim() const @nogc nothrow pure @safe
    {
        return lenToDim(_len);
    }

    /**
    Returns: Number of bits in the `BitArray`.
    */
    @property size_t length() const @nogc nothrow pure @safe
    {
        return _len;
    }

    /**********************************************
     * Sets the amount of bits in the `BitArray`.
     * $(RED Warning: increasing length may overwrite bits in
     * the final word of the current underlying data regardless
     * of whether it is shared between BitArray objects. i.e. D
     * dynamic array extension semantics are not followed.)
     */
    @property size_t length(size_t newlen) pure nothrow @system
    {
        if (newlen != _len)
        {
            size_t olddim = dim;
            immutable newdim = lenToDim(newlen);

            if (newdim != olddim)
            {
                // Create a fake array so we can use D's realloc machinery
                auto b = _ptr[0 .. olddim];
                b.length = newdim;                // realloc
                _ptr = b.ptr;
            }

            auto oldlen = _len;
            _len = newlen;
            if (oldlen < newlen)
            {
                auto end = ((oldlen / bitsPerSizeT) + 1) * bitsPerSizeT;
                if (end > newlen)
                    end = newlen;
                this[oldlen .. end] = 0;
            }
        }
        return _len;
    }

    // https://issues.dlang.org/show_bug.cgi?id=20240
    @system unittest
    {
        BitArray ba;

        ba.length = 1;
        ba[0] = 1;
        ba.length = 0;
        ba.length = 1;
        assert(ba[0] == 0); // OK

        ba.length = 2;
        ba[1] = 1;
        ba.length = 1;
        ba.length = 2;
        assert(ba[1] == 0); // Fail
    }

    /**********************************************
     * Gets the `i`'th bit in the `BitArray`.
     */
    bool opIndex(size_t i) const @nogc pure nothrow
    in
    {
        assert(i < _len, "i must be less than the length");
    }
    do
    {
        return cast(bool) bt(_ptr, i);
    }

    ///
    @system unittest
    {
        static void fun(const BitArray arr)
        {
            auto x = arr[0];
            assert(x == 1);
        }
        BitArray a;
        a.length = 3;
        a[0] = 1;
        fun(a);
    }

    /**********************************************
     * Sets the `i`'th bit in the `BitArray`.
     */
    bool opIndexAssign(bool b, size_t i) @nogc pure nothrow
    in
    {
        assert(i < _len, "i must be less than the length");
    }
    do
    {
        if (b)
            bts(_ptr, i);
        else
            btr(_ptr, i);
        return b;
    }

    /**
      Sets all the values in the `BitArray` to the
      value specified by `val`.
     */
    void opSliceAssign(bool val) @nogc pure nothrow
    {
        _ptr[0 .. fullWords] = val ? ~size_t(0) : 0;
        if (endBits)
        {
            if (val)
                _ptr[fullWords] |= endMask;
            else
                _ptr[fullWords] &= ~endMask;
        }
    }

    ///
    @system pure nothrow unittest
    {
        import std.algorithm.comparison : equal;

        auto b = BitArray([1, 0, 1, 0, 1, 1]);

        b[] = true;
        // all bits are set
        assert(b.bitsSet.equal([0, 1, 2, 3, 4, 5]));

        b[] = false;
        // none of the bits are set
        assert(b.bitsSet.empty);
    }

    /**
      Sets the bits of a slice of `BitArray` starting
      at index `start` and ends at index ($D end - 1)
      with the values specified by `val`.
     */
    void opSliceAssign(bool val, size_t start, size_t end) @nogc pure nothrow
    in
    {
        assert(start <= end, "start must be less or equal to end");
        assert(end <= length, "end must be less or equal to the length");
    }
    do
    {
        size_t startBlock = start / bitsPerSizeT;
        size_t endBlock = end / bitsPerSizeT;
        size_t startOffset = start % bitsPerSizeT;
        size_t endOffset = end % bitsPerSizeT;

        if (startBlock == endBlock)
        {
            size_t startBlockMask = ~((size_t(1) << startOffset) - 1);
            size_t endBlockMask = (size_t(1) << endOffset) - 1;
            size_t joinMask = startBlockMask & endBlockMask;
            if (val)
                _ptr[startBlock] |= joinMask;
            else
                _ptr[startBlock] &= ~joinMask;
            return;
        }

        if (startOffset != 0)
        {
            size_t startBlockMask = ~((size_t(1) << startOffset) - 1);
            if (val)
                _ptr[startBlock] |= startBlockMask;
            else
                _ptr[startBlock] &= ~startBlockMask;
            ++startBlock;
        }
        if (endOffset != 0)
        {
            size_t endBlockMask = (size_t(1) << endOffset) - 1;
            if (val)
                _ptr[endBlock] |= endBlockMask;
            else
                _ptr[endBlock] &= ~endBlockMask;
        }
        _ptr[startBlock .. endBlock] = size_t(0) - size_t(val);
    }

    ///
    @system pure nothrow unittest
    {
        import std.algorithm.comparison : equal;
        import std.range : iota;
        import std.stdio;

        auto b = BitArray([1, 0, 0, 0, 1, 1, 0]);
        b[1 .. 3] = true;
        assert(b.bitsSet.equal([0, 1, 2, 4, 5]));

        bool[72] bitArray;
        auto b1 = BitArray(bitArray);
        b1[63 .. 67] = true;
        assert(b1.bitsSet.equal([63, 64, 65, 66]));
        b1[63 .. 67] = false;
        assert(b1.bitsSet.empty);
        b1[0 .. 64] = true;
        assert(b1.bitsSet.equal(iota(0, 64)));
        b1[0 .. 64] = false;
        assert(b1.bitsSet.empty);

        bool[256] bitArray2;
        auto b2 = BitArray(bitArray2);
        b2[3 .. 245] = true;
        assert(b2.bitsSet.equal(iota(3, 245)));
        b2[3 .. 245] = false;
        assert(b2.bitsSet.empty);
    }

    /**
      Flips all the bits in the `BitArray`
     */
    void flip() @nogc pure nothrow
    {
        foreach (i; 0 .. fullWords)
            _ptr[i] = ~_ptr[i];

        if (endBits)
            _ptr[fullWords] = (~_ptr[fullWords]) & endMask;
    }

    ///
    @system pure nothrow unittest
    {
        import std.algorithm.comparison : equal;
        import std.range : iota;

        // positions 0, 2, 4 are set
        auto b = BitArray([1, 0, 1, 0, 1, 0]);
        b.flip();
        // after flipping, positions 1, 3, 5 are set
        assert(b.bitsSet.equal([1, 3, 5]));

        bool[270] bits;
        auto b1 = BitArray(bits);
        b1.flip();
        assert(b1.bitsSet.equal(iota(0, 270)));
    }

    /**
      Flips a single bit, specified by `pos`
     */
    void flip(size_t pos) @nogc pure nothrow
    {
        bt(_ptr, pos) ? btr(_ptr, pos) : bts(_ptr, pos);
    }

    ///
    @system pure nothrow unittest
    {
        auto ax = BitArray([1, 0, 0, 1]);
        ax.flip(0);
        assert(ax[0] == 0);

        bool[200] y;
        y[90 .. 130] = true;
        auto ay = BitArray(y);
        ay.flip(100);
        assert(ay[100] == 0);
    }

    /**********************************************
     * Counts all the set bits in the `BitArray`
     */
    size_t count() const scope @safe @nogc pure nothrow
    {
        if (_ptr)
        {
            size_t bitCount;
            foreach (i; 0 .. fullWords)
                bitCount += (() @trusted => countBitsSet(_ptr[i]))();
            if (endBits)
                bitCount += (() @trusted => countBitsSet(_ptr[fullWords] & endMask))();
            return bitCount;
        }
        else
        {
            return 0;
        }
    }

    ///
    @system pure nothrow unittest
    {
        auto a = BitArray([0, 1, 1, 0, 0, 1, 1]);
        assert(a.count == 4);

        BitArray b;
        assert(b.count == 0);

        bool[200] boolArray;
        boolArray[45 .. 130] = true;
        auto c = BitArray(boolArray);
        assert(c.count == 85);
    }

    /**********************************************
     * Duplicates the `BitArray` and its contents.
     */
    @property BitArray dup() const pure nothrow
    {
        BitArray ba;

        auto b = _ptr[0 .. dim].dup;
        ba._len = _len;
        ba._ptr = b.ptr;
        return ba;
    }

    ///
    @system unittest
    {
        BitArray a;
        BitArray b;

        a.length = 3;
        a[0] = 1; a[1] = 0; a[2] = 1;
        b = a.dup;
        assert(b.length == 3);
        foreach (i; 0 .. 3)
            assert(b[i] == (((i ^ 1) & 1) ? true : false));
    }

    /**********************************************
     * Support for `foreach` loops for `BitArray`.
     */
    int opApply(scope int delegate(ref bool) dg)
    {
        int result;

        foreach (i; 0 .. _len)
        {
            bool b = opIndex(i);
            result = dg(b);
            this[i] = b;
            if (result)
                break;
        }
        return result;
    }

    /** ditto */
    int opApply(scope int delegate(bool) dg) const
    {
        int result;

        foreach (i; 0 .. _len)
        {
            immutable b = opIndex(i);
            result = dg(b);
            if (result)
                break;
        }
        return result;
    }

    /** ditto */
    int opApply(scope int delegate(size_t, ref bool) dg)
    {
        int result;

        foreach (i; 0 .. _len)
        {
            bool b = opIndex(i);
            result = dg(i, b);
            this[i] = b;
            if (result)
                break;
        }
        return result;
    }

    /** ditto */
    int opApply(scope int delegate(size_t, bool) dg) const
    {
        int result;

        foreach (i; 0 .. _len)
        {
            immutable b = opIndex(i);
            result = dg(i, b);
            if (result)
                break;
        }
        return result;
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1];

        auto a = BitArray(ba);

        int i;
        foreach (b;a)
        {
            switch (i)
            {
                case 0: assert(b == true); break;
                case 1: assert(b == false); break;
                case 2: assert(b == true); break;
                default: assert(0);
            }
            i++;
        }

        foreach (j,b;a)
        {
            switch (j)
            {
                case 0: assert(b == true); break;
                case 1: assert(b == false); break;
                case 2: assert(b == true); break;
                default: assert(0);
            }
        }
    }


    /**********************************************
     * Reverses the bits of the `BitArray`.
     */
    @property BitArray reverse() @nogc pure nothrow return
    out (result)
    {
        assert(result == this, "the result must be equal to this");
    }
    do
    {
        if (_len >= 2)
        {
            bool t;
            size_t lo, hi;

            lo = 0;
            hi = _len - 1;
            for (; lo < hi; lo++, hi--)
            {
                t = this[lo];
                this[lo] = this[hi];
                this[hi] = t;
            }
        }
        return this;
    }

    ///
    @system unittest
    {
        BitArray b;
        bool[5] data = [1,0,1,1,0];

        b = BitArray(data);
        b.reverse;
        foreach (i; 0 .. data.length)
            assert(b[i] == data[4 - i]);
    }


    /**********************************************
     * Sorts the `BitArray`'s elements.
     */
    @property BitArray sort() @nogc pure nothrow return
    out (result)
    {
        assert(result == this, "the result must be equal to this");
    }
    do
    {
        if (_len >= 2)
        {
            size_t lo, hi;

            lo = 0;
            hi = _len - 1;
            while (1)
            {
                while (1)
                {
                    if (lo >= hi)
                        goto Ldone;
                    if (this[lo] == true)
                        break;
                    lo++;
                }

                while (1)
                {
                    if (lo >= hi)
                        goto Ldone;
                    if (this[hi] == false)
                        break;
                    hi--;
                }

                this[lo] = false;
                this[hi] = true;

                lo++;
                hi--;
            }
        }
    Ldone:
        return this;
    }

    ///
    @system unittest
    {
        size_t x = 0b1100011000;
        auto ba = BitArray(10, &x);
        ba.sort;
        foreach (i; 0 .. 6)
            assert(ba[i] == false);
        foreach (i; 6 .. 10)
            assert(ba[i] == true);
    }


    /***************************************
     * Support for operators == and != for `BitArray`.
     */
    bool opEquals(const ref BitArray a2) const @nogc pure nothrow
    {
        if (this.length != a2.length)
            return false;
        auto p1 = this._ptr;
        auto p2 = a2._ptr;

        if (p1[0 .. fullWords] != p2[0 .. fullWords])
            return false;

        if (!endBits)
            return true;

        auto i = fullWords;
        return (p1[i] & endMask) == (p2[i] & endMask);
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1];
        bool[] bc = [1,0,1,0,1,0,1];
        bool[] bd = [1,0,1,1,1];
        bool[] be = [1,0,1,0,1];
        bool[] bf = [1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
        bool[] bg = [1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1];

        auto a = BitArray(ba);
        auto b = BitArray(bb);
        auto c = BitArray(bc);
        auto d = BitArray(bd);
        auto e = BitArray(be);
        auto f = BitArray(bf);
        auto g = BitArray(bg);

        assert(a != b);
        assert(a != c);
        assert(a != d);
        assert(a == e);
        assert(f != g);
    }

    /***************************************
     * Supports comparison operators for `BitArray`.
     */
    int opCmp(BitArray a2) const @nogc pure nothrow
    {
        const lesser = this.length < a2.length ? &this : &a2;
        immutable fullWords = lesser.fullWords;
        immutable endBits = lesser.endBits;
        auto p1 = this._ptr;
        auto p2 = a2._ptr;

        foreach (i; 0 .. fullWords)
        {
            if (p1[i] != p2[i])
            {
                return p1[i] & (size_t(1) << bsf(p1[i] ^ p2[i])) ? 1 : -1;
            }
        }

        if (endBits)
        {
            immutable i = fullWords;
            immutable diff = p1[i] ^ p2[i];
            if (diff)
            {
                immutable index = bsf(diff);
                if (index < endBits)
                {
                    return p1[i] & (size_t(1) << index) ? 1 : -1;
                }
            }
        }

        // Standard:
        // A bool value can be implicitly converted to any integral type,
        // with false becoming 0 and true becoming 1
        return (this.length > a2.length) - (this.length < a2.length);
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1];
        bool[] bc = [1,0,1,0,1,0,1];
        bool[] bd = [1,0,1,1,1];
        bool[] be = [1,0,1,0,1];
        bool[] bf = [1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1];
        bool[] bg = [1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);
        auto c = BitArray(bc);
        auto d = BitArray(bd);
        auto e = BitArray(be);
        auto f = BitArray(bf);
        auto g = BitArray(bg);

        assert(a >  b);
        assert(a >= b);
        assert(a <  c);
        assert(a <= c);
        assert(a <  d);
        assert(a <= d);
        assert(a == e);
        assert(a <= e);
        assert(a >= e);
        assert(f <  g);
        assert(g <= g);
    }

    @system unittest
    {
        bool[] v;
        foreach  (i; 1 .. 256)
        {
            v.length = i;
            v[] = false;
            auto x = BitArray(v);
            v[i-1] = true;
            auto y = BitArray(v);
            assert(x < y);
            assert(x <= y);
        }

        BitArray a1, a2;

        for (size_t len = 4; len <= 256; len <<= 1)
        {
            a1.length = a2.length = len;
            a1[len-2] = a2[len-1] = true;
            assert(a1 > a2);
            a1[len-2] = a2[len-1] = false;
        }

        foreach (j; 1 .. a1.length)
        {
            a1[j-1] = a2[j] = true;
            assert(a1 > a2);
            a1[j-1] = a2[j] = false;
        }
    }

    /***************************************
     * Support for hashing for `BitArray`.
     */
    size_t toHash() const @nogc pure nothrow
    {
        size_t hash = 3557;
        auto fullBytes = _len / 8;
        foreach (i; 0 .. fullBytes)
        {
            hash *= 3559;
            hash += (cast(byte*) this._ptr)[i];
        }
        foreach (i; 8*fullBytes .. _len)
        {
            hash *= 3571;
            hash += this[i];
        }
        return hash;
    }

    /***************************************
     * Convert to `void[]`.
     */
    inout(void)[] opCast(T : const void[])() inout @nogc pure nothrow
    {
        return cast(inout void[]) _ptr[0 .. dim];
    }

    /***************************************
     * Convert to `size_t[]`.
     */
    inout(size_t)[] opCast(T : const size_t[])() inout @nogc pure nothrow
    {
        return _ptr[0 .. dim];
    }

    ///
    @system unittest
    {
        import std.array : array;
        import std.range : repeat, take;

        // bit array with 300 elements
        auto a = BitArray(true.repeat.take(300).array);
        size_t[] v = cast(size_t[]) a;
        const blockSize = size_t.sizeof * 8;
        assert(v.length == (a.length + blockSize - 1) / blockSize);
    }

    // https://issues.dlang.org/show_bug.cgi?id=20606
    @system unittest
    {
        import std.meta : AliasSeq;

        static foreach (alias T; AliasSeq!(void, size_t))
        {{
            BitArray m;
            T[] ma = cast(T[]) m;

            const BitArray c;
            const(T)[] ca = cast(const T[]) c;

            immutable BitArray i;
            immutable(T)[] ia = cast(immutable T[]) i;

            // Cross-mutability
            ca = cast(const T[]) m;
            ca = cast(const T[]) i;

            // Invalid cast don't compile
            static assert(!is(typeof(cast(T[]) c)));
            static assert(!is(typeof(cast(T[]) i)));
            static assert(!is(typeof(cast(immutable T[]) m)));
            static assert(!is(typeof(cast(immutable T[]) c)));
        }}
    }

    /***************************************
     * Support for unary operator ~ for `BitArray`.
     */
    BitArray opUnary(string op)() const pure nothrow
        if (op == "~")
    {
        auto dim = this.dim;

        BitArray result;
        result.length = _len;

        result._ptr[0 .. dim] = ~this._ptr[0 .. dim];

        // Avoid putting garbage in extra bits
        // Remove once we zero on length extension
        if (endBits)
            result._ptr[dim - 1] &= endMask;

        return result;
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1];

        auto a = BitArray(ba);
        BitArray b = ~a;

        assert(b[0] == 0);
        assert(b[1] == 1);
        assert(b[2] == 0);
        assert(b[3] == 1);
        assert(b[4] == 0);
    }


    /***************************************
     * Support for binary bitwise operators for `BitArray`.
     */
    BitArray opBinary(string op)(const BitArray e2) const pure nothrow
        if (op == "-" || op == "&" || op == "|" || op == "^")
    in
    {
        assert(e2.length == _len, "e2 must have the same length as this");
    }
    do
    {
        auto dim = this.dim;

        BitArray result;
        result.length = _len;

        static if (op == "-")
            result._ptr[0 .. dim] = this._ptr[0 .. dim] & ~e2._ptr[0 .. dim];
        else
            mixin("result._ptr[0 .. dim] = this._ptr[0 .. dim]"~op~" e2._ptr[0 .. dim];");

        // Avoid putting garbage in extra bits
        // Remove once we zero on length extension
        if (endBits)
            result._ptr[dim - 1] &= endMask;

        return result;
    }

    ///
    @system unittest
    {
        static bool[] ba = [1,0,1,0,1];
        static bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        BitArray c = a & b;

        assert(c[0] == 1);
        assert(c[1] == 0);
        assert(c[2] == 1);
        assert(c[3] == 0);
        assert(c[4] == 0);
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        BitArray c = a | b;

        assert(c[0] == 1);
        assert(c[1] == 0);
        assert(c[2] == 1);
        assert(c[3] == 1);
        assert(c[4] == 1);
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        BitArray c = a ^ b;

        assert(c[0] == 0);
        assert(c[1] == 0);
        assert(c[2] == 0);
        assert(c[3] == 1);
        assert(c[4] == 1);
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        BitArray c = a - b;

        assert(c[0] == 0);
        assert(c[1] == 0);
        assert(c[2] == 0);
        assert(c[3] == 0);
        assert(c[4] == 1);
    }


    /***************************************
     * Support for operator op= for `BitArray`.
     */
    BitArray opOpAssign(string op)(const BitArray e2) @nogc pure nothrow return scope
        if (op == "-" || op == "&" || op == "|" || op == "^")
    in
    {
        assert(e2.length == _len, "e2 must have the same length as this");
    }
    do
    {
        foreach (i; 0 .. fullWords)
        {
            static if (op == "-")
                _ptr[i] &= ~e2._ptr[i];
            else
                mixin("_ptr[i] "~op~"= e2._ptr[i];");
        }
        if (!endBits)
            return this;

        size_t i = fullWords;
        size_t endWord = _ptr[i];
        static if (op == "-")
            endWord &= ~e2._ptr[i];
        else
            mixin("endWord "~op~"= e2._ptr[i];");
        _ptr[i] = (_ptr[i] & ~endMask) | (endWord & endMask);

        return this;
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1,1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];
        auto a = BitArray(ba);
        auto b = BitArray(bb);
        BitArray c = a;
        c.length = 5;
        c &= b;
        assert(a[5] == 1);
        assert(a[6] == 0);
        assert(a[7] == 1);
        assert(a[8] == 0);
        assert(a[9] == 1);
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        a &= b;
        assert(a[0] == 1);
        assert(a[1] == 0);
        assert(a[2] == 1);
        assert(a[3] == 0);
        assert(a[4] == 0);
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        a |= b;
        assert(a[0] == 1);
        assert(a[1] == 0);
        assert(a[2] == 1);
        assert(a[3] == 1);
        assert(a[4] == 1);
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        a ^= b;
        assert(a[0] == 0);
        assert(a[1] == 0);
        assert(a[2] == 0);
        assert(a[3] == 1);
        assert(a[4] == 1);
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        a -= b;
        assert(a[0] == 0);
        assert(a[1] == 0);
        assert(a[2] == 0);
        assert(a[3] == 0);
        assert(a[4] == 1);
    }

    /***************************************
     * Support for operator ~= for `BitArray`.
     * $(RED Warning: This will overwrite a bit in the final word
     * of the current underlying data regardless of whether it is
     * shared between BitArray objects. i.e. D dynamic array
     * concatenation semantics are not followed)
     */
    BitArray opOpAssign(string op)(bool b) pure nothrow return scope
        if (op == "~")
    {
        length = _len + 1;
        this[_len - 1] = b;
        return this;
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0,1,0,1];

        auto a = BitArray(ba);
        BitArray b;

        b = (a ~= true);
        assert(a[0] == 1);
        assert(a[1] == 0);
        assert(a[2] == 1);
        assert(a[3] == 0);
        assert(a[4] == 1);
        assert(a[5] == 1);

        assert(b == a);
    }

    /***************************************
     * ditto
     */
    BitArray opOpAssign(string op)(BitArray b) pure nothrow return scope
        if (op == "~")
    {
        auto istart = _len;
        length = _len + b.length;
        for (auto i = istart; i < _len; i++)
            this[i] = b[i - istart];
        return this;
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0];
        bool[] bb = [0,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);
        BitArray c;

        c = (a ~= b);
        assert(a.length == 5);
        assert(a[0] == 1);
        assert(a[1] == 0);
        assert(a[2] == 0);
        assert(a[3] == 1);
        assert(a[4] == 0);

        assert(c == a);
    }

    /***************************************
     * Support for binary operator ~ for `BitArray`.
     */
    BitArray opBinary(string op)(bool b) const pure nothrow
        if (op == "~")
    {
        BitArray r;

        r = this.dup;
        r.length = _len + 1;
        r[_len] = b;
        return r;
    }

    /** ditto */
    BitArray opBinaryRight(string op)(bool b) const pure nothrow
        if (op == "~")
    {
        BitArray r;

        r.length = _len + 1;
        r[0] = b;
        foreach (i; 0 .. _len)
            r[1 + i] = this[i];
        return r;
    }

    /** ditto */
    BitArray opBinary(string op)(BitArray b) const pure nothrow
        if (op == "~")
    {
        BitArray r;

        r = this.dup;
        r ~= b;
        return r;
    }

    ///
    @system unittest
    {
        bool[] ba = [1,0];
        bool[] bb = [0,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);
        BitArray c;

        c = (a ~ b);
        assert(c.length == 5);
        assert(c[0] == 1);
        assert(c[1] == 0);
        assert(c[2] == 0);
        assert(c[3] == 1);
        assert(c[4] == 0);

        c = (a ~ true);
        assert(c.length == 3);
        assert(c[0] == 1);
        assert(c[1] == 0);
        assert(c[2] == 1);

        c = (false ~ a);
        assert(c.length == 3);
        assert(c[0] == 0);
        assert(c[1] == 1);
        assert(c[2] == 0);
    }

    // Rolls double word (upper, lower) to the right by n bits and returns the
    // lower word of the result.
    private static size_t rollRight()(size_t upper, size_t lower, size_t nbits)
        pure @safe nothrow @nogc
    in
    {
        assert(nbits < bitsPerSizeT, "nbits must be less than bitsPerSizeT");
    }
    do
    {
        if (nbits == 0)
            return lower;
        return (upper << (bitsPerSizeT - nbits)) | (lower >> nbits);
    }

    @safe unittest
    {
        static if (size_t.sizeof == 8)
        {
            size_t x = 0x12345678_90ABCDEF;
            size_t y = 0xFEDBCA09_87654321;

            assert(rollRight(x, y, 32) == 0x90ABCDEF_FEDBCA09);
            assert(rollRight(y, x, 4) == 0x11234567_890ABCDE);
        }
        else static if (size_t.sizeof == 4)
        {
            size_t x = 0x12345678;
            size_t y = 0x90ABCDEF;

            assert(rollRight(x, y, 16) == 0x567890AB);
            assert(rollRight(y, x, 4) == 0xF1234567);
        }
        else
            static assert(0, "Unsupported size_t width");
    }

    // Rolls double word (upper, lower) to the left by n bits and returns the
    // upper word of the result.
    private static size_t rollLeft()(size_t upper, size_t lower, size_t nbits)
        pure @safe nothrow @nogc
    in
    {
        assert(nbits < bitsPerSizeT, "nbits must be less than bitsPerSizeT");
    }
    do
    {
        if (nbits == 0)
            return upper;
        return (upper << nbits) | (lower >> (bitsPerSizeT - nbits));
    }

    @safe unittest
    {
        static if (size_t.sizeof == 8)
        {
            size_t x = 0x12345678_90ABCDEF;
            size_t y = 0xFEDBCA09_87654321;

            assert(rollLeft(x, y, 32) == 0x90ABCDEF_FEDBCA09);
            assert(rollLeft(y, x, 4) == 0xEDBCA098_76543211);
        }
        else static if (size_t.sizeof == 4)
        {
            size_t x = 0x12345678;
            size_t y = 0x90ABCDEF;

            assert(rollLeft(x, y, 16) == 0x567890AB);
            assert(rollLeft(y, x, 4) == 0x0ABCDEF1);
        }
    }

    /**
     * Operator `<<=` support.
     *
     * Shifts all the bits in the array to the left by the given number of
     * bits.  The leftmost bits are dropped, and 0's are appended to the end
     * to fill up the vacant bits.
     *
     * $(RED Warning: unused bits in the final word up to the next word
     * boundary may be overwritten by this operation. It does not attempt to
     * preserve bits past the end of the array.)
     */
    void opOpAssign(string op)(size_t nbits) @nogc pure nothrow
        if (op == "<<")
    {
        size_t wordsToShift = nbits / bitsPerSizeT;
        size_t bitsToShift = nbits % bitsPerSizeT;

        if (wordsToShift < dim)
        {
            foreach_reverse (i; 1 .. dim - wordsToShift)
            {
                _ptr[i + wordsToShift] = rollLeft(_ptr[i], _ptr[i-1],
                                                 bitsToShift);
            }
            _ptr[wordsToShift] = rollLeft(_ptr[0], 0, bitsToShift);
        }

        import std.algorithm.comparison : min;
        foreach (i; 0 .. min(wordsToShift, dim))
        {
            _ptr[i] = 0;
        }
    }

    /**
     * Operator `>>=` support.
     *
     * Shifts all the bits in the array to the right by the given number of
     * bits.  The rightmost bits are dropped, and 0's are inserted at the back
     * to fill up the vacant bits.
     *
     * $(RED Warning: unused bits in the final word up to the next word
     * boundary may be overwritten by this operation. It does not attempt to
     * preserve bits past the end of the array.)
     */
    void opOpAssign(string op)(size_t nbits) @nogc pure nothrow
        if (op == ">>")
    {
        size_t wordsToShift = nbits / bitsPerSizeT;
        size_t bitsToShift = nbits % bitsPerSizeT;

        if (wordsToShift + 1 < dim)
        {
            foreach (i; 0 .. dim - wordsToShift - 1)
            {
                _ptr[i] = rollRight(_ptr[i + wordsToShift + 1],
                                   _ptr[i + wordsToShift], bitsToShift);
            }
        }

        // The last word needs some care, as it must shift in 0's from past the
        // end of the array.
        if (wordsToShift < dim)
        {
            if (bitsToShift == 0)
                _ptr[dim - wordsToShift - 1] = _ptr[dim - 1];
            else
            {
                // Special case: if endBits == 0, then also endMask == 0.
                size_t lastWord = (endBits ? (_ptr[fullWords] & endMask) : _ptr[fullWords - 1]);
                _ptr[dim - wordsToShift - 1] = rollRight(0, lastWord, bitsToShift);
            }
        }

        import std.algorithm.comparison : min;
        foreach (i; 0 .. min(wordsToShift, dim))
        {
            _ptr[dim - i - 1] = 0;
        }
    }

    // https://issues.dlang.org/show_bug.cgi?id=17467
    @system unittest
    {
        import std.algorithm.comparison : equal;
        import std.range : iota;

        bool[] buf = new bool[64*3];
        buf[0 .. 64] = true;
        BitArray b = BitArray(buf);
        assert(equal(b.bitsSet, iota(0, 64)));
        b <<= 64;
        assert(equal(b.bitsSet, iota(64, 128)));

        buf = new bool[64*3];
        buf[64*2 .. 64*3] = true;
        b = BitArray(buf);
        assert(equal(b.bitsSet, iota(64*2, 64*3)));
        b >>= 64;
        assert(equal(b.bitsSet, iota(64, 128)));
    }

    // https://issues.dlang.org/show_bug.cgi?id=18134
    // shifting right when length is a multiple of 8 * size_t.sizeof.
    @system unittest
    {
        import std.algorithm.comparison : equal;
        import std.array : array;
        import std.range : repeat, iota;

        immutable r = size_t.sizeof * 8;

        BitArray a = true.repeat(r / 2).array;
        a >>= 0;
        assert(a.bitsSet.equal(iota(0, r / 2)));
        a >>= 1;
        assert(a.bitsSet.equal(iota(0, r / 2 - 1)));

        BitArray b = true.repeat(r).array;
        b >>= 0;
        assert(b.bitsSet.equal(iota(0, r)));
        b >>= 1;
        assert(b.bitsSet.equal(iota(0, r - 1)));

        BitArray c = true.repeat(2 * r).array;
        c >>= 0;
        assert(c.bitsSet.equal(iota(0, 2 * r)));
        c >>= 10;
        assert(c.bitsSet.equal(iota(0, 2 * r - 10)));
    }

    ///
    @system unittest
    {
        import std.format : format;

        auto b = BitArray([1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1]);

        b <<= 1;
        assert(format("%b", b) == "01100_10101101");

        b >>= 1;
        assert(format("%b", b) == "11001_01011010");

        b <<= 4;
        assert(format("%b", b) == "00001_10010101");

        b >>= 5;
        assert(format("%b", b) == "10010_10100000");

        b <<= 13;
        assert(format("%b", b) == "00000_00000000");

        b = BitArray([1, 0, 1, 1, 0, 1, 1, 1]);
        b >>= 8;
        assert(format("%b", b) == "00000000");

    }

    // Test multi-word case
    @system unittest
    {
        import std.format : format;

        // This has to be long enough to occupy more than one size_t. On 64-bit
        // machines, this would be at least 64 bits.
        auto b = BitArray([
            1, 0, 0, 0, 0, 0, 0, 0,  1, 1, 0, 0, 0, 0, 0, 0,
            1, 1, 1, 0, 0, 0, 0, 0,  1, 1, 1, 1, 0, 0, 0, 0,
            1, 1, 1, 1, 1, 0, 0, 0,  1, 1, 1, 1, 1, 1, 0, 0,
            1, 1, 1, 1, 1, 1, 1, 0,  1, 1, 1, 1, 1, 1, 1, 1,
            1, 0, 1, 0, 1, 0, 1, 0,  0, 1, 0, 1, 0, 1, 0, 1,
        ]);
        b <<= 8;
        assert(format("%b", b) ==
               "00000000_10000000_"~
               "11000000_11100000_"~
               "11110000_11111000_"~
               "11111100_11111110_"~
               "11111111_10101010");

        // Test right shift of more than one size_t's worth of bits
        b <<= 68;
        assert(format("%b", b) ==
               "00000000_00000000_"~
               "00000000_00000000_"~
               "00000000_00000000_"~
               "00000000_00000000_"~
               "00000000_00001000");

        b = BitArray([
            1, 0, 0, 0, 0, 0, 0, 0,  1, 1, 0, 0, 0, 0, 0, 0,
            1, 1, 1, 0, 0, 0, 0, 0,  1, 1, 1, 1, 0, 0, 0, 0,
            1, 1, 1, 1, 1, 0, 0, 0,  1, 1, 1, 1, 1, 1, 0, 0,
            1, 1, 1, 1, 1, 1, 1, 0,  1, 1, 1, 1, 1, 1, 1, 1,
            1, 0, 1, 0, 1, 0, 1, 0,  0, 1, 0, 1, 0, 1, 0, 1,
        ]);
        b >>= 8;
        assert(format("%b", b) ==
               "11000000_11100000_"~
               "11110000_11111000_"~
               "11111100_11111110_"~
               "11111111_10101010_"~
               "01010101_00000000");

        // Test left shift of more than 1 size_t's worth of bits
        b >>= 68;
        assert(format("%b", b) ==
               "01010000_00000000_"~
               "00000000_00000000_"~
               "00000000_00000000_"~
               "00000000_00000000_"~
               "00000000_00000000");
    }

    /***************************************
     * Return a string representation of this BitArray.
     *
     * Two format specifiers are supported:
     * $(LI $(B %s) which prints the bits as an array, and)
     * $(LI $(B %b) which prints the bits as 8-bit byte packets)
     * separated with an underscore.
     *
     * Params:
     *     sink = A `char` accepting
     *     $(REF_ALTTEXT output range, isOutputRange, std, range, primitives).
     *     fmt = A $(REF FormatSpec, std,format) which controls how the data
     *     is displayed.
     */
    void toString(W)(ref W sink, scope const ref FormatSpec!char fmt) const
    if (isOutputRange!(W, char))
    {
        const spec = fmt.spec;
        switch (spec)
        {
            case 'b':
                return formatBitString(sink);
            case 's':
                return formatBitArray(sink);
            default:
                throw new Exception("Unknown format specifier: %" ~ spec);
        }
    }

    ///
    @system pure unittest
    {
        import std.format : format;

        auto b = BitArray([0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);

        auto s1 = format("%s", b);
        assert(s1 == "[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]");

        auto s2 = format("%b", b);
        assert(s2 == "00001111_00001111");
    }

    /***************************************
     * Return a lazy range of the indices of set bits.
     */
    @property auto bitsSet() const nothrow
    {
        import std.algorithm.iteration : filter, map, joiner;
        import std.range : iota, chain;

        return chain(
            iota(fullWords)
                .filter!(i => _ptr[i])()
                .map!(i => BitsSet!size_t(_ptr[i], i * bitsPerSizeT))()
                .joiner(),
            iota(fullWords * bitsPerSizeT, _len)
                .filter!(i => this[i])()
        );
    }

    ///
    @system unittest
    {
        import std.algorithm.comparison : equal;

        auto b1 = BitArray([0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
        assert(b1.bitsSet.equal([4, 5, 6, 7, 12, 13, 14, 15]));

        BitArray b2;
        b2.length = 1000;
        b2[333] = true;
        b2[666] = true;
        b2[999] = true;
        assert(b2.bitsSet.equal([333, 666, 999]));
    }

    @system unittest
    {
        import std.algorithm.comparison : equal;
        import std.range : iota;

        BitArray b;
        enum wordBits = size_t.sizeof * 8;
        b = BitArray([size_t.max], 0);
        assert(b.bitsSet.empty);
        b = BitArray([size_t.max], 1);
        assert(b.bitsSet.equal([0]));
        b = BitArray([size_t.max], wordBits);
        assert(b.bitsSet.equal(iota(wordBits)));
        b = BitArray([size_t.max, size_t.max], wordBits);
        assert(b.bitsSet.equal(iota(wordBits)));
        b = BitArray([size_t.max, size_t.max], wordBits + 1);
        assert(b.bitsSet.equal(iota(wordBits + 1)));
        b = BitArray([size_t.max, size_t.max], wordBits * 2);
        assert(b.bitsSet.equal(iota(wordBits * 2)));
    }

    // https://issues.dlang.org/show_bug.cgi?id=20241
    @system unittest
    {
        BitArray ba;
        ba.length = 2;
        ba[1] = 1;
        ba.length = 1;
        assert(ba.bitsSet.empty);
    }

    private void formatBitString(Writer)(auto ref Writer sink) const
    {
        if (!length)
            return;

        auto leftover = _len % 8;
        foreach (idx; 0 .. leftover)
        {
            put(sink, cast(char)(this[idx] + '0'));
        }

        if (leftover && _len > 8)
            put(sink, "_");

        size_t count;
        foreach (idx; leftover .. _len)
        {
            put(sink, cast(char)(this[idx] + '0'));
            if (++count == 8 && idx != _len - 1)
            {
                put(sink, "_");
                count = 0;
            }
        }
    }

    private void formatBitArray(Writer)(auto ref Writer sink) const
    {
        put(sink, "[");
        foreach (idx; 0 .. _len)
        {
            put(sink, cast(char)(this[idx] + '0'));
            if (idx + 1 < _len)
                put(sink, ", ");
        }
        put(sink, "]");
    }

    // https://issues.dlang.org/show_bug.cgi?id=20639
    // Separate @nogc test because public tests use array literals
    // (and workarounds needlessly uglify those examples)
    @system @nogc unittest
    {
        size_t[2] buffer;
        BitArray b = BitArray(buffer[], buffer.sizeof * 8);

        b[] = true;
        b[0 .. 1] = true;
        b.flip();
        b.flip(1);
        cast(void) b.count();
    }
}

/// Slicing & bitsSet
@system unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota;

    bool[] buf = new bool[64 * 3];
    buf[0 .. 64] = true;
    BitArray b = BitArray(buf);
    assert(b.bitsSet.equal(iota(0, 64)));
    b <<= 64;
    assert(b.bitsSet.equal(iota(64, 128)));
}

/// Concatenation and appending
@system unittest
{
    import std.algorithm.comparison : equal;

    auto b = BitArray([1, 0]);
    b ~= true;
    assert(b[2] == 1);
    b ~= BitArray([0, 1]);
    auto c = BitArray([1, 0, 1, 0, 1]);
    assert(b == c);
    assert(b.bitsSet.equal([0, 2, 4]));
}

/// Bit flipping
@system unittest
{
    import std.algorithm.comparison : equal;

    auto b = BitArray([1, 1, 0, 1]);
    b &= BitArray([0, 1, 1, 0]);
    assert(b.bitsSet.equal([1]));
    b.flip;
    assert(b.bitsSet.equal([0, 2, 3]));
}

/// String format of bitarrays
@system unittest
{
    import std.format : format;
    auto b = BitArray([1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%b", b) == "1_00001111_00001111");
}

///
@system unittest
{
    import std.format : format;

    BitArray b;

    b = BitArray([]);
    assert(format("%s", b) == "[]");
    assert(format("%b", b) is null);

    b = BitArray([1]);
    assert(format("%s", b) == "[1]");
    assert(format("%b", b) == "1");

    b = BitArray([0, 0, 0, 0]);
    assert(format("%b", b) == "0000");

    b = BitArray([0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%s", b) == "[0, 0, 0, 0, 1, 1, 1, 1]");
    assert(format("%b", b) == "00001111");

    b = BitArray([0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%s", b) == "[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]");
    assert(format("%b", b) == "00001111_00001111");

    b = BitArray([1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%b", b) == "1_00001111");

    b = BitArray([1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%b", b) == "1_00001111_00001111");
}

@system unittest
{
    BitArray a;
    a.length = 5;
    foreach (ref bool b; a)
    {
        assert(b == 0);
        b = 1;
    }
    foreach (bool b; a)
        assert(b == 1);
}

/++
    Swaps the endianness of the given integral value or character.
  +/
T swapEndian(T)(const T val) @safe pure nothrow @nogc
if (isIntegral!T || isSomeChar!T || isBoolean!T)
{
    import core.bitop : bswap, byteswap;
    static if (val.sizeof == 1)
        return val;
    else static if (T.sizeof == 2)
        return cast(T) byteswap(cast(ushort) val);
    else static if (T.sizeof == 4)
        return cast(T) bswap(cast(uint) val);
    else static if (T.sizeof == 8)
        return cast(T) bswap(cast(ulong) val);
    else
        static assert(0, T.stringof ~ " unsupported by swapEndian.");
}

///
@safe unittest
{
    assert(42.swapEndian == 704643072);
    assert(42.swapEndian.swapEndian == 42); // reflexive
    assert(1.swapEndian == 16777216);

    assert(true.swapEndian == true);
    assert(byte(10).swapEndian == 10);
    assert(char(10).swapEndian == 10);

    assert(ushort(10).swapEndian == 2560);
    assert(long(10).swapEndian == 720575940379279360);
    assert(ulong(10).swapEndian == 720575940379279360);
}

@safe unittest
{
    import std.meta;
    import std.stdio;
    static foreach (T; AliasSeq!(bool, byte, ubyte, short, ushort, int, uint, long, ulong, char, wchar, dchar))
    {{
        scope(failure) writeln("Failed type: ", T.stringof);
        T val;
        const T cval;
        immutable T ival;

        assert(swapEndian(swapEndian(val)) == val);
        assert(swapEndian(swapEndian(cval)) == cval);
        assert(swapEndian(swapEndian(ival)) == ival);
        assert(swapEndian(swapEndian(T.min)) == T.min);
        assert(swapEndian(swapEndian(T.max)) == T.max);

        // Check CTFE compiles.
        static assert(swapEndian(swapEndian(T(1))) is T(1));

        foreach (i; 2 .. 10)
        {
            immutable T maxI = cast(T)(T.max / i);
            immutable T minI = cast(T)(T.min / i);

            assert(swapEndian(swapEndian(maxI)) == maxI);

            static if (isSigned!T)
                assert(swapEndian(swapEndian(minI)) == minI);
        }

        static if (isSigned!T)
            assert(swapEndian(swapEndian(cast(T) 0)) == 0);

        // used to trigger https://issues.dlang.org/show_bug.cgi?id=6354
        static if (T.sizeof > 1 && isUnsigned!T)
        {
            T left = 0xffU;
            left <<= (T.sizeof - 1) * 8;
            T right = 0xffU;

            for (size_t i = 1; i < T.sizeof; ++i)
            {
                assert(swapEndian(left) == right);
                assert(swapEndian(right) == left);
                left >>= 8;
                right <<= 8;
            }
        }
    }}
}


private union EndianSwapper(T)
if (canSwapEndianness!T)
{
    T value;
    ubyte[T.sizeof] array;

    static if (is(immutable FloatingPointTypeOf!(T) == immutable float))
        uint  intValue;
    else static if (is(immutable FloatingPointTypeOf!(T) == immutable double))
        ulong intValue;

}

// Can't use EndianSwapper union during CTFE.
private auto ctfeRead(T)(const ubyte[T.sizeof] array)
if (__traits(isIntegral, T))
{
    Unqual!T result;
    version (LittleEndian)
        foreach_reverse (b; array)
            result = cast(Unqual!T) ((result << 8) | b);
    else
        foreach (b; array)
            result = cast(Unqual!T) ((result << 8) | b);
    return cast(T) result;
}

// Can't use EndianSwapper union during CTFE.
private auto ctfeBytes(T)(const T value)
if (__traits(isIntegral, T))
{
    ubyte[T.sizeof] result;
    Unqual!T tmp = value;
    version (LittleEndian)
    {
        foreach (i; 0 .. T.sizeof)
        {
            result[i] = cast(ubyte) tmp;
            tmp = cast(Unqual!T) (tmp >>> 8);
        }
    }
    else
    {
        foreach_reverse (i; 0 .. T.sizeof)
        {
            result[i] = cast(ubyte) tmp;
            tmp = cast(Unqual!T) (tmp >>> 8);
        }
    }
    return result;
}

/++
    Converts the given value from the native endianness to big endian and
    returns it as a `ubyte[n]` where `n` is the size of the given type.

    Returning a `ubyte[n]` helps prevent accidentally using a swapped value
    as a regular one (and in the case of floating point values, it's necessary,
    because the FPU will mess up any swapped floating point values. So, you
    can't actually have swapped floating point values as floating point values).

    `real` is not supported, because its size is implementation-dependent
    and therefore could vary from machine to machine (which could make it
    unusable if you tried to transfer it to another machine).
  +/
auto nativeToBigEndian(T)(const T val) @safe pure nothrow @nogc
if (canSwapEndianness!T)
{
    version (LittleEndian)
        return nativeToEndianImpl!true(val);
    else
        return nativeToEndianImpl!false(val);
}

///
@safe unittest
{
    int i = 12345;
    ubyte[4] swappedI = nativeToBigEndian(i);
    assert(i == bigEndianToNative!int(swappedI));

    float f = 123.45f;
    ubyte[4] swappedF = nativeToBigEndian(f);
    assert(f == bigEndianToNative!float(swappedF));

    const float cf = 123.45f;
    ubyte[4] swappedCF = nativeToBigEndian(cf);
    assert(cf == bigEndianToNative!float(swappedCF));

    double d = 123.45;
    ubyte[8] swappedD = nativeToBigEndian(d);
    assert(d == bigEndianToNative!double(swappedD));

    const double cd = 123.45;
    ubyte[8] swappedCD = nativeToBigEndian(cd);
    assert(cd == bigEndianToNative!double(swappedCD));
}

private auto nativeToEndianImpl(bool swap, T)(const T val) @safe pure nothrow @nogc
if (__traits(isIntegral, T))
{
    if (!__ctfe)
    {
        static if (swap)
            return EndianSwapper!T(swapEndian(val)).array;
        else
            return EndianSwapper!T(val).array;
    }
    else
    {
        // Can't use EndianSwapper in CTFE.
        static if (swap)
            return ctfeBytes(swapEndian(val));
        else
            return ctfeBytes(val);
    }
}

@safe unittest
{
    import std.meta;
    import std.stdio;
    static foreach (T; AliasSeq!(bool, byte, ubyte, short, ushort, int, uint, long, ulong,
                         char, wchar, dchar
        /* The trouble here is with floats and doubles being compared against nan
         * using a bit compare. There are two kinds of nans, quiet and signaling.
         * When a nan passes through the x87, it converts signaling to quiet.
         * When a nan passes through the XMM, it does not convert signaling to quiet.
         * float.init is a signaling nan.
         * The binary API sometimes passes the data through the XMM, sometimes through
         * the x87, meaning these will fail the 'is' bit compare under some circumstances.
         * I cannot think of a fix for this that makes consistent sense.
         */
                          /*,float, double*/))
    {{
        scope(failure) writeln("Failed type: ", T.stringof);
        T val;
        const T cval;
        immutable T ival;

        //is instead of == because of NaN for floating point values.
        assert(bigEndianToNative!T(nativeToBigEndian(val)) is val);
        assert(bigEndianToNative!T(nativeToBigEndian(cval)) is cval);
        assert(bigEndianToNative!T(nativeToBigEndian(ival)) is ival);
        assert(bigEndianToNative!T(nativeToBigEndian(T.min)) == T.min);
        assert(bigEndianToNative!T(nativeToBigEndian(T.max)) == T.max);

        //Check CTFE compiles.
        static assert(bigEndianToNative!T(nativeToBigEndian(T(1))) is T(1));

        static if (isSigned!T)
            assert(bigEndianToNative!T(nativeToBigEndian(cast(T) 0)) == 0);

        static if (!is(T == bool))
        {
            foreach (i; [2, 4, 6, 7, 9, 11])
            {
                immutable T maxI = cast(T)(T.max / i);
                immutable T minI = cast(T)(T.min / i);

                assert(bigEndianToNative!T(nativeToBigEndian(maxI)) == maxI);

                static if (T.sizeof > 1)
                    assert(nativeToBigEndian(maxI) != nativeToLittleEndian(maxI));
                else
                    assert(nativeToBigEndian(maxI) == nativeToLittleEndian(maxI));

                static if (isSigned!T)
                {
                    assert(bigEndianToNative!T(nativeToBigEndian(minI)) == minI);

                    static if (T.sizeof > 1)
                        assert(nativeToBigEndian(minI) != nativeToLittleEndian(minI));
                    else
                        assert(nativeToBigEndian(minI) == nativeToLittleEndian(minI));
                }
            }
        }

        static if (isUnsigned!T || T.sizeof == 1 || is(T == wchar))
            assert(nativeToBigEndian(T.max) == nativeToLittleEndian(T.max));
        else
            assert(nativeToBigEndian(T.max) != nativeToLittleEndian(T.max));

        static if (isUnsigned!T || T.sizeof == 1 || isSomeChar!T)
            assert(nativeToBigEndian(T.min) == nativeToLittleEndian(T.min));
        else
            assert(nativeToBigEndian(T.min) != nativeToLittleEndian(T.min));
    }}
}


/++
    Converts the given value from big endian to the native endianness and
    returns it. The value is given as a `ubyte[n]` where `n` is the size
    of the target type. You must give the target type as a template argument,
    because there are multiple types with the same size and so the type of the
    argument is not enough to determine the return type.

    Taking a `ubyte[n]` helps prevent accidentally using a swapped value
    as a regular one (and in the case of floating point values, it's necessary,
    because the FPU will mess up any swapped floating point values. So, you
    can't actually have swapped floating point values as floating point values).
  +/
T bigEndianToNative(T, size_t n)(ubyte[n] val) @safe pure nothrow @nogc
if (canSwapEndianness!T && n == T.sizeof)
{
    version (LittleEndian)
        return endianToNativeImpl!(true, T, n)(val);
    else
        return endianToNativeImpl!(false, T, n)(val);
}

///
@safe unittest
{
    ushort i = 12345;
    ubyte[2] swappedI = nativeToBigEndian(i);
    assert(i == bigEndianToNative!ushort(swappedI));

    dchar c = 'D';
    ubyte[4] swappedC = nativeToBigEndian(c);
    assert(c == bigEndianToNative!dchar(swappedC));
}

/++
    Converts the given value from the native endianness to little endian and
    returns it as a `ubyte[n]` where `n` is the size of the given type.

    Returning a `ubyte[n]` helps prevent accidentally using a swapped value
    as a regular one (and in the case of floating point values, it's necessary,
    because the FPU will mess up any swapped floating point values. So, you
    can't actually have swapped floating point values as floating point values).
  +/
auto nativeToLittleEndian(T)(const T val) @safe pure nothrow @nogc
if (canSwapEndianness!T)
{
    version (BigEndian)
        return nativeToEndianImpl!true(val);
    else
        return nativeToEndianImpl!false(val);
}

///
@safe unittest
{
    int i = 12345;
    ubyte[4] swappedI = nativeToLittleEndian(i);
    assert(i == littleEndianToNative!int(swappedI));

    double d = 123.45;
    ubyte[8] swappedD = nativeToLittleEndian(d);
    assert(d == littleEndianToNative!double(swappedD));
}

@safe unittest
{
    import std.meta;
    import std.stdio;
    static foreach (T; AliasSeq!(bool, byte, ubyte, short, ushort, int, uint, long, ulong,
                         char, wchar, dchar/*,
                         float, double*/))
    {{
        scope(failure) writeln("Failed type: ", T.stringof);
        T val;
        const T cval;
        immutable T ival;

        //is instead of == because of NaN for floating point values.
        assert(littleEndianToNative!T(nativeToLittleEndian(val)) is val);
        assert(littleEndianToNative!T(nativeToLittleEndian(cval)) is cval);
        assert(littleEndianToNative!T(nativeToLittleEndian(ival)) is ival);
        assert(littleEndianToNative!T(nativeToLittleEndian(T.min)) == T.min);
        assert(littleEndianToNative!T(nativeToLittleEndian(T.max)) == T.max);

        //Check CTFE compiles.
        static assert(littleEndianToNative!T(nativeToLittleEndian(T(1))) is T(1));

        static if (isSigned!T)
            assert(littleEndianToNative!T(nativeToLittleEndian(cast(T) 0)) == 0);

        static if (!is(T == bool))
        {
            foreach (i; 2 .. 10)
            {
                immutable T maxI = cast(T)(T.max / i);
                immutable T minI = cast(T)(T.min / i);

                assert(littleEndianToNative!T(nativeToLittleEndian(maxI)) == maxI);

                static if (isSigned!T)
                    assert(littleEndianToNative!T(nativeToLittleEndian(minI)) == minI);
            }
        }
    }}
}


/++
    Converts the given value from little endian to the native endianness and
    returns it. The value is given as a `ubyte[n]` where `n` is the size
    of the target type. You must give the target type as a template argument,
    because there are multiple types with the same size and so the type of the
    argument is not enough to determine the return type.

    Taking a `ubyte[n]` helps prevent accidentally using a swapped value
    as a regular one (and in the case of floating point values, it's necessary,
    because the FPU will mess up any swapped floating point values. So, you
    can't actually have swapped floating point values as floating point values).

    `real` is not supported, because its size is implementation-dependent
    and therefore could vary from machine to machine (which could make it
    unusable if you tried to transfer it to another machine).
  +/
T littleEndianToNative(T, size_t n)(ubyte[n] val) @safe pure nothrow @nogc
if (canSwapEndianness!T && n == T.sizeof)
{
    version (BigEndian)
        return endianToNativeImpl!(true, T, n)(val);
    else
        return endianToNativeImpl!(false, T, n)(val);
}

///
@safe unittest
{
    ushort i = 12345;
    ubyte[2] swappedI = nativeToLittleEndian(i);
    assert(i == littleEndianToNative!ushort(swappedI));

    dchar c = 'D';
    ubyte[4] swappedC = nativeToLittleEndian(c);
    assert(c == littleEndianToNative!dchar(swappedC));
}

private T endianToNativeImpl(bool swap, T, size_t n)(ubyte[n] val) @nogc nothrow pure @safe
if (__traits(isIntegral, T) && n == T.sizeof)
{
    if (!__ctfe)
    {
        EndianSwapper!T es = { array: val };
        static if (swap)
            return swapEndian(es.value);
        else
            return es.value;
    }
    else
    {
        static if (swap)
            return swapEndian(ctfeRead!T(val));
        else
            return ctfeRead!T(val);
    }
}

private auto nativeToEndianImpl(bool swap, T)(const T val) @trusted pure nothrow @nogc
if (isFloatOrDouble!T)
{
    if (!__ctfe)
    {
        EndianSwapper!T es = EndianSwapper!T(val);
        static if (swap)
            es.intValue = swapEndian(es.intValue);
        return es.array;
    }
    else
    {
        static if (T.sizeof == 4)
            uint intValue = *cast(const uint*) &val;
        else static if (T.sizeof == 8)
            ulong intValue = *cast(const ulong*) & val;
        static if (swap)
            intValue = swapEndian(intValue);
        return ctfeBytes(intValue);
    }
}

private auto endianToNativeImpl(bool swap, T, size_t n)(ubyte[n] val) @trusted pure nothrow @nogc
if (isFloatOrDouble!T && n == T.sizeof)
{
    if (!__ctfe)
    {
        EndianSwapper!T es = { array: val };
        static if (swap)
            es.intValue = swapEndian(es.intValue);
        return es.value;
    }
    else
    {
        static if (n == 4)
            uint x = ctfeRead!uint(val);
        else static if (n == 8)
            ulong x = ctfeRead!ulong(val);
        static if (swap)
            x = swapEndian(x);
        return *cast(T*) &x;
    }
}

private template isFloatOrDouble(T)
{
    enum isFloatOrDouble = isFloatingPoint!T &&
                           !is(immutable FloatingPointTypeOf!T == immutable real);
}

@safe unittest
{
    import std.meta;
    static foreach (T; AliasSeq!(float, double))
    {
        static assert(isFloatOrDouble!(T));
        static assert(isFloatOrDouble!(const T));
        static assert(isFloatOrDouble!(immutable T));
        static assert(isFloatOrDouble!(shared T));
        static assert(isFloatOrDouble!(shared(const T)));
        static assert(isFloatOrDouble!(shared(immutable T)));
    }

    static assert(!isFloatOrDouble!(real));
    static assert(!isFloatOrDouble!(const real));
    static assert(!isFloatOrDouble!(immutable real));
    static assert(!isFloatOrDouble!(shared real));
    static assert(!isFloatOrDouble!(shared(const real)));
    static assert(!isFloatOrDouble!(shared(immutable real)));
}

private template canSwapEndianness(T)
{
    enum canSwapEndianness = isIntegral!T ||
                             isSomeChar!T ||
                             isBoolean!T ||
                             isFloatOrDouble!T;
}

@safe unittest
{
    import std.meta;
    static foreach (T; AliasSeq!(bool, ubyte, byte, ushort, short, uint, int, ulong,
                         long, char, wchar, dchar, float, double))
    {
        static assert(canSwapEndianness!(T));
        static assert(canSwapEndianness!(const T));
        static assert(canSwapEndianness!(immutable T));
        static assert(canSwapEndianness!(shared(T)));
        static assert(canSwapEndianness!(shared(const T)));
        static assert(canSwapEndianness!(shared(immutable T)));
    }

    //!
    static foreach (T; AliasSeq!(real, string, wstring, dstring))
    {
        static assert(!canSwapEndianness!(T));
        static assert(!canSwapEndianness!(const T));
        static assert(!canSwapEndianness!(immutable T));
        static assert(!canSwapEndianness!(shared(T)));
        static assert(!canSwapEndianness!(shared(const T)));
        static assert(!canSwapEndianness!(shared(immutable T)));
    }
}

/++
    Takes a range of `ubyte`s and converts the first `T.sizeof` bytes to
    `T`. The value returned is converted from the given endianness to the
    native endianness. The range is not consumed.

    Params:
        T     = The integral type to convert the first `T.sizeof` bytes to.
        endianness = The endianness that the bytes are assumed to be in.
        range = The range to read from.
        index = The index to start reading from (instead of starting at the
                front). If index is a pointer, then it is updated to the index
                after the bytes read. The overloads with index are only
                available if `hasSlicing!R` is `true`.
  +/

T peek(T, Endian endianness = Endian.bigEndian, R)(R range)
if (canSwapEndianness!T &&
    isForwardRange!R &&
    is(ElementType!R : const ubyte))
{
    static if (hasSlicing!R)
        const ubyte[T.sizeof] bytes = range[0 .. T.sizeof];
    else
    {
        ubyte[T.sizeof] bytes;
        //Make sure that range is not consumed, even if it's a class.
        range = range.save;

        foreach (ref e; bytes)
        {
            e = range.front;
            range.popFront();
        }
    }

    static if (endianness == Endian.bigEndian)
        return bigEndianToNative!T(bytes);
    else
        return littleEndianToNative!T(bytes);
}

/++ Ditto +/
T peek(T, Endian endianness = Endian.bigEndian, R)(R range, size_t index)
if (canSwapEndianness!T &&
    isForwardRange!R &&
    hasSlicing!R &&
    is(ElementType!R : const ubyte))
{
    return peek!(T, endianness)(range, &index);
}

/++ Ditto +/
T peek(T, Endian endianness = Endian.bigEndian, R)(R range, size_t* index)
if (canSwapEndianness!T &&
    isForwardRange!R &&
    hasSlicing!R &&
    is(ElementType!R : const ubyte))
{
    assert(index, "index must not point to null");

    immutable begin = *index;
    immutable end = begin + T.sizeof;
    const ubyte[T.sizeof] bytes = range[begin .. end];
    *index = end;

    static if (endianness == Endian.bigEndian)
        return bigEndianToNative!T(bytes);
    else
        return littleEndianToNative!T(bytes);
}

///
@system unittest
{
    ubyte[] buffer = [1, 5, 22, 9, 44, 255, 8];
    assert(buffer.peek!uint() == 17110537);
    assert(buffer.peek!ushort() == 261);
    assert(buffer.peek!ubyte() == 1);

    assert(buffer.peek!uint(2) == 369700095);
    assert(buffer.peek!ushort(2) == 5641);
    assert(buffer.peek!ubyte(2) == 22);

    size_t index = 0;
    assert(buffer.peek!ushort(&index) == 261);
    assert(index == 2);

    assert(buffer.peek!uint(&index) == 369700095);
    assert(index == 6);

    assert(buffer.peek!ubyte(&index) == 8);
    assert(index == 7);
}

///
@safe unittest
{
    import std.algorithm.iteration : filter;
    ubyte[] buffer = [1, 5, 22, 9, 44, 255, 7];
    auto range = filter!"true"(buffer);
    assert(range.peek!uint() == 17110537);
    assert(range.peek!ushort() == 261);
    assert(range.peek!ubyte() == 1);
}

@system unittest
{
    {
        //bool
        ubyte[] buffer = [0, 1];
        assert(buffer.peek!bool() == false);
        assert(buffer.peek!bool(1) == true);

        size_t index = 0;
        assert(buffer.peek!bool(&index) == false);
        assert(index == 1);

        assert(buffer.peek!bool(&index) == true);
        assert(index == 2);
    }

    {
        //char (8bit)
        ubyte[] buffer = [97, 98, 99, 100];
        assert(buffer.peek!char() == 'a');
        assert(buffer.peek!char(1) == 'b');

        size_t index = 0;
        assert(buffer.peek!char(&index) == 'a');
        assert(index == 1);

        assert(buffer.peek!char(&index) == 'b');
        assert(index == 2);
    }

    {
        //wchar (16bit - 2x ubyte)
        ubyte[] buffer = [1, 5, 32, 29, 1, 7];
        assert(buffer.peek!wchar() == 'ą');
        assert(buffer.peek!wchar(2) == '”');
        assert(buffer.peek!wchar(4) == 'ć');

        size_t index = 0;
        assert(buffer.peek!wchar(&index) == 'ą');
        assert(index == 2);

        assert(buffer.peek!wchar(&index) == '”');
        assert(index == 4);

        assert(buffer.peek!wchar(&index) == 'ć');
        assert(index == 6);
    }

    {
        //dchar (32bit - 4x ubyte)
        ubyte[] buffer = [0, 0, 1, 5, 0, 0, 32, 29, 0, 0, 1, 7];
        assert(buffer.peek!dchar() == 'ą');
        assert(buffer.peek!dchar(4) == '”');
        assert(buffer.peek!dchar(8) == 'ć');

        size_t index = 0;
        assert(buffer.peek!dchar(&index) == 'ą');
        assert(index == 4);

        assert(buffer.peek!dchar(&index) == '”');
        assert(index == 8);

        assert(buffer.peek!dchar(&index) == 'ć');
        assert(index == 12);
    }

    {
        //float (32bit - 4x ubyte)
        ubyte[] buffer = [66, 0, 0, 0, 65, 200, 0, 0];
        assert(buffer.peek!float()== 32.0);
        assert(buffer.peek!float(4) == 25.0f);

        size_t index = 0;
        assert(buffer.peek!float(&index) == 32.0f);
        assert(index == 4);

        assert(buffer.peek!float(&index) == 25.0f);
        assert(index == 8);
    }

    {
        //double (64bit - 8x ubyte)
        ubyte[] buffer = [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0];
        assert(buffer.peek!double() == 32.0);
        assert(buffer.peek!double(8) == 25.0);

        size_t index = 0;
        assert(buffer.peek!double(&index) == 32.0);
        assert(index == 8);

        assert(buffer.peek!double(&index) == 25.0);
        assert(index == 16);
    }

    {
        //enum
        ubyte[] buffer = [0, 0, 0, 10, 0, 0, 0, 20, 0, 0, 0, 30];

        enum Foo
        {
            one = 10,
            two = 20,
            three = 30
        }

        assert(buffer.peek!Foo() == Foo.one);
        assert(buffer.peek!Foo(0) == Foo.one);
        assert(buffer.peek!Foo(4) == Foo.two);
        assert(buffer.peek!Foo(8) == Foo.three);

        size_t index = 0;
        assert(buffer.peek!Foo(&index) == Foo.one);
        assert(index == 4);

        assert(buffer.peek!Foo(&index) == Foo.two);
        assert(index == 8);

        assert(buffer.peek!Foo(&index) == Foo.three);
        assert(index == 12);
    }

    {
        //enum - bool
        ubyte[] buffer = [0, 1];

        enum Bool: bool
        {
            bfalse = false,
            btrue = true,
        }

        assert(buffer.peek!Bool() == Bool.bfalse);
        assert(buffer.peek!Bool(0) == Bool.bfalse);
        assert(buffer.peek!Bool(1) == Bool.btrue);

        size_t index = 0;
        assert(buffer.peek!Bool(&index) == Bool.bfalse);
        assert(index == 1);

        assert(buffer.peek!Bool(&index) == Bool.btrue);
        assert(index == 2);
    }

    {
        //enum - float
        ubyte[] buffer = [66, 0, 0, 0, 65, 200, 0, 0];

        enum Float: float
        {
            one = 32.0f,
            two = 25.0f
        }

        assert(buffer.peek!Float() == Float.one);
        assert(buffer.peek!Float(0) == Float.one);
        assert(buffer.peek!Float(4) == Float.two);

        size_t index = 0;
        assert(buffer.peek!Float(&index) == Float.one);
        assert(index == 4);

        assert(buffer.peek!Float(&index) == Float.two);
        assert(index == 8);
    }

    {
        //enum - double
        ubyte[] buffer = [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0];

        enum Double: double
        {
            one = 32.0,
            two = 25.0
        }

        assert(buffer.peek!Double() == Double.one);
        assert(buffer.peek!Double(0) == Double.one);
        assert(buffer.peek!Double(8) == Double.two);

        size_t index = 0;
        assert(buffer.peek!Double(&index) == Double.one);
        assert(index == 8);

        assert(buffer.peek!Double(&index) == Double.two);
        assert(index == 16);
    }

    {
        //enum - real
        ubyte[] buffer = [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0];

        enum Real: real
        {
            one = 32.0,
            two = 25.0
        }

        static assert(!__traits(compiles, buffer.peek!Real()));
    }
}

/++
    Takes a range of `ubyte`s and converts the first `T.sizeof` bytes to
    `T`. The value returned is converted from the given endianness to the
    native endianness. The `T.sizeof` bytes which are read are consumed from
    the range.

    Params:
        T     = The integral type to convert the first `T.sizeof` bytes to.
        endianness = The endianness that the bytes are assumed to be in.
        range = The range to read from.
  +/
T read(T, Endian endianness = Endian.bigEndian, R)(ref R range)
if (canSwapEndianness!T && isInputRange!R && is(ElementType!R : const ubyte))
{
    static if (hasSlicing!R && is(typeof(R.init[0 .. 0]) : const(ubyte)[]))
    {
        const ubyte[T.sizeof] bytes = range[0 .. T.sizeof];
        range.popFrontN(T.sizeof);
    }
    else
    {
        ubyte[T.sizeof] bytes;

        foreach (ref e; bytes)
        {
            e = range.front;
            range.popFront();
        }
    }

    static if (endianness == Endian.bigEndian)
        return bigEndianToNative!T(bytes);
    else
        return littleEndianToNative!T(bytes);
}

///
@safe unittest
{
    import std.range.primitives : empty;
    ubyte[] buffer = [1, 5, 22, 9, 44, 255, 8];
    assert(buffer.length == 7);

    assert(buffer.read!ushort() == 261);
    assert(buffer.length == 5);

    assert(buffer.read!uint() == 369700095);
    assert(buffer.length == 1);

    assert(buffer.read!ubyte() == 8);
    assert(buffer.empty);
}

@safe unittest
{
    {
        //bool
        ubyte[] buffer = [0, 1];
        assert(buffer.length == 2);

        assert(buffer.read!bool() == false);
        assert(buffer.length == 1);

        assert(buffer.read!bool() == true);
        assert(buffer.empty);
    }

    {
        //char (8bit)
        ubyte[] buffer = [97, 98, 99];
        assert(buffer.length == 3);

        assert(buffer.read!char() == 'a');
        assert(buffer.length == 2);

        assert(buffer.read!char() == 'b');
        assert(buffer.length == 1);

        assert(buffer.read!char() == 'c');
        assert(buffer.empty);
    }

    {
        //wchar (16bit - 2x ubyte)
        ubyte[] buffer = [1, 5, 32, 29, 1, 7];
        assert(buffer.length == 6);

        assert(buffer.read!wchar() == 'ą');
        assert(buffer.length == 4);

        assert(buffer.read!wchar() == '”');
        assert(buffer.length == 2);

        assert(buffer.read!wchar() == 'ć');
        assert(buffer.empty);
    }

    {
        //dchar (32bit - 4x ubyte)
        ubyte[] buffer = [0, 0, 1, 5, 0, 0, 32, 29, 0, 0, 1, 7];
        assert(buffer.length == 12);

        assert(buffer.read!dchar() == 'ą');
        assert(buffer.length == 8);

        assert(buffer.read!dchar() == '”');
        assert(buffer.length == 4);

        assert(buffer.read!dchar() == 'ć');
        assert(buffer.empty);
    }

    {
        //float (32bit - 4x ubyte)
        ubyte[] buffer = [66, 0, 0, 0, 65, 200, 0, 0];
        assert(buffer.length == 8);

        assert(buffer.read!float()== 32.0);
        assert(buffer.length == 4);

        assert(buffer.read!float() == 25.0f);
        assert(buffer.empty);
    }

    {
        //double (64bit - 8x ubyte)
        ubyte[] buffer = [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0];
        assert(buffer.length == 16);

        assert(buffer.read!double() == 32.0);
        assert(buffer.length == 8);

        assert(buffer.read!double() == 25.0);
        assert(buffer.empty);
    }

    {
        //enum - uint
        ubyte[] buffer = [0, 0, 0, 10, 0, 0, 0, 20, 0, 0, 0, 30];
        assert(buffer.length == 12);

        enum Foo
        {
            one = 10,
            two = 20,
            three = 30
        }

        assert(buffer.read!Foo() == Foo.one);
        assert(buffer.length == 8);

        assert(buffer.read!Foo() == Foo.two);
        assert(buffer.length == 4);

        assert(buffer.read!Foo() == Foo.three);
        assert(buffer.empty);
    }

    {
        //enum - bool
        ubyte[] buffer = [0, 1];
        assert(buffer.length == 2);

        enum Bool: bool
        {
            bfalse = false,
            btrue = true,
        }

        assert(buffer.read!Bool() == Bool.bfalse);
        assert(buffer.length == 1);

        assert(buffer.read!Bool() == Bool.btrue);
        assert(buffer.empty);
    }

    {
        //enum - float
        ubyte[] buffer = [66, 0, 0, 0, 65, 200, 0, 0];
        assert(buffer.length == 8);

        enum Float: float
        {
            one = 32.0f,
            two = 25.0f
        }

        assert(buffer.read!Float() == Float.one);
        assert(buffer.length == 4);

        assert(buffer.read!Float() == Float.two);
        assert(buffer.empty);
    }

    {
        //enum - double
        ubyte[] buffer = [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0];
        assert(buffer.length == 16);

        enum Double: double
        {
            one = 32.0,
            two = 25.0
        }

        assert(buffer.read!Double() == Double.one);
        assert(buffer.length == 8);

        assert(buffer.read!Double() == Double.two);
        assert(buffer.empty);
    }

    {
        //enum - real
        ubyte[] buffer = [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0];

        enum Real: real
        {
            one = 32.0,
            two = 25.0
        }

        static assert(!__traits(compiles, buffer.read!Real()));
    }
}

// https://issues.dlang.org/show_bug.cgi?id=17247
@safe unittest
{
    struct UbyteRange
    {
        ubyte[] impl;
        @property bool empty() { return impl.empty; }
        @property ubyte front() { return impl.front; }
        void popFront() { impl.popFront(); }
        @property UbyteRange save() { return this; }

        // N.B. support slicing but do not return ubyte[] slices.
        UbyteRange opSlice(size_t start, size_t end)
        {
            return UbyteRange(impl[start .. end]);
        }
        @property size_t length() { return impl.length; }
        alias opDollar = length;
    }
    static assert(hasSlicing!UbyteRange);

    auto r = UbyteRange([0x01, 0x00, 0x00, 0x00]);
    int x = r.read!(int, Endian.littleEndian)();
    assert(x == 1);
}


/++
    Takes an integral value, converts it to the given endianness, and writes it
    to the given range of `ubyte`s as a sequence of `T.sizeof` `ubyte`s
    starting at index. `hasSlicing!R` must be `true`.

    Params:
        T     = The integral type to convert the first `T.sizeof` bytes to.
        endianness = The endianness to _write the bytes in.
        range = The range to _write to.
        value = The value to _write.
        index = The index to start writing to. If index is a pointer, then it
                is updated to the index after the bytes read.
  +/
void write(T, Endian endianness = Endian.bigEndian, R)(R range, const T value, size_t index)
if (canSwapEndianness!T &&
    isForwardRange!R &&
    hasSlicing!R &&
    is(ElementType!R : ubyte))
{
    write!(T, endianness)(range, value, &index);
}

/++ Ditto +/
void write(T, Endian endianness = Endian.bigEndian, R)(R range, const T value, size_t* index)
if (canSwapEndianness!T &&
    isForwardRange!R &&
    hasSlicing!R &&
    is(ElementType!R : ubyte))
{
    assert(index, "index must not point to null");

    static if (endianness == Endian.bigEndian)
        immutable bytes = nativeToBigEndian!T(value);
    else
        immutable bytes = nativeToLittleEndian!T(value);

    immutable begin = *index;
    immutable end = begin + T.sizeof;
    *index = end;
    range[begin .. end] = bytes[0 .. T.sizeof];
}

///
@system unittest
{
    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0];
    buffer.write!uint(29110231u, 0);
    assert(buffer == [1, 188, 47, 215, 0, 0, 0, 0]);

    buffer.write!ushort(927, 0);
    assert(buffer == [3, 159, 47, 215, 0, 0, 0, 0]);

    buffer.write!ubyte(42, 0);
    assert(buffer == [42, 159, 47, 215, 0, 0, 0, 0]);
}

///
@system unittest
{
    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0, 0];
    buffer.write!uint(142700095u, 2);
    assert(buffer == [0, 0, 8, 129, 110, 63, 0, 0, 0]);

    buffer.write!ushort(19839, 2);
    assert(buffer == [0, 0, 77, 127, 110, 63, 0, 0, 0]);

    buffer.write!ubyte(132, 2);
    assert(buffer == [0, 0, 132, 127, 110, 63, 0, 0, 0]);
}

///
@system unittest
{
    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0];
    size_t index = 0;
    buffer.write!ushort(261, &index);
    assert(buffer == [1, 5, 0, 0, 0, 0, 0, 0]);
    assert(index == 2);

    buffer.write!uint(369700095u, &index);
    assert(buffer == [1, 5, 22, 9, 44, 255, 0, 0]);
    assert(index == 6);

    buffer.write!ubyte(8, &index);
    assert(buffer == [1, 5, 22, 9, 44, 255, 8, 0]);
    assert(index == 7);
}

/// bool
@system unittest
{
    ubyte[] buffer = [0, 0];
    buffer.write!bool(false, 0);
    assert(buffer == [0, 0]);

    buffer.write!bool(true, 0);
    assert(buffer == [1, 0]);

    buffer.write!bool(true, 1);
    assert(buffer == [1, 1]);

    buffer.write!bool(false, 1);
    assert(buffer == [1, 0]);

    size_t index = 0;
    buffer.write!bool(false, &index);
    assert(buffer == [0, 0]);
    assert(index == 1);

    buffer.write!bool(true, &index);
    assert(buffer == [0, 1]);
    assert(index == 2);
}

/// char(8-bit)
@system unittest
{
    ubyte[] buffer = [0, 0, 0];

    buffer.write!char('a', 0);
    assert(buffer == [97, 0, 0]);

    buffer.write!char('b', 1);
    assert(buffer == [97, 98, 0]);

    size_t index = 0;
    buffer.write!char('a', &index);
    assert(buffer == [97, 98, 0]);
    assert(index == 1);

    buffer.write!char('b', &index);
    assert(buffer == [97, 98, 0]);
    assert(index == 2);

    buffer.write!char('c', &index);
    assert(buffer == [97, 98, 99]);
    assert(index == 3);
}

/// wchar (16bit - 2x ubyte)
@system unittest
{
    ubyte[] buffer = [0, 0, 0, 0];

    buffer.write!wchar('ą', 0);
    assert(buffer == [1, 5, 0, 0]);

    buffer.write!wchar('”', 2);
    assert(buffer == [1, 5, 32, 29]);

    size_t index = 0;
    buffer.write!wchar('ć', &index);
    assert(buffer == [1, 7, 32, 29]);
    assert(index == 2);

    buffer.write!wchar('ą', &index);
    assert(buffer == [1, 7, 1, 5]);
    assert(index == 4);
}

/// dchar (32bit - 4x ubyte)
@system unittest
{
    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0];

    buffer.write!dchar('ą', 0);
    assert(buffer == [0, 0, 1, 5, 0, 0, 0, 0]);

    buffer.write!dchar('”', 4);
    assert(buffer == [0, 0, 1, 5, 0, 0, 32, 29]);

    size_t index = 0;
    buffer.write!dchar('ć', &index);
    assert(buffer == [0, 0, 1, 7, 0, 0, 32, 29]);
    assert(index == 4);

    buffer.write!dchar('ą', &index);
    assert(buffer == [0, 0, 1, 7, 0, 0, 1, 5]);
    assert(index == 8);
}

/// float (32bit - 4x ubyte)
@system unittest
{
    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0];

    buffer.write!float(32.0f, 0);
    assert(buffer == [66, 0, 0, 0, 0, 0, 0, 0]);

    buffer.write!float(25.0f, 4);
    assert(buffer == [66, 0, 0, 0, 65, 200, 0, 0]);

    size_t index = 0;
    buffer.write!float(25.0f, &index);
    assert(buffer == [65, 200, 0, 0, 65, 200, 0, 0]);
    assert(index == 4);

    buffer.write!float(32.0f, &index);
    assert(buffer == [65, 200, 0, 0, 66, 0, 0, 0]);
    assert(index == 8);
}

/// double (64bit - 8x ubyte)
@system unittest
{
    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    buffer.write!double(32.0, 0);
    assert(buffer == [64, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);

    buffer.write!double(25.0, 8);
    assert(buffer == [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0]);

    size_t index = 0;
    buffer.write!double(25.0, &index);
    assert(buffer == [64, 57, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0]);
    assert(index == 8);

    buffer.write!double(32.0, &index);
    assert(buffer == [64, 57, 0, 0, 0, 0, 0, 0, 64, 64, 0, 0, 0, 0, 0, 0]);
    assert(index == 16);
}

/// enum
@system unittest
{
    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    enum Foo
    {
        one = 10,
        two = 20,
        three = 30
    }

    buffer.write!Foo(Foo.one, 0);
    assert(buffer == [0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0]);

    buffer.write!Foo(Foo.two, 4);
    assert(buffer == [0, 0, 0, 10, 0, 0, 0, 20, 0, 0, 0, 0]);

    buffer.write!Foo(Foo.three, 8);
    assert(buffer == [0, 0, 0, 10, 0, 0, 0, 20, 0, 0, 0, 30]);

    size_t index = 0;
    buffer.write!Foo(Foo.three, &index);
    assert(buffer == [0, 0, 0, 30, 0, 0, 0, 20, 0, 0, 0, 30]);
    assert(index == 4);

    buffer.write!Foo(Foo.one, &index);
    assert(buffer == [0, 0, 0, 30, 0, 0, 0, 10, 0, 0, 0, 30]);
    assert(index == 8);

    buffer.write!Foo(Foo.two, &index);
    assert(buffer == [0, 0, 0, 30, 0, 0, 0, 10, 0, 0, 0, 20]);
    assert(index == 12);
}

// enum - bool
@system unittest
{
    ubyte[] buffer = [0, 0];

    enum Bool: bool
    {
        bfalse = false,
        btrue = true,
    }

    buffer.write!Bool(Bool.btrue, 0);
    assert(buffer == [1, 0]);

    buffer.write!Bool(Bool.btrue, 1);
    assert(buffer == [1, 1]);

    size_t index = 0;
    buffer.write!Bool(Bool.bfalse, &index);
    assert(buffer == [0, 1]);
    assert(index == 1);

    buffer.write!Bool(Bool.bfalse, &index);
    assert(buffer == [0, 0]);
    assert(index == 2);
}

/// enum - float
@system unittest
{
    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0];

    enum Float: float
    {
        one = 32.0f,
        two = 25.0f
    }

    buffer.write!Float(Float.one, 0);
    assert(buffer == [66, 0, 0, 0, 0, 0, 0, 0]);

    buffer.write!Float(Float.two, 4);
    assert(buffer == [66, 0, 0, 0, 65, 200, 0, 0]);

    size_t index = 0;
    buffer.write!Float(Float.two, &index);
    assert(buffer == [65, 200, 0, 0, 65, 200, 0, 0]);
    assert(index == 4);

    buffer.write!Float(Float.one, &index);
    assert(buffer == [65, 200, 0, 0, 66, 0, 0, 0]);
    assert(index == 8);
}

/// enum - double
@system unittest
{
    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    enum Double: double
    {
        one = 32.0,
        two = 25.0
    }

    buffer.write!Double(Double.one, 0);
    assert(buffer == [64, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);

    buffer.write!Double(Double.two, 8);
    assert(buffer == [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0]);

    size_t index = 0;
    buffer.write!Double(Double.two, &index);
    assert(buffer == [64, 57, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0]);
    assert(index == 8);

    buffer.write!Double(Double.one, &index);
    assert(buffer == [64, 57, 0, 0, 0, 0, 0, 0, 64, 64, 0, 0, 0, 0, 0, 0]);
    assert(index == 16);
}

/// enum - real
@system unittest
{
    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    enum Real: real
    {
        one = 32.0,
        two = 25.0
    }

    static assert(!__traits(compiles, buffer.write!Real(Real.one)));
}


/++
    Takes an integral value, converts it to the given endianness, and appends
    it to the given range of `ubyte`s (using `put`) as a sequence of
    `T.sizeof` `ubyte`s starting at index. `hasSlicing!R` must be
    `true`.

    Params:
        T     = The integral type to convert the first `T.sizeof` bytes to.
        endianness = The endianness to write the bytes in.
        range = The range to _append to.
        value = The value to _append.
  +/
void append(T, Endian endianness = Endian.bigEndian, R)(R range, const T value)
if (canSwapEndianness!T && isOutputRange!(R, ubyte))
{
    static if (endianness == Endian.bigEndian)
        immutable bytes = nativeToBigEndian!T(value);
    else
        immutable bytes = nativeToLittleEndian!T(value);

    put(range, bytes[]);
}

///
@safe unittest
{
    import std.array;
    auto buffer = appender!(const ubyte[])();
    buffer.append!ushort(261);
    assert(buffer.data == [1, 5]);

    buffer.append!uint(369700095u);
    assert(buffer.data == [1, 5, 22, 9, 44, 255]);

    buffer.append!ubyte(8);
    assert(buffer.data == [1, 5, 22, 9, 44, 255, 8]);
}

/// bool
@safe unittest
{
    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    buffer.append!bool(true);
    assert(buffer.data == [1]);

    buffer.append!bool(false);
    assert(buffer.data == [1, 0]);
}

/// char wchar dchar
@safe unittest
{
    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    buffer.append!char('a');
    assert(buffer.data == [97]);

    buffer.append!char('b');
    assert(buffer.data == [97, 98]);

    buffer.append!wchar('ą');
    assert(buffer.data == [97, 98, 1, 5]);

    buffer.append!dchar('ą');
        assert(buffer.data == [97, 98, 1, 5, 0, 0, 1, 5]);
}

/// float double
@safe unittest
{
    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    buffer.append!float(32.0f);
    assert(buffer.data == [66, 0, 0, 0]);

    buffer.append!double(32.0);
    assert(buffer.data == [66, 0, 0, 0, 64, 64, 0, 0, 0, 0, 0, 0]);
}

/// enum
@safe unittest
{
    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    enum Foo
    {
        one = 10,
        two = 20,
        three = 30
    }

    buffer.append!Foo(Foo.one);
    assert(buffer.data == [0, 0, 0, 10]);

    buffer.append!Foo(Foo.two);
    assert(buffer.data == [0, 0, 0, 10, 0, 0, 0, 20]);

    buffer.append!Foo(Foo.three);
    assert(buffer.data == [0, 0, 0, 10, 0, 0, 0, 20, 0, 0, 0, 30]);
}

/// enum - bool
@safe unittest
{
    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    enum Bool: bool
    {
        bfalse = false,
        btrue = true,
    }

    buffer.append!Bool(Bool.btrue);
    assert(buffer.data == [1]);

    buffer.append!Bool(Bool.bfalse);
    assert(buffer.data == [1, 0]);

    buffer.append!Bool(Bool.btrue);
    assert(buffer.data == [1, 0, 1]);
}

/// enum - float
@safe unittest
{
    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    enum Float: float
    {
        one = 32.0f,
        two = 25.0f
    }

    buffer.append!Float(Float.one);
    assert(buffer.data == [66, 0, 0, 0]);

    buffer.append!Float(Float.two);
    assert(buffer.data == [66, 0, 0, 0, 65, 200, 0, 0]);
}

/// enum - double
@safe unittest
{
    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    enum Double: double
    {
        one = 32.0,
        two = 25.0
    }

    buffer.append!Double(Double.one);
    assert(buffer.data == [64, 64, 0, 0, 0, 0, 0, 0]);

    buffer.append!Double(Double.two);
    assert(buffer.data == [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0]);
}

/// enum - real
@safe unittest
{
    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    enum Real: real
    {
        one = 32.0,
        two = 25.0
    }

    static assert(!__traits(compiles, buffer.append!Real(Real.one)));
}

@system unittest
{
    import std.array;
    import std.format : format;
    import std.meta : AliasSeq;
    static foreach (endianness; [Endian.bigEndian, Endian.littleEndian])
    {{
        auto toWrite = appender!(ubyte[])();
        alias Types = AliasSeq!(uint, int, long, ulong, short, ubyte, ushort, byte, uint);
        ulong[] values = [42, -11, long.max, 1098911981329L, 16, 255, 19012, 2, 17];
        assert(Types.length == values.length);

        size_t index = 0;
        size_t length = 0;
        static foreach (T; Types)
        {
            toWrite.append!(T, endianness)(cast(T) values[index++]);
            length += T.sizeof;
        }

        auto toRead = toWrite.data;
        assert(toRead.length == length);

        index = 0;
        static foreach (T; Types)
        {
            assert(toRead.peek!(T, endianness)() == values[index], format("Failed Index: %s", index));
            assert(toRead.peek!(T, endianness)(0) == values[index], format("Failed Index: %s", index));
            assert(toRead.length == length,
                   format("Failed Index [%s], Actual Length: %s", index, toRead.length));
            assert(toRead.read!(T, endianness)() == values[index], format("Failed Index: %s", index));
            length -= T.sizeof;
            assert(toRead.length == length,
                   format("Failed Index [%s], Actual Length: %s", index, toRead.length));
            ++index;
        }
        assert(toRead.empty);
    }}
}

/**
Counts the number of set bits in the binary representation of `value`.
For signed integers, the sign bit is included in the count.
*/
private uint countBitsSet(T)(const T value)
if (isIntegral!T)
{
    static if (T.sizeof == 8)
    {
        import core.bitop : popcnt;
        const c = popcnt(cast(ulong) value);
    }
    else static if (T.sizeof == 4)
    {
        import core.bitop : popcnt;
        const c = popcnt(cast(uint) value);
    }
    // http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
    else static if (T.sizeof == 2)
    {
        uint c = value - ((value >> 1) & 0x5555);
        c = ((c >> 2) & 0x3333) + (c & 0X3333);
        c = ((c >> 4) + c) & 0x0F0F;
        c = ((c >> 8) + c) & 0x00FF;
    }
    else static if (T.sizeof == 1)
    {
        uint c = value - ((value >> 1) & 0x55);
        c = ((c >> 2) & 0x33) + (c & 0X33);
        c = ((c >> 4) + c) & 0x0F;
    }
    else
    {
        static assert(false, "countBitsSet only supports 1, 2, 4, or 8 byte sized integers.");
    }
    return cast(uint) c;
}

@safe unittest
{
    assert(countBitsSet(1) == 1);
    assert(countBitsSet(0) == 0);
    assert(countBitsSet(int.min) == 1);
    assert(countBitsSet(uint.max) == 32);
}

@safe unittest
{
    import std.meta;
    static foreach (T; AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong))
    {
        assert(countBitsSet(cast(T) 0) == 0);
        assert(countBitsSet(cast(T) 1) == 1);
        assert(countBitsSet(cast(T) 2) == 1);
        assert(countBitsSet(cast(T) 3) == 2);
        assert(countBitsSet(cast(T) 4) == 1);
        assert(countBitsSet(cast(T) 5) == 2);
        assert(countBitsSet(cast(T) 127) == 7);
        static if (isSigned!T)
        {
            assert(countBitsSet(cast(T)-1) == 8 * T.sizeof);
            assert(countBitsSet(T.min) == 1);
        }
        else
        {
            assert(countBitsSet(T.max) == 8 * T.sizeof);
        }
        // Check CTFE compiles.
        static assert(countBitsSet(cast(T) 1) == 1);
    }
    assert(countBitsSet(1_000_000) == 7);
    foreach (i; 0 .. 63)
        assert(countBitsSet(1UL << i) == 1);
}

private struct BitsSet(T)
{
    static assert(T.sizeof <= 8, "bitsSet assumes T is no more than 64-bit.");

@nogc pure nothrow:

    this(T value, size_t startIndex = 0)
    {
        _value = value;
        // Further calculation is only valid and needed when the range is non-empty.
        if (!_value)
            return;

        import core.bitop : bsf;
        immutable trailingZerosCount = bsf(value);
        _value >>>= trailingZerosCount;
        _index = startIndex + trailingZerosCount;
    }

    @property size_t front() const
    {
        return _index;
    }

    @property bool empty() const
    {
        return !_value;
    }

    void popFront()
    {
        assert(_value, "Cannot call popFront on empty range.");

        _value >>>= 1;
        // Further calculation is only valid and needed when the range is non-empty.
        if (!_value)
            return;

        import core.bitop : bsf;
        immutable trailingZerosCount = bsf(_value);
        _value >>>= trailingZerosCount;
        _index += trailingZerosCount + 1;
    }

    @property BitsSet save() const
    {
        return this;
    }

    @property size_t length() const
    {
        return countBitsSet(_value);
    }

    private T _value;
    private size_t _index;
}

/**
Range that iterates the indices of the set bits in `value`.
Index 0 corresponds to the least significant bit.
For signed integers, the highest index corresponds to the sign bit.
*/
auto bitsSet(T)(const T value) @nogc pure nothrow
if (isIntegral!T)
{
    return BitsSet!T(value);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota;

    assert(bitsSet(1).equal([0]));
    assert(bitsSet(5).equal([0, 2]));
    assert(bitsSet(-1).equal(iota(32)));
    assert(bitsSet(int.min).equal([31]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota;

    import std.meta;
    static foreach (T; AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong))
    {
        assert(bitsSet(cast(T) 0).empty);
        assert(bitsSet(cast(T) 1).equal([0]));
        assert(bitsSet(cast(T) 2).equal([1]));
        assert(bitsSet(cast(T) 3).equal([0, 1]));
        assert(bitsSet(cast(T) 4).equal([2]));
        assert(bitsSet(cast(T) 5).equal([0, 2]));
        assert(bitsSet(cast(T) 127).equal(iota(7)));
        static if (isSigned!T)
        {
            assert(bitsSet(cast(T)-1).equal(iota(8 * T.sizeof)));
            assert(bitsSet(T.min).equal([8 * T.sizeof - 1]));
        }
        else
        {
            assert(bitsSet(T.max).equal(iota(8 * T.sizeof)));
        }
    }
    assert(bitsSet(1_000_000).equal([6, 9, 14, 16, 17, 18, 19]));
    foreach (i; 0 .. 63)
        assert(bitsSet(1UL << i).equal([i]));
}
