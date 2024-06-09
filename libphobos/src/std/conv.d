// Written in the D programming language.

/**
A one-stop shop for converting values from one type to another.

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Generic) $(TD
        $(LREF asOriginalType)
        $(LREF castFrom)
        $(LREF parse)
        $(LREF to)
        $(LREF toChars)
))
$(TR $(TD Strings) $(TD
        $(LREF text)
        $(LREF wtext)
        $(LREF dtext)
        $(LREF hexString)
))
$(TR $(TD Numeric) $(TD
        $(LREF octal)
        $(LREF roundTo)
        $(LREF signed)
        $(LREF unsigned)
))
$(TR $(TD Exceptions) $(TD
        $(LREF ConvException)
        $(LREF ConvOverflowException)
))
))

Copyright: Copyright The D Language Foundation 2007-.

License:   $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors:   $(HTTP digitalmars.com, Walter Bright),
           $(HTTP erdani.org, Andrei Alexandrescu),
           Shin Fujishiro,
           Adam D. Ruppe,
           Kenji Hara

Source:    $(PHOBOSSRC std/conv.d)

*/
module std.conv;

public import std.ascii : LetterCase;

import std.meta;
import std.range;
import std.traits;
import std.typecons : Flag, Yes, No, tuple, isTuple;

// Same as std.string.format, but "self-importing".
// Helps reduce code and imports, particularly in static asserts.
// Also helps with missing imports errors.
package template convFormat()
{
    import std.format : format;
    alias convFormat = format;
}

/* ************* Exceptions *************** */

/**
 * Thrown on conversion errors.
 */
class ConvException : Exception
{
    import std.exception : basicExceptionCtors;
    ///
    mixin basicExceptionCtors;
}

///
@safe unittest
{
    import std.exception : assertThrown;
    assertThrown!ConvException(to!int("abc"));
}

private auto convError(S, T)(S source, string fn = __FILE__, size_t ln = __LINE__)
{
    string msg;

    if (source.empty)
        msg = "Unexpected end of input when converting from type " ~ S.stringof ~ " to type " ~ T.stringof;
    else
    {
        ElementType!S el = source.front;

        if (el == '\n')
            msg = text("Unexpected '\\n' when converting from type " ~ S.stringof ~ " to type " ~ T.stringof);
        else
            msg =  text("Unexpected '", el,
                 "' when converting from type " ~ S.stringof ~ " to type " ~ T.stringof);
    }

    return new ConvException(msg, fn, ln);
}

@safe pure/* nothrow*/  // lazy parameter bug
private auto parseError(lazy string msg, string fn = __FILE__, size_t ln = __LINE__)
{
    return new ConvException(text("Can't parse string: ", msg), fn, ln);
}

private void parseCheck(alias source)(dchar c, string fn = __FILE__, size_t ln = __LINE__)
{
    if (source.empty)
        throw parseError(text("unexpected end of input when expecting \"", c, "\""));
    if (source.front != c)
        throw parseError(text("\"", c, "\" is missing"), fn, ln);
    source.popFront();
}

private
{
    T toStr(T, S)(S src)
    if (isSomeString!T)
    {
        // workaround for https://issues.dlang.org/show_bug.cgi?id=14198
        static if (is(S == bool) && is(typeof({ T s = "string"; })))
        {
            return src ? "true" : "false";
        }
        else
        {
            import std.array : appender;
            import std.format.spec : FormatSpec;
            import std.format.write : formatValue;

            auto w = appender!T();
            FormatSpec!(ElementEncodingType!T) f;
            formatValue(w, src, f);
            return w.data;
        }
    }

    template isExactSomeString(T)
    {
        enum isExactSomeString = isSomeString!T && !is(T == enum);
    }

    template isEnumStrToStr(S, T)
    {
        enum isEnumStrToStr = is(S : T) &&
                              is(S == enum) && isExactSomeString!T;
    }
    template isNullToStr(S, T)
    {
        enum isNullToStr = is(S : T) &&
                           (is(immutable S == immutable typeof(null))) && isExactSomeString!T;
    }
}

/**
 * Thrown on conversion overflow errors.
 */
class ConvOverflowException : ConvException
{
    @safe pure nothrow
    this(string s, string fn = __FILE__, size_t ln = __LINE__)
    {
        super(s, fn, ln);
    }
}

///
@safe unittest
{
    import std.exception : assertThrown;
    assertThrown!ConvOverflowException(to!ubyte(1_000_000));
}

/**
The `to` template converts a value from one type _to another.
The source type is deduced and the target type must be specified, for example the
expression `to!int(42.0)` converts the number 42 from
`double` _to `int`. The conversion is "safe", i.e.,
it checks for overflow; `to!int(4.2e10)` would throw the
`ConvOverflowException` exception. Overflow checks are only
inserted when necessary, e.g., `to!double(42)` does not do
any checking because any `int` fits in a `double`.

Conversions from string _to numeric types differ from the C equivalents
`atoi()` and `atol()` by checking for overflow and not allowing whitespace.

For conversion of strings _to signed types, the grammar recognized is:
$(PRE $(I Integer):
    $(I Sign UnsignedInteger)
    $(I UnsignedInteger)
$(I Sign):
    $(B +)
    $(B -))

For conversion _to unsigned types, the grammar recognized is:
$(PRE $(I UnsignedInteger):
    $(I DecimalDigit)
    $(I DecimalDigit) $(I UnsignedInteger))
 */
template to(T)
{
    T to(A...)(A args)
        if (A.length > 0)
    {
        return toImpl!T(args);
    }

    // Fix https://issues.dlang.org/show_bug.cgi?id=6175
    T to(S)(ref S arg)
        if (isStaticArray!S)
    {
        return toImpl!T(arg);
    }

    // Fix https://issues.dlang.org/show_bug.cgi?id=16108
    T to(S)(ref S arg)
        if (isAggregateType!S && !isCopyable!S)
    {
        return toImpl!T(arg);
    }
}

/**
 * Converting a value _to its own type (useful mostly for generic code)
 * simply returns its argument.
 */
@safe pure unittest
{
    int a = 42;
    int b = to!int(a);
    double c = to!double(3.14); // c is double with value 3.14
}

/**
 * Converting among numeric types is a safe way _to cast them around.
 *
 * Conversions from floating-point types _to integral types allow loss of
 * precision (the fractional part of a floating-point number). The
 * conversion is truncating towards zero, the same way a cast would
 * truncate. (_To round a floating point value when casting _to an
 * integral, use `roundTo`.)
 */
@safe pure unittest
{
    import std.exception : assertThrown;

    int a = 420;
    assert(to!long(a) == a);
    assertThrown!ConvOverflowException(to!byte(a));

    assert(to!int(4.2e6) == 4200000);
    assertThrown!ConvOverflowException(to!uint(-3.14));
    assert(to!uint(3.14) == 3);
    assert(to!uint(3.99) == 3);
    assert(to!int(-3.99) == -3);
}

/**
 * When converting strings _to numeric types, note that D hexadecimal and binary
 * literals are not handled. Neither the prefixes that indicate the base, nor the
 * horizontal bar used _to separate groups of digits are recognized. This also
 * applies to the suffixes that indicate the type.
 *
 * _To work around this, you can specify a radix for conversions involving numbers.
 */
@safe pure unittest
{
    auto str = to!string(42, 16);
    assert(str == "2A");
    auto i = to!int(str, 16);
    assert(i == 42);
}

/**
 * Conversions from integral types _to floating-point types always
 * succeed, but might lose accuracy. The largest integers with a
 * predecessor representable in floating-point format are `2^24-1` for
 * `float`, `2^53-1` for `double`, and `2^64-1` for `real` (when
 * `real` is 80-bit, e.g. on Intel machines).
 */
@safe pure unittest
{
    // 2^24 - 1, largest proper integer representable as float
    int a = 16_777_215;
    assert(to!int(to!float(a)) == a);
    assert(to!int(to!float(-a)) == -a);
}

/**
   Conversion from string types to char types enforces the input
   to consist of a single code point, and said code point must
   fit in the target type. Otherwise, $(LREF ConvException) is thrown.
 */
@safe pure unittest
{
    import std.exception : assertThrown;

    assert(to!char("a") == 'a');
    assertThrown(to!char("ñ")); // 'ñ' does not fit into a char
    assert(to!wchar("ñ") == 'ñ');
    assertThrown(to!wchar("😃")); // '😃' does not fit into a wchar
    assert(to!dchar("😃") == '😃');

    // Using wstring or dstring as source type does not affect the result
    assert(to!char("a"w) == 'a');
    assert(to!char("a"d) == 'a');

    // Two code points cannot be converted to a single one
    assertThrown(to!char("ab"));
}

/**
 * Converting an array _to another array type works by converting each
 * element in turn. Associative arrays can be converted _to associative
 * arrays as long as keys and values can in turn be converted.
 */
@safe pure unittest
{
    import std.string : split;

    int[] a = [1, 2, 3];
    auto b = to!(float[])(a);
    assert(b == [1.0f, 2, 3]);
    string str = "1 2 3 4 5 6";
    auto numbers = to!(double[])(split(str));
    assert(numbers == [1.0, 2, 3, 4, 5, 6]);
    int[string] c;
    c["a"] = 1;
    c["b"] = 2;
    auto d = to!(double[wstring])(c);
    assert(d["a"w] == 1 && d["b"w] == 2);
}

/**
 * Conversions operate transitively, meaning that they work on arrays and
 * associative arrays of any complexity.
 *
 * This conversion works because `to!short` applies _to an `int`, `to!wstring`
 * applies _to a `string`, `to!string` applies _to a `double`, and
 * `to!(double[])` applies _to an `int[]`. The conversion might throw an
 * exception because `to!short` might fail the range check.
 */
@safe unittest
{
    int[string][double[int[]]] a;
    auto b = to!(short[wstring][string[double[]]])(a);
}

/**
 * Object-to-object conversions by dynamic casting throw exception when
 * the source is non-null and the target is null.
 */
@safe pure unittest
{
    import std.exception : assertThrown;
    // Testing object conversions
    class A {}
    class B : A {}
    class C : A {}
    A a1 = new A, a2 = new B, a3 = new C;
    assert(to!B(a2) is a2);
    assert(to!C(a3) is a3);
    assertThrown!ConvException(to!B(a3));
}

/**
 * Stringize conversion from all types is supported.
 * $(UL
 *   $(LI String _to string conversion works for any two string types having
 *        (`char`, `wchar`, `dchar`) character widths and any
 *        combination of qualifiers (mutable, `const`, or `immutable`).)
 *   $(LI Converts array (other than strings) _to string.
 *        Each element is converted by calling `to!T`.)
 *   $(LI Associative array _to string conversion.
 *        Each element is converted by calling `to!T`.)
 *   $(LI Object _to string conversion calls `toString` against the object or
 *        returns `"null"` if the object is null.)
 *   $(LI Struct _to string conversion calls `toString` against the struct if
 *        it is defined.)
 *   $(LI For structs that do not define `toString`, the conversion _to string
 *        produces the list of fields.)
 *   $(LI Enumerated types are converted _to strings as their symbolic names.)
 *   $(LI Boolean values are converted to `"true"` or `"false"`.)
 *   $(LI `char`, `wchar`, `dchar` _to a string type.)
 *   $(LI Unsigned or signed integers _to strings.
 *        $(DL $(DT [special case])
 *             $(DD Convert integral value _to string in $(D_PARAM radix) radix.
 *             radix must be a value from 2 to 36.
 *             value is treated as a signed value only if radix is 10.
 *             The characters A through Z are used to represent values 10 through 36
 *             and their case is determined by the $(D_PARAM letterCase) parameter.)))
 *   $(LI All floating point types _to all string types.)
 *   $(LI Pointer to string conversions convert the pointer to a `size_t` value.
 *        If pointer is `char*`, treat it as C-style strings.
 *        In that case, this function is `@system`.))
 * See $(REF formatValue, std,format) on how `toString` should be defined.
 */
@system pure unittest // @system due to cast and ptr
{
    // Conversion representing dynamic/static array with string
    long[] a = [ 1, 3, 5 ];
    assert(to!string(a) == "[1, 3, 5]");

    // Conversion representing associative array with string
    int[string] associativeArray = ["0":1, "1":2];
    assert(to!string(associativeArray) == `["0":1, "1":2]` ||
           to!string(associativeArray) == `["1":2, "0":1]`);

    // char* to string conversion
    assert(to!string(cast(char*) null) == "");
    assert(to!string("foo\0".ptr) == "foo");

    // Conversion reinterpreting void array to string
    auto w = "abcx"w;
    const(void)[] b = w;
    assert(b.length == 8);

    auto c = to!(wchar[])(b);
    assert(c == "abcx");
}

/**
 * Strings can be converted to enum types. The enum member with the same name as the
 * input string is returned. The comparison is case-sensitive.
 *
 * A $(LREF ConvException) is thrown if the enum does not have the specified member.
 */
@safe pure unittest
{
    import std.exception : assertThrown;

    enum E { a, b, c }
    assert(to!E("a") == E.a);
    assert(to!E("b") == E.b);
    assertThrown!ConvException(to!E("A"));
}

// Tests for https://issues.dlang.org/show_bug.cgi?id=6175
@safe pure nothrow unittest
{
    char[9] sarr = "blablabla";
    auto darr = to!(char[])(sarr);
    assert(sarr.ptr == darr.ptr);
    assert(sarr.length == darr.length);
}

// Tests for https://issues.dlang.org/show_bug.cgi?id=7348
@safe pure /+nothrow+/ unittest
{
    assert(to!string(null) == "null");
    assert(text(null) == "null");
}

// Test `scope` inference of parameters of `text`
@safe unittest
{
    static struct S
    {
        int* x; // make S a type with pointers
        string toString() const scope
        {
            return "S";
        }
    }
    scope S s;
    assert(text("a", s) == "aS");
}

// Tests for https://issues.dlang.org/show_bug.cgi?id=11390
@safe pure /+nothrow+/ unittest
{
    const(typeof(null)) ctn;
    immutable(typeof(null)) itn;
    assert(to!string(ctn) == "null");
    assert(to!string(itn) == "null");
}

// Tests for https://issues.dlang.org/show_bug.cgi?id=8729: do NOT skip leading WS
@safe pure unittest
{
    import std.exception;
    static foreach (T; AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong))
    {
        assertThrown!ConvException(to!T(" 0"));
        assertThrown!ConvException(to!T(" 0", 8));
    }
    static foreach (T; AliasSeq!(float, double, real))
    {
        assertThrown!ConvException(to!T(" 0"));
    }

    assertThrown!ConvException(to!bool(" true"));

    alias NullType = typeof(null);
    assertThrown!ConvException(to!NullType(" null"));

    alias ARR = int[];
    assertThrown!ConvException(to!ARR(" [1]"));

    alias AA = int[int];
    assertThrown!ConvException(to!AA(" [1:1]"));
}

// https://issues.dlang.org/show_bug.cgi?id=20623
@safe pure nothrow unittest
{
    // static class C
    // {
    //     override string toString() const
    //     {
    //         return "C()";
    //     }
    // }

    static struct S
    {
        bool b;
        int i;
        float f;
        int[] a;
        int[int] aa;
        S* p;
        // C c; // TODO: Fails because of hasToString

        void fun() inout
        {
            static foreach (const idx; 0 .. this.tupleof.length)
            {
                {
                    const _ = this.tupleof[idx].to!string();
                }
            }
        }
    }
}

/**
If the source type is implicitly convertible to the target type, $(D
to) simply performs the implicit conversion.
 */
private T toImpl(T, S)(S value)
if (is(S : T) &&
    !isEnumStrToStr!(S, T) && !isNullToStr!(S, T))
{
    template isSignedInt(T)
    {
        enum isSignedInt = isIntegral!T && isSigned!T;
    }
    alias isUnsignedInt = isUnsigned;

    // Conversion from integer to integer, and changing its sign
    static if (isUnsignedInt!S && isSignedInt!T && S.sizeof == T.sizeof)
    {   // unsigned to signed & same size
        import std.exception : enforce;
        enforce(value <= cast(S) T.max,
                new ConvOverflowException("Conversion positive overflow"));
    }
    else static if (isSignedInt!S && isUnsignedInt!T)
    {   // signed to unsigned
        import std.exception : enforce;
        enforce(0 <= value,
                new ConvOverflowException("Conversion negative overflow"));
    }

    return value;
}

// https://issues.dlang.org/show_bug.cgi?id=9523: Allow identity enum conversion
@safe pure nothrow unittest
{
    enum E { a }
    auto e = to!E(E.a);
    assert(e == E.a);
}

@safe pure nothrow unittest
{
    int a = 42;
    auto b = to!long(a);
    assert(a == b);
}

// https://issues.dlang.org/show_bug.cgi?id=6377
@safe pure unittest
{
    import std.exception;
    // Conversion between same size
    static foreach (S; AliasSeq!(byte, short, int, long))
    {{
        alias U = Unsigned!S;

        static foreach (Sint; AliasSeq!(S, const S, immutable S))
        static foreach (Uint; AliasSeq!(U, const U, immutable U))
        {{
            // positive overflow
            Uint un = Uint.max;
            assertThrown!ConvOverflowException(to!Sint(un),
                text(Sint.stringof, ' ', Uint.stringof, ' ', un));

            // negative overflow
            Sint sn = -1;
            assertThrown!ConvOverflowException(to!Uint(sn),
                text(Sint.stringof, ' ', Uint.stringof, ' ', un));
        }}
    }}

    // Conversion between different size
    static foreach (i, S1; AliasSeq!(byte, short, int, long))
    static foreach (   S2; AliasSeq!(byte, short, int, long)[i+1..$])
    {{
        alias U1 = Unsigned!S1;
        alias U2 = Unsigned!S2;

        static assert(U1.sizeof < S2.sizeof);

        // small unsigned to big signed
        static foreach (Uint; AliasSeq!(U1, const U1, immutable U1))
        static foreach (Sint; AliasSeq!(S2, const S2, immutable S2))
        {{
            Uint un = Uint.max;
            assertNotThrown(to!Sint(un));
            assert(to!Sint(un) == un);
        }}

        // big unsigned to small signed
        static foreach (Uint; AliasSeq!(U2, const U2, immutable U2))
        static foreach (Sint; AliasSeq!(S1, const S1, immutable S1))
        {{
            Uint un = Uint.max;
            assertThrown(to!Sint(un));
        }}

        static assert(S1.sizeof < U2.sizeof);

        // small signed to big unsigned
        static foreach (Sint; AliasSeq!(S1, const S1, immutable S1))
        static foreach (Uint; AliasSeq!(U2, const U2, immutable U2))
        {{
            Sint sn = -1;
            assertThrown!ConvOverflowException(to!Uint(sn));
        }}

        // big signed to small unsigned
        static foreach (Sint; AliasSeq!(S2, const S2, immutable S2))
        static foreach (Uint; AliasSeq!(U1, const U1, immutable U1))
        {{
            Sint sn = -1;
            assertThrown!ConvOverflowException(to!Uint(sn));
        }}
    }}
}

// https://issues.dlang.org/show_bug.cgi?id=13551
private T toImpl(T, S)(S value)
if (isTuple!T)
{
    T t;
    static foreach (i; 0 .. T.length)
    {
        t[i] = value[i].to!(typeof(T[i]));
    }
    return t;
}

@safe unittest
{
    import std.typecons : Tuple;

    auto test = ["10", "20", "30"];
    assert(test.to!(Tuple!(int, int, int)) == Tuple!(int, int, int)(10, 20, 30));

    auto test1 = [1, 2];
    assert(test1.to!(Tuple!(int, int)) == Tuple!(int, int)(1, 2));

    auto test2 = [1.0, 2.0, 3.0];
    assert(test2.to!(Tuple!(int, int, int)) == Tuple!(int, int, int)(1, 2, 3));
}

/*
  Converting static arrays forwards to their dynamic counterparts.
 */
private T toImpl(T, S)(ref S s)
if (isStaticArray!S)
{
    return toImpl!(T, typeof(s[0])[])(s);
}

@safe pure nothrow unittest
{
    char[4] test = ['a', 'b', 'c', 'd'];
    static assert(!isInputRange!(Unqual!(char[4])));
    assert(to!string(test) == test);
}

/**
When source type supports member template function opCast, it is used.
*/
private T toImpl(T, S)(S value)
if (!is(S : T) &&
    is(typeof(S.init.opCast!T()) : T) &&
    !isExactSomeString!T &&
    !is(typeof(T(value))))
{
    return value.opCast!T();
}

@safe pure unittest
{
    static struct Test
    {
        struct T
        {
            this(S s) @safe pure { }
        }
        struct S
        {
            T opCast(U)() @safe pure { assert(false); }
        }
    }
    cast(void) to!(Test.T)(Test.S());

    // make sure std.conv.to is doing the same thing as initialization
    Test.S s;
    Test.T t = s;
}

@safe pure unittest
{
    class B
    {
        T opCast(T)() { return 43; }
    }
    auto b = new B;
    assert(to!int(b) == 43);

    struct S
    {
        T opCast(T)() { return 43; }
    }
    auto s = S();
    assert(to!int(s) == 43);
}

/**
When target type supports 'converting construction', it is used.
$(UL $(LI If target type is struct, `T(value)` is used.)
     $(LI If target type is class, $(D new T(value)) is used.))
*/
private T toImpl(T, S)(S value)
if (!is(S : T) &&
    is(T == struct) && is(typeof(T(value))))
{
    return T(value);
}

// https://issues.dlang.org/show_bug.cgi?id=3961
@safe pure unittest
{
    struct Int
    {
        int x;
    }
    Int i = to!Int(1);

    static struct Int2
    {
        int x;
        this(int x) @safe pure { this.x = x; }
    }
    Int2 i2 = to!Int2(1);

    static struct Int3
    {
        int x;
        static Int3 opCall(int x) @safe pure
        {
            Int3 i;
            i.x = x;
            return i;
        }
    }
    Int3 i3 = to!Int3(1);
}

// https://issues.dlang.org/show_bug.cgi?id=6808
@safe pure unittest
{
    static struct FakeBigInt
    {
        this(string s) @safe pure {}
    }

    string s = "101";
    auto i3 = to!FakeBigInt(s);
}

/// ditto
private T toImpl(T, S)(S value)
if (!is(S : T) &&
    is(T == class) && is(typeof(new T(value))))
{
    return new T(value);
}

@safe pure unittest
{
    static struct S
    {
        int x;
    }
    static class C
    {
        int x;
        this(int x) @safe pure { this.x = x; }
    }

    static class B
    {
        int value;
        this(S src) @safe pure { value = src.x; }
        this(C src) @safe pure { value = src.x; }
    }

    S s = S(1);
    auto b1 = to!B(s);  // == new B(s)
    assert(b1.value == 1);

    C c = new C(2);
    auto b2 = to!B(c);  // == new B(c)
    assert(b2.value == 2);

    auto c2 = to!C(3);   // == new C(3)
    assert(c2.x == 3);
}

@safe pure unittest
{
    struct S
    {
        class A
        {
            this(B b) @safe pure {}
        }
        class B : A
        {
            this() @safe pure { super(this); }
        }
    }

    S.B b = new S.B();
    S.A a = to!(S.A)(b);      // == cast(S.A) b
                              // (do not run construction conversion like new S.A(b))
    assert(b is a);

    static class C : Object
    {
        this() @safe pure {}
        this(Object o) @safe pure {}
    }

    Object oc = new C();
    C a2 = to!C(oc);    // == new C(a)
                        // Construction conversion overrides down-casting conversion
    assert(a2 !is a);   //
}

/**
Object-to-object conversions by dynamic casting throw exception when the source is
non-null and the target is null.
 */
private T toImpl(T, S)(S value)
if (!is(S : T) &&
    (is(S == class) || is(S == interface)) && !is(typeof(value.opCast!T()) : T) &&
    (is(T == class) || is(T == interface)) && !is(typeof(new T(value))))
{
    static if (is(T == immutable))
    {
            // immutable <- immutable
            enum isModConvertible = is(S == immutable);
    }
    else static if (is(T == const))
    {
        static if (is(T == shared))
        {
            // shared const <- shared
            // shared const <- shared const
            // shared const <- immutable
            enum isModConvertible = is(S == shared) || is(S == immutable);
        }
        else
        {
            // const <- mutable
            // const <- immutable
            enum isModConvertible = !is(S == shared);
        }
    }
    else
    {
        static if (is(T == shared))
        {
            // shared <- shared mutable
            enum isModConvertible = is(S == shared) && !is(S == const);
        }
        else
        {
            // (mutable) <- (mutable)
            enum isModConvertible = is(Unqual!S == S);
        }
    }
    static assert(isModConvertible, "Bad modifier conversion: "~S.stringof~" to "~T.stringof);

    auto result = ()@trusted{ return cast(T) value; }();
    if (!result && value)
    {
        throw new ConvException("Cannot convert object of static type "
                ~S.classinfo.name~" and dynamic type "~value.classinfo.name
                ~" to type "~T.classinfo.name);
    }
    return result;
}

// Unittest for 6288
@safe pure unittest
{
    import std.exception;

    alias Identity(T)      =              T;
    alias toConst(T)       =        const T;
    alias toShared(T)      =       shared T;
    alias toSharedConst(T) = shared const T;
    alias toImmutable(T)   =    immutable T;
    template AddModifier(int n)
    if (0 <= n && n < 5)
    {
             static if (n == 0) alias AddModifier = Identity;
        else static if (n == 1) alias AddModifier = toConst;
        else static if (n == 2) alias AddModifier = toShared;
        else static if (n == 3) alias AddModifier = toSharedConst;
        else static if (n == 4) alias AddModifier = toImmutable;
    }

    interface I {}
    interface J {}

    class A {}
    class B : A {}
    class C : B, I, J {}
    class D : I {}

    static foreach (m1; 0 .. 5) // enumerate modifiers
    static foreach (m2; 0 .. 5) // ditto
    {{
        alias srcmod = AddModifier!m1;
        alias tgtmod = AddModifier!m2;

        // Compile time convertible equals to modifier convertible.
        static if (is(srcmod!Object : tgtmod!Object))
        {
            // Test runtime conversions: class to class, class to interface,
            // interface to class, and interface to interface

            // Check that the runtime conversion to succeed
            srcmod!A ac = new srcmod!C();
            srcmod!I ic = new srcmod!C();
            assert(to!(tgtmod!C)(ac) !is null); // A(c) to C
            assert(to!(tgtmod!I)(ac) !is null); // A(c) to I
            assert(to!(tgtmod!C)(ic) !is null); // I(c) to C
            assert(to!(tgtmod!J)(ic) !is null); // I(c) to J

            // Check that the runtime conversion fails
            srcmod!A ab = new srcmod!B();
            srcmod!I id = new srcmod!D();
            assertThrown(to!(tgtmod!C)(ab));    // A(b) to C
            assertThrown(to!(tgtmod!I)(ab));    // A(b) to I
            assertThrown(to!(tgtmod!C)(id));    // I(d) to C
            assertThrown(to!(tgtmod!J)(id));    // I(d) to J
        }
        else
        {
            // Check that the conversion is rejected statically
            static assert(!is(typeof(to!(tgtmod!C)(srcmod!A.init))));   // A to C
            static assert(!is(typeof(to!(tgtmod!I)(srcmod!A.init))));   // A to I
            static assert(!is(typeof(to!(tgtmod!C)(srcmod!I.init))));   // I to C
            static assert(!is(typeof(to!(tgtmod!J)(srcmod!I.init))));   // I to J
        }
    }}
}

/**
Handles type _to string conversions
*/
private T toImpl(T, S)(S value)
if (!(is(S : T) &&
    !isEnumStrToStr!(S, T) && !isNullToStr!(S, T)) &&
    !isInfinite!S && isExactSomeString!T)
{
    static if (isExactSomeString!S && value[0].sizeof == ElementEncodingType!T.sizeof)
    {
        // string-to-string with incompatible qualifier conversion
        static if (is(ElementEncodingType!T == immutable))
        {
            // conversion (mutable|const) -> immutable
            return value.idup;
        }
        else
        {
            // conversion (immutable|const) -> mutable
            return value.dup;
        }
    }
    else static if (isExactSomeString!S)
    {
        import std.array : appender;
        // other string-to-string
        //Use Appender directly instead of toStr, which also uses a formatedWrite
        auto w = appender!T();
        w.put(value);
        return w.data;
    }
    else static if (isIntegral!S && !is(S == enum))
    {
        // other integral-to-string conversions with default radix

        import core.internal.string : signedToTempString, unsignedToTempString;

        alias EEType = Unqual!(ElementEncodingType!T);
        EEType[long.sizeof * 3 + 1] buf = void;
        EEType[] t = isSigned!S
            ?   signedToTempString!(10, false, EEType)(value, buf)
            : unsignedToTempString!(10, false, EEType)(value, buf);
        return t.dup;
    }
    else static if (is(S == void[]) || is(S == const(void)[]) || is(S == immutable(void)[]))
    {
        import core.stdc.string : memcpy;
        import std.exception : enforce;
        // Converting void array to string
        alias Char = Unqual!(ElementEncodingType!T);
        auto raw = cast(const(ubyte)[]) value;
        enforce(raw.length % Char.sizeof == 0,
                new ConvException("Alignment mismatch in converting a "
                        ~ S.stringof ~ " to a "
                        ~ T.stringof));
        auto result = new Char[raw.length / Char.sizeof];
        ()@trusted{ memcpy(result.ptr, value.ptr, value.length); }();
        return cast(T) result;
    }
    else static if (isPointer!S && isSomeChar!(PointerTarget!S))
    {
        // This is unsafe because we cannot guarantee that the pointer is null terminated.
        return () @system {
            static if (is(S : const(char)*))
                import core.stdc.string : strlen;
            else
                size_t strlen(S s) nothrow
                {
                    S p = s;
                    while (*p++) {}
                    return p-s-1;
                }
            return toImpl!T(value ? value[0 .. strlen(value)].dup : null);
        }();
    }
    else static if (isSomeString!T && is(S == enum))
    {
        static if (isSwitchable!(OriginalType!S) && EnumMembers!S.length <= 50)
        {
            switch (value)
            {
                foreach (member; NoDuplicates!(EnumMembers!S))
                {
                    case member:
                        return to!T(enumRep!(immutable(T), S, member));
                }
                default:
            }
        }
        else
        {
            foreach (member; EnumMembers!S)
            {
                if (value == member)
                    return to!T(enumRep!(immutable(T), S, member));
            }
        }

        import std.array : appender;
        import std.format.spec : FormatSpec;
        import std.format.write : formatValue;

        //Default case, delegate to format
        //Note: we don't call toStr directly, to avoid duplicate work.
        auto app = appender!T();
        app.put("cast(" ~ S.stringof ~ ")");
        FormatSpec!char f;
        formatValue(app, cast(OriginalType!S) value, f);
        return app.data;
    }
    else
    {
        // other non-string values runs formatting
        return toStr!T(value);
    }
}

// https://issues.dlang.org/show_bug.cgi?id=14042
@system unittest
{
    immutable(char)* ptr = "hello".ptr;
    auto result = ptr.to!(char[]);
}
// https://issues.dlang.org/show_bug.cgi?id=8384
@system unittest
{
    void test1(T)(T lp, string cmp)
    {
        static foreach (e; AliasSeq!(char, wchar, dchar))
        {
            test2!(e[])(lp, cmp);
            test2!(const(e)[])(lp, cmp);
            test2!(immutable(e)[])(lp, cmp);
        }
    }

    void test2(D, S)(S lp, string cmp)
    {
        assert(to!string(to!D(lp)) == cmp);
    }

    static foreach (e; AliasSeq!("Hello, world!", "Hello, world!"w, "Hello, world!"d))
    {
        test1(e, "Hello, world!");
        test1(e.ptr, "Hello, world!");
    }
    static foreach (e; AliasSeq!("", ""w, ""d))
    {
        test1(e, "");
        test1(e.ptr, "");
    }
}

/*
    To string conversion for non copy-able structs
 */
private T toImpl(T, S)(ref S value)
if (!(is(S : T) &&
    !isEnumStrToStr!(S, T) && !isNullToStr!(S, T)) &&
    !isInfinite!S && isExactSomeString!T && !isCopyable!S && !isStaticArray!S)
{
    import std.array : appender;
    import std.format.spec : FormatSpec;
    import std.format.write : formatValue;

    auto w = appender!T();
    FormatSpec!(ElementEncodingType!T) f;
    formatValue(w, value, f);
    return w.data;
}

// https://issues.dlang.org/show_bug.cgi?id=16108
@safe unittest
{
    static struct A
    {
        int val;
        bool flag;

        string toString() { return text(val, ":", flag); }

        @disable this(this);
    }

    auto a = A();
    assert(to!string(a) == "0:false");

    static struct B
    {
        int val;
        bool flag;

        @disable this(this);
    }

    auto b = B();
    assert(to!string(b) == "B(0, false)");
}

// https://issues.dlang.org/show_bug.cgi?id=20070
@safe unittest
{
    void writeThem(T)(ref inout(T) them)
    {
        assert(them.to!string == "[1, 2, 3, 4]");
    }

    const(uint)[4] vals = [ 1, 2, 3, 4 ];
    writeThem(vals);
}

/*
    Check whether type `T` can be used in a switch statement.
    This is useful for compile-time generation of switch case statements.
*/
private template isSwitchable(E)
{
    enum bool isSwitchable = is(typeof({
        switch (E.init) { default: }
    }));
}

//
@safe unittest
{
    static assert(isSwitchable!int);
    static assert(!isSwitchable!double);
    static assert(!isSwitchable!real);
}

//Static representation of the index I of the enum S,
//In representation T.
//T must be an immutable string (avoids un-necessary initializations).
private template enumRep(T, S, S value)
if (is (T == immutable) && isExactSomeString!T && is(S == enum))
{
    static T enumRep = toStr!T(value);
}

@safe pure unittest
{
    import std.exception;
    void dg()
    {
        // string to string conversion
        alias Chars = AliasSeq!(char, wchar, dchar);
        foreach (LhsC; Chars)
        {
            alias LhStrings = AliasSeq!(LhsC[], const(LhsC)[], immutable(LhsC)[]);
            foreach (Lhs; LhStrings)
            {
                foreach (RhsC; Chars)
                {
                    alias RhStrings = AliasSeq!(RhsC[], const(RhsC)[], immutable(RhsC)[]);
                    foreach (Rhs; RhStrings)
                    {
                        Lhs s1 = to!Lhs("wyda");
                        Rhs s2 = to!Rhs(s1);
                        //writeln(Lhs.stringof, " -> ", Rhs.stringof);
                        assert(s1 == to!Lhs(s2));
                    }
                }
            }
        }

        foreach (T; Chars)
        {
            foreach (U; Chars)
            {
                T[] s1 = to!(T[])("Hello, world!");
                auto s2 = to!(U[])(s1);
                assert(s1 == to!(T[])(s2));
                auto s3 = to!(const(U)[])(s1);
                assert(s1 == to!(T[])(s3));
                auto s4 = to!(immutable(U)[])(s1);
                assert(s1 == to!(T[])(s4));
            }
        }
    }
    dg();
    assertCTFEable!dg;
}

@safe pure unittest
{
    // Conversion representing bool value with string
    bool b;
    assert(to!string(b) == "false");
    b = true;
    assert(to!string(b) == "true");
}

@safe pure unittest
{
    // Conversion representing character value with string
    alias AllChars =
        AliasSeq!( char, const( char), immutable( char),
                  wchar, const(wchar), immutable(wchar),
                  dchar, const(dchar), immutable(dchar));
    foreach (Char1; AllChars)
    {
        foreach (Char2; AllChars)
        {
            Char1 c = 'a';
            assert(to!(Char2[])(c)[0] == c);
        }
        uint x = 4;
        assert(to!(Char1[])(x) == "4");
    }

    string s = "foo";
    string s2;
    foreach (char c; s)
    {
        s2 ~= to!string(c);
    }
    assert(s2 == "foo");
}

@safe pure nothrow unittest
{
    import std.exception;
    // Conversion representing integer values with string

    static foreach (Int; AliasSeq!(ubyte, ushort, uint, ulong))
    {
        assert(to!string(Int(0)) == "0");
        assert(to!string(Int(9)) == "9");
        assert(to!string(Int(123)) == "123");
    }

    static foreach (Int; AliasSeq!(byte, short, int, long))
    {
        assert(to!string(Int(0)) == "0");
        assert(to!string(Int(9)) == "9");
        assert(to!string(Int(123)) == "123");
        assert(to!string(Int(-0)) == "0");
        assert(to!string(Int(-9)) == "-9");
        assert(to!string(Int(-123)) == "-123");
        assert(to!string(const(Int)(6)) == "6");
    }

    assert(wtext(int.max) == "2147483647"w);
    assert(wtext(int.min) == "-2147483648"w);
    assert(to!string(0L) == "0");

    assertCTFEable!(
    {
        assert(to!string(1uL << 62) == "4611686018427387904");
        assert(to!string(0x100000000) == "4294967296");
        assert(to!string(-138L) == "-138");
    });
}

@safe unittest // sprintf issue
{
    double[2] a = [ 1.5, 2.5 ];
    assert(to!string(a) == "[1.5, 2.5]");
}

@safe unittest
{
    // Conversion representing class object with string
    class A
    {
        override string toString() @safe const { return "an A"; }
    }
    A a;
    assert(to!string(a) == "null");
    a = new A;
    assert(to!string(a) == "an A");

    // https://issues.dlang.org/show_bug.cgi?id=7660
    class C { override string toString() @safe const { return "C"; } }
    struct S { C c; alias c this; }
    S s; s.c = new C();
    assert(to!string(s) == "C");
}

@safe unittest
{
    // Conversion representing struct object with string
    struct S1
    {
        string toString() { return "wyda"; }
    }
    assert(to!string(S1()) == "wyda");

    struct S2
    {
        int a = 42;
        float b = 43.5;
    }
    S2 s2;
    assert(to!string(s2) == "S2(42, 43.5)");

    // Test for https://issues.dlang.org/show_bug.cgi?id=8080
    struct S8080
    {
        short[4] data;
        alias data this;
        string toString() { return "<S>"; }
    }
    S8080 s8080;
    assert(to!string(s8080) == "<S>");
}

@safe unittest
{
    // Conversion representing enum value with string
    enum EB : bool { a = true }
    enum EU : uint { a = 0, b = 1, c = 2 }  // base type is unsigned
    // base type is signed (https://issues.dlang.org/show_bug.cgi?id=7909)
    enum EI : int { a = -1, b = 0, c = 1 }
    enum EF : real { a = 1.414, b = 1.732, c = 2.236 }
    enum EC : char { a = 'x', b = 'y' }
    enum ES : string { a = "aaa", b = "bbb" }

    static foreach (E; AliasSeq!(EB, EU, EI, EF, EC, ES))
    {
        assert(to! string(E.a) == "a"c);
        assert(to!wstring(E.a) == "a"w);
        assert(to!dstring(E.a) == "a"d);
    }

    // Test an value not corresponding to an enum member.
    auto o = cast(EU) 5;
    assert(to! string(o) == "cast(EU)5"c);
    assert(to!wstring(o) == "cast(EU)5"w);
    assert(to!dstring(o) == "cast(EU)5"d);
}

@safe unittest
{
    enum E
    {
        foo,
        doo = foo, // check duplicate switch statements
        bar,
    }

    //Test regression 12494
    assert(to!string(E.foo) == "foo");
    assert(to!string(E.doo) == "foo");
    assert(to!string(E.bar) == "bar");

    static foreach (S; AliasSeq!(string, wstring, dstring, const(char[]), const(wchar[]), const(dchar[])))
    {{
        auto s1 = to!S(E.foo);
        auto s2 = to!S(E.foo);
        assert(s1 == s2);
        // ensure we don't allocate when it's unnecessary
        assert(s1 is s2);
    }}

    static foreach (S; AliasSeq!(char[], wchar[], dchar[]))
    {{
        auto s1 = to!S(E.foo);
        auto s2 = to!S(E.foo);
        assert(s1 == s2);
        // ensure each mutable array is unique
        assert(s1 !is s2);
    }}
}

// ditto
@trusted pure private T toImpl(T, S)(S value, uint radix, LetterCase letterCase = LetterCase.upper)
if (isIntegral!S &&
    isExactSomeString!T)
in
{
    assert(radix >= 2 && radix <= 36, "radix must be in range [2,36]");
}
do
{
    alias EEType = Unqual!(ElementEncodingType!T);

    T toStringRadixConvert(size_t bufLen)(uint runtimeRadix = 0)
    {
        Unsigned!(Unqual!S) div = void, mValue = unsigned(value);

        size_t index = bufLen;
        EEType[bufLen] buffer = void;
        char baseChar = letterCase == LetterCase.lower ? 'a' : 'A';
        char mod = void;

        do
        {
            div = cast(S)(mValue / runtimeRadix );
            mod = cast(ubyte)(mValue % runtimeRadix);
            mod += mod < 10 ? '0' : baseChar - 10;
            buffer[--index] = cast(char) mod;
            mValue = div;
        } while (mValue);

        return cast(T) buffer[index .. $].dup;
    }

    import std.array : array;
    switch (radix)
    {
        case 10:
            // The (value+0) is so integral promotions happen to the type
            return toChars!(10, EEType)(value + 0).array;
        case 16:
            // The unsigned(unsigned(value)+0) is so unsigned integral promotions happen to the type
            if (letterCase == letterCase.upper)
                return toChars!(16, EEType, LetterCase.upper)(unsigned(unsigned(value) + 0)).array;
            else
                return toChars!(16, EEType, LetterCase.lower)(unsigned(unsigned(value) + 0)).array;
        case 2:
            return toChars!(2, EEType)(unsigned(unsigned(value) + 0)).array;
        case 8:
            return toChars!(8, EEType)(unsigned(unsigned(value) + 0)).array;

        default:
            return toStringRadixConvert!(S.sizeof * 6)(radix);
    }
}

@safe pure nothrow unittest
{
    static foreach (Int; AliasSeq!(uint, ulong))
    {
        assert(to!string(Int(16), 16) == "10");
        assert(to!string(Int(15), 2u) == "1111");
        assert(to!string(Int(1), 2u) == "1");
        assert(to!string(Int(0x1234AF), 16u) == "1234AF");
        assert(to!string(Int(0x1234BCD), 16u, LetterCase.upper) == "1234BCD");
        assert(to!string(Int(0x1234AF), 16u, LetterCase.lower) == "1234af");
    }

    static foreach (Int; AliasSeq!(int, long))
    {
        assert(to!string(Int(-10), 10u) == "-10");
    }

    assert(to!string(byte(-10), 16) == "F6");
    assert(to!string(long.min) == "-9223372036854775808");
    assert(to!string(long.max) == "9223372036854775807");
}

/**
Narrowing numeric-numeric conversions throw when the value does not
fit in the narrower type.
 */
private T toImpl(T, S)(S value)
if (!is(S : T) &&
    (isNumeric!S || isSomeChar!S || isBoolean!S) &&
    (isNumeric!T || isSomeChar!T || isBoolean!T) && !is(T == enum))
{
    static if (isFloatingPoint!S && isIntegral!T)
    {
        import std.math.traits : isNaN;
        if (value.isNaN) throw new ConvException("Input was NaN");
    }

    enum sSmallest = mostNegative!S;
    enum tSmallest = mostNegative!T;
    static if (sSmallest < 0)
    {
        // possible underflow converting from a signed
        static if (tSmallest == 0)
        {
            immutable good = value >= 0;
        }
        else
        {
            static assert(tSmallest < 0,
                "minimum value of T must be smaller than 0");
            immutable good = value >= tSmallest;
        }
        if (!good)
            throw new ConvOverflowException("Conversion negative overflow");
    }
    static if (S.max > T.max)
    {
        // possible overflow
        if (value > T.max)
            throw new ConvOverflowException("Conversion positive overflow");
    }
    return (ref value)@trusted{ return cast(T) value; }(value);
}

@safe pure unittest
{
    import std.exception;

    dchar a = ' ';
    assert(to!char(a) == ' ');
    a = 300;
    assert(collectException(to!char(a)));

    dchar from0 = 'A';
    char to0 = to!char(from0);

    wchar from1 = 'A';
    char to1 = to!char(from1);

    char from2 = 'A';
    char to2 = to!char(from2);

    char from3 = 'A';
    wchar to3 = to!wchar(from3);

    char from4 = 'A';
    dchar to4 = to!dchar(from4);
}

@safe unittest
{
    import std.exception;

    // Narrowing conversions from enum -> integral should be allowed, but they
    // should throw at runtime if the enum value doesn't fit in the target
    // type.
    enum E1 : ulong { A = 1, B = 1UL << 48, C = 0 }
    assert(to!int(E1.A) == 1);
    assert(to!bool(E1.A) == true);
    assertThrown!ConvOverflowException(to!int(E1.B)); // E1.B overflows int
    assertThrown!ConvOverflowException(to!bool(E1.B)); // E1.B overflows bool
    assert(to!bool(E1.C) == false);

    enum E2 : long { A = -1L << 48, B = -1 << 31, C = 1 << 31 }
    assertThrown!ConvOverflowException(to!int(E2.A)); // E2.A overflows int
    assertThrown!ConvOverflowException(to!uint(E2.B)); // E2.B overflows uint
    assert(to!int(E2.B) == -1 << 31); // but does not overflow int
    assert(to!int(E2.C) == 1 << 31);  // E2.C does not overflow int

    enum E3 : int { A = -1, B = 1, C = 255, D = 0 }
    assertThrown!ConvOverflowException(to!ubyte(E3.A));
    assertThrown!ConvOverflowException(to!bool(E3.A));
    assert(to!byte(E3.A) == -1);
    assert(to!byte(E3.B) == 1);
    assert(to!ubyte(E3.C) == 255);
    assert(to!bool(E3.B) == true);
    assertThrown!ConvOverflowException(to!byte(E3.C));
    assertThrown!ConvOverflowException(to!bool(E3.C));
    assert(to!bool(E3.D) == false);

}

@safe unittest
{
    import std.exception;
    import std.math.traits : isNaN;

    double d = double.nan;
    float f = to!float(d);
    assert(f.isNaN);
    assert(to!double(f).isNaN);
    assertThrown!ConvException(to!int(d));
    assertThrown!ConvException(to!int(f));
    auto ex = collectException(d.to!int);
    assert(ex.msg == "Input was NaN");
}

/**
Array-to-array conversion (except when target is a string type)
converts each element in turn by using `to`.
 */
private T toImpl(T, S)(scope S value)
if (!is(S : T) &&
    !isSomeString!S && isDynamicArray!S &&
    !isExactSomeString!T && isArray!T)
{
    alias E = typeof(T.init[0]);

    static if (isStaticArray!T)
    {
        import std.exception : enforce;
        auto res = to!(E[])(value);
        enforce!ConvException(T.length == res.length,
            convFormat("Length mismatch when converting to static array: %s vs %s", T.length, res.length));
        return res[0 .. T.length];
    }
    else
    {
        import std.array : appender;
        auto w = appender!(E[])();
        w.reserve(value.length);
        foreach (ref e; value)
        {
            w.put(to!E(e));
        }
        return w.data;
    }
}

@safe pure unittest
{
    import std.exception;

    // array to array conversions
    uint[] a = [ 1u, 2, 3 ];
    auto b = to!(float[])(a);
    assert(b == [ 1.0f, 2, 3 ]);

    immutable(int)[3] d = [ 1, 2, 3 ];
    b = to!(float[])(d);
    assert(b == [ 1.0f, 2, 3 ]);

    uint[][] e = [ a, a ];
    auto f = to!(float[][])(e);
    assert(f[0] == b && f[1] == b);

    // Test for https://issues.dlang.org/show_bug.cgi?id=8264
    struct Wrap
    {
        string wrap;
        alias wrap this;
    }
    Wrap[] warr = to!(Wrap[])(["foo", "bar"]);  // should work

    // https://issues.dlang.org/show_bug.cgi?id=12633
    import std.conv : to;
    const s2 = ["10", "20"];

    immutable int[2] a3 = s2.to!(int[2]);
    assert(a3 == [10, 20]);

    // verify length mismatches are caught
    immutable s4 = [1, 2, 3, 4];
    foreach (i; [1, 4])
    {
        auto ex = collectException(s4[0 .. i].to!(int[2]));
            assert(ex && ex.msg == "Length mismatch when converting to static array: 2 vs " ~ [cast(char)(i + '0')],
                ex ? ex.msg : "Exception was not thrown!");
    }
}

@safe unittest
{
    auto b = [ 1.0f, 2, 3 ];

    auto c = to!(string[])(b);
    assert(c[0] == "1" && c[1] == "2" && c[2] == "3");
}

/**
Associative array to associative array conversion converts each key
and each value in turn.
 */
private T toImpl(T, S)(S value)
if (!is(S : T) && isAssociativeArray!S &&
    isAssociativeArray!T && !is(T == enum))
{
    /* This code is potentially unsafe.
     */
    alias K2 = KeyType!T;
    alias V2 = ValueType!T;

    // While we are "building" the AA, we need to unqualify its values, and only re-qualify at the end
    Unqual!V2[K2] result;

    foreach (k1, v1; value)
    {
        // Cast values temporarily to Unqual!V2 to store them to result variable
        result[to!K2(k1)] = to!(Unqual!V2)(v1);
    }
    // Cast back to original type
    return () @trusted { return cast(T) result; }();
}

@safe unittest
{
    // hash to hash conversions
    int[string] a;
    a["0"] = 1;
    a["1"] = 2;
    auto b = to!(double[dstring])(a);
    assert(b["0"d] == 1 && b["1"d] == 2);
}

// https://issues.dlang.org/show_bug.cgi?id=8705, from doc
@safe unittest
{
    import std.exception;
    int[string][double[int[]]] a;
    auto b = to!(short[wstring][string[double[]]])(a);
    a = [null:["hello":int.max]];
    assertThrown!ConvOverflowException(to!(short[wstring][string[double[]]])(a));
}
@system unittest // Extra cases for AA with qualifiers conversion
{
    int[][int[]] a;// = [[], []];
    auto b = to!(immutable(short[])[immutable short[]])(a);

    double[dstring][int[long[]]] c;
    auto d = to!(immutable(short[immutable wstring])[immutable string[double[]]])(c);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.array : byPair;

    int[int] a;
    assert(a.to!(int[int]) == a);
    assert(a.to!(const(int)[int]).byPair.equal(a.byPair));
}

@safe pure unittest
{
    static void testIntegralToFloating(Integral, Floating)()
    {
        Integral a = 42;
        auto b = to!Floating(a);
        assert(a == b);
        assert(a == to!Integral(b));
    }
    static void testFloatingToIntegral(Floating, Integral)()
    {
        import std.math.traits : floatTraits, RealFormat;

        bool convFails(Source, Target, E)(Source src)
        {
            try
                cast(void) to!Target(src);
            catch (E)
                return true;
            return false;
        }

        // convert some value
        Floating a = 4.2e1;
        auto b = to!Integral(a);
        assert(is(typeof(b) == Integral) && b == 42);
        // convert some negative value (if applicable)
        a = -4.2e1;
        static if (Integral.min < 0)
        {
            b = to!Integral(a);
            assert(is(typeof(b) == Integral) && b == -42);
        }
        else
        {
            // no go for unsigned types
            assert(convFails!(Floating, Integral, ConvOverflowException)(a));
        }
        // convert to the smallest integral value
        a = 0.0 + Integral.min;
        static if (Integral.min < 0)
        {
            a = -a; // -Integral.min not representable as an Integral
            assert(convFails!(Floating, Integral, ConvOverflowException)(a)
                    || Floating.sizeof <= Integral.sizeof
                    || floatTraits!Floating.realFormat == RealFormat.ieeeExtended53);
        }
        a = 0.0 + Integral.min;
        assert(to!Integral(a) == Integral.min);
        --a; // no more representable as an Integral
        assert(convFails!(Floating, Integral, ConvOverflowException)(a)
                || Floating.sizeof <= Integral.sizeof
                || floatTraits!Floating.realFormat == RealFormat.ieeeExtended53);
        a = 0.0 + Integral.max;
        assert(to!Integral(a) == Integral.max
                || Floating.sizeof <= Integral.sizeof
                || floatTraits!Floating.realFormat == RealFormat.ieeeExtended53);
        ++a; // no more representable as an Integral
        assert(convFails!(Floating, Integral, ConvOverflowException)(a)
                || Floating.sizeof <= Integral.sizeof
                || floatTraits!Floating.realFormat == RealFormat.ieeeExtended53);
        // convert a value with a fractional part
        a = 3.14;
        assert(to!Integral(a) == 3);
        a = 3.99;
        assert(to!Integral(a) == 3);
        static if (Integral.min < 0)
        {
            a = -3.14;
            assert(to!Integral(a) == -3);
            a = -3.99;
            assert(to!Integral(a) == -3);
        }
    }

    alias AllInts = AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong);
    alias AllFloats = AliasSeq!(float, double, real);
    alias AllNumerics = AliasSeq!(AllInts, AllFloats);
    // test with same type
    {
        foreach (T; AllNumerics)
        {
            T a = 42;
            auto b = to!T(a);
            assert(is(typeof(a) == typeof(b)) && a == b);
        }
    }
    // test that floating-point numbers convert properly to largest ints
    // see http://oregonstate.edu/~peterseb/mth351/docs/351s2001_fp80x87.html
    // look for "largest fp integer with a predecessor"
    {
        // float
        int a = 16_777_215; // 2^24 - 1
        assert(to!int(to!float(a)) == a);
        assert(to!int(to!float(-a)) == -a);
        // double
        long b = 9_007_199_254_740_991; // 2^53 - 1
        assert(to!long(to!double(b)) == b);
        assert(to!long(to!double(-b)) == -b);
        // real
        static if (real.mant_dig >= 64)
        {
            ulong c = 18_446_744_073_709_551_615UL; // 2^64 - 1
            assert(to!ulong(to!real(c)) == c);
        }
    }
    // test conversions floating => integral
    {
        foreach (Integral; AllInts)
        {
            foreach (Floating; AllFloats)
            {
                testFloatingToIntegral!(Floating, Integral)();
            }
        }
    }
    // test conversion integral => floating
    {
        foreach (Integral; AllInts)
        {
            foreach (Floating; AllFloats)
            {
                testIntegralToFloating!(Integral, Floating)();
            }
        }
    }
    // test parsing
    {
        foreach (T; AllNumerics)
        {
            // from type immutable(char)[2]
            auto a = to!T("42");
            assert(a == 42);
            // from type char[]
            char[] s1 = "42".dup;
            a = to!T(s1);
            assert(a == 42);
            // from type char[2]
            char[2] s2;
            s2[] = "42";
            a = to!T(s2);
            assert(a == 42);
            // from type immutable(wchar)[2]
            a = to!T("42"w);
            assert(a == 42);
        }
    }
}

@safe unittest
{
    alias AllInts = AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong);
    alias AllFloats = AliasSeq!(float, double, real);
    alias AllNumerics = AliasSeq!(AllInts, AllFloats);
    // test conversions to string
    {
        foreach (T; AllNumerics)
        {
            T a = 42;
            string s = to!string(a);
            assert(s == "42", s);
            wstring ws = to!wstring(a);
            assert(ws == "42"w, to!string(ws));
            dstring ds = to!dstring(a);
            assert(ds == "42"d, to!string(ds));
            // array test
            T[] b = new T[2];
            b[0] = 42;
            b[1] = 33;
            assert(to!string(b) == "[42, 33]");
        }
    }
    // test array to string conversion
    foreach (T ; AllNumerics)
    {
        auto a = [to!T(1), 2, 3];
        assert(to!string(a) == "[1, 2, 3]");
    }
    // test enum to int conversion
    enum Testing { Test1, Test2 }
    Testing t;
    auto a = to!string(t);
    assert(a == "Test1");
}


/**
String, or string-like input range, to non-string conversion runs parsing.
$(UL
  $(LI When the source is a wide string, it is first converted to a narrow
       string and then parsed.)
  $(LI When the source is a narrow string, normal text parsing occurs.))
*/
private T toImpl(T, S)(S value)
if (isInputRange!S && isSomeChar!(ElementEncodingType!S) &&
    !isExactSomeString!T && is(typeof(parse!T(value))) &&
    // https://issues.dlang.org/show_bug.cgi?id=20539
    !(is(T == enum) && is(typeof(value == OriginalType!T.init)) && !isSomeString!(OriginalType!T)))
{
    scope(success)
    {
        if (!value.empty)
        {
            throw convError!(S, T)(value);
        }
    }
    return parse!T(value);
}

/// ditto
private T toImpl(T, S)(S value, uint radix)
if (isSomeFiniteCharInputRange!S &&
    isIntegral!T && is(typeof(parse!T(value, radix))))
{
    scope(success)
    {
        if (!value.empty)
        {
            throw convError!(S, T)(value);
        }
    }
    return parse!T(value, radix);
}

@safe pure unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=6668
    // ensure no collaterals thrown
    try { to!uint("-1"); }
    catch (ConvException e) { assert(e.next is null); }
}

@safe pure unittest
{
    static foreach (Str; AliasSeq!(string, wstring, dstring))
    {{
        Str a = "123";
        assert(to!int(a) == 123);
        assert(to!double(a) == 123);
    }}

    // https://issues.dlang.org/show_bug.cgi?id=6255
    auto n = to!int("FF", 16);
    assert(n == 255);
}

// https://issues.dlang.org/show_bug.cgi?id=15800
@safe unittest
{
    import std.utf : byCodeUnit, byChar, byWchar, byDchar;

    assert(to!int(byCodeUnit("10")) == 10);
    assert(to!int(byCodeUnit("10"), 10) == 10);
    assert(to!int(byCodeUnit("10"w)) == 10);
    assert(to!int(byCodeUnit("10"w), 10) == 10);

    assert(to!int(byChar("10")) == 10);
    assert(to!int(byChar("10"), 10) == 10);
    assert(to!int(byWchar("10")) == 10);
    assert(to!int(byWchar("10"), 10) == 10);
    assert(to!int(byDchar("10")) == 10);
    assert(to!int(byDchar("10"), 10) == 10);
}

/**
String, or string-like input range, to char type not directly
supported by parse parses the first dchar of the source.

Returns: the first code point of the input range, converted
         to type T.

Throws: ConvException if the input range contains more than
        a single code point, or if the code point does not
        fit into a code unit of type T.
*/
private T toImpl(T, S)(S value)
if (isSomeChar!T && !is(typeof(parse!T(value))) &&
    is(typeof(parse!dchar(value))))
{
    import std.utf : encode;

    immutable dchar codepoint = parse!dchar(value);
    if (!value.empty)
        throw new ConvException(convFormat("Cannot convert \"%s\" to %s because it " ~
                                           "contains more than a single code point.",
                                           value, T.stringof));
    T[dchar.sizeof / T.sizeof] decodedCodepoint;
    if (encode(decodedCodepoint, codepoint) != 1)
        throw new ConvException(convFormat("First code point '%s' of \"%s\" does not fit into a " ~
                                           "single %s code unit", codepoint, value, T.stringof));
    return decodedCodepoint[0];
}

@safe pure unittest
{
    import std.exception : assertThrown;

    assert(toImpl!wchar("a") == 'a');

    assert(toImpl!char("a"d) == 'a');
    assert(toImpl!char("a"w) == 'a');
    assert(toImpl!wchar("a"d) == 'a');

    assertThrown!ConvException(toImpl!wchar("ab"));
    assertThrown!ConvException(toImpl!char("😃"d));
}

/**
Convert a value that is implicitly convertible to the enum base type
into an Enum value. If the value does not match any enum member values
a ConvException is thrown.
Enums with floating-point or string base types are not supported.
*/
private T toImpl(T, S)(S value)
if (is(T == enum) && !is(S == enum)
    && is(typeof(value == OriginalType!T.init))
    && !isFloatingPoint!(OriginalType!T) && !isSomeString!(OriginalType!T))
{
    foreach (Member; EnumMembers!T)
    {
        if (Member == value)
            return Member;
    }
    throw new ConvException(convFormat("Value (%s) does not match any member value of enum '%s'", value, T.stringof));
}

@safe pure unittest
{
    import std.exception;
    enum En8143 : int { A = 10, B = 20, C = 30, D = 20 }
    enum En8143[][] m3 = to!(En8143[][])([[10, 30], [30, 10]]);
    static assert(m3 == [[En8143.A, En8143.C], [En8143.C, En8143.A]]);

    En8143 en1 = to!En8143(10);
    assert(en1 == En8143.A);
    assertThrown!ConvException(to!En8143(5));   // matches none
    En8143[][] m1 = to!(En8143[][])([[10, 30], [30, 10]]);
    assert(m1 == [[En8143.A, En8143.C], [En8143.C, En8143.A]]);
}

// https://issues.dlang.org/show_bug.cgi?id=20539
@safe pure unittest
{
    import std.exception : assertNotThrown;

    // To test that the bug is fixed it is required that the struct is static,
    // otherwise, the frame pointer makes the test pass even if the bug is not
    // fixed.

    static struct A
    {
        auto opEquals(U)(U)
        {
            return true;
        }
    }

    enum ColorA
    {
        red = A()
    }

    assertNotThrown("xxx".to!ColorA);

    // This is a guard for the future.

    struct B
    {
        auto opEquals(U)(U)
        {
            return true;
        }
    }

    enum ColorB
    {
        red = B()
    }

    assertNotThrown("xxx".to!ColorB);
}

/***************************************************************
 Rounded conversion from floating point to integral.

Rounded conversions do not work with non-integral target types.
 */

template roundTo(Target)
{
    Target roundTo(Source)(Source value)
    {
        import core.math : abs = fabs;
        import std.math.exponential : log2;
        import std.math.rounding : trunc;

        static assert(isFloatingPoint!Source);
        static assert(isIntegral!Target);

        // If value >= 2 ^^ (real.mant_dig - 1), the number is an integer
        // and adding 0.5 won't work, but we allready know, that we do
        // not have to round anything.
        if (log2(abs(value)) >= real.mant_dig - 1)
            return to!Target(value);

        return to!Target(trunc(value + (value < 0 ? -0.5L : 0.5L)));
    }
}

///
@safe unittest
{
    assert(roundTo!int(3.14) == 3);
    assert(roundTo!int(3.49) == 3);
    assert(roundTo!int(3.5) == 4);
    assert(roundTo!int(3.999) == 4);
    assert(roundTo!int(-3.14) == -3);
    assert(roundTo!int(-3.49) == -3);
    assert(roundTo!int(-3.5) == -4);
    assert(roundTo!int(-3.999) == -4);
    assert(roundTo!(const int)(to!(const double)(-3.999)) == -4);
}

@safe unittest
{
    import std.exception;
    // boundary values
    static foreach (Int; AliasSeq!(byte, ubyte, short, ushort, int, uint))
    {
        assert(roundTo!Int(Int.min - 0.4L) == Int.min);
        assert(roundTo!Int(Int.max + 0.4L) == Int.max);
        assertThrown!ConvOverflowException(roundTo!Int(Int.min - 0.5L));
        assertThrown!ConvOverflowException(roundTo!Int(Int.max + 0.5L));
    }
}

@safe unittest
{
    import std.exception;
    assertThrown!ConvException(roundTo!int(float.init));
    auto ex = collectException(roundTo!int(float.init));
    assert(ex.msg == "Input was NaN");
}

// https://issues.dlang.org/show_bug.cgi?id=5232
@safe pure unittest
{
    static if (real.mant_dig >= 64)
        ulong maxOdd = ulong.max;
    else
        ulong maxOdd = (1UL << real.mant_dig) - 1;

    real r1 = maxOdd;
    assert(roundTo!ulong(r1) == maxOdd);

    real r2 = maxOdd - 1;
    assert(roundTo!ulong(r2) == maxOdd - 1);

    real r3 = maxOdd / 2;
    assert(roundTo!ulong(r3) == maxOdd / 2);

    real r4 = maxOdd / 2 + 1;
    assert(roundTo!ulong(r4) == maxOdd / 2 + 1);

    // this is only an issue on computers where real == double
    long l = -((1L << double.mant_dig) - 1);
    double r5 = l;
    assert(roundTo!long(r5) == l);
}

/**
$(PANEL
The `parse` family of functions works quite like the $(LREF to)
family, except that:
$(OL
    $(LI It only works with character ranges as input.)
    $(LI It takes the input by reference. This means that rvalues (such
    as string literals) are not accepted: use `to` instead.)
    $(LI It advances the input to the position following the conversion.)
    $(LI It does not throw if it could not convert the entire input.))
)

This overload parses a `bool` from a character input range.

Params:
    Target = the boolean type to convert to
    source = the lvalue of an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    doCount = the flag for deciding to report the number of consumed characters

Returns:
$(UL
    $(LI A `bool` if `doCount` is set to `No.doCount`)
    $(LI A `tuple` containing a `bool` and a `size_t` if `doCount` is set to `Yes.doCount`))

Throws:
    A $(LREF ConvException) if the range does not represent a `bool`.

Note:
    All character input range conversions using $(LREF to) are forwarded
    to `parse` and do not require lvalues.
*/
auto parse(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source source)
if (is(immutable Target == immutable bool) &&
    isInputRange!Source &&
    isSomeChar!(ElementType!Source))
{
    import std.ascii : toLower;

    static if (isNarrowString!Source)
    {
        import std.string : representation;
        auto s = source.representation;
    }
    else
    {
        alias s = source;
    }

    if (!s.empty)
    {
        auto c1 = toLower(s.front);
        bool result = c1 == 't';
        if (result || c1 == 'f')
        {
            s.popFront();
            foreach (c; result ? "rue" : "alse")
            {
                if (s.empty || toLower(s.front) != c)
                    goto Lerr;
                s.popFront();
            }

            static if (isNarrowString!Source)
                source = cast(Source) s;

            static if (doCount)
            {
                if (result)
                    return tuple!("data", "count")(result, 4);
                return tuple!("data", "count")(result, 5);
            }
            else
            {
                return result;
            }
        }
    }
Lerr:
    throw parseError("bool should be case-insensitive 'true' or 'false'");
}

///
@safe unittest
{
    import std.typecons : Flag, Yes, No;
    auto s = "true";
    bool b = parse!bool(s);
    assert(b);
    auto s2 = "true";
    bool b2 = parse!(bool, string, No.doCount)(s2);
    assert(b2);
    auto s3 = "true";
    auto b3 = parse!(bool, string, Yes.doCount)(s3);
    assert(b3.data && b3.count == 4);
    auto s4 = "falSE";
    auto b4 = parse!(bool, string, Yes.doCount)(s4);
    assert(!b4.data && b4.count == 5);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.exception;
    struct InputString
    {
        string _s;
        @property auto front() { return _s.front; }
        @property bool empty() { return _s.empty; }
        void popFront() { _s.popFront(); }
    }

    auto s = InputString("trueFALSETrueFalsetRUEfALSE");
    assert(parse!bool(s) == true);
    assert(s.equal("FALSETrueFalsetRUEfALSE"));
    assert(parse!bool(s) == false);
    assert(s.equal("TrueFalsetRUEfALSE"));
    assert(parse!bool(s) == true);
    assert(s.equal("FalsetRUEfALSE"));
    assert(parse!bool(s) == false);
    assert(s.equal("tRUEfALSE"));
    assert(parse!bool(s) == true);
    assert(s.equal("fALSE"));
    assert(parse!bool(s) == false);
    assert(s.empty);

    foreach (ss; ["tfalse", "ftrue", "t", "f", "tru", "fals", ""])
    {
        s = InputString(ss);
        assertThrown!ConvException(parse!bool(s));
    }
}

/**
Parses an integer from a character $(REF_ALTTEXT input range, isInputRange, std,range,primitives).

Params:
    Target = the integral type to convert to
    s = the lvalue of an input range
    doCount = the flag for deciding to report the number of consumed characters

Returns:
$(UL
    $(LI A number of type `Target` if `doCount` is set to `No.doCount`)
    $(LI A `tuple` containing a number of type `Target` and a `size_t` if `doCount` is set to `Yes.doCount`))

Throws:
    A $(LREF ConvException) If an overflow occurred during conversion or
    if no character of the input was meaningfully converted.
*/
auto parse(Target, Source, Flag!"doCount" doCount = No.doCount)(ref scope Source s)
if (isIntegral!Target && !is(Target == enum) &&
    isSomeChar!(ElementType!Source))
{
    static if (Target.sizeof < int.sizeof)
    {
        // smaller types are handled like integers
        auto v = .parse!(Select!(Target.min < 0, int, uint), Source, Yes.doCount)(s);
        auto result = (() @trusted => cast (Target) v.data)();
        if (result == v.data)
        {
            static if (doCount)
            {
                return tuple!("data", "count")(result, v.count);
            }
            else
            {
                return result;
            }
        }
        throw new ConvOverflowException("Overflow in integral conversion");
    }
    else
    {
        // int or larger types

        static if (Target.min < 0)
            bool sign = false;
        else
            enum bool sign = false;

        enum char maxLastDigit = Target.min < 0 ? 7 : 5;
        uint c;

        static if (isNarrowString!Source)
        {
            import std.string : representation;
            auto source = s.representation;
        }
        else
        {
            alias source = s;
        }

        size_t count = 0;

        if (source.empty)
            goto Lerr;

        c = source.front;

        static if (Target.min < 0)
        {
            switch (c)
            {
                case '-':
                    sign = true;
                    goto case '+';
                case '+':
                    ++count;
                    source.popFront();

                    if (source.empty)
                        goto Lerr;

                    c = source.front;

                    break;

                default:
                    break;
            }
        }
        c -= '0';
        if (c <= 9)
        {
            Target v = cast(Target) c;

            ++count;
            source.popFront();

            while (!source.empty)
            {
                c = cast(typeof(c)) (source.front - '0');

                if (c > 9)
                    break;

                if (v >= 0 && (v < Target.max/10 ||
                    (v == Target.max/10 && c <= maxLastDigit + sign)))
                {
                    // Note: `v` can become negative here in case of parsing
                    // the most negative value:
                    v = cast(Target) (v * 10 + c);
                    ++count;
                    source.popFront();
                }
                else
                    throw new ConvOverflowException("Overflow in integral conversion");
            }

            if (sign)
                v = -v;

            static if (isNarrowString!Source)
                s = s[$-source.length..$];

            static if (doCount)
            {
                return tuple!("data", "count")(v, count);
            }
            else
            {
                return v;
            }
        }
Lerr:
        static if (isNarrowString!Source)
            throw convError!(Source, Target)(cast(Source) source);
        else
            throw convError!(Source, Target)(source);
    }
}

///
@safe pure unittest
{
    import std.typecons : Flag, Yes, No;
    string s = "123";
    auto a = parse!int(s);
    assert(a == 123);

    string s1 = "123";
    auto a1 = parse!(int, string, Yes.doCount)(s1);
    assert(a1.data == 123 && a1.count == 3);

    // parse only accepts lvalues
    static assert(!__traits(compiles, parse!int("123")));
}

///
@safe pure unittest
{
    import std.string : tr;
    import std.typecons : Flag, Yes, No;
    string test = "123 \t  76.14";
    auto a = parse!uint(test);
    assert(a == 123);
    assert(test == " \t  76.14"); // parse bumps string
    test = tr(test, " \t\n\r", "", "d"); // skip ws
    assert(test == "76.14");
    auto b = parse!double(test);
    assert(b == 76.14);
    assert(test == "");

    string test2 = "123 \t  76.14";
    auto a2 = parse!(uint, string, Yes.doCount)(test2);
    assert(a2.data == 123 && a2.count == 3);
    assert(test2 == " \t  76.14");// parse bumps string
    test2 = tr(test2, " \t\n\r", "", "d"); // skip ws
    assert(test2 == "76.14");
    auto b2 = parse!(double, string, Yes.doCount)(test2);
    assert(b2.data == 76.14 && b2.count == 5);
    assert(test2 == "");

}

@safe pure unittest
{
    static foreach (Int; AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong))
    {
        {
            assert(to!Int("0") == 0);

            static if (isSigned!Int)
            {
                assert(to!Int("+0") == 0);
                assert(to!Int("-0") == 0);
            }
        }

        static if (Int.sizeof >= byte.sizeof)
        {
                assert(to!Int("6") == 6);
                assert(to!Int("23") == 23);
                assert(to!Int("68") == 68);
                assert(to!Int("127") == 0x7F);

            static if (isUnsigned!Int)
            {
                assert(to!Int("255") == 0xFF);
            }
            static if (isSigned!Int)
            {
                assert(to!Int("+6") == 6);
                assert(to!Int("+23") == 23);
                assert(to!Int("+68") == 68);
                assert(to!Int("+127") == 0x7F);

                assert(to!Int("-6") == -6);
                assert(to!Int("-23") == -23);
                assert(to!Int("-68") == -68);
                assert(to!Int("-128") == -128);
            }
        }

        static if (Int.sizeof >= short.sizeof)
        {
                assert(to!Int("468") == 468);
                assert(to!Int("32767") == 0x7FFF);

            static if (isUnsigned!Int)
            {
                assert(to!Int("65535") == 0xFFFF);
            }
            static if (isSigned!Int)
            {
                assert(to!Int("+468") == 468);
                assert(to!Int("+32767") == 0x7FFF);

                assert(to!Int("-468") == -468);
                assert(to!Int("-32768") == -32768);
            }
        }

        static if (Int.sizeof >= int.sizeof)
        {
                assert(to!Int("2147483647") == 0x7FFFFFFF);

            static if (isUnsigned!Int)
            {
                assert(to!Int("4294967295") == 0xFFFFFFFF);
            }

            static if (isSigned!Int)
            {
                assert(to!Int("+2147483647") == 0x7FFFFFFF);

                assert(to!Int("-2147483648") == -2147483648);
            }
        }

        static if (Int.sizeof >= long.sizeof)
        {
                assert(to!Int("9223372036854775807") == 0x7FFFFFFFFFFFFFFF);

            static if (isUnsigned!Int)
            {
                assert(to!Int("18446744073709551615") == 0xFFFFFFFFFFFFFFFF);
            }

            static if (isSigned!Int)
            {
                assert(to!Int("+9223372036854775807") == 0x7FFFFFFFFFFFFFFF);

                assert(to!Int("-9223372036854775808") == 0x8000000000000000);
            }
        }
    }
}

@safe pure unittest
{
    import std.exception;

    immutable string[] errors =
    [
        "",
        "-",
        "+",
        "-+",
        " ",
        " 0",
        "0 ",
        "- 0",
        "1-",
        "xx",
        "123h",
        "-+1",
        "--1",
        "+-1",
        "++1",
    ];

    immutable string[] unsignedErrors =
    [
        "+5",
        "-78",
    ];

    // parsing error check
    static foreach (Int; AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong))
    {
        foreach (j, s; errors)
            assertThrown!ConvException(to!Int(s));

        // parse!SomeUnsigned cannot parse head sign.
        static if (isUnsigned!Int)
        {
            foreach (j, s; unsignedErrors)
                assertThrown!ConvException(to!Int(s));
        }
    }

    immutable string[] positiveOverflowErrors =
    [
        "128",                  // > byte.max
        "256",                  // > ubyte.max
        "32768",                // > short.max
        "65536",                // > ushort.max
        "2147483648",           // > int.max
        "4294967296",           // > uint.max
        "9223372036854775808",  // > long.max
        "18446744073709551616", // > ulong.max
    ];
    // positive overflow check
    static foreach (i, Int; AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong))
    {
        foreach (j, s; positiveOverflowErrors[i..$])
            assertThrown!ConvOverflowException(to!Int(s));
    }

    immutable string[] negativeOverflowErrors =
    [
        "-129",                 // < byte.min
        "-32769",               // < short.min
        "-2147483649",          // < int.min
        "-9223372036854775809", // < long.min
    ];
    // negative overflow check
    static foreach (i, Int; AliasSeq!(byte, short, int, long))
    {
        foreach (j, s; negativeOverflowErrors[i..$])
            assertThrown!ConvOverflowException(to!Int(s));
    }
}

@safe pure unittest
{
    void checkErrMsg(string input, dchar charInMsg, dchar charNotInMsg)
    {
        try
        {
            int x = input.to!int();
            assert(false, "Invalid conversion did not throw");
        }
        catch (ConvException e)
        {
            // Ensure error message contains failing character, not the character
            // beyond.
            import std.algorithm.searching : canFind;
            assert( e.msg.canFind(charInMsg) &&
                   !e.msg.canFind(charNotInMsg));
        }
        catch (Exception e)
        {
            assert(false, "Did not throw ConvException");
        }
    }
    checkErrMsg("@$", '@', '$');
    checkErrMsg("@$123", '@', '$');
    checkErrMsg("1@$23", '@', '$');
    checkErrMsg("1@$", '@', '$');
    checkErrMsg("1@$2", '@', '$');
    checkErrMsg("12@$", '@', '$');
}

@safe pure unittest
{
    import std.exception;
    assertCTFEable!({ string s =  "1234abc"; assert(parse! int(s) ==  1234 && s == "abc"); });
    assertCTFEable!({ string s = "-1234abc"; assert(parse! int(s) == -1234 && s == "abc"); });
    assertCTFEable!({ string s =  "1234abc"; assert(parse!uint(s) ==  1234 && s == "abc"); });

    assertCTFEable!({ string s =  "1234abc"; assert(parse!( int, string, Yes.doCount)(s) ==
        tuple( 1234, 4) && s == "abc"); });
    assertCTFEable!({ string s = "-1234abc"; assert(parse!( int, string, Yes.doCount)(s) ==
        tuple(-1234, 5) && s == "abc"); });
    assertCTFEable!({ string s =  "1234abc"; assert(parse!(uint, string, Yes.doCount)(s) ==
        tuple( 1234 ,4) && s == "abc"); });
}

// https://issues.dlang.org/show_bug.cgi?id=13931
@safe pure unittest
{
    import std.exception;

    assertThrown!ConvOverflowException("-21474836480".to!int());
    assertThrown!ConvOverflowException("-92233720368547758080".to!long());
}

// https://issues.dlang.org/show_bug.cgi?id=14396
@safe pure unittest
{
    struct StrInputRange
    {
        this (string s) { str = s; }
        char front() const @property { return str[front_index]; }
        char popFront() { return str[front_index++]; }
        bool empty() const @property { return str.length <= front_index; }
        string str;
        size_t front_index = 0;
    }
    auto input = StrInputRange("777");
    assert(parse!int(input) == 777);

    auto input2 = StrInputRange("777");
    assert(parse!(int, StrInputRange, Yes.doCount)(input2) == tuple(777, 3));
}

// https://issues.dlang.org/show_bug.cgi?id=9621
@safe pure unittest
{
    string s1 = "[ \"\\141\", \"\\0\", \"\\41\", \"\\418\" ]";
    assert(parse!(string[])(s1) == ["a", "\0", "!", "!8"]);

    s1 = "[ \"\\141\", \"\\0\", \"\\41\", \"\\418\" ]";
    auto len = s1.length;
    assert(parse!(string[], string, Yes.doCount)(s1) == tuple(["a", "\0", "!", "!8"], len));
}

/// ditto
auto parse(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source source, uint radix)
if (isIntegral!Target && !is(Target == enum) &&
    isSomeChar!(ElementType!Source))
in
{
    assert(radix >= 2 && radix <= 36, "radix must be in range [2,36]");
}
do
{
    import core.checkedint : mulu, addu;
    import std.exception : enforce;

    if (radix == 10)
    {
        return parse!(Target, Source, doCount)(source);
    }

    enforce!ConvException(!source.empty, "s must not be empty in integral parse");

    immutable uint beyond = (radix < 10 ? '0' : 'a'-10) + radix;
    Target v = 0;

    static if (isNarrowString!Source)
    {
        import std.string : representation;
        scope s = source.representation;
    }
    else
    {
        alias s = source;
    }

    size_t count = 0;
    auto found = false;
    do
    {
        uint c = s.front;
        if (c < '0')
            break;
        if (radix < 10)
        {
            if (c >= beyond)
                break;
        }
        else
        {
            if (c > '9')
            {
                c |= 0x20;//poorman's tolower
                if (c < 'a' || c >= beyond)
                    break;
                c -= 'a'-10-'0';
            }
        }

        bool overflow = false;
        auto nextv = v.mulu(radix, overflow).addu(c - '0', overflow);
        enforce!ConvOverflowException(!overflow && nextv <= Target.max, "Overflow in integral conversion");
        v = cast(Target) nextv;
        ++count;
        s.popFront();
        found = true;
    } while (!s.empty);

    if (!found)
    {
        static if (isNarrowString!Source)
            throw convError!(Source, Target)(cast(Source) source);
        else
            throw convError!(Source, Target)(source);
    }

    static if (isNarrowString!Source)
        source = source[$ - s.length .. $];

    static if (doCount)
    {
        return tuple!("data", "count")(v, count);
    }
    else
    {
        return v;
    }
}

@safe pure unittest
{
    string s; // parse doesn't accept rvalues
    foreach (i; 2 .. 37)
    {
        assert(parse!int(s = "0", i) == 0);
        assert(parse!int(s = "1", i) == 1);
        assert(parse!byte(s = "10", i) == i);
        assert(parse!(int, string, Yes.doCount)(s = "0", i) == tuple(0, 1));
        assert(parse!(int, string, Yes.doCount)(s = "1", i) == tuple(1, 1));
        assert(parse!(byte, string, Yes.doCount)(s = "10", i) == tuple(i, 2));
    }

    assert(parse!int(s = "0011001101101", 2) == 0b0011001101101);
    assert(parse!int(s = "765", 8) == octal!765);
    assert(parse!int(s = "000135", 8) == octal!"135");
    assert(parse!int(s = "fCDe", 16) == 0xfcde);

    // https://issues.dlang.org/show_bug.cgi?id=6609
    assert(parse!int(s = "-42", 10) == -42);

    assert(parse!ubyte(s = "ff", 16) == 0xFF);
}

// https://issues.dlang.org/show_bug.cgi?id=7302
@safe pure unittest
{
    import std.range : cycle;
    auto r = cycle("2A!");
    auto u = parse!uint(r, 16);
    assert(u == 42);
    assert(r.front == '!');

    auto r2 = cycle("2A!");
    auto u2 = parse!(uint, typeof(r2), Yes.doCount)(r2, 16);
    assert(u2.data == 42 && u2.count == 2);
    assert(r2.front == '!');
}

// https://issues.dlang.org/show_bug.cgi?id=13163
@safe pure unittest
{
    import std.exception;
    foreach (s; ["fff", "123"])
        assertThrown!ConvOverflowException(s.parse!ubyte(16));
}

// https://issues.dlang.org/show_bug.cgi?id=17282
@safe pure unittest
{
    auto str = "0=\x00\x02\x55\x40&\xff\xf0\n\x00\x04\x55\x40\xff\xf0~4+10\n";
    assert(parse!uint(str) == 0);

    str = "0=\x00\x02\x55\x40&\xff\xf0\n\x00\x04\x55\x40\xff\xf0~4+10\n";
    assert(parse!(uint, string, Yes.doCount)(str) == tuple(0, 1));
}

// https://issues.dlang.org/show_bug.cgi?id=18248
@safe pure unittest
{
    import std.exception : assertThrown;

    auto str = ";";
    assertThrown(str.parse!uint(16));
    assertThrown(str.parse!(uint, string, Yes.doCount)(16));
}

/**
 * Parses an `enum` type from a string representing an enum member name.
 *
 * Params:
 *     Target = the `enum` type to convert to
 *     s = the lvalue of the range to _parse
 *     doCount = the flag for deciding to report the number of consumed characters
 *
 * Returns:
 $(UL
 *     $(LI An `enum` of type `Target` if `doCount` is set to `No.doCount`)
 *     $(LI A `tuple` containing an `enum` of type `Target` and a `size_t` if `doCount` is set to `Yes.doCount`))
 *
 * Throws:
 *     A $(LREF ConvException) if type `Target` does not have a member
 *     represented by `s`.
 */
auto parse(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source s)
if (is(Target == enum) && isSomeString!Source && !is(Source == enum))
{
    import std.algorithm.searching : startsWith;
    import std.traits : Unqual, EnumMembers;

    Unqual!Target result;
    size_t longest_match = 0;

    foreach (i, e; EnumMembers!Target)
    {
        auto ident = __traits(allMembers, Target)[i];
        if (longest_match < ident.length && s.startsWith(ident))
        {
            result = e;
            longest_match = ident.length ;
        }
    }

    if (longest_match > 0)
    {
        s = s[longest_match .. $];
        static if (doCount)
        {
            return tuple!("data", "count")(result, longest_match);
        }
        else
        {
            return result;
        }
    }

    throw new ConvException(
        Target.stringof ~ " does not have a member named '"
        ~ to!string(s) ~ "'");
}

///
@safe unittest
{
    import std.typecons : Flag, Yes, No, tuple;
    enum EnumType : bool { a = true, b = false, c = a }

    auto str = "a";
    assert(parse!EnumType(str) == EnumType.a);
    auto str2 = "a";
    assert(parse!(EnumType, string, No.doCount)(str2) == EnumType.a);
    auto str3 = "a";
    assert(parse!(EnumType, string, Yes.doCount)(str3) == tuple(EnumType.a, 1));

}

@safe unittest
{
    import std.exception;

    enum EB : bool { a = true, b = false, c = a }
    enum EU { a, b, c }
    enum EI { a = -1, b = 0, c = 1 }
    enum EF : real { a = 1.414, b = 1.732, c = 2.236 }
    enum EC : char { a = 'a', b = 'b', c = 'c' }
    enum ES : string { a = "aaa", b = "bbb", c = "ccc" }

    static foreach (E; AliasSeq!(EB, EU, EI, EF, EC, ES))
    {
        assert(to!E("a"c) == E.a);
        assert(to!E("b"w) == E.b);
        assert(to!E("c"d) == E.c);

        assert(to!(const E)("a") == E.a);
        assert(to!(immutable E)("a") == E.a);
        assert(to!(shared E)("a") == E.a);

        assertThrown!ConvException(to!E("d"));
    }
}

// https://issues.dlang.org/show_bug.cgi?id=4744
@safe pure unittest
{
    enum A { member1, member11, member111 }
    assert(to!A("member1"  ) == A.member1  );
    assert(to!A("member11" ) == A.member11 );
    assert(to!A("member111") == A.member111);
    auto s = "member1111";
    assert(parse!A(s) == A.member111 && s == "1");
    auto s2 = "member1111";
    assert(parse!(A, string, No.doCount)(s2) == A.member111 && s2 == "1");
    auto s3 = "member1111";
    assert(parse!(A, string, Yes.doCount)(s3) == tuple(A.member111, 9) && s3 == "1");
}

/**
 * Parses a floating point number from a character range.
 *
 * Params:
 *     Target = a floating point type
 *     source = the lvalue of the range to _parse
 *     doCount = the flag for deciding to report the number of consumed characters
 *
 * Returns:
 $(UL
 *     $(LI A floating point number of type `Target` if `doCount` is set to `No.doCount`)
 *     $(LI A `tuple` containing a floating point number of·type `Target` and a `size_t`
 *     if `doCount` is set to `Yes.doCount`))
 *
 * Throws:
 *     A $(LREF ConvException) if `source` is empty, if no number could be
 *     parsed, or if an overflow occurred.
 */
auto parse(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source source)
if (isFloatingPoint!Target && !is(Target == enum) &&
    isInputRange!Source && isSomeChar!(ElementType!Source) && !is(Source == enum))
{
    import std.ascii : isDigit, isAlpha, toLower, toUpper, isHexDigit;
    import std.exception : enforce;

    static if (isNarrowString!Source)
    {
        import std.string : representation;
        scope p = source.representation;
    }
    else
    {
        alias p = source;
    }

    void advanceSource()
    {
        static if (isNarrowString!Source)
            source = source[$ - p.length .. $];
    }

    static immutable real[14] negtab =
        [ 1e-4096L,1e-2048L,1e-1024L,1e-512L,1e-256L,1e-128L,1e-64L,1e-32L,
                1e-16L,1e-8L,1e-4L,1e-2L,1e-1L,1.0L ];
    static immutable real[13] postab =
        [ 1e+4096L,1e+2048L,1e+1024L,1e+512L,1e+256L,1e+128L,1e+64L,1e+32L,
                1e+16L,1e+8L,1e+4L,1e+2L,1e+1L ];

    ConvException bailOut()(string msg = null, string fn = __FILE__, size_t ln = __LINE__)
    {
        if (msg == null)
            msg = "Floating point conversion error";
        return new ConvException(text(msg, " for input \"", source, "\"."), fn, ln);
    }

    enforce(!p.empty, bailOut());


    size_t count = 0;
    bool sign = false;
    switch (p.front)
    {
    case '-':
        sign = true;
        ++count;
        p.popFront();
        enforce(!p.empty, bailOut());
        if (toLower(p.front) == 'i')
            goto case 'i';
        break;
    case '+':
        ++count;
        p.popFront();
        enforce(!p.empty, bailOut());
        break;
    case 'i': case 'I':
        // inf
        ++count;
        p.popFront();
        enforce(!p.empty && toUpper(p.front) == 'N',
               bailOut("error converting input to floating point"));
        ++count;
        p.popFront();
        enforce(!p.empty && toUpper(p.front) == 'F',
               bailOut("error converting input to floating point"));
        // skip past the last 'f'
        ++count;
        p.popFront();
        advanceSource();
        static if (doCount)
        {
            return tuple!("data", "count")(sign ? -Target.infinity : Target.infinity, count);
        }
        else
        {
            return sign ? -Target.infinity : Target.infinity;
        }
    default: {}
    }

    bool isHex = false;
    bool startsWithZero = p.front == '0';
    if (startsWithZero)
    {
        ++count;
        p.popFront();
        if (p.empty)
        {
            advanceSource();
            static if (doCount)
            {
                return tuple!("data", "count")(cast (Target) (sign ? -0.0 : 0.0), count);
            }
            else
            {
                return sign ? -0.0 : 0.0;
            }
        }

        isHex = p.front == 'x' || p.front == 'X';
        if (isHex)
        {
            ++count;
            p.popFront();
        }
    }
    else if (toLower(p.front) == 'n')
    {
        // nan
        ++count;
        p.popFront();
        enforce(!p.empty && toUpper(p.front) == 'A',
               bailOut("error converting input to floating point"));
        ++count;
        p.popFront();
        enforce(!p.empty && toUpper(p.front) == 'N',
               bailOut("error converting input to floating point"));
        // skip past the last 'n'
        ++count;
        p.popFront();
        advanceSource();
        static if (doCount)
        {
            return tuple!("data", "count")(Target.nan, count);
        }
        else
        {
            return typeof(return).nan;
        }
    }

    /*
     * The following algorithm consists of 2 steps:
     * 1) parseDigits processes the textual input into msdec and possibly
     *    lsdec/msscale variables, followed by the exponent parser which sets
     *    exp below.
     *    Hex: input is 0xaaaaa...p+000... where aaaa is the mantissa in hex
     *    and 000 is the exponent in decimal format with base 2.
     *    Decimal: input is 0.00333...p+000... where 0.0033 is the mantissa
     *    in decimal and 000 is the exponent in decimal format with base 10.
     * 2) Convert msdec/lsdec and exp into native real format
     */

    real ldval = 0.0;
    char dot = 0;                        /* if decimal point has been seen */
    int exp = 0;
    ulong msdec = 0, lsdec = 0;
    ulong msscale = 1;
    bool sawDigits;

    enum { hex, decimal }

    // sets msdec, lsdec/msscale, and sawDigits by parsing the mantissa digits
    void parseDigits(alias FloatFormat)()
    {
        static if (FloatFormat == hex)
        {
            enum uint base = 16;
            enum ulong msscaleMax = 0x1000_0000_0000_0000UL; // largest power of 16 a ulong holds
            enum ubyte expIter = 4; // iterate the base-2 exponent by 4 for every hex digit
            alias checkDigit = isHexDigit;
            /*
             * convert letter to binary representation: First clear bit
             * to convert lower space chars to upperspace, then -('A'-10)
             * converts letter A to 10, letter B to 11, ...
             */
            alias convertDigit = (int x) => isAlpha(x) ? ((x & ~0x20) - ('A' - 10)) : x - '0';
            sawDigits = false;
        }
        else static if (FloatFormat == decimal)
        {
            enum uint base = 10;
            enum ulong msscaleMax = 10_000_000_000_000_000_000UL; // largest power of 10 a ulong holds
            enum ubyte expIter = 1; // iterate the base-10 exponent once for every decimal digit
            alias checkDigit = isDigit;
            alias convertDigit = (int x) => x - '0';
            // Used to enforce that any mantissa digits are present
            sawDigits = startsWithZero;
        }
        else
            static assert(false, "Unrecognized floating-point format used.");

        while (!p.empty)
        {
            int i = p.front;
            while (checkDigit(i))
            {
                sawDigits = true;        /* must have at least 1 digit   */

                i = convertDigit(i);

                if (msdec < (ulong.max - base)/base)
                {
                    // For base 16: Y = ... + y3*16^3 + y2*16^2 + y1*16^1 + y0*16^0
                    msdec = msdec * base + i;
                }
                else if (msscale < msscaleMax)
                {
                    lsdec = lsdec * base + i;
                    msscale *= base;
                }
                else
                {
                    exp += expIter;
                }
                exp -= dot;
                ++count;
                p.popFront();
                if (p.empty)
                    break;
                i = p.front;
                if (i == '_')
                {
                    ++count;
                    p.popFront();
                    if (p.empty)
                        break;
                    i = p.front;
                }
            }
            if (i == '.' && !dot)
            {
                ++count;
                p.popFront();
                dot += expIter;
            }
            else
                break;
        }

        // Have we seen any mantissa digits so far?
        enforce(sawDigits, bailOut("no digits seen"));
        static if (FloatFormat == hex)
            enforce(!p.empty && (p.front == 'p' || p.front == 'P'),
                    bailOut("Floating point parsing: exponent is required"));
    }

    if (isHex)
        parseDigits!hex;
    else
        parseDigits!decimal;

    if (isHex || (!p.empty && (p.front == 'e' || p.front == 'E')))
    {
        char sexp = 0;
        int e = 0;

        ++count;
        p.popFront();
        enforce(!p.empty, new ConvException("Unexpected end of input"));
        switch (p.front)
        {
            case '-':    sexp++;
                         goto case;
            case '+':    ++count;
                         p.popFront();
                         break;
            default: {}
        }
        sawDigits = false;
        while (!p.empty && isDigit(p.front))
        {
            if (e < 0x7FFFFFFF / 10 - 10)   // prevent integer overflow
            {
                e = e * 10 + p.front - '0';
            }
            ++count;
            p.popFront();
            sawDigits = true;
        }
        exp += (sexp) ? -e : e;
        enforce(sawDigits, new ConvException("No digits seen."));
    }

    ldval = msdec;
    if (msscale != 1)               /* if stuff was accumulated in lsdec */
        ldval = ldval * msscale + lsdec;
    if (isHex)
    {
        import core.math : ldexp;

        // Exponent is power of 2, not power of 10
        ldval = ldexp(ldval,exp);
    }
    else if (ldval)
    {
        uint u = 0;
        int pow = 4096;

        while (exp > 0)
        {
            while (exp >= pow)
            {
                ldval *= postab[u];
                exp -= pow;
            }
            pow >>= 1;
            u++;
        }
        while (exp < 0)
        {
            while (exp <= -pow)
            {
                ldval *= negtab[u];
                enforce(ldval != 0, new ConvException("Range error"));
                exp += pow;
            }
            pow >>= 1;
            u++;
        }
    }

    Target result = cast(Target) (sign ? -ldval : ldval);

    // if overflow occurred
    import std.math.traits : isFinite;
    enforce(isFinite(result), new ConvException("Range error"));

    advanceSource();
    static if (doCount)
    {
        return tuple!("data", "count")(result, count);
    }
    else
    {
        return result;
    }
}


///
@safe unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isNaN, isInfinity;
    import std.typecons : Flag, Yes, No;
    auto str = "123.456";
    assert(parse!double(str).isClose(123.456));
    auto str2 = "123.456";
    assert(parse!(double, string, No.doCount)(str2).isClose(123.456));
    auto str3 = "123.456";
    auto r = parse!(double, string, Yes.doCount)(str3);
    assert(r.data.isClose(123.456));
    assert(r.count == 7);
    auto str4 = "-123.456";
    r = parse!(double, string, Yes.doCount)(str4);
    assert(r.data.isClose(-123.456));
    assert(r.count == 8);
    auto str5 = "+123.456";
    r = parse!(double, string, Yes.doCount)(str5);
    assert(r.data.isClose(123.456));
    assert(r.count == 8);
    auto str6 = "inf0";
    r = parse!(double, string, Yes.doCount)(str6);
    assert(isInfinity(r.data) && r.count == 3 && str6 == "0");
    auto str7 = "-0";
    auto r2 = parse!(float, string, Yes.doCount)(str7);
    assert(r2.data.isClose(0.0) && r2.count == 2);
    auto str8 = "nan";
    auto r3 = parse!(real, string, Yes.doCount)(str8);
    assert(isNaN(r3.data) && r3.count == 3);
}

@safe unittest
{
    import std.exception;
    import std.math.traits : isNaN, isInfinity;
    import std.math.algebraic : fabs;

    // Compare reals with given precision
    bool feq(in real rx, in real ry, in real precision = 0.000001L)
    {
        if (rx == ry)
            return 1;

        if (isNaN(rx))
            return cast(bool) isNaN(ry);

        if (isNaN(ry))
            return 0;

        return cast(bool)(fabs(rx - ry) <= precision);
    }

    // Make given typed literal
    F Literal(F)(F f)
    {
        return f;
    }

    static foreach (Float; AliasSeq!(float, double, real))
    {
        assert(to!Float("123") == Literal!Float(123));
        assert(to!Float("+123") == Literal!Float(+123));
        assert(to!Float("-123") == Literal!Float(-123));
        assert(to!Float("123e2") == Literal!Float(123e2));
        assert(to!Float("123e+2") == Literal!Float(123e+2));
        assert(to!Float("123e-2") == Literal!Float(123e-2L));
        assert(to!Float("123.") == Literal!Float(123.0));
        assert(to!Float(".375") == Literal!Float(.375));

        assert(to!Float("1.23375E+2") == Literal!Float(1.23375E+2));

        assert(to!Float("0") is 0.0);
        assert(to!Float("-0") is -0.0);

        assert(isNaN(to!Float("nan")));

        assertThrown!ConvException(to!Float("\x00"));
    }

    // min and max
    float f = to!float("1.17549e-38");
    assert(feq(cast(real) f, cast(real) 1.17549e-38));
    assert(feq(cast(real) f, cast(real) float.min_normal));
    f = to!float("3.40282e+38");
    assert(to!string(f) == to!string(3.40282e+38));

    // min and max
    double d = to!double("2.22508e-308");
    assert(feq(cast(real) d, cast(real) 2.22508e-308));
    assert(feq(cast(real) d, cast(real) double.min_normal));
    d = to!double("1.79769e+308");
    assert(to!string(d) == to!string(1.79769e+308));
    assert(to!string(d) == to!string(double.max));

    auto z = real.max / 2L;
    static assert(is(typeof(z) == real));
    assert(!isNaN(z));
    assert(!isInfinity(z));
    string a = to!string(z);
    real b = to!real(a);
    string c = to!string(b);

    assert(c == a, "\n" ~ c ~ "\n" ~ a);

    assert(to!string(to!real(to!string(real.max / 2L))) == to!string(real.max / 2L));

    // min and max
    real r = to!real(to!string(real.min_normal));
    version (NetBSD)
    {
        // NetBSD notice
        // to!string returns 3.3621e-4932L. It is less than real.min_normal and it is subnormal value
        // Simple C code
        //     long double rd = 3.3621e-4932L;
        //     printf("%Le\n", rd);
        // has unexpected result: 1.681050e-4932
        //
        // Bug report: http://gnats.netbsd.org/cgi-bin/query-pr-single.pl?number=50937
    }
    else
    {
        assert(to!string(r) == to!string(real.min_normal));
    }
    r = to!real(to!string(real.max));
    assert(to!string(r) == to!string(real.max));

    real pi = 3.1415926535897932384626433832795028841971693993751L;
    string fullPrecision = "3.1415926535897932384626433832795028841971693993751";
    assert(feq(parse!real(fullPrecision), pi, 2*real.epsilon));
    string fullPrecision2 = "3.1415926535897932384626433832795028841971693993751";
    assert(feq(parse!(real, string, No.doCount)(fullPrecision2), pi, 2*real.epsilon));
    string fullPrecision3= "3.1415926535897932384626433832795028841971693993751";
    auto len = fullPrecision3.length;
    auto res = parse!(real, string, Yes.doCount)(fullPrecision3);
    assert(feq(res.data, pi, 2*real.epsilon));
    assert(res.count == len);

    real x = 0x1.FAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAAFAAFAFAFAFAFAFAFAP-252L;
    string full = "0x1.FAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAAFAAFAFAFAFAFAFAFAP-252";
    assert(parse!real(full) == x);
    string full2 = "0x1.FAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAAFAAFAFAFAFAFAFAFAP-252";
    assert(parse!(real, string, No.doCount)(full2) == x);
    string full3 = "0x1.FAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAAFAAFAFAFAFAFAFAFAP-252";
    auto len2 = full3.length;
    assert(parse!(real, string, Yes.doCount)(full3) == tuple(x, len2));
}

// Tests for the double implementation
@system unittest
{
    // @system because strtod is not @safe.
    import std.math.traits : floatTraits, RealFormat;

    static if (floatTraits!real.realFormat == RealFormat.ieeeDouble)
    {
        import core.stdc.stdlib, std.exception, std.math;

        //Should be parsed exactly: 53 bit mantissa
        string s = "0x1A_BCDE_F012_3456p10";
        auto x = parse!real(s);
        assert(x == 0x1A_BCDE_F012_3456p10L);
        //1 bit is implicit
        assert(((*cast(ulong*)&x) & 0x000F_FFFF_FFFF_FFFF) == 0xA_BCDE_F012_3456);
        assert(strtod("0x1ABCDEF0123456p10", null) == x);

        s = "0x1A_BCDE_F012_3456p10";
        auto len = s.length;
        assert(parse!(real, string, Yes.doCount)(s) == tuple(x, len));

        //Should be parsed exactly: 10 bit mantissa
        s = "0x3FFp10";
        x = parse!real(s);
        assert(x == 0x03FFp10);
        //1 bit is implicit
        assert(((*cast(ulong*)&x) & 0x000F_FFFF_FFFF_FFFF) == 0x000F_F800_0000_0000);
        assert(strtod("0x3FFp10", null) == x);

        //60 bit mantissa, round up
        s = "0xFFF_FFFF_FFFF_FFFFp10";
        x = parse!real(s);
        assert(isClose(x, 0xFFF_FFFF_FFFF_FFFFp10));
        //1 bit is implicit
        assert(((*cast(ulong*)&x) & 0x000F_FFFF_FFFF_FFFF) == 0x0000_0000_0000_0000);
        assert(strtod("0xFFFFFFFFFFFFFFFp10", null) == x);

        //60 bit mantissa, round down
        s = "0xFFF_FFFF_FFFF_FF90p10";
        x = parse!real(s);
        assert(isClose(x, 0xFFF_FFFF_FFFF_FF90p10));
        //1 bit is implicit
        assert(((*cast(ulong*)&x) & 0x000F_FFFF_FFFF_FFFF) == 0x000F_FFFF_FFFF_FFFF);
        assert(strtod("0xFFFFFFFFFFFFF90p10", null) == x);

        //61 bit mantissa, round up 2
        s = "0x1F0F_FFFF_FFFF_FFFFp10";
        x = parse!real(s);
        assert(isClose(x, 0x1F0F_FFFF_FFFF_FFFFp10));
        //1 bit is implicit
        assert(((*cast(ulong*)&x) & 0x000F_FFFF_FFFF_FFFF) == 0x000F_1000_0000_0000);
        assert(strtod("0x1F0FFFFFFFFFFFFFp10", null) == x);

        //61 bit mantissa, round down 2
        s = "0x1F0F_FFFF_FFFF_FF10p10";
        x = parse!real(s);
        assert(isClose(x, 0x1F0F_FFFF_FFFF_FF10p10));
        //1 bit is implicit
        assert(((*cast(ulong*)&x) & 0x000F_FFFF_FFFF_FFFF) == 0x000F_0FFF_FFFF_FFFF);
        assert(strtod("0x1F0FFFFFFFFFFF10p10", null) == x);

        //Huge exponent
        s = "0x1F_FFFF_FFFF_FFFFp900";
        x = parse!real(s);
        assert(strtod("0x1FFFFFFFFFFFFFp900", null) == x);

        //exponent too big -> converror
        s = "";
        assertThrown!ConvException(x = parse!real(s));
        assert(strtod("0x1FFFFFFFFFFFFFp1024", null) == real.infinity);

        //-exponent too big -> 0
        s = "0x1FFFFFFFFFFFFFp-2000";
        x = parse!real(s);
        assert(x == 0);
        assert(strtod("0x1FFFFFFFFFFFFFp-2000", null) == x);

        s = "0x1FFFFFFFFFFFFFp-2000";
        len = s.length;
        assert(parse!(real, string, Yes.doCount)(s) == tuple(x, len));
    }
}

@system unittest
{
    import core.stdc.errno;
    import core.stdc.stdlib;
    import std.math.traits : floatTraits, RealFormat;

    errno = 0;  // In case it was set by another unittest in a different module.
    struct longdouble
    {
        static if (floatTraits!real.realFormat == RealFormat.ieeeQuadruple)
        {
            ushort[8] value;
        }
        else static if (floatTraits!real.realFormat == RealFormat.ieeeExtended ||
                        floatTraits!real.realFormat == RealFormat.ieeeExtended53)
        {
            ushort[5] value;
        }
        else static if (floatTraits!real.realFormat == RealFormat.ieeeDouble)
        {
            ushort[4] value;
        }
        else
            static assert(false, "Not implemented");
    }

    real ld;
    longdouble x;
    real ld1;
    longdouble x1;
    int i;

    static if (floatTraits!real.realFormat == RealFormat.ieeeQuadruple)
        enum s = "0x1.FFFFFFFFFFFFFFFFFFFFFFFFFFFFp-16382";
    else static if (floatTraits!real.realFormat == RealFormat.ieeeExtended)
        enum s = "0x1.FFFFFFFFFFFFFFFEp-16382";
    else static if (floatTraits!real.realFormat == RealFormat.ieeeExtended53)
        enum s = "0x1.FFFFFFFFFFFFFFFEp-16382";
    else static if (floatTraits!real.realFormat == RealFormat.ieeeDouble)
        enum s = "0x1.FFFFFFFFFFFFFFFEp-1000";
    else
        static assert(false, "Floating point format for real not supported");

    auto s2 = s.idup;
    ld = parse!real(s2);
    assert(s2.empty);
    x = *cast(longdouble *)&ld;

    static if (floatTraits!real.realFormat == RealFormat.ieeeExtended)
    {
        version (CRuntime_Microsoft)
            ld1 = 0x1.FFFFFFFFFFFFFFFEp-16382L; // strtold currently mapped to strtod
        else
            ld1 = strtold(s.ptr, null);
    }
    else static if (floatTraits!real.realFormat == RealFormat.ieeeExtended53)
        ld1 = 0x1.FFFFFFFFFFFFFFFEp-16382L; // strtold rounds to 53 bits.
    else
        ld1 = strtold(s.ptr, null);

    x1 = *cast(longdouble *)&ld1;
    assert(x1 == x && ld1 == ld);

    assert(!errno);

    s2 = "1.0e5";
    ld = parse!real(s2);
    assert(s2.empty);
    x = *cast(longdouble *)&ld;
    ld1 = strtold("1.0e5", null);
    x1 = *cast(longdouble *)&ld1;
}

@safe pure unittest
{
    import std.exception;

    // https://issues.dlang.org/show_bug.cgi?id=4959
    {
        auto s = "0 ";
        auto x = parse!double(s);
        assert(s == " ");
        assert(x == 0.0);
    }
    {
        auto s = "0 ";
        auto x = parse!(double, string, Yes.doCount)(s);
        assert(s == " ");
        assert(x == tuple(0.0, 1));
    }

    // https://issues.dlang.org/show_bug.cgi?id=3369
    assert(to!float("inf") == float.infinity);
    assert(to!float("-inf") == -float.infinity);

    // https://issues.dlang.org/show_bug.cgi?id=6160
    assert(6_5.536e3L == to!real("6_5.536e3"));                     // 2^16
    assert(0x1000_000_000_p10 == to!real("0x1000_000_000_p10"));    // 7.03687e+13

    // https://issues.dlang.org/show_bug.cgi?id=6258
    assertThrown!ConvException(to!real("-"));
    assertThrown!ConvException(to!real("in"));

    // https://issues.dlang.org/show_bug.cgi?id=7055
    assertThrown!ConvException(to!float("INF2"));

    //extra stress testing
    auto ssOK    = ["1.", "1.1.1", "1.e5", "2e1e", "2a", "2e1_1", "3.4_",
                    "inf", "-inf", "infa", "-infa", "inf2e2", "-inf2e2",
                    "nan", "-NAN", "+NaN", "-nAna", "NAn2e2", "-naN2e2"];
    auto ssKO    = ["", " ", "2e", "2e+", "2e-", "2ee", "2e++1", "2e--1", "2e_1",
                    "+inf", "-in", "I", "+N", "-NaD", "0x3.F"];
    foreach (s; ssOK)
        parse!double(s);
    foreach (s; ssKO)
        assertThrown!ConvException(parse!double(s));
}

@safe unittest // https://issues.dlang.org/show_bug.cgi?id=22637
{
    import std.exception : assertThrown, assertNotThrown;
    auto src = "9991232549867999698999493543521458974414359998784641646846435132132543645435456345634541999999999999999"
    ~ "9999999943321231321311999231345312413646846354354354399999934153465464654646464654134135354199999999996515734999"
    ~ "9999999320135273486741354354731567431324134999999999999999999999999999999999999999999999135411.9";
    assertThrown!ConvException(parse!double(src));
    static if (real.max_10_exp > 310) assertNotThrown!ConvException(parse!real(src));
}

/**
Parses one character from a character range.

Params:
    Target = the type to convert to
    s = the lvalue of an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    doCount = the flag for deciding to report the number of consumed characters

Returns:
$(UL
    $(LI A character of type `Target` if `doCount` is set to `No.doCount`)
    $(LI A `tuple` containing a character of type `Target` and a `size_t` if `doCount` is set to `Yes.doCount`))

Throws:
    A $(LREF ConvException) if the range is empty.
 */
auto parse(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source s)
if (staticIndexOf!(immutable Target, immutable dchar, immutable ElementEncodingType!Source) >= 0 &&
    isSomeString!Source && !is(Source == enum))
{
    if (s.empty)
        throw convError!(Source, Target)(s);
    static if (is(immutable Target == immutable dchar))
    {
        Target result = s.front;
        s.popFront();
        static if (doCount)
        {
            return tuple!("data", "count")(result, 1);
        }
        else
        {
            return result;
        }

    }
    else
    {
        // Special case: okay so parse a Char off a Char[]
        Target result = s[0];
        s = s[1 .. $];
        static if (doCount)
        {
            return tuple!("data", "count")(result, 1);
        }
        else
        {
            return result;
        }
    }
}

@safe pure unittest
{
    static foreach (Str; AliasSeq!(string, wstring, dstring))
    {
        static foreach (Char; AliasSeq!(char, wchar, dchar))
        {{
            static if (is(immutable Char == immutable dchar) ||
                       Char.sizeof == ElementEncodingType!Str.sizeof)
            {
                Str s = "aaa";
                assert(parse!Char(s) == 'a');
                assert(s == "aa");
                assert(parse!(Char, typeof(s), No.doCount)(s) == 'a');
                assert(s == "a");
                assert(parse!(Char, typeof(s), Yes.doCount)(s) == tuple('a', 1) && s == "");
            }
        }}
    }
}

/// ditto
auto parse(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source s)
if (isSomeChar!Target && Target.sizeof >= ElementType!Source.sizeof && !is(Target == enum) &&
    !isSomeString!Source && isInputRange!Source && isSomeChar!(ElementType!Source))
{
    if (s.empty)
        throw convError!(Source, Target)(s);
    Target result = s.front;
    s.popFront();
    static if (doCount)
    {
        return tuple!("data", "count")(result, 1);
    }
    else
    {
        return result;
    }
}

///
@safe pure unittest
{
    import std.typecons : Flag, Yes, No;
    auto s = "Hello, World!";
    char first = parse!char(s);
    assert(first == 'H');
    assert(s == "ello, World!");
    char second = parse!(char, string, No.doCount)(s);
    assert(second == 'e');
    assert(s == "llo, World!");
    auto third = parse!(char, string, Yes.doCount)(s);
    assert(third.data == 'l' && third.count == 1);
    assert(s == "lo, World!");
}


/*
    Tests for to!bool and parse!bool
*/
@safe pure unittest
{
    import std.exception;

    assert(to!bool("TruE") == true);
    assert(to!bool("faLse"d) == false);
    assertThrown!ConvException(to!bool("maybe"));

    auto t = "TrueType";
    assert(parse!bool(t) == true);
    assert(t == "Type");

    auto f = "False killer whale"d;
    assert(parse!bool(f) == false);
    assert(f == " killer whale"d);

    f = "False killer whale"d;
    assert(parse!(bool, dstring, Yes.doCount)(f) == tuple(false, 5));
    assert(f == " killer whale"d);

    auto m = "maybe";
    assertThrown!ConvException(parse!bool(m));
    assertThrown!ConvException(parse!(bool, string, Yes.doCount)(m));
    assert(m == "maybe");  // m shouldn't change on failure

    auto s = "true";
    auto b = parse!(const(bool))(s);
    assert(b == true);
}

/**
Parses `typeof(null)` from a character range if the range
spells `"null"`. This function is case insensitive.

Params:
    Target = the type to convert to
    s = the lvalue of an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    doCount = the flag for deciding to report the number of consumed characters

Returns:
$(UL
    $(LI `null` if `doCount` is set to `No.doCount`)
    $(LI A `tuple` containing `null` and a `size_t` if `doCount` is set to `Yes.doCount`))

Throws:
    A $(LREF ConvException) if the range doesn't represent `null`.
 */
auto parse(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source s)
if (is(immutable Target == immutable typeof(null)) &&
    isInputRange!Source &&
    isSomeChar!(ElementType!Source))
{
    import std.ascii : toLower;
    foreach (c; "null")
    {
        if (s.empty || toLower(s.front) != c)
            throw parseError("null should be case-insensitive 'null'");
        s.popFront();
    }
    static if (doCount)
    {
        return tuple!("data", "count")(null, 4);
    }
    else
    {
        return null;
    }
}

///
@safe pure unittest
{
    import std.exception : assertThrown;
    import std.typecons : Flag, Yes, No;

    alias NullType = typeof(null);
    auto s1 = "null";
    assert(parse!NullType(s1) is null);
    assert(s1 == "");

    auto s2 = "NUll"d;
    assert(parse!NullType(s2) is null);
    assert(s2 == "");

    auto s3 = "nuLlNULl";
    assert(parse!(NullType, string, No.doCount)(s3) is null);
    auto r = parse!(NullType, string, Yes.doCount)(s3);
    assert(r.data is null && r.count == 4);

    auto m = "maybe";
    assertThrown!ConvException(parse!NullType(m));
    assertThrown!ConvException(parse!(NullType, string, Yes.doCount)(m));
    assert(m == "maybe");  // m shouldn't change on failure

    auto s = "NULL";
    assert(parse!(const NullType)(s) is null);
}

//Used internally by parse Array/AA, to remove ascii whites
package auto skipWS(R, Flag!"doCount" doCount = No.doCount)(ref R r)
{
    import std.ascii : isWhite;
    static if (isSomeString!R)
    {
        //Implementation inspired from stripLeft.
        foreach (i, c; r)
        {
            if (!isWhite(c))
            {
                r = r[i .. $];
                static if (doCount)
                {
                    return i;
                }
                else
                {
                    return;
                }
            }
        }
        auto len = r.length;
        r = r[0 .. 0]; //Empty string with correct type.
        static if (doCount)
        {
            return len;
        }
        else
        {
            return;
        }
    }
    else
    {
        size_t i = 0;
        for (; !r.empty && isWhite(r.front); r.popFront(), ++i)
        { }
        static if (doCount)
        {
            return i;
        }
    }
}

/**
 * Parses an array from a string given the left bracket (default $(D
 * '[')), right bracket (default `']'`), and element separator (by
 * default `','`). A trailing separator is allowed.
 *
 * Params:
 *     s = The string to parse
 *     lbracket = the character that starts the array
 *     rbracket = the character that ends the array
 *     comma = the character that separates the elements of the array
 *     doCount = the flag for deciding to report the number of consumed characters
 *
 * Returns:
 $(UL
 *     $(LI An array of type `Target` if `doCount` is set to `No.doCount`)
 *     $(LI A `tuple` containing an array of type `Target` and a `size_t` if `doCount` is set to `Yes.doCount`))
 */
auto parse(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source s, dchar lbracket = '[',
    dchar rbracket = ']', dchar comma = ',')
if (isDynamicArray!Target && !is(Target == enum) &&
    isSomeString!Source && !is(Source == enum))
{
    import std.array : appender;

    auto result = appender!Target();

    parseCheck!s(lbracket);
    size_t count = 1 + skipWS!(Source, Yes.doCount)(s);
    if (s.empty)
        throw convError!(Source, Target)(s);
    if (s.front == rbracket)
    {
        s.popFront();
        static if (doCount)
        {
            return tuple!("data", "count")(result.data, ++count);
        }
        else
        {
            return result.data;
        }
    }
    for (;; s.popFront(), count += 1 + skipWS!(Source, Yes.doCount)(s))
    {
        if (!s.empty && s.front == rbracket)
            break;
        auto r = parseElement!(WideElementType!Target, Source, Yes.doCount)(s);
        result ~= r.data;
        count += r.count + skipWS!(Source, Yes.doCount)(s);
        if (s.empty)
            throw convError!(Source, Target)(s);
        if (s.front != comma)
            break;
    }
    parseCheck!s(rbracket);
    static if (doCount)
    {
        return tuple!("data", "count")(result.data, ++count);
    }
    else
    {
        return result.data;
    }
}

///
@safe pure unittest
{
    import std.typecons : Flag, Yes, No;
    auto s1 = `[['h', 'e', 'l', 'l', 'o'], "world"]`;
    auto a1 = parse!(string[])(s1);
    assert(a1 == ["hello", "world"]);

    auto s2 = `["aaa", "bbb", "ccc"]`;
    auto a2 = parse!(string[])(s2);
    assert(a2 == ["aaa", "bbb", "ccc"]);

    auto s3 = `[['h', 'e', 'l', 'l', 'o'], "world"]`;
    auto len3 = s3.length;
    auto a3 = parse!(string[], string, Yes.doCount)(s3);
    assert(a3.data == ["hello", "world"]);
    assert(a3.count == len3);
}

// https://issues.dlang.org/show_bug.cgi?id=9615
@safe unittest
{
    import std.typecons : Flag, Yes, No, tuple;
    string s0 = "[1,2, ]";
    string s1 = "[1,2, \t\v\r\n]";
    string s2 = "[1,2]";
    assert(s0.parse!(int[]) == [1,2]);
    assert(s1.parse!(int[]) == [1,2]);
    assert(s2.parse!(int[]) == [1,2]);

    s0 = "[1,2, ]";
    auto len0 = s0.length;
    s1 = "[1,2, \t\v\r\n]";
    auto len1 = s1.length;
    s2 = "[1,2]";
    auto len2 = s2.length;
    assert(s0.parse!(int[], string, Yes.doCount) == tuple([1,2], len0));
    assert(s1.parse!(int[], string, Yes.doCount) == tuple([1,2], len1));
    assert(s2.parse!(int[], string, Yes.doCount) == tuple([1,2], len2));

    string s3 = `["a","b",]`;
    string s4 = `["a","b"]`;
    assert(s3.parse!(string[]) == ["a","b"]);
    assert(s4.parse!(string[]) == ["a","b"]);

    s3 = `["a","b",]`;
    auto len3 = s3.length;
    assert(s3.parse!(string[], string, Yes.doCount) == tuple(["a","b"], len3));

    s3 = `[    ]`;
    assert(tuple([], s3.length) == s3.parse!(string[], string, Yes.doCount));

    import std.exception : assertThrown;
    string s5 = "[,]";
    string s6 = "[, \t,]";
    assertThrown!ConvException(parse!(string[])(s5));
    assertThrown!ConvException(parse!(int[])(s6));

    s5 = "[,]";
    s6 = "[,·\t,]";
    assertThrown!ConvException(parse!(string[], string, Yes.doCount)(s5));
    assertThrown!ConvException(parse!(string[], string, Yes.doCount)(s6));
}

@safe unittest
{
    int[] a = [1, 2, 3, 4, 5];
    auto s = to!string(a);
    assert(to!(int[])(s) == a);
}

@safe unittest
{
    int[][] a = [ [1, 2] , [3], [4, 5] ];
    auto s = to!string(a);
    assert(to!(int[][])(s) == a);
}

@safe unittest
{
    int[][][] ia = [ [[1,2],[3,4],[5]] , [[6],[],[7,8,9]] , [[]] ];

    char[] s = to!(char[])(ia);
    int[][][] ia2;

    ia2 = to!(typeof(ia2))(s);
    assert( ia == ia2);
}

@safe pure unittest
{
    import std.exception;
    import std.typecons : Flag, Yes, No;

    //Check proper failure
    auto s = "[ 1 , 2 , 3 ]";
    auto s2 = s.save;
    foreach (i ; 0 .. s.length-1)
    {
        auto ss = s[0 .. i];
        assertThrown!ConvException(parse!(int[])(ss));
        assertThrown!ConvException(parse!(int[], string, Yes.doCount)(ss));
    }
    int[] arr = parse!(int[])(s);
    auto arr2 = parse!(int[], string, Yes.doCount)(s2);
    arr = arr2.data;
}

@safe pure unittest
{
    //Checks parsing of strings with escaped characters
    string s1 = `[
        "Contains a\0null!",
        "tab\there",
        "line\nbreak",
        "backslash \\ slash / question \?",
        "number \x35 five",
        "unicode \u65E5 sun",
        "very long \U000065E5 sun"
    ]`;

    //Note: escaped characters purposefully replaced and isolated to guarantee
    //there are no typos in the escape syntax
    string[] s2 = [
        "Contains a" ~ '\0' ~ "null!",
        "tab" ~ '\t' ~ "here",
        "line" ~ '\n' ~ "break",
        "backslash " ~ '\\' ~ " slash / question ?",
        "number 5 five",
        "unicode 日 sun",
        "very long 日 sun"
    ];
    string s3 = s1.save;
    assert(s2 == parse!(string[])(s1));
    assert(s1.empty);
    assert(tuple(s2, s3.length) == parse!(string[], string, Yes.doCount)(s3));
}

/// ditto
auto parse(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source s, dchar lbracket = '[',
    dchar rbracket = ']', dchar comma = ',')
if (isStaticArray!Target && !is(Target == enum) &&
    isExactSomeString!Source)
{
    static if (hasIndirections!Target)
        Target result = Target.init[0].init;
    else
        Target result = void;

    parseCheck!s(lbracket);
    size_t count = 1 + skipWS!(Source, Yes.doCount)(s);
    if (s.empty)
        throw convError!(Source, Target)(s);
    if (s.front == rbracket)
    {
        static if (result.length != 0)
            goto Lmanyerr;
        else
        {
            s.popFront();
            static if (doCount)
            {
                return tuple!("data", "count")(result, ++count);
            }
            else
            {
                return result;
            }
        }
    }
    for (size_t i = 0; ; s.popFront(), count += 1 + skipWS!(Source, Yes.doCount)(s))
    {
        if (i == result.length)
            goto Lmanyerr;
        auto r = parseElement!(ElementType!Target, Source, Yes.doCount)(s);
        result[i++] = r.data;
        count += r.count + skipWS!(Source, Yes.doCount)(s);
        if (s.empty)
            throw convError!(Source, Target)(s);
        if (s.front != comma)
        {
            if (i != result.length)
                goto Lfewerr;
            break;
        }
    }
    parseCheck!s(rbracket);
    static if (doCount)
    {
        return tuple!("data", "count")(result, ++count);
    }
    else
    {
        return result;
    }


Lmanyerr:
    throw parseError(text("Too many elements in input, ", result.length, " elements expected."));

Lfewerr:
    throw parseError(text("Too few elements in input, ", result.length, " elements expected."));
}

@safe pure unittest
{
    import std.exception;

    auto s1 = "[1,2,3,4]";
    auto sa1 = parse!(int[4])(s1);
    assert(sa1 == [1,2,3,4]);
    s1 = "[1,2,3,4]";
    assert(tuple([1,2,3,4], s1.length) == parse!(int[4], string, Yes.doCount)(s1));

    auto s2 = "[[1],[2,3],[4]]";
    auto sa2 = parse!(int[][3])(s2);
    assert(sa2 == [[1],[2,3],[4]]);
    s2 = "[[1],[2,3],[4]]";
    assert(tuple([[1],[2,3],[4]], s2.length) == parse!(int[][3], string, Yes.doCount)(s2));

    auto s3 = "[1,2,3]";
    assertThrown!ConvException(parse!(int[4])(s3));
    assertThrown!ConvException(parse!(int[4], string, Yes.doCount)(s3));

    auto s4 = "[1,2,3,4,5]";
    assertThrown!ConvException(parse!(int[4])(s4));
    assertThrown!ConvException(parse!(int[4], string, Yes.doCount)(s4));
}

/**
 * Parses an associative array from a string given the left bracket (default $(D
 * '[')), right bracket (default `']'`), key-value separator (default $(D
 * ':')), and element seprator (by default `','`).
 *
 * Params:
 *     s = the string to parse
 *     lbracket = the character that starts the associative array
 *     rbracket = the character that ends the associative array
 *     keyval = the character that associates the key with the value
 *     comma = the character that separates the elements of the associative array
 *     doCount = the flag for deciding to report the number of consumed characters
 *
 * Returns:
 $(UL
 *     $(LI An associative array of type `Target` if `doCount` is set to `No.doCount`)
 *     $(LI A `tuple` containing an associative array of type `Target` and a `size_t`
 *     if `doCount` is set to `Yes.doCount`))
 */
auto parse(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source s, dchar lbracket = '[',
                             dchar rbracket = ']', dchar keyval = ':', dchar comma = ',')
if (isAssociativeArray!Target && !is(Target == enum) &&
    isSomeString!Source && !is(Source == enum))
{
    alias KeyType = typeof(Target.init.keys[0]);
    alias ValType = typeof(Target.init.values[0]);

    Target result;

    parseCheck!s(lbracket);
    size_t count = 1 + skipWS!(Source, Yes.doCount)(s);
    if (s.empty)
        throw convError!(Source, Target)(s);
    if (s.front == rbracket)
    {
        s.popFront();
        static if (doCount)
        {
            return tuple!("data", "count")(result, ++count);
        }
        else
        {
            return result;
        }
    }
    for (;; s.popFront(), count += 1 + skipWS!(Source, Yes.doCount)(s))
    {
        auto key = parseElement!(KeyType, Source, Yes.doCount)(s);
        count += key.count + skipWS!(Source, Yes.doCount)(s);
        parseCheck!s(keyval);
        count += 1 + skipWS!(Source, Yes.doCount)(s);
        auto val = parseElement!(ValType, Source, Yes.doCount)(s);
        count += val.count + skipWS!(Source, Yes.doCount)(s);
        result[key.data] = val.data;
        if (s.empty)
            throw convError!(Source, Target)(s);
        if (s.front != comma)
            break;
    }
    parseCheck!s(rbracket);
    static if (doCount)
    {
        return tuple!("data", "count")(result, ++count);
    }
    else
    {
        return result;
    }
}

///
@safe pure unittest
{
    import std.typecons : Flag, Yes, No, tuple;
    import std.range.primitives : save;
    import std.array : assocArray;
    auto s1 = "[1:10, 2:20, 3:30]";
    auto copyS1 = s1.save;
    auto aa1 = parse!(int[int])(s1);
    assert(aa1 == [1:10, 2:20, 3:30]);
    assert(tuple([1:10, 2:20, 3:30], copyS1.length) == parse!(int[int], string, Yes.doCount)(copyS1));

    auto s2 = `["aaa":10, "bbb":20, "ccc":30]`;
    auto copyS2 = s2.save;
    auto aa2 = parse!(int[string])(s2);
    assert(aa2 == ["aaa":10, "bbb":20, "ccc":30]);
    assert(tuple(["aaa":10, "bbb":20, "ccc":30], copyS2.length) ==
        parse!(int[string], string, Yes.doCount)(copyS2));

    auto s3 = `["aaa":[1], "bbb":[2,3], "ccc":[4,5,6]]`;
    auto copyS3 = s3.save;
    auto aa3 = parse!(int[][string])(s3);
    assert(aa3 == ["aaa":[1], "bbb":[2,3], "ccc":[4,5,6]]);
    assert(tuple(["aaa":[1], "bbb":[2,3], "ccc":[4,5,6]], copyS3.length) ==
        parse!(int[][string], string, Yes.doCount)(copyS3));

    auto s4 = `[]`;
    int[int] emptyAA;
    assert(tuple(emptyAA, s4.length) == parse!(int[int], string, Yes.doCount)(s4));
}

@safe pure unittest
{
    import std.exception;

    //Check proper failure
    auto s = "[1:10, 2:20, 3:30]";
    auto s2 = s.save;
    foreach (i ; 0 .. s.length-1)
    {
        auto ss = s[0 .. i];
        assertThrown!ConvException(parse!(int[int])(ss));
        assertThrown!ConvException(parse!(int[int], string, Yes.doCount)(ss));
    }
    int[int] aa = parse!(int[int])(s);
    auto aa2 = parse!(int[int], string, Yes.doCount)(s2);
    aa  = aa2[0];

}

private auto parseEscape(Source, Flag!"doCount" doCount = No.doCount)(ref Source s)
if (isInputRange!Source && isSomeChar!(ElementType!Source))
{
    parseCheck!s('\\');
    size_t count = 1;
    if (s.empty)
        throw parseError("Unterminated escape sequence");

    // consumes 1 element from Source
    dchar getHexDigit()(ref Source s_ = s)  // workaround
    {
        import std.ascii : isAlpha, isHexDigit;
        if (s_.empty)
            throw parseError("Unterminated escape sequence");
        s_.popFront();
        if (s_.empty)
            throw parseError("Unterminated escape sequence");
        dchar c = s_.front;
        if (!isHexDigit(c))
            throw parseError("Hex digit is missing");
        return isAlpha(c) ? ((c & ~0x20) - ('A' - 10)) : c - '0';
    }

    // We need to do octals separate, because they need a lookahead to find out,
    // where the escape sequence ends.
    auto first = s.front;
    if (first >= '0' && first <= '7')
    {
        dchar c1 = s.front;
        ++count;
        s.popFront();
        if (s.empty)
        {
            static if (doCount)
            {
                return tuple!("data", "count")(cast (dchar) (c1 - '0'), count);
            }
            else
            {
                return cast (dchar) (c1 - '0');
            }
        }
        dchar c2 = s.front;
        if (c2 < '0' || c2 > '7')
        {
            static if (doCount)
            {
                return tuple!("data", "count")(cast (dchar)(c1 - '0'), count);
            }
            else
            {
                return cast (dchar)(c1 - '0');
            }
        }
        ++count;
        s.popFront();
        dchar c3 = s.front;
        if (c3 < '0' || c3 > '7')
        {
            static if (doCount)
            {
                return tuple!("data", "count")(cast (dchar) (8 * (c1 - '0') + (c2 - '0')), count);
            }
            else
            {
                return cast (dchar) (8 * (c1 - '0') + (c2 - '0'));
            }
        }
        ++count;
        s.popFront();
        if (c1 > '3')
            throw parseError("Octal sequence is larger than \\377");
        static if (doCount)
        {
            return tuple!("data", "count")(cast (dchar) (64 * (c1 - '0') + 8 * (c2 - '0') + (c3 - '0')), count);
        }
        else
        {
            return cast (dchar) (64 * (c1 - '0') + 8 * (c2 - '0') + (c3 - '0'));
        }
    }

    dchar result;

    switch (first)
    {
        case '"':   result = '\"';  break;
        case '\'':  result = '\'';  break;
        case '?':   result = '\?';  break;
        case '\\':  result = '\\';  break;
        case 'a':   result = '\a';  break;
        case 'b':   result = '\b';  break;
        case 'f':   result = '\f';  break;
        case 'n':   result = '\n';  break;
        case 'r':   result = '\r';  break;
        case 't':   result = '\t';  break;
        case 'v':   result = '\v';  break;
        case 'x':
            result  = getHexDigit() << 4;
            result |= getHexDigit();
            count += 2;
            break;
        case 'u':
            result  = getHexDigit() << 12;
            result |= getHexDigit() << 8;
            result |= getHexDigit() << 4;
            result |= getHexDigit();
            count += 4;
            break;
        case 'U':
            result  = getHexDigit() << 28;
            result |= getHexDigit() << 24;
            result |= getHexDigit() << 20;
            result |= getHexDigit() << 16;
            result |= getHexDigit() << 12;
            result |= getHexDigit() << 8;
            result |= getHexDigit() << 4;
            result |= getHexDigit();
            count += 8;
            break;
        default:
            throw parseError("Unknown escape character " ~ to!string(s.front));
    }
    if (s.empty)
        throw parseError("Unterminated escape sequence");

    s.popFront();

    static if (doCount)
    {
        return tuple!("data", "count")(cast (dchar) result, ++count);
    }
    else
    {
        return cast (dchar) result;
    }
}

@safe pure unittest
{
    string[] s1 = [
        `\"`, `\'`, `\?`, `\\`, `\a`, `\b`, `\f`, `\n`, `\r`, `\t`, `\v`, //Normal escapes
        `\141`,
        `\x61`,
        `\u65E5`, `\U00012456`,
         // https://issues.dlang.org/show_bug.cgi?id=9621 (Named Character Entities)
        //`\&amp;`, `\&quot;`,
    ];
    string[] copyS1 = s1 ~ s1[0 .. 0];

    const(dchar)[] s2 = [
        '\"', '\'', '\?', '\\', '\a', '\b', '\f', '\n', '\r', '\t', '\v', //Normal escapes
        '\141',
        '\x61',
        '\u65E5', '\U00012456',
        // https://issues.dlang.org/show_bug.cgi?id=9621 (Named Character Entities)
        //'\&amp;', '\&quot;',
    ];

    foreach (i ; 0 .. s1.length)
    {
        assert(s2[i] == parseEscape(s1[i]));
        assert(s1[i].empty);

        assert(tuple(s2[i], copyS1[i].length) == parseEscape!(string, Yes.doCount)(copyS1[i]));
        assert(copyS1[i].empty);
    }
}

@safe pure unittest
{
    import std.exception;

    string[] ss = [
        `hello!`,  //Not an escape
        `\`,       //Premature termination
        `\/`,      //Not an escape
        `\gggg`,   //Not an escape
        `\xzz`,    //Not an hex
        `\x0`,     //Premature hex end
        `\XB9`,    //Not legal hex syntax
        `\u!!`,    //Not a unicode hex
        `\777`,    //Octal is larger than a byte
        `\80`,     //Wrong digit at beginning of octal
        `\u123`,   //Premature hex end
        `\U123123` //Premature hex end
    ];
    foreach (s ; ss)
    {
        assertThrown!ConvException(parseEscape(s));
        assertThrown!ConvException(parseEscape!(string, Yes.doCount)(s));
    }
}

// Undocumented
auto parseElement(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source s)
if (isInputRange!Source && isSomeChar!(ElementType!Source) && !is(Source == enum) &&
    isExactSomeString!Target)
{
    import std.array : appender;
    auto result = appender!Target();

    // parse array of chars
    if (s.empty)
        throw convError!(Source, Target)(s);
    if (s.front == '[')
    {
        return parse!(Target, Source, doCount)(s);
    }

    parseCheck!s('\"');
    size_t count = 1;
    if (s.empty)
        throw convError!(Source, Target)(s);
    if (s.front == '\"')
    {
        s.popFront();
        static if (doCount)
        {
            return tuple!("data", "count")(result.data, ++count);
        }
        else
        {
            return result.data;
        }

    }
    while (true)
    {
        if (s.empty)
            throw parseError("Unterminated quoted string");
        switch (s.front)
        {
            case '\"':
                s.popFront();
                static if (doCount)
                {
                    return tuple!("data", "count")(result.data, ++count);
                }
                else
                {
                    return result.data;
                }
            case '\\':
                auto r = parseEscape!(typeof(s), Yes.doCount)(s);
                result.put(r[0]);
                count += r[1];
                break;
            default:
                result.put(s.front);
                ++count;
                s.popFront();
                break;
        }
    }
    assert(false, "Unexpected fallthrough");
}

// ditto
auto parseElement(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source s)
if (isInputRange!Source && isSomeChar!(ElementType!Source) && !is(Source == enum) &&
    is(CharTypeOf!Target == dchar) && !is(Target == enum))
{
    Unqual!Target c;

    parseCheck!s('\'');
    size_t count = 1;
    if (s.empty)
        throw convError!(Source, Target)(s);
    ++count; // for the following if-else sequence
    if (s.front != '\\')
    {
        c = s.front;
        s.popFront();
    }
    else
        c = parseEscape(s);
    parseCheck!s('\'');
    static if (doCount)
    {
        return tuple!("data", "count")(c, ++count);
    }
    else
    {
        return c;
    }
}

// ditto
auto parseElement(Target, Source, Flag!"doCount" doCount = No.doCount)(ref Source s)
if (isInputRange!Source && isSomeChar!(ElementType!Source) &&
    !isSomeString!Target && !isSomeChar!Target)
{
    return parse!(Target, Source, doCount)(s);
}

// Use this when parsing a type that will ultimately be appended to a
// string.
package template WideElementType(T)
{
    alias E = ElementType!T;
    static if (isSomeChar!E)
        alias WideElementType = dchar;
    else
        alias WideElementType = E;
}


/***************************************************************
 * Convenience functions for converting one or more arguments
 * of any type into _text (the three character widths).
 */
string text(T...)(T args)
if (T.length > 0) { return textImpl!string(args); }

///ditto
wstring wtext(T...)(T args)
if (T.length > 0) { return textImpl!wstring(args); }

///ditto
dstring dtext(T...)(T args)
if (T.length > 0) { return textImpl!dstring(args); }

///
@safe unittest
{
    assert( text(42, ' ', 1.5, ": xyz") == "42 1.5: xyz"c);
    assert(wtext(42, ' ', 1.5, ": xyz") == "42 1.5: xyz"w);
    assert(dtext(42, ' ', 1.5, ": xyz") == "42 1.5: xyz"d);
}

@safe unittest
{
    char  c = 'h';
    wchar w = '你';
    dchar d = 'እ';

    assert( text(c, "ello", ' ', w, "好 ", d, "ው ሰላም ነው") == "hello 你好 እው ሰላም ነው"c);
    assert(wtext(c, "ello", ' ', w, "好 ", d, "ው ሰላም ነው") == "hello 你好 እው ሰላም ነው"w);
    assert(dtext(c, "ello", ' ', w, "好 ", d, "ው ሰላም ነው") == "hello 你好 እው ሰላም ነው"d);

    string  cs = "今日は";
    wstring ws = "여보세요";
    dstring ds = "Здравствуйте";

    assert( text(cs, ' ', ws, " ", ds) == "今日は 여보세요 Здравствуйте"c);
    assert(wtext(cs, ' ', ws, " ", ds) == "今日は 여보세요 Здравствуйте"w);
    assert(dtext(cs, ' ', ws, " ", ds) == "今日は 여보세요 Здравствуйте"d);
}

private S textImpl(S, U...)(U args)
{
    static if (U.length == 0)
    {
        return null;
    }
    else static if (U.length == 1)
    {
        return to!S(args[0]);
    }
    else
    {
        import std.array : appender;
        import std.traits : isSomeChar, isSomeString;

        auto app = appender!S();

        // assume that on average, parameters will have less
        // than 20 elements
        app.reserve(U.length * 20);
        // Must be static foreach because of https://issues.dlang.org/show_bug.cgi?id=21209
        static foreach (arg; args)
        {
            static if (
                isSomeChar!(typeof(arg))
                || isSomeString!(typeof(arg))
                || ( isInputRange!(typeof(arg)) && isSomeChar!(ElementType!(typeof(arg))) )
            )
                app.put(arg);
            else static if (

                is(immutable typeof(arg) == immutable uint) || is(immutable typeof(arg) == immutable ulong) ||
                is(immutable typeof(arg) == immutable int) || is(immutable typeof(arg) == immutable long)
            )
                // https://issues.dlang.org/show_bug.cgi?id=17712#c15
                app.put(textImpl!(S)(arg));
            else
                app.put(to!S(arg));
        }

        return app.data;
    }
}


/***************************************************************
The `octal` facility provides a means to declare a number in base 8.
Using `octal!177` or `octal!"177"` for 127 represented in octal
(same as 0177 in C).

The rules for strings are the usual for literals: If it can fit in an
`int`, it is an `int`. Otherwise, it is a `long`. But, if the
user specifically asks for a `long` with the `L` suffix, always
give the `long`. Give an unsigned iff it is asked for with the $(D
U) or `u` suffix. _Octals created from integers preserve the type
of the passed-in integral.

See_Also:
    $(LREF parse) for parsing octal strings at runtime.
 */
template octal(string num)
if (isOctalLiteral(num))
{
    static if ((octalFitsInInt!num && !literalIsLong!num) && !literalIsUnsigned!num)
        enum octal = octal!int(num);
    else static if ((!octalFitsInInt!num || literalIsLong!num) && !literalIsUnsigned!num)
        enum octal = octal!long(num);
    else static if ((octalFitsInInt!num && !literalIsLong!num) && literalIsUnsigned!num)
        enum octal = octal!uint(num);
    else static if ((!octalFitsInInt!(num) || literalIsLong!(num)) && literalIsUnsigned!(num))
        enum octal = octal!ulong(num);
    else
        static assert(false, "Unusable input " ~ num);
}

/// Ditto
template octal(alias decimalInteger)
if (is(typeof(decimalInteger)) && isIntegral!(typeof(decimalInteger)))
{
    enum octal = convertToOctal(decimalInteger);
}

///
@safe unittest
{
    // Same as 0177
    auto a = octal!177;
    // octal is a compile-time device
    enum b = octal!160;
    // Create an unsigned octal
    auto c = octal!"1_000_000u";
    // Leading zeros are allowed when converting from a string
    auto d = octal!"0001_200_000";
}

/*************************************
 * Convert a decimal integer to an octal integer with the same digits.
 * Params:
 *    i = integer to convert
 * Returns:
 *    octal integer with the same type and same digits
 */
private T convertToOctal(T)(T i)
{
    assert((i % 10) < 8);
    return i ? convertToOctal(i / 10) * 8 + i % 10 : 0;
}

/*
    Takes a string, num, which is an octal literal, and returns its
    value, in the type T specified.
*/
private T octal(T)(const string num)
{
    assert(isOctalLiteral(num), num ~ " is not an octal literal");

    T value = 0;

    foreach (const char s; num)
    {
        if (s < '0' || s > '7') // we only care about digits; skip the rest
        // safe to skip - this is checked out in the assert so these
        // are just suffixes
            continue;

        value *= 8;
        value += s - '0';
    }

    return value;
}

@safe unittest
{
    int a = octal!int("10");
    assert(a == 8);

    int b = octal!int("000137");
    assert(b == 95);
}

/*
Take a look at int.max and int.max+1 in octal and the logic for this
function follows directly.
 */
private template octalFitsInInt(string octalNum)
{
    // note it is important to strip the literal of all
    // non-numbers. kill the suffix and underscores lest they mess up
    // the number of digits here that we depend on.
    enum bool octalFitsInInt = strippedOctalLiteral(octalNum).length < 11 ||
        strippedOctalLiteral(octalNum).length == 11 &&
        strippedOctalLiteral(octalNum)[0] == '1';
}

private string strippedOctalLiteral(string original)
{
    string stripped = "";
    bool leading_zeros = true;
    foreach (c; original)
    {
        if (!('0' <= c && c <= '7'))
            continue;
        if (c == '0')
        {
            if (leading_zeros)
                continue;
        }
        else
        {
            leading_zeros = false;
        }
        stripped ~= c;
    }
    if (stripped.length == 0)
    {
        assert(leading_zeros);
        return "0";
    }
    return stripped;
}

@safe unittest
{
    static assert(strippedOctalLiteral("7") == "7");
    static assert(strippedOctalLiteral("123") == "123");
    static assert(strippedOctalLiteral("00123") == "123");
    static assert(strippedOctalLiteral("01230") == "1230");
    static assert(strippedOctalLiteral("0") == "0");
    static assert(strippedOctalLiteral("00_000") == "0");
    static assert(strippedOctalLiteral("000_000_12_300") == "12300");
}

private template literalIsLong(string num)
{
    static if (num.length > 1)
    // can be xxL or xxLu according to spec
        enum literalIsLong = (num[$-1] == 'L' || num[$-2] == 'L');
    else
        enum literalIsLong = false;
}

private template literalIsUnsigned(string num)
{
    static if (num.length > 1)
    // can be xxU or xxUL according to spec
        enum literalIsUnsigned = (num[$-1] == 'u' || num[$-2] == 'u')
            // both cases are allowed too
            || (num[$-1] == 'U' || num[$-2] == 'U');
    else
        enum literalIsUnsigned = false;
}

/*
Returns if the given string is a correctly formatted octal literal.

The format is specified in spec/lex.html. The leading zeros are allowed,
but not required.
 */
@safe pure nothrow @nogc
private bool isOctalLiteral(const string num)
{
    if (num.length == 0)
        return false;

    // Must start with a digit.
    if (num[0] < '0' || num[0] > '7')
        return false;

    foreach (i, c; num)
    {
        if (('0' <= c && c <= '7') || c == '_')  // a legal character
            continue;

        if (i < num.length - 2)
            return false;

        // gotta check for those suffixes
        if (c != 'U' && c != 'u' && c != 'L')
            return false;
        if (i != num.length - 1)
        {
            // if we're not the last one, the next one must
            // also be a suffix to be valid
            char c2 = num[$-1];
            if (c2 != 'U' && c2 != 'u' && c2 != 'L')
                return false; // spam at the end of the string
            if (c2 == c)
                return false; // repeats are disallowed
        }
    }

    return true;
}

@safe unittest
{
    // ensure that you get the right types, even with embedded underscores
    auto w = octal!"100_000_000_000";
    static assert(!is(typeof(w) == int));
    auto w2 = octal!"1_000_000_000";
    static assert(is(typeof(w2) == int));

    static assert(octal!"45" == 37);
    static assert(octal!"0" == 0);
    static assert(octal!"7" == 7);
    static assert(octal!"10" == 8);
    static assert(octal!"666" == 438);
    static assert(octal!"0004001" == 2049);
    static assert(octal!"00" == 0);
    static assert(octal!"0_0" == 0);

    static assert(octal!45 == 37);
    static assert(octal!0 == 0);
    static assert(octal!7 == 7);
    static assert(octal!10 == 8);
    static assert(octal!666 == 438);

    static assert(octal!"66_6" == 438);
    static assert(octal!"0_0_66_6" == 438);

    static assert(octal!2520046213 == 356535435);
    static assert(octal!"2520046213" == 356535435);

    static assert(octal!17777777777 == int.max);

    static assert(!__traits(compiles, octal!823));

    static assert(!__traits(compiles, octal!"823"));

    static assert(!__traits(compiles, octal!"_823"));
    static assert(!__traits(compiles, octal!"spam"));
    static assert(!__traits(compiles, octal!"77%"));

    static assert(is(typeof(octal!"17777777777") == int));
    static assert(octal!"17777777777" == int.max);

    static assert(is(typeof(octal!"20000000000U") == ulong)); // Shouldn't this be uint?
    static assert(octal!"20000000000" == uint(int.max) + 1);

    static assert(is(typeof(octal!"777777777777777777777") == long));
    static assert(octal!"777777777777777777777" == long.max);

    static assert(is(typeof(octal!"1000000000000000000000U") == ulong));
    static assert(octal!"1000000000000000000000" == ulong(long.max) + 1);

    int a;
    long b;

    // biggest value that should fit in an it
    a = octal!"17777777777";
    assert(a == int.max);
    // should not fit in the int
    static assert(!__traits(compiles, a = octal!"20000000000"));
    // ... but should fit in a long
    b = octal!"20000000000";
    assert(b == 1L + int.max);

    b = octal!"1L";
    assert(b == 1);
    b = octal!1L;
    assert(b == 1);
}

// emplace() used to be here but was moved to druntime
public import core.lifetime : emplace;

// https://issues.dlang.org/show_bug.cgi?id=9559
@safe unittest
{
    import std.algorithm.iteration : map;
    import std.array : array;
    import std.typecons : Nullable;
    alias I = Nullable!int;
    auto ints = [0, 1, 2].map!(i => i & 1 ? I.init : I(i))();
    auto asArray = array(ints);
}

@system unittest //http://forum.dlang.org/post/nxbdgtdlmwscocbiypjs@forum.dlang.org
{
    import std.array : array;
    import std.datetime : SysTime, UTC;
    import std.math.traits : isNaN;

    static struct A
    {
        double i;
    }

    static struct B
    {
        invariant()
        {
            if (j == 0)
                assert(a.i.isNaN(), "why is 'j' zero?? and i is not NaN?");
            else
                assert(!a.i.isNaN());
        }
        SysTime when; // comment this line avoid the breakage
        int j;
        A a;
    }

    B b1 = B.init;
    assert(&b1); // verify that default eyes invariants are ok;

    auto b2 = B(SysTime(0, UTC()), 1, A(1));
    assert(&b2);
    auto b3 = B(SysTime(0, UTC()), 1, A(1));
    assert(&b3);

    auto arr = [b2, b3];

    assert(arr[0].j == 1);
    assert(arr[1].j == 1);
    auto a2 = arr.array(); // << bang, invariant is raised, also if b2 and b3 are good
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    // Check fix for https://issues.dlang.org/show_bug.cgi?id=2971
    assert(equal(map!(to!int)(["42", "34", "345"]), [42, 34, 345]));
}

// Undocumented for the time being
void toTextRange(T, W)(T value, W writer)
if (isIntegral!T && isOutputRange!(W, char))
{
    import core.internal.string : SignedStringBuf, signedToTempString,
                                  UnsignedStringBuf, unsignedToTempString;

    if (value < 0)
    {
        SignedStringBuf buf = void;
        put(writer, signedToTempString(value, buf));
    }
    else
    {
        UnsignedStringBuf buf = void;
        put(writer, unsignedToTempString(value, buf));
    }
}

@safe unittest
{
    import std.array : appender;
    auto result = appender!(char[])();
    toTextRange(-1, result);
    assert(result.data == "-1");
}


/**
    Returns the corresponding _unsigned value for `x` (e.g. if `x` has type
    `int`, it returns $(D cast(uint) x)). The advantage compared to the cast
    is that you do not need to rewrite the cast if `x` later changes type
    (e.g from `int` to `long`).

    Note that the result is always mutable even if the original type was const
    or immutable. In order to retain the constness, use $(REF Unsigned, std,traits).
 */
auto unsigned(T)(T x)
if (isIntegral!T)
{
    return cast(Unqual!(Unsigned!T))x;
}

///
@safe unittest
{
    import std.traits : Unsigned;
    immutable int s = 42;
    auto u1 = unsigned(s); //not qualified
    static assert(is(typeof(u1) == uint));
    Unsigned!(typeof(s)) u2 = unsigned(s); //same qualification
    static assert(is(typeof(u2) == immutable uint));
    immutable u3 = unsigned(s); //explicitly qualified
}

/// Ditto
auto unsigned(T)(T x)
if (isSomeChar!T)
{
    // All characters are unsigned
    static assert(T.min == 0, T.stringof ~ ".min must be zero");
    return cast(Unqual!T) x;
}

@safe unittest
{
    static foreach (T; AliasSeq!(byte, ubyte))
    {
        static assert(is(typeof(unsigned(cast(T) 1)) == ubyte));
        static assert(is(typeof(unsigned(cast(const T) 1)) == ubyte));
        static assert(is(typeof(unsigned(cast(immutable T) 1)) == ubyte));
    }

    static foreach (T; AliasSeq!(short, ushort))
    {
        static assert(is(typeof(unsigned(cast(T) 1)) == ushort));
        static assert(is(typeof(unsigned(cast(const T) 1)) == ushort));
        static assert(is(typeof(unsigned(cast(immutable T) 1)) == ushort));
    }

    static foreach (T; AliasSeq!(int, uint))
    {
        static assert(is(typeof(unsigned(cast(T) 1)) == uint));
        static assert(is(typeof(unsigned(cast(const T) 1)) == uint));
        static assert(is(typeof(unsigned(cast(immutable T) 1)) == uint));
    }

    static foreach (T; AliasSeq!(long, ulong))
    {
        static assert(is(typeof(unsigned(cast(T) 1)) == ulong));
        static assert(is(typeof(unsigned(cast(const T) 1)) == ulong));
        static assert(is(typeof(unsigned(cast(immutable T) 1)) == ulong));
    }
}

@safe unittest
{
    static foreach (T; AliasSeq!(char, wchar, dchar))
    {
        static assert(is(typeof(unsigned(cast(T)'A')) == T));
        static assert(is(typeof(unsigned(cast(const T)'A')) == T));
        static assert(is(typeof(unsigned(cast(immutable T)'A')) == T));
    }
}


/**
    Returns the corresponding _signed value for `x` (e.g. if `x` has type
    `uint`, it returns $(D cast(int) x)). The advantage compared to the cast
    is that you do not need to rewrite the cast if `x` later changes type
    (e.g from `uint` to `ulong`).

    Note that the result is always mutable even if the original type was const
    or immutable. In order to retain the constness, use $(REF Signed, std,traits).
 */
auto signed(T)(T x)
if (isIntegral!T)
{
    return cast(Unqual!(Signed!T))x;
}

///
@safe unittest
{
    import std.traits : Signed;

    immutable uint u = 42;
    auto s1 = signed(u); //not qualified
    static assert(is(typeof(s1) == int));
    Signed!(typeof(u)) s2 = signed(u); //same qualification
    static assert(is(typeof(s2) == immutable int));
    immutable s3 = signed(u); //explicitly qualified
}

@system unittest
{
    static foreach (T; AliasSeq!(byte, ubyte))
    {
        static assert(is(typeof(signed(cast(T) 1)) == byte));
        static assert(is(typeof(signed(cast(const T) 1)) == byte));
        static assert(is(typeof(signed(cast(immutable T) 1)) == byte));
    }

    static foreach (T; AliasSeq!(short, ushort))
    {
        static assert(is(typeof(signed(cast(T) 1)) == short));
        static assert(is(typeof(signed(cast(const T) 1)) == short));
        static assert(is(typeof(signed(cast(immutable T) 1)) == short));
    }

    static foreach (T; AliasSeq!(int, uint))
    {
        static assert(is(typeof(signed(cast(T) 1)) == int));
        static assert(is(typeof(signed(cast(const T) 1)) == int));
        static assert(is(typeof(signed(cast(immutable T) 1)) == int));
    }

    static foreach (T; AliasSeq!(long, ulong))
    {
        static assert(is(typeof(signed(cast(T) 1)) == long));
        static assert(is(typeof(signed(cast(const T) 1)) == long));
        static assert(is(typeof(signed(cast(immutable T) 1)) == long));
    }
}

// https://issues.dlang.org/show_bug.cgi?id=10874
@safe unittest
{
    enum Test { a = 0 }
    ulong l = 0;
    auto t = l.to!Test;
}

// asOriginalType
/**
Returns the representation of an enumerated value, i.e. the value converted to
the base type of the enumeration.
*/
OriginalType!E asOriginalType(E)(E value)
if (is(E == enum))
{
    return value;
}

///
@safe unittest
{
    enum A { a = 42 }
    static assert(is(typeof(A.a.asOriginalType) == int));
    assert(A.a.asOriginalType == 42);
    enum B : double { a = 43 }
    static assert(is(typeof(B.a.asOriginalType) == double));
    assert(B.a.asOriginalType == 43);
}

/**
    A wrapper on top of the built-in cast operator that allows one to restrict
    casting of the original type of the value.

    A common issue with using a raw cast is that it may silently continue to
    compile even if the value's type has changed during refactoring,
    which breaks the initial assumption about the cast.

    Params:
        From  = The type to cast from. The programmer must ensure it is legal
                to make this cast.
 */
template castFrom(From)
{
    /**
        Params:
            To    = The type _to cast _to.
            value = The value _to cast. It must be of type `From`,
                    otherwise a compile-time error is emitted.

        Returns:
            the value after the cast, returned by reference if possible.
     */
    auto ref to(To, T)(auto ref T value) @system
    {
        static assert(
            is(From == T),
            "the value to cast is not of specified type '" ~ From.stringof ~
                 "', it is of type '" ~ T.stringof ~ "'"
        );

        static assert(
            is(typeof(cast(To) value)),
            "can't cast from '" ~ From.stringof ~ "' to '" ~ To.stringof ~ "'"
        );

        return cast(To) value;
    }
}

///
@system unittest
{
    // Regular cast, which has been verified to be legal by the programmer:
    {
        long x;
        auto y = cast(int) x;
    }

    // However this will still compile if 'x' is changed to be a pointer:
    {
        long* x;
        auto y = cast(int) x;
    }

    // castFrom provides a more reliable alternative to casting:
    {
        long x;
        auto y = castFrom!long.to!int(x);
    }

    // Changing the type of 'x' will now issue a compiler error,
    // allowing bad casts to be caught before it's too late:
    {
        long* x;
        static assert(
            !__traits(compiles, castFrom!long.to!int(x))
        );

        // if cast is still needed, must be changed to:
        auto y = castFrom!(long*).to!int(x);
    }
}

// https://issues.dlang.org/show_bug.cgi?id=16667
@system unittest
{
    ubyte[] a = ['a', 'b', 'c'];
    assert(castFrom!(ubyte[]).to!(string)(a) == "abc");
}

/**
Check the correctness of a string for `hexString`.
The result is true if and only if the input string is composed of whitespace
characters (\f\n\r\t\v lineSep paraSep nelSep) and
an even number of hexadecimal digits (regardless of the case).
*/
@safe pure @nogc
private bool isHexLiteral(String)(scope const String hexData)
{
    import std.ascii : isHexDigit;
    import std.uni : lineSep, paraSep, nelSep;
    size_t i;
    foreach (const dchar c; hexData)
    {
        switch (c)
        {
            case ' ':
            case '\t':
            case '\v':
            case '\f':
            case '\r':
            case '\n':
            case lineSep:
            case paraSep:
            case nelSep:
                continue;

            default:
                break;
        }
        if (c.isHexDigit)
            ++i;
        else
            return false;
    }
    return !(i & 1);
}

@safe unittest
{
    // test all the hex digits
    static assert( ("0123456789abcdefABCDEF").isHexLiteral);
    // empty or white strings are not valid
    static assert( "\r\n\t".isHexLiteral);
    // but are accepted if the count of hex digits is even
    static assert( "A\r\n\tB".isHexLiteral);
}

@safe unittest
{
    import std.ascii;
    // empty/whites
    static assert( "".isHexLiteral);
    static assert( " \r".isHexLiteral);
    static assert( whitespace.isHexLiteral);
    static assert( ""w.isHexLiteral);
    static assert( " \r"w.isHexLiteral);
    static assert( ""d.isHexLiteral);
    static assert( " \r"d.isHexLiteral);
    static assert( "\u2028\u2029\u0085"d.isHexLiteral);
    // odd x strings
    static assert( !("5" ~ whitespace).isHexLiteral);
    static assert( !"123".isHexLiteral);
    static assert( !"1A3".isHexLiteral);
    static assert( !"1 23".isHexLiteral);
    static assert( !"\r\n\tC".isHexLiteral);
    static assert( !"123"w.isHexLiteral);
    static assert( !"1A3"w.isHexLiteral);
    static assert( !"1 23"w.isHexLiteral);
    static assert( !"\r\n\tC"w.isHexLiteral);
    static assert( !"123"d.isHexLiteral);
    static assert( !"1A3"d.isHexLiteral);
    static assert( !"1 23"d.isHexLiteral);
    static assert( !"\r\n\tC"d.isHexLiteral);
    // even x strings with invalid charset
    static assert( !"12gG".isHexLiteral);
    static assert( !"2A  3q".isHexLiteral);
    static assert( !"12gG"w.isHexLiteral);
    static assert( !"2A  3q"w.isHexLiteral);
    static assert( !"12gG"d.isHexLiteral);
    static assert( !"2A  3q"d.isHexLiteral);
    // valid x strings
    static assert( ("5A" ~ whitespace).isHexLiteral);
    static assert( ("5A 01A C FF de 1b").isHexLiteral);
    static assert( ("0123456789abcdefABCDEF").isHexLiteral);
    static assert( (" 012 34 5 6789 abcd ef\rAB\nCDEF").isHexLiteral);
    static assert( ("5A 01A C FF de 1b"w).isHexLiteral);
    static assert( ("0123456789abcdefABCDEF"w).isHexLiteral);
    static assert( (" 012 34 5 6789 abcd ef\rAB\nCDEF"w).isHexLiteral);
    static assert( ("5A 01A C FF de 1b"d).isHexLiteral);
    static assert( ("0123456789abcdefABCDEF"d).isHexLiteral);
    static assert( (" 012 34 5 6789 abcd ef\rAB\nCDEF"d).isHexLiteral);
    // library version allows what's pointed by https://issues.dlang.org/show_bug.cgi?id=10454
    static assert( ("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF").isHexLiteral);
}

/**
Converts a hex literal to a string at compile time.

Takes a string made of hexadecimal digits and returns
the matching string by converting each pair of digits to a character.
The input string can also include white characters, which can be used
to keep the literal string readable in the source code.

The function is intended to replace the hexadecimal literal strings
starting with `'x'`, which could be removed to simplify the core language.

Params:
    hexData = string to be converted.

Returns:
    a `string`, a `wstring` or a `dstring`, according to the type of hexData.
 */
template hexString(string hexData)
if (hexData.isHexLiteral)
{
    enum hexString = mixin(hexToString(hexData));
}

/// ditto
template hexString(wstring hexData)
if (hexData.isHexLiteral)
{
    enum wstring hexString = mixin(hexToString(hexData));
}

/// ditto
template hexString(dstring hexData)
if (hexData.isHexLiteral)
{
    enum dstring hexString = mixin(hexToString(hexData));
}

///
@safe unittest
{
    // conversion at compile time
    auto string1 = hexString!"304A314B";
    assert(string1 == "0J1K");
    auto string2 = hexString!"304A314B"w;
    assert(string2 == "0J1K"w);
    auto string3 = hexString!"304A314B"d;
    assert(string3 == "0J1K"d);
}

@safe nothrow pure private
{
    /* These are meant to be used with CTFE.
     * They cause the instantiations of hexStrLiteral()
     * to be in Phobos, not user code.
     */
    string hexToString(string s)
    {
        return hexStrLiteral(s);
    }

    wstring hexToString(wstring s)
    {
        return hexStrLiteral(s);
    }

    dstring hexToString(dstring s)
    {
        return hexStrLiteral(s);
    }
}

/*
    Turn a hexadecimal string into a regular string literal.
    I.e. "dead beef" is transformed into "\xde\xad\xbe\xef"
    suitable for use in a mixin.
    Params:
        hexData is string, wstring, or dstring and validated by isHexLiteral()
 */
@trusted nothrow pure
private auto hexStrLiteral(String)(scope String hexData)
{
    import std.ascii : isHexDigit;
    alias C = Unqual!(ElementEncodingType!String);    // char, wchar or dchar
    C[] result;
    result.length = 1 + hexData.length * 2 + 1;       // don't forget the " "
    /* Use a pointer because we know it won't overrun,
     * and this will reduce the size of the function substantially
     * by not doing the array bounds checks.
     * This is why this function is @trusted.
     */
    auto r = result.ptr;
    r[0] = '"';
    size_t cnt = 0;
    foreach (c; hexData)
    {
        if (c.isHexDigit)
        {
            if ((cnt & 1) == 0)
            {
                r[1 + cnt]     = '\\';
                r[1 + cnt + 1] = 'x';
                cnt += 2;
            }
            r[1 + cnt] = c;
            ++cnt;
        }
    }
    r[1 + cnt] = '"';
    result.length = 1 + cnt + 1;        // trim off any excess length
    return result;
}


@safe unittest
{
    // compile time
    assert(hexString!"46 47 48 49 4A 4B" == "FGHIJK");
    assert(hexString!"30\r\n\t\f\v31 32 33 32 31 30" == "0123210");
    assert(hexString!"ab cd" == hexString!"ABCD");
}


/**
 * Convert integer to a range of characters.
 * Intended to be lightweight and fast.
 *
 * Params:
 *      radix = 2, 8, 10, 16
 *      Char = character type for output
 *      letterCase = lower for deadbeef, upper for DEADBEEF
 *      value = integer to convert. Can be ubyte, ushort, uint or ulong. If radix
 *              is 10, can also be byte, short, int or long.
 * Returns:
 *      Random access range with slicing and everything
 */

auto toChars(ubyte radix = 10, Char = char, LetterCase letterCase = LetterCase.lower, T)(T value)
    pure nothrow @nogc @safe
if ((radix == 2 || radix == 8 || radix == 10 || radix == 16) &&
        isIntegral!T && (radix == 10 || isUnsigned!T))
{
    alias UT = Unqual!T;

    static if (radix == 10)
    {
        /* uint.max  is 42_9496_7295
         *  int.max  is 21_4748_3647
         * ulong.max is 1844_6744_0737_0955_1615
         *  long.max is  922_3372_0368_5477_5807
         */
        static struct Result
        {
            void initialize(UT value)
            {
                import core.internal.string : signedToTempString, unsignedToTempString;

                char[] t = value < 0
                    ?   signedToTempString!(10, false, char)(value, buf)
                    : unsignedToTempString!(10, false, char)(value, buf);

                lwr = cast(uint) (buf.length - t.length);
                upr = cast(uint) buf.length;
            }

            @property size_t length() { return upr - lwr; }

            alias opDollar = length;

            @property bool empty() { return upr == lwr; }

            @property Char front() { return buf[lwr]; }

            void popFront() { ++lwr; }

            @property Char back() { return buf[upr - 1]; }

            void popBack() { --upr; }

            @property Result save() { return this; }

            Char opIndex(size_t i) { return buf[lwr + i]; }

            Result opSlice(size_t lwr, size_t upr)
            {
                Result result = void;
                result.buf = buf;
                result.lwr = cast(uint)(this.lwr + lwr);
                result.upr = cast(uint)(this.lwr + upr);
                return result;
            }

          private:
            uint lwr = void, upr = void;
            char[(UT.sizeof == 4) ? 10 + isSigned!T : 20] buf = void;
        }

        Result result;
        result.initialize(value);
        return result;
    }
    else
    {
        static if (radix == 2)
            enum SHIFT = 1;
        else static if (radix == 8)
            enum SHIFT = 3;
        else static if (radix == 16)
            enum SHIFT = 4;
        else
            static assert(false, "radix must be 2, 8, 10, or 16");
        static struct Result
        {
            this(UT value)
            {
                this.value = value;

                ubyte len = 1;
                while (value >>>= SHIFT)
                   ++len;
                this.len = len;
            }

            @property size_t length() { return len; }

            @property bool empty() { return len == 0; }

            @property Char front() { return opIndex(0); }

            void popFront() { --len; }

            @property Char back() { return opIndex(len - 1); }

            void popBack()
            {
                value >>>= SHIFT;
                --len;
            }

            @property Result save() { return this; }

            Char opIndex(size_t i)
            {
                Char c = (value >>> ((len - i - 1) * SHIFT)) & ((1 << SHIFT) - 1);
                return cast(Char)((radix < 10 || c < 10) ? c + '0'
                                                         : (letterCase == LetterCase.upper ? c + 'A' - 10
                                                                                           : c + 'a' - 10));
            }

            Result opSlice(size_t lwr, size_t upr)
            {
                Result result = void;
                result.value = value >>> ((len - upr) * SHIFT);
                result.len = cast(ubyte)(upr - lwr);
                return result;
            }

          private:
            UT value;
            ubyte len;
        }

        return Result(value);
    }
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;

    assert(toChars(1).equal("1"));
    assert(toChars(1_000_000).equal("1000000"));

    assert(toChars!(2)(2U).equal("10"));
    assert(toChars!(16)(255U).equal("ff"));
    assert(toChars!(16, char, LetterCase.upper)(255U).equal("FF"));
}


@safe unittest
{
    import std.array;
    import std.range;

    assert(toChars(123) == toChars(123));

    {
        assert(toChars!2(ubyte(0)).array == "0");
        assert(toChars!2(ushort(0)).array == "0");
        assert(toChars!2(0u).array == "0");
        assert(toChars!2(0Lu).array == "0");
        assert(toChars!2(ubyte(1)).array == "1");
        assert(toChars!2(ushort(1)).array == "1");
        assert(toChars!2(1u).array == "1");
        assert(toChars!2(1Lu).array == "1");

        auto r = toChars!2(2u);
        assert(r.length == 2);
        assert(r[0] == '1');
        assert(r[1 .. 2].array == "0");
        auto s = r.save;
        assert(r.array == "10");
        assert(s.retro.array == "01");
    }
    {
        assert(toChars!8(ubyte(0)).array == "0");
        assert(toChars!8(ushort(0)).array == "0");
        assert(toChars!8(0u).array == "0");
        assert(toChars!8(0Lu).array == "0");
        assert(toChars!8(1u).array == "1");
        assert(toChars!8(1234567Lu).array == "4553207");
        assert(toChars!8(ubyte.max).array == "377");
        assert(toChars!8(ushort.max).array == "177777");

        auto r = toChars!8(8u);
        assert(r.length == 2);
        assert(r[0] == '1');
        assert(r[1 .. 2].array == "0");
        auto s = r.save;
        assert(r.array == "10");
        assert(s.retro.array == "01");
    }
    {
        assert(toChars!10(ubyte(0)).array == "0");
        assert(toChars!10(ushort(0)).array == "0");
        assert(toChars!10(0u).array == "0");
        assert(toChars!10(0Lu).array == "0");
        assert(toChars!10(1u).array == "1");
        assert(toChars!10(1234567Lu).array == "1234567");
        assert(toChars!10(ubyte.max).array == "255");
        assert(toChars!10(ushort.max).array == "65535");
        assert(toChars!10(uint.max).array == "4294967295");
        assert(toChars!10(ulong.max).array == "18446744073709551615");

        auto r = toChars(10u);
        assert(r.length == 2);
        assert(r[0] == '1');
        assert(r[1 .. 2].array == "0");
        auto s = r.save;
        assert(r.array == "10");
        assert(s.retro.array == "01");
    }
    {
        assert(toChars!10(0).array == "0");
        assert(toChars!10(0L).array == "0");
        assert(toChars!10(1).array == "1");
        assert(toChars!10(1234567L).array == "1234567");
        assert(toChars!10(byte.max).array == "127");
        assert(toChars!10(short.max).array == "32767");
        assert(toChars!10(int.max).array == "2147483647");
        assert(toChars!10(long.max).array == "9223372036854775807");
        assert(toChars!10(-byte.max).array == "-127");
        assert(toChars!10(-short.max).array == "-32767");
        assert(toChars!10(-int.max).array == "-2147483647");
        assert(toChars!10(-long.max).array == "-9223372036854775807");
        assert(toChars!10(byte.min).array == "-128");
        assert(toChars!10(short.min).array == "-32768");
        assert(toChars!10(int.min).array == "-2147483648");
        assert(toChars!10(long.min).array == "-9223372036854775808");

        auto r = toChars!10(10);
        assert(r.length == 2);
        assert(r[0] == '1');
        assert(r[1 .. 2].array == "0");
        auto s = r.save;
        assert(r.array == "10");
        assert(s.retro.array == "01");
    }
    {
        assert(toChars!(16)(0u).array == "0");
        assert(toChars!(16)(0Lu).array == "0");
        assert(toChars!(16)(10u).array == "a");
        assert(toChars!(16, char, LetterCase.upper)(0x12AF34567Lu).array == "12AF34567");
        assert(toChars!(16)(ubyte(0)).array == "0");
        assert(toChars!(16)(ushort(0)).array == "0");
        assert(toChars!(16)(ubyte.max).array == "ff");
        assert(toChars!(16)(ushort.max).array == "ffff");

        auto r = toChars!(16)(16u);
        assert(r.length == 2);
        assert(r[0] == '1');
        assert(r[1 .. 2].array == "0");
        auto s = r.save;
        assert(r.array == "10");
        assert(s.retro.array == "01");
    }
}

@safe unittest // opSlice (https://issues.dlang.org/show_bug.cgi?id=16192)
{
    import std.meta : AliasSeq;

    static struct Test { ubyte radix; uint number; }

    alias tests = AliasSeq!(
        Test(2, 0b1_0110_0111u),
        Test(2, 0b10_1100_1110u),
        Test(8, octal!123456701u),
        Test(8, octal!1234567012u),
        Test(10, 123456789u),
        Test(10, 1234567890u),
        Test(16, 0x789ABCDu),
        Test(16, 0x789ABCDEu),
    );

    foreach (test; tests)
    {
        enum ubyte radix = test.radix;
        auto original = toChars!radix(test.number);

        // opSlice vs popFront
        auto r = original.save;
        size_t i = 0;
        for (; !r.empty; r.popFront(), ++i)
        {
            assert(original[i .. original.length].tupleof == r.tupleof);
                // tupleof is used to work around https://issues.dlang.org/show_bug.cgi?id=16216.
        }

        // opSlice vs popBack
        r = original.save;
        i = 0;
        for (; !r.empty; r.popBack(), ++i)
        {
            assert(original[0 .. original.length - i].tupleof == r.tupleof);
        }

        // opSlice vs both popFront and popBack
        r = original.save;
        i = 0;
        for (; r.length >= 2; r.popFront(), r.popBack(), ++i)
        {
            assert(original[i .. original.length - i].tupleof == r.tupleof);
        }
    }
}

// Converts an unsigned integer to a compile-time string constant.
package enum toCtString(ulong n) = n.stringof[0 .. $ - "LU".length];

// Check that .stringof does what we expect, since it's not guaranteed by the
// language spec.
@safe /*@betterC*/ unittest
{
    assert(toCtString!0 == "0");
    assert(toCtString!123456 == "123456");
}
