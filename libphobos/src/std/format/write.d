// Written in the D programming language.

/**
This is a submodule of $(MREF std, format).

It provides two functions for writing formatted output: $(LREF
formatValue) and $(LREF formattedWrite). The former writes a single
value. The latter writes several values at once, interspersed with
unformatted text.

The following combinations of format characters and types are
available:

$(BOOKTABLE ,
$(TR $(TH) $(TH s) $(TH c) $(TH d, u, b, o) $(TH x, X) $(TH e, E, f, F, g, G, a, A) $(TH r) $(TH compound))
$(TR $(TD `bool`) $(TD yes) $(TD $(MDASH)) $(TD yes) $(TD yes) $(TD $(MDASH)) $(TD yes) $(TD $(MDASH)))
$(TR $(TD `null`) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)))
$(TR $(TD $(I integer)) $(TD yes) $(TD $(MDASH)) $(TD yes) $(TD yes) $(TD yes) $(TD yes) $(TD $(MDASH)))
$(TR $(TD $(I floating point)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD yes) $(TD yes) $(TD $(MDASH)))
$(TR $(TD $(I character)) $(TD yes) $(TD yes) $(TD yes) $(TD yes) $(TD $(MDASH)) $(TD yes) $(TD $(MDASH)))
$(TR $(TD $(I string)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD yes) $(TD yes))
$(TR $(TD $(I array)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD yes) $(TD yes))
$(TR $(TD $(I associative array)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD yes))
$(TR $(TD $(I pointer)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)))
$(TR $(TD $(I SIMD vectors)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD yes) $(TD yes))
$(TR $(TD $(I delegates)) $(TD yes) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD $(MDASH)) $(TD yes) $(TD yes))
)

Enums can be used with all format characters of the base type.

$(H3 $(LNAME2 aggregates, Structs, Unions, Classes, and Interfaces))

Aggregate types can define various `toString` functions. If this
function takes a $(REF_ALTTEXT FormatSpec, FormatSpec, std, format,
spec) or a $(I format string) as argument, the function decides
which format characters are accepted. If no `toString` is defined and
the aggregate is an $(REF_ALTTEXT input range, isInputRange, std,
range, primitives), it is treated like a range, that is $(B 's'), $(B
'r') and a compound specifier are accepted. In all other cases
aggregate types only accept $(B 's').

`toString` should have one of the following signatures:

---
void toString(Writer, Char)(ref Writer w, const ref FormatSpec!Char fmt)
void toString(Writer)(ref Writer w)
string toString();
---

Where `Writer` is an $(REF_ALTTEXT output range, isOutputRange,
std,range,primitives) which accepts characters $(LPAREN)of type
`Char` in the first version$(RPAREN). The template type does not have
to be called `Writer`.

Sometimes it's not possible to use a template, for example when
`toString` overrides `Object.toString`. In this case, the following
$(LPAREN)slower and less flexible$(RPAREN) functions can be used:

---
void toString(void delegate(const(char)[]) sink, const ref FormatSpec!char fmt);
void toString(void delegate(const(char)[]) sink, string fmt);
void toString(void delegate(const(char)[]) sink);
---

When several of the above `toString` versions are available, the
versions with `Writer` take precedence over the versions with a
`sink`. `string toString()` has the lowest priority.

If none of the above mentioned `toString` versions are available, the
aggregates will be formatted by other means, in the following
order:

If an aggregate is an $(REF_ALTTEXT input range, isInputRange, std,
range, primitives), it is formatted like an input range.

If an aggregate is a builtin type (using `alias this`), it is formatted
like the builtin type.

If all else fails, structs are formatted like `Type(field1, field2, ...)`,
classes and interfaces are formatted with their fully qualified name
and unions with their base name.

Copyright: Copyright The D Language Foundation 2000-2013.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP walterbright.com, Walter Bright), $(HTTP erdani.com,
Andrei Alexandrescu), and Kenji Hara

Source: $(PHOBOSSRC std/format/write.d)
 */
module std.format.write;

/**
`bool`s are formatted as `"true"` or `"false"` with `%s` and like the
`byte`s 1 and 0 with all other format characters.
 */
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    formatValue(w1, true, spec1);

    assert(w1.data == "true");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%#x");
    formatValue(w2, true, spec2);

    assert(w2.data == "0x1");
}

/// The `null` literal is formatted as `"null"`.
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%s");
    formatValue(w, null, spec);

    assert(w.data == "null");
}

/**
Integrals are formatted in (signed) every day notation with `%s` and
`%d` and as an (unsigned) image of the underlying bit representation
with `%b` (binary), `%u` (decimal), `%o` (octal), and `%x` (hexadecimal).
 */
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%d");
    formatValue(w1, -1337, spec1);

    assert(w1.data == "-1337");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%x");
    formatValue(w2, -1337, spec2);

    assert(w2.data == "fffffac7");
}

/**
Floating-point values are formatted in natural notation with `%f`, in
scientific notation with `%e`, in short notation with `%g`, and in
hexadecimal scientific notation with `%a`. If a rounding mode is
available, they are rounded according to this rounding mode, otherwise
they are rounded to the nearest value, ties to even.
 */
@safe unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%.3f");
    formatValue(w1, 1337.7779, spec1);

    assert(w1.data == "1337.778");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%.3e");
    formatValue(w2, 1337.7779, spec2);

    assert(w2.data == "1.338e+03");

    auto w3 = appender!string();
    auto spec3 = singleSpec("%.3g");
    formatValue(w3, 1337.7779, spec3);

    assert(w3.data == "1.34e+03");

    auto w4 = appender!string();
    auto spec4 = singleSpec("%.3a");
    formatValue(w4, 1337.7779, spec4);

    assert(w4.data == "0x1.4e7p+10");
}

/**
Individual characters (`char`, `wchar`, or `dchar`) are formatted as
Unicode characters with `%s` and `%c` and as integers (`ubyte`,
`ushort`, `uint`) with all other format characters. With
$(MREF_ALTTEXT compound specifiers, std,format) characters are
treated differently.
 */
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%c");
    formatValue(w1, 'ì', spec1);

    assert(w1.data == "ì");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%#x");
    formatValue(w2, 'ì', spec2);

    assert(w2.data == "0xec");
}

/**
Strings are formatted as a sequence of characters with `%s`.
Non-printable characters are not escaped. With a compound specifier
the string is treated like a range of characters. With $(MREF_ALTTEXT
compound specifiers, std,format) strings are treated differently.
 */
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    formatValue(w1, "hello", spec1);

    assert(w1.data == "hello");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%(%#x%|/%)");
    formatValue(w2, "hello", spec2);

    assert(w2.data == "0x68/0x65/0x6c/0x6c/0x6f");
}

/// Static arrays are formatted as dynamic arrays.
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%s");
    int[2] two = [1, 2];
    formatValue(w, two, spec);

    assert(w.data == "[1, 2]");
}

/**
Dynamic arrays are formatted as input ranges.
 */
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    auto two = [1, 2];
    formatValue(w1, two, spec1);

    assert(w1.data == "[1, 2]");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%(%g%|, %)");
    auto consts = [3.1415926, 299792458, 6.67430e-11];
    formatValue(w2, consts, spec2);

    assert(w2.data == "3.14159, 2.99792e+08, 6.6743e-11");

    // void[] is treated like ubyte[]
    auto w3 = appender!string();
    auto spec3 = singleSpec("%s");
    void[] val = cast(void[]) cast(ubyte[])[1, 2, 3];
    formatValue(w3, val, spec3);

    assert(w3.data == "[1, 2, 3]");
}

/**
Associative arrays are formatted by using `':'` and `", "` as
separators, enclosed by `'['` and `']'` when used with `%s`. It's
also possible to use a compound specifier for better control.

Please note, that the order of the elements is not defined, therefore
the result of this function might differ.
 */
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto aa = [10:17.5, 20:9.99];

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    formatValue(w1, aa, spec1);

    assert(w1.data == "[10:17.5, 20:9.99]" || w1.data == "[20:9.99, 10:17.5]");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%(%x = %.0e%| # %)");
    formatValue(w2, aa, spec2);

    assert(w2.data == "a = 2e+01 # 14 = 1e+01" || w2.data == "14 = 1e+01 # a = 2e+01");
}

/**
`enum`s are formatted as their name when used with `%s` and like
their base value else.
 */
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    enum A { first, second, third }

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    formatValue(w1, A.second, spec1);

    assert(w1.data == "second");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%d");
    formatValue(w2, A.second, spec2);

    assert(w2.data == "1");

    // values of an enum that have no name are formatted with %s using a cast
    A a = A.third;
    a++;

    auto w3 = appender!string();
    auto spec3 = singleSpec("%s");
    formatValue(w3, a, spec3);

    assert(w3.data == "cast(A)3");
}

/**
`structs`, `unions`, `classes` and `interfaces` can be formatted in
several different ways. The following example highlights `struct`
formatting, however, it applies to other aggregates as well.
 */
@safe unittest
{
    import std.array : appender;
    import std.format.spec : FormatSpec, singleSpec;

    // Using a `toString` with a writer
    static struct Point1
    {
        import std.range.primitives : isOutputRange, put;

        int x, y;

        void toString(W)(ref W writer, scope const ref FormatSpec!char f)
        if (isOutputRange!(W, char))
        {
            put(writer, "(");
            formatValue(writer, x, f);
            put(writer, ",");
            formatValue(writer, y, f);
            put(writer, ")");
        }
    }

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    auto p1 = Point1(16, 11);

    formatValue(w1, p1, spec1);
    assert(w1.data == "(16,11)");

    // Using a `toString` with a sink
    static struct Point2
    {
        int x, y;

        void toString(scope void delegate(scope const(char)[]) @safe sink,
                      scope const FormatSpec!char fmt) const
        {
            sink("(");
            sink.formatValue(x, fmt);
            sink(",");
            sink.formatValue(y, fmt);
            sink(")");
        }
    }

    auto w2 = appender!string();
    auto spec2 = singleSpec("%03d");
    auto p2 = Point2(16,11);

    formatValue(w2, p2, spec2);
    assert(w2.data == "(016,011)");

    // Using `string toString()`
    static struct Point3
    {
        int x, y;

        string toString()
        {
            import std.conv : to;

            return "(" ~ to!string(x) ~ "," ~ to!string(y) ~ ")";
        }
    }

    auto w3 = appender!string();
    auto spec3 = singleSpec("%s"); // has to be %s
    auto p3 = Point3(16,11);

    formatValue(w3, p3, spec3);
    assert(w3.data == "(16,11)");

    // without `toString`
    static struct Point4
    {
        int x, y;
    }

    auto w4 = appender!string();
    auto spec4 = singleSpec("%s"); // has to be %s
    auto p4 = Point4(16,11);

    formatValue(w4, p4, spec3);
    assert(w4.data == "Point4(16, 11)");
}

/// Pointers are formatted as hexadecimal integers.
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    auto p1 = () @trusted { return cast(void*) 0xFFEECCAA; } ();
    formatValue(w1, p1, spec1);

    assert(w1.data == "FFEECCAA");

    // null pointers are printed as `"null"` when used with `%s` and as hexadecimal integer else
    auto w2 = appender!string();
    auto spec2 = singleSpec("%s");
    auto p2 = () @trusted { return cast(void*) 0x00000000; } ();
    formatValue(w2, p2, spec2);

    assert(w2.data == "null");

    auto w3 = appender!string();
    auto spec3 = singleSpec("%x");
    formatValue(w3, p2, spec3);

    assert(w3.data == "0");
}

/// SIMD vectors are formatted as arrays.
@safe unittest
{
    import core.simd; // cannot be selective, because float4 might not be defined
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%s");

    static if (is(float4))
    {
        version (X86) {}
        else
        {
            float4 f4;
            f4.array[0] = 1;
            f4.array[1] = 2;
            f4.array[2] = 3;
            f4.array[3] = 4;

            formatValue(w, f4, spec);
            assert(w.data == "[1, 2, 3, 4]");
        }
    }
}

import std.format.internal.write;

import std.format.spec : FormatSpec;
import std.traits : isSomeString;

/**
Converts its arguments according to a format string and writes
the result to an output range.

The second version of `formattedWrite` takes the format string as a
template argument. In this case, it is checked for consistency at
compile-time.

Params:
    w = an $(REF_ALTTEXT output range, isOutputRange, std, range, primitives),
        where the formatted result is written to
    fmt = a $(MREF_ALTTEXT format string, std,format)
    args = a variadic list of arguments to be formatted
    Writer = the type of the writer `w`
    Char = character type of `fmt`
    Args = a variadic list of types of the arguments

Returns:
    The index of the last argument that was formatted. If no positional
    arguments are used, this is the number of arguments that where formatted.

Throws:
    A $(REF_ALTTEXT FormatException, FormatException, std, format)
    if formatting did not succeed.

Note:
    In theory this function should be `@nogc`. But with the current
    implementation there are some cases where allocations occur.
    See $(REF_ALTTEXT $(D sformat), sformat, std, format) for more details.
 */
uint formattedWrite(Writer, Char, Args...)(auto ref Writer w, const scope Char[] fmt, Args args)
{
    import std.conv : text;
    import std.format : enforceFmt, FormatException;
    import std.traits : isSomeChar;

    auto spec = FormatSpec!Char(fmt);

    // Are we already done with formats? Then just dump each parameter in turn
    uint currentArg = 0;
    while (spec.writeUpToNextSpec(w))
    {
        if (currentArg == Args.length && !spec.indexStart)
        {
            // leftover spec?
            enforceFmt(fmt.length == 0,
                text("Orphan format specifier: %", spec.spec));
            break;
        }

        if (spec.width == spec.DYNAMIC)
        {
            auto width = getNthInt!"integer width"(currentArg, args);
            if (width < 0)
            {
                spec.flDash = true;
                width = -width;
            }
            spec.width = width;
            ++currentArg;
        }
        else if (spec.width < 0)
        {
            // means: get width as a positional parameter
            auto index = cast(uint) -spec.width;
            assert(index > 0, "The index must be greater than zero");
            auto width = getNthInt!"integer width"(index - 1, args);
            if (currentArg < index) currentArg = index;
            if (width < 0)
            {
                spec.flDash = true;
                width = -width;
            }
            spec.width = width;
        }

        if (spec.precision == spec.DYNAMIC)
        {
            auto precision = getNthInt!"integer precision"(currentArg, args);
            if (precision >= 0) spec.precision = precision;
            // else negative precision is same as no precision
            else spec.precision = spec.UNSPECIFIED;
            ++currentArg;
        }
        else if (spec.precision < 0)
        {
            // means: get precision as a positional parameter
            auto index = cast(uint) -spec.precision;
            assert(index > 0, "The precision must be greater than zero");
            auto precision = getNthInt!"integer precision"(index- 1, args);
            if (currentArg < index) currentArg = index;
            if (precision >= 0) spec.precision = precision;
            // else negative precision is same as no precision
            else spec.precision = spec.UNSPECIFIED;
        }

        if (spec.separators == spec.DYNAMIC)
        {
            auto separators = getNthInt!"separator digit width"(currentArg, args);
            spec.separators = separators;
            ++currentArg;
        }

        if (spec.dynamicSeparatorChar)
        {
            auto separatorChar =
                getNth!("separator character", isSomeChar, dchar)(currentArg, args);
            spec.separatorChar = separatorChar;
            spec.dynamicSeparatorChar = false;
            ++currentArg;
        }

        if (currentArg == Args.length && !spec.indexStart)
        {
            // leftover spec?
            enforceFmt(fmt.length == 0,
                text("Orphan format specifier: %", spec.spec));
            break;
        }

        // Format an argument
        // This switch uses a static foreach to generate a jump table.
        // Currently `spec.indexStart` use the special value '0' to signal
        // we should use the current argument. An enhancement would be to
        // always store the index.
        size_t index = currentArg;
        if (spec.indexStart != 0)
            index = spec.indexStart - 1;
        else
            ++currentArg;
    SWITCH: switch (index)
        {
            foreach (i, Tunused; Args)
            {
            case i:
                formatValue(w, args[i], spec);
                if (currentArg < spec.indexEnd)
                    currentArg = spec.indexEnd;
                // A little know feature of format is to format a range
                // of arguments, e.g. `%1:3$` will format the first 3
                // arguments. Since they have to be consecutive we can
                // just use explicit fallthrough to cover that case.
                if (i + 1 < spec.indexEnd)
                {
                    // You cannot goto case if the next case is the default
                    static if (i + 1 < Args.length)
                        goto case;
                    else
                        goto default;
                }
                else
                    break SWITCH;
            }
        default:
            throw new FormatException(
                text("Positional specifier %", spec.indexStart, '$', spec.spec,
                     " index exceeds ", Args.length));
        }
    }
    return currentArg;
}

///
@safe pure unittest
{
    import std.array : appender;

    auto writer1 = appender!string();
    formattedWrite(writer1, "%s is the ultimate %s.", 42, "answer");
    assert(writer1[] == "42 is the ultimate answer.");

    auto writer2 = appender!string();
    formattedWrite(writer2, "Increase: %7.2f %%", 17.4285);
    assert(writer2[] == "Increase:   17.43 %");
}

/// ditto
uint formattedWrite(alias fmt, Writer, Args...)(auto ref Writer w, Args args)
if (isSomeString!(typeof(fmt)))
{
    import std.format : checkFormatException;

    alias e = checkFormatException!(fmt, Args);
    static assert(!e, e);
    return .formattedWrite(w, fmt, args);
}

/// The format string can be checked at compile-time:
@safe pure unittest
{
    import std.array : appender;

    auto writer = appender!string();
    writer.formattedWrite!"%d is the ultimate %s."(42, "answer");
    assert(writer[] == "42 is the ultimate answer.");

    // This line doesn't compile, because 3.14 cannot be formatted with %d:
    // writer.formattedWrite!"%d is the ultimate %s."(3.14, "answer");
}

@safe pure unittest
{
    import std.array : appender;

    auto stream = appender!string();
    formattedWrite(stream, "%s", 1.1);
    assert(stream.data == "1.1", stream.data);
}

@safe pure unittest
{
    import std.array;

    auto w = appender!string();
    formattedWrite(w, "%s %d", "@safe/pure", 42);
    assert(w.data == "@safe/pure 42");
}

@safe pure unittest
{
    char[20] buf;
    auto w = buf[];
    formattedWrite(w, "%s %d", "@safe/pure", 42);
    assert(buf[0 .. $ - w.length] == "@safe/pure 42");
}

@safe pure unittest
{
    import std.algorithm.iteration : map;
    import std.array : appender;

    auto stream = appender!string();
    formattedWrite(stream, "%s", map!"a*a"([2, 3, 5]));
    assert(stream.data == "[4, 9, 25]", stream.data);

    // Test shared data.
    stream = appender!string();
    shared int s = 6;
    formattedWrite(stream, "%s", s);
    assert(stream.data == "6");
}

@safe pure unittest
{
    // testing positional parameters
    import std.array : appender;
    import std.exception : collectExceptionMsg;
    import std.format : FormatException;

    auto w = appender!(char[])();
    formattedWrite(w,
            "Numbers %2$s and %1$s are reversed and %1$s%2$s repeated",
            42, 0);
    assert(w.data == "Numbers 0 and 42 are reversed and 420 repeated",
            w.data);
    assert(collectExceptionMsg!FormatException(formattedWrite(w, "%1$s, %3$s", 1, 2))
        == "Positional specifier %3$s index exceeds 2");

    w.clear();
    formattedWrite(w, "asd%s", 23);
    assert(w.data == "asd23", w.data);
    w.clear();
    formattedWrite(w, "%s%s", 23, 45);
    assert(w.data == "2345", w.data);
}

// https://issues.dlang.org/show_bug.cgi?id=3479
@safe unittest
{
    import std.array : appender;

    auto stream = appender!(char[])();
    formattedWrite(stream, "%2$.*1$d", 12, 10);
    assert(stream.data == "000000000010", stream.data);
}

// https://issues.dlang.org/show_bug.cgi?id=6893
@safe unittest
{
    import std.array : appender;

    enum E : ulong { A, B, C }
    auto stream = appender!(char[])();
    formattedWrite(stream, "%s", E.C);
    assert(stream.data == "C");
}

@safe pure unittest
{
    import std.array : appender;

    auto stream = appender!string();
    formattedWrite(stream, "%u", 42);
    assert(stream.data == "42", stream.data);
}

@safe pure unittest
{
    // testing raw writes
    import std.array : appender;

    auto w = appender!(char[])();
    uint a = 0x02030405;
    formattedWrite(w, "%+r", a);
    assert(w.data.length == 4 && w.data[0] == 2 && w.data[1] == 3
        && w.data[2] == 4 && w.data[3] == 5);

    w.clear();
    formattedWrite(w, "%-r", a);
    assert(w.data.length == 4 && w.data[0] == 5 && w.data[1] == 4
        && w.data[2] == 3 && w.data[3] == 2);
}

@safe unittest
{
    import std.array : appender;
    import std.conv : text, octal;

    auto stream = appender!(char[])();

    formattedWrite(stream, "hello world! %s %s ", true, 57, 1_000_000_000, 'x', " foo");
    assert(stream.data == "hello world! true 57 ", stream.data);
    stream.clear();

    formattedWrite(stream, "%g %A %s", 1.67, -1.28, float.nan);
    assert(stream.data == "1.67 -0X1.47AE147AE147BP+0 nan", stream.data);
    stream.clear();

    formattedWrite(stream, "%x %X", 0x1234AF, 0xAFAFAFAF);
    assert(stream.data == "1234af AFAFAFAF");
    stream.clear();

    formattedWrite(stream, "%b %o", 0x1234AF, 0xAFAFAFAF);
    assert(stream.data == "100100011010010101111 25753727657");
    stream.clear();

    formattedWrite(stream, "%d %s", 0x1234AF, 0xAFAFAFAF);
    assert(stream.data == "1193135 2947526575");
    stream.clear();

    formattedWrite(stream, "%a %A", 1.32, 6.78f);
    assert(stream.data == "0x1.51eb851eb851fp+0 0X1.B1EB86P+2");
    stream.clear();

    formattedWrite(stream, "%#06.*f", 2, 12.345);
    assert(stream.data == "012.35");
    stream.clear();

    formattedWrite(stream, "%#0*.*f", 6, 2, 12.345);
    assert(stream.data == "012.35");
    stream.clear();

    const real constreal = 1;
    formattedWrite(stream, "%g",constreal);
    assert(stream.data == "1");
    stream.clear();

    formattedWrite(stream, "%7.4g:", 12.678);
    assert(stream.data == "  12.68:");
    stream.clear();

    formattedWrite(stream, "%7.4g:", 12.678L);
    assert(stream.data == "  12.68:");
    stream.clear();

    formattedWrite(stream, "%04f|%05d|%#05x|%#5x", -4.0, -10, 1, 1);
    assert(stream.data == "-4.000000|-0010|0x001|  0x1", stream.data);
    stream.clear();

    int i;
    string s;

    i = -10;
    formattedWrite(stream, "%d|%3d|%03d|%1d|%01.4f", i, i, i, i, cast(double) i);
    assert(stream.data == "-10|-10|-10|-10|-10.0000");
    stream.clear();

    i = -5;
    formattedWrite(stream, "%d|%3d|%03d|%1d|%01.4f", i, i, i, i, cast(double) i);
    assert(stream.data == "-5| -5|-05|-5|-5.0000");
    stream.clear();

    i = 0;
    formattedWrite(stream, "%d|%3d|%03d|%1d|%01.4f", i, i, i, i, cast(double) i);
    assert(stream.data == "0|  0|000|0|0.0000");
    stream.clear();

    i = 5;
    formattedWrite(stream, "%d|%3d|%03d|%1d|%01.4f", i, i, i, i, cast(double) i);
    assert(stream.data == "5|  5|005|5|5.0000");
    stream.clear();

    i = 10;
    formattedWrite(stream, "%d|%3d|%03d|%1d|%01.4f", i, i, i, i, cast(double) i);
    assert(stream.data == "10| 10|010|10|10.0000");
    stream.clear();

    formattedWrite(stream, "%.0d", 0);
    assert(stream.data == "0");
    stream.clear();

    formattedWrite(stream, "%.g", .34);
    assert(stream.data == "0.3");
    stream.clear();

    stream.clear();
    formattedWrite(stream, "%.0g", .34);
    assert(stream.data == "0.3");

    stream.clear();
    formattedWrite(stream, "%.2g", .34);
    assert(stream.data == "0.34");

    stream.clear();
    formattedWrite(stream, "%0.0008f", 1e-08);
    assert(stream.data == "0.00000001");

    stream.clear();
    formattedWrite(stream, "%0.0008f", 1e-05);
    assert(stream.data == "0.00001000");

    s = "helloworld";
    string r;
    stream.clear();
    formattedWrite(stream, "%.2s", s[0 .. 5]);
    assert(stream.data == "he");
    stream.clear();
    formattedWrite(stream, "%.20s", s[0 .. 5]);
    assert(stream.data == "hello");
    stream.clear();
    formattedWrite(stream, "%8s", s[0 .. 5]);
    assert(stream.data == "   hello");

    byte[] arrbyte = new byte[4];
    arrbyte[0] = 100;
    arrbyte[1] = -99;
    arrbyte[3] = 0;
    stream.clear();
    formattedWrite(stream, "%s", arrbyte);
    assert(stream.data == "[100, -99, 0, 0]", stream.data);

    ubyte[] arrubyte = new ubyte[4];
    arrubyte[0] = 100;
    arrubyte[1] = 200;
    arrubyte[3] = 0;
    stream.clear();
    formattedWrite(stream, "%s", arrubyte);
    assert(stream.data == "[100, 200, 0, 0]", stream.data);

    short[] arrshort = new short[4];
    arrshort[0] = 100;
    arrshort[1] = -999;
    arrshort[3] = 0;
    stream.clear();
    formattedWrite(stream, "%s", arrshort);
    assert(stream.data == "[100, -999, 0, 0]");
    stream.clear();
    formattedWrite(stream, "%s", arrshort);
    assert(stream.data == "[100, -999, 0, 0]");

    ushort[] arrushort = new ushort[4];
    arrushort[0] = 100;
    arrushort[1] = 20_000;
    arrushort[3] = 0;
    stream.clear();
    formattedWrite(stream, "%s", arrushort);
    assert(stream.data == "[100, 20000, 0, 0]");

    int[] arrint = new int[4];
    arrint[0] = 100;
    arrint[1] = -999;
    arrint[3] = 0;
    stream.clear();
    formattedWrite(stream, "%s", arrint);
    assert(stream.data == "[100, -999, 0, 0]");
    stream.clear();
    formattedWrite(stream, "%s", arrint);
    assert(stream.data == "[100, -999, 0, 0]");

    long[] arrlong = new long[4];
    arrlong[0] = 100;
    arrlong[1] = -999;
    arrlong[3] = 0;
    stream.clear();
    formattedWrite(stream, "%s", arrlong);
    assert(stream.data == "[100, -999, 0, 0]");
    stream.clear();
    formattedWrite(stream, "%s",arrlong);
    assert(stream.data == "[100, -999, 0, 0]");

    ulong[] arrulong = new ulong[4];
    arrulong[0] = 100;
    arrulong[1] = 999;
    arrulong[3] = 0;
    stream.clear();
    formattedWrite(stream, "%s", arrulong);
    assert(stream.data == "[100, 999, 0, 0]");

    string[] arr2 = new string[4];
    arr2[0] = "hello";
    arr2[1] = "world";
    arr2[3] = "foo";
    stream.clear();
    formattedWrite(stream, "%s", arr2);
    assert(stream.data == `["hello", "world", "", "foo"]`, stream.data);

    stream.clear();
    formattedWrite(stream, "%.8d", 7);
    assert(stream.data == "00000007");

    stream.clear();
    formattedWrite(stream, "%.8x", 10);
    assert(stream.data == "0000000a");

    stream.clear();
    formattedWrite(stream, "%-3d", 7);
    assert(stream.data == "7  ");

    stream.clear();
    formattedWrite(stream, "%*d", -3, 7);
    assert(stream.data == "7  ");

    stream.clear();
    formattedWrite(stream, "%.*d", -3, 7);
    assert(stream.data == "7");

    stream.clear();
    formattedWrite(stream, "%s", "abc"c);
    assert(stream.data == "abc");
    stream.clear();
    formattedWrite(stream, "%s", "def"w);
    assert(stream.data == "def", text(stream.data.length));
    stream.clear();
    formattedWrite(stream, "%s", "ghi"d);
    assert(stream.data == "ghi");

    @trusted void* deadBeef() { return cast(void*) 0xDEADBEEF; }
    stream.clear();
    formattedWrite(stream, "%s", deadBeef());
    assert(stream.data == "DEADBEEF", stream.data);

    stream.clear();
    formattedWrite(stream, "%#x", 0xabcd);
    assert(stream.data == "0xabcd");
    stream.clear();
    formattedWrite(stream, "%#X", 0xABCD);
    assert(stream.data == "0XABCD");

    stream.clear();
    formattedWrite(stream, "%#o", octal!12345);
    assert(stream.data == "012345");
    stream.clear();
    formattedWrite(stream, "%o", 9);
    assert(stream.data == "11");

    stream.clear();
    formattedWrite(stream, "%+d", 123);
    assert(stream.data == "+123");
    stream.clear();
    formattedWrite(stream, "%+d", -123);
    assert(stream.data == "-123");
    stream.clear();
    formattedWrite(stream, "% d", 123);
    assert(stream.data == " 123");
    stream.clear();
    formattedWrite(stream, "% d", -123);
    assert(stream.data == "-123");

    stream.clear();
    formattedWrite(stream, "%%");
    assert(stream.data == "%");

    stream.clear();
    formattedWrite(stream, "%d", true);
    assert(stream.data == "1");
    stream.clear();
    formattedWrite(stream, "%d", false);
    assert(stream.data == "0");

    stream.clear();
    formattedWrite(stream, "%d", 'a');
    assert(stream.data == "97", stream.data);
    wchar wc = 'a';
    stream.clear();
    formattedWrite(stream, "%d", wc);
    assert(stream.data == "97");
    dchar dc = 'a';
    stream.clear();
    formattedWrite(stream, "%d", dc);
    assert(stream.data == "97");

    byte b = byte.max;
    stream.clear();
    formattedWrite(stream, "%x", b);
    assert(stream.data == "7f");
    stream.clear();
    formattedWrite(stream, "%x", ++b);
    assert(stream.data == "80");
    stream.clear();
    formattedWrite(stream, "%x", ++b);
    assert(stream.data == "81");

    short sh = short.max;
    stream.clear();
    formattedWrite(stream, "%x", sh);
    assert(stream.data == "7fff");
    stream.clear();
    formattedWrite(stream, "%x", ++sh);
    assert(stream.data == "8000");
    stream.clear();
    formattedWrite(stream, "%x", ++sh);
    assert(stream.data == "8001");

    i = int.max;
    stream.clear();
    formattedWrite(stream, "%x", i);
    assert(stream.data == "7fffffff");
    stream.clear();
    formattedWrite(stream, "%x", ++i);
    assert(stream.data == "80000000");
    stream.clear();
    formattedWrite(stream, "%x", ++i);
    assert(stream.data == "80000001");

    stream.clear();
    formattedWrite(stream, "%x", 10);
    assert(stream.data == "a");
    stream.clear();
    formattedWrite(stream, "%X", 10);
    assert(stream.data == "A");
    stream.clear();
    formattedWrite(stream, "%x", 15);
    assert(stream.data == "f");
    stream.clear();
    formattedWrite(stream, "%X", 15);
    assert(stream.data == "F");

    @trusted void ObjectTest()
    {
        Object c = null;
        stream.clear();
        formattedWrite(stream, "%s", c);
        assert(stream.data == "null");
    }
    ObjectTest();

    enum TestEnum
    {
        Value1, Value2
    }
    stream.clear();
    formattedWrite(stream, "%s", TestEnum.Value2);
    assert(stream.data == "Value2", stream.data);
    stream.clear();
    formattedWrite(stream, "%s", cast(TestEnum) 5);
    assert(stream.data == "cast(TestEnum)5", stream.data);

    //immutable(char[5])[int] aa = ([3:"hello", 4:"betty"]);
    //stream.clear();
    //formattedWrite(stream, "%s", aa.values);
    //assert(stream.data == "[[h,e,l,l,o],[b,e,t,t,y]]");
    //stream.clear();
    //formattedWrite(stream, "%s", aa);
    //assert(stream.data == "[3:[h,e,l,l,o],4:[b,e,t,t,y]]");

    static const dchar[] ds = ['a','b'];
    for (int j = 0; j < ds.length; ++j)
    {
        stream.clear(); formattedWrite(stream, " %d", ds[j]);
        if (j == 0)
            assert(stream.data == " 97");
        else
            assert(stream.data == " 98");
    }

    stream.clear();
    formattedWrite(stream, "%.-3d", 7);
    assert(stream.data == "7", ">" ~ stream.data ~ "<");
}

@safe unittest
{
    import std.array : appender;
    import std.meta : AliasSeq;

    immutable(char[5])[int] aa = ([3:"hello", 4:"betty"]);
    assert(aa[3] == "hello");
    assert(aa[4] == "betty");

    auto stream = appender!(char[])();
    alias AllNumerics =
        AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong,
                  float, double, real);
    foreach (T; AllNumerics)
    {
        T value = 1;
        stream.clear();
        formattedWrite(stream, "%s", value);
        assert(stream.data == "1");
    }

    stream.clear();
    formattedWrite(stream, "%s", aa);
}

/**
Formats a value of any type according to a format specifier and
writes the result to an output range.

More details about how types are formatted, and how the format
specifier influences the outcome, can be found in the definition of a
$(MREF_ALTTEXT format string, std,format).

Params:
    w = an $(REF_ALTTEXT output range, isOutputRange, std, range, primitives) where
        the formatted value is written to
    val = the value to write
    f = a $(REF_ALTTEXT FormatSpec, FormatSpec, std, format, spec) defining the
        format specifier
    Writer = the type of the output range `w`
    T = the type of value `val`
    Char = the character type used for `f`

Throws:
    A $(LREF FormatException) if formatting did not succeed.

Note:
    In theory this function should be `@nogc`. But with the current
    implementation there are some cases where allocations occur.
    See $(REF_ALTTEXT $(D sformat), sformat, std, format) for more details.

See_Also:
    $(LREF formattedWrite) which formats several values at once.
 */
void formatValue(Writer, T, Char)(auto ref Writer w, auto ref T val, scope const ref FormatSpec!Char f)
{
    import std.format : enforceFmt;

    enforceFmt(f.width != f.DYNAMIC && f.precision != f.DYNAMIC
               && f.separators != f.DYNAMIC && !f.dynamicSeparatorChar,
               "Dynamic argument not allowed for `formatValue`");

    formatValueImpl(w, val, f);
}

///
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto writer = appender!string();
    auto spec = singleSpec("%08b");
    writer.formatValue(42, spec);
    assert(writer.data == "00101010");

    spec = singleSpec("%2s");
    writer.formatValue('=', spec);
    assert(writer.data == "00101010 =");

    spec = singleSpec("%+14.6e");
    writer.formatValue(42.0, spec);
    assert(writer.data == "00101010 = +4.200000e+01");
}

// https://issues.dlang.org/show_bug.cgi?id=15386
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : FormatSpec;
    import std.format : FormatException;
    import std.exception : assertThrown;

    auto w = appender!(char[])();
    auto dor = appender!(char[])();
    auto fs = FormatSpec!char("%.*s");
    fs.writeUpToNextSpec(dor);
    assertThrown!FormatException(formatValue(w, 0, fs));

    fs = FormatSpec!char("%*s");
    fs.writeUpToNextSpec(dor);
    assertThrown!FormatException(formatValue(w, 0, fs));

    fs = FormatSpec!char("%,*s");
    fs.writeUpToNextSpec(dor);
    assertThrown!FormatException(formatValue(w, 0, fs));

    fs = FormatSpec!char("%,?s");
    fs.writeUpToNextSpec(dor);
    assertThrown!FormatException(formatValue(w, 0, fs));

    assertThrown!FormatException(formattedWrite(w, "%(%0*d%)", new int[1]));
}

// https://issues.dlang.org/show_bug.cgi?id=22609
@safe pure unittest
{
    static enum State: ubyte { INACTIVE }
    static struct S {
        State state = State.INACTIVE;
        int generation = 1;
        alias state this;
        // DMDBUG: https://issues.dlang.org/show_bug.cgi?id=16657
        auto opEquals(S other) const { return state == other.state && generation == other.generation; }
        auto opEquals(State other) const { return state == other; }
    }

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto writer = appender!string();
    const spec = singleSpec("%s");
    S a;
    writer.formatValue(a, spec);
    assert(writer.data == "0");
}

// https://issues.dlang.org/show_bug.cgi?id=23400
@safe pure unittest
{
    import std.range : nullSink;
    import std.format.spec : singleSpec;

    static struct S
    {
        // non-const opEquals method
        bool opEquals(S rhs) { return false; }
    }

    enum E { a = S() }

    E e;
    auto writer = nullSink;
    const spec = singleSpec("%s");
    writer.formatValue(e, spec);
}
