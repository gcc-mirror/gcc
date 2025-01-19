// Written in the D programming language.

/*
   Copyright: Copyright The D Language Foundation 2000-2013.

   License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

   Authors: $(HTTP walterbright.com, Walter Bright), $(HTTP erdani.com,
   Andrei Alexandrescu), and Kenji Hara

   Source: $(PHOBOSSRC std/format/internal/write.d)
 */
module std.format.internal.write;

import std.format.spec : FormatSpec;
import std.range.primitives : isInputRange;
import std.traits;

version (StdUnittest)
{
    import std.exception : assertCTFEable;
    import std.format : format;
}

package(std.format):

/*
    `bool`s are formatted as `"true"` or `"false"` with `%s` and as `1` or
    `0` with integral-specific format specs.
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, const(T) obj, scope const ref FormatSpec!Char f)
if (is(BooleanTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    BooleanTypeOf!T val = obj;

    if (f.spec == 's')
        writeAligned(w, val ? "true" : "false", f);
    else
        formatValueImpl(w, cast(byte) val, f);
}

@safe pure unittest
{
    assertCTFEable!(
    {
        formatTest(false, "false");
        formatTest(true,  "true");
    });
}

@safe unittest
{
    struct S1
    {
        bool val;
        alias val this;
    }

    struct S2
    {
        bool val;
        alias val this;
        string toString() const { return "S"; }
    }

    formatTest(S1(false), "false");
    formatTest(S1(true),  "true");
    formatTest(S2(false), "S");
    formatTest(S2(true),  "S");
}

@safe pure unittest
{
    string t1 = format("[%6s] [%6s] [%-6s]", true, false, true);
    assert(t1 == "[  true] [ false] [true  ]");

    string t2 = format("[%3s] [%-2s]", true, false);
    assert(t2 == "[true] [false]");
}

// https://issues.dlang.org/show_bug.cgi?id=20534
@safe pure unittest
{
    assert(format("%r",false) == "\0");
}

@safe pure unittest
{
    assert(format("%07s",true) == "   true");
}

@safe pure unittest
{
    assert(format("%=8s",true)    == "  true  ");
    assert(format("%=9s",false)   == "  false  ");
    assert(format("%=9s",true)    == "   true  ");
    assert(format("%-=9s",true)   == "  true   ");
    assert(format("%=10s",false)  == "   false  ");
    assert(format("%-=10s",false) == "  false   ");
}

/*
    `null` literal is formatted as `"null"`
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, const(T) obj, scope const ref FormatSpec!Char f)
if (is(immutable T == immutable typeof(null)) && !is(T == enum) && !hasToString!(T, Char))
{
    import std.format : enforceFmt;

    const spec = f.spec;
    enforceFmt(spec == 's', "null literal cannot match %" ~ spec);

    writeAligned(w, "null", f);
}

@safe pure unittest
{
    import std.exception : collectExceptionMsg;
    import std.format : FormatException;
    import std.range.primitives : back;

    assert(collectExceptionMsg!FormatException(format("%p", null)).back == 'p');

    assertCTFEable!(
    {
        formatTest(null, "null");
    });
}

@safe pure unittest
{
    string t = format("[%6s] [%-6s]", null, null);
    assert(t == "[  null] [null  ]");
}

/*
    Integrals are formatted like $(REF printf, core, stdc, stdio).
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, const(T) obj, scope const ref FormatSpec!Char f)
if (is(IntegralTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    alias U = IntegralTypeOf!T;
    U val = obj;    // Extracting alias this may be impure/system/may-throw

    if (f.spec == 'r')
    {
        // raw write, skip all else and write the thing
        auto raw = (ref val) @trusted {
            return (cast(const char*) &val)[0 .. val.sizeof];
        }(val);
        import std.range.primitives : put;
        if (needToSwapEndianess(f))
            foreach_reverse (c; raw)
                put(w, c);
        else
            foreach (c; raw)
                put(w, c);
        return;
    }

    static if (isSigned!U)
    {
        const negative = val < 0 && f.spec != 'x' && f.spec != 'X' && f.spec != 'b' && f.spec != 'o' && f.spec != 'u';
        ulong arg = negative ? -cast(ulong) val : val;
    }
    else
    {
        const negative = false;
        ulong arg = val;
    }
    arg &= Unsigned!U.max;

    formatValueImplUlong!(Writer, Char)(w, arg, negative, f);
}

// Helper function for `formatValueImpl` that avoids template bloat
private void formatValueImplUlong(Writer, Char)(auto ref Writer w, ulong arg, in bool negative,
                                                scope const ref FormatSpec!Char f)
{
    immutable uint base = baseOfSpec(f.spec);

    const bool zero = arg == 0;
    char[64] digits = void;
    size_t pos = digits.length - 1;
    do
    {
        /* `cast(char)` is needed because value range propagation (VRP) cannot
         * analyze `base` because it’s computed in a separate function
         * (`baseOfSpec`). */
        digits[pos--] = cast(char) ('0' + arg % base);
        if (base > 10 && digits[pos + 1] > '9')
            digits[pos + 1] += ((f.spec == 'x' || f.spec == 'a') ? 'a' : 'A') - '0' - 10;
        arg /= base;
    } while (arg > 0);

    char[3] prefix = void;
    size_t left = 2;
    size_t right = 2;

    // add sign
    if (f.spec != 'x' && f.spec != 'X' && f.spec != 'b' && f.spec != 'o' && f.spec != 'u')
    {
        if (negative)
            prefix[right++] = '-';
        else if (f.flPlus)
            prefix[right++] = '+';
        else if (f.flSpace)
            prefix[right++] = ' ';
    }

    // not a floating point like spec
    if (f.spec == 'x' || f.spec == 'X' || f.spec == 'b' || f.spec == 'o' || f.spec == 'u'
        || f.spec == 'd' || f.spec == 's')
    {
        if (f.flHash && (base == 16) && !zero)
        {
            prefix[--left] = f.spec;
            prefix[--left] = '0';
        }
        if (f.flHash && (base == 8) && !zero
            && (digits.length - (pos + 1) >= f.precision || f.precision == f.UNSPECIFIED))
            prefix[--left] = '0';

        writeAligned(w, prefix[left .. right], digits[pos + 1 .. $], "", f, true);
        return;
    }

    FormatSpec!Char fs = f;
    if (f.precision == f.UNSPECIFIED)
        fs.precision = cast(typeof(fs.precision)) (digits.length - pos - 2);

    // %f like output
    if (f.spec == 'f' || f.spec == 'F'
        || ((f.spec == 'g' || f.spec == 'G') && (fs.precision >= digits.length - pos - 2)))
    {
        if (f.precision == f.UNSPECIFIED)
            fs.precision = 0;

        writeAligned(w, prefix[left .. right], digits[pos + 1 .. $], ".", "", fs,
                     (f.spec == 'g' || f.spec == 'G') ? PrecisionType.allDigits : PrecisionType.fractionalDigits);

        return;
    }

    import std.algorithm.searching : all;

    // at least one digit for %g
    if ((f.spec == 'g' || f.spec == 'G') && fs.precision == 0)
        fs.precision = 1;

    // rounding
    size_t digit_end = pos + fs.precision + ((f.spec == 'g' || f.spec == 'G') ? 1 : 2);
    if (digit_end <= digits.length)
    {
        RoundingClass rt = RoundingClass.ZERO;
        if (digit_end < digits.length)
        {
            auto tie = (f.spec == 'a' || f.spec == 'A') ? '8' : '5';
            if (digits[digit_end] >= tie)
            {
                rt = RoundingClass.UPPER;
                if (digits[digit_end] == tie && digits[digit_end + 1 .. $].all!(a => a == '0'))
                    rt = RoundingClass.FIVE;
            }
            else
            {
                rt = RoundingClass.LOWER;
                if (digits[digit_end .. $].all!(a => a == '0'))
                    rt = RoundingClass.ZERO;
            }
        }

        if (round(digits, pos + 1, digit_end, rt, negative,
                  f.spec == 'a' ? 'f' : (f.spec == 'A' ? 'F' : '9')))
        {
            pos--;
            digit_end--;
        }
    }

    // convert to scientific notation
    char[1] int_digit = void;
    int_digit[0] = digits[pos + 1];
    digits[pos + 1] = '.';

    char[4] suffix = void;

    if (f.spec == 'e' || f.spec == 'E' || f.spec == 'g' || f.spec == 'G')
    {
        suffix[0] = (f.spec == 'e' || f.spec == 'g') ? 'e' : 'E';
        suffix[1] = '+';
        suffix[2] = cast(char) ('0' + (digits.length - pos - 2) / 10);
        suffix[3] = cast(char) ('0' + (digits.length - pos - 2) % 10);
    }
    else
    {
        if (right == 3)
            prefix[0] = prefix[2];
        prefix[1] = '0';
        prefix[2] = f.spec == 'a' ? 'x' : 'X';

        left = right == 3 ? 0 : 1;
        right = 3;

        suffix[0] = f.spec == 'a' ? 'p' : 'P';
        suffix[1] = '+';
        suffix[2] = cast(char) ('0' + ((digits.length - pos - 2) * 4) / 10);
        suffix[3] = cast(char) ('0' + ((digits.length - pos - 2) * 4) % 10);
    }

    import std.algorithm.comparison : min;

    // remove trailing zeros
    if ((f.spec == 'g' || f.spec == 'G') && !f.flHash)
    {
        digit_end = min(digit_end, digits.length);
        while (digit_end > pos + 1 &&
               (digits[digit_end - 1] == '0' || digits[digit_end - 1] == '.'))
            digit_end--;
    }

    writeAligned(w, prefix[left .. right], int_digit[0 .. $],
                 digits[pos + 1 .. min(digit_end, $)],
                 suffix[0 .. $], fs,
                 (f.spec == 'g' || f.spec == 'G') ? PrecisionType.allDigits : PrecisionType.fractionalDigits);
}

private uint baseOfSpec(in char spec) @safe pure
{
    typeof(return) base =
        spec == 'x' || spec == 'X' || spec == 'a' || spec == 'A' ? 16 :
        spec == 'o' ? 8 :
        spec == 'b' ? 2 :
        spec == 's' || spec == 'd' || spec == 'u'
        || spec == 'e' || spec == 'E' || spec == 'f' || spec == 'F'
        || spec == 'g' || spec == 'G' ? 10 :
        0;

    import std.format : enforceFmt;
    enforceFmt(base > 0,
        "incompatible format character for integral argument: %" ~ spec);

    return base;
}

@safe pure unittest
{
    assertCTFEable!(
    {
        formatTest(byte.min, "-128");
        formatTest(byte.max, "127");
        formatTest(short.min, "-32768");
        formatTest(short.max, "32767");
        formatTest(int.min, "-2147483648");
        formatTest(int.max, "2147483647");
        formatTest(long.min, "-9223372036854775808");
        formatTest(long.max, "9223372036854775807");

        formatTest(ubyte.min, "0");
        formatTest(ubyte.max, "255");
        formatTest(ushort.min, "0");
        formatTest(ushort.max, "65535");
        formatTest(uint.min, "0");
        formatTest(uint.max, "4294967295");
        formatTest(ulong.min, "0");
        formatTest(ulong.max, "18446744073709551615");
    });
}

// https://issues.dlang.org/show_bug.cgi?id=18838
@safe pure unittest
{
    assert("%12,d".format(0) == "           0");
}

@safe pure unittest
{
    import std.exception : collectExceptionMsg;
    import std.format : FormatException;
    import std.range.primitives : back;

    assert(collectExceptionMsg!FormatException(format("%c", 5)).back == 'c');

    assertCTFEable!(
    {
        formatTest(9, "9");
        formatTest(10, "10");
    });
}

@safe unittest
{
    struct S1
    {
        long val;
        alias val this;
    }

    struct S2
    {
        long val;
        alias val this;
        string toString() const { return "S"; }
    }

    formatTest(S1(10), "10");
    formatTest(S2(10), "S");
}

// https://issues.dlang.org/show_bug.cgi?id=20064
@safe unittest
{
    assert(format( "%03,d",  1234) ==              "1,234");
    assert(format( "%04,d",  1234) ==              "1,234");
    assert(format( "%05,d",  1234) ==              "1,234");
    assert(format( "%06,d",  1234) ==             "01,234");
    assert(format( "%07,d",  1234) ==            "001,234");
    assert(format( "%08,d",  1234) ==          "0,001,234");
    assert(format( "%09,d",  1234) ==          "0,001,234");
    assert(format("%010,d",  1234) ==         "00,001,234");
    assert(format("%011,d",  1234) ==        "000,001,234");
    assert(format("%012,d",  1234) ==      "0,000,001,234");
    assert(format("%013,d",  1234) ==      "0,000,001,234");
    assert(format("%014,d",  1234) ==     "00,000,001,234");
    assert(format("%015,d",  1234) ==    "000,000,001,234");
    assert(format("%016,d",  1234) ==  "0,000,000,001,234");
    assert(format("%017,d",  1234) ==  "0,000,000,001,234");

    assert(format( "%03,d", -1234) ==             "-1,234");
    assert(format( "%04,d", -1234) ==             "-1,234");
    assert(format( "%05,d", -1234) ==             "-1,234");
    assert(format( "%06,d", -1234) ==             "-1,234");
    assert(format( "%07,d", -1234) ==            "-01,234");
    assert(format( "%08,d", -1234) ==           "-001,234");
    assert(format( "%09,d", -1234) ==         "-0,001,234");
    assert(format("%010,d", -1234) ==         "-0,001,234");
    assert(format("%011,d", -1234) ==        "-00,001,234");
    assert(format("%012,d", -1234) ==       "-000,001,234");
    assert(format("%013,d", -1234) ==     "-0,000,001,234");
    assert(format("%014,d", -1234) ==     "-0,000,001,234");
    assert(format("%015,d", -1234) ==    "-00,000,001,234");
    assert(format("%016,d", -1234) ==   "-000,000,001,234");
    assert(format("%017,d", -1234) == "-0,000,000,001,234");
}

@safe pure unittest
{
    string t1 = format("[%6s] [%-6s]", 123, 123);
    assert(t1 == "[   123] [123   ]");

    string t2 = format("[%6s] [%-6s]", -123, -123);
    assert(t2 == "[  -123] [-123  ]");
}

@safe pure unittest
{
    formatTest(byte.min, "-128");
    formatTest(short.min, "-32768");
    formatTest(int.min, "-2147483648");
    formatTest(long.min, "-9223372036854775808");
}

// https://issues.dlang.org/show_bug.cgi?id=21777
@safe pure unittest
{
    assert(format!"%20.5,d"(cast(short) 120) == "              00,120");
    assert(format!"%20.5,o"(cast(short) 120) == "              00,170");
    assert(format!"%20.5,x"(cast(short) 120) == "              00,078");
    assert(format!"%20.5,2d"(cast(short) 120) == "             0,01,20");
    assert(format!"%20.5,2o"(cast(short) 120) == "             0,01,70");
    assert(format!"%20.5,4d"(cast(short) 120) == "              0,0120");
    assert(format!"%20.5,4o"(cast(short) 120) == "              0,0170");
    assert(format!"%20.5,4x"(cast(short) 120) == "              0,0078");
    assert(format!"%20.5,2x"(3000) == "             0,0b,b8");
    assert(format!"%20.5,4d"(3000) == "              0,3000");
    assert(format!"%20.5,4o"(3000) == "              0,5670");
    assert(format!"%20.5,4x"(3000) == "              0,0bb8");
    assert(format!"%20.5,d"(-400) == "             -00,400");
    assert(format!"%20.30d"(-400) == "-000000000000000000000000000400");
    assert(format!"%20.5,4d"(0) == "              0,0000");
    assert(format!"%0#.8,2s"(12345) == "00,01,23,45");
    assert(format!"%0#.9,3x"(55) == "0x000,000,037");
}

// https://issues.dlang.org/show_bug.cgi?id=21814
@safe pure unittest
{
    assert(format("%,0d",1000) == "1000");
}

// https://issues.dlang.org/show_bug.cgi?id=21817
@safe pure unittest
{
    assert(format!"%u"(-5) == "4294967291");
}

// https://issues.dlang.org/show_bug.cgi?id=21820
@safe pure unittest
{
    assert(format!"%#.0o"(0) == "0");
}

@safe pure unittest
{
    assert(format!"%e"(10000) == "1.0000e+04");
    assert(format!"%.2e"(10000) == "1.00e+04");
    assert(format!"%.10e"(10000) == "1.0000000000e+04");

    assert(format!"%e"(9999) == "9.999e+03");
    assert(format!"%.2e"(9999) == "1.00e+04");
    assert(format!"%.10e"(9999) == "9.9990000000e+03");

    assert(format!"%f"(10000) == "10000");
    assert(format!"%.2f"(10000) == "10000.00");

    assert(format!"%g"(10000) == "10000");
    assert(format!"%.2g"(10000) == "1e+04");
    assert(format!"%.10g"(10000) == "10000");

    assert(format!"%#g"(10000) == "10000.");
    assert(format!"%#.2g"(10000) == "1.0e+04");
    assert(format!"%#.10g"(10000) == "10000.00000");

    assert(format!"%g"(9999) == "9999");
    assert(format!"%.2g"(9999) == "1e+04");
    assert(format!"%.10g"(9999) == "9999");

    assert(format!"%a"(0x10000) == "0x1.0000p+16");
    assert(format!"%.2a"(0x10000) == "0x1.00p+16");
    assert(format!"%.10a"(0x10000) == "0x1.0000000000p+16");

    assert(format!"%a"(0xffff) == "0xf.fffp+12");
    assert(format!"%.2a"(0xffff) == "0x1.00p+16");
    assert(format!"%.10a"(0xffff) == "0xf.fff0000000p+12");
}

@safe pure unittest
{
    assert(format!"%.3e"(ulong.max) == "1.845e+19");
    assert(format!"%.3f"(ulong.max) == "18446744073709551615.000");
    assert(format!"%.3g"(ulong.max) == "1.84e+19");
    assert(format!"%.3a"(ulong.max) == "0x1.000p+64");

    assert(format!"%.3e"(long.min) == "-9.223e+18");
    assert(format!"%.3f"(long.min) == "-9223372036854775808.000");
    assert(format!"%.3g"(long.min) == "-9.22e+18");
    assert(format!"%.3a"(long.min) == "-0x8.000p+60");

    assert(format!"%e"(0) == "0e+00");
    assert(format!"%f"(0) == "0");
    assert(format!"%g"(0) == "0");
    assert(format!"%a"(0) == "0x0p+00");
}

@safe pure unittest
{
    assert(format!"%.0g"(1500) == "2e+03");
}

// https://issues.dlang.org/show_bug.cgi?id=21900#
@safe pure unittest
{
    assert(format!"%.1a"(472) == "0x1.ep+08");
}

/*
    Floating-point values are formatted like $(REF printf, core, stdc, stdio)
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, const(T) obj,
                                      scope const ref FormatSpec!Char f)
if (is(FloatingPointTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    import std.format : enforceFmt;
    import std.range.primitives : put;
    import std.format.internal.floats : printFloat, isFloatSpec;

    FloatingPointTypeOf!T val = obj;
    const char spec = f.spec;

    if (spec == 'r')
    {
        // raw write, skip all else and write the thing
        auto raw = (ref val) @trusted {
            return (cast(const char*) &val)[0 .. val.sizeof];
        }(val);

        if (needToSwapEndianess(f))
        {
            foreach_reverse (c; raw)
                put(w, c);
        }
        else
        {
            foreach (c; raw)
                put(w, c);
        }
        return;
    }

    FormatSpec!Char fs = f; // fs is copy for change its values.
    fs.spec = spec == 's' ? 'g' : spec;
    enforceFmt(isFloatSpec(fs.spec), "incompatible format character for floating point argument: %" ~ spec);

    static if (is(T == float) || is(T == double)
               || (is(T == real) && (T.mant_dig == double.mant_dig || T.mant_dig == 64)))
    {
        alias tval = val;
    }
    else
    {
        import std.math.traits : isInfinity;
        import std.math.operations : nextUp;

        // reals that are not supported by printFloat are cast to double.
        double tval = val;

        // Numbers greater than double.max are converted to double.max:
        if (val > double.max && !isInfinity(val))
            tval = double.max;
        if (val < -double.max && !isInfinity(val))
            tval = -double.max;

        // Numbers between the smallest representable double subnormal and 0.0
        // are converted to the smallest representable double subnormal:
        enum doubleLowest = nextUp(0.0);
        if (val > 0 && val < doubleLowest)
            tval = doubleLowest;
        if (val < 0 && val > -doubleLowest)
            tval = -doubleLowest;
    }

    printFloat(w, tval, fs);
}

@safe unittest
{
    assert(format("%.1f", 1337.7) == "1337.7");
    assert(format("%,3.2f", 1331.982) == "1,331.98");
    assert(format("%,3.0f", 1303.1982) == "1,303");
    assert(format("%#,3.4f", 1303.1982) == "1,303.1982");
    assert(format("%#,3.0f", 1303.1982) == "1,303.");
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : collectExceptionMsg;
    import std.format : FormatException;
    import std.meta : AliasSeq;
    import std.range.primitives : back;

    assert(collectExceptionMsg!FormatException(format("%d", 5.1)).back == 'd');

    static foreach (T; AliasSeq!(float, double, real))
    {
        formatTest(to!(          T)(5.5), "5.5");
        formatTest(to!(    const T)(5.5), "5.5");
        formatTest(to!(immutable T)(5.5), "5.5");

        formatTest(T.nan, "nan");
    }
}

@safe unittest
{
    formatTest(2.25, "2.25");

    struct S1
    {
        double val;
        alias val this;
    }
    struct S2
    {
        double val;
        alias val this;
        string toString() const { return "S"; }
    }

    formatTest(S1(2.25), "2.25");
    formatTest(S2(2.25), "S");
}

// https://issues.dlang.org/show_bug.cgi?id=19939
@safe unittest
{
    assert(format("^%13,3.2f$",          1.00) == "^         1.00$");
    assert(format("^%13,3.2f$",         10.00) == "^        10.00$");
    assert(format("^%13,3.2f$",        100.00) == "^       100.00$");
    assert(format("^%13,3.2f$",      1_000.00) == "^     1,000.00$");
    assert(format("^%13,3.2f$",     10_000.00) == "^    10,000.00$");
    assert(format("^%13,3.2f$",    100_000.00) == "^   100,000.00$");
    assert(format("^%13,3.2f$",  1_000_000.00) == "^ 1,000,000.00$");
    assert(format("^%13,3.2f$", 10_000_000.00) == "^10,000,000.00$");
}

// https://issues.dlang.org/show_bug.cgi?id=20069
@safe unittest
{
    assert(format("%012,f",   -1234.0) ==    "-1,234.000000");
    assert(format("%013,f",   -1234.0) ==    "-1,234.000000");
    assert(format("%014,f",   -1234.0) ==   "-01,234.000000");
    assert(format("%011,f",    1234.0) ==     "1,234.000000");
    assert(format("%012,f",    1234.0) ==     "1,234.000000");
    assert(format("%013,f",    1234.0) ==    "01,234.000000");
    assert(format("%014,f",    1234.0) ==   "001,234.000000");
    assert(format("%015,f",    1234.0) == "0,001,234.000000");
    assert(format("%016,f",    1234.0) == "0,001,234.000000");

    assert(format( "%08,.2f", -1234.0) ==        "-1,234.00");
    assert(format( "%09,.2f", -1234.0) ==        "-1,234.00");
    assert(format("%010,.2f", -1234.0) ==       "-01,234.00");
    assert(format("%011,.2f", -1234.0) ==      "-001,234.00");
    assert(format("%012,.2f", -1234.0) ==    "-0,001,234.00");
    assert(format("%013,.2f", -1234.0) ==    "-0,001,234.00");
    assert(format("%014,.2f", -1234.0) ==   "-00,001,234.00");
    assert(format( "%08,.2f",  1234.0) ==         "1,234.00");
    assert(format( "%09,.2f",  1234.0) ==        "01,234.00");
    assert(format("%010,.2f",  1234.0) ==       "001,234.00");
    assert(format("%011,.2f",  1234.0) ==     "0,001,234.00");
    assert(format("%012,.2f",  1234.0) ==     "0,001,234.00");
    assert(format("%013,.2f",  1234.0) ==    "00,001,234.00");
    assert(format("%014,.2f",  1234.0) ==   "000,001,234.00");
    assert(format("%015,.2f",  1234.0) == "0,000,001,234.00");
    assert(format("%016,.2f",  1234.0) == "0,000,001,234.00");
}

@safe unittest
{
    import std.math.hardware; // cannot be selective, because FloatingPointControl might not be defined

    // std.math's FloatingPointControl isn't available on all target platforms
    static if (is(FloatingPointControl))
    {
        assert(FloatingPointControl.rounding == FloatingPointControl.roundToNearest);
    }

    // https://issues.dlang.org/show_bug.cgi?id=20320
    real a = 0.16;
    real b = 0.016;
    assert(format("%.1f", a) == "0.2");
    assert(format("%.2f", b) == "0.02");

    double a1 = 0.16;
    double b1 = 0.016;
    assert(format("%.1f", a1) == "0.2");
    assert(format("%.2f", b1) == "0.02");

    // https://issues.dlang.org/show_bug.cgi?id=9889
    assert(format("%.1f", 0.09) == "0.1");
    assert(format("%.1f", -0.09) == "-0.1");
    assert(format("%.1f", 0.095) == "0.1");
    assert(format("%.1f", -0.095) == "-0.1");
    assert(format("%.1f", 0.094) == "0.1");
    assert(format("%.1f", -0.094) == "-0.1");
}

@safe unittest
{
    double a = 123.456;
    double b = -123.456;
    double c = 123.0;

    assert(format("%10.4f",a)  == "  123.4560");
    assert(format("%-10.4f",a) == "123.4560  ");
    assert(format("%+10.4f",a) == " +123.4560");
    assert(format("% 10.4f",a) == "  123.4560");
    assert(format("%010.4f",a) == "00123.4560");
    assert(format("%#10.4f",a) == "  123.4560");

    assert(format("%10.4f",b)  == " -123.4560");
    assert(format("%-10.4f",b) == "-123.4560 ");
    assert(format("%+10.4f",b) == " -123.4560");
    assert(format("% 10.4f",b) == " -123.4560");
    assert(format("%010.4f",b) == "-0123.4560");
    assert(format("%#10.4f",b) == " -123.4560");

    assert(format("%10.0f",c)  == "       123");
    assert(format("%-10.0f",c) == "123       ");
    assert(format("%+10.0f",c) == "      +123");
    assert(format("% 10.0f",c) == "       123");
    assert(format("%010.0f",c) == "0000000123");
    assert(format("%#10.0f",c) == "      123.");

    assert(format("%+010.4f",a) == "+0123.4560");
    assert(format("% 010.4f",a) == " 0123.4560");
    assert(format("% +010.4f",a) == "+0123.4560");
}

@safe unittest
{
    string t1 = format("[%6s] [%-6s]", 12.3, 12.3);
    assert(t1 == "[  12.3] [12.3  ]");

    string t2 = format("[%6s] [%-6s]", -12.3, -12.3);
    assert(t2 == "[ -12.3] [-12.3 ]");
}

// https://issues.dlang.org/show_bug.cgi?id=20396
@safe unittest
{
    import std.math.operations : nextUp;

    assert(format!"%a"(nextUp(0.0f)) == "0x0.000002p-126");
    assert(format!"%a"(nextUp(0.0)) == "0x0.0000000000001p-1022");
}

// https://issues.dlang.org/show_bug.cgi?id=20371
@safe unittest
{
    assert(format!"%.1000a"(1.0).length == 1007);
    assert(format!"%.600f"(0.1).length == 602);
    assert(format!"%.600e"(0.1L).length == 606);
}

@safe unittest
{
    import std.math.hardware; // cannot be selective, because FloatingPointControl might not be defined

    // std.math's FloatingPointControl isn't available on all target platforms
    static if (is(FloatingPointControl))
    {
        FloatingPointControl fpctrl;

        fpctrl.rounding = FloatingPointControl.roundUp;
        assert(format!"%.0e"(3.5) == "4e+00");
        assert(format!"%.0e"(4.5) == "5e+00");
        assert(format!"%.0e"(-3.5) == "-3e+00");
        assert(format!"%.0e"(-4.5) == "-4e+00");

        fpctrl.rounding = FloatingPointControl.roundDown;
        assert(format!"%.0e"(3.5) == "3e+00");
        assert(format!"%.0e"(4.5) == "4e+00");
        assert(format!"%.0e"(-3.5) == "-4e+00");
        assert(format!"%.0e"(-4.5) == "-5e+00");

        fpctrl.rounding = FloatingPointControl.roundToZero;
        assert(format!"%.0e"(3.5) == "3e+00");
        assert(format!"%.0e"(4.5) == "4e+00");
        assert(format!"%.0e"(-3.5) == "-3e+00");
        assert(format!"%.0e"(-4.5) == "-4e+00");

        fpctrl.rounding = FloatingPointControl.roundToNearest;
        assert(format!"%.0e"(3.5) == "4e+00");
        assert(format!"%.0e"(4.5) == "4e+00");
        assert(format!"%.0e"(-3.5) == "-4e+00");
        assert(format!"%.0e"(-4.5) == "-4e+00");
    }
}

@safe pure unittest
{
    static assert(format("%e",1.0) == "1.000000e+00");
    static assert(format("%e",-1.234e156) == "-1.234000e+156");
    static assert(format("%a",1.0) == "0x1p+0");
    static assert(format("%a",-1.234e156) == "-0x1.7024c96ca3ce4p+518");
    static assert(format("%f",1.0) == "1.000000");
    static assert(format("%f",-1.234e156) ==
                  "-123399999999999990477495546305353609103201879173427886566531" ~
                  "0740685826234179310516880117527217443004051984432279880308552" ~
                  "009640198043032289366552939010719744.000000");
    static assert(format("%g",1.0) == "1");
    static assert(format("%g",-1.234e156) == "-1.234e+156");

    static assert(format("%e",1.0f) == "1.000000e+00");
    static assert(format("%e",-1.234e23f) == "-1.234000e+23");
    static assert(format("%a",1.0f) == "0x1p+0");
    static assert(format("%a",-1.234e23f) == "-0x1.a2187p+76");
    static assert(format("%f",1.0f) == "1.000000");
    static assert(format("%f",-1.234e23f) == "-123399998884238311030784.000000");
    static assert(format("%g",1.0f) == "1");
    static assert(format("%g",-1.234e23f) == "-1.234e+23");
}

// https://issues.dlang.org/show_bug.cgi?id=21641
@safe unittest
{
    float a = -999999.8125;
    assert(format("%#.5g",a) == "-1.0000e+06");
    assert(format("%#.6g",a) == "-1.00000e+06");
}

// https://issues.dlang.org/show_bug.cgi?id=8424
@safe pure unittest
{
    static assert(format("%s", 0.6f) == "0.6");
    static assert(format("%s", 0.6) == "0.6");
    static assert(format("%s", 0.6L) == "0.6");
}

// https://issues.dlang.org/show_bug.cgi?id=9297
@safe pure unittest
{
    static if (real.mant_dig == 64) // 80 bit reals
    {
        assert(format("%.25f", 1.6180339887_4989484820_4586834365L) == "1.6180339887498948482072100");
    }
}

// https://issues.dlang.org/show_bug.cgi?id=21853
@safe pure unittest
{
    import std.math.exponential : log2;

    // log2 is broken for x87-reals on some computers in CTFE
    // the following test excludes these computers from the test
    // (https://issues.dlang.org/show_bug.cgi?id=21757)
    enum test = cast(int) log2(3.05e2312L);
    static if (real.mant_dig == 64 && test == 7681) // 80 bit reals
    {
        static assert(format!"%e"(real.max) == "1.189731e+4932");
    }
}

// https://issues.dlang.org/show_bug.cgi?id=21842
@safe pure unittest
{
    assert(format!"%-+05,g"(1.0) == "+1   ");
}

// https://issues.dlang.org/show_bug.cgi?id=20536
@safe pure unittest
{
    real r = .00000095367431640625L;
    assert(format("%a", r) == "0x1p-20");
}

// https://issues.dlang.org/show_bug.cgi?id=21840
@safe pure unittest
{
    assert(format!"% 0,e"(0.0) == " 0.000000e+00");
}

// https://issues.dlang.org/show_bug.cgi?id=21841
@safe pure unittest
{
    assert(format!"%0.0,e"(0.0) == "0e+00");
}

// https://issues.dlang.org/show_bug.cgi?id=21836
@safe pure unittest
{
    assert(format!"%-5,1g"(0.0) == "0    ");
}

// https://issues.dlang.org/show_bug.cgi?id=21838
@safe pure unittest
{
    assert(format!"%#,a"(0.0) == "0x0.p+0");
}

/*
    Formatting a `creal` is deprecated but still kept around for a while.
 */
deprecated("Use of complex types is deprecated. Use std.complex")
void formatValueImpl(Writer, T, Char)(auto ref Writer w, const(T) obj, scope const ref FormatSpec!Char f)
if (is(immutable T : immutable creal) && !is(T == enum) && !hasToString!(T, Char))
{
    import std.range.primitives : put;

    immutable creal val = obj;

    formatValueImpl(w, val.re, f);
    if (val.im >= 0)
    {
        put(w, '+');
    }
    formatValueImpl(w, val.im, f);
    put(w, 'i');
}

/*
    Formatting an `ireal` is deprecated but still kept around for a while.
 */
deprecated("Use of imaginary types is deprecated. Use std.complex")
void formatValueImpl(Writer, T, Char)(auto ref Writer w, const(T) obj, scope const ref FormatSpec!Char f)
if (is(immutable T : immutable ireal) && !is(T == enum) && !hasToString!(T, Char))
{
    import std.range.primitives : put;

    immutable ireal val = obj;

    formatValueImpl(w, val.im, f);
    put(w, 'i');
}

/*
    Individual characters are formatted as Unicode characters with `%s`
    and as integers with integral-specific format specs
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, const(T) obj, scope const ref FormatSpec!Char f)
if (is(CharTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    import std.meta : AliasSeq;

    CharTypeOf!T[1] val = obj;

    if (f.spec == 's' || f.spec == 'c')
        writeAligned(w, val[], f);
    else
    {
        alias U = AliasSeq!(ubyte, ushort, uint)[CharTypeOf!T.sizeof/2];
        formatValueImpl(w, cast(U) val[0], f);
    }
}

@safe pure unittest
{
    assertCTFEable!(
    {
        formatTest('c', "c");
    });
}

@safe unittest
{
    struct S1
    {
        char val;
        alias val this;
    }

    struct S2
    {
        char val;
        alias val this;
        string toString() const { return "S"; }
    }

    formatTest(S1('c'), "c");
    formatTest(S2('c'), "S");
}

@safe unittest
{
    //Little Endian
    formatTest("%-r", cast( char)'c', ['c'         ]);
    formatTest("%-r", cast(wchar)'c', ['c', 0      ]);
    formatTest("%-r", cast(dchar)'c', ['c', 0, 0, 0]);
    formatTest("%-r", '本', ['\x2c', '\x67'] );

    //Big Endian
    formatTest("%+r", cast( char)'c', [         'c']);
    formatTest("%+r", cast(wchar)'c', [0,       'c']);
    formatTest("%+r", cast(dchar)'c', [0, 0, 0, 'c']);
    formatTest("%+r", '本', ['\x67', '\x2c']);
}


@safe pure unittest
{
    string t1 = format("[%6s] [%-6s]", 'A', 'A');
    assert(t1 == "[     A] [A     ]");
    string t2 = format("[%6s] [%-6s]", '本', '本');
    assert(t2 == "[     本] [本     ]");
}

/*
    Strings are formatted like $(REF printf, core, stdc, stdio)
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, scope const(T) obj,
    scope const ref FormatSpec!Char f)
if (is(StringTypeOf!T) && !is(StaticArrayTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    Unqual!(const(StringTypeOf!T)) val = obj;  // for `alias this`, see bug5371
    formatRange(w, val, f);
}

@safe unittest
{
    formatTest("abc", "abc");
}

@safe pure unittest
{
    import std.exception : collectExceptionMsg;
    import std.range.primitives : back;

    assert(collectExceptionMsg(format("%d", "hi")).back == 'd');
}

@safe unittest
{
    // Test for bug 5371 for structs
    struct S1
    {
        const string var;
        alias var this;
    }

    struct S2
    {
        string var;
        alias var this;
    }

    formatTest(S1("s1"), "s1");
    formatTest(S2("s2"), "s2");
}

@safe unittest
{
    struct S3
    {
        string val; alias val this;
        string toString() const { return "S"; }
    }

    formatTest(S3("s3"), "S");
}

@safe pure unittest
{
    //Little Endian
    formatTest("%-r", "ab"c, ['a'         , 'b'         ]);
    formatTest("%-r", "ab"w, ['a', 0      , 'b', 0      ]);
    formatTest("%-r", "ab"d, ['a', 0, 0, 0, 'b', 0, 0, 0]);
    formatTest("%-r", "日本語"c, ['\xe6', '\x97', '\xa5', '\xe6', '\x9c', '\xac',
                                  '\xe8', '\xaa', '\x9e']);
    formatTest("%-r", "日本語"w, ['\xe5', '\x65', '\x2c', '\x67', '\x9e', '\x8a']);
    formatTest("%-r", "日本語"d, ['\xe5', '\x65', '\x00', '\x00', '\x2c', '\x67',
                                  '\x00', '\x00', '\x9e', '\x8a', '\x00', '\x00']);

    //Big Endian
    formatTest("%+r", "ab"c, [         'a',          'b']);
    formatTest("%+r", "ab"w, [      0, 'a',       0, 'b']);
    formatTest("%+r", "ab"d, [0, 0, 0, 'a', 0, 0, 0, 'b']);
    formatTest("%+r", "日本語"c, ['\xe6', '\x97', '\xa5', '\xe6', '\x9c', '\xac',
                                  '\xe8', '\xaa', '\x9e']);
    formatTest("%+r", "日本語"w, ['\x65', '\xe5', '\x67', '\x2c', '\x8a', '\x9e']);
    formatTest("%+r", "日本語"d, ['\x00', '\x00', '\x65', '\xe5', '\x00', '\x00',
                                  '\x67', '\x2c', '\x00', '\x00', '\x8a', '\x9e']);
}

@safe pure unittest
{
    string t1 = format("[%6s] [%-6s]", "AB", "AB");
    assert(t1 == "[    AB] [AB    ]");
    string t2 = format("[%6s] [%-6s]", "本Ä", "本Ä");
    assert(t2 == "[    本Ä] [本Ä    ]");
}

// https://issues.dlang.org/show_bug.cgi?id=6640
@safe unittest
{
    import std.range.primitives : front, popFront;

    struct Range
    {
        @safe:

        string value;
        @property bool empty() const { return !value.length; }
        @property dchar front() const { return value.front; }
        void popFront() { value.popFront(); }

        @property size_t length() const { return value.length; }
    }
    immutable table =
    [
        ["[%s]", "[string]"],
        ["[%10s]", "[    string]"],
        ["[%-10s]", "[string    ]"],
        ["[%(%02x %)]", "[73 74 72 69 6e 67]"],
        ["[%(%c %)]", "[s t r i n g]"],
    ];
    foreach (e; table)
    {
        formatTest(e[0], "string", e[1]);
        formatTest(e[0], Range("string"), e[1]);
    }
}

@safe unittest
{
    import std.meta : AliasSeq;

    // string literal from valid UTF sequence is encoding free.
    static foreach (StrType; AliasSeq!(string, wstring, dstring))
    {
        // Valid and printable (ASCII)
        formatTest([cast(StrType)"hello"],
                   `["hello"]`);

        // 1 character escape sequences (' is not escaped in strings)
        formatTest([cast(StrType)"\"'\0\\\a\b\f\n\r\t\v"],
                   `["\"'\0\\\a\b\f\n\r\t\v"]`);

        // 1 character optional escape sequences
        formatTest([cast(StrType)"\'\?"],
                   `["'?"]`);

        // Valid and non-printable code point (<= U+FF)
        formatTest([cast(StrType)"\x10\x1F\x20test"],
                   `["\x10\x1F test"]`);

        // Valid and non-printable code point (<= U+FFFF)
        formatTest([cast(StrType)"\u200B..\u200F"],
                   `["\u200B..\u200F"]`);

        // Valid and non-printable code point (<= U+10FFFF)
        formatTest([cast(StrType)"\U000E0020..\U000E007F"],
                   `["\U000E0020..\U000E007F"]`);
    }

    // invalid UTF sequence needs hex-string literal postfix (c/w/d)
    () @trusted
    {
        // U+FFFF with UTF-8 (Invalid code point for interchange)
        formatTest([cast(string)[0xEF, 0xBF, 0xBF]],
                   `[[cast(char) 0xEF, cast(char) 0xBF, cast(char) 0xBF]]`);

        // U+FFFF with UTF-16 (Invalid code point for interchange)
        formatTest([cast(wstring)[0xFFFF]],
                   `[[cast(wchar) 0xFFFF]]`);

        // U+FFFF with UTF-32 (Invalid code point for interchange)
        formatTest([cast(dstring)[0xFFFF]],
                   `[[cast(dchar) 0xFFFF]]`);
    } ();
}

/*
    Static-size arrays are formatted as dynamic arrays.
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, auto ref T obj,
    scope const ref FormatSpec!Char f)
if (is(StaticArrayTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    formatValueImpl(w, obj[], f);
}

// Test for https://issues.dlang.org/show_bug.cgi?id=8310
@safe unittest
{
    import std.array : appender;
    import std.format : formatValue;

    FormatSpec!char f;
    auto w = appender!string();

    char[2] two = ['a', 'b'];
    formatValue(w, two, f);

    char[2] getTwo() { return two; }
    formatValue(w, getTwo(), f);
}

// https://issues.dlang.org/show_bug.cgi?id=18205
@safe pure unittest
{
    assert("|%8s|".format("abc")       == "|     abc|");
    assert("|%8s|".format("αβγ")       == "|     αβγ|");
    assert("|%8s|".format("   ")       == "|        |");
    assert("|%8s|".format("été"d)      == "|     été|");
    assert("|%8s|".format("été 2018"w) == "|été 2018|");

    assert("%2s".format("e\u0301"w) == " e\u0301");
    assert("%2s".format("a\u0310\u0337"d) == " a\u0310\u0337");
}

/*
    Dynamic arrays are formatted as input ranges.
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, T obj, scope const ref FormatSpec!Char f)
if (is(DynamicArrayTypeOf!T) && !is(StringTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    static if (is(immutable(ArrayTypeOf!T) == immutable(void[])))
    {
        formatValueImpl(w, cast(const ubyte[]) obj, f);
    }
    else static if (!isInputRange!T)
    {
        alias U = Unqual!(ArrayTypeOf!T);
        static assert(isInputRange!U, U.stringof ~ " must be an InputRange");
        U val = obj;
        formatValueImpl(w, val, f);
    }
    else
    {
        formatRange(w, obj, f);
    }
}

// https://issues.dlang.org/show_bug.cgi?id=20848
@safe unittest
{
    class C
    {
        immutable(void)[] data;
    }

    import std.typecons : Nullable;
    Nullable!C c;
}

// alias this, input range I/F, and toString()
@safe unittest
{
    struct S(int flags)
    {
        int[] arr;
        static if (flags & 1)
            alias arr this;

        static if (flags & 2)
        {
            @property bool empty() const { return arr.length == 0; }
            @property int front() const { return arr[0] * 2; }
            void popFront() { arr = arr[1 .. $]; }
        }

        static if (flags & 4)
            string toString() const { return "S"; }
    }

    formatTest(S!0b000([0, 1, 2]), "S!0([0, 1, 2])");
    formatTest(S!0b001([0, 1, 2]), "[0, 1, 2]");        // Test for bug 7628
    formatTest(S!0b010([0, 1, 2]), "[0, 2, 4]");
    formatTest(S!0b011([0, 1, 2]), "[0, 2, 4]");
    formatTest(S!0b100([0, 1, 2]), "S");
    formatTest(S!0b101([0, 1, 2]), "S");                // Test for bug 7628
    formatTest(S!0b110([0, 1, 2]), "S");
    formatTest(S!0b111([0, 1, 2]), "S");
}

@safe unittest
{
    // void[]
    void[] val0;
    formatTest(val0, "[]");

    void[] val = cast(void[]) cast(ubyte[])[1, 2, 3];
    formatTest(val, "[1, 2, 3]");

    void[0] sval0 = [];
    formatTest(sval0, "[]");

    void[3] sval = () @trusted { return cast(void[3]) cast(ubyte[3])[1, 2, 3]; } ();
    formatTest(sval, "[1, 2, 3]");
}

@safe unittest
{
    // const(T[]) -> const(T)[]
    const short[] a = [1, 2, 3];
    formatTest(a, "[1, 2, 3]");

    struct S
    {
        const(int[]) arr;
        alias arr this;
    }

    auto s = S([1,2,3]);
    formatTest(s, "[1, 2, 3]");
}

@safe unittest
{
    // nested range formatting with array of string
    formatTest("%({%(%02x %)}%| %)", ["test", "msg"],
               `{74 65 73 74} {6d 73 67}`);
}

@safe unittest
{
    // stop auto escaping inside range formatting
    auto arr = ["hello", "world"];
    formatTest("%(%s, %)",  arr, `"hello", "world"`);
    formatTest("%-(%s, %)", arr, `hello, world`);

    auto aa1 = [1:"hello", 2:"world"];
    formatTest("%(%s:%s, %)",  aa1, [`1:"hello", 2:"world"`, `2:"world", 1:"hello"`]);
    formatTest("%-(%s:%s, %)", aa1, [`1:hello, 2:world`, `2:world, 1:hello`]);

    auto aa2 = [1:["ab", "cd"], 2:["ef", "gh"]];
    formatTest("%-(%s:%s, %)",        aa2, [`1:["ab", "cd"], 2:["ef", "gh"]`, `2:["ef", "gh"], 1:["ab", "cd"]`]);
    formatTest("%-(%s:%(%s%), %)",    aa2, [`1:"ab""cd", 2:"ef""gh"`, `2:"ef""gh", 1:"ab""cd"`]);
    formatTest("%-(%s:%-(%s%)%|, %)", aa2, [`1:abcd, 2:efgh`, `2:efgh, 1:abcd`]);
}

// https://issues.dlang.org/show_bug.cgi?id=18778
@safe pure unittest
{
    assert(format("%-(%1$s - %1$s, %)", ["A", "B", "C"]) == "A - A, B - B, C - C");
}

@safe pure unittest
{
    int[] a = [ 1, 3, 2 ];
    formatTest("testing %(%s & %) embedded", a,
               "testing 1 & 3 & 2 embedded");
    formatTest("testing %((%s) %)) wyda3", a,
               "testing (1) (3) (2) wyda3");

    int[0] empt = [];
    formatTest("(%s)", empt, "([])");
}

// input range formatting
private void formatRange(Writer, T, Char)(ref Writer w, ref T val, scope const ref FormatSpec!Char f)
if (isInputRange!T)
{
    import std.conv : text;
    import std.format : FormatException, formatValue, NoOpSink;
    import std.range.primitives : ElementType, empty, front, hasLength,
        walkLength, isForwardRange, isInfinite, popFront, put;

    // in this mode, we just want to do a representative print to discover
    // if the format spec is valid
    enum formatTestMode = is(Writer == NoOpSink);

    // Formatting character ranges like string
    if (f.spec == 's')
    {
        alias E = ElementType!T;

        static if (!is(E == enum) && is(CharTypeOf!E))
        {
            static if (is(StringTypeOf!T))
                writeAligned(w, val[0 .. f.precision < $ ? f.precision : $], f);
            else
            {
                if (!f.flDash)
                {
                    static if (hasLength!T)
                    {
                        // right align
                        auto len = val.length;
                    }
                    else static if (isForwardRange!T && !isInfinite!T)
                    {
                        auto len = walkLength(val.save);
                    }
                    else
                    {
                        import std.format : enforceFmt;
                        enforceFmt(f.width == 0, "Cannot right-align a range without length");
                        size_t len = 0;
                    }
                    if (f.precision != f.UNSPECIFIED && len > f.precision)
                        len = f.precision;

                    if (f.width > len)
                        foreach (i ; 0 .. f.width - len)
                            put(w, ' ');
                    if (f.precision == f.UNSPECIFIED)
                        put(w, val);
                    else
                    {
                        size_t printed = 0;
                        for (; !val.empty && printed < f.precision; val.popFront(), ++printed)
                            put(w, val.front);
                    }
                }
                else
                {
                    size_t printed = void;

                    // left align
                    if (f.precision == f.UNSPECIFIED)
                    {
                        static if (hasLength!T)
                        {
                            printed = val.length;
                            put(w, val);
                        }
                        else
                        {
                            printed = 0;
                            for (; !val.empty; val.popFront(), ++printed)
                            {
                                put(w, val.front);
                                static if (formatTestMode) break; // one is enough to test
                            }
                        }
                    }
                    else
                    {
                        printed = 0;
                        for (; !val.empty && printed < f.precision; val.popFront(), ++printed)
                            put(w, val.front);
                    }

                    if (f.width > printed)
                        foreach (i ; 0 .. f.width - printed)
                            put(w, ' ');
                }
            }
        }
        else
        {
            put(w, f.seqBefore);
            if (!val.empty)
            {
                formatElement(w, val.front, f);
                val.popFront();
                for (size_t i; !val.empty; val.popFront(), ++i)
                {
                    put(w, f.seqSeparator);
                    formatElement(w, val.front, f);
                    static if (formatTestMode) break; // one is enough to test
                }
            }
            static if (!isInfinite!T) put(w, f.seqAfter);
        }
    }
    else if (f.spec == 'r')
    {
        static if (is(DynamicArrayTypeOf!T))
        {
            alias ARR = DynamicArrayTypeOf!T;
            scope a = cast(ARR) val;
            foreach (e ; a)
            {
                formatValue(w, e, f);
                static if (formatTestMode) break; // one is enough to test
            }
        }
        else
        {
            for (size_t i; !val.empty; val.popFront(), ++i)
            {
                formatValue(w, val.front, f);
                static if (formatTestMode) break; // one is enough to test
            }
        }
    }
    else if (f.spec == '(')
    {
        if (val.empty)
            return;
        // Nested specifier is to be used
        for (;;)
        {
            auto fmt = FormatSpec!Char(f.nested);
            w: while (true)
            {
                immutable r = fmt.writeUpToNextSpec(w);
                // There was no format specifier, so break
                if (!r)
                    break;
                if (f.flDash)
                    formatValue(w, val.front, fmt);
                else
                    formatElement(w, val.front, fmt);
                // Check if there will be a format specifier farther on in the
                // string. If so, continue the loop, otherwise break. This
                // prevents extra copies of the `sep` from showing up.
                foreach (size_t i; 0 .. fmt.trailing.length)
                    if (fmt.trailing[i] == '%')
                        continue w;
                break w;
            }
            static if (formatTestMode)
            {
                break; // one is enough to test
            }
            else
            {
                if (f.sep !is null)
                {
                    put(w, fmt.trailing);
                    val.popFront();
                    if (val.empty)
                        break;
                    put(w, f.sep);
                }
                else
                {
                    val.popFront();
                    if (val.empty)
                        break;
                    put(w, fmt.trailing);
                }
            }
        }
    }
    else
        throw new FormatException(text("Incorrect format specifier for range: %", f.spec));
}

// https://issues.dlang.org/show_bug.cgi?id=20218
@safe pure unittest
{
    void notCalled()
    {
        import std.range : repeat;

        auto value = 1.repeat;

        // test that range is not evaluated to completion at compiletime
        format!"%s"(value);
    }
}

// character formatting with ecaping
void formatChar(Writer)(ref Writer w, in dchar c, in char quote)
{
    import std.format : formattedWrite;
    import std.range.primitives : put;
    import std.uni : isGraphical;

    string fmt;
    if (isGraphical(c))
    {
        if (c == quote || c == '\\')
            put(w, '\\');
        put(w, c);
        return;
    }
    else if (c <= 0xFF)
    {
        if (c < 0x20)
        {
            foreach (i, k; "\n\r\t\a\b\f\v\0")
            {
                if (c == k)
                {
                    put(w, '\\');
                    put(w, "nrtabfv0"[i]);
                    return;
                }
            }
        }
        fmt = "\\x%02X";
    }
    else if (c <= 0xFFFF)
        fmt = "\\u%04X";
    else
        fmt = "\\U%08X";

    formattedWrite(w, fmt, cast(uint) c);
}

/*
    Associative arrays are formatted by using `':'` and $(D ", ") as
    separators, and enclosed by `'['` and `']'`.
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, T obj, scope const ref FormatSpec!Char f)
if (is(AssocArrayTypeOf!T) && !is(T == enum) && !hasToString!(T, Char))
{
    import std.format : enforceFmt, formatValue;
    import std.range.primitives : put;

    AssocArrayTypeOf!T val = obj;
    const spec = f.spec;

    enforceFmt(spec == 's' || spec == '(',
        "incompatible format character for associative array argument: %" ~ spec);

    enum const(Char)[] defSpec = "%s" ~ f.keySeparator ~ "%s" ~ f.seqSeparator;
    auto fmtSpec = spec == '(' ? f.nested : defSpec;

    auto key_first = true;

    // testing correct nested format spec
    import std.format : NoOpSink;
    auto noop = NoOpSink();
    auto test = FormatSpec!Char(fmtSpec);
    enforceFmt(test.writeUpToNextSpec(noop),
        "nested format string for associative array contains no format specifier");
    enforceFmt(test.indexStart <= 2,
        "positional parameter in nested format string for associative array may only be 1 or 2");
    if (test.indexStart == 2)
        key_first = false;

    enforceFmt(test.writeUpToNextSpec(noop),
        "nested format string for associative array contains only one format specifier");
    enforceFmt(test.indexStart <= 2,
        "positional parameter in nested format string for associative array may only be 1 or 2");
    enforceFmt(test.indexStart == 0 || ((test.indexStart == 2) == key_first),
        "wrong combination of positional parameters in nested format string");

    enforceFmt(!test.writeUpToNextSpec(noop),
        "nested format string for associative array contains more than two format specifiers");

    size_t i = 0;
    immutable end = val.length;

    if (spec == 's')
        put(w, f.seqBefore);
    foreach (k, ref v; val)
    {
        auto fmt = FormatSpec!Char(fmtSpec);

        foreach (pos; 1 .. 3)
        {
            fmt.writeUpToNextSpec(w);

            if (key_first == (pos == 1))
            {
                if (f.flDash)
                    formatValue(w, k, fmt);
                else
                    formatElement(w, k, fmt);
            }
            else
            {
                if (f.flDash)
                    formatValue(w, v, fmt);
                else
                    formatElement(w, v, fmt);
            }
        }

        if (f.sep !is null)
        {
            fmt.writeUpToNextSpec(w);
            if (++i != end)
                put(w, f.sep);
        }
        else
        {
            if (++i != end)
                fmt.writeUpToNextSpec(w);
        }
    }
    if (spec == 's')
        put(w, f.seqAfter);
}

@safe unittest
{
    import std.exception : collectExceptionMsg;
    import std.format : FormatException;
    import std.range.primitives : back;

    assert(collectExceptionMsg!FormatException(format("%d", [0:1])).back == 'd');

    int[string] aa0;
    formatTest(aa0, `[]`);

    // elements escaping
    formatTest(["aaa":1, "bbb":2],
               [`["aaa":1, "bbb":2]`, `["bbb":2, "aaa":1]`]);
    formatTest(['c':"str"],
               `['c':"str"]`);
    formatTest(['"':"\"", '\'':"'"],
               [`['"':"\"", '\'':"'"]`, `['\'':"'", '"':"\""]`]);

    // range formatting for AA
    auto aa3 = [1:"hello", 2:"world"];
    // escape
    formatTest("{%(%s:%s $ %)}", aa3,
               [`{1:"hello" $ 2:"world"}`, `{2:"world" $ 1:"hello"}`]);
    // use range formatting for key and value, and use %|
    formatTest("{%([%04d->%(%c.%)]%| $ %)}", aa3,
               [`{[0001->h.e.l.l.o] $ [0002->w.o.r.l.d]}`,
                `{[0002->w.o.r.l.d] $ [0001->h.e.l.l.o]}`]);

    // https://issues.dlang.org/show_bug.cgi?id=12135
    formatTest("%(%s:<%s>%|,%)", [1:2], "1:<2>");
    formatTest("%(%s:<%s>%|%)" , [1:2], "1:<2>");
}

@safe unittest
{
    struct S1
    {
        int[char] val;
        alias val this;
    }

    struct S2
    {
        int[char] val;
        alias val this;
        string toString() const { return "S"; }
    }

    formatTest(S1(['c':1, 'd':2]), [`['c':1, 'd':2]`, `['d':2, 'c':1]`]);
    formatTest(S2(['c':1, 'd':2]), "S");
}

// https://issues.dlang.org/show_bug.cgi?id=21875
@safe unittest
{
    import std.exception : assertThrown;
    import std.format : FormatException;

    auto aa = [ 1 : "x", 2 : "y", 3 : "z" ];

    assertThrown!FormatException(format("%(%)", aa));
    assertThrown!FormatException(format("%(%s%)", aa));
    assertThrown!FormatException(format("%(%s%s%s%)", aa));
}

@safe unittest
{
    import std.exception : assertThrown;
    import std.format : FormatException;

    auto aa = [ 1 : "x", 2 : "y", 3 : "z" ];

    assertThrown!FormatException(format("%(%3$s%s%)", aa));
    assertThrown!FormatException(format("%(%s%3$s%)", aa));
    assertThrown!FormatException(format("%(%1$s%1$s%)", aa));
    assertThrown!FormatException(format("%(%2$s%2$s%)", aa));
    assertThrown!FormatException(format("%(%s%1$s%)", aa));
}

// https://issues.dlang.org/show_bug.cgi?id=21808
@safe unittest
{
    auto spelled = [ 1 : "one" ];
    assert(format("%-(%2$s (%1$s)%|, %)", spelled) == "one (1)");

    spelled[2] = "two";
    auto result = format("%-(%2$s (%1$s)%|, %)", spelled);
    assert(result == "one (1), two (2)" || result == "two (2), one (1)");
}

enum HasToStringResult
{
    none,
    hasSomeToString,
    inCharSink,
    inCharSinkFormatString,
    inCharSinkFormatSpec,
    constCharSink,
    constCharSinkFormatString,
    constCharSinkFormatSpec,
    customPutWriter,
    customPutWriterFormatSpec,
}

private alias DScannerBug895 = int[256];
private immutable bool hasPreviewIn = ((in DScannerBug895 a) { return __traits(isRef, a); })(DScannerBug895.init);

template hasToString(T, Char)
{
    static if (isPointer!T)
    {
        // X* does not have toString, even if X is aggregate type has toString.
        enum hasToString = HasToStringResult.none;
    }
    else static if (is(typeof(
        (T val) {
            const FormatSpec!Char f;
            static struct S
            {
                @disable this(this);
                void put(scope Char s){}
            }
            S s;
            val.toString(s, f);
        })))
    {
        enum hasToString = HasToStringResult.customPutWriterFormatSpec;
    }
    else static if (is(typeof(
        (T val) {
            static struct S
            {
                @disable this(this);
                void put(scope Char s){}
            }
            S s;
            val.toString(s);
        })))
    {
        enum hasToString = HasToStringResult.customPutWriter;
    }
    else static if (is(typeof((T val) { FormatSpec!Char f; val.toString((scope const(char)[] s){}, f); })))
    {
        enum hasToString = HasToStringResult.constCharSinkFormatSpec;
    }
    else static if (is(typeof((T val) { val.toString((scope const(char)[] s){}, "%s"); })))
    {
        enum hasToString = HasToStringResult.constCharSinkFormatString;
    }
    else static if (is(typeof((T val) { val.toString((scope const(char)[] s){}); })))
    {
        enum hasToString = HasToStringResult.constCharSink;
    }

    else static if (hasPreviewIn &&
                    is(typeof((T val) { FormatSpec!Char f; val.toString((in char[] s){}, f); })))
    {
        enum hasToString = HasToStringResult.inCharSinkFormatSpec;
    }
    else static if (hasPreviewIn &&
                    is(typeof((T val) { val.toString((in char[] s){}, "%s"); })))
    {
        enum hasToString = HasToStringResult.inCharSinkFormatString;
    }
    else static if (hasPreviewIn &&
                    is(typeof((T val) { val.toString((in char[] s){}); })))
    {
        enum hasToString = HasToStringResult.inCharSink;
    }

    else static if (is(ReturnType!((T val) { return val.toString(); }) S) && isSomeString!S)
    {
        enum hasToString = HasToStringResult.hasSomeToString;
    }
    else
    {
        enum hasToString = HasToStringResult.none;
    }
}

@safe unittest
{
    import std.range.primitives : isOutputRange;

    static struct A
    {
        void toString(Writer)(ref Writer w)
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct B
    {
        void toString(scope void delegate(scope const(char)[]) sink, scope FormatSpec!char fmt) {}
    }
    static struct C
    {
        void toString(scope void delegate(scope const(char)[]) sink, string fmt) {}
    }
    static struct D
    {
        void toString(scope void delegate(scope const(char)[]) sink) {}
    }
    static struct E
    {
        string toString() {return "";}
    }
    static struct F
    {
        void toString(Writer)(ref Writer w, scope const ref FormatSpec!char fmt)
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct G
    {
        string toString() {return "";}
        void toString(Writer)(ref Writer w)
        if (isOutputRange!(Writer, string)) {}
    }
    static struct H
    {
        string toString() {return "";}
        void toString(Writer)(ref Writer w, scope const ref FormatSpec!char fmt)
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct I
    {
        void toString(Writer)(ref Writer w)
        if (isOutputRange!(Writer, string)) {}
        void toString(Writer)(ref Writer w, scope const ref FormatSpec!char fmt)
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct J
    {
        string toString() {return "";}
        void toString(Writer)(ref Writer w, scope ref FormatSpec!char fmt)
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct K
    {
        void toString(Writer)(Writer w, scope const ref FormatSpec!char fmt)
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct L
    {
        void toString(Writer)(ref Writer w, scope const FormatSpec!char fmt)
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct M
    {
        void toString(scope void delegate(in char[]) sink, in FormatSpec!char fmt) {}
    }
    static struct N
    {
        void toString(scope void delegate(in char[]) sink, string fmt) {}
    }
    static struct O
    {
        void toString(scope void delegate(in char[]) sink) {}
    }

    with(HasToStringResult)
    {
        static assert(hasToString!(A, char) == customPutWriter);
        static assert(hasToString!(B, char) == constCharSinkFormatSpec);
        static assert(hasToString!(C, char) == constCharSinkFormatString);
        static assert(hasToString!(D, char) == constCharSink);
        static assert(hasToString!(E, char) == hasSomeToString);
        static assert(hasToString!(F, char) == customPutWriterFormatSpec);
        static assert(hasToString!(G, char) == customPutWriter);
        static assert(hasToString!(H, char) == customPutWriterFormatSpec);
        static assert(hasToString!(I, char) == customPutWriterFormatSpec);
        static assert(hasToString!(J, char) == hasSomeToString
            || hasToString!(J, char) == constCharSinkFormatSpec); // depends on -preview=rvaluerefparam
        static assert(hasToString!(K, char) == constCharSinkFormatSpec);
        static assert(hasToString!(L, char) == customPutWriterFormatSpec);
        static if (hasPreviewIn)
        {
            static assert(hasToString!(M, char) == inCharSinkFormatSpec);
            static assert(hasToString!(N, char) == inCharSinkFormatString);
            static assert(hasToString!(O, char) == inCharSink);
        }
    }
}

// const toString methods
@safe unittest
{
    import std.range.primitives : isOutputRange;

    static struct A
    {
        void toString(Writer)(ref Writer w) const
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct B
    {
        void toString(scope void delegate(scope const(char)[]) sink, scope FormatSpec!char fmt) const {}
    }
    static struct C
    {
        void toString(scope void delegate(scope const(char)[]) sink, string fmt) const {}
    }
    static struct D
    {
        void toString(scope void delegate(scope const(char)[]) sink) const {}
    }
    static struct E
    {
        string toString() const {return "";}
    }
    static struct F
    {
        void toString(Writer)(ref Writer w, scope const ref FormatSpec!char fmt) const
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct G
    {
        string toString() const {return "";}
        void toString(Writer)(ref Writer w) const
        if (isOutputRange!(Writer, string)) {}
    }
    static struct H
    {
        string toString() const {return "";}
        void toString(Writer)(ref Writer w, scope const ref FormatSpec!char fmt) const
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct I
    {
        void toString(Writer)(ref Writer w) const
        if (isOutputRange!(Writer, string)) {}
        void toString(Writer)(ref Writer w, scope const ref FormatSpec!char fmt) const
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct J
    {
        string toString() const {return "";}
        void toString(Writer)(ref Writer w, scope ref FormatSpec!char fmt) const
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct K
    {
        void toString(Writer)(Writer w, scope const ref FormatSpec!char fmt) const
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct L
    {
        void toString(Writer)(ref Writer w, scope const FormatSpec!char fmt) const
        if (isOutputRange!(Writer, string))
        {}
    }
    static struct M
    {
        void toString(scope void delegate(in char[]) sink, in FormatSpec!char fmt) const {}
    }
    static struct N
    {
        void toString(scope void delegate(in char[]) sink, string fmt) const {}
    }
    static struct O
    {
        void toString(scope void delegate(in char[]) sink) const {}
    }

    with(HasToStringResult)
    {
        static assert(hasToString!(A, char) == customPutWriter);
        static assert(hasToString!(B, char) == constCharSinkFormatSpec);
        static assert(hasToString!(C, char) == constCharSinkFormatString);
        static assert(hasToString!(D, char) == constCharSink);
        static assert(hasToString!(E, char) == hasSomeToString);
        static assert(hasToString!(F, char) == customPutWriterFormatSpec);
        static assert(hasToString!(G, char) == customPutWriter);
        static assert(hasToString!(H, char) == customPutWriterFormatSpec);
        static assert(hasToString!(I, char) == customPutWriterFormatSpec);
        static assert(hasToString!(J, char) == hasSomeToString
            || hasToString!(J, char) == constCharSinkFormatSpec); // depends on -preview=rvaluerefparam
        static assert(hasToString!(K, char) == constCharSinkFormatSpec);
        static assert(hasToString!(L, char) == HasToStringResult.customPutWriterFormatSpec);
        static if (hasPreviewIn)
        {
            static assert(hasToString!(M, char) == inCharSinkFormatSpec);
            static assert(hasToString!(N, char) == inCharSinkFormatString);
            static assert(hasToString!(O, char) == inCharSink);
        }

        // https://issues.dlang.org/show_bug.cgi?id=22873
        static assert(hasToString!(inout(A), char) == customPutWriter);
        static assert(hasToString!(inout(B), char) == constCharSinkFormatSpec);
        static assert(hasToString!(inout(C), char) == constCharSinkFormatString);
        static assert(hasToString!(inout(D), char) == constCharSink);
        static assert(hasToString!(inout(E), char) == hasSomeToString);
        static assert(hasToString!(inout(F), char) == customPutWriterFormatSpec);
        static assert(hasToString!(inout(G), char) == customPutWriter);
        static assert(hasToString!(inout(H), char) == customPutWriterFormatSpec);
        static assert(hasToString!(inout(I), char) == customPutWriterFormatSpec);
        static assert(hasToString!(inout(J), char) == hasSomeToString
            || hasToString!(inout(J), char) == constCharSinkFormatSpec); // depends on -preview=rvaluerefparam
        static assert(hasToString!(inout(K), char) == constCharSinkFormatSpec);
        static assert(hasToString!(inout(L), char) == customPutWriterFormatSpec);
        static if (hasPreviewIn)
        {
            static assert(hasToString!(inout(M), char) == inCharSinkFormatSpec);
            static assert(hasToString!(inout(N), char) == inCharSinkFormatString);
            static assert(hasToString!(inout(O), char) == inCharSink);
        }
    }
}

// object formatting with toString
private void formatObject(Writer, T, Char)(ref Writer w, ref T val, scope const ref FormatSpec!Char f)
if (hasToString!(T, Char))
{
    import std.format : NoOpSink;
    import std.range.primitives : put;

    enum overload = hasToString!(T, Char);

    enum noop = is(Writer == NoOpSink);

    static if (overload == HasToStringResult.customPutWriterFormatSpec)
    {
        static if (!noop) val.toString(w, f);
    }
    else static if (overload == HasToStringResult.customPutWriter)
    {
        static if (!noop) val.toString(w);
    }
    else static if (overload == HasToStringResult.constCharSinkFormatSpec)
    {
        static if (!noop) val.toString((scope const(char)[] s) { put(w, s); }, f);
    }
    else static if (overload == HasToStringResult.constCharSinkFormatString)
    {
        static if (!noop) val.toString((scope const(char)[] s) { put(w, s); }, f.getCurFmtStr());
    }
    else static if (overload == HasToStringResult.constCharSink)
    {
        static if (!noop) val.toString((scope const(char)[] s) { put(w, s); });
    }
    else static if (overload == HasToStringResult.inCharSinkFormatSpec)
    {
        static if (!noop) val.toString((in char[] s) { put(w, s); }, f);
    }
    else static if (overload == HasToStringResult.inCharSinkFormatString)
    {
        static if (!noop) val.toString((in char[] s) { put(w, s); }, f.getCurFmtStr());
    }
    else static if (overload == HasToStringResult.inCharSink)
    {
        static if (!noop) val.toString((in char[] s) { put(w, s); });
    }
    else static if (overload == HasToStringResult.hasSomeToString)
    {
        static if (!noop) put(w, val.toString());
    }
    else
    {
        static assert(0, "No way found to format " ~ T.stringof ~ " as string");
    }
}

@system unittest
{
    import std.exception : assertThrown;
    import std.format : FormatException;

    static interface IF1 { }
    class CIF1 : IF1 { }
    static struct SF1 { }
    static union UF1 { }
    static class CF1 { }

    static interface IF2 { string toString(); }
    static class CIF2 : IF2 { override string toString() { return ""; } }
    static struct SF2 { string toString() { return ""; } }
    static union UF2 { string toString() { return ""; } }
    static class CF2 { override string toString() { return ""; } }

    static interface IK1 { void toString(scope void delegate(scope const(char)[]) sink,
                           FormatSpec!char) const; }
    static class CIK1 : IK1 { override void toString(scope void delegate(scope const(char)[]) sink,
                              FormatSpec!char) const { sink("CIK1"); } }
    static struct KS1 { void toString(scope void delegate(scope const(char)[]) sink,
                        FormatSpec!char) const { sink("KS1"); } }

    static union KU1 { void toString(scope void delegate(scope const(char)[]) sink,
                       FormatSpec!char) const { sink("KU1"); } }

    static class KC1 { void toString(scope void delegate(scope const(char)[]) sink,
                       FormatSpec!char) const { sink("KC1"); } }

    IF1 cif1 = new CIF1;
    assertThrown!FormatException(format("%f", cif1));
    assertThrown!FormatException(format("%f", SF1()));
    assertThrown!FormatException(format("%f", UF1()));
    assertThrown!FormatException(format("%f", new CF1()));

    IF2 cif2 = new CIF2;
    assertThrown!FormatException(format("%f", cif2));
    assertThrown!FormatException(format("%f", SF2()));
    assertThrown!FormatException(format("%f", UF2()));
    assertThrown!FormatException(format("%f", new CF2()));

    IK1 cik1 = new CIK1;
    assert(format("%f", cik1) == "CIK1");
    assert(format("%f", KS1()) == "KS1");
    assert(format("%f", KU1()) == "KU1");
    assert(format("%f", new KC1()) == "KC1");
}

/*
    Aggregates
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, T val, scope const ref FormatSpec!Char f)
if (is(T == class) && !is(T == enum))
{
    import std.range.primitives : put;

    enforceValidFormatSpec!(T, Char)(f);

    // TODO: remove this check once `@disable override` deprecation cycle is finished
    static if (__traits(hasMember, T, "toString") && isSomeFunction!(val.toString))
        static assert(!__traits(isDisabled, T.toString), T.stringof ~
            " cannot be formatted because its `toString` is marked with `@disable`");

    if (val is null)
        put(w, "null");
    else
    {
        import std.algorithm.comparison : among;
        enum overload = hasToString!(T, Char);
        with(HasToStringResult)
        static if ((is(T == immutable) || is(T == const) || is(T == shared)) && overload == none)
        {
            // Remove this when Object gets const toString
            // https://issues.dlang.org/show_bug.cgi?id=7879
            static if (is(T == immutable))
                put(w, "immutable(");
            else static if (is(T == const))
                put(w, "const(");
            else static if (is(T == shared))
                put(w, "shared(");

            put(w, typeid(Unqual!T).name);
            put(w, ')');
        }
        else static if (overload.among(constCharSink, constCharSinkFormatString, constCharSinkFormatSpec) ||
                       (!isInputRange!T && !is(BuiltinTypeOf!T)))
        {
            formatObject!(Writer, T, Char)(w, val, f);
        }
        else
        {
            static if (!is(__traits(parent, T.toString) == Object)) // not inherited Object.toString
            {
                formatObject(w, val, f);
            }
            else static if (isInputRange!T)
            {
                formatRange(w, val, f);
            }
            else static if (is(BuiltinTypeOf!T X))
            {
                X x = val;
                formatValueImpl(w, x, f);
            }
            else
            {
                formatObject(w, val, f);
            }
        }
    }
}

@system unittest
{
    import std.array : appender;
    import std.range.interfaces : inputRangeObject;

    // class range (https://issues.dlang.org/show_bug.cgi?id=5154)
    auto c = inputRangeObject([1,2,3,4]);
    formatTest(c, "[1, 2, 3, 4]");
    assert(c.empty);
    c = null;
    formatTest(c, "null");
}

@system unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=5354
    // If the class has both range I/F and custom toString, the use of custom
    // toString routine is prioritized.

    // Enable the use of custom toString that gets a sink delegate
    // for class formatting.

    enum inputRangeCode =
    q{
        int[] arr;
        this(int[] a){ arr = a; }
        @property int front() const { return arr[0]; }
        @property bool empty() const { return arr.length == 0; }
        void popFront(){ arr = arr[1 .. $]; }
    };

    class C1
    {
        mixin(inputRangeCode);
        void toString(scope void delegate(scope const(char)[]) dg,
                      scope const ref FormatSpec!char f) const
        {
            dg("[012]");
        }
    }
    class C2
    {
        mixin(inputRangeCode);
        void toString(scope void delegate(const(char)[]) dg, string f) const { dg("[012]"); }
    }
    class C3
    {
        mixin(inputRangeCode);
        void toString(scope void delegate(const(char)[]) dg) const { dg("[012]"); }
    }
    class C4
    {
        mixin(inputRangeCode);
        override string toString() const { return "[012]"; }
    }
    class C5
    {
        mixin(inputRangeCode);
    }

    formatTest(new C1([0, 1, 2]), "[012]");
    formatTest(new C2([0, 1, 2]), "[012]");
    formatTest(new C3([0, 1, 2]), "[012]");
    formatTest(new C4([0, 1, 2]), "[012]");
    formatTest(new C5([0, 1, 2]), "[0, 1, 2]");
}

// outside the unittest block, otherwise the FQN of the
// class contains the line number of the unittest
version (StdUnittest)
{
    private class C {}
}

// https://issues.dlang.org/show_bug.cgi?id=7879
@safe unittest
{
    const(C) c;
    auto s = format("%s", c);
    assert(s == "null");

    immutable(C) c2 = new C();
    s = format("%s", c2);
    assert(s == "immutable(std.format.internal.write.C)");

    const(C) c3 = new C();
    s = format("%s", c3);
    assert(s == "const(std.format.internal.write.C)");

    shared(C) c4 = new C();
    s = format("%s", c4);
    assert(s == "shared(std.format.internal.write.C)");
}

// https://issues.dlang.org/show_bug.cgi?id=7879
@safe unittest
{
    class F
    {
        override string toString() const @safe
        {
            return "Foo";
        }
    }

    const(F) c;
    auto s = format("%s", c);
    assert(s == "null");

    const(F) c2 = new F();
    s = format("%s", c2);
    assert(s == "Foo", s);
}

void formatValueImpl(Writer, T, Char)(auto ref Writer w, T val, scope const ref FormatSpec!Char f)
if (is(T == interface) && (hasToString!(T, Char) || !is(BuiltinTypeOf!T)) && !is(T == enum))
{
    import std.range.primitives : put;

    enforceValidFormatSpec!(T, Char)(f);
    if (val is null)
        put(w, "null");
    else
    {
        static if (__traits(hasMember, T, "toString") && isSomeFunction!(val.toString))
            static assert(!__traits(isDisabled, T.toString), T.stringof ~
                " cannot be formatted because its `toString` is marked with `@disable`");

        static if (hasToString!(T, Char) != HasToStringResult.none)
        {
            formatObject(w, val, f);
        }
        else static if (isInputRange!T)
        {
            formatRange(w, val, f);
        }
        else
        {
            version (Windows)
            {
                import core.sys.windows.com : IUnknown;
                static if (is(T : IUnknown))
                {
                    formatValueImpl(w, *cast(void**)&val, f);
                }
                else
                {
                    formatValueImpl(w, cast(Object) val, f);
                }
            }
            else
            {
                formatValueImpl(w, cast(Object) val, f);
            }
        }
    }
}

@system unittest
{
    import std.range.interfaces : InputRange, inputRangeObject;

    // interface
    InputRange!int i = inputRangeObject([1,2,3,4]);
    formatTest(i, "[1, 2, 3, 4]");
    assert(i.empty);
    i = null;
    formatTest(i, "null");

    // interface (downcast to Object)
    interface Whatever {}
    class C : Whatever
    {
        override @property string toString() const { return "ab"; }
    }
    Whatever val = new C;
    formatTest(val, "ab");

    // https://issues.dlang.org/show_bug.cgi?id=11175
    version (Windows)
    {
        import core.sys.windows.com : IID, IUnknown;
        import core.sys.windows.windef : HRESULT;

        interface IUnknown2 : IUnknown { }

        class D : IUnknown2
        {
            extern(Windows) HRESULT QueryInterface(const(IID)* riid, void** pvObject) { return typeof(return).init; }
            extern(Windows) uint AddRef() { return 0; }
            extern(Windows) uint Release() { return 0; }
        }

        IUnknown2 d = new D;
        string expected = format("%X", cast(void*) d);
        formatTest(d, expected);
    }
}

// Maybe T is noncopyable struct, so receive it by 'auto ref'.
void formatValueImpl(Writer, T, Char)(auto ref Writer w, auto ref T val,
    scope const ref FormatSpec!Char f)
if ((is(T == struct) || is(T == union)) && (hasToString!(T, Char) || !is(BuiltinTypeOf!T))
    && !is(T == enum))
{
    import std.range.primitives : put;

    static if (__traits(hasMember, T, "toString") && isSomeFunction!(val.toString))
        static assert(!__traits(isDisabled, T.toString), T.stringof ~
            " cannot be formatted because its `toString` is marked with `@disable`");

    enforceValidFormatSpec!(T, Char)(f);
    static if (hasToString!(T, Char))
    {
        formatObject(w, val, f);
    }
    else static if (isInputRange!T)
    {
        formatRange(w, val, f);
    }
    else static if (is(T == struct))
    {
        enum left = T.stringof~"(";
        enum separator = ", ";
        enum right = ")";

        put(w, left);
        static foreach (i; 0 .. T.tupleof.length)
        {{
            static if (__traits(identifier, val.tupleof[i]) == "this")
            {
                // ignore hidden context pointer
            }
            else static if (0 < i && T.tupleof[i-1].offsetof == T.tupleof[i].offsetof)
            {
                static if (i == T.tupleof.length - 1 || T.tupleof[i].offsetof != T.tupleof[i+1].offsetof)
                {
                    enum el = separator ~ __traits(identifier, T.tupleof[i]) ~ "}";
                    put(w, el);
                }
                else
                {
                    enum el = separator ~ __traits(identifier, T.tupleof[i]);
                    put(w, el);
                }
            }
            else static if (i+1 < T.tupleof.length && T.tupleof[i].offsetof == T.tupleof[i+1].offsetof)
            {
                enum el = (i > 0 ? separator : "") ~ "#{overlap " ~ __traits(identifier, T.tupleof[i]);
                put(w, el);
            }
            else
            {
                static if (i > 0)
                    put(w, separator);
                formatElement(w, val.tupleof[i], f);
            }
        }}
        put(w, right);
    }
    else
    {
        put(w, T.stringof);
    }
}

// https://issues.dlang.org/show_bug.cgi?id=9588
@safe pure unittest
{
    struct S { int x; bool empty() { return false; } }
    formatTest(S(), "S(0)");
}

// https://issues.dlang.org/show_bug.cgi?id=4638
@safe unittest
{
    struct U8  {  string toString() const { return "blah"; } }
    struct U16 { wstring toString() const { return "blah"; } }
    struct U32 { dstring toString() const { return "blah"; } }
    formatTest(U8(), "blah");
    formatTest(U16(), "blah");
    formatTest(U32(), "blah");
}

// https://issues.dlang.org/show_bug.cgi?id=3890
@safe unittest
{
    struct Int{ int n; }
    struct Pair{ string s; Int i; }
    formatTest(Pair("hello", Int(5)),
               `Pair("hello", Int(5))`);
}

// https://issues.dlang.org/show_bug.cgi?id=9117
@safe unittest
{
    import std.format : formattedWrite;

    static struct Frop {}

    static struct Foo
    {
        int n = 0;
        alias n this;
        T opCast(T) ()
        if (is(T == Frop))
        {
            return Frop();
        }
        string toString()
        {
            return "Foo";
        }
    }

    static struct Bar
    {
        Foo foo;
        alias foo this;
        string toString()
        {
            return "Bar";
        }
    }

    const(char)[] result;
    void put(scope const char[] s) { result ~= s; }

    Foo foo;
    formattedWrite(&put, "%s", foo);    // OK
    assert(result == "Foo");

    result = null;

    Bar bar;
    formattedWrite(&put, "%s", bar);    // NG
    assert(result == "Bar");

    result = null;

    int i = 9;
    formattedWrite(&put, "%s", 9);
    assert(result == "9");
}

@safe unittest
{
    // union formatting without toString
    union U1
    {
        int n;
        string s;
    }
    U1 u1;
    formatTest(u1, "U1");

    // union formatting with toString
    union U2
    {
        int n;
        string s;
        string toString() @trusted const { return s; }
    }
    U2 u2;
    () @trusted { u2.s = "hello"; } ();
    formatTest(u2, "hello");
}

@safe unittest
{
    import std.array : appender;
    import std.format : formatValue;

    // https://issues.dlang.org/show_bug.cgi?id=7230
    static struct Bug7230
    {
        string s = "hello";
        union {
            string a;
            int b;
            double c;
        }
        long x = 10;
    }

    Bug7230 bug;
    bug.b = 123;

    FormatSpec!char f;
    auto w = appender!(char[])();
    formatValue(w, bug, f);
    assert(w.data == `Bug7230("hello", #{overlap a, b, c}, 10)`);
}

@safe unittest
{
    import std.array : appender;
    import std.format : formatValue;

    static struct S{ @disable this(this); }
    S s;

    FormatSpec!char f;
    auto w = appender!string();
    formatValue(w, s, f);
    assert(w.data == "S()");
}

@safe unittest
{
    import std.array : appender;
    import std.format : formatValue;

    //struct Foo { @disable string toString(); }
    //Foo foo;

    interface Bar { @disable string toString(); }
    Bar bar;

    auto w = appender!(char[])();
    FormatSpec!char f;

    // NOTE: structs cant be tested : the assertion is correct so compilation
    // continues and fails when trying to link the unimplemented toString.
    //static assert(!__traits(compiles, formatValue(w, foo, f)));
    static assert(!__traits(compiles, formatValue(w, bar, f)));
}

// https://issues.dlang.org/show_bug.cgi?id=21722
@safe unittest
{
    struct Bar
    {
        void toString (scope void delegate (scope const(char)[]) sink, string fmt)
        {
            sink("Hello");
        }
    }

    Bar b;
    auto result = () @trusted { return format("%b", b); } ();
    assert(result == "Hello");

    static if (hasPreviewIn)
    {
        struct Foo
        {
            void toString(scope void delegate(in char[]) sink, in FormatSpec!char fmt)
            {
                sink("Hello");
            }
        }

        Foo f;
        assert(format("%b", f) == "Hello");

        struct Foo2
        {
            void toString(scope void delegate(in char[]) sink, string fmt)
            {
                sink("Hello");
            }
        }

        Foo2 f2;
        assert(format("%b", f2) == "Hello");
    }
}

@safe unittest
{
    import std.array : appender;
    import std.format : singleSpec;

    // Bug #17269. Behavior similar to `struct A { Nullable!string B; }`
    struct StringAliasThis
    {
        @property string value() const { assert(0); }
        alias value this;
        string toString() { return "helloworld"; }
        private string _value;
    }
    struct TestContainer
    {
        StringAliasThis testVar;
    }

    auto w = appender!string();
    auto spec = singleSpec("%s");
    formatElement(w, TestContainer(), spec);

    assert(w.data == "TestContainer(helloworld)", w.data);
}

// https://issues.dlang.org/show_bug.cgi?id=17269
@safe unittest
{
    import std.typecons : Nullable;

    struct Foo
    {
        Nullable!string bar;
    }

    Foo f;
    formatTest(f, "Foo(Nullable.null)");
}

// https://issues.dlang.org/show_bug.cgi?id=19003
@safe unittest
{
    struct S
    {
        int i;

        @disable this();

        invariant { assert(this.i); }

        this(int i) @safe in { assert(i); } do { this.i = i; }

        string toString() { return "S"; }
    }

    S s = S(1);

    format!"%s"(s);
}

void enforceValidFormatSpec(T, Char)(scope const ref FormatSpec!Char f)
{
    import std.format : enforceFmt;
    import std.range : isInputRange;
    import std.format.internal.write : hasToString, HasToStringResult;

    enum overload = hasToString!(T, Char);
    static if (
            overload != HasToStringResult.constCharSinkFormatSpec &&
            overload != HasToStringResult.constCharSinkFormatString &&
            overload != HasToStringResult.inCharSinkFormatSpec &&
            overload != HasToStringResult.inCharSinkFormatString &&
            overload != HasToStringResult.customPutWriterFormatSpec &&
            !isInputRange!T)
    {
        enforceFmt(f.spec == 's',
            "Expected '%s' format specifier for type '" ~ T.stringof ~ "'");
    }
}

/*
    `enum`s are formatted like their base value
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, T val, scope const ref FormatSpec!Char f)
if (is(T == enum))
{
    import std.array : appender;
    import std.range.primitives : put;

    if (f.spec != 's')
        return formatValueImpl(w, cast(OriginalType!T) val, f);

    foreach (immutable member; __traits(allMembers, T))
        if (val == __traits(getMember, T, member))
            return formatValueImpl(w, member, f);

    auto w2 = appender!string();

    // val is not a member of T, output cast(T) rawValue instead.
    enum prefix = "cast(" ~ T.stringof ~ ")";
    put(w2, prefix);
    static assert(!is(OriginalType!T == T), "OriginalType!" ~ T.stringof ~
                  "must not be equal to " ~ T.stringof);

    FormatSpec!Char f2 = f;
    f2.width = 0;
    formatValueImpl(w2, cast(OriginalType!T) val, f2);
    writeAligned(w, w2.data, f);
}

@safe unittest
{
    enum A { first, second, third }
    formatTest(A.second, "second");
    formatTest(cast(A) 72, "cast(A)72");
}
@safe unittest
{
    enum A : string { one = "uno", two = "dos", three = "tres" }
    formatTest(A.three, "three");
    formatTest(cast(A)"mill\&oacute;n", "cast(A)mill\&oacute;n");
}
@safe unittest
{
    enum A : bool { no, yes }
    formatTest(A.yes, "yes");
    formatTest(A.no, "no");
}
@safe unittest
{
    // Test for bug 6892
    enum Foo { A = 10 }
    formatTest("%s",    Foo.A, "A");
    formatTest(">%4s<", Foo.A, ">   A<");
    formatTest("%04d",  Foo.A, "0010");
    formatTest("%+2u",  Foo.A, "10");
    formatTest("%02x",  Foo.A, "0a");
    formatTest("%3o",   Foo.A, " 12");
    formatTest("%b",    Foo.A, "1010");
}

@safe pure unittest
{
    enum A { one, two, three }

    string t1 = format("[%6s] [%-6s]", A.one, A.one);
    assert(t1 == "[   one] [one   ]");
    string t2 = format("[%10s] [%-10s]", cast(A) 10, cast(A) 10);
    assert(t2 == "[ cast(A)" ~ "10] [cast(A)" ~ "10 ]"); // due to bug in style checker
}

// https://issues.dlang.org/show_bug.cgi?id=8921
@safe unittest
{
    enum E : char { A = 'a', B = 'b', C = 'c' }
    E[3] e = [E.A, E.B, E.C];
    formatTest(e, "[A, B, C]");

    E[] e2 = [E.A, E.B, E.C];
    formatTest(e2, "[A, B, C]");
}

/*
    Pointers are formatted as hex integers.
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, scope const(T) val, scope const ref FormatSpec!Char f)
if (isPointer!T && !is(T == enum) && !hasToString!(T, Char))
{
    static if (is(typeof({ shared const void* p = val; })))
        alias SharedOf(T) = shared(T);
    else
        alias SharedOf(T) = T;

    const SharedOf!(void*) p = val;
    const pnum = () @trusted { return cast(ulong) p; }();

    if (f.spec == 's')
    {
        if (p is null)
        {
            writeAligned(w, "null", f);
            return;
        }
        FormatSpec!Char fs = f; // fs is copy for change its values.
        fs.spec = 'X';
        formatValueImpl(w, pnum, fs);
    }
    else
    {
        import std.format : enforceFmt;
        enforceFmt(f.spec == 'X' || f.spec == 'x',
            "Expected one of %s, %x or %X for pointer type.");
        formatValueImpl(w, pnum, f);
    }
}

@safe pure unittest
{
    int* p;

    string t1 = format("[%6s] [%-6s]", p, p);
    assert(t1 == "[  null] [null  ]");
}

@safe pure unittest
{
    int* p = null;
    formatTest(p, "null");

    auto q = () @trusted { return cast(void*) 0xFFEECCAA; }();
    formatTest(q, "FFEECCAA");
}

// https://issues.dlang.org/show_bug.cgi?id=11782
@safe pure unittest
{
    import std.range : iota;

    auto a = iota(0, 10);
    auto b = iota(0, 10);
    auto p = () @trusted { auto result = &a; return result; }();

    assert(format("%s",p) != format("%s",b));
}

@safe pure unittest
{
    // Test for https://issues.dlang.org/show_bug.cgi?id=7869
    struct S
    {
        string toString() const { return ""; }
    }
    S* p = null;
    formatTest(p, "null");

    S* q = () @trusted { return cast(S*) 0xFFEECCAA; } ();
    formatTest(q, "FFEECCAA");
}

// https://issues.dlang.org/show_bug.cgi?id=9336
@system pure unittest
{
    shared int i;
    format("%s", &i);
}

// https://issues.dlang.org/show_bug.cgi?id=11778
@safe pure unittest
{
    import std.exception : assertThrown;
    import std.format : FormatException;

    int* p = null;
    assertThrown!FormatException(format("%d", p));
    assertThrown!FormatException(format("%04d", () @trusted { return p + 2; } ()));
}

// https://issues.dlang.org/show_bug.cgi?id=12505
@safe pure unittest
{
    void* p = null;
    formatTest("%08X", p, "00000000");
}

/*
    SIMD vectors are formatted as arrays.
 */
void formatValueImpl(Writer, V, Char)(auto ref Writer w, const(V) val, scope const ref FormatSpec!Char f)
if (isSIMDVector!V)
{
    formatValueImpl(w, val.array, f);
}

@safe unittest
{
    import core.simd; // cannot be selective, because float4 might not be defined

    static if (is(float4))
    {
        version (X86)
        {
            version (OSX) {/* https://issues.dlang.org/show_bug.cgi?id=17823 */}
        }
        else
        {
            float4 f;
            f.array[0] = 1;
            f.array[1] = 2;
            f.array[2] = 3;
            f.array[3] = 4;
            formatTest(f, "[1, 2, 3, 4]");
        }
    }
}

/*
    Delegates are formatted by `ReturnType delegate(Parameters) FunctionAttributes`

    Known bug: Because of issue https://issues.dlang.org/show_bug.cgi?id=18269
               the FunctionAttributes might be wrong.
 */
void formatValueImpl(Writer, T, Char)(auto ref Writer w, scope const(T), scope const ref FormatSpec!Char f)
if (isDelegate!T)
{
    formatValueImpl(w, T.stringof, f);
}

@safe unittest
{
    import std.array : appender;
    import std.format : formatValue;

    void func() @system { __gshared int x; ++x; throw new Exception("msg"); }
    version (linux)
    {
        FormatSpec!char f;
        auto w = appender!string();
        formatValue(w, &func, f);
        assert(w.data.length >= 15 && w.data[0 .. 15] == "void delegate()");
    }
}

// string elements are formatted like UTF-8 string literals.
void formatElement(Writer, T, Char)(auto ref Writer w, T val, scope const ref FormatSpec!Char f)
if (is(StringTypeOf!T) && !hasToString!(T, Char) && !is(T == enum))
{
    import std.array : appender;
    import std.format.write : formattedWrite, formatValue;
    import std.range.primitives : put;
    import std.utf : decode, UTFException;

    StringTypeOf!T str = val;   // https://issues.dlang.org/show_bug.cgi?id=8015

    if (f.spec == 's')
    {
        try
        {
            // ignore other specifications and quote
            for (size_t i = 0; i < str.length; )
            {
                auto c = decode(str, i);
                // \uFFFE and \uFFFF are considered valid by isValidDchar,
                // so need checking for interchange.
                if (c == 0xFFFE || c == 0xFFFF)
                    goto LinvalidSeq;
            }
            put(w, '\"');
            for (size_t i = 0; i < str.length; )
            {
                auto c = decode(str, i);
                formatChar(w, c, '"');
            }
            put(w, '\"');
            return;
        }
        catch (UTFException)
        {
        }

        // If val contains invalid UTF sequence, formatted like HexString literal
    LinvalidSeq:
        static if (is(typeof(str[0]) : const(char)))
        {
            enum type = "";
            alias IntArr = const(ubyte)[];
        }
        else static if (is(typeof(str[0]) : const(wchar)))
        {
            enum type = "w";
            alias IntArr = const(ushort)[];
        }
        else static if (is(typeof(str[0]) : const(dchar)))
        {
            enum type = "d";
            alias IntArr = const(uint)[];
        }
        formattedWrite(w, "[%(cast(" ~ type ~ "char) 0x%X%|, %)]", cast(IntArr) str);
    }
    else
        formatValue(w, str, f);
}

@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%s");
    formatElement(w, "Hello World", spec);

    assert(w.data == "\"Hello World\"");
}

@safe unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%s");
    formatElement(w, "H", spec);

    assert(w.data == "\"H\"", w.data);
}

// https://issues.dlang.org/show_bug.cgi?id=15888
@safe pure unittest
{
    import std.array : appender;
    import std.format.spec : singleSpec;

    ushort[] a = [0xFF_FE, 0x42];
    auto w = appender!string();
    auto spec = singleSpec("%s");
    formatElement(w, cast(wchar[]) a, spec);
    assert(w.data == `[cast(wchar) 0xFFFE, cast(wchar) 0x42]`);

    uint[] b = [0x0F_FF_FF_FF, 0x42];
    w = appender!string();
    spec = singleSpec("%s");
    formatElement(w, cast(dchar[]) b, spec);
    assert(w.data == `[cast(dchar) 0xFFFFFFF, cast(dchar) 0x42]`);
}

// Character elements are formatted like UTF-8 character literals.
void formatElement(Writer, T, Char)(auto ref Writer w, T val, scope const ref FormatSpec!Char f)
if (is(CharTypeOf!T) && !is(T == enum))
{
    import std.range.primitives : put;
    import std.format.write : formatValue;

    if (f.spec == 's')
    {
        put(w, '\'');
        formatChar(w, val, '\'');
        put(w, '\'');
    }
    else
        formatValue(w, val, f);
}

// Maybe T is noncopyable struct, so receive it by 'auto ref'.
void formatElement(Writer, T, Char)(auto ref Writer w, auto ref T val, scope const ref FormatSpec!Char f)
if ((!is(StringTypeOf!T) || hasToString!(T, Char)) && !is(CharTypeOf!T) || is(T == enum))
{
    import std.format.write : formatValue;

    formatValue(w, val, f);
}

// Fix for https://issues.dlang.org/show_bug.cgi?id=1591
int getNthInt(string kind, A...)(uint index, A args)
{
    return getNth!(kind, isIntegral, int)(index, args);
}

T getNth(string kind, alias Condition, T, A...)(uint index, A args)
{
    import std.conv : text, to;
    import std.format : FormatException;

    switch (index)
    {
        foreach (n, _; A)
        {
            case n:
                static if (Condition!(typeof(args[n])))
                {
                    return to!T(args[n]);
                }
                else
                {
                    throw new FormatException(
                        text(kind, " expected, not ", typeof(args[n]).stringof,
                            " for argument #", index + 1));
                }
        }
        default:
            throw new FormatException(text("Missing ", kind, " argument"));
    }
}

private bool needToSwapEndianess(Char)(scope const ref FormatSpec!Char f)
{
    import std.system : endian, Endian;

    return endian == Endian.littleEndian && f.flPlus
        || endian == Endian.bigEndian && f.flDash;
}

void writeAligned(Writer, T, Char)(auto ref Writer w, T s, scope const ref FormatSpec!Char f)
if (isSomeString!T)
{
    FormatSpec!Char fs = f;
    fs.flZero = false;
    writeAligned(w, "", "", s, fs);
}

@safe pure unittest
{
    import std.array : appender;
    import std.format : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%s");
    writeAligned(w, "a本Ä", spec);
    assert(w.data == "a本Ä", w.data);
}

@safe pure unittest
{
    import std.array : appender;
    import std.format : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%10s");
    writeAligned(w, "a本Ä", spec);
    assert(w.data == "       a本Ä", "|" ~ w.data ~ "|");
}

@safe pure unittest
{
    import std.array : appender;
    import std.format : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%-10s");
    writeAligned(w, "a本Ä", spec);
    assert(w.data == "a本Ä       ", w.data);
}

enum PrecisionType
{
    none,
    integer,
    fractionalDigits,
    allDigits,
}

void writeAligned(Writer, T1, T2, T3, Char)(auto ref Writer w,
    T1 prefix, T2 grouped, T3 suffix, scope const ref FormatSpec!Char f,
    bool integer_precision = false)
if (isSomeString!T1 && isSomeString!T2 && isSomeString!T3)
{
    writeAligned(w, prefix, grouped, "", suffix, f,
                 integer_precision ? PrecisionType.integer : PrecisionType.none);
}

void writeAligned(Writer, T1, T2, T3, T4, Char)(auto ref Writer w,
    T1 prefix, T2 grouped, T3 fracts, T4 suffix, scope const ref FormatSpec!Char f,
    PrecisionType p = PrecisionType.none)
if (isSomeString!T1 && isSomeString!T2 && isSomeString!T3 && isSomeString!T4)
{
    // writes: left padding, prefix, leading zeros, grouped, fracts, suffix, right padding

    if (p == PrecisionType.integer && f.precision == f.UNSPECIFIED)
        p = PrecisionType.none;

    import std.range.primitives : put;

    long prefixWidth;
    long groupedWidth = grouped.length; // TODO: does not take graphemes into account
    long fractsWidth = fracts.length; // TODO: does not take graphemes into account
    long suffixWidth;

    // TODO: remove this workaround which hides https://issues.dlang.org/show_bug.cgi?id=21815
    if (f.width > 0)
    {
        prefixWidth = getWidth(prefix);
        suffixWidth = getWidth(suffix);
    }

    auto doGrouping = f.flSeparator && groupedWidth > 0
                      && f.separators > 0 && f.separators != f.UNSPECIFIED;
    // front = number of symbols left of the leftmost separator
    long front = doGrouping ? (groupedWidth - 1) % f.separators + 1 : 0;
    // sepCount = number of separators to be inserted
    long sepCount = doGrouping ? (groupedWidth - 1) / f.separators : 0;

    long trailingZeros = 0;
    if (p == PrecisionType.fractionalDigits)
        trailingZeros = f.precision - (fractsWidth - 1);
    if (p == PrecisionType.allDigits && f.flHash)
    {
        if (grouped != "0")
            trailingZeros = f.precision - (fractsWidth - 1) - groupedWidth;
        else
        {
            trailingZeros = f.precision - fractsWidth;
            foreach (i;0 .. fracts.length)
                if (fracts[i] != '0' && fracts[i] != '.')
                {
                    trailingZeros = f.precision - (fracts.length - i);
                    break;
                }
        }
    }

    auto nodot = fracts == "." && trailingZeros == 0 && !f.flHash;

    if (nodot) fractsWidth = 0;

    long width = prefixWidth + sepCount + groupedWidth + fractsWidth + trailingZeros + suffixWidth;
    long delta = f.width - width;

    // with integers, precision is considered the minimum number of digits;
    // if digits are missing, we have to recalculate everything
    long pregrouped = 0;
    if (p == PrecisionType.integer && groupedWidth < f.precision)
    {
        pregrouped = f.precision - groupedWidth;
        delta -= pregrouped;
        if (doGrouping)
        {
            front = ((front - 1) + pregrouped) % f.separators + 1;
            delta -= (f.precision - 1) / f.separators - sepCount;
        }
    }

    // left padding
    if ((!f.flZero || p == PrecisionType.integer) && delta > 0)
    {
        if (f.flEqual)
        {
            foreach (i ; 0 .. delta / 2 + ((delta % 2 == 1 && !f.flDash) ? 1 : 0))
                put(w, ' ');
        }
        else if (!f.flDash)
        {
            foreach (i ; 0 .. delta)
                put(w, ' ');
        }
    }

    // prefix
    put(w, prefix);

    // leading grouped zeros
    if (f.flZero && p != PrecisionType.integer && !f.flDash && delta > 0)
    {
        if (doGrouping)
        {
            // front2 and sepCount2 are the same as above for the leading zeros
            long front2 = (delta + front - 1) % (f.separators + 1) + 1;
            long sepCount2 = (delta + front - 1) / (f.separators + 1);
            delta -= sepCount2;

            // according to POSIX: if the first symbol is a separator,
            // an additional zero is put left of it, even if that means, that
            // the total width is one more then specified
            if (front2 > f.separators) { front2 = 1; }

            foreach (i ; 0 .. delta)
            {
                if (front2 == 0)
                {
                    put(w, f.separatorChar);
                    front2 = f.separators;
                }
                front2--;

                put(w, '0');
            }

            // separator between zeros and grouped
            if (front == f.separators)
                put(w, f.separatorChar);
        }
        else
            foreach (i ; 0 .. delta)
                put(w, '0');
    }

    // grouped content
    if (doGrouping)
    {
        // TODO: this does not take graphemes into account
        foreach (i;0 .. pregrouped + grouped.length)
        {
            if (front == 0)
            {
                put(w, f.separatorChar);
                front = f.separators;
            }
            front--;

            put(w, i < pregrouped ? '0' : grouped[cast(size_t) (i - pregrouped)]);
        }
    }
    else
    {
        foreach (i;0 .. pregrouped)
            put(w, '0');
        put(w, grouped);
    }

    // fracts
    if (!nodot)
        put(w, fracts);

    // trailing zeros
    foreach (i ; 0 .. trailingZeros)
        put(w, '0');

    // suffix
    put(w, suffix);

    // right padding
    if (delta > 0)
    {
        if (f.flEqual)
        {
            foreach (i ; 0 .. delta / 2 + ((delta % 2 == 1 && f.flDash) ? 1 : 0))
                put(w, ' ');
        }
        else if (f.flDash)
        {
            foreach (i ; 0 .. delta)
                put(w, ' ');
        }
    }
}

@safe pure unittest
{
    import std.array : appender;
    import std.format : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "pregroupingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%20s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "      pregroupingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%-20s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "pregroupingsuf      ", w.data);

    w = appender!string();
    spec = singleSpec("%020s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "pre000000groupingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%-020s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "pregroupingsuf      ", w.data);

    w = appender!string();
    spec = singleSpec("%20,1s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "preg,r,o,u,p,i,n,gsuf", w.data);

    w = appender!string();
    spec = singleSpec("%20,2s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "   pregr,ou,pi,ngsuf", w.data);

    w = appender!string();
    spec = singleSpec("%20,3s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "    pregr,oup,ingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%20,10s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "      pregroupingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%020,1s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "preg,r,o,u,p,i,n,gsuf", w.data);

    w = appender!string();
    spec = singleSpec("%020,2s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "pre00,gr,ou,pi,ngsuf", w.data);

    w = appender!string();
    spec = singleSpec("%020,3s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "pre00,0gr,oup,ingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%020,10s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "pre000,00groupingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%021,3s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "pre000,0gr,oup,ingsuf", w.data);

    // According to https://github.com/dlang/phobos/pull/7112 this
    // is defined by POSIX standard:
    w = appender!string();
    spec = singleSpec("%022,3s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "pre0,000,0gr,oup,ingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%023,3s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "pre0,000,0gr,oup,ingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%,3s");
    writeAligned(w, "pre", "grouping", "suf", spec);
    assert(w.data == "pregr,oup,ingsuf", w.data);
}

@safe pure unittest
{
    import std.array : appender;
    import std.format : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%.10s");
    writeAligned(w, "pre", "grouping", "suf", spec, true);
    assert(w.data == "pre00groupingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%.10,3s");
    writeAligned(w, "pre", "grouping", "suf", spec, true);
    assert(w.data == "pre0,0gr,oup,ingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%25.10,3s");
    writeAligned(w, "pre", "grouping", "suf", spec, true);
    assert(w.data == "      pre0,0gr,oup,ingsuf", w.data);

    // precision has precedence over zero flag
    w = appender!string();
    spec = singleSpec("%025.12,3s");
    writeAligned(w, "pre", "grouping", "suf", spec, true);
    assert(w.data == "    pre000,0gr,oup,ingsuf", w.data);

    w = appender!string();
    spec = singleSpec("%025.13,3s");
    writeAligned(w, "pre", "grouping", "suf", spec, true);
    assert(w.data == "  pre0,000,0gr,oup,ingsuf", w.data);
}

@safe unittest
{
    assert(format("%,d", 1000) == "1,000");
    assert(format("%,f", 1234567.891011) == "1,234,567.891011");
    assert(format("%,?d", '?', 1000) == "1?000");
    assert(format("%,1d", 1000) == "1,0,0,0", format("%,1d", 1000));
    assert(format("%,*d", 4, -12345) == "-1,2345");
    assert(format("%,*?d", 4, '_', -12345) == "-1_2345");
    assert(format("%,6?d", '_', -12345678) == "-12_345678");
    assert(format("%12,3.3f", 1234.5678) == "   1,234.568", "'" ~
           format("%12,3.3f", 1234.5678) ~ "'");
}

private long getWidth(T)(T s)
{
    import std.algorithm.searching : all;
    import std.uni : graphemeStride;

    // check for non-ascii character
    if (s.all!(a => a <= 0x7F)) return s.length;

    //TODO: optimize this
    long width = 0;
    for (size_t i; i < s.length; i += graphemeStride(s, i))
        ++width;
    return width;
}

enum RoundingClass { ZERO, LOWER, FIVE, UPPER }
enum RoundingMode { up, down, toZero, toNearestTiesToEven, toNearestTiesAwayFromZero }

bool round(T)(ref T sequence, size_t left, size_t right, RoundingClass type, bool negative, char max = '9')
in (left >= 0) // should be left > 0, but if you know ahead, that there's no carry, left == 0 is fine
in (left < sequence.length)
in (right >= 0)
in (right <= sequence.length)
in (right >= left)
in (max == '9' || max == 'f' || max == 'F')
{
    import std.math.hardware;

    auto mode = RoundingMode.toNearestTiesToEven;

    if (!__ctfe)
    {
        // std.math's FloatingPointControl isn't available on all target platforms
        static if (is(FloatingPointControl))
        {
            switch (FloatingPointControl.rounding)
            {
            case FloatingPointControl.roundUp:
                mode = RoundingMode.up;
                break;
            case FloatingPointControl.roundDown:
                mode = RoundingMode.down;
                break;
            case FloatingPointControl.roundToZero:
                mode = RoundingMode.toZero;
                break;
            case FloatingPointControl.roundToNearest:
                mode = RoundingMode.toNearestTiesToEven;
                break;
            default: assert(false, "Unknown floating point rounding mode");
            }
        }
    }

    bool roundUp = false;
    if (mode == RoundingMode.up)
        roundUp = type != RoundingClass.ZERO && !negative;
    else if (mode == RoundingMode.down)
        roundUp = type != RoundingClass.ZERO && negative;
    else if (mode == RoundingMode.toZero)
        roundUp = false;
    else
    {
        roundUp = type == RoundingClass.UPPER;

        if (type == RoundingClass.FIVE)
        {
            // IEEE754 allows for two different ways of implementing roundToNearest:

            if (mode == RoundingMode.toNearestTiesAwayFromZero)
                roundUp = true;
            else
            {
                // Round to nearest, ties to even
                auto last = sequence[right - 1];
                if (last == '.') last = sequence[right - 2];
                roundUp = (last <= '9' && last % 2 != 0) || (last > '9' && last % 2 == 0);
            }
        }
    }

    if (!roundUp) return false;

    foreach_reverse (i;left .. right)
    {
        if (sequence[i] == '.') continue;
        if (sequence[i] == max)
            sequence[i] = '0';
        else
        {
            if (max != '9' && sequence[i] == '9')
                sequence[i] = max == 'f' ? 'a' : 'A';
            else
                sequence[i]++;
            return false;
        }
    }

    sequence[left - 1] = '1';
    return true;
}

@safe unittest
{
    char[10] c;
    size_t left = 5;
    size_t right = 8;

    c[4 .. 8] = "x.99";
    assert(round(c, left, right, RoundingClass.UPPER, false) == true);
    assert(c[4 .. 8] == "1.00");

    c[4 .. 8] = "x.99";
    assert(round(c, left, right, RoundingClass.FIVE, false) == true);
    assert(c[4 .. 8] == "1.00");

    c[4 .. 8] = "x.99";
    assert(round(c, left, right, RoundingClass.LOWER, false) == false);
    assert(c[4 .. 8] == "x.99");

    c[4 .. 8] = "x.99";
    assert(round(c, left, right, RoundingClass.ZERO, false) == false);
    assert(c[4 .. 8] == "x.99");

    import std.math.hardware;
    static if (is(FloatingPointControl))
    {
        FloatingPointControl fpctrl;

        fpctrl.rounding = FloatingPointControl.roundUp;

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.UPPER, false) == true);
        assert(c[4 .. 8] == "1.00");

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.FIVE, false) == true);
        assert(c[4 .. 8] == "1.00");

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.LOWER, false) == true);
        assert(c[4 .. 8] == "1.00");

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.ZERO, false) == false);
        assert(c[4 .. 8] == "x.99");

        fpctrl.rounding = FloatingPointControl.roundDown;

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.UPPER, false) == false);
        assert(c[4 .. 8] == "x.99");

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.FIVE, false) == false);
        assert(c[4 .. 8] == "x.99");

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.LOWER, false) == false);
        assert(c[4 .. 8] == "x.99");

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.ZERO, false) == false);
        assert(c[4 .. 8] == "x.99");

        fpctrl.rounding = FloatingPointControl.roundToZero;

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.UPPER, false) == false);
        assert(c[4 .. 8] == "x.99");

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.FIVE, false) == false);
        assert(c[4 .. 8] == "x.99");

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.LOWER, false) == false);
        assert(c[4 .. 8] == "x.99");

        c[4 .. 8] = "x.99";
        assert(round(c, left, right, RoundingClass.ZERO, false) == false);
        assert(c[4 .. 8] == "x.99");
    }
}

@safe unittest
{
    char[10] c;
    size_t left = 5;
    size_t right = 8;

    c[4 .. 8] = "x8.5";
    assert(round(c, left, right, RoundingClass.UPPER, true) == false);
    assert(c[4 .. 8] == "x8.6");

    c[4 .. 8] = "x8.5";
    assert(round(c, left, right, RoundingClass.FIVE, true) == false);
    assert(c[4 .. 8] == "x8.6");

    c[4 .. 8] = "x8.4";
    assert(round(c, left, right, RoundingClass.FIVE, true) == false);
    assert(c[4 .. 8] == "x8.4");

    c[4 .. 8] = "x8.5";
    assert(round(c, left, right, RoundingClass.LOWER, true) == false);
    assert(c[4 .. 8] == "x8.5");

    c[4 .. 8] = "x8.5";
    assert(round(c, left, right, RoundingClass.ZERO, true) == false);
    assert(c[4 .. 8] == "x8.5");

    import std.math.hardware;
    static if (is(FloatingPointControl))
    {
        FloatingPointControl fpctrl;

        fpctrl.rounding = FloatingPointControl.roundUp;

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.UPPER, true) == false);
        assert(c[4 .. 8] == "x8.5");

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.FIVE, true) == false);
        assert(c[4 .. 8] == "x8.5");

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.LOWER, true) == false);
        assert(c[4 .. 8] == "x8.5");

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.ZERO, true) == false);
        assert(c[4 .. 8] == "x8.5");

        fpctrl.rounding = FloatingPointControl.roundDown;

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.UPPER, true) == false);
        assert(c[4 .. 8] == "x8.6");

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.FIVE, true) == false);
        assert(c[4 .. 8] == "x8.6");

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.LOWER, true) == false);
        assert(c[4 .. 8] == "x8.6");

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.ZERO, true) == false);
        assert(c[4 .. 8] == "x8.5");

        fpctrl.rounding = FloatingPointControl.roundToZero;

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.UPPER, true) == false);
        assert(c[4 .. 8] == "x8.5");

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.FIVE, true) == false);
        assert(c[4 .. 8] == "x8.5");

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.LOWER, true) == false);
        assert(c[4 .. 8] == "x8.5");

        c[4 .. 8] = "x8.5";
        assert(round(c, left, right, RoundingClass.ZERO, true) == false);
        assert(c[4 .. 8] == "x8.5");
    }
}

@safe unittest
{
    char[10] c;
    size_t left = 5;
    size_t right = 8;

    c[4 .. 8] = "x8.9";
    assert(round(c, left, right, RoundingClass.UPPER, true, 'f') == false);
    assert(c[4 .. 8] == "x8.a");

    c[4 .. 8] = "x8.9";
    assert(round(c, left, right, RoundingClass.UPPER, true, 'F') == false);
    assert(c[4 .. 8] == "x8.A");

    c[4 .. 8] = "x8.f";
    assert(round(c, left, right, RoundingClass.UPPER, true, 'f') == false);
    assert(c[4 .. 8] == "x9.0");
}

version (StdUnittest)
private void formatTest(T)(T val, string expected, size_t ln = __LINE__, string fn = __FILE__)
{
    formatTest(val, [expected], ln, fn);
}

version (StdUnittest)
private void formatTest(T)(string fmt, T val, string expected, size_t ln = __LINE__, string fn = __FILE__) @safe
{
    formatTest(fmt, val, [expected], ln, fn);
}

version (StdUnittest)
private void formatTest(T)(T val, string[] expected, size_t ln = __LINE__, string fn = __FILE__)
{
    import core.exception : AssertError;
    import std.algorithm.searching : canFind;
    import std.array : appender;
    import std.conv : text;
    import std.exception : enforce;
    import std.format.write : formatValue;

    FormatSpec!char f;
    auto w = appender!string();
    formatValue(w, val, f);
    enforce!AssertError(expected.canFind(w.data),
        text("expected one of `", expected, "`, result = `", w.data, "`"), fn, ln);
}

version (StdUnittest)
private void formatTest(T)(string fmt, T val, string[] expected, size_t ln = __LINE__, string fn = __FILE__) @safe
{
    import core.exception : AssertError;
    import std.algorithm.searching : canFind;
    import std.array : appender;
    import std.conv : text;
    import std.exception : enforce;
    import std.format.write : formattedWrite;

    auto w = appender!string();
    formattedWrite(w, fmt, val);
    enforce!AssertError(expected.canFind(w.data),
        text("expected one of `", expected, "`, result = `", w.data, "`"), fn, ln);
}
