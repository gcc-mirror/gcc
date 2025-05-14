// Written in the D programming language.

/*
   Helper functions for formatting floating point numbers.

   Copyright: Copyright The D Language Foundation 2019 -

   License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

   Authors: Bernhard Seckinger

   Source: $(PHOBOSSRC std/format/internal/floats.d)
 */

module std.format.internal.floats;

import std.format.spec : FormatSpec;

// wrapper for unittests
private auto printFloat(T, Char)(const(T) val, FormatSpec!Char f)
if (is(T == float) || is(T == double)
    || (is(T == real) && (T.mant_dig == double.mant_dig || T.mant_dig == 64)))
{
    import std.array : appender;
    auto w = appender!string();

    printFloat(w, val, f);
    return w.data;
}

/// Returns: whether `c` is a supported format specifier for floats
package(std.format) bool isFloatSpec(char c) nothrow @nogc pure @safe
{
    return c == 'a' || c == 'A'
           || c == 'e' || c == 'E'
           || c == 'f' || c == 'F'
           || c == 'g' || c == 'G';
}

package(std.format) void printFloat(Writer, T, Char)(auto ref Writer w, const(T) val, FormatSpec!Char f)
if (is(T == float) || is(T == double)
    || (is(T == real) && (T.mant_dig == double.mant_dig || T.mant_dig == 64)))
{
    import std.math.operations : extractBitpattern, FloatingPointBitpattern;

    auto bp = extractBitpattern(val);

    ulong mnt = bp.mantissa;
    int exp = bp.exponent;
    string sgn = bp.negative ? "-" : "";

    if (sgn == "" && f.flPlus) sgn = "+";
    if (sgn == "" && f.flSpace) sgn = " ";

    assert(isFloatSpec(f.spec), "unsupported format specifier");
    bool is_upper = f.spec == 'A' || f.spec == 'E' || f.spec=='F' || f.spec=='G';

    // special treatment for nan and inf
    if (exp == T.max_exp)
    {
        import std.format.internal.write : writeAligned;

        f.flZero = false;
        writeAligned(w, sgn, "", (mnt == 0) ? ( is_upper ? "INF" : "inf" ) : ( is_upper ? "NAN" : "nan" ), f);
        return;
    }

    final switch (f.spec)
    {
        case 'a': case 'A':
            printFloatA(w, val, f, sgn, exp, mnt, is_upper);
            break;
        case 'e': case 'E':
            printFloatE!false(w, val, f, sgn, exp, mnt, is_upper);
            break;
        case 'f': case 'F':
            printFloatF!false(w, val, f, sgn, exp, mnt, is_upper);
            break;
        case 'g': case 'G':
            printFloatG(w, val, f, sgn, exp, mnt, is_upper);
            break;
    }
}

private void printFloatA(Writer, T, Char)(auto ref Writer w, const(T) val,
    FormatSpec!Char f, string sgn, int exp, ulong mnt, bool is_upper)
if (is(T == float) || is(T == double)
    || (is(T == real) && (T.mant_dig == double.mant_dig || T.mant_dig == 64)))
{
    import std.algorithm.comparison : max;
    import std.format.internal.write : writeAligned, PrecisionType;

    char[3] prefix;
    if (sgn != "") prefix[0] = sgn[0];
    prefix[1] = '0';
    prefix[2] = is_upper ? 'X' : 'x';

    // print exponent
    if (mnt == 0)
    {
        if (f.precision == f.UNSPECIFIED)
            f.precision = 0;
        writeAligned(w, prefix[1 - sgn.length .. $], "0", ".", is_upper ? "P+0" : "p+0",
                     f, PrecisionType.fractionalDigits);
        return;
    }

    // save integer part
    char first = '0' + ((mnt >> (T.mant_dig - 1)) & 1);
    mnt &= (1L << (T.mant_dig - 1)) - 1;

    static if (is(T == float) || (is(T == real) && T.mant_dig == 64))
    {
        mnt <<= 1; // make mnt dividable by 4
        enum mant_len = T.mant_dig;
    }
    else
        enum mant_len = T.mant_dig - 1;
    static assert(mant_len % 4 == 0, "mantissa with wrong length");

    // print full mantissa
    char[(mant_len - 1) / 4 + 3] hex_mant;
    size_t hex_mant_pos = 2;
    size_t pos = mant_len;

    auto gap = 39 - 32 * is_upper;
    while (pos >= 4 && (mnt & (((1L << (pos - 1)) - 1) << 1) + 1) != 0)
    {
        pos -= 4;
        size_t tmp = (mnt >> pos) & 15;
        // For speed reasons the better readable
        // ... = tmp < 10 ? ('0' + tmp) : ((is_upper ? 'A' : 'a') + tmp - 10))
        // has been replaced with an expression without branches, doing the same
        hex_mant[hex_mant_pos++] = cast(char) (tmp + gap * ((tmp + 6) >> 4) + '0');
    }
    hex_mant[0] = first;
    hex_mant[1] = '.';

    if (f.precision == f.UNSPECIFIED)
        f.precision = cast(int) hex_mant_pos - 2;

    auto exp_sgn = exp >= 0 ? '+' : '-';
    if (exp < 0) exp = -exp;

    static if (is(T == real) && real.mant_dig == 64)
        enum max_exp_digits = 8;
    else static if (is(T == float))
        enum max_exp_digits = 5;
    else
        enum max_exp_digits = 6;

    char[max_exp_digits] exp_str;
    size_t exp_pos = max_exp_digits;

    do
    {
        exp_str[--exp_pos] = '0' + exp % 10;
        exp /= 10;
    } while (exp > 0);

    exp_str[--exp_pos] = exp_sgn;
    exp_str[--exp_pos] = is_upper ? 'P' : 'p';

    if (f.precision < hex_mant_pos - 2)
    {
        import std.format.internal.write : RoundingClass, round;

        RoundingClass rc;

        if (hex_mant[f.precision + 2] == '0')
            rc = RoundingClass.ZERO;
        else if (hex_mant[f.precision + 2] < '8')
            rc = RoundingClass.LOWER;
        else if (hex_mant[f.precision + 2] > '8')
            rc = RoundingClass.UPPER;
        else
            rc = RoundingClass.FIVE;

        if (rc == RoundingClass.ZERO || rc == RoundingClass.FIVE)
        {
            foreach (i;f.precision + 3 .. hex_mant_pos)
            {
                if (hex_mant[i] > '0')
                {
                    rc = rc == RoundingClass.ZERO ? RoundingClass.LOWER : RoundingClass.UPPER;
                    break;
                }
            }
        }

        hex_mant_pos = f.precision + 2;

        round(hex_mant, 0, hex_mant_pos, rc, sgn == "-", is_upper ? 'F' : 'f');
    }

    writeAligned(w, prefix[1 - sgn.length .. $], hex_mant[0 .. 1], hex_mant[1 .. hex_mant_pos],
                 exp_str[exp_pos .. $], f, PrecisionType.fractionalDigits);
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'a';
    assert(printFloat(float.nan, f) == "nan");
    assert(printFloat(-float.nan, f) == "-nan");
    assert(printFloat(float.infinity, f) == "inf");
    assert(printFloat(-float.infinity, f) == "-inf");
    assert(printFloat(0.0f, f) == "0x0p+0");
    assert(printFloat(-0.0f, f) == "-0x0p+0");

    assert(printFloat(double.nan, f) == "nan");
    assert(printFloat(-double.nan, f) == "-nan");
    assert(printFloat(double.infinity, f) == "inf");
    assert(printFloat(-double.infinity, f) == "-inf");
    assert(printFloat(0.0, f) == "0x0p+0");
    assert(printFloat(-0.0, f) == "-0x0p+0");

    static if (real.mant_dig > 64)
    {
        pragma(msg, "printFloat tests disabled because of unsupported `real` format");
    }
    else
    {
        assert(printFloat(real.nan, f) == "nan");
        assert(printFloat(-real.nan, f) == "-nan");
        assert(printFloat(real.infinity, f) == "inf");
        assert(printFloat(-real.infinity, f) == "-inf");
        assert(printFloat(0.0L, f) == "0x0p+0");
        assert(printFloat(-0.0L, f) == "-0x0p+0");
    }

    import std.math.operations : nextUp;

    assert(printFloat(nextUp(0.0f), f) == "0x0.000002p-126");
    assert(printFloat(float.epsilon, f) == "0x1p-23");
    assert(printFloat(float.min_normal, f) == "0x1p-126");
    assert(printFloat(float.max, f) == "0x1.fffffep+127");

    assert(printFloat(nextUp(0.0), f) == "0x0.0000000000001p-1022");
    assert(printFloat(double.epsilon, f) == "0x1p-52");
    assert(printFloat(double.min_normal, f) == "0x1p-1022");
    assert(printFloat(double.max, f) == "0x1.fffffffffffffp+1023");

    static if (real.mant_dig == 64)
    {
        assert(printFloat(nextUp(0.0L), f) == "0x0.0000000000000002p-16382");
        assert(printFloat(real.epsilon, f) == "0x1p-63");
        assert(printFloat(real.min_normal, f) == "0x1p-16382");
        assert(printFloat(real.max, f) == "0x1.fffffffffffffffep+16383");
    }

    import std.math.constants : E, PI, PI_2, PI_4, M_1_PI, M_2_PI, M_2_SQRTPI,
                                LN10, LN2, LOG2, LOG2E, LOG2T, LOG10E, SQRT2, SQRT1_2;

    assert(printFloat(cast(float) E, f) == "0x1.5bf0a8p+1");
    assert(printFloat(cast(float) PI, f) == "0x1.921fb6p+1");
    assert(printFloat(cast(float) PI_2, f) == "0x1.921fb6p+0");
    assert(printFloat(cast(float) PI_4, f) == "0x1.921fb6p-1");
    assert(printFloat(cast(float) M_1_PI, f) == "0x1.45f306p-2");
    assert(printFloat(cast(float) M_2_PI, f) == "0x1.45f306p-1");
    assert(printFloat(cast(float) M_2_SQRTPI, f) == "0x1.20dd76p+0");
    assert(printFloat(cast(float) LN10, f) == "0x1.26bb1cp+1");
    assert(printFloat(cast(float) LN2, f) == "0x1.62e43p-1");
    assert(printFloat(cast(float) LOG2, f) == "0x1.344136p-2");
    assert(printFloat(cast(float) LOG2E, f) == "0x1.715476p+0");
    assert(printFloat(cast(float) LOG2T, f) == "0x1.a934fp+1");
    assert(printFloat(cast(float) LOG10E, f) == "0x1.bcb7b2p-2");
    assert(printFloat(cast(float) SQRT2, f) == "0x1.6a09e6p+0");
    assert(printFloat(cast(float) SQRT1_2, f) == "0x1.6a09e6p-1");

    assert(printFloat(cast(double) E, f) == "0x1.5bf0a8b145769p+1");
    assert(printFloat(cast(double) PI, f) == "0x1.921fb54442d18p+1");
    assert(printFloat(cast(double) PI_2, f) == "0x1.921fb54442d18p+0");
    assert(printFloat(cast(double) PI_4, f) == "0x1.921fb54442d18p-1");
    assert(printFloat(cast(double) M_1_PI, f) == "0x1.45f306dc9c883p-2");
    assert(printFloat(cast(double) M_2_PI, f) == "0x1.45f306dc9c883p-1");
    assert(printFloat(cast(double) M_2_SQRTPI, f) == "0x1.20dd750429b6dp+0");
    assert(printFloat(cast(double) LN10, f) == "0x1.26bb1bbb55516p+1");
    assert(printFloat(cast(double) LN2, f) == "0x1.62e42fefa39efp-1");
    assert(printFloat(cast(double) LOG2, f) == "0x1.34413509f79ffp-2");
    assert(printFloat(cast(double) LOG2E, f) == "0x1.71547652b82fep+0");
    assert(printFloat(cast(double) LOG2T, f) == "0x1.a934f0979a371p+1");
    assert(printFloat(cast(double) LOG10E, f) == "0x1.bcb7b1526e50ep-2");
    assert(printFloat(cast(double) SQRT2, f) == "0x1.6a09e667f3bcdp+0");
    assert(printFloat(cast(double) SQRT1_2, f) == "0x1.6a09e667f3bcdp-1");

    static if (real.mant_dig == 64)
    {
        assert(printFloat(E, f) == "0x1.5bf0a8b145769536p+1");
        assert(printFloat(PI, f) == "0x1.921fb54442d1846ap+1");
        assert(printFloat(PI_2, f) == "0x1.921fb54442d1846ap+0");
        assert(printFloat(PI_4, f) == "0x1.921fb54442d1846ap-1");
        assert(printFloat(M_1_PI, f) == "0x1.45f306dc9c882a54p-2");
        assert(printFloat(M_2_PI, f) == "0x1.45f306dc9c882a54p-1");
        assert(printFloat(M_2_SQRTPI, f) == "0x1.20dd750429b6d11ap+0");
        assert(printFloat(LN10, f) == "0x1.26bb1bbb5551582ep+1");
        assert(printFloat(LN2, f) == "0x1.62e42fefa39ef358p-1");
        assert(printFloat(LOG2, f) == "0x1.34413509f79fef32p-2");
        assert(printFloat(LOG2E, f) == "0x1.71547652b82fe178p+0");
        assert(printFloat(LOG2T, f) == "0x1.a934f0979a3715fcp+1");
        assert(printFloat(LOG10E, f) == "0x1.bcb7b1526e50e32ap-2");
        assert(printFloat(SQRT2, f) == "0x1.6a09e667f3bcc908p+0");
        assert(printFloat(SQRT1_2, f) == "0x1.6a09e667f3bcc908p-1");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'a';
    f.precision = 3;

    assert(printFloat(1.0f, f) == "0x1.000p+0");
    assert(printFloat(3.3f, f) == "0x1.a66p+1");
    assert(printFloat(2.9f, f) == "0x1.733p+1");

    assert(printFloat(1.0, f) == "0x1.000p+0");
    assert(printFloat(3.3, f) == "0x1.a66p+1");
    assert(printFloat(2.9, f) == "0x1.733p+1");

    static if (real.mant_dig == 64)
    {
        assert(printFloat(1.0L, f) == "0x1.000p+0");
        assert(printFloat(3.3L, f) == "0x1.a66p+1");
        assert(printFloat(2.9L, f) == "0x1.733p+1");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'a';
    f.precision = 0;

    assert(printFloat(1.0f, f) == "0x1p+0");
    assert(printFloat(3.3f, f) == "0x2p+1");
    assert(printFloat(2.9f, f) == "0x1p+1");

    assert(printFloat(1.0, f) == "0x1p+0");
    assert(printFloat(3.3, f) == "0x2p+1");
    assert(printFloat(2.9, f) == "0x1p+1");

    static if (real.mant_dig == 64)
    {
        assert(printFloat(1.0L, f) == "0x1p+0");
        assert(printFloat(3.3L, f) == "0x2p+1");
        assert(printFloat(2.9L, f) == "0x1p+1");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'a';
    f.precision = 0;
    f.flHash = true;

    assert(printFloat(1.0f, f) == "0x1.p+0");
    assert(printFloat(3.3f, f) == "0x2.p+1");
    assert(printFloat(2.9f, f) == "0x1.p+1");

    assert(printFloat(1.0, f) == "0x1.p+0");
    assert(printFloat(3.3, f) == "0x2.p+1");
    assert(printFloat(2.9, f) == "0x1.p+1");

    static if (real.mant_dig == 64)
    {
        assert(printFloat(1.0L, f) == "0x1.p+0");
        assert(printFloat(3.3L, f) == "0x2.p+1");
        assert(printFloat(2.9L, f) == "0x1.p+1");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'a';
    f.width = 22;

    assert(printFloat(1.0f, f) == "                0x1p+0");
    assert(printFloat(3.3f, f) == "         0x1.a66666p+1");
    assert(printFloat(2.9f, f) == "         0x1.733334p+1");

    assert(printFloat(1.0, f) == "                0x1p+0");
    assert(printFloat(3.3, f) == "  0x1.a666666666666p+1");
    assert(printFloat(2.9, f) == "  0x1.7333333333333p+1");

    static if (real.mant_dig == 64)
    {
        f.width = 25;
        assert(printFloat(1.0L, f) == "                   0x1p+0");
        assert(printFloat(3.3L, f) == "  0x1.a666666666666666p+1");
        assert(printFloat(2.9L, f) == "  0x1.7333333333333334p+1");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'a';
    f.width = 22;
    f.flDash = true;

    assert(printFloat(1.0f, f) == "0x1p+0                ");
    assert(printFloat(3.3f, f) == "0x1.a66666p+1         ");
    assert(printFloat(2.9f, f) == "0x1.733334p+1         ");

    assert(printFloat(1.0, f) == "0x1p+0                ");
    assert(printFloat(3.3, f) == "0x1.a666666666666p+1  ");
    assert(printFloat(2.9, f) == "0x1.7333333333333p+1  ");

    static if (real.mant_dig == 64)
    {
        f.width = 25;
        assert(printFloat(1.0L, f) == "0x1p+0                   ");
        assert(printFloat(3.3L, f) == "0x1.a666666666666666p+1  ");
        assert(printFloat(2.9L, f) == "0x1.7333333333333334p+1  ");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'a';
    f.width = 22;
    f.flZero = true;

    assert(printFloat(1.0f, f) == "0x00000000000000001p+0");
    assert(printFloat(3.3f, f) == "0x0000000001.a66666p+1");
    assert(printFloat(2.9f, f) == "0x0000000001.733334p+1");

    assert(printFloat(1.0, f) == "0x00000000000000001p+0");
    assert(printFloat(3.3, f) == "0x001.a666666666666p+1");
    assert(printFloat(2.9, f) == "0x001.7333333333333p+1");

    static if (real.mant_dig == 64)
    {
        f.width = 25;
        assert(printFloat(1.0L, f) == "0x00000000000000000001p+0");
        assert(printFloat(3.3L, f) == "0x001.a666666666666666p+1");
        assert(printFloat(2.9L, f) == "0x001.7333333333333334p+1");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'a';
    f.width = 22;
    f.flPlus = true;

    assert(printFloat(1.0f, f) == "               +0x1p+0");
    assert(printFloat(3.3f, f) == "        +0x1.a66666p+1");
    assert(printFloat(2.9f, f) == "        +0x1.733334p+1");

    assert(printFloat(1.0, f) == "               +0x1p+0");
    assert(printFloat(3.3, f) == " +0x1.a666666666666p+1");
    assert(printFloat(2.9, f) == " +0x1.7333333333333p+1");

    static if (real.mant_dig == 64)
    {
        f.width = 25;
        assert(printFloat(1.0L, f) == "                  +0x1p+0");
        assert(printFloat(3.3L, f) == " +0x1.a666666666666666p+1");
        assert(printFloat(2.9L, f) == " +0x1.7333333333333334p+1");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'a';
    f.width = 22;
    f.flDash = true;
    f.flSpace = true;

    assert(printFloat(1.0f, f) == " 0x1p+0               ");
    assert(printFloat(3.3f, f) == " 0x1.a66666p+1        ");
    assert(printFloat(2.9f, f) == " 0x1.733334p+1        ");

    assert(printFloat(1.0, f) == " 0x1p+0               ");
    assert(printFloat(3.3, f) == " 0x1.a666666666666p+1 ");
    assert(printFloat(2.9, f) == " 0x1.7333333333333p+1 ");

    static if (real.mant_dig == 64)
    {
        f.width = 25;
        assert(printFloat(1.0L, f) == " 0x1p+0                  ");
        assert(printFloat(3.3L, f) == " 0x1.a666666666666666p+1 ");
        assert(printFloat(2.9L, f) == " 0x1.7333333333333334p+1 ");
    }
}

@safe unittest
{
    import std.math.hardware; // cannot be selective, because FloatingPointControl might not be defined

    // std.math's FloatingPointControl isn't available on all target platforms
    static if (is(FloatingPointControl))
    {
        FloatingPointControl fpctrl;

        auto f = FormatSpec!dchar("");
        f.spec = 'a';
        f.precision = 1;

        fpctrl.rounding = FloatingPointControl.roundToNearest;

        /* tiesAwayFromZero currently not supported
         assert(printFloat(0x1.18p0,  f) == "0x1.2p+0");
         assert(printFloat(0x1.28p0,  f) == "0x1.3p+0");
         assert(printFloat(0x1.1ap0,  f) == "0x1.2p+0");
         assert(printFloat(0x1.16p0,  f) == "0x1.1p+0");
         assert(printFloat(0x1.10p0,  f) == "0x1.1p+0");
         assert(printFloat(-0x1.18p0, f) == "-0x1.2p+0");
         assert(printFloat(-0x1.28p0, f) == "-0x1.3p+0");
         assert(printFloat(-0x1.1ap0, f) == "-0x1.2p+0");
         assert(printFloat(-0x1.16p0, f) == "-0x1.1p+0");
         assert(printFloat(-0x1.10p0, f) == "-0x1.1p+0");
         */

        assert(printFloat(0x1.18p0,  f) == "0x1.2p+0");
        assert(printFloat(0x1.28p0,  f) == "0x1.2p+0");
        assert(printFloat(0x1.1ap0,  f) == "0x1.2p+0");
        assert(printFloat(0x1.16p0,  f) == "0x1.1p+0");
        assert(printFloat(0x1.10p0,  f) == "0x1.1p+0");
        assert(printFloat(-0x1.18p0, f) == "-0x1.2p+0");
        assert(printFloat(-0x1.28p0, f) == "-0x1.2p+0");
        assert(printFloat(-0x1.1ap0, f) == "-0x1.2p+0");
        assert(printFloat(-0x1.16p0, f) == "-0x1.1p+0");
        assert(printFloat(-0x1.10p0, f) == "-0x1.1p+0");

        fpctrl.rounding = FloatingPointControl.roundToZero;

        assert(printFloat(0x1.18p0,  f) == "0x1.1p+0");
        assert(printFloat(0x1.28p0,  f) == "0x1.2p+0");
        assert(printFloat(0x1.1ap0,  f) == "0x1.1p+0");
        assert(printFloat(0x1.16p0,  f) == "0x1.1p+0");
        assert(printFloat(0x1.10p0,  f) == "0x1.1p+0");
        assert(printFloat(-0x1.18p0, f) == "-0x1.1p+0");
        assert(printFloat(-0x1.28p0, f) == "-0x1.2p+0");
        assert(printFloat(-0x1.1ap0, f) == "-0x1.1p+0");
        assert(printFloat(-0x1.16p0, f) == "-0x1.1p+0");
        assert(printFloat(-0x1.10p0, f) == "-0x1.1p+0");

        fpctrl.rounding = FloatingPointControl.roundUp;

        assert(printFloat(0x1.18p0,  f) == "0x1.2p+0");
        assert(printFloat(0x1.28p0,  f) == "0x1.3p+0");
        assert(printFloat(0x1.1ap0,  f) == "0x1.2p+0");
        assert(printFloat(0x1.16p0,  f) == "0x1.2p+0");
        assert(printFloat(0x1.10p0,  f) == "0x1.1p+0");
        assert(printFloat(-0x1.18p0, f) == "-0x1.1p+0");
        assert(printFloat(-0x1.28p0, f) == "-0x1.2p+0");
        assert(printFloat(-0x1.1ap0, f) == "-0x1.1p+0");
        assert(printFloat(-0x1.16p0, f) == "-0x1.1p+0");
        assert(printFloat(-0x1.10p0, f) == "-0x1.1p+0");

        fpctrl.rounding = FloatingPointControl.roundDown;

        assert(printFloat(0x1.18p0,  f) == "0x1.1p+0");
        assert(printFloat(0x1.28p0,  f) == "0x1.2p+0");
        assert(printFloat(0x1.1ap0,  f) == "0x1.1p+0");
        assert(printFloat(0x1.16p0,  f) == "0x1.1p+0");
        assert(printFloat(0x1.10p0,  f) == "0x1.1p+0");
        assert(printFloat(-0x1.18p0, f) == "-0x1.2p+0");
        assert(printFloat(-0x1.28p0, f) == "-0x1.3p+0");
        assert(printFloat(-0x1.1ap0, f) == "-0x1.2p+0");
        assert(printFloat(-0x1.16p0, f) == "-0x1.2p+0");
        assert(printFloat(-0x1.10p0, f) == "-0x1.1p+0");
    }
}

// for 100% coverage
@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'a';
    f.precision = 3;

    assert(printFloat(0x1.19f81p0, f) == "0x1.1a0p+0");
    assert(printFloat(0x1.19f01p0, f) == "0x1.19fp+0");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'A';
    f.precision = 3;

    assert(printFloat(0x1.19f81p0, f) == "0X1.1A0P+0");
    assert(printFloat(0x1.19f01p0, f) == "0X1.19FP+0");
}

private void printFloatE(bool g, Writer, T, Char)(auto ref Writer w, const(T) val,
    FormatSpec!Char f, string sgn, int exp, ulong mnt, bool is_upper)
if (is(T == float) || is(T == double)
    || (is(T == real) && (T.mant_dig == double.mant_dig || T.mant_dig == 64)))
{
    import std.format.internal.write : writeAligned, PrecisionType, RoundingClass, round;

    static if (!g)
    {
        if (f.precision == f.UNSPECIFIED)
            f.precision = 6;
    }

    // special treatment for 0.0
    if (mnt == 0)
    {
        static if (g)
            writeAligned(w, sgn, "0", ".", "", f, PrecisionType.allDigits);
        else
            writeAligned(w, sgn, "0", ".", is_upper ? "E+00" : "e+00", f, PrecisionType.fractionalDigits);
        return;
    }

    char[T.mant_dig + T.max_exp] dec_buf;
    char[T.max_10_exp.stringof.length + 2] exp_buf;

    int final_exp = 0;

    RoundingClass rc;

    // Depending on exp, we will use one of three algorithms:
    //
    // Algorithm A: For large exponents (exp >= T.mant_dig)
    // Algorithm B: For small exponents (exp < T.mant_dig - 61)
    // Algorithm C: For exponents close to 0.
    //
    // Algorithm A:
    //   The number to print looks like this: mantissa followed by several zeros.
    //
    //   We know, that there is no fractional part, so we can just use integer division,
    //   consecutivly dividing by 10 and writing down the remainder from right to left.
    //   Unfortunately the integer is too large to fit in an ulong, so we use something
    //   like BigInt: An array of ulongs. We only use 60 bits of that ulongs, because
    //   this simplifies (and speeds up) the division to come.
    //
    //   For the division we use integer division with reminder for each ulong and put
    //   the reminder of each step in the first 4 bits of ulong of the next step (think of
    //   long division for the rationale behind this). The final reminder is the next
    //   digit (from right to left).
    //
    //   This results in the output we would have for the %f specifier. We now adjust this
    //   for %e: First we calculate the place, where the exponent should be printed, filling
    //   up with zeros if needed and second we move the leftmost digit one to the left
    //   and inserting a dot.
    //
    //   After that we decide on the rounding type, using the digits right of the position,
    //   where the exponent will be printed (currently they are still there, but will be
    //   overwritten later).
    //
    // Algorithm B:
    //   The number to print looks like this: zero dot several zeros followed by the mantissa
    //
    //   We know, that the number has no integer part. The algorithm consecutivly multiplies
    //   by 10. The integer part (rounded down) after the multiplication is the next digit
    //   (from left to right). This integer part is removed after each step.
    //   Again, the number is represented as an array of ulongs, with only 60 bits used of
    //   every ulong.
    //
    //   For the multiplication we use normal integer multiplication, which can result in digits
    //   in the uppermost 4 bits. These 4 digits are the carry which is added to the result
    //   of the next multiplication and finally the last carry is the next digit.
    //
    //   Other than for the %f specifier, this multiplication is splitted into two almost
    //   identical parts. The first part lasts as long as we find zeros. We need to do this
    //   to calculate the correct exponent.
    //
    //   The second part will stop, when only zeros remain or when we've got enough digits
    //   for the requested precision. In the second case, we have to find out, which rounding
    //   we have. Aside from special cases we do this by calculating one more digit.
    //
    // Algorithm C:
    //   This time, we know, that the integral part and the fractional part each fit into a
    //   ulong. The mantissa might be partially in both parts or completely in the fractional
    //   part.
    //
    //   We first calculate the integral part by consecutive division by 10. Depending on the
    //   precision this might result in more digits, than we need. In that case we calculate
    //   the position of the exponent and the rounding type.
    //
    //   If there is no integral part, we need to find the first non zero digit. We do this by
    //   consecutive multiplication by 10, saving the first non zero digit followed by a dot.
    //
    //   In either case, we continue filling up with the fractional part until we have enough
    //   digits. If still necessary, we decide the rounding type, mainly by looking at the
    //   next digit.

    size_t right = 1;
    size_t start = 1;
    size_t left = 1;

    static if (is(T == real) && real.mant_dig == 64)
    {
        enum small_bound = 0;
        enum max_buf = 275;
    }
    else
    {
        enum small_bound = T.mant_dig - 61;
        static if (is(T == float))
            enum max_buf = 4;
        else
            enum max_buf = 18;
    }

    ulong[max_buf] bigbuf;
    if (exp >= T.mant_dig)
    {
        start = left = right = dec_buf.length;

        // large number without fractional digits
        //
        // As this number does not fit in a ulong, we use an array of ulongs. We only use 60 of the 64 bits,
        // because this makes it much more easy to implement the division by 10.
        int count = exp / 60 + 1;

        // only the first few ulongs contain the mantiassa. The rest are zeros.
        int lower = 60 - (exp - T.mant_dig + 1) % 60;

        static if (is(T == real) && real.mant_dig == 64)
        {
            // for x87 reals, the lowest ulong may contain more than 60 bits,
            // because the mantissa is 63 (>60) bits long
            // therefore we need one ulong less
            if (lower <= 3) count--;
        }

        // saved in big endian format
        ulong[] mybig = bigbuf[0 .. count];

        if (lower < T.mant_dig)
        {
            mybig[0] = mnt >> lower;
            mybig[1] = (mnt & ((1L << lower) - 1)) << 60 - lower;
        }
        else
            mybig[0] = (mnt & ((1L << lower) - 1)) << 60 - lower;

        // Generation of digits by consecutive division with reminder by 10.
        int msu = 0; // Most significant ulong; when it get's zero, we can ignore it further on
        while (msu < count - 1 || mybig[$ - 1] != 0)
        {
            ulong mod = 0;
            foreach (i;msu .. count)
            {
                mybig[i] |= mod << 60;
                mod = mybig[i] % 10;
                mybig[i] /= 10;
            }
            if (mybig[msu] == 0)
                ++msu;

            dec_buf[--left] = cast(byte) ('0' + mod);
            ++final_exp;
        }
        --final_exp;

        static if (g)
            start = left + f.precision;
        else
            start = left + f.precision + 1;

        // move leftmost digit one more left and add dot between
        dec_buf[left - 1] = dec_buf[left];
        dec_buf[left] = '.';
        --left;

        // rounding type
        if (start >= right)
            rc = RoundingClass.ZERO;
        else if (dec_buf[start] != '0' && dec_buf[start] != '5')
            rc = dec_buf[start] > '5' ? RoundingClass.UPPER : RoundingClass.LOWER;
        else
        {
            rc = dec_buf[start] == '5' ? RoundingClass.FIVE : RoundingClass.ZERO;
            foreach (i; start + 1 .. right)
                if (dec_buf[i] > '0')
                {
                    rc = rc == RoundingClass.FIVE ? RoundingClass.UPPER : RoundingClass.LOWER;
                    break;
                }
        }

        if (start < right) right = start;
    }
    else if (exp < small_bound)
    {
        // small number without integer digits
        //
        // Again this number does not fit in a ulong and we use an array of ulongs. And again we
        // only use 60 bits, because this simplifies the multiplication by 10.
        int count = (T.mant_dig - exp - 2) / 60 + 1;

        // saved in little endian format
        ulong[] mybig = bigbuf[0 .. count];

        // only the last few ulongs contain the mantiassa. Because of little endian
        // format these are the ulongs at index 0 and 1 (and 2 in case of x87 reals).
        // The rest are zeros.
        int upper = 60 - (-exp - 1) % 60;

        static if (is(T == real) && real.mant_dig == 64)
        {
            if (upper < 4)
            {
                mybig[0] = (mnt & ((1L << (4 - upper)) - 1)) << 56 + upper;
                mybig[1] = (mnt >> (4 - upper)) & ((1L << 60) - 1);
                mybig[2] = mnt >> 64 - upper;
            }
            else
            {
                mybig[0] = (mnt & ((1L << (T.mant_dig - upper)) - 1)) << 60 - (T.mant_dig - upper);
                mybig[1] = mnt >> (T.mant_dig - upper);
            }
        }
        else
        {
            if (upper < T.mant_dig)
            {
                mybig[0] = (mnt & ((1L << (T.mant_dig - upper)) - 1)) << 60 - (T.mant_dig - upper);
                mybig[1] = mnt >> (T.mant_dig - upper);
            }
            else
                mybig[0] = mnt << (upper - T.mant_dig);
        }

        int lsu = 0; // Least significant ulong; when it get's zero, we can ignore it further on

        // adding zeros, until we reach first nonzero
        while (lsu < count - 1 || mybig[$ - 1]!=0)
        {
            ulong over = 0;
            foreach (i; lsu .. count)
            {
                mybig[i] = mybig[i] * 10 + over;
                over = mybig[i] >> 60;
                mybig[i] &= (1L << 60) - 1;
            }
            if (mybig[lsu] == 0)
                ++lsu;
            --final_exp;

            if (over != 0)
            {
                dec_buf[right++] = cast(byte) ('0' + over);
                dec_buf[right++] = '.';
                break;
            }
        }

        // adding more digits
        static if (g)
            start = right - 1;
        else
            start = right;
        while ((lsu < count - 1 || mybig[$ - 1] != 0) && right - start < f.precision)
        {
            ulong over = 0;
            foreach (i;lsu .. count)
            {
                mybig[i] = mybig[i] * 10 + over;
                over = mybig[i] >> 60;
                mybig[i] &= (1L << 60) - 1;
            }
            if (mybig[lsu] == 0)
                ++lsu;

            dec_buf[right++] = cast(byte) ('0' + over);
        }

        // rounding type
        if (lsu >= count - 1 && mybig[count - 1] == 0)
            rc = RoundingClass.ZERO;
        else if (lsu == count - 1 && mybig[lsu] == 1L << 59)
            rc = RoundingClass.FIVE;
        else
        {
            ulong over = 0;
            foreach (i;lsu .. count)
            {
                mybig[i] = mybig[i] * 10 + over;
                over = mybig[i] >> 60;
                mybig[i] &= (1L << 60) - 1;
            }
            rc = over >= 5 ? RoundingClass.UPPER : RoundingClass.LOWER;
        }
    }
    else
    {
        // medium sized number, probably with integer and fractional digits
        // this is fastest, because both parts fit into a ulong each
        ulong int_part = mnt >> (T.mant_dig - 1 - exp);
        ulong frac_part = mnt & ((1L << (T.mant_dig - 1 - exp)) - 1);

        // for x87 reals the mantiassa might be up to 3 bits too long
        // we need to save these bits as a tail and handle this separately
        static if (is(T == real) && real.mant_dig == 64)
        {
            ulong tail = 0;
            ulong tail_length = 0;
            if (exp < 3)
            {
                tail = frac_part & ((1L << (3 - exp)) - 1);
                tail_length = 3 - exp;
                frac_part >>= 3 - exp;
                exp = 3;
            }
        }

        start = 0;

        // could we already decide on the rounding mode in the integer part?
        bool found = false;

        if (int_part > 0)
        {
            import core.bitop : bsr;
            left = right = int_part.bsr * 100 / 332 + 4;

            // integer part, if there is something to print
            while (int_part >= 10)
            {
                dec_buf[--left] = '0' + (int_part % 10);
                int_part /= 10;
                ++final_exp;
                ++start;
            }

            dec_buf[--left] = '.';
            dec_buf[--left] = cast(byte) ('0' + int_part);

            static if (g)
                auto limit = f.precision + 1;
            else
                auto limit = f.precision + 2;

            if (right - left > limit)
            {
                auto old_right = right;
                right = left + limit;

                if (dec_buf[right] == '5' || dec_buf[right] == '0')
                {
                    rc = dec_buf[right] == '5' ? RoundingClass.FIVE : RoundingClass.ZERO;
                    if (frac_part != 0)
                        rc = rc == RoundingClass.FIVE ? RoundingClass.UPPER : RoundingClass.LOWER;
                    else
                        foreach (i;right + 1 .. old_right)
                            if (dec_buf[i] > '0')
                            {
                                rc = rc == RoundingClass.FIVE ? RoundingClass.UPPER : RoundingClass.LOWER;
                                break;
                            }
                }
                else
                    rc = dec_buf[right] > '5' ? RoundingClass.UPPER : RoundingClass.LOWER;
                found = true;
            }
        }
        else
        {
            // fractional part, skipping leading zeros
            while (frac_part != 0)
            {
                --final_exp;
                frac_part *= 10;
                static if (is(T == real) && real.mant_dig == 64)
                {
                    if (tail_length > 0)
                    {
                        // together this is *= 10;
                        tail *= 5;
                        tail_length--;

                        frac_part += tail >> tail_length;
                        if (tail_length > 0)
                            tail &= (1L << tail_length) - 1;
                    }
                }
                auto tmp = frac_part >> (T.mant_dig - 1 - exp);
                frac_part &= ((1L << (T.mant_dig - 1 - exp)) - 1);
                if (tmp > 0)
                {
                    dec_buf[right++] = cast(byte) ('0' + tmp);
                    dec_buf[right++] = '.';
                    break;
                }
            }

            rc = RoundingClass.ZERO;
        }

        static if (g)
            size_t limit = f.precision - 1;
        else
            size_t limit = f.precision;

        // the fractional part after the zeros
        while (frac_part != 0 && start < limit)
        {
            frac_part *= 10;
            static if (is(T == real) && real.mant_dig == 64)
            {
                if (tail_length > 0)
                {
                    // together this is *= 10;
                    tail *= 5;
                    tail_length--;

                    frac_part += tail >> tail_length;
                    if (tail_length > 0)
                        tail &= (1L << tail_length) - 1;
                }
            }
            dec_buf[right++] = cast(byte) ('0' + (frac_part >> (T.mant_dig - 1 - exp)));
            frac_part &= ((1L << (T.mant_dig - 1 - exp)) - 1);
            ++start;
        }

        static if (g)
            limit = right - left - 1;
        else
            limit = start;

        // rounding mode, if not allready known
        if (frac_part != 0 && !found)
        {
            frac_part *= 10;
            auto nextDigit = frac_part >> (T.mant_dig - 1 - exp);
            frac_part &= ((1L << (T.mant_dig - 1 - exp)) - 1);

            if (nextDigit == 5 && frac_part == 0)
                rc = RoundingClass.FIVE;
            else if (nextDigit >= 5)
                rc = RoundingClass.UPPER;
            else
                rc = RoundingClass.LOWER;
        }
    }

    if (round(dec_buf, left, right, rc, sgn == "-"))
    {
        left--;
        right--;
        dec_buf[left + 2] = dec_buf[left + 1];
        dec_buf[left + 1] = '.';
        final_exp++;
    }

    // printing exponent
    auto neg = final_exp < 0;
    if (neg) final_exp = -final_exp;

    size_t exp_pos = exp_buf.length;

    do
    {
        exp_buf[--exp_pos] = '0' + final_exp%10;
        final_exp /= 10;
    } while (final_exp > 0);
    if (exp_buf.length - exp_pos == 1)
        exp_buf[--exp_pos] = '0';
    exp_buf[--exp_pos] = neg ? '-' : '+';
    exp_buf[--exp_pos] = is_upper ? 'E' : 'e';

    while (right > left + 1 && dec_buf[right - 1] == '0') right--;

    if (right == left + 1)
        dec_buf[right++] = '.';

    static if (g)
        writeAligned(w, sgn, dec_buf[left .. left + 1], dec_buf[left + 1 .. right],
                     exp_buf[exp_pos .. $], f, PrecisionType.allDigits);
    else
        writeAligned(w, sgn, dec_buf[left .. left + 1], dec_buf[left + 1 .. right],
                     exp_buf[exp_pos .. $], f, PrecisionType.fractionalDigits);
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'e';
    assert(printFloat(float.nan, f) == "nan");
    assert(printFloat(-float.nan, f) == "-nan");
    assert(printFloat(float.infinity, f) == "inf");
    assert(printFloat(-float.infinity, f) == "-inf");
    assert(printFloat(0.0f, f) == "0.000000e+00");
    assert(printFloat(-0.0f, f) == "-0.000000e+00");
    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "9.999946e-41");
    assert(printFloat(cast(float) -1e-40, f) == "-9.999946e-41");
    assert(printFloat(1e-30f, f) == "1.000000e-30");
    assert(printFloat(-1e-30f, f) == "-1.000000e-30");
    assert(printFloat(1e-10f, f) == "1.000000e-10");
    assert(printFloat(-1e-10f, f) == "-1.000000e-10");
    assert(printFloat(0.1f, f) == "1.000000e-01");
    assert(printFloat(-0.1f, f) == "-1.000000e-01");
    assert(printFloat(10.0f, f) == "1.000000e+01");
    assert(printFloat(-10.0f, f) == "-1.000000e+01");
    assert(printFloat(1e30f, f) == "1.000000e+30");
    assert(printFloat(-1e30f, f) == "-1.000000e+30");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "1.401298e-45");
    assert(printFloat(nextDown(-0.0f), f) == "-1.401298e-45");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'e';
    f.width = 20;
    f.precision = 10;

    assert(printFloat(float.nan, f) == "                 nan");
    assert(printFloat(-float.nan, f) == "                -nan");
    assert(printFloat(float.infinity, f) == "                 inf");
    assert(printFloat(-float.infinity, f) == "                -inf");
    assert(printFloat(0.0f, f) == "    0.0000000000e+00");
    assert(printFloat(-0.0f, f) == "   -0.0000000000e+00");
    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "    9.9999461011e-41");
    assert(printFloat(cast(float) -1e-40, f) == "   -9.9999461011e-41");
    assert(printFloat(1e-30f, f) == "    1.0000000032e-30");
    assert(printFloat(-1e-30f, f) == "   -1.0000000032e-30");
    assert(printFloat(1e-10f, f) == "    1.0000000134e-10");
    assert(printFloat(-1e-10f, f) == "   -1.0000000134e-10");
    assert(printFloat(0.1f, f) == "    1.0000000149e-01");
    assert(printFloat(-0.1f, f) == "   -1.0000000149e-01");
    assert(printFloat(10.0f, f) == "    1.0000000000e+01");
    assert(printFloat(-10.0f, f) == "   -1.0000000000e+01");
    assert(printFloat(1e30f, f) == "    1.0000000150e+30");
    assert(printFloat(-1e30f, f) == "   -1.0000000150e+30");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "    1.4012984643e-45");
    assert(printFloat(nextDown(-0.0f), f) == "   -1.4012984643e-45");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'e';
    f.width = 20;
    f.precision = 10;
    f.flDash = true;

    assert(printFloat(float.nan, f) == "nan                 ");
    assert(printFloat(-float.nan, f) == "-nan                ");
    assert(printFloat(float.infinity, f) == "inf                 ");
    assert(printFloat(-float.infinity, f) == "-inf                ");
    assert(printFloat(0.0f, f) == "0.0000000000e+00    ");
    assert(printFloat(-0.0f, f) == "-0.0000000000e+00   ");
    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "9.9999461011e-41    ");
    assert(printFloat(cast(float) -1e-40, f) == "-9.9999461011e-41   ");
    assert(printFloat(1e-30f, f) == "1.0000000032e-30    ");
    assert(printFloat(-1e-30f, f) == "-1.0000000032e-30   ");
    assert(printFloat(1e-10f, f) == "1.0000000134e-10    ");
    assert(printFloat(-1e-10f, f) == "-1.0000000134e-10   ");
    assert(printFloat(0.1f, f) == "1.0000000149e-01    ");
    assert(printFloat(-0.1f, f) == "-1.0000000149e-01   ");
    assert(printFloat(10.0f, f) == "1.0000000000e+01    ");
    assert(printFloat(-10.0f, f) == "-1.0000000000e+01   ");
    assert(printFloat(1e30f, f) == "1.0000000150e+30    ");
    assert(printFloat(-1e30f, f) == "-1.0000000150e+30   ");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "1.4012984643e-45    ");
    assert(printFloat(nextDown(-0.0f), f) == "-1.4012984643e-45   ");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'e';
    f.width = 20;
    f.precision = 10;
    f.flZero = true;

    assert(printFloat(float.nan, f) == "                 nan");
    assert(printFloat(-float.nan, f) == "                -nan");
    assert(printFloat(float.infinity, f) == "                 inf");
    assert(printFloat(-float.infinity, f) == "                -inf");
    assert(printFloat(0.0f, f) == "00000.0000000000e+00");
    assert(printFloat(-0.0f, f) == "-0000.0000000000e+00");
    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "00009.9999461011e-41");
    assert(printFloat(cast(float) -1e-40, f) == "-0009.9999461011e-41");
    assert(printFloat(1e-30f, f) == "00001.0000000032e-30");
    assert(printFloat(-1e-30f, f) == "-0001.0000000032e-30");
    assert(printFloat(1e-10f, f) == "00001.0000000134e-10");
    assert(printFloat(-1e-10f, f) == "-0001.0000000134e-10");
    assert(printFloat(0.1f, f) == "00001.0000000149e-01");
    assert(printFloat(-0.1f, f) == "-0001.0000000149e-01");
    assert(printFloat(10.0f, f) == "00001.0000000000e+01");
    assert(printFloat(-10.0f, f) == "-0001.0000000000e+01");
    assert(printFloat(1e30f, f) == "00001.0000000150e+30");
    assert(printFloat(-1e30f, f) == "-0001.0000000150e+30");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "00001.4012984643e-45");
    assert(printFloat(nextDown(-0.0f), f) == "-0001.4012984643e-45");
}

@safe unittest
{
    import std.math.hardware; // cannot be selective, because FloatingPointControl might not be defined

    // std.math's FloatingPointControl isn't available on all target platforms
    static if (is(FloatingPointControl))
    {
        FloatingPointControl fpctrl;

        auto f = FormatSpec!dchar("");
        f.spec = 'e';
        f.precision = 1;

        fpctrl.rounding = FloatingPointControl.roundToNearest;

        /*
        assert(printFloat(11.5f, f) == "1.2e+01");
        assert(printFloat(12.5f, f) == "1.3e+01");
        assert(printFloat(11.7f, f) == "1.2e+01");
        assert(printFloat(11.3f, f) == "1.1e+01");
        assert(printFloat(11.0f, f) == "1.1e+01");
        assert(printFloat(-11.5f, f) == "-1.2e+01");
        assert(printFloat(-12.5f, f) == "-1.3e+01");
        assert(printFloat(-11.7f, f) == "-1.2e+01");
        assert(printFloat(-11.3f, f) == "-1.1e+01");
        assert(printFloat(-11.0f, f) == "-1.1e+01");
         */

        assert(printFloat(11.5f, f) == "1.2e+01");
        assert(printFloat(12.5f, f) == "1.2e+01");
        assert(printFloat(11.7f, f) == "1.2e+01");
        assert(printFloat(11.3f, f) == "1.1e+01");
        assert(printFloat(11.0f, f) == "1.1e+01");
        assert(printFloat(-11.5f, f) == "-1.2e+01");
        assert(printFloat(-12.5f, f) == "-1.2e+01");
        assert(printFloat(-11.7f, f) == "-1.2e+01");
        assert(printFloat(-11.3f, f) == "-1.1e+01");
        assert(printFloat(-11.0f, f) == "-1.1e+01");

        fpctrl.rounding = FloatingPointControl.roundToZero;

        assert(printFloat(11.5f, f) == "1.1e+01");
        assert(printFloat(12.5f, f) == "1.2e+01");
        assert(printFloat(11.7f, f) == "1.1e+01");
        assert(printFloat(11.3f, f) == "1.1e+01");
        assert(printFloat(11.0f, f) == "1.1e+01");
        assert(printFloat(-11.5f, f) == "-1.1e+01");
        assert(printFloat(-12.5f, f) == "-1.2e+01");
        assert(printFloat(-11.7f, f) == "-1.1e+01");
        assert(printFloat(-11.3f, f) == "-1.1e+01");
        assert(printFloat(-11.0f, f) == "-1.1e+01");

        fpctrl.rounding = FloatingPointControl.roundUp;

        assert(printFloat(11.5f, f) == "1.2e+01");
        assert(printFloat(12.5f, f) == "1.3e+01");
        assert(printFloat(11.7f, f) == "1.2e+01");
        assert(printFloat(11.3f, f) == "1.2e+01");
        assert(printFloat(11.0f, f) == "1.1e+01");
        assert(printFloat(-11.5f, f) == "-1.1e+01");
        assert(printFloat(-12.5f, f) == "-1.2e+01");
        assert(printFloat(-11.7f, f) == "-1.1e+01");
        assert(printFloat(-11.3f, f) == "-1.1e+01");
        assert(printFloat(-11.0f, f) == "-1.1e+01");

        fpctrl.rounding = FloatingPointControl.roundDown;

        assert(printFloat(11.5f, f) == "1.1e+01");
        assert(printFloat(12.5f, f) == "1.2e+01");
        assert(printFloat(11.7f, f) == "1.1e+01");
        assert(printFloat(11.3f, f) == "1.1e+01");
        assert(printFloat(11.0f, f) == "1.1e+01");
        assert(printFloat(-11.5f, f) == "-1.2e+01");
        assert(printFloat(-12.5f, f) == "-1.3e+01");
        assert(printFloat(-11.7f, f) == "-1.2e+01");
        assert(printFloat(-11.3f, f) == "-1.2e+01");
        assert(printFloat(-11.0f, f) == "-1.1e+01");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'e';
    assert(printFloat(double.nan, f) == "nan");
    assert(printFloat(-double.nan, f) == "-nan");
    assert(printFloat(double.infinity, f) == "inf");
    assert(printFloat(-double.infinity, f) == "-inf");
    assert(printFloat(0.0, f) == "0.000000e+00");
    assert(printFloat(-0.0, f) == "-0.000000e+00");
    // / 1000 needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(1e-307 / 1000, f) == "1.000000e-310");
    assert(printFloat(-1e-307 / 1000, f) == "-1.000000e-310");
    assert(printFloat(1e-30, f) == "1.000000e-30");
    assert(printFloat(-1e-30, f) == "-1.000000e-30");
    assert(printFloat(1e-10, f) == "1.000000e-10");
    assert(printFloat(-1e-10, f) == "-1.000000e-10");
    assert(printFloat(0.1, f) == "1.000000e-01");
    assert(printFloat(-0.1, f) == "-1.000000e-01");
    assert(printFloat(10.0, f) == "1.000000e+01");
    assert(printFloat(-10.0, f) == "-1.000000e+01");
    assert(printFloat(1e300, f) == "1.000000e+300");
    assert(printFloat(-1e300, f) == "-1.000000e+300");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0), f) == "4.940656e-324");
    assert(printFloat(nextDown(-0.0), f) == "-4.940656e-324");
}

@safe unittest
{
    static if (real.mant_dig > 64)
    {
        pragma(msg, "printFloat tests disabled because of unsupported `real` format");
    }
    else
    {
        auto f = FormatSpec!dchar("");
        f.spec = 'e';
        assert(printFloat(real.nan, f) == "nan");
        assert(printFloat(-real.nan, f) == "-nan");
        assert(printFloat(real.infinity, f) == "inf");
        assert(printFloat(-real.infinity, f) == "-inf");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'e';

    import std.math.operations : nextUp;

    double eps = nextUp(0.0);
    f.precision = 1000;
    assert(printFloat(eps, f) ==
           "4.9406564584124654417656879286822137236505980261432476442558568250067550727020875186529983636163599"
           ~"23797965646954457177309266567103559397963987747960107818781263007131903114045278458171678489821036"
           ~"88718636056998730723050006387409153564984387312473397273169615140031715385398074126238565591171026"
           ~"65855668676818703956031062493194527159149245532930545654440112748012970999954193198940908041656332"
           ~"45247571478690147267801593552386115501348035264934720193790268107107491703332226844753335720832431"
           ~"93609238289345836806010601150616980975307834227731832924790498252473077637592724787465608477820373"
           ~"44696995336470179726777175851256605511991315048911014510378627381672509558373897335989936648099411"
           ~"64205702637090279242767544565229087538682506419718265533447265625000000000000000000000000000000000"
           ~"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
           ~"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
           ~"000000000000000000000e-324");

    f.precision = 50;
    assert(printFloat(double.max, f) ==
           "1.79769313486231570814527423731704356798070567525845e+308");
    assert(printFloat(double.epsilon, f) ==
           "2.22044604925031308084726333618164062500000000000000e-16");

    f.precision = 10;
    assert(printFloat(1.0/3.0, f) == "3.3333333333e-01");
    assert(printFloat(1.0/7.0, f) == "1.4285714286e-01");
    assert(printFloat(1.0/9.0, f) == "1.1111111111e-01");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'e';
    f.precision = 15;

    import std.math.constants : E, PI, PI_2, PI_4, M_1_PI, M_2_PI, M_2_SQRTPI,
                                LN10, LN2, LOG2, LOG2E, LOG2T, LOG10E, SQRT2, SQRT1_2;

    assert(printFloat(cast(double) E, f) == "2.718281828459045e+00");
    assert(printFloat(cast(double) PI, f) == "3.141592653589793e+00");
    assert(printFloat(cast(double) PI_2, f) == "1.570796326794897e+00");
    assert(printFloat(cast(double) PI_4, f) == "7.853981633974483e-01");
    assert(printFloat(cast(double) M_1_PI, f) == "3.183098861837907e-01");
    assert(printFloat(cast(double) M_2_PI, f) == "6.366197723675814e-01");
    assert(printFloat(cast(double) M_2_SQRTPI, f) == "1.128379167095513e+00");
    assert(printFloat(cast(double) LN10, f) == "2.302585092994046e+00");
    assert(printFloat(cast(double) LN2, f) == "6.931471805599453e-01");
    assert(printFloat(cast(double) LOG2, f) == "3.010299956639812e-01");
    assert(printFloat(cast(double) LOG2E, f) == "1.442695040888963e+00");
    assert(printFloat(cast(double) LOG2T, f) == "3.321928094887362e+00");
    assert(printFloat(cast(double) LOG10E, f) == "4.342944819032518e-01");
    assert(printFloat(cast(double) SQRT2, f) == "1.414213562373095e+00");
    assert(printFloat(cast(double) SQRT1_2, f) == "7.071067811865476e-01");
}

// for 100% coverage
@safe unittest
{
    import std.math.hardware; // cannot be selective, because FloatingPointControl might not be defined

    auto f = FormatSpec!dchar("");
    f.spec = 'E';
    f.precision = 80;
    assert(printFloat(5.62776e+12f, f) ==
           "5.62775982080000000000000000000000000000000000000000000000000000000000000000000000E+12");

    f.precision = 49;
    assert(printFloat(2.5997869e-12f, f) ==
           "2.5997869221999758693186777236405760049819946289062E-12");

    f.precision = 6;
    assert(printFloat(-1.1418613e+07f, f) == "-1.141861E+07");
    assert(printFloat(-1.368281e+07f, f) == "-1.368281E+07");

    f.precision = 1;
    assert(printFloat(-245.666f, f) == "-2.5E+02");

    static if (is(FloatingPointControl))
    {
        FloatingPointControl fpctrl;

        fpctrl.rounding = FloatingPointControl.roundUp;

        f.precision = 0;
        assert(printFloat(709422.0f, f) == "8E+05");
    }
}

@safe unittest
{
    static if (real.mant_dig > 64)
    {
        pragma(msg, "printFloat tests disabled because of unsupported `real` format");
    }
    else
    {
        auto f = FormatSpec!dchar("");
        f.spec = 'e';
        assert(printFloat(real.nan, f) == "nan");
        assert(printFloat(-real.nan, f) == "-nan");
        assert(printFloat(real.infinity, f) == "inf");
        assert(printFloat(-real.infinity, f) == "-inf");
        assert(printFloat(0.0L, f) == "0.000000e+00");
        assert(printFloat(-0.0L, f) == "-0.000000e+00");
    }

    static if (real.mant_dig == 64)
    {
        assert(printFloat(1e-4940L, f) == "1.000000e-4940");
        assert(printFloat(-1e-4940L, f) == "-1.000000e-4940");
        assert(printFloat(1e-30L, f) == "1.000000e-30");
        assert(printFloat(-1e-30L, f) == "-1.000000e-30");
        assert(printFloat(1e-10L, f) == "1.000000e-10");
        assert(printFloat(-1e-10L, f) == "-1.000000e-10");
        assert(printFloat(0.1L, f) == "1.000000e-01");
        assert(printFloat(-0.1L, f) == "-1.000000e-01");
        assert(printFloat(10.0L, f) == "1.000000e+01");
        assert(printFloat(-10.0L, f) == "-1.000000e+01");
        version (Windows) {} // https://issues.dlang.org/show_bug.cgi?id=20972
        else
        {
            assert(printFloat(1e4000L, f) == "1.000000e+4000");
            assert(printFloat(-1e4000L, f) == "-1.000000e+4000");
        }

        import std.math.operations : nextUp, nextDown;
        assert(printFloat(nextUp(0.0L), f) == "3.645200e-4951");
        assert(printFloat(nextDown(-0.0L), f) == "-3.645200e-4951");
    }
}

@safe unittest
{
    import std.exception : assertCTFEable;
    import std.math.exponential : log2;
    import std.math.operations : nextDown;

    assertCTFEable!(
    {
        static if (real.mant_dig == 64) // 80 bit reals
        {
            // log2 is broken for x87-reals on some computers in CTFE
            // the following tests excludes these computers from the tests
            // (https://issues.dlang.org/show_bug.cgi?id=21757)
            enum test = cast(int) log2(3.05e2312L);
            static if (test == 7681)
            {
                auto f = FormatSpec!dchar("");
                f.spec = 'e';
                assert(printFloat(real.infinity, f) == "inf");
                assert(printFloat(10.0L, f) == "1.000000e+01");
                assert(printFloat(2.6080L, f) == "2.608000e+00");
                assert(printFloat(3.05e2312L, f) == "3.050000e+2312");

                f.precision = 60;
                assert(printFloat(2.65e-54L, f) ==
                       "2.650000000000000000059009987400547013941028940935296547599415e-54");

                /*
                 commented out, because CTFE is currently too slow for 5000 digits with extreme values

                f.precision = 5000;
                auto result2 = printFloat(1.2119e-4822L, f);
                assert(result2.length == 5008);
                assert(result2[$ - 20 .. $] == "60729486595339e-4822");
                auto result3 = printFloat(real.min_normal, f);
                assert(result3.length == 5008);
                assert(result3[$ - 20 .. $] == "20781410082267e-4932");
                auto result4 = printFloat(real.min_normal.nextDown, f);
                assert(result4.length == 5008);
                assert(result4[$ - 20 .. $] == "81413263331006e-4932");
                 */
            }
        }
    });
}

private void printFloatF(bool g, Writer, T, Char)(auto ref Writer w, const(T) val,
    FormatSpec!Char f, string sgn, int exp, ulong mnt, bool is_upper)
if (is(T == float) || is(T == double)
    || (is(T == real) && (T.mant_dig == double.mant_dig || T.mant_dig == 64)))
{
    import std.format.internal.write : writeAligned, PrecisionType, RoundingClass, round;

    static if (!g)
    {
        if (f.precision == f.UNSPECIFIED)
            f.precision = 6;
    }

    // special treatment for 0.0
    if (exp == 0 && mnt == 0)
    {
        writeAligned(w, sgn, "0", ".", "", f, PrecisionType.fractionalDigits);
        return;
    }

    char[T.max_exp + T.mant_dig + 1] dec_buf;

    RoundingClass rc;

    // Depending on exp, we will use one of three algorithms:
    //
    // Algorithm A: For large exponents (exp >= T.mant_dig)
    // Algorithm B: For small exponents (exp < T.mant_dig - 61)
    // Algorithm C: For exponents close to 0.
    //
    // Algorithm A:
    //   The number to print looks like this: mantissa followed by several zeros.
    //
    //   We know, that there is no fractional part, so we can just use integer division,
    //   consecutivly dividing by 10 and writing down the remainder from right to left.
    //   Unfortunately the integer is too large to fit in an ulong, so we use something
    //   like BigInt: An array of ulongs. We only use 60 bits of that ulongs, because
    //   this simplifies (and speeds up) the division to come.
    //
    //   For the division we use integer division with reminder for each ulong and put
    //   the reminder of each step in the first 4 bits of ulong of the next step (think of
    //   long division for the rationale behind this). The final reminder is the next
    //   digit (from right to left).
    //
    // Algorithm B:
    //   The number to print looks like this: zero dot several zeros followed by the mantissa
    //
    //   We know, that the number has no integer part. The algorithm consecutivly multiplies
    //   by 10. The integer part (rounded down) after the multiplication is the next digit
    //   (from left to right). This integer part is removed after each step.
    //   Again, the number is represented as an array of ulongs, with only 60 bits used of
    //   every ulong.
    //
    //   For the multiplication we use normal integer multiplication, which can result in digits
    //   in the uppermost 4 bits. These 4 digits are the carry which is added to the result
    //   of the next multiplication and finally the last carry is the next digit.
    //
    //   The calculation will stop, when only zeros remain or when we've got enough digits
    //   for the requested precision. In the second case, we have to find out, which rounding
    //   we have. Aside from special cases we do this by calculating one more digit.
    //
    // Algorithm C:
    //   This time, we know, that the integral part and the fractional part each fit into a
    //   ulong. The mantissa might be partially in both parts or completely in the fractional
    //   part.
    //
    //   We first calculate the integral part by consecutive division by 10. Then we calculate
    //   the fractional part by consecutive multiplication by 10. Again only until we have enough
    //   digits. Finally, we decide the rounding type, mainly by looking at the next digit.

    static if (is(T == real) && real.mant_dig == 64)
    {
        enum small_bound = 0;
        enum max_buf = 275;
    }
    else
    {
        enum small_bound = T.mant_dig - 61;
        static if (is(T == float))
            enum max_buf = 4;
        else
            enum max_buf = 18;
    }

    size_t start = 2;
    size_t left = 2;
    size_t right = 2;

    ulong[max_buf] bigbuf;
    if (exp >= T.mant_dig)
    {
        left = start = dec_buf.length - 1;
        right = dec_buf.length;
        dec_buf[start] = '.';

        // large number without fractional digits
        //
        // As this number does not fit in a ulong, we use an array of ulongs. We only use 60 of the 64 bits,
        // because this makes it much more easy to implement the division by 10.
        int count = exp / 60 + 1;

        // only the first few ulongs contain the mantiassa. The rest are zeros.
        int lower = 60 - (exp - T.mant_dig + 1) % 60;

        static if (is(T == real) && real.mant_dig == 64)
        {
            // for x87 reals, the lowest ulong may contain more than 60 bits,
            // because the mantissa is 63 (>60) bits long
            // therefore we need one ulong less
            if (lower <= 3) count--;
        }

        // saved in big endian format
        ulong[] mybig = bigbuf[0 .. count];

        if (lower < T.mant_dig)
        {
            mybig[0] = mnt >> lower;
            mybig[1] = (mnt & ((1L << lower) - 1)) << 60 - lower;
        }
        else
            mybig[0] = (mnt & ((1L << lower) - 1)) << 60 - lower;

        // Generation of digits by consecutive division with reminder by 10.
        int msu = 0; // Most significant ulong; when it get's zero, we can ignore it furtheron
        while (msu < count - 1 || mybig[$ - 1] != 0)
        {
            ulong mod = 0;
            foreach (i;msu .. count)
            {
                mybig[i] |= mod << 60;
                mod = mybig[i] % 10;
                mybig[i] /= 10;
            }
            if (mybig[msu] == 0)
                ++msu;

            dec_buf[--left] = cast(byte) ('0' + mod);
        }

        rc = RoundingClass.ZERO;
    }
    else if (exp < small_bound)
    {
        // small number without integer digits
        //
        // Again this number does not fit in a ulong and we use an array of ulongs. And again we
        // only use 60 bits, because this simplifies the multiplication by 10.
        int count = (T.mant_dig - exp - 2) / 60 + 1;

        // saved in little endian format
        ulong[] mybig = bigbuf[0 .. count];

        // only the last few ulongs contain the mantiassa. Because of little endian
        // format these are the ulongs at index 0 and 1 (and 2 in case of x87 reals).
        // The rest are zeros.
        int upper = 60 - (-exp - 1) % 60;

        static if (is(T == real) && real.mant_dig == 64)
        {
            if (upper < 4)
            {
                mybig[0] = (mnt & ((1L << (4 - upper)) - 1)) << 56 + upper;
                mybig[1] = (mnt >> (4 - upper)) & ((1L << 60) - 1);
                mybig[2] = mnt >> 64 - upper;
            }
            else
            {
                mybig[0] = (mnt & ((1L << (T.mant_dig - upper)) - 1)) << 60 - (T.mant_dig - upper);
                mybig[1] = mnt >> (T.mant_dig - upper);
            }
        }
        else
        {
            if (upper < T.mant_dig)
            {
                mybig[0] = (mnt & ((1L << (T.mant_dig - upper)) - 1)) << 60 - (T.mant_dig - upper);
                mybig[1] = mnt >> (T.mant_dig - upper);
            }
            else
                mybig[0] = mnt << (upper - T.mant_dig);
        }

        dec_buf[--left] = '0'; // 0 left of the dot
        dec_buf[right++] = '.';

        static if (g)
        {
            // precision starts at first non zero, so we move start
            // to the right, until we found first non zero, thus avoiding
            // a premature break of the loop
            bool found = false;
            start = left + 1;
        }

        // Generation of digits by consecutive multiplication by 10.
        int lsu = 0; // Least significant ulong; when it get's zero, we can ignore it furtheron
        while ((lsu < count - 1 || mybig[$ - 1] != 0) && right - start - 1 < f.precision)
        {
            ulong over = 0;
            foreach (i;lsu .. count)
            {
                mybig[i] = mybig[i] * 10 + over;
                over = mybig[i] >> 60;
                mybig[i] &= (1L << 60) - 1;
            }
            if (mybig[lsu] == 0)
                ++lsu;

            dec_buf[right++] = cast(byte) ('0' + over);

            static if (g)
            {
                if (dec_buf[right - 1] != '0')
                    found = true;
                else if (!found)
                    start++;
            }
        }

        static if (g) start = 2;

        if (lsu >= count - 1 && mybig[count - 1] == 0)
            rc = RoundingClass.ZERO;
        else if (lsu == count - 1 && mybig[lsu] == 1L << 59)
            rc = RoundingClass.FIVE;
        else
        {
            ulong over = 0;
            foreach (i;lsu .. count)
            {
                mybig[i] = mybig[i] * 10 + over;
                over = mybig[i] >> 60;
                mybig[i] &= (1L << 60) - 1;
            }
            rc = over >= 5 ? RoundingClass.UPPER : RoundingClass.LOWER;
        }
    }
    else
    {
        // medium sized number, probably with integer and fractional digits
        // this is fastest, because both parts fit into a ulong each
        ulong int_part = mnt >> (T.mant_dig - 1 - exp);
        ulong frac_part = mnt & ((1L << (T.mant_dig - 1 - exp)) - 1);

        // for x87 reals the mantiassa might be up to 3 bits too long
        // we need to save these bits as a tail and handle this separately
        static if (is(T == real) && real.mant_dig == 64)
        {
            ulong tail = 0;
            ulong tail_length = 0;
            if (exp < 3)
            {
                tail = frac_part & ((1L << (3 - exp)) - 1);
                tail_length = 3 - exp;
                frac_part >>= 3 - exp;
                exp = 3;
            }
        }

        static if (g) auto found = int_part > 0; // searching first non zero

        // creating int part
        if (int_part == 0)
            dec_buf[--left] = '0';
        else
        {
            import core.bitop : bsr;
            left = right = start = int_part.bsr * 100 / 332 + 4;

            while (int_part > 0)
            {
                dec_buf[--left] = '0' + (int_part % 10);
                int_part /= 10;
            }
        }

        static if (g) size_t save_start = right;

        dec_buf[right++] = '.';

        // creating frac part
        static if (g) start = left + (found ? 0 : 1);
        while (frac_part != 0 && right - start - 1 < f.precision)
        {
            frac_part *= 10;
            static if (is(T == real) && real.mant_dig == 64)
            {
                if (tail_length > 0)
                {
                    // together this is *= 10;
                    tail *= 5;
                    tail_length--;

                    frac_part += tail >> tail_length;
                    if (tail_length > 0)
                        tail &= (1L << tail_length) - 1;
                }
            }
            dec_buf[right++] = cast(byte)('0' + (frac_part >> (T.mant_dig - 1 - exp)));

            static if (g)
            {
                if (dec_buf[right - 1] != '0')
                    found = true;
                else if (!found)
                    start++;
            }

            frac_part &= ((1L << (T.mant_dig - 1 - exp)) - 1);
        }

        static if (g) start = save_start;

        if (frac_part == 0)
            rc = RoundingClass.ZERO;
        else
        {
            frac_part *= 10;
            auto nextDigit = frac_part >> (T.mant_dig - 1 - exp);
            frac_part &= ((1L << (T.mant_dig - 1 - exp)) - 1);

            if (nextDigit == 5 && frac_part == 0)
                rc = RoundingClass.FIVE;
            else if (nextDigit >= 5)
                rc = RoundingClass.UPPER;
            else
                rc = RoundingClass.LOWER;
        }
    }

    if (round(dec_buf, left, right, rc, sgn == "-")) left--;

    while (right > start + 1 && dec_buf[right - 1] == '0') right--;

    static if (g)
        writeAligned(w, sgn, dec_buf[left .. start], dec_buf[start .. right], "", f, PrecisionType.allDigits);
    else
        writeAligned(w, sgn, dec_buf[left .. start], dec_buf[start .. right], "", f, PrecisionType.fractionalDigits);
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'f';
    assert(printFloat(float.nan, f) == "nan");
    assert(printFloat(-float.nan, f) == "-nan");
    assert(printFloat(float.infinity, f) == "inf");
    assert(printFloat(-float.infinity, f) == "-inf");
    assert(printFloat(0.0f, f) == "0.000000");
    assert(printFloat(-0.0f, f) == "-0.000000");
    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "0.000000");
    assert(printFloat(cast(float) -1e-40, f) == "-0.000000");
    assert(printFloat(1e-30f, f) == "0.000000");
    assert(printFloat(-1e-30f, f) == "-0.000000");
    assert(printFloat(1e-10f, f) == "0.000000");
    assert(printFloat(-1e-10f, f) == "-0.000000");
    assert(printFloat(0.1f, f) == "0.100000");
    assert(printFloat(-0.1f, f) == "-0.100000");
    assert(printFloat(10.0f, f) == "10.000000");
    assert(printFloat(-10.0f, f) == "-10.000000");
    assert(printFloat(1e30f, f) == "1000000015047466219876688855040.000000");
    assert(printFloat(-1e30f, f) == "-1000000015047466219876688855040.000000");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "0.000000");
    assert(printFloat(nextDown(-0.0f), f) == "-0.000000");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'f';
    f.width = 20;
    f.precision = 10;

    assert(printFloat(float.nan, f) == "                 nan");
    assert(printFloat(-float.nan, f) == "                -nan");
    assert(printFloat(float.infinity, f) == "                 inf");
    assert(printFloat(-float.infinity, f) == "                -inf");
    assert(printFloat(0.0f, f) == "        0.0000000000");
    assert(printFloat(-0.0f, f) == "       -0.0000000000");
    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "        0.0000000000");
    assert(printFloat(cast(float) -1e-40, f) == "       -0.0000000000");
    assert(printFloat(1e-30f, f) == "        0.0000000000");
    assert(printFloat(-1e-30f, f) == "       -0.0000000000");
    assert(printFloat(1e-10f, f) == "        0.0000000001");
    assert(printFloat(-1e-10f, f) == "       -0.0000000001");
    assert(printFloat(0.1f, f) == "        0.1000000015");
    assert(printFloat(-0.1f, f) == "       -0.1000000015");
    assert(printFloat(10.0f, f) == "       10.0000000000");
    assert(printFloat(-10.0f, f) == "      -10.0000000000");
    assert(printFloat(1e30f, f) == "1000000015047466219876688855040.0000000000");
    assert(printFloat(-1e30f, f) == "-1000000015047466219876688855040.0000000000");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "        0.0000000000");
    assert(printFloat(nextDown(-0.0f), f) == "       -0.0000000000");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'f';
    f.width = 20;
    f.precision = 10;
    f.flDash = true;

    assert(printFloat(float.nan, f) == "nan                 ");
    assert(printFloat(-float.nan, f) == "-nan                ");
    assert(printFloat(float.infinity, f) == "inf                 ");
    assert(printFloat(-float.infinity, f) == "-inf                ");
    assert(printFloat(0.0f, f) == "0.0000000000        ");
    assert(printFloat(-0.0f, f) == "-0.0000000000       ");
    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "0.0000000000        ");
    assert(printFloat(cast(float) -1e-40, f) == "-0.0000000000       ");
    assert(printFloat(1e-30f, f) == "0.0000000000        ");
    assert(printFloat(-1e-30f, f) == "-0.0000000000       ");
    assert(printFloat(1e-10f, f) == "0.0000000001        ");
    assert(printFloat(-1e-10f, f) == "-0.0000000001       ");
    assert(printFloat(0.1f, f) == "0.1000000015        ");
    assert(printFloat(-0.1f, f) == "-0.1000000015       ");
    assert(printFloat(10.0f, f) == "10.0000000000       ");
    assert(printFloat(-10.0f, f) == "-10.0000000000      ");
    assert(printFloat(1e30f, f) == "1000000015047466219876688855040.0000000000");
    assert(printFloat(-1e30f, f) == "-1000000015047466219876688855040.0000000000");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "0.0000000000        ");
    assert(printFloat(nextDown(-0.0f), f) == "-0.0000000000       ");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'f';
    f.width = 20;
    f.precision = 10;
    f.flZero = true;

    assert(printFloat(float.nan, f) == "                 nan");
    assert(printFloat(-float.nan, f) == "                -nan");
    assert(printFloat(float.infinity, f) == "                 inf");
    assert(printFloat(-float.infinity, f) == "                -inf");
    assert(printFloat(0.0f, f) == "000000000.0000000000");
    assert(printFloat(-0.0f, f) == "-00000000.0000000000");
    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "000000000.0000000000");
    assert(printFloat(cast(float) -1e-40, f) == "-00000000.0000000000");
    assert(printFloat(1e-30f, f) == "000000000.0000000000");
    assert(printFloat(-1e-30f, f) == "-00000000.0000000000");
    assert(printFloat(1e-10f, f) == "000000000.0000000001");
    assert(printFloat(-1e-10f, f) == "-00000000.0000000001");
    assert(printFloat(0.1f, f) == "000000000.1000000015");
    assert(printFloat(-0.1f, f) == "-00000000.1000000015");
    assert(printFloat(10.0f, f) == "000000010.0000000000");
    assert(printFloat(-10.0f, f) == "-00000010.0000000000");
    assert(printFloat(1e30f, f) == "1000000015047466219876688855040.0000000000");
    assert(printFloat(-1e30f, f) == "-1000000015047466219876688855040.0000000000");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "000000000.0000000000");
    assert(printFloat(nextDown(-0.0f), f) == "-00000000.0000000000");
}

@safe unittest
{
    import std.math.hardware; // cannot be selective, because FloatingPointControl might not be defined

    // std.math's FloatingPointControl isn't available on all target platforms
    static if (is(FloatingPointControl))
    {
        FloatingPointControl fpctrl;

        auto f = FormatSpec!dchar("");
        f.spec = 'f';
        f.precision = 0;

        fpctrl.rounding = FloatingPointControl.roundToNearest;

        /*
         assert(printFloat(11.5f, f) == "12");
         assert(printFloat(12.5f, f) == "13");
         assert(printFloat(11.7f, f) == "12");
         assert(printFloat(11.3f, f) == "11");
         assert(printFloat(11.0f, f) == "11");
         assert(printFloat(-11.5f, f) == "-12");
         assert(printFloat(-12.5f, f) == "-13");
         assert(printFloat(-11.7f, f) == "-12");
         assert(printFloat(-11.3f, f) == "-11");
         assert(printFloat(-11.0f, f) == "-11");
         */

        assert(printFloat(11.5f, f) == "12");
        assert(printFloat(12.5f, f) == "12");
        assert(printFloat(11.7f, f) == "12");
        assert(printFloat(11.3f, f) == "11");
        assert(printFloat(11.0f, f) == "11");
        assert(printFloat(-11.5f, f) == "-12");
        assert(printFloat(-12.5f, f) == "-12");
        assert(printFloat(-11.7f, f) == "-12");
        assert(printFloat(-11.3f, f) == "-11");
        assert(printFloat(-11.0f, f) == "-11");

        fpctrl.rounding = FloatingPointControl.roundToZero;

        assert(printFloat(11.5f, f) == "11");
        assert(printFloat(12.5f, f) == "12");
        assert(printFloat(11.7f, f) == "11");
        assert(printFloat(11.3f, f) == "11");
        assert(printFloat(11.0f, f) == "11");
        assert(printFloat(-11.5f, f) == "-11");
        assert(printFloat(-12.5f, f) == "-12");
        assert(printFloat(-11.7f, f) == "-11");
        assert(printFloat(-11.3f, f) == "-11");
        assert(printFloat(-11.0f, f) == "-11");

        fpctrl.rounding = FloatingPointControl.roundUp;

        assert(printFloat(11.5f, f) == "12");
        assert(printFloat(12.5f, f) == "13");
        assert(printFloat(11.7f, f) == "12");
        assert(printFloat(11.3f, f) == "12");
        assert(printFloat(11.0f, f) == "11");
        assert(printFloat(-11.5f, f) == "-11");
        assert(printFloat(-12.5f, f) == "-12");
        assert(printFloat(-11.7f, f) == "-11");
        assert(printFloat(-11.3f, f) == "-11");
        assert(printFloat(-11.0f, f) == "-11");

        fpctrl.rounding = FloatingPointControl.roundDown;

        assert(printFloat(11.5f, f) == "11");
        assert(printFloat(12.5f, f) == "12");
        assert(printFloat(11.7f, f) == "11");
        assert(printFloat(11.3f, f) == "11");
        assert(printFloat(11.0f, f) == "11");
        assert(printFloat(-11.5f, f) == "-12");
        assert(printFloat(-12.5f, f) == "-13");
        assert(printFloat(-11.7f, f) == "-12");
        assert(printFloat(-11.3f, f) == "-12");
        assert(printFloat(-11.0f, f) == "-11");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'f';
    assert(printFloat(double.nan, f) == "nan");
    assert(printFloat(-double.nan, f) == "-nan");
    assert(printFloat(double.infinity, f) == "inf");
    assert(printFloat(-double.infinity, f) == "-inf");
    assert(printFloat(0.0, f) == "0.000000");
    assert(printFloat(-0.0, f) == "-0.000000");
    // / 1000 needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(1e-307 / 1000, f) == "0.000000");
    assert(printFloat(-1e-307 / 1000, f) == "-0.000000");
    assert(printFloat(1e-30, f) == "0.000000");
    assert(printFloat(-1e-30, f) == "-0.000000");
    assert(printFloat(1e-10, f) == "0.000000");
    assert(printFloat(-1e-10, f) == "-0.000000");
    assert(printFloat(0.1, f) == "0.100000");
    assert(printFloat(-0.1, f) == "-0.100000");
    assert(printFloat(10.0, f) == "10.000000");
    assert(printFloat(-10.0, f) == "-10.000000");
    assert(printFloat(1e300, f) ==
           "100000000000000005250476025520442024870446858110815915491585411551180245798890819578637137508044786"
          ~"404370444383288387817694252323536043057564479218478670698284838720092657580373783023379478809005936"
          ~"895323497079994508111903896764088007465274278014249457925878882005684283811566947219638686545940054"
          ~"0160.000000");
    assert(printFloat(-1e300, f) ==
           "-100000000000000005250476025520442024870446858110815915491585411551180245798890819578637137508044786"
          ~"404370444383288387817694252323536043057564479218478670698284838720092657580373783023379478809005936"
          ~"895323497079994508111903896764088007465274278014249457925878882005684283811566947219638686545940054"
          ~"0160.000000");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0), f) == "0.000000");
    assert(printFloat(nextDown(-0.0), f) == "-0.000000");
}

@safe unittest
{
    static if (real.mant_dig > 64)
    {
        pragma(msg, "printFloat tests disabled because of unsupported `real` format");
    }
    else
    {
        auto f = FormatSpec!dchar("");
        f.spec = 'f';
        assert(printFloat(real.nan, f) == "nan");
        assert(printFloat(-real.nan, f) == "-nan");
        assert(printFloat(real.infinity, f) == "inf");
        assert(printFloat(-real.infinity, f) == "-inf");
        assert(printFloat(0.0L, f) == "0.000000");
        assert(printFloat(-0.0L, f) == "-0.000000");
    }

    static if (real.mant_dig == 64)
    {
        assert(printFloat(1e-4940L, f) == "0.000000");
        assert(printFloat(-1e-4940L, f) == "-0.000000");
        assert(printFloat(1e-30L, f) == "0.000000");
        assert(printFloat(-1e-30L, f) == "-0.000000");
        assert(printFloat(1e-10L, f) == "0.000000");
        assert(printFloat(-1e-10L, f) == "-0.000000");
        assert(printFloat(0.1L, f) == "0.100000");
        assert(printFloat(-0.1L, f) == "-0.100000");
        assert(printFloat(10.0L, f) == "10.000000");
        assert(printFloat(-10.0L, f) == "-10.000000");
        version (Windows) {} // https://issues.dlang.org/show_bug.cgi?id=20972
        else
        {
            auto result1 = printFloat(1e4000L, f);
            assert(result1.length == 4007 && result1[0 .. 40] == "9999999999999999999965463873099623784932");
            auto result2 = printFloat(-1e4000L, f);
            assert(result2.length == 4008 && result2[0 .. 40] == "-999999999999999999996546387309962378493");
        }

        import std.math.operations : nextUp, nextDown;
        assert(printFloat(nextUp(0.0L), f) == "0.000000");
        assert(printFloat(nextDown(-0.0L), f) == "-0.000000");
    }
}

@safe unittest
{
    import std.exception : assertCTFEable;
    import std.math.exponential : log2;
    import std.math.operations : nextDown;

    assertCTFEable!(
    {
        static if (real.mant_dig == 64) // 80 bit reals
        {
            // log2 is broken for x87-reals on some computers in CTFE
            // the following tests excludes these computers from the tests
            // (https://issues.dlang.org/show_bug.cgi?id=21757)
            enum test = cast(int) log2(3.05e2312L);
            static if (test == 7681)
            {
                auto f = FormatSpec!dchar("");
                f.spec = 'f';
                assert(printFloat(real.infinity, f) == "inf");
                assert(printFloat(10.0L, f) == "10.000000");
                assert(printFloat(2.6080L, f) == "2.608000");
                auto result1 = printFloat(3.05e2312L, f);
                assert(result1.length == 2320);
                assert(result1[0 .. 20] == "30499999999999999999");

                f.precision = 60;
                assert(printFloat(2.65e-54L, f) ==
                       "0.000000000000000000000000000000000000000000000000000002650000");

                /*
                 commented out, because CTFE is currently too slow for 5000 digits with extreme values

                f.precision = 5000;
                auto result2 = printFloat(1.2119e-4822L, f);
                assert(result2.length == 5002);
                assert(result2[$ - 20 .. $] == "60076763752233836613");
                auto result3 = printFloat(real.min_normal, f);
                assert(result3.length == 5002);
                assert(result3[$ - 20 .. $] == "47124010882722980874");
                auto result4 = printFloat(real.min_normal.nextDown, f);
                assert(result4.length == 5002);
                assert(result4[$ - 20 .. $] == "52925846892214823939");
                 */
            }
        }
    });
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'f';

    import std.math.operations : nextUp;

    double eps = nextUp(0.0);
    f.precision = 1000;
    assert(printFloat(eps, f) ==
           "0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
           ~"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
           ~"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
           ~"00000000000000000000000000000049406564584124654417656879286822137236505980261432476442558568250067"
           ~"55072702087518652998363616359923797965646954457177309266567103559397963987747960107818781263007131"
           ~"90311404527845817167848982103688718636056998730723050006387409153564984387312473397273169615140031"
           ~"71538539807412623856559117102665855668676818703956031062493194527159149245532930545654440112748012"
           ~"97099995419319894090804165633245247571478690147267801593552386115501348035264934720193790268107107"
           ~"49170333222684475333572083243193609238289345836806010601150616980975307834227731832924790498252473"
           ~"07763759272478746560847782037344696995336470179726777175851256605511991315048911014510378627381672"
           ~"509558373897335989937");

    f.precision = 0;
    assert(printFloat(double.max, f) ==
           "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878"
           ~"17154045895351438246423432132688946418276846754670353751698604991057655128207624549009038932894407"
           ~"58685084551339423045832369032229481658085593321233482747978262041447231687381771809192998812504040"
           ~"26184124858368");

    f.precision = 50;
    assert(printFloat(double.epsilon, f) ==
           "0.00000000000000022204460492503130808472633361816406");

    f.precision = 10;
    assert(printFloat(1.0/3.0, f) == "0.3333333333");
    assert(printFloat(1.0/7.0, f) == "0.1428571429");
    assert(printFloat(1.0/9.0, f) == "0.1111111111");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'f';
    f.precision = 15;

    import std.math.constants : E, PI, PI_2, PI_4, M_1_PI, M_2_PI, M_2_SQRTPI,
                                LN10, LN2, LOG2, LOG2E, LOG2T, LOG10E, SQRT2, SQRT1_2;

    assert(printFloat(cast(double) E, f) == "2.718281828459045");
    assert(printFloat(cast(double) PI, f) == "3.141592653589793");
    assert(printFloat(cast(double) PI_2, f) == "1.570796326794897");
    assert(printFloat(cast(double) PI_4, f) == "0.785398163397448");
    assert(printFloat(cast(double) M_1_PI, f) == "0.318309886183791");
    assert(printFloat(cast(double) M_2_PI, f) == "0.636619772367581");
    assert(printFloat(cast(double) M_2_SQRTPI, f) == "1.128379167095513");
    assert(printFloat(cast(double) LN10, f) == "2.302585092994046");
    assert(printFloat(cast(double) LN2, f) == "0.693147180559945");
    assert(printFloat(cast(double) LOG2, f) == "0.301029995663981");
    assert(printFloat(cast(double) LOG2E, f) == "1.442695040888963");
    assert(printFloat(cast(double) LOG2T, f) == "3.321928094887362");
    assert(printFloat(cast(double) LOG10E, f) == "0.434294481903252");
    assert(printFloat(cast(double) SQRT2, f) == "1.414213562373095");
    assert(printFloat(cast(double) SQRT1_2, f) == "0.707106781186548");
}

// for 100% coverage
@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'f';
    f.precision = 1;
    assert(printFloat(9.99, f) == "10.0");

    import std.math.operations : nextUp;

    float eps = nextUp(0.0f);

    f.precision = 148;
    assert(printFloat(eps, f) ==
           "0.0000000000000000000000000000000000000000000014012984643248170709237295832899161312802619418765157"
           ~"717570682838897910826858606014866381883621215820312");

    f.precision = 149;
    assert(printFloat(eps, f) ==
           "0.0000000000000000000000000000000000000000000014012984643248170709237295832899161312802619418765157"
           ~"7175706828388979108268586060148663818836212158203125");
}

private void printFloatG(Writer, T, Char)(auto ref Writer w, const(T) val,
    FormatSpec!Char f, string sgn, int exp, ulong mnt, bool is_upper)
if (is(T == float) || is(T == double)
    || (is(T == real) && (T.mant_dig == double.mant_dig || T.mant_dig == 64)))
{
    import core.math : abs = fabs;

    if (f.precision == f.UNSPECIFIED)
        f.precision = 6;

    if (f.precision == 0)
        f.precision = 1;

    import std.math.hardware;
    import std.format.internal.write : RoundingMode;

    auto rm = RoundingMode.toNearestTiesToEven;

    if (!__ctfe)
    {
        // std.math's FloatingPointControl isn't available on all target platforms
        static if (is(FloatingPointControl))
        {
            switch (FloatingPointControl.rounding)
            {
            case FloatingPointControl.roundUp:
                rm = RoundingMode.up;
                break;
            case FloatingPointControl.roundDown:
                rm = RoundingMode.down;
                break;
            case FloatingPointControl.roundToZero:
                rm = RoundingMode.toZero;
                break;
            case FloatingPointControl.roundToNearest:
                rm = RoundingMode.toNearestTiesToEven;
                break;
            default: assert(false, "Unknown floating point rounding mode");
            }
        }
    }

    bool useE = false;

    final switch (rm)
    {
    case RoundingMode.up:
        useE = abs(val) >= 10.0 ^^ f.precision - (val > 0 ? 1 : 0)
            || abs(val) < 0.0001 - (val > 0 ? (10.0 ^^ (-4 - f.precision)) : 0);
        break;
    case RoundingMode.down:
        useE = abs(val) >= 10.0 ^^ f.precision - (val < 0 ? 1 : 0)
            || abs(val) < 0.0001 - (val < 0 ? (10.0 ^^ (-4 - f.precision)) : 0);
        break;
    case RoundingMode.toZero:
        useE = abs(val) >= 10.0 ^^ f.precision
            || abs(val) < 0.0001;
        break;
    case RoundingMode.toNearestTiesToEven:
    case RoundingMode.toNearestTiesAwayFromZero:
        useE = abs(val) >= 10.0 ^^ f.precision - 0.5
            || abs(val) < 0.0001 - 0.5 * (10.0 ^^ (-4 - f.precision));
        break;
    }

    if (useE)
        return printFloatE!true(w, val, f, sgn, exp, mnt, is_upper);
    else
        return printFloatF!true(w, val, f, sgn, exp, mnt, is_upper);
}

@safe unittest
{
    // This one tests the switch between e-like and f-like output.
    // There is a small gap left between the two, where the used
    // variation is not clearly defined. This is intentional and due
    // to the way, D handles floating point numbers. On different
    // computers with different reals the results may vary in this gap.

    import std.math.operations : nextDown, nextUp;
    import std.math.hardware; // cannot be selective, because FloatingPointControl might not be defined

    auto f = FormatSpec!dchar("");
    f.spec = 'g';

    double val = 999999.5;
    assert(printFloat(val.nextUp, f) == "1e+06");
    val = nextDown(val);
    assert(printFloat(val.nextDown, f) == "999999");

    val = 0.00009999995;
    assert(printFloat(val.nextUp, f) == "0.0001");
    val = nextDown(val);
    assert(printFloat(val.nextDown, f) == "9.99999e-05");

    static if (is(FloatingPointControl))
    {
        FloatingPointControl fpctrl;

        fpctrl.rounding = FloatingPointControl.roundToZero;

        val = 1000000;
        assert(printFloat(val.nextUp, f) == "1e+06");
        val = nextDown(val);
        assert(printFloat(val.nextDown, f) == "999999");

        val = 0.0001;
        assert(printFloat(val.nextUp, f) == "0.0001");
        val = nextDown(val);
        assert(printFloat(val.nextDown, f) == "9.99999e-05");

        fpctrl.rounding = FloatingPointControl.roundUp;

        val = 999999;
        assert(printFloat(val.nextUp, f) == "1e+06");
        val = nextDown(val);
        assert(printFloat(val.nextDown, f) == "999999");

        // 0.0000999999 is actually represented as 0.0000999998999..., which is
        // less than 0.0000999999, so we need to use nextUp to get the corner case here
        val = nextUp(0.0000999999);
        assert(printFloat(val.nextUp, f) == "0.0001");
        val = nextDown(val);
        assert(printFloat(val.nextDown, f) == "9.99999e-05");

        fpctrl.rounding = FloatingPointControl.roundDown;

        val = 1000000;
        assert(printFloat(val.nextUp, f) == "1e+06");
        val = nextDown(val);
        assert(printFloat(val.nextDown, f) == "999999");

        val = 0.0001;
        assert(printFloat(val.nextUp, f) == "0.0001");
        val = nextDown(val);
        assert(printFloat(val.nextDown, f) == "9.99999e-05");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'g';
    assert(printFloat(float.nan, f) == "nan");
    assert(printFloat(-float.nan, f) == "-nan");
    assert(printFloat(float.infinity, f) == "inf");
    assert(printFloat(-float.infinity, f) == "-inf");
    assert(printFloat(0.0f, f) == "0");
    assert(printFloat(-0.0f, f) == "-0");

    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "9.99995e-41");
    assert(printFloat(cast(float) -1e-40, f) == "-9.99995e-41");
    assert(printFloat(1e-30f, f) == "1e-30");
    assert(printFloat(-1e-30f, f) == "-1e-30");
    assert(printFloat(1e-10f, f) == "1e-10");
    assert(printFloat(-1e-10f, f) == "-1e-10");
    assert(printFloat(0.1f, f) == "0.1");
    assert(printFloat(-0.1f, f) == "-0.1");
    assert(printFloat(10.0f, f) == "10");
    assert(printFloat(-10.0f, f) == "-10");
    assert(printFloat(1e30f, f) == "1e+30");
    assert(printFloat(-1e30f, f) == "-1e+30");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "1.4013e-45");
    assert(printFloat(nextDown(-0.0f), f) == "-1.4013e-45");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'g';
    f.width = 20;
    f.precision = 10;

    assert(printFloat(float.nan, f) == "                 nan");
    assert(printFloat(-float.nan, f) == "                -nan");
    assert(printFloat(float.infinity, f) == "                 inf");
    assert(printFloat(-float.infinity, f) == "                -inf");
    assert(printFloat(0.0f, f) == "                   0");
    assert(printFloat(-0.0f, f) == "                  -0");
    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "     9.999946101e-41");
    assert(printFloat(cast(float) -1e-40, f) == "    -9.999946101e-41");
    assert(printFloat(1e-30f, f) == "     1.000000003e-30");
    assert(printFloat(-1e-30f, f) == "    -1.000000003e-30");
    assert(printFloat(1e-10f, f) == "     1.000000013e-10");
    assert(printFloat(-1e-10f, f) == "    -1.000000013e-10");
    assert(printFloat(0.1f, f) == "        0.1000000015");
    assert(printFloat(-0.1f, f) == "       -0.1000000015");
    assert(printFloat(10.0f, f) == "                  10");
    assert(printFloat(-10.0f, f) == "                 -10");
    assert(printFloat(1e30f, f) == "     1.000000015e+30");
    assert(printFloat(-1e30f, f) == "    -1.000000015e+30");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "     1.401298464e-45");
    assert(printFloat(nextDown(-0.0f), f) == "    -1.401298464e-45");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'g';
    f.width = 20;
    f.precision = 10;
    f.flDash = true;

    assert(printFloat(float.nan, f) == "nan                 ");
    assert(printFloat(-float.nan, f) == "-nan                ");
    assert(printFloat(float.infinity, f) == "inf                 ");
    assert(printFloat(-float.infinity, f) == "-inf                ");
    assert(printFloat(0.0f, f) == "0                   ");
    assert(printFloat(-0.0f, f) == "-0                  ");

    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "9.999946101e-41     ");
    assert(printFloat(cast(float) -1e-40, f) == "-9.999946101e-41    ");
    assert(printFloat(1e-30f, f) == "1.000000003e-30     ");
    assert(printFloat(-1e-30f, f) == "-1.000000003e-30    ");
    assert(printFloat(1e-10f, f) == "1.000000013e-10     ");
    assert(printFloat(-1e-10f, f) == "-1.000000013e-10    ");
    assert(printFloat(0.1f, f) == "0.1000000015        ");
    assert(printFloat(-0.1f, f) == "-0.1000000015       ");
    assert(printFloat(10.0f, f) == "10                  ");
    assert(printFloat(-10.0f, f) == "-10                 ");
    assert(printFloat(1e30f, f) == "1.000000015e+30     ");
    assert(printFloat(-1e30f, f) == "-1.000000015e+30    ");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "1.401298464e-45     ");
    assert(printFloat(nextDown(-0.0f), f) == "-1.401298464e-45    ");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'g';
    f.width = 20;
    f.precision = 10;
    f.flZero = true;

    assert(printFloat(float.nan, f) == "                 nan");
    assert(printFloat(-float.nan, f) == "                -nan");
    assert(printFloat(float.infinity, f) == "                 inf");
    assert(printFloat(-float.infinity, f) == "                -inf");
    assert(printFloat(0.0f, f) == "00000000000000000000");
    assert(printFloat(-0.0f, f) == "-0000000000000000000");

    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "000009.999946101e-41");
    assert(printFloat(cast(float) -1e-40, f) == "-00009.999946101e-41");
    assert(printFloat(1e-30f, f) == "000001.000000003e-30");
    assert(printFloat(-1e-30f, f) == "-00001.000000003e-30");
    assert(printFloat(1e-10f, f) == "000001.000000013e-10");
    assert(printFloat(-1e-10f, f) == "-00001.000000013e-10");
    assert(printFloat(0.1f, f) == "000000000.1000000015");
    assert(printFloat(-0.1f, f) == "-00000000.1000000015");
    assert(printFloat(10.0f, f) == "00000000000000000010");
    assert(printFloat(-10.0f, f) == "-0000000000000000010");
    assert(printFloat(1e30f, f) == "000001.000000015e+30");
    assert(printFloat(-1e30f, f) == "-00001.000000015e+30");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "000001.401298464e-45");
    assert(printFloat(nextDown(-0.0f), f) == "-00001.401298464e-45");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'g';
    f.precision = 10;
    f.flHash = true;

    assert(printFloat(float.nan, f) == "nan");
    assert(printFloat(-float.nan, f) == "-nan");
    assert(printFloat(float.infinity, f) == "inf");
    assert(printFloat(-float.infinity, f) == "-inf");
    assert(printFloat(0.0f, f) == "0.000000000");
    assert(printFloat(-0.0f, f) == "-0.000000000");

    // cast needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(cast(float) 1e-40, f) == "9.999946101e-41");
    assert(printFloat(cast(float) -1e-40, f) == "-9.999946101e-41");
    assert(printFloat(1e-30f, f) == "1.000000003e-30");
    assert(printFloat(-1e-30f, f) == "-1.000000003e-30");
    assert(printFloat(1e-10f, f) == "1.000000013e-10");
    assert(printFloat(-1e-10f, f) == "-1.000000013e-10");
    assert(printFloat(0.1f, f) == "0.1000000015");
    assert(printFloat(-0.1f, f) == "-0.1000000015");
    assert(printFloat(10.0f, f) == "10.00000000");
    assert(printFloat(-10.0f, f) == "-10.00000000");
    assert(printFloat(1e30f, f) == "1.000000015e+30");
    assert(printFloat(-1e30f, f) == "-1.000000015e+30");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0f), f) == "1.401298464e-45");
    assert(printFloat(nextDown(-0.0f), f) == "-1.401298464e-45");
}

@safe unittest
{
    import std.math.hardware; // cannot be selective, because FloatingPointControl might not be defined

    // std.math's FloatingPointControl isn't available on all target platforms
    static if (is(FloatingPointControl))
    {
        FloatingPointControl fpctrl;

        char[256] buf;
        auto f = FormatSpec!dchar("");
        f.spec = 'g';
        f.precision = 2;

        fpctrl.rounding = FloatingPointControl.roundToNearest;

        /*
         assert(printFloat(11.5f, f, RoundingMode.toNearestTiesAwayFromZero) == "12");
         assert(printFloat(12.5f, f, RoundingMode.toNearestTiesAwayFromZero) == "13");
         assert(printFloat(11.7f, f, RoundingMode.toNearestTiesAwayFromZero) == "12");
         assert(printFloat(11.3f, f, RoundingMode.toNearestTiesAwayFromZero) == "11");
         assert(printFloat(11.0f, f, RoundingMode.toNearestTiesAwayFromZero) == "11");
         assert(printFloat(-11.5f, f, RoundingMode.toNearestTiesAwayFromZero) == "-12");
         assert(printFloat(-12.5f, f, RoundingMode.toNearestTiesAwayFromZero) == "-13");
         assert(printFloat(-11.7f, f, RoundingMode.toNearestTiesAwayFromZero) == "-12");
         assert(printFloat(-11.3f, f, RoundingMode.toNearestTiesAwayFromZero) == "-11");
         assert(printFloat(-11.0f, f, RoundingMode.toNearestTiesAwayFromZero) == "-11");
         */

        // ties to even
        assert(printFloat(11.5f, f) == "12");
        assert(printFloat(12.5f, f) == "12");
        assert(printFloat(11.7f, f) == "12");
        assert(printFloat(11.3f, f) == "11");
        assert(printFloat(11.0f, f) == "11");
        assert(printFloat(-11.5f, f) == "-12");
        assert(printFloat(-12.5f, f) == "-12");
        assert(printFloat(-11.7f, f) == "-12");
        assert(printFloat(-11.3f, f) == "-11");
        assert(printFloat(-11.0f, f) == "-11");

        fpctrl.rounding = FloatingPointControl.roundToZero;

        assert(printFloat(11.5f, f) == "11");
        assert(printFloat(12.5f, f) == "12");
        assert(printFloat(11.7f, f) == "11");
        assert(printFloat(11.3f, f) == "11");
        assert(printFloat(11.0f, f) == "11");
        assert(printFloat(-11.5f, f) == "-11");
        assert(printFloat(-12.5f, f) == "-12");
        assert(printFloat(-11.7f, f) == "-11");
        assert(printFloat(-11.3f, f) == "-11");
        assert(printFloat(-11.0f, f) == "-11");

        fpctrl.rounding = FloatingPointControl.roundUp;

        assert(printFloat(11.5f, f) == "12");
        assert(printFloat(12.5f, f) == "13");
        assert(printFloat(11.7f, f) == "12");
        assert(printFloat(11.3f, f) == "12");
        assert(printFloat(11.0f, f) == "11");
        assert(printFloat(-11.5f, f) == "-11");
        assert(printFloat(-12.5f, f) == "-12");
        assert(printFloat(-11.7f, f) == "-11");
        assert(printFloat(-11.3f, f) == "-11");
        assert(printFloat(-11.0f, f) == "-11");

        fpctrl.rounding = FloatingPointControl.roundDown;

        assert(printFloat(11.5f, f) == "11");
        assert(printFloat(12.5f, f) == "12");
        assert(printFloat(11.7f, f) == "11");
        assert(printFloat(11.3f, f) == "11");
        assert(printFloat(11.0f, f) == "11");
        assert(printFloat(-11.5f, f) == "-12");
        assert(printFloat(-12.5f, f) == "-13");
        assert(printFloat(-11.7f, f) == "-12");
        assert(printFloat(-11.3f, f) == "-12");
        assert(printFloat(-11.0f, f) == "-11");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'g';

    assert(printFloat(double.nan, f) == "nan");
    assert(printFloat(-double.nan, f) == "-nan");
    assert(printFloat(double.infinity, f) == "inf");
    assert(printFloat(-double.infinity, f) == "-inf");
    assert(printFloat(0.0, f) == "0");
    assert(printFloat(-0.0, f) == "-0");

    // / 1000 needed due to https://issues.dlang.org/show_bug.cgi?id=20361
    assert(printFloat(1e-307 / 1000, f) == "1e-310");
    assert(printFloat(-1e-307 / 1000, f) == "-1e-310");
    assert(printFloat(1e-30, f) == "1e-30");
    assert(printFloat(-1e-30, f) == "-1e-30");
    assert(printFloat(1e-10, f) == "1e-10");
    assert(printFloat(-1e-10, f) == "-1e-10");
    assert(printFloat(0.1, f) == "0.1");
    assert(printFloat(-0.1, f) == "-0.1");
    assert(printFloat(10.0, f) == "10");
    assert(printFloat(-10.0, f) == "-10");
    assert(printFloat(1e300, f) == "1e+300");
    assert(printFloat(-1e300, f) == "-1e+300");

    import std.math.operations : nextUp, nextDown;
    assert(printFloat(nextUp(0.0), f) == "4.94066e-324");
    assert(printFloat(nextDown(-0.0), f) == "-4.94066e-324");
}

@safe unittest
{
    static if (real.mant_dig > 64)
    {
        pragma(msg, "printFloat tests disabled because of unsupported `real` format");
    }
    else
    {
        char[256] buf;
        auto f = FormatSpec!dchar("");
        f.spec = 'g';

        assert(printFloat(real.nan, f) == "nan");
        assert(printFloat(-real.nan, f) == "-nan");
        assert(printFloat(real.infinity, f) == "inf");
        assert(printFloat(-real.infinity, f) == "-inf");
    }
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'g';

    import std.math.operations : nextUp;

    double eps = nextUp(0.0);
    f.precision = 1000;
    assert(printFloat(eps, f) ==
           "4.940656458412465441765687928682213723650598026143247644255856825006"
           ~ "755072702087518652998363616359923797965646954457177309266567103559"
           ~ "397963987747960107818781263007131903114045278458171678489821036887"
           ~ "186360569987307230500063874091535649843873124733972731696151400317"
           ~ "153853980741262385655911710266585566867681870395603106249319452715"
           ~ "914924553293054565444011274801297099995419319894090804165633245247"
           ~ "571478690147267801593552386115501348035264934720193790268107107491"
           ~ "703332226844753335720832431936092382893458368060106011506169809753"
           ~ "078342277318329247904982524730776375927247874656084778203734469699"
           ~ "533647017972677717585125660551199131504891101451037862738167250955"
           ~ "837389733598993664809941164205702637090279242767544565229087538682"
           ~ "506419718265533447265625e-324");

    f.precision = 50;
    assert(printFloat(double.max, f) ==
           "1.7976931348623157081452742373170435679807056752584e+308");
    assert(printFloat(double.epsilon, f) ==
           "2.220446049250313080847263336181640625e-16");

    f.precision = 10;
    assert(printFloat(1.0/3.0, f) == "0.3333333333");
    assert(printFloat(1.0/7.0, f) == "0.1428571429");
    assert(printFloat(1.0/9.0, f) == "0.1111111111");
}

@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'g';
    f.precision = 15;

    import std.math.constants : E, PI, PI_2, PI_4, M_1_PI, M_2_PI, M_2_SQRTPI,
                                LN10, LN2, LOG2, LOG2E, LOG2T, LOG10E, SQRT2, SQRT1_2;

    assert(printFloat(cast(double) E, f) == "2.71828182845905");
    assert(printFloat(cast(double) PI, f) == "3.14159265358979");
    assert(printFloat(cast(double) PI_2, f) == "1.5707963267949");
    assert(printFloat(cast(double) PI_4, f) == "0.785398163397448");
    assert(printFloat(cast(double) M_1_PI, f) == "0.318309886183791");
    assert(printFloat(cast(double) M_2_PI, f) == "0.636619772367581");
    assert(printFloat(cast(double) M_2_SQRTPI, f) == "1.12837916709551");
    assert(printFloat(cast(double) LN10, f) == "2.30258509299405");
    assert(printFloat(cast(double) LN2, f) == "0.693147180559945");
    assert(printFloat(cast(double) LOG2, f) == "0.301029995663981");
    assert(printFloat(cast(double) LOG2E, f) == "1.44269504088896");
    assert(printFloat(cast(double) LOG2T, f) == "3.32192809488736");
    assert(printFloat(cast(double) LOG10E, f) == "0.434294481903252");
    assert(printFloat(cast(double) SQRT2, f) == "1.4142135623731");
    assert(printFloat(cast(double) SQRT1_2, f) == "0.707106781186548");
}

// for 100% coverage
@safe unittest
{
    auto f = FormatSpec!dchar("");
    f.spec = 'g';
    f.precision = 0;

    assert(printFloat(0.009999, f) == "0.01");
}

@safe unittest
{
    static if (real.mant_dig > 64)
    {
        pragma(msg, "printFloat tests disabled because of unsupported `real` format");
    }
    else
    {
        auto f = FormatSpec!dchar("");
        f.spec = 'g';
        assert(printFloat(real.nan, f) == "nan");
        assert(printFloat(-real.nan, f) == "-nan");
        assert(printFloat(real.infinity, f) == "inf");
        assert(printFloat(-real.infinity, f) == "-inf");
        assert(printFloat(0.0L, f) == "0");
        assert(printFloat(-0.0L, f) == "-0");
    }

    static if (real.mant_dig == 64)
    {
        assert(printFloat(1e-4940L, f) == "1e-4940");
        assert(printFloat(-1e-4940L, f) == "-1e-4940");
        assert(printFloat(1e-30L, f) == "1e-30");
        assert(printFloat(-1e-30L, f) == "-1e-30");
        assert(printFloat(1e-10L, f) == "1e-10");
        assert(printFloat(-1e-10L, f) == "-1e-10");
        assert(printFloat(0.1L, f) == "0.1");
        assert(printFloat(-0.1L, f) == "-0.1");
        assert(printFloat(10.0L, f) == "10");
        assert(printFloat(-10.0L, f) == "-10");
        version (Windows) {} // https://issues.dlang.org/show_bug.cgi?id=20972
        else
        {
            assert(printFloat(1e4000L, f) == "1e+4000");
            assert(printFloat(-1e4000L, f) == "-1e+4000");
        }

        import std.math.operations : nextUp, nextDown;
        assert(printFloat(nextUp(0.0L), f) == "3.6452e-4951");
        assert(printFloat(nextDown(-0.0L), f) == "-3.6452e-4951");
    }
}

@safe unittest
{
    import std.exception : assertCTFEable;
    import std.math.exponential : log2;
    import std.math.operations : nextDown;

    assertCTFEable!(
    {
        static if (real.mant_dig == 64) // 80 bit reals
        {
            // log2 is broken for x87-reals on some computers in CTFE
            // the following tests excludes these computers from the tests
            // (https://issues.dlang.org/show_bug.cgi?id=21757)
            enum test = cast(int) log2(3.05e2312L);
            static if (test == 7681)
            {
                auto f = FormatSpec!dchar("");
                f.spec = 'g';
                assert(printFloat(real.infinity, f) == "inf");
                assert(printFloat(10.0L, f) == "10");
                assert(printFloat(2.6080L, f) == "2.608");
                assert(printFloat(3.05e2312L, f) == "3.05e+2312");

                f.precision = 60;
                assert(printFloat(2.65e-54L, f) ==
                       "2.65000000000000000005900998740054701394102894093529654759941e-54");

                /*
                 commented out, because CTFE is currently too slow for 5000 digits with extreme values

                f.precision = 5000;
                auto result2 = printFloat(1.2119e-4822L, f);
                assert(result2.length == 5007);
                assert(result2[$ - 20 .. $] == "26072948659534e-4822");
                auto result3 = printFloat(real.min_normal, f);
                assert(result3.length == 5007);
                assert(result3[$ - 20 .. $] == "72078141008227e-4932");
                auto result4 = printFloat(real.min_normal.nextDown, f);
                assert(result4.length == 5007);
                assert(result4[$ - 20 .. $] == "48141326333101e-4932");
                 */
            }
        }
    });
}

// check no allocations
@safe unittest
{
    import std.format : NoOpSink;
    auto w = NoOpSink();

    import core.memory;
    auto stats = () @trusted { return GC.stats; } ();

    auto f = FormatSpec!dchar("");
    f.spec = 'a';
    printFloat(w, float.nan, f);
    printFloat(w, -float.infinity, f);
    printFloat(w, 0.0f, f);

    printFloat(w, -double.nan, f);
    printFloat(w, double.infinity, f);
    printFloat(w, -0.0, f);

    import std.math.operations : nextUp;
    import std.math.constants : E;

    printFloat(w, nextUp(0.0f), f);
    printFloat(w, cast(float) E, f);

    f.precision = 1000;
    printFloat(w, float.nan, f);
    printFloat(w, 0.0, f);
    printFloat(w, 1.23456789e+100, f);

    f.spec = 'E';
    f.precision = 80;
    printFloat(w, 5.62776e+12f, f);

    f.precision = 6;
    printFloat(w, -1.1418613e+07f, f);

    f.precision = 20;
    printFloat(w, double.max, f);
    printFloat(w, nextUp(0.0), f);

    f.precision = 1000;
    printFloat(w, 1.0, f);

    f.spec = 'f';
    f.precision = 15;
    printFloat(w, cast(double) E, f);

    f.precision = 20;
    printFloat(w, double.max, f);
    printFloat(w, nextUp(0.0), f);

    f.precision = 1000;
    printFloat(w, 1.0, f);

    f.spec = 'g';
    f.precision = 15;
    printFloat(w, cast(double) E, f);

    f.precision = 20;
    printFloat(w, double.max, f);
    printFloat(w, nextUp(0.0), f);

    f.flHash = true;
    f.precision = 1000;
    printFloat(w, 1.0, f);

    assert(() @trusted { return GC.stats.usedSize; } () == stats.usedSize);
}
