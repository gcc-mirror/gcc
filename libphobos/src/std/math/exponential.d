// Written in the D programming language.

/**
This is a submodule of $(MREF std, math).

It contains several exponential and logarithm functions.

Copyright: Copyright The D Language Foundation 2000 - 2011.
           D implementations of exp, expm1, exp2, log, log10, log1p, and log2
           functions are based on the CEPHES math library, which is Copyright
           (C) 2001 Stephen L. Moshier $(LT)steve@moshier.net$(GT) and are
           incorporated herein by permission of the author. The author reserves
           the right to distribute this material elsewhere under different
           copying permissions. These modifications are distributed here under
           the following terms:
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright), Don Clugston,
           Conversion of CEPHES math library to D by Iain Buclaw and David Nadlinger
Source: $(PHOBOSSRC std/math/exponential.d)

Macros:
    TABLE_SV = <table border="1" cellpadding="4" cellspacing="0">
               <caption>Special Values</caption>
               $0</table>
    NAN = $(RED NAN)
    PLUSMN = &plusmn;
    INFIN = &infin;
    PLUSMNINF = &plusmn;&infin;
    LT = &lt;
    GT = &gt;
 */

module std.math.exponential;

import std.traits :  isFloatingPoint, isIntegral, isSigned, isUnsigned, Largest, Unqual;

static import core.math;
static import core.stdc.math;

version (DigitalMars)
{
    version (OSX) { }             // macOS 13 (M1) has issues emulating instruction
    else version = INLINE_YL2X;   // x87 has opcodes for these
}

version (D_InlineAsm_X86)    version = InlineAsm_X86_Any;
version (D_InlineAsm_X86_64) version = InlineAsm_X86_Any;

version (InlineAsm_X86_Any) version = InlineAsm_X87;
version (InlineAsm_X87)
{
    static assert(real.mant_dig == 64);
    version (CRuntime_Microsoft) version = InlineAsm_X87_MSVC;
}

version (D_HardFloat)
{
    // FloatingPointControl.clearExceptions() depends on version IeeeFlagsSupport
    version (IeeeFlagsSupport) version = FloatingPointControlSupport;
}

/**
 * Compute the value of x $(SUPERSCRIPT n), where n is an integer
 */
Unqual!F pow(F, G)(F x, G n) @nogc @trusted pure nothrow
if (isFloatingPoint!(F) && isIntegral!(G))
{
    import std.traits : Unsigned;

    real p = 1.0, v = void;
    Unsigned!(Unqual!G) m = n;

    if (n < 0)
    {
        if (n == -1) return 1 / x;

        m = cast(typeof(m))(0 - n);
        v = p / x;
    }
    else
    {
        switch (n)
        {
        case 0:
            return 1.0;
        case 1:
            return x;
        case 2:
            return x * x;
        default:
        }

        v = x;
    }

    while (1)
    {
        if (m & 1)
            p *= v;
        m >>= 1;
        if (!m)
            break;
        v *= v;
    }
    return p;
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : feqrel;

    assert(pow(2.0, 5) == 32.0);
    assert(pow(1.5, 9).feqrel(38.4433) > 16);
    assert(pow(real.nan, 2) is real.nan);
    assert(pow(real.infinity, 2) == real.infinity);
}

@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose, feqrel;

    // Make sure it instantiates and works properly on immutable values and
    // with various integer and float types.
    immutable real x = 46;
    immutable float xf = x;
    immutable double xd = x;
    immutable uint one = 1;
    immutable ushort two = 2;
    immutable ubyte three = 3;
    immutable ulong eight = 8;

    immutable int neg1 = -1;
    immutable short neg2 = -2;
    immutable byte neg3 = -3;
    immutable long neg8 = -8;


    assert(pow(x,0) == 1.0);
    assert(pow(xd,one) == x);
    assert(pow(xf,two) == x * x);
    assert(pow(x,three) == x * x * x);
    assert(pow(x,eight) == (x * x) * (x * x) * (x * x) * (x * x));

    assert(pow(x, neg1) == 1 / x);

    assert(isClose(pow(xd, neg2), cast(double) (1 / (x * x)), 1e-15));
    assert(isClose(pow(xf, neg8), cast(float) (1 / ((x * x) * (x * x) * (x * x) * (x * x))), 1e-15));

    assert(feqrel(pow(x, neg3),  1 / (x * x * x)) >= real.mant_dig - 1);
}

@safe @nogc nothrow unittest
{
    import std.math.operations : isClose;

    assert(isClose(pow(2.0L, 10L), 1024, 1e-18));
}

// https://issues.dlang.org/show_bug.cgi?id=21601
@safe @nogc nothrow pure unittest
{
    // When reals are large enough the results of pow(b, e) can be
    // calculated correctly, if b is of type float or double and e is
    // not too large.
    static if (real.mant_dig >= 64)
    {
        // expected result: 3.790e-42
        assert(pow(-513645318757045764096.0f, -2) > 0.0);

        // expected result: 3.763915357831797e-309
        assert(pow(-1.6299717435255677e+154, -2) > 0.0);
    }
}

@safe @nogc nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isInfinity;

    static float f1 = 19100.0f;
    static float f2 = 0.000012f;

    assert(isClose(pow(f1,9), 3.3829868e+38f));
    assert(isInfinity(pow(f1,10)));
    assert(pow(f2,9) > 0.0f);
    assert(isClose(pow(f2,10), 0.0f, 0.0, float.min_normal));

    static double d1 = 21800.0;
    static double d2 = 0.000012;

    assert(isClose(pow(d1,71), 1.0725339442974e+308));
    assert(isInfinity(pow(d1,72)));
    assert(pow(d2,65) > 0.0f);
    assert(isClose(pow(d2,66), 0.0, 0.0, double.min_normal));

    static if (real.mant_dig == 64) // x87
    {
        static real r1 = 21950.0L;
        static real r2 = 0.000011L;

        assert(isClose(pow(r1,1136), 7.4066175654969242752260330529e+4931L));
        assert(isInfinity(pow(r1,1137)));
        assert(pow(r2,998) > 0.0L);
        assert(isClose(pow(r2,999), 0.0L, 0.0, real.min_normal));
    }
}

@safe @nogc nothrow pure unittest
{
    import std.math.operations : isClose;

    enum f1 = 19100.0f;
    enum f2 = 0.000012f;

    static assert(isClose(pow(f1,9), 3.3829868e+38f));
    static assert(pow(f1,10) > float.max);
    static assert(pow(f2,9) > 0.0f);
    static assert(isClose(pow(f2,10), 0.0f, 0.0, float.min_normal));

    enum d1 = 21800.0;
    enum d2 = 0.000012;

    static assert(isClose(pow(d1,71), 1.0725339442974e+308));
    static assert(pow(d1,72) > double.max);
    static assert(pow(d2,65) > 0.0f);
    static assert(isClose(pow(d2,66), 0.0, 0.0, double.min_normal));

    static if (real.mant_dig == 64) // x87
    {
        enum r1 = 21950.0L;
        enum r2 = 0.000011L;

        static assert(isClose(pow(r1,1136), 7.4066175654969242752260330529e+4931L));
        static assert(pow(r1,1137) > real.max);
        static assert(pow(r2,998) > 0.0L);
        static assert(isClose(pow(r2,999), 0.0L, 0.0, real.min_normal));
    }
}

/**
 * Compute the power of two integral numbers.
 *
 * Params:
 *     x = base
 *     n = exponent
 *
 * Returns:
 *     x raised to the power of n. If n is negative the result is 1 / pow(x, -n),
 *     which is calculated as integer division with remainder. This may result in
 *     a division by zero error.
 *
 *     If both x and n are 0, the result is 1.
 *
 * Throws:
 *     If x is 0 and n is negative, the result is the same as the result of a
 *     division by zero.
 */
typeof(Unqual!(F).init * Unqual!(G).init) pow(F, G)(F x, G n) @nogc @safe pure nothrow
if (isIntegral!(F) && isIntegral!(G))
{
    import std.traits : isSigned;

    typeof(return) p, v = void;
    Unqual!G m = n;

    static if (isSigned!(F))
    {
        if (x == -1) return cast(typeof(return)) (m & 1 ? -1 : 1);
    }
    static if (isSigned!(G))
    {
        if (x == 0 && m <= -1) return x / 0;
    }
    if (x == 1) return 1;
    static if (isSigned!(G))
    {
        if (m < 0) return 0;
    }

    switch (m)
    {
    case 0:
        p = 1;
        break;

    case 1:
        p = x;
        break;

    case 2:
        p = x * x;
        break;

    default:
        v = x;
        p = 1;
        while (1)
        {
            if (m & 1)
                p *= v;
            m >>= 1;
            if (!m)
                break;
            v *= v;
        }
        break;
    }
    return p;
}

///
@safe pure nothrow @nogc unittest
{
    assert(pow(2, 3) == 8);
    assert(pow(3, 2) == 9);

    assert(pow(2, 10) == 1_024);
    assert(pow(2, 20) == 1_048_576);
    assert(pow(2, 30) == 1_073_741_824);

    assert(pow(0, 0) == 1);

    assert(pow(1, -5) == 1);
    assert(pow(1, -6) == 1);
    assert(pow(-1, -5) == -1);
    assert(pow(-1, -6) == 1);

    assert(pow(-2, 5) == -32);
    assert(pow(-2, -5) == 0);
    assert(pow(cast(double) -2, -5) == -0.03125);
}

@safe pure nothrow @nogc unittest
{
    immutable int one = 1;
    immutable byte two = 2;
    immutable ubyte three = 3;
    immutable short four = 4;
    immutable long ten = 10;

    assert(pow(two, three) == 8);
    assert(pow(two, ten) == 1024);
    assert(pow(one, ten) == 1);
    assert(pow(ten, four) == 10_000);
    assert(pow(four, 10) == 1_048_576);
    assert(pow(three, four) == 81);
}

// https://issues.dlang.org/show_bug.cgi?id=7006
@safe pure nothrow @nogc unittest
{
    assert(pow(5, -1) == 0);
    assert(pow(-5, -1) == 0);
    assert(pow(5, -2) == 0);
    assert(pow(-5, -2) == 0);
    assert(pow(-1, int.min) == 1);
    assert(pow(-2, int.min) == 0);

    assert(pow(4294967290UL,2) == 18446744022169944100UL);
    assert(pow(0,uint.max) == 0);
}

/**Computes integer to floating point powers.*/
real pow(I, F)(I x, F y) @nogc @trusted pure nothrow
if (isIntegral!I && isFloatingPoint!F)
{
    return pow(cast(real) x, cast(Unqual!F) y);
}

///
@safe pure nothrow @nogc unittest
{
    assert(pow(2, 5.0) == 32.0);
    assert(pow(7, 3.0) == 343.0);
    assert(pow(2, real.nan) is real.nan);
    assert(pow(2, real.infinity) == real.infinity);
}

/**
 * Calculates x$(SUPERSCRIPT y).
 *
 * $(TABLE_SV
 * $(TR $(TH x) $(TH y) $(TH pow(x, y))
 *      $(TH div 0) $(TH invalid?))
 * $(TR $(TD anything)      $(TD $(PLUSMN)0.0)                $(TD 1.0)
 *      $(TD no)        $(TD no) )
 * $(TR $(TD |x| $(GT) 1)    $(TD +$(INFIN))                  $(TD +$(INFIN))
 *      $(TD no)        $(TD no) )
 * $(TR $(TD |x| $(LT) 1)    $(TD +$(INFIN))                  $(TD +0.0)
 *      $(TD no)        $(TD no) )
 * $(TR $(TD |x| $(GT) 1)    $(TD -$(INFIN))                  $(TD +0.0)
 *      $(TD no)        $(TD no) )
 * $(TR $(TD |x| $(LT) 1)    $(TD -$(INFIN))                  $(TD +$(INFIN))
 *      $(TD no)        $(TD no) )
 * $(TR $(TD +$(INFIN))      $(TD $(GT) 0.0)                  $(TD +$(INFIN))
 *      $(TD no)        $(TD no) )
 * $(TR $(TD +$(INFIN))      $(TD $(LT) 0.0)                  $(TD +0.0)
 *      $(TD no)        $(TD no) )
 * $(TR $(TD -$(INFIN))      $(TD odd integer $(GT) 0.0)      $(TD -$(INFIN))
 *      $(TD no)        $(TD no) )
 * $(TR $(TD -$(INFIN))      $(TD $(GT) 0.0, not odd integer) $(TD +$(INFIN))
 *      $(TD no)        $(TD no))
 * $(TR $(TD -$(INFIN))      $(TD odd integer $(LT) 0.0)      $(TD -0.0)
 *      $(TD no)        $(TD no) )
 * $(TR $(TD -$(INFIN))      $(TD $(LT) 0.0, not odd integer) $(TD +0.0)
 *      $(TD no)        $(TD no) )
 * $(TR $(TD $(PLUSMN)1.0)   $(TD $(PLUSMN)$(INFIN))          $(TD -$(NAN))
 *      $(TD no)        $(TD yes) )
 * $(TR $(TD $(LT) 0.0)      $(TD finite, nonintegral)        $(TD $(NAN))
 *      $(TD no)        $(TD yes))
 * $(TR $(TD $(PLUSMN)0.0)   $(TD odd integer $(LT) 0.0)      $(TD $(PLUSMNINF))
 *      $(TD yes)       $(TD no) )
 * $(TR $(TD $(PLUSMN)0.0)   $(TD $(LT) 0.0, not odd integer) $(TD +$(INFIN))
 *      $(TD yes)       $(TD no))
 * $(TR $(TD $(PLUSMN)0.0)   $(TD odd integer $(GT) 0.0)      $(TD $(PLUSMN)0.0)
 *      $(TD no)        $(TD no) )
 * $(TR $(TD $(PLUSMN)0.0)   $(TD $(GT) 0.0, not odd integer) $(TD +0.0)
 *      $(TD no)        $(TD no) )
 * )
 */
Unqual!(Largest!(F, G)) pow(F, G)(F x, G y) @nogc @trusted pure nothrow
if (isFloatingPoint!(F) && isFloatingPoint!(G))
{
    return _powImpl(x, y);
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;

    assert(isClose(pow(2.0, 3.0), 8.0));
    assert(isClose(pow(1.5, 10.0), 57.6650390625));

    // square root of 9
    assert(isClose(pow(9.0, 0.5), 3.0));
    // 10th root of 1024
    assert(isClose(pow(1024.0, 0.1), 2.0));

    assert(isClose(pow(-4.0, 3.0), -64.0));

    // reciprocal of 4 ^^ 2
    assert(isClose(pow(4.0, -2.0), 0.0625));
    // reciprocal of (-2) ^^ 3
    assert(isClose(pow(-2.0, -3.0), -0.125));

    assert(isClose(pow(-2.5, 3.0), -15.625));
    // reciprocal of 2.5 ^^ 3
    assert(isClose(pow(2.5, -3.0), 0.064));
    // reciprocal of (-2.5) ^^ 3
    assert(isClose(pow(-2.5, -3.0), -0.064));

    // reciprocal of square root of 4
    assert(isClose(pow(4.0, -0.5), 0.5));

    // per definition
    assert(isClose(pow(0.0, 0.0), 1.0));
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;

    // the result is a complex number
    // which cannot be represented as floating point number
    import std.math.traits : isNaN;
    assert(isNaN(pow(-2.5, -1.5)));

    // use the ^^-operator of std.complex instead
    import std.complex : complex;
    auto c1 = complex(-2.5, 0.0);
    auto c2 = complex(-1.5, 0.0);
    auto result = c1 ^^ c2;
    // exact result apparently depends on `real` precision => increased tolerance
    assert(isClose(result.re, -4.64705438e-17, 2e-4));
    assert(isClose(result.im, 2.52982e-1, 2e-4));
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN;

    assert(pow(1.5, real.infinity) == real.infinity);
    assert(pow(0.5, real.infinity) == 0.0);
    assert(pow(1.5, -real.infinity) == 0.0);
    assert(pow(0.5, -real.infinity) == real.infinity);
    assert(pow(real.infinity, 1.0) == real.infinity);
    assert(pow(real.infinity, -1.0) == 0.0);
    assert(pow(real.infinity, real.infinity) == real.infinity);
    assert(pow(-real.infinity, 1.0) == -real.infinity);
    assert(pow(-real.infinity, 2.0) == real.infinity);
    assert(pow(-real.infinity, -1.0) == -0.0);
    assert(pow(-real.infinity, -2.0) == 0.0);
    assert(isNaN(pow(1.0, real.infinity)));
    assert(pow(0.0, -1.0) == real.infinity);
    assert(pow(real.nan, 0.0) == 1.0);
    assert(isNaN(pow(real.nan, 3.0)));
    assert(isNaN(pow(3.0, real.nan)));
}

@safe @nogc nothrow unittest
{
    import std.math.operations : isClose;

    assert(isClose(pow(2.0L, 10.0L), 1024, 1e-18));
}

@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isIdentical, isNaN;
    import std.math.constants : PI;

    // Test all the special values.  These unittests can be run on Windows
    // by temporarily changing the version (linux) to version (all).
    immutable float zero = 0;
    immutable real one = 1;
    immutable double two = 2;
    immutable float three = 3;
    immutable float fnan = float.nan;
    immutable double dnan = double.nan;
    immutable real rnan = real.nan;
    immutable dinf = double.infinity;
    immutable rninf = -real.infinity;

    assert(pow(fnan, zero) == 1);
    assert(pow(dnan, zero) == 1);
    assert(pow(rnan, zero) == 1);

    assert(pow(two, dinf) == double.infinity);
    assert(isIdentical(pow(0.2f, dinf), +0.0));
    assert(pow(0.99999999L, rninf) == real.infinity);
    assert(isIdentical(pow(1.000000001, rninf), +0.0));
    assert(pow(dinf, 0.001) == dinf);
    assert(isIdentical(pow(dinf, -0.001), +0.0));
    assert(pow(rninf, 3.0L) == rninf);
    assert(pow(rninf, 2.0L) == real.infinity);
    assert(isIdentical(pow(rninf, -3.0), -0.0));
    assert(isIdentical(pow(rninf, -2.0), +0.0));

    // @@@BUG@@@ somewhere
    version (OSX) {} else assert(isNaN(pow(one, dinf)));
    version (OSX) {} else assert(isNaN(pow(-one, dinf)));
    assert(isNaN(pow(-0.2, PI)));
    // boundary cases. Note that epsilon == 2^^-n for some n,
    // so 1/epsilon == 2^^n is always even.
    assert(pow(-1.0L, 1/real.epsilon - 1.0L) == -1.0L);
    assert(pow(-1.0L, 1/real.epsilon) == 1.0L);
    assert(isNaN(pow(-1.0L, 1/real.epsilon-0.5L)));
    assert(isNaN(pow(-1.0L, -1/real.epsilon+0.5L)));

    assert(pow(0.0, -3.0) == double.infinity);
    assert(pow(-0.0, -3.0) == -double.infinity);
    assert(pow(0.0, -PI) == double.infinity);
    assert(pow(-0.0, -PI) == double.infinity);
    assert(isIdentical(pow(0.0, 5.0), 0.0));
    assert(isIdentical(pow(-0.0, 5.0), -0.0));
    assert(isIdentical(pow(0.0, 6.0), 0.0));
    assert(isIdentical(pow(-0.0, 6.0), 0.0));

    // https://issues.dlang.org/show_bug.cgi?id=14786 fixed
    immutable real maxOdd = pow(2.0L, real.mant_dig) - 1.0L;
    assert(pow(-1.0L,  maxOdd) == -1.0L);
    assert(pow(-1.0L, -maxOdd) == -1.0L);
    assert(pow(-1.0L, maxOdd + 1.0L) == 1.0L);
    assert(pow(-1.0L, -maxOdd + 1.0L) == 1.0L);
    assert(pow(-1.0L, maxOdd - 1.0L) == 1.0L);
    assert(pow(-1.0L, -maxOdd - 1.0L) == 1.0L);

    // Now, actual numbers.
    assert(isClose(pow(two, three), 8.0));
    assert(isClose(pow(two, -2.5), 0.1767766953));

    // Test integer to float power.
    immutable uint twoI = 2;
    assert(isClose(pow(twoI, three), 8.0));
}

// https://issues.dlang.org/show_bug.cgi?id=20508
@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN;

    assert(isNaN(pow(-double.infinity, 0.5)));

    assert(isNaN(pow(-real.infinity, real.infinity)));
    assert(isNaN(pow(-real.infinity, -real.infinity)));
    assert(isNaN(pow(-real.infinity, 1.234)));
    assert(isNaN(pow(-real.infinity, -0.751)));
    assert(pow(-real.infinity, 0.0) == 1.0);
}

private real _powImpl(real x, real y) @safe @nogc pure nothrow
{
    alias F = real;
    import core.math : fabs, sqrt;
    import std.math.traits : isInfinity, isNaN, signbit;

    // Special cases.
    if (isNaN(y))
        return y;
    if (isNaN(x) && y != 0.0)
        return x;

    // Even if x is NaN.
    if (y == 0.0)
        return 1.0;
    if (y == 1.0)
        return x;

    if (isInfinity(y))
    {
        if (isInfinity(x))
        {
            if (!signbit(y) && !signbit(x))
                return F.infinity;
            else
                return F.nan;
        }
        else if (fabs(x) > 1)
        {
            if (signbit(y))
                return +0.0;
            else
                return F.infinity;
        }
        else if (fabs(x) == 1)
        {
            return F.nan;
        }
        else // < 1
        {
            if (signbit(y))
                return F.infinity;
            else
                return +0.0;
        }
    }
    if (isInfinity(x))
    {
        if (signbit(x))
        {
            long i = cast(long) y;
            if (y > 0.0)
            {
                if (i == y && i & 1)
                    return -F.infinity;
                else if (i == y)
                    return F.infinity;
                else
                    return -F.nan;
            }
            else if (y < 0.0)
            {
                if (i == y && i & 1)
                    return -0.0;
                else if (i == y)
                    return +0.0;
                else
                    return F.nan;
            }
        }
        else
        {
            if (y > 0.0)
                return F.infinity;
            else if (y < 0.0)
                return +0.0;
        }
    }

    if (x == 0.0)
    {
        if (signbit(x))
        {
            long i = cast(long) y;
            if (y > 0.0)
            {
                if (i == y && i & 1)
                    return -0.0;
                else
                    return +0.0;
            }
            else if (y < 0.0)
            {
                if (i == y && i & 1)
                    return -F.infinity;
                else
                    return F.infinity;
            }
        }
        else
        {
            if (y > 0.0)
                return +0.0;
            else if (y < 0.0)
                return F.infinity;
        }
    }
    if (x == 1.0)
        return 1.0;

    if (y >= F.max)
    {
        if ((x > 0.0 && x < 1.0) || (x > -1.0 && x < 0.0))
            return 0.0;
        if (x > 1.0 || x < -1.0)
            return F.infinity;
    }
    if (y <= -F.max)
    {
        if ((x > 0.0 && x < 1.0) || (x > -1.0 && x < 0.0))
            return F.infinity;
        if (x > 1.0 || x < -1.0)
            return 0.0;
    }

    if (x >= F.max)
    {
        if (y > 0.0)
            return F.infinity;
        else
            return 0.0;
    }
    if (x <= -F.max)
    {
        long i = cast(long) y;
        if (y > 0.0)
        {
            if (i == y && i & 1)
                return -F.infinity;
            else
                return F.infinity;
        }
        else if (y < 0.0)
        {
            if (i == y && i & 1)
                return -0.0;
            else
                return +0.0;
        }
    }

    // Integer power of x.
    long iy = cast(long) y;
    if (iy == y && fabs(y) < 32_768.0)
        return pow(x, iy);

    real sign = 1.0;
    if (x < 0)
    {
        // Result is real only if y is an integer
        // Check for a non-zero fractional part
        enum maxOdd = pow(2.0L, real.mant_dig) - 1.0L;
        static if (maxOdd > ulong.max)
        {
            // Generic method, for any FP type
            import std.math.rounding : floor;
            if (floor(y) != y)
                return sqrt(x); // Complex result -- create a NaN

            const hy = 0.5 * y;
            if (floor(hy) != hy)
                sign = -1.0;
        }
        else
        {
            // Much faster, if ulong has enough precision
            const absY = fabs(y);
            if (absY <= maxOdd)
            {
                const uy = cast(ulong) absY;
                if (uy != absY)
                    return sqrt(x); // Complex result -- create a NaN

                if (uy & 1)
                    sign = -1.0;
            }
        }
        x = -x;
    }
    version (INLINE_YL2X)
    {
        // If x > 0, x ^^ y == 2 ^^ ( y * log2(x) )
        // TODO: This is not accurate in practice. A fast and accurate
        // (though complicated) method is described in:
        // "An efficient rounding boundary test for pow(x, y)
        // in double precision", C.Q. Lauter and V. Lefèvre, INRIA (2007).
        return sign * exp2( core.math.yl2x(x, y) );
    }
    else
    {
        // If x > 0, x ^^ y == 2 ^^ ( y * log2(x) )
        // TODO: This is not accurate in practice. A fast and accurate
        // (though complicated) method is described in:
        // "An efficient rounding boundary test for pow(x, y)
        // in double precision", C.Q. Lauter and V. Lefèvre, INRIA (2007).
        auto w = exp2(y * log2(x));
        return sign * w;
    }
}

/** Computes the value of a positive integer `x`, raised to the power `n`, modulo `m`.
 *
 *  Params:
 *      x = base
 *      n = exponent
 *      m = modulus
 *
 *  Returns:
 *      `x` to the power `n`, modulo `m`.
 *      The return type is the largest of `x`'s and `m`'s type.
 *
 * The function requires that all values have unsigned types.
 */
Unqual!(Largest!(F, H)) powmod(F, G, H)(F x, G n, H m)
if (isUnsigned!F && isUnsigned!G && isUnsigned!H)
{
    import std.meta : AliasSeq;

    alias T = Unqual!(Largest!(F, H));
    static if (T.sizeof <= 4)
    {
        alias DoubleT = AliasSeq!(void, ushort, uint, void, ulong)[T.sizeof];
    }

    static T mulmod(T a, T b, T c)
    {
        static if (T.sizeof == 8)
        {
            static T addmod(T a, T b, T c)
            {
                b = c - b;
                if (a >= b)
                    return a - b;
                else
                    return c - b + a;
            }

            T result = 0, tmp;

            b %= c;
            while (a > 0)
            {
                if (a & 1)
                    result = addmod(result, b, c);

                a >>= 1;
                b = addmod(b, b, c);
            }

            return result;
        }
        else
        {
            DoubleT result = cast(DoubleT) (cast(DoubleT) a * cast(DoubleT) b);
            return result % c;
        }
    }

    T base = x, result = 1, modulus = m;
    Unqual!G exponent = n;

    while (exponent > 0)
    {
        if (exponent & 1)
            result = mulmod(result, base, modulus);

        base = mulmod(base, base, modulus);
        exponent >>= 1;
    }

    return result;
}

///
@safe pure nothrow @nogc unittest
{
    assert(powmod(1U, 10U, 3U) == 1);
    assert(powmod(3U, 2U, 6U) == 3);
    assert(powmod(5U, 5U, 15U) == 5);
    assert(powmod(2U, 3U, 5U) == 3);
    assert(powmod(2U, 4U, 5U) == 1);
    assert(powmod(2U, 5U, 5U) == 2);
}

@safe pure nothrow @nogc unittest
{
    ulong a = 18446744073709551615u, b = 20u, c = 18446744073709551610u;
    assert(powmod(a, b, c) == 95367431640625u);
    a = 100; b = 7919; c = 18446744073709551557u;
    assert(powmod(a, b, c) == 18223853583554725198u);
    a = 117; b = 7919; c = 18446744073709551557u;
    assert(powmod(a, b, c) == 11493139548346411394u);
    a = 134; b = 7919; c = 18446744073709551557u;
    assert(powmod(a, b, c) == 10979163786734356774u);
    a = 151; b = 7919; c = 18446744073709551557u;
    assert(powmod(a, b, c) == 7023018419737782840u);
    a = 168; b = 7919; c = 18446744073709551557u;
    assert(powmod(a, b, c) == 58082701842386811u);
    a = 185; b = 7919; c = 18446744073709551557u;
    assert(powmod(a, b, c) == 17423478386299876798u);
    a = 202; b = 7919; c = 18446744073709551557u;
    assert(powmod(a, b, c) == 5522733478579799075u);
    a = 219; b = 7919; c = 18446744073709551557u;
    assert(powmod(a, b, c) == 15230218982491623487u);
    a = 236; b = 7919; c = 18446744073709551557u;
    assert(powmod(a, b, c) == 5198328724976436000u);

    a = 0; b = 7919; c = 18446744073709551557u;
    assert(powmod(a, b, c) == 0);
    a = 123; b = 0; c = 18446744073709551557u;
    assert(powmod(a, b, c) == 1);

    immutable ulong a1 = 253, b1 = 7919, c1 = 18446744073709551557u;
    assert(powmod(a1, b1, c1) == 3883707345459248860u);

    uint x = 100 ,y = 7919, z = 1844674407u;
    assert(powmod(x, y, z) == 1613100340u);
    x = 134; y = 7919; z = 1844674407u;
    assert(powmod(x, y, z) == 734956622u);
    x = 151; y = 7919; z = 1844674407u;
    assert(powmod(x, y, z) == 1738696945u);
    x = 168; y = 7919; z = 1844674407u;
    assert(powmod(x, y, z) == 1247580927u);
    x = 185; y = 7919; z = 1844674407u;
    assert(powmod(x, y, z) == 1293855176u);
    x = 202; y = 7919; z = 1844674407u;
    assert(powmod(x, y, z) == 1566963682u);
    x = 219; y = 7919; z = 1844674407u;
    assert(powmod(x, y, z) == 181227807u);
    x = 236; y = 7919; z = 1844674407u;
    assert(powmod(x, y, z) == 217988321u);
    x = 253; y = 7919; z = 1844674407u;
    assert(powmod(x, y, z) == 1588843243u);

    x = 0; y = 7919; z = 184467u;
    assert(powmod(x, y, z) == 0);
    x = 123; y = 0; z = 1844674u;
    assert(powmod(x, y, z) == 1);

    immutable ubyte x1 = 117;
    immutable uint y1 = 7919;
    immutable uint z1 = 1844674407u;
    auto res = powmod(x1, y1, z1);
    assert(is(typeof(res) == uint));
    assert(res == 9479781u);

    immutable ushort x2 = 123;
    immutable uint y2 = 203;
    immutable ubyte z2 = 113;
    auto res2 = powmod(x2, y2, z2);
    assert(is(typeof(res2) == ushort));
    assert(res2 == 42u);
}

/**
 * Calculates e$(SUPERSCRIPT x).
 *
 *  $(TABLE_SV
 *    $(TR $(TH x)             $(TH e$(SUPERSCRIPT x)) )
 *    $(TR $(TD +$(INFIN))     $(TD +$(INFIN)) )
 *    $(TR $(TD -$(INFIN))     $(TD +0.0)      )
 *    $(TR $(TD $(NAN))        $(TD $(NAN))    )
 *  )
 */
pragma(inline, true)
real exp(real x) @trusted pure nothrow @nogc // TODO: @safe
{
    version (InlineAsm_X87)
    {
        import std.math.constants : LOG2E;

        //  e^^x = 2^^(LOG2E*x)
        // (This is valid because the overflow & underflow limits for exp
        // and exp2 are so similar).
        if (!__ctfe)
            return exp2Asm(LOG2E*x);
    }
    return expImpl(x);
}

/// ditto
pragma(inline, true)
double exp(double x) @safe pure nothrow @nogc { return __ctfe ? cast(double) exp(cast(real) x) : expImpl(x); }

/// ditto
pragma(inline, true)
float exp(float x) @safe pure nothrow @nogc { return __ctfe ? cast(float) exp(cast(real) x) : expImpl(x); }

///
@safe unittest
{
    import std.math.operations : feqrel;
    import std.math.constants : E;

    assert(exp(0.0) == 1.0);
    assert(exp(3.0).feqrel(E * E * E) > 16);
}

private T expImpl(T)(T x) @safe pure nothrow @nogc
{
    import std.math.traits : floatTraits, RealFormat, isNaN;
    import std.math.rounding : floor;
    import std.math.algebraic : poly;
    import std.math.constants : LOG2E;

    alias F = floatTraits!T;
    static if (F.realFormat == RealFormat.ieeeSingle)
    {
        static immutable T[6] P = [
            5.0000001201E-1,
            1.6666665459E-1,
            4.1665795894E-2,
            8.3334519073E-3,
            1.3981999507E-3,
            1.9875691500E-4,
        ];

        enum T C1 = 0.693359375;
        enum T C2 = -2.12194440e-4;

        // Overflow and Underflow limits.
        enum T OF = 88.72283905206835;
        enum T UF = -103.278929903431851103; // ln(2^-149)
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        // Coefficients for exp(x)
        static immutable T[3] P = [
            9.99999999999999999910E-1L,
            3.02994407707441961300E-2L,
            1.26177193074810590878E-4L,
        ];
        static immutable T[4] Q = [
            2.00000000000000000009E0L,
            2.27265548208155028766E-1L,
            2.52448340349684104192E-3L,
            3.00198505138664455042E-6L,
        ];

        // C1 + C2 = LN2.
        enum T C1 = 6.93145751953125E-1;
        enum T C2 = 1.42860682030941723212E-6;

        // Overflow and Underflow limits.
        enum T OF =  7.09782712893383996732E2;  // ln((1-2^-53) * 2^1024)
        enum T UF = -7.451332191019412076235E2; // ln(2^-1075)
    }
    else static if (F.realFormat == RealFormat.ieeeExtended ||
                    F.realFormat == RealFormat.ieeeExtended53)
    {
        // Coefficients for exp(x)
        static immutable T[3] P = [
            9.9999999999999999991025E-1L,
            3.0299440770744196129956E-2L,
            1.2617719307481059087798E-4L,
        ];
        static immutable T[4] Q = [
            2.0000000000000000000897E0L,
            2.2726554820815502876593E-1L,
            2.5244834034968410419224E-3L,
            3.0019850513866445504159E-6L,
        ];

        // C1 + C2 = LN2.
        enum T C1 = 6.9314575195312500000000E-1L;
        enum T C2 = 1.4286068203094172321215E-6L;

        // Overflow and Underflow limits.
        enum T OF =  1.1356523406294143949492E4L;  // ln((1-2^-64) * 2^16384)
        enum T UF = -1.13994985314888605586758E4L; // ln(2^-16446)
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple)
    {
        // Coefficients for exp(x) - 1
        static immutable T[5] P = [
            9.999999999999999999999999999999999998502E-1L,
            3.508710990737834361215404761139478627390E-2L,
            2.708775201978218837374512615596512792224E-4L,
            6.141506007208645008909088812338454698548E-7L,
            3.279723985560247033712687707263393506266E-10L
        ];
        static immutable T[6] Q = [
            2.000000000000000000000000000000000000150E0,
            2.368408864814233538909747618894558968880E-1L,
            3.611828913847589925056132680618007270344E-3L,
            1.504792651814944826817779302637284053660E-5L,
            1.771372078166251484503904874657985291164E-8L,
            2.980756652081995192255342779918052538681E-12L
        ];

        // C1 + C2 = LN2.
        enum T C1 = 6.93145751953125E-1L;
        enum T C2 = 1.428606820309417232121458176568075500134E-6L;

        // Overflow and Underflow limits.
        enum T OF =  1.135583025911358400418251384584930671458833e4L;
        enum T UF = -1.143276959615573793352782661133116431383730e4L;
    }
    else
        static assert(0, "Not implemented for this architecture");

    // Special cases.
    if (isNaN(x))
        return x;
    if (x > OF)
        return real.infinity;
    if (x < UF)
        return 0.0;

    // Express: e^^x = e^^g * 2^^n
    //   = e^^g * e^^(n * LOG2E)
    //   = e^^(g + n * LOG2E)
    T xx = floor((cast(T) LOG2E) * x + cast(T) 0.5);
    const int n = cast(int) xx;
    x -= xx * C1;
    x -= xx * C2;

    static if (F.realFormat == RealFormat.ieeeSingle)
    {
        xx = x * x;
        x = poly(x, P) * xx + x + 1.0f;
    }
    else
    {
        // Rational approximation for exponential of the fractional part:
        //  e^^x = 1 + 2x P(x^^2) / (Q(x^^2) - P(x^^2))
        xx = x * x;
        const T px = x * poly(xx, P);
        x = px / (poly(xx, Q) - px);
        x = (cast(T) 1.0) + (cast(T) 2.0) * x;
    }

    // Scale by power of 2.
    x = core.math.ldexp(x, n);

    return x;
}

@safe @nogc nothrow unittest
{
    import std.math.traits : floatTraits, RealFormat;
    import std.math.operations : NaN, feqrel, isClose;
    import std.math.constants : E;
    import std.math.traits : isIdentical;
    import std.math.algebraic : abs;

    version (IeeeFlagsSupport) import std.math.hardware : IeeeFlags, resetIeeeFlags, ieeeFlags;
    version (FloatingPointControlSupport)
    {
        import std.math.hardware : FloatingPointControl;

        FloatingPointControl ctrl;
        if (FloatingPointControl.hasExceptionTraps)
            ctrl.disableExceptions(FloatingPointControl.allExceptions);
        ctrl.rounding = FloatingPointControl.roundToNearest;
    }

    static void testExp(T)()
    {
        enum realFormat = floatTraits!T.realFormat;
        static if (realFormat == RealFormat.ieeeQuadruple)
        {
            static immutable T[2][] exptestpoints =
            [ //  x               exp(x)
                [ 1.0L,           E                                        ],
                [ 0.5L,           0x1.a61298e1e069bc972dfefab6df34p+0L     ],
                [ 3.0L,           E*E*E                                    ],
                [ 0x1.6p+13L,     0x1.6e509d45728655cdb4840542acb5p+16250L ], // near overflow
                [ 0x1.7p+13L,     T.infinity                               ], // close overflow
                [ 0x1p+80L,       T.infinity                               ], // far overflow
                [ T.infinity,     T.infinity                               ],
                [-0x1.18p+13L,    0x1.5e4bf54b4807034ea97fef0059a6p-12927L ], // near underflow
                [-0x1.625p+13L,   0x1.a6bd68a39d11fec3a250cd97f524p-16358L ], // ditto
                [-0x1.62dafp+13L, 0x0.cb629e9813b80ed4d639e875be6cp-16382L ], // near underflow - subnormal
                [-0x1.6549p+13L,  0x0.0000000000000000000000000001p-16382L ], // ditto
                [-0x1.655p+13L,   0                                        ], // close underflow
                [-0x1p+30L,       0                                        ], // far underflow
            ];
        }
        else static if (realFormat == RealFormat.ieeeExtended ||
                        realFormat == RealFormat.ieeeExtended53)
        {
            static immutable T[2][] exptestpoints =
            [ //  x               exp(x)
                [ 1.0L,           E                            ],
                [ 0.5L,           0x1.a61298e1e069bc97p+0L     ],
                [ 3.0L,           E*E*E                        ],
                [ 0x1.1p+13L,     0x1.29aeffefc8ec645p+12557L  ], // near overflow
                [ 0x1.7p+13L,     T.infinity                   ], // close overflow
                [ 0x1p+80L,       T.infinity                   ], // far overflow
                [ T.infinity,     T.infinity                   ],
                [-0x1.18p+13L,    0x1.5e4bf54b4806db9p-12927L  ], // near underflow
                [-0x1.625p+13L,   0x1.a6bd68a39d11f35cp-16358L ], // ditto
                [-0x1.62dafp+13L, 0x1.96c53d30277021dp-16383L  ], // near underflow - subnormal
                [-0x1.643p+13L,   0x1p-16444L                  ], // ditto
                [-0x1.645p+13L,   0                            ], // close underflow
                [-0x1p+30L,       0                            ], // far underflow
            ];
        }
        else static if (realFormat == RealFormat.ieeeDouble)
        {
            static immutable T[2][] exptestpoints =
            [ //  x,             exp(x)
                [ 1.0L,          E                        ],
                [ 0.5L,          0x1.a61298e1e069cp+0L    ],
                [ 3.0L,          E*E*E                    ],
                [ 0x1.6p+9L,     0x1.93bf4ec282efbp+1015L ], // near overflow
                [ 0x1.7p+9L,     T.infinity               ], // close overflow
                [ 0x1p+80L,      T.infinity               ], // far overflow
                [ T.infinity,    T.infinity               ],
                [-0x1.6p+9L,     0x1.44a3824e5285fp-1016L ], // near underflow
                [-0x1.64p+9L,    0x0.06f84920bb2d4p-1022L ], // near underflow - subnormal
                [-0x1.743p+9L,   0x0.0000000000001p-1022L ], // ditto
                [-0x1.8p+9L,     0                        ], // close underflow
                [-0x1p+30L,      0                        ], // far underflow
            ];
        }
        else static if (realFormat == RealFormat.ieeeSingle)
        {
            static immutable T[2][] exptestpoints =
            [ //  x,             exp(x)
                [ 1.0L,          E                ],
                [ 0.5L,          0x1.a61299p+0L   ],
                [ 3.0L,          E*E*E            ],
                [ 0x1.62p+6L,    0x1.99b988p+127L ], // near overflow
                [ 0x1.7p+6L,     T.infinity       ], // close overflow
                [ 0x1p+80L,      T.infinity       ], // far overflow
                [ T.infinity,    T.infinity       ],
                [-0x1.5cp+6L,    0x1.666d0ep-126L ], // near underflow
                [-0x1.7p+6L,     0x0.026a42p-126L ], // near underflow - subnormal
                [-0x1.9cp+6L,    0x0.000002p-126L ], // ditto
                [-0x1.ap+6L,     0                ], // close underflow
                [-0x1p+30L,      0                ], // far underflow
            ];
        }
        else
            static assert(0, "No exp() tests for real type!");

        const minEqualMantissaBits = T.mant_dig - 13;
        T x;
        version (IeeeFlagsSupport) IeeeFlags f;
        foreach (ref pair; exptestpoints)
        {
            version (IeeeFlagsSupport) resetIeeeFlags();
            x = exp(pair[0]);
            //printf("exp(%La) = %La, should be %La\n", cast(real) pair[0], cast(real) x, cast(real) pair[1]);
            assert(feqrel(x, pair[1]) >= minEqualMantissaBits);
        }

        // Ideally, exp(0) would not set the inexact flag.
        // Unfortunately, fldl2e sets it!
        // So it's not realistic to avoid setting it.
        assert(exp(cast(T) 0.0) == 1.0);

        // NaN propagation. Doesn't set flags, bcos was already NaN.
        version (IeeeFlagsSupport)
        {
            resetIeeeFlags();
            x = exp(T.nan);
            f = ieeeFlags;
            assert(isIdentical(abs(x), T.nan));
            assert(f.flags == 0);

            resetIeeeFlags();
            x = exp(-T.nan);
            f = ieeeFlags;
            assert(isIdentical(abs(x), T.nan));
            assert(f.flags == 0);
        }
        else
        {
            x = exp(T.nan);
            assert(isIdentical(abs(x), T.nan));

            x = exp(-T.nan);
            assert(isIdentical(abs(x), T.nan));
        }

        x = exp(NaN(0x123));
        assert(isIdentical(x, NaN(0x123)));
    }

    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(real, double, float))
        testExp!T();

    // High resolution test (verified against GNU MPFR/Mathematica).
    assert(exp(0.5L) == 0x1.A612_98E1_E069_BC97_2DFE_FAB6_DF34p+0L);

    assert(isClose(exp(3.0L), E * E * E, real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}

/**
 * Calculates the value of the natural logarithm base (e)
 * raised to the power of x, minus 1.
 *
 * For very small x, expm1(x) is more accurate
 * than exp(x)-1.
 *
 *  $(TABLE_SV
 *    $(TR $(TH x)             $(TH e$(SUPERSCRIPT x)-1)  )
 *    $(TR $(TD $(PLUSMN)0.0)  $(TD $(PLUSMN)0.0) )
 *    $(TR $(TD +$(INFIN))     $(TD +$(INFIN))    )
 *    $(TR $(TD -$(INFIN))     $(TD -1.0)         )
 *    $(TR $(TD $(NAN))        $(TD $(NAN))       )
 *  )
 */
pragma(inline, true)
real expm1(real x) @trusted pure nothrow @nogc // TODO: @safe
{
    version (InlineAsm_X87)
    {
        if (!__ctfe)
            return expm1Asm(x);
    }
    return expm1Impl(x);
}

/// ditto
pragma(inline, true)
double expm1(double x) @safe pure nothrow @nogc
{
    return __ctfe ? cast(double) expm1(cast(real) x) : expm1Impl(x);
}

/// ditto
pragma(inline, true)
float expm1(float x) @safe pure nothrow @nogc
{
    // no single-precision version in Cephes => use double precision
    return __ctfe ? cast(float) expm1(cast(real) x) : cast(float) expm1Impl(cast(double) x);
}

///
@safe unittest
{
    import std.math.traits : isIdentical;
    import std.math.operations : feqrel;

    assert(isIdentical(expm1(0.0), 0.0));
    assert(expm1(1.0).feqrel(1.71828) > 16);
    assert(expm1(2.0).feqrel(6.3890) > 16);
}

version (InlineAsm_X87)
private real expm1Asm(real x) @trusted pure nothrow @nogc
{
    version (X86)
    {
        enum PARAMSIZE = (real.sizeof+3)&(0xFFFF_FFFC); // always a multiple of 4
        asm pure nothrow @nogc
        {
            /*  expm1() for x87 80-bit reals, IEEE754-2008 conformant.
             * Author: Don Clugston.
             *
             *    expm1(x) = 2^^(rndint(y))* 2^^(y-rndint(y)) - 1 where y = LN2*x.
             *    = 2rndy * 2ym1 + 2rndy - 1, where 2rndy = 2^^(rndint(y))
             *     and 2ym1 = (2^^(y-rndint(y))-1).
             *    If 2rndy  < 0.5*real.epsilon, result is -1.
             *    Implementation is otherwise the same as for exp2()
             */
            naked;
            fld real ptr [ESP+4] ; // x
            mov AX, [ESP+4+8]; // AX = exponent and sign
            sub ESP, 12+8; // Create scratch space on the stack
            // [ESP,ESP+2] = scratchint
            // [ESP+4..+6, +8..+10, +10] = scratchreal
            // set scratchreal mantissa = 1.0
            mov dword ptr [ESP+8], 0;
            mov dword ptr [ESP+8+4], 0x80000000;
            and AX, 0x7FFF; // drop sign bit
            cmp AX, 0x401D; // avoid InvalidException in fist
            jae L_extreme;
            fldl2e;
            fmulp ST(1), ST; // y = x*log2(e)
            fist dword ptr [ESP]; // scratchint = rndint(y)
            fisub dword ptr [ESP]; // y - rndint(y)
            // and now set scratchreal exponent
            mov EAX, [ESP];
            add EAX, 0x3fff;
            jle short L_largenegative;
            cmp EAX,0x8000;
            jge short L_largepositive;
            mov [ESP+8+8],AX;
            f2xm1; // 2ym1 = 2^^(y-rndint(y)) -1
            fld real ptr [ESP+8] ; // 2rndy = 2^^rndint(y)
            fmul ST(1), ST;  // ST=2rndy, ST(1)=2rndy*2ym1
            fld1;
            fsubp ST(1), ST; // ST = 2rndy-1, ST(1) = 2rndy * 2ym1 - 1
            faddp ST(1), ST; // ST = 2rndy * 2ym1 + 2rndy - 1
            add ESP,12+8;
            ret PARAMSIZE;

L_extreme:  // Extreme exponent. X is very large positive, very
            // large negative, infinity, or NaN.
            fxam;
            fstsw AX;
            test AX, 0x0400; // NaN_or_zero, but we already know x != 0
            jz L_was_nan;  // if x is NaN, returns x
            test AX, 0x0200;
            jnz L_largenegative;
L_largepositive:
            // Set scratchreal = real.max.
            // squaring it will create infinity, and set overflow flag.
            mov word  ptr [ESP+8+8], 0x7FFE;
            fstp ST(0);
            fld real ptr [ESP+8];  // load scratchreal
            fmul ST(0), ST;        // square it, to create havoc!
L_was_nan:
            add ESP,12+8;
            ret PARAMSIZE;
L_largenegative:
            fstp ST(0);
            fld1;
            fchs; // return -1. Underflow flag is not set.
            add ESP,12+8;
            ret PARAMSIZE;
        }
    }
    else version (X86_64)
    {
        asm pure nothrow @nogc
        {
            naked;
        }
        version (Win64)
        {
            asm pure nothrow @nogc
            {
                fld   real ptr [RCX];  // x
                mov   AX,[RCX+8];      // AX = exponent and sign
            }
        }
        else
        {
            asm pure nothrow @nogc
            {
                fld   real ptr [RSP+8];  // x
                mov   AX,[RSP+8+8];      // AX = exponent and sign
            }
        }
        asm pure nothrow @nogc
        {
            /*  expm1() for x87 80-bit reals, IEEE754-2008 conformant.
             * Author: Don Clugston.
             *
             *    expm1(x) = 2^(rndint(y))* 2^(y-rndint(y)) - 1 where y = LN2*x.
             *    = 2rndy * 2ym1 + 2rndy - 1, where 2rndy = 2^(rndint(y))
             *     and 2ym1 = (2^(y-rndint(y))-1).
             *    If 2rndy  < 0.5*real.epsilon, result is -1.
             *    Implementation is otherwise the same as for exp2()
             */
            sub RSP, 24;       // Create scratch space on the stack
            // [RSP,RSP+2] = scratchint
            // [RSP+4..+6, +8..+10, +10] = scratchreal
            // set scratchreal mantissa = 1.0
            mov dword ptr [RSP+8], 0;
            mov dword ptr [RSP+8+4], 0x80000000;
            and AX, 0x7FFF; // drop sign bit
            cmp AX, 0x401D; // avoid InvalidException in fist
            jae L_extreme;
            fldl2e;
            fmul ; // y = x*log2(e)
            fist dword ptr [RSP]; // scratchint = rndint(y)
            fisub dword ptr [RSP]; // y - rndint(y)
            // and now set scratchreal exponent
            mov EAX, [RSP];
            add EAX, 0x3fff;
            jle short L_largenegative;
            cmp EAX,0x8000;
            jge short L_largepositive;
            mov [RSP+8+8],AX;
            f2xm1; // 2^(y-rndint(y)) -1
            fld real ptr [RSP+8] ; // 2^rndint(y)
            fmul ST(1), ST;
            fld1;
            fsubp ST(1), ST;
            fadd;
            add RSP,24;
            ret;

L_extreme:  // Extreme exponent. X is very large positive, very
            // large negative, infinity, or NaN.
            fxam;
            fstsw AX;
            test AX, 0x0400; // NaN_or_zero, but we already know x != 0
            jz L_was_nan;  // if x is NaN, returns x
            test AX, 0x0200;
            jnz L_largenegative;
L_largepositive:
            // Set scratchreal = real.max.
            // squaring it will create infinity, and set overflow flag.
            mov word  ptr [RSP+8+8], 0x7FFE;
            fstp ST(0);
            fld real ptr [RSP+8];  // load scratchreal
            fmul ST(0), ST;        // square it, to create havoc!
L_was_nan:
            add RSP,24;
            ret;

L_largenegative:
            fstp ST(0);
            fld1;
            fchs; // return -1. Underflow flag is not set.
            add RSP,24;
            ret;
        }
    }
    else
        static assert(0);
}

private T expm1Impl(T)(T x) @safe pure nothrow @nogc
{
    import std.math.traits : floatTraits, RealFormat;
    import std.math.rounding : floor;
    import std.math.algebraic : poly;
    import std.math.constants : LN2;

    // Coefficients for exp(x) - 1 and overflow/underflow limits.
    enum realFormat = floatTraits!T.realFormat;
    static if (realFormat == RealFormat.ieeeQuadruple)
    {
        static immutable T[8] P = [
            2.943520915569954073888921213330863757240E8L,
            -5.722847283900608941516165725053359168840E7L,
            8.944630806357575461578107295909719817253E6L,
            -7.212432713558031519943281748462837065308E5L,
            4.578962475841642634225390068461943438441E4L,
            -1.716772506388927649032068540558788106762E3L,
            4.401308817383362136048032038528753151144E1L,
            -4.888737542888633647784737721812546636240E-1L
        ];

        static immutable T[9] Q = [
            1.766112549341972444333352727998584753865E9L,
            -7.848989743695296475743081255027098295771E8L,
            1.615869009634292424463780387327037251069E8L,
            -2.019684072836541751428967854947019415698E7L,
            1.682912729190313538934190635536631941751E6L,
            -9.615511549171441430850103489315371768998E4L,
            3.697714952261803935521187272204485251835E3L,
            -8.802340681794263968892934703309274564037E1L,
            1.0
        ];

        enum T OF = 1.1356523406294143949491931077970764891253E4L;
        enum T UF = -1.143276959615573793352782661133116431383730e4L;
    }
    else static if (realFormat == RealFormat.ieeeExtended)
    {
        static immutable T[5] P = [
           -1.586135578666346600772998894928250240826E4L,
            2.642771505685952966904660652518429479531E3L,
           -3.423199068835684263987132888286791620673E2L,
            1.800826371455042224581246202420972737840E1L,
           -5.238523121205561042771939008061958820811E-1L,
        ];
        static immutable T[6] Q = [
           -9.516813471998079611319047060563358064497E4L,
            3.964866271411091674556850458227710004570E4L,
           -7.207678383830091850230366618190187434796E3L,
            7.206038318724600171970199625081491823079E2L,
           -4.002027679107076077238836622982900945173E1L,
            1.0
        ];

        enum T OF =  1.1356523406294143949492E4L;
        enum T UF = -4.5054566736396445112120088E1L;
    }
    else static if (realFormat == RealFormat.ieeeDouble)
    {
        static immutable T[3] P = [
            9.9999999999999999991025E-1,
            3.0299440770744196129956E-2,
            1.2617719307481059087798E-4,
        ];
        static immutable T[4] Q = [
            2.0000000000000000000897E0,
            2.2726554820815502876593E-1,
            2.5244834034968410419224E-3,
            3.0019850513866445504159E-6,
        ];
    }
    else
        static assert(0, "no coefficients for expm1()");

    static if (realFormat == RealFormat.ieeeDouble) // special case for double precision
    {
        if (x < -0.5 || x > 0.5)
            return exp(x) - 1.0;
        if (x == 0.0)
            return x;

        const T xx = x * x;
        x = x * poly(xx, P);
        x = x / (poly(xx, Q) - x);
        return x + x;
    }
    else
    {
        // C1 + C2 = LN2.
        enum T C1 = 6.9314575195312500000000E-1L;
        enum T C2 = 1.428606820309417232121458176568075500134E-6L;

        // Special cases.
        if (x > OF)
            return real.infinity;
        if (x == cast(T) 0.0)
            return x;
        if (x < UF)
            return -1.0;

        // Express x = LN2 (n + remainder), remainder not exceeding 1/2.
        int n = cast(int) floor((cast(T) 0.5) + x / cast(T) LN2);
        x -= n * C1;
        x -= n * C2;

        // Rational approximation:
        //  exp(x) - 1 = x + 0.5 x^^2 + x^^3 P(x) / Q(x)
        T px = x * poly(x, P);
        T qx = poly(x, Q);
        const T xx = x * x;
        qx = x + ((cast(T) 0.5) * xx + xx * px / qx);

        // We have qx = exp(remainder LN2) - 1, so:
        //  exp(x) - 1 = 2^^n (qx + 1) - 1 = 2^^n qx + 2^^n - 1.
        px = core.math.ldexp(cast(T) 1.0, n);
        x = px * qx + (px - cast(T) 1.0);

        return x;
    }
}

@safe @nogc nothrow unittest
{
    import std.math.traits : isNaN;
    import std.math.operations : isClose, CommonDefaultFor;

    static void testExpm1(T)()
    {
        // NaN
        assert(isNaN(expm1(cast(T) T.nan)));

        static immutable T[] xs = [ -2, -0.75, -0.3, 0.0, 0.1, 0.2, 0.5, 1.0 ];
        foreach (x; xs)
        {
            const T e = expm1(x);
            const T r = exp(x) - 1;

            //printf("expm1(%Lg) = %Lg, should approximately be %Lg\n", cast(real) x, cast(real) e, cast(real) r);
            assert(isClose(r, e, CommonDefaultFor!(T,T), CommonDefaultFor!(T,T)));
        }
    }

    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(real, double))
        testExpm1!T();
}

/**
 * Calculates 2$(SUPERSCRIPT x).
 *
 *  $(TABLE_SV
 *    $(TR $(TH x)             $(TH exp2(x))   )
 *    $(TR $(TD +$(INFIN))     $(TD +$(INFIN)) )
 *    $(TR $(TD -$(INFIN))     $(TD +0.0)      )
 *    $(TR $(TD $(NAN))        $(TD $(NAN))    )
 *  )
 */
pragma(inline, true)
real exp2(real x) @nogc @trusted pure nothrow // TODO: @safe
{
    version (InlineAsm_X87)
    {
        if (!__ctfe)
            return exp2Asm(x);
    }
    return exp2Impl(x);
}

/// ditto
pragma(inline, true)
double exp2(double x) @nogc @safe pure nothrow { return __ctfe ? cast(double) exp2(cast(real) x) : exp2Impl(x); }

/// ditto
pragma(inline, true)
float exp2(float x) @nogc @safe pure nothrow { return __ctfe ? cast(float) exp2(cast(real) x) : exp2Impl(x); }

///
@safe unittest
{
    import std.math.traits : isIdentical;
    import std.math.operations : feqrel;

    assert(isIdentical(exp2(0.0), 1.0));
    assert(exp2(2.0).feqrel(4.0) > 16);
    assert(exp2(8.0).feqrel(256.0) > 16);
}

@safe unittest
{
    version (CRuntime_Microsoft) {} else // aexp2/exp2f/exp2l not implemented
    {
        assert( core.stdc.math.exp2f(0.0f) == 1 );
        assert( core.stdc.math.exp2 (0.0)  == 1 );
        assert( core.stdc.math.exp2l(0.0L) == 1 );
    }
}

version (InlineAsm_X87)
private real exp2Asm(real x) @nogc @trusted pure nothrow
{
    version (X86)
    {
        enum PARAMSIZE = (real.sizeof+3)&(0xFFFF_FFFC); // always a multiple of 4

        asm pure nothrow @nogc
        {
            /*  exp2() for x87 80-bit reals, IEEE754-2008 conformant.
             * Author: Don Clugston.
             *
             * exp2(x) = 2^^(rndint(x))* 2^^(y-rndint(x))
             * The trick for high performance is to avoid the fscale(28cycles on core2),
             * frndint(19 cycles), leaving f2xm1(19 cycles) as the only slow instruction.
             *
             * We can do frndint by using fist. BUT we can't use it for huge numbers,
             * because it will set the Invalid Operation flag if overflow or NaN occurs.
             * Fortunately, whenever this happens the result would be zero or infinity.
             *
             * We can perform fscale by directly poking into the exponent. BUT this doesn't
             * work for the (very rare) cases where the result is subnormal. So we fall back
             * to the slow method in that case.
             */
            naked;
            fld real ptr [ESP+4] ; // x
            mov AX, [ESP+4+8]; // AX = exponent and sign
            sub ESP, 12+8; // Create scratch space on the stack
            // [ESP,ESP+2] = scratchint
            // [ESP+4..+6, +8..+10, +10] = scratchreal
            // set scratchreal mantissa = 1.0
            mov dword ptr [ESP+8], 0;
            mov dword ptr [ESP+8+4], 0x80000000;
            and AX, 0x7FFF; // drop sign bit
            cmp AX, 0x401D; // avoid InvalidException in fist
            jae L_extreme;
            fist dword ptr [ESP]; // scratchint = rndint(x)
            fisub dword ptr [ESP]; // x - rndint(x)
            // and now set scratchreal exponent
            mov EAX, [ESP];
            add EAX, 0x3fff;
            jle short L_subnormal;
            cmp EAX,0x8000;
            jge short L_overflow;
            mov [ESP+8+8],AX;
L_normal:
            f2xm1;
            fld1;
            faddp ST(1), ST; // 2^^(x-rndint(x))
            fld real ptr [ESP+8] ; // 2^^rndint(x)
            add ESP,12+8;
            fmulp ST(1), ST;
            ret PARAMSIZE;

L_subnormal:
            // Result will be subnormal.
            // In this rare case, the simple poking method doesn't work.
            // The speed doesn't matter, so use the slow fscale method.
            fild dword ptr [ESP];  // scratchint
            fld1;
            fscale;
            fstp real ptr [ESP+8]; // scratchreal = 2^^scratchint
            fstp ST(0);         // drop scratchint
            jmp L_normal;

L_extreme:  // Extreme exponent. X is very large positive, very
            // large negative, infinity, or NaN.
            fxam;
            fstsw AX;
            test AX, 0x0400; // NaN_or_zero, but we already know x != 0
            jz L_was_nan;  // if x is NaN, returns x
            // set scratchreal = real.min_normal
            // squaring it will return 0, setting underflow flag
            mov word  ptr [ESP+8+8], 1;
            test AX, 0x0200;
            jnz L_waslargenegative;
L_overflow:
            // Set scratchreal = real.max.
            // squaring it will create infinity, and set overflow flag.
            mov word  ptr [ESP+8+8], 0x7FFE;
L_waslargenegative:
            fstp ST(0);
            fld real ptr [ESP+8];  // load scratchreal
            fmul ST(0), ST;        // square it, to create havoc!
L_was_nan:
            add ESP,12+8;
            ret PARAMSIZE;
        }
    }
    else version (X86_64)
    {
        asm pure nothrow @nogc
        {
            naked;
        }
        version (Win64)
        {
            asm pure nothrow @nogc
            {
                fld   real ptr [RCX];  // x
                mov   AX,[RCX+8];      // AX = exponent and sign
            }
        }
        else
        {
            asm pure nothrow @nogc
            {
                fld   real ptr [RSP+8];  // x
                mov   AX,[RSP+8+8];      // AX = exponent and sign
            }
        }
        asm pure nothrow @nogc
        {
            /*  exp2() for x87 80-bit reals, IEEE754-2008 conformant.
             * Author: Don Clugston.
             *
             * exp2(x) = 2^(rndint(x))* 2^(y-rndint(x))
             * The trick for high performance is to avoid the fscale(28cycles on core2),
             * frndint(19 cycles), leaving f2xm1(19 cycles) as the only slow instruction.
             *
             * We can do frndint by using fist. BUT we can't use it for huge numbers,
             * because it will set the Invalid Operation flag is overflow or NaN occurs.
             * Fortunately, whenever this happens the result would be zero or infinity.
             *
             * We can perform fscale by directly poking into the exponent. BUT this doesn't
             * work for the (very rare) cases where the result is subnormal. So we fall back
             * to the slow method in that case.
             */
            sub RSP, 24; // Create scratch space on the stack
            // [RSP,RSP+2] = scratchint
            // [RSP+4..+6, +8..+10, +10] = scratchreal
            // set scratchreal mantissa = 1.0
            mov dword ptr [RSP+8], 0;
            mov dword ptr [RSP+8+4], 0x80000000;
            and AX, 0x7FFF; // drop sign bit
            cmp AX, 0x401D; // avoid InvalidException in fist
            jae L_extreme;
            fist dword ptr [RSP]; // scratchint = rndint(x)
            fisub dword ptr [RSP]; // x - rndint(x)
            // and now set scratchreal exponent
            mov EAX, [RSP];
            add EAX, 0x3fff;
            jle short L_subnormal;
            cmp EAX,0x8000;
            jge short L_overflow;
            mov [RSP+8+8],AX;
L_normal:
            f2xm1;
            fld1;
            fadd; // 2^(x-rndint(x))
            fld real ptr [RSP+8] ; // 2^rndint(x)
            add RSP,24;
            fmulp ST(1), ST;
            ret;

L_subnormal:
            // Result will be subnormal.
            // In this rare case, the simple poking method doesn't work.
            // The speed doesn't matter, so use the slow fscale method.
            fild dword ptr [RSP];  // scratchint
            fld1;
            fscale;
            fstp real ptr [RSP+8]; // scratchreal = 2^scratchint
            fstp ST(0);         // drop scratchint
            jmp L_normal;

L_extreme:  // Extreme exponent. X is very large positive, very
            // large negative, infinity, or NaN.
            fxam;
            fstsw AX;
            test AX, 0x0400; // NaN_or_zero, but we already know x != 0
            jz L_was_nan;  // if x is NaN, returns x
            // set scratchreal = real.min
            // squaring it will return 0, setting underflow flag
            mov word  ptr [RSP+8+8], 1;
            test AX, 0x0200;
            jnz L_waslargenegative;
L_overflow:
            // Set scratchreal = real.max.
            // squaring it will create infinity, and set overflow flag.
            mov word  ptr [RSP+8+8], 0x7FFE;
L_waslargenegative:
            fstp ST(0);
            fld real ptr [RSP+8];  // load scratchreal
            fmul ST(0), ST;        // square it, to create havoc!
L_was_nan:
            add RSP,24;
            ret;
        }
    }
    else
        static assert(0);
}

private T exp2Impl(T)(T x) @nogc @safe pure nothrow
{
    import std.math.traits : floatTraits, RealFormat, isNaN;
    import std.math.rounding : floor;
    import std.math.algebraic : poly;

    // Coefficients for exp2(x)
    enum realFormat = floatTraits!T.realFormat;
    static if (realFormat == RealFormat.ieeeQuadruple)
    {
        static immutable T[5] P = [
            9.079594442980146270952372234833529694788E12L,
            1.530625323728429161131811299626419117557E11L,
            5.677513871931844661829755443994214173883E8L,
            6.185032670011643762127954396427045467506E5L,
            1.587171580015525194694938306936721666031E2L
        ];

        static immutable T[6] Q = [
            2.619817175234089411411070339065679229869E13L,
            1.490560994263653042761789432690793026977E12L,
            1.092141473886177435056423606755843616331E10L,
            2.186249607051644894762167991800811827835E7L,
            1.236602014442099053716561665053645270207E4L,
            1.0
        ];
    }
    else static if (realFormat == RealFormat.ieeeExtended)
    {
        static immutable T[3] P = [
            2.0803843631901852422887E6L,
            3.0286971917562792508623E4L,
            6.0614853552242266094567E1L,
        ];
        static immutable T[4] Q = [
            6.0027204078348487957118E6L,
            3.2772515434906797273099E5L,
            1.7492876999891839021063E3L,
            1.0000000000000000000000E0L,
        ];
    }
    else static if (realFormat == RealFormat.ieeeDouble)
    {
        static immutable T[3] P = [
            1.51390680115615096133E3L,
            2.02020656693165307700E1L,
            2.30933477057345225087E-2L,
        ];
        static immutable T[3] Q = [
            4.36821166879210612817E3L,
            2.33184211722314911771E2L,
            1.00000000000000000000E0L,
        ];
    }
    else static if (realFormat == RealFormat.ieeeSingle)
    {
        static immutable T[6] P = [
            6.931472028550421E-001L,
            2.402264791363012E-001L,
            5.550332471162809E-002L,
            9.618437357674640E-003L,
            1.339887440266574E-003L,
            1.535336188319500E-004L,
        ];
    }
    else
        static assert(0, "no coefficients for exp2()");

    // Overflow and Underflow limits.
    enum T OF = T.max_exp;
    enum T UF = T.min_exp - 1;

    // Special cases.
    if (isNaN(x))
        return x;
    if (x > OF)
        return real.infinity;
    if (x < UF)
        return 0.0;

    static if (realFormat == RealFormat.ieeeSingle) // special case for single precision
    {
        // The following is necessary because range reduction blows up.
        if (x == 0.0f)
            return 1.0f;

        // Separate into integer and fractional parts.
        const T i = floor(x);
        int n = cast(int) i;
        x -= i;
        if (x > 0.5f)
        {
            n += 1;
            x -= 1.0f;
        }

        // Rational approximation:
        //  exp2(x) = 1.0 + x P(x)
        x = 1.0f + x * poly(x, P);
    }
    else
    {
        // Separate into integer and fractional parts.
        const T i = floor(x + cast(T) 0.5);
        int n = cast(int) i;
        x -= i;

        // Rational approximation:
        //  exp2(x) = 1.0 + 2x P(x^^2) / (Q(x^^2) - P(x^^2))
        const T xx = x * x;
        const T px = x * poly(xx, P);
        x = px / (poly(xx, Q) - px);
        x = (cast(T) 1.0) + (cast(T) 2.0) * x;
    }

    // Scale by power of 2.
    x = core.math.ldexp(x, n);

    return x;
}

@safe @nogc nothrow unittest
{
    import std.math.operations : feqrel, NaN, isClose;
    import std.math.traits : isIdentical;
    import std.math.constants : SQRT2;

    assert(feqrel(exp2(0.5L), SQRT2) >= real.mant_dig -1);
    assert(exp2(8.0L) == 256.0);
    assert(exp2(-9.0L)== 1.0L/512.0);

    static void testExp2(T)()
    {
        // NaN
        const T specialNaN = NaN(0x0123L);
        assert(isIdentical(exp2(specialNaN), specialNaN));

        // over-/underflow
        enum T OF = T.max_exp;
        enum T UF = T.min_exp - T.mant_dig;
        assert(isIdentical(exp2(OF + 1), cast(T) T.infinity));
        assert(isIdentical(exp2(UF - 1), cast(T) 0.0));

        static immutable T[2][] vals =
        [
            // x, exp2(x)
            [  0.0, 1.0 ],
            [ -0.0, 1.0 ],
            [  0.5, SQRT2 ],
            [  8.0, 256.0 ],
            [ -9.0, 1.0 / 512 ],
        ];

        foreach (ref val; vals)
        {
            const T x = val[0];
            const T r = val[1];
            const T e = exp2(x);

            //printf("exp2(%Lg) = %Lg, should be %Lg\n", cast(real) x, cast(real) e, cast(real) r);
            assert(isClose(r, e));
        }
    }

    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(real, double, float))
        testExp2!T();
}

/*********************************************************************
 * Separate floating point value into significand and exponent.
 *
 * Returns:
 *      Calculate and return $(I x) and $(I exp) such that
 *      value =$(I x)*2$(SUPERSCRIPT exp) and
 *      .5 $(LT)= |$(I x)| $(LT) 1.0
 *
 *      $(I x) has same sign as value.
 *
 *      $(TABLE_SV
 *      $(TR $(TH value)           $(TH returns)         $(TH exp))
 *      $(TR $(TD $(PLUSMN)0.0)    $(TD $(PLUSMN)0.0)    $(TD 0))
 *      $(TR $(TD +$(INFIN))       $(TD +$(INFIN))       $(TD int.max))
 *      $(TR $(TD -$(INFIN))       $(TD -$(INFIN))       $(TD int.min))
 *      $(TR $(TD $(PLUSMN)$(NAN)) $(TD $(PLUSMN)$(NAN)) $(TD int.min))
 *      )
 */
T frexp(T)(const T value, out int exp) @trusted pure nothrow @nogc
if (isFloatingPoint!T)
{
    import std.math.traits : floatTraits, RealFormat, MANTISSA_MSB, MANTISSA_LSB, isSubnormal;

    if (__ctfe)
    {
        // Handle special cases.
        if (value == 0) { exp = 0; return value; }
        if (value == T.infinity) { exp = int.max; return value; }
        if (value == -T.infinity || value != value) { exp = int.min; return value; }
        // Handle ordinary cases.
        // In CTFE there is no performance advantage for having separate
        // paths for different floating point types.
        T absValue = value < 0 ? -value : value;
        int expCount;
        static if (T.mant_dig > double.mant_dig)
        {
            for (; absValue >= 0x1.0p+1024L; absValue *= 0x1.0p-1024L)
                expCount += 1024;
            for (; absValue < 0x1.0p-1021L; absValue *= 0x1.0p+1021L)
                expCount -= 1021;
        }
        const double dval = cast(double) absValue;
        int dexp = cast(int) (((*cast(const long*) &dval) >>> 52) & 0x7FF) + double.min_exp - 2;
        dexp++;
        expCount += dexp;
        absValue *= 2.0 ^^ -dexp;
        // If the original value was subnormal or if it was a real
        // then absValue can still be outside the [0.5, 1.0) range.
        if (absValue < 0.5)
        {
            assert(T.mant_dig > double.mant_dig || isSubnormal(value));
            do
            {
                absValue += absValue;
                expCount--;
            } while (absValue < 0.5);
        }
        else
        {
            assert(absValue < 1 || T.mant_dig > double.mant_dig);
            for (; absValue >= 1; absValue *= T(0.5))
                expCount++;
        }
        exp = expCount;
        return value < 0 ? -absValue : absValue;
    }

    Unqual!T vf = value;
    ushort* vu = cast(ushort*)&vf;
    static if (is(immutable T == immutable float))
        int* vi = cast(int*)&vf;
    else
        long* vl = cast(long*)&vf;
    int ex;
    alias F = floatTraits!T;

    ex = vu[F.EXPPOS_SHORT] & F.EXPMASK;
    static if (F.realFormat == RealFormat.ieeeExtended ||
               F.realFormat == RealFormat.ieeeExtended53)
    {
        if (ex)
        {   // If exponent is non-zero
            if (ex == F.EXPMASK) // infinity or NaN
            {
                if (*vl &  0x7FFF_FFFF_FFFF_FFFF)  // NaN
                {
                    *vl |= 0xC000_0000_0000_0000;  // convert NaNS to NaNQ
                    exp = int.min;
                }
                else if (vu[F.EXPPOS_SHORT] & 0x8000)   // negative infinity
                    exp = int.min;
                else   // positive infinity
                    exp = int.max;

            }
            else
            {
                exp = ex - F.EXPBIAS;
                vu[F.EXPPOS_SHORT] = (0x8000 & vu[F.EXPPOS_SHORT]) | 0x3FFE;
            }
        }
        else if (!*vl)
        {
            // vf is +-0.0
            exp = 0;
        }
        else
        {
            // subnormal

            vf *= F.RECIP_EPSILON;
            ex = vu[F.EXPPOS_SHORT] & F.EXPMASK;
            exp = ex - F.EXPBIAS - T.mant_dig + 1;
            vu[F.EXPPOS_SHORT] = ((-1 - F.EXPMASK) & vu[F.EXPPOS_SHORT]) | 0x3FFE;
        }
        return vf;
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple)
    {
        if (ex)     // If exponent is non-zero
        {
            if (ex == F.EXPMASK)
            {
                // infinity or NaN
                if (vl[MANTISSA_LSB] |
                    (vl[MANTISSA_MSB] & 0x0000_FFFF_FFFF_FFFF))  // NaN
                {
                    // convert NaNS to NaNQ
                    vl[MANTISSA_MSB] |= 0x0000_8000_0000_0000;
                    exp = int.min;
                }
                else if (vu[F.EXPPOS_SHORT] & 0x8000)   // negative infinity
                    exp = int.min;
                else   // positive infinity
                    exp = int.max;
            }
            else
            {
                exp = ex - F.EXPBIAS;
                vu[F.EXPPOS_SHORT] = F.EXPBIAS | (0x8000 & vu[F.EXPPOS_SHORT]);
            }
        }
        else if ((vl[MANTISSA_LSB] |
            (vl[MANTISSA_MSB] & 0x0000_FFFF_FFFF_FFFF)) == 0)
        {
            // vf is +-0.0
            exp = 0;
        }
        else
        {
            // subnormal
            vf *= F.RECIP_EPSILON;
            ex = vu[F.EXPPOS_SHORT] & F.EXPMASK;
            exp = ex - F.EXPBIAS - T.mant_dig + 1;
            vu[F.EXPPOS_SHORT] = F.EXPBIAS | (0x8000 & vu[F.EXPPOS_SHORT]);
        }
        return vf;
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        if (ex) // If exponent is non-zero
        {
            if (ex == F.EXPMASK)   // infinity or NaN
            {
                if (*vl == 0x7FF0_0000_0000_0000)  // positive infinity
                {
                    exp = int.max;
                }
                else if (*vl == 0xFFF0_0000_0000_0000) // negative infinity
                    exp = int.min;
                else
                { // NaN
                    *vl |= 0x0008_0000_0000_0000;  // convert NaNS to NaNQ
                    exp = int.min;
                }
            }
            else
            {
                exp = (ex - F.EXPBIAS) >> 4;
                vu[F.EXPPOS_SHORT] = cast(ushort)((0x800F & vu[F.EXPPOS_SHORT]) | 0x3FE0);
            }
        }
        else if (!(*vl & 0x7FFF_FFFF_FFFF_FFFF))
        {
            // vf is +-0.0
            exp = 0;
        }
        else
        {
            // subnormal
            vf *= F.RECIP_EPSILON;
            ex = vu[F.EXPPOS_SHORT] & F.EXPMASK;
            exp = ((ex - F.EXPBIAS) >> 4) - T.mant_dig + 1;
            vu[F.EXPPOS_SHORT] =
                cast(ushort)(((-1 - F.EXPMASK) & vu[F.EXPPOS_SHORT]) | 0x3FE0);
        }
        return vf;
    }
    else static if (F.realFormat == RealFormat.ieeeSingle)
    {
        if (ex) // If exponent is non-zero
        {
            if (ex == F.EXPMASK)   // infinity or NaN
            {
                if (*vi == 0x7F80_0000)  // positive infinity
                {
                    exp = int.max;
                }
                else if (*vi == 0xFF80_0000) // negative infinity
                    exp = int.min;
                else
                { // NaN
                    *vi |= 0x0040_0000;  // convert NaNS to NaNQ
                    exp = int.min;
                }
            }
            else
            {
                exp = (ex - F.EXPBIAS) >> 7;
                vu[F.EXPPOS_SHORT] = cast(ushort)((0x807F & vu[F.EXPPOS_SHORT]) | 0x3F00);
            }
        }
        else if (!(*vi & 0x7FFF_FFFF))
        {
            // vf is +-0.0
            exp = 0;
        }
        else
        {
            // subnormal
            vf *= F.RECIP_EPSILON;
            ex = vu[F.EXPPOS_SHORT] & F.EXPMASK;
            exp = ((ex - F.EXPBIAS) >> 7) - T.mant_dig + 1;
            vu[F.EXPPOS_SHORT] =
                cast(ushort)(((-1 - F.EXPMASK) & vu[F.EXPPOS_SHORT]) | 0x3F00);
        }
        return vf;
    }
    else // static if (F.realFormat == RealFormat.ibmExtended)
    {
        assert(0, "frexp not implemented");
    }
}

///
@safe unittest
{
    import std.math.operations : isClose;

    int exp;
    real mantissa = frexp(123.456L, exp);

    assert(isClose(mantissa * pow(2.0L, cast(real) exp), 123.456L));

    assert(frexp(-real.nan, exp) && exp == int.min);
    assert(frexp(real.nan, exp) && exp == int.min);
    assert(frexp(-real.infinity, exp) == -real.infinity && exp == int.min);
    assert(frexp(real.infinity, exp) == real.infinity && exp == int.max);
    assert(frexp(-0.0, exp) == -0.0 && exp == 0);
    assert(frexp(0.0, exp) == 0.0 && exp == 0);
}

@safe @nogc nothrow unittest
{
    import std.math.operations : isClose;

    int exp;
    real mantissa = frexp(123.456L, exp);

    // check if values are equal to 19 decimal digits of precision
    assert(isClose(mantissa * pow(2.0L, cast(real) exp), 123.456L, 1e-18));
}

@safe unittest
{
    import std.math.traits : floatTraits, RealFormat, isIdentical;
    import std.meta : AliasSeq;
    import std.typecons : tuple, Tuple;

    static foreach (T; AliasSeq!(real, double, float))
    {{
        Tuple!(T, T, int)[] vals = [   // x,frexp,exp
            tuple(T(0.0),  T( 0.0 ), 0),
            tuple(T(-0.0), T( -0.0), 0),
            tuple(T(1.0),  T( .5  ), 1),
            tuple(T(-1.0), T( -.5 ), 1),
            tuple(T(2.0),  T( .5  ), 2),
            tuple(T(float.min_normal/2.0f), T(.5), -126),
            tuple(T.infinity, T.infinity, int.max),
            tuple(-T.infinity, -T.infinity, int.min),
            tuple(T.nan, T.nan, int.min),
            tuple(-T.nan, -T.nan, int.min),

            // https://issues.dlang.org/show_bug.cgi?id=16026:
            tuple(3 * (T.min_normal * T.epsilon), T( .75), (T.min_exp - T.mant_dig) + 2)
        ];

        foreach (elem; vals)
        {
            T x = elem[0];
            T e = elem[1];
            int exp = elem[2];
            int eptr;
            T v = frexp(x, eptr);
            assert(isIdentical(e, v));
            assert(exp == eptr);
        }

        static if (floatTraits!(T).realFormat == RealFormat.ieeeExtended)
        {
            static T[3][] extendedvals = [ // x,frexp,exp
                [0x1.a5f1c2eb3fe4efp+73L,    0x1.A5F1C2EB3FE4EFp-1L,     74],    // normal
                [0x1.fa01712e8f0471ap-1064L, 0x1.fa01712e8f0471ap-1L, -1063],
                [T.min_normal,      .5, -16381],
                [T.min_normal/2.0L, .5, -16382]    // subnormal
            ];
            foreach (elem; extendedvals)
            {
                T x = elem[0];
                T e = elem[1];
                int exp = cast(int) elem[2];
                int eptr;
                T v = frexp(x, eptr);
                assert(isIdentical(e, v));
                assert(exp == eptr);
            }
        }
    }}

    // CTFE
    alias CtfeFrexpResult= Tuple!(real, int);
    static CtfeFrexpResult ctfeFrexp(T)(const T value)
    {
        int exp;
        auto significand = frexp(value, exp);
        return CtfeFrexpResult(significand, exp);
    }
    static foreach (T; AliasSeq!(real, double, float))
    {{
        enum Tuple!(T, T, int)[] vals = [   // x,frexp,exp
            tuple(T(0.0),  T( 0.0 ), 0),
            tuple(T(-0.0), T( -0.0), 0),
            tuple(T(1.0),  T( .5  ), 1),
            tuple(T(-1.0), T( -.5 ), 1),
            tuple(T(2.0),  T( .5  ), 2),
            tuple(T(float.min_normal/2.0f), T(.5), -126),
            tuple(T.infinity, T.infinity, int.max),
            tuple(-T.infinity, -T.infinity, int.min),
            tuple(T.nan, T.nan, int.min),
            tuple(-T.nan, -T.nan, int.min),

            // https://issues.dlang.org/show_bug.cgi?id=16026:
            tuple(3 * (T.min_normal * T.epsilon), T( .75), (T.min_exp - T.mant_dig) + 2)
        ];

        static foreach (elem; vals)
        {
            static assert(ctfeFrexp(elem[0]) is CtfeFrexpResult(elem[1], elem[2]));
        }

        static if (floatTraits!(T).realFormat == RealFormat.ieeeExtended)
        {
            enum T[3][] extendedvals = [ // x,frexp,exp
                [0x1.a5f1c2eb3fe4efp+73L,    0x1.A5F1C2EB3FE4EFp-1L,     74],    // normal
                [0x1.fa01712e8f0471ap-1064L, 0x1.fa01712e8f0471ap-1L, -1063],
                [T.min_normal,      .5, -16381],
                [T.min_normal/2.0L, .5, -16382]    // subnormal
            ];
            static foreach (elem; extendedvals)
            {
                static assert(ctfeFrexp(elem[0]) is CtfeFrexpResult(elem[1], cast(int) elem[2]));
            }
        }
    }}
}

@safe unittest
{
    import std.meta : AliasSeq;
    void foo() {
        static foreach (T; AliasSeq!(real, double, float))
        {{
            int exp;
            const T a = 1;
            immutable T b = 2;
            auto c = frexp(a, exp);
            auto d = frexp(b, exp);
        }}
    }
}

/******************************************
 * Extracts the exponent of x as a signed integral value.
 *
 * If x is not a special value, the result is the same as
 * $(D cast(int) logb(x)).
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                $(TH ilogb(x))     $(TH Range error?))
 *      $(TR $(TD 0)                 $(TD FP_ILOGB0)   $(TD yes))
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD int.max)     $(TD no))
 *      $(TR $(TD $(NAN))            $(TD FP_ILOGBNAN) $(TD no))
 *      )
 */
int ilogb(T)(const T x) @trusted pure nothrow @nogc
if (isFloatingPoint!T)
{
    import std.math.traits : floatTraits, RealFormat, MANTISSA_MSB, MANTISSA_LSB;

    import core.bitop : bsr;
    alias F = floatTraits!T;

    union floatBits
    {
        T rv;
        ushort[T.sizeof/2] vu;
        uint[T.sizeof/4] vui;
        static if (T.sizeof >= 8)
            ulong[T.sizeof/8] vul;
    }
    floatBits y = void;
    y.rv = x;

    int ex = y.vu[F.EXPPOS_SHORT] & F.EXPMASK;
    static if (F.realFormat == RealFormat.ieeeExtended ||
               F.realFormat == RealFormat.ieeeExtended53)
    {
        if (ex)
        {
            // If exponent is non-zero
            if (ex == F.EXPMASK) // infinity or NaN
            {
                if (y.vul[0] &  0x7FFF_FFFF_FFFF_FFFF)  // NaN
                    return FP_ILOGBNAN;
                else // +-infinity
                    return int.max;
            }
            else
            {
                return ex - F.EXPBIAS - 1;
            }
        }
        else if (!y.vul[0])
        {
            // vf is +-0.0
            return FP_ILOGB0;
        }
        else
        {
            // subnormal
            return ex - F.EXPBIAS - T.mant_dig + 1 + bsr(y.vul[0]);
        }
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple)
    {
        if (ex)    // If exponent is non-zero
        {
            if (ex == F.EXPMASK)
            {
                // infinity or NaN
                if (y.vul[MANTISSA_LSB] | ( y.vul[MANTISSA_MSB] & 0x0000_FFFF_FFFF_FFFF))  // NaN
                    return FP_ILOGBNAN;
                else // +- infinity
                    return int.max;
            }
            else
            {
                return ex - F.EXPBIAS - 1;
            }
        }
        else if ((y.vul[MANTISSA_LSB] | (y.vul[MANTISSA_MSB] & 0x0000_FFFF_FFFF_FFFF)) == 0)
        {
            // vf is +-0.0
            return FP_ILOGB0;
        }
        else
        {
            // subnormal
            const ulong msb = y.vul[MANTISSA_MSB] & 0x0000_FFFF_FFFF_FFFF;
            const ulong lsb = y.vul[MANTISSA_LSB];
            if (msb)
                return ex - F.EXPBIAS - T.mant_dig + 1 + bsr(msb) + 64;
            else
                return ex - F.EXPBIAS - T.mant_dig + 1 + bsr(lsb);
        }
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        if (ex) // If exponent is non-zero
        {
            if (ex == F.EXPMASK)   // infinity or NaN
            {
                if ((y.vul[0] & 0x7FFF_FFFF_FFFF_FFFF) == 0x7FF0_0000_0000_0000)  // +- infinity
                    return int.max;
                else // NaN
                    return FP_ILOGBNAN;
            }
            else
            {
                return ((ex - F.EXPBIAS) >> 4) - 1;
            }
        }
        else if (!(y.vul[0] & 0x7FFF_FFFF_FFFF_FFFF))
        {
            // vf is +-0.0
            return FP_ILOGB0;
        }
        else
        {
            // subnormal
            enum MANTISSAMASK_64 = ((cast(ulong) F.MANTISSAMASK_INT) << 32) | 0xFFFF_FFFF;
            return ((ex - F.EXPBIAS) >> 4) - T.mant_dig + 1 + bsr(y.vul[0] & MANTISSAMASK_64);
        }
    }
    else static if (F.realFormat == RealFormat.ieeeSingle)
    {
        if (ex) // If exponent is non-zero
        {
            if (ex == F.EXPMASK)   // infinity or NaN
            {
                if ((y.vui[0] & 0x7FFF_FFFF) == 0x7F80_0000)  // +- infinity
                    return int.max;
                else // NaN
                    return FP_ILOGBNAN;
            }
            else
            {
                return ((ex - F.EXPBIAS) >> 7) - 1;
            }
        }
        else if (!(y.vui[0] & 0x7FFF_FFFF))
        {
            // vf is +-0.0
            return FP_ILOGB0;
        }
        else
        {
            // subnormal
            const uint mantissa = y.vui[0] & F.MANTISSAMASK_INT;
            return ((ex - F.EXPBIAS) >> 7) - T.mant_dig + 1 + bsr(mantissa);
        }
    }
    else // static if (F.realFormat == RealFormat.ibmExtended)
    {
        assert(0, "ilogb not implemented");
    }
}
/// ditto
int ilogb(T)(const T x) @safe pure nothrow @nogc
if (isIntegral!T && isUnsigned!T)
{
    import core.bitop : bsr;
    if (x == 0)
        return FP_ILOGB0;
    else
    {
        static assert(T.sizeof <= ulong.sizeof, "integer size too large for the current ilogb implementation");
        return bsr(x);
    }
}
/// ditto
int ilogb(T)(const T x) @safe pure nothrow @nogc
if (isIntegral!T && isSigned!T)
{
    import std.traits : Unsigned;
    // Note: abs(x) can not be used because the return type is not Unsigned and
    //       the return value would be wrong for x == int.min
    Unsigned!T absx =  x >= 0 ? x : -x;
    return ilogb(absx);
}

///
@safe pure unittest
{
    assert(ilogb(1) == 0);
    assert(ilogb(3) == 1);
    assert(ilogb(3.0) == 1);
    assert(ilogb(100_000_000) == 26);

    assert(ilogb(0) == FP_ILOGB0);
    assert(ilogb(0.0) == FP_ILOGB0);
    assert(ilogb(double.nan) == FP_ILOGBNAN);
    assert(ilogb(double.infinity) == int.max);
}

/**
Special return values of $(LREF ilogb).
 */
alias FP_ILOGB0   = core.stdc.math.FP_ILOGB0;
/// ditto
alias FP_ILOGBNAN = core.stdc.math.FP_ILOGBNAN;

///
@safe pure unittest
{
    assert(ilogb(0) == FP_ILOGB0);
    assert(ilogb(0.0) == FP_ILOGB0);
    assert(ilogb(double.nan) == FP_ILOGBNAN);
}

@safe nothrow @nogc unittest
{
    import std.math.traits : floatTraits, RealFormat;
    import std.math.operations : nextUp;
    import std.meta : AliasSeq;
    import std.typecons : Tuple;
    static foreach (F; AliasSeq!(float, double, real))
    {{
        alias T = Tuple!(F, int);
        T[13] vals =   // x, ilogb(x)
        [
            T(  F.nan     , FP_ILOGBNAN ),
            T( -F.nan     , FP_ILOGBNAN ),
            T(  F.infinity, int.max     ),
            T( -F.infinity, int.max     ),
            T(  0.0       , FP_ILOGB0   ),
            T( -0.0       , FP_ILOGB0   ),
            T(  2.0       , 1           ),
            T(  2.0001    , 1           ),
            T(  1.9999    , 0           ),
            T(  0.5       , -1          ),
            T(  123.123   , 6           ),
            T( -123.123   , 6           ),
            T(  0.123     , -4          ),
        ];

        foreach (elem; vals)
        {
            assert(ilogb(elem[0]) == elem[1]);
        }
    }}

    // min_normal and subnormals
    assert(ilogb(-float.min_normal) == -126);
    assert(ilogb(nextUp(-float.min_normal)) == -127);
    assert(ilogb(nextUp(-float(0.0))) == -149);
    assert(ilogb(-double.min_normal) == -1022);
    assert(ilogb(nextUp(-double.min_normal)) == -1023);
    assert(ilogb(nextUp(-double(0.0))) == -1074);
    static if (floatTraits!(real).realFormat == RealFormat.ieeeExtended)
    {
        assert(ilogb(-real.min_normal) == -16382);
        assert(ilogb(nextUp(-real.min_normal)) == -16383);
        assert(ilogb(nextUp(-real(0.0))) == -16445);
    }
    else static if (floatTraits!(real).realFormat == RealFormat.ieeeDouble)
    {
        assert(ilogb(-real.min_normal) == -1022);
        assert(ilogb(nextUp(-real.min_normal)) == -1023);
        assert(ilogb(nextUp(-real(0.0))) == -1074);
    }

    // test integer types
    assert(ilogb(0) == FP_ILOGB0);
    assert(ilogb(int.max) == 30);
    assert(ilogb(int.min) == 31);
    assert(ilogb(uint.max) == 31);
    assert(ilogb(long.max) == 62);
    assert(ilogb(long.min) == 63);
    assert(ilogb(ulong.max) == 63);
}

/*******************************************
 * Compute n * 2$(SUPERSCRIPT exp)
 * References: frexp
 */

pragma(inline, true)
real ldexp(real n, int exp)     @safe pure nothrow @nogc { return core.math.ldexp(n, exp); }
///ditto
pragma(inline, true)
double ldexp(double n, int exp) @safe pure nothrow @nogc { return core.math.ldexp(n, exp); }
///ditto
pragma(inline, true)
float ldexp(float n, int exp)   @safe pure nothrow @nogc { return core.math.ldexp(n, exp); }

///
@nogc @safe pure nothrow unittest
{
    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(float, double, real))
    {{
        T r;

        r = ldexp(3.0L, 3);
        assert(r == 24);

        r = ldexp(cast(T) 3.0, cast(int) 3);
        assert(r == 24);

        T n = 3.0;
        int exp = 3;
        r = ldexp(n, exp);
        assert(r == 24);
    }}
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : floatTraits, RealFormat;

    static if (floatTraits!(real).realFormat == RealFormat.ieeeExtended ||
               floatTraits!(real).realFormat == RealFormat.ieeeExtended53 ||
               floatTraits!(real).realFormat == RealFormat.ieeeQuadruple)
    {
        assert(ldexp(1.0L, -16384) == 0x1p-16384L);
        assert(ldexp(1.0L, -16382) == 0x1p-16382L);
        int x;
        real n = frexp(0x1p-16384L, x);
        assert(n == 0.5L);
        assert(x==-16383);
        assert(ldexp(n, x)==0x1p-16384L);
    }
    else static if (floatTraits!(real).realFormat == RealFormat.ieeeDouble)
    {
        assert(ldexp(1.0L, -1024) == 0x1p-1024L);
        assert(ldexp(1.0L, -1022) == 0x1p-1022L);
        int x;
        real n = frexp(0x1p-1024L, x);
        assert(n == 0.5L);
        assert(x==-1023);
        assert(ldexp(n, x)==0x1p-1024L);
    }
    else static assert(false, "Floating point type real not supported");
}

/* workaround https://issues.dlang.org/show_bug.cgi?id=14718
   float parsing depends on platform strtold
@safe pure nothrow @nogc unittest
{
    assert(ldexp(1.0, -1024) == 0x1p-1024);
    assert(ldexp(1.0, -1022) == 0x1p-1022);
    int x;
    double n = frexp(0x1p-1024, x);
    assert(n == 0.5);
    assert(x==-1023);
    assert(ldexp(n, x)==0x1p-1024);
}

@safe pure nothrow @nogc unittest
{
    assert(ldexp(1.0f, -128) == 0x1p-128f);
    assert(ldexp(1.0f, -126) == 0x1p-126f);
    int x;
    float n = frexp(0x1p-128f, x);
    assert(n == 0.5f);
    assert(x==-127);
    assert(ldexp(n, x)==0x1p-128f);
}
*/

@safe @nogc nothrow unittest
{
    import std.math.operations : isClose;

    static real[3][] vals =    // value,exp,ldexp
    [
    [    0,    0,    0],
    [    1,    0,    1],
    [    -1,    0,    -1],
    [    1,    1,    2],
    [    123,    10,    125952],
    [    real.max,    int.max,    real.infinity],
    [    real.max,    -int.max,    0],
    [    real.min_normal,    -int.max,    0],
    ];
    int i;

    for (i = 0; i < vals.length; i++)
    {
        real x = vals[i][0];
        int exp = cast(int) vals[i][1];
        real z = vals[i][2];
        real l = ldexp(x, exp);

        assert(isClose(z, l, 1e-6));
    }

    real function(real, int) pldexp = &ldexp;
    assert(pldexp != null);
}

private
{
    // Coefficients shared across log(), log2(), log10(), log1p().
    template LogCoeffs(T)
    {
        import std.math.traits : floatTraits, RealFormat;

        static if (floatTraits!T.realFormat == RealFormat.ieeeQuadruple)
        {
            // Coefficients for log(1 + x) = x - x^^2/2 + x^^3 P(x)/Q(x)
            // Theoretical peak relative error = 5.3e-37
            static immutable real[13] logP = [
                1.313572404063446165910279910527789794488E4L,
                7.771154681358524243729929227226708890930E4L,
                2.014652742082537582487669938141683759923E5L,
                3.007007295140399532324943111654767187848E5L,
                2.854829159639697837788887080758954924001E5L,
                1.797628303815655343403735250238293741397E5L,
                7.594356839258970405033155585486712125861E4L,
                2.128857716871515081352991964243375186031E4L,
                3.824952356185897735160588078446136783779E3L,
                4.114517881637811823002128927449878962058E2L,
                2.321125933898420063925789532045674660756E1L,
                4.998469661968096229986658302195402690910E-1L,
                1.538612243596254322971797716843006400388E-6L
            ];
            static immutable real[13] logQ = [
                3.940717212190338497730839731583397586124E4L,
                2.626900195321832660448791748036714883242E5L,
                7.777690340007566932935753241556479363645E5L,
                1.347518538384329112529391120390701166528E6L,
                1.514882452993549494932585972882995548426E6L,
                1.158019977462989115839826904108208787040E6L,
                6.132189329546557743179177159925690841200E5L,
                2.248234257620569139969141618556349415120E5L,
                5.605842085972455027590989944010492125825E4L,
                9.147150349299596453976674231612674085381E3L,
                9.104928120962988414618126155557301584078E2L,
                4.839208193348159620282142911143429644326E1L,
                1.0
            ];

            // log2 uses the same coefficients as log.
            alias log2P = logP;
            alias log2Q = logQ;

            // log10 uses the same coefficients as log.
            alias log10P = logP;
            alias log10Q = logQ;

            // Coefficients for log(x) = z + z^^3 R(z^^2)/S(z^^2)
            // where z = 2(x-1)/(x+1)
            // Theoretical peak relative error = 1.1e-35
            static immutable real[6] logR = [
                1.418134209872192732479751274970992665513E5L,
                -8.977257995689735303686582344659576526998E4L,
                2.048819892795278657810231591630928516206E4L,
                -2.024301798136027039250415126250455056397E3L,
                8.057002716646055371965756206836056074715E1L,
                -8.828896441624934385266096344596648080902E-1L
            ];
            static immutable real[7] logS = [
                1.701761051846631278975701529965589676574E6L,
                -1.332535117259762928288745111081235577029E6L,
                4.001557694070773974936904547424676279307E5L,
                -5.748542087379434595104154610899551484314E4L,
                3.998526750980007367835804959888064681098E3L,
                -1.186359407982897997337150403816839480438E2L,
                1.0
            ];
        }
        else static if (floatTraits!T.realFormat == RealFormat.ieeeExtended ||
                        floatTraits!T.realFormat == RealFormat.ieeeExtended53)
        {
            // Coefficients for log(1 + x) = x - x^^2/2 + x^^3 P(x)/Q(x)
            // Theoretical peak relative error = 2.32e-20
            static immutable real[7] logP = [
                2.0039553499201281259648E1L,
                5.7112963590585538103336E1L,
                6.0949667980987787057556E1L,
                2.9911919328553073277375E1L,
                6.5787325942061044846969E0L,
                4.9854102823193375972212E-1L,
                4.5270000862445199635215E-5L,
            ];
            static immutable real[7] logQ = [
                6.0118660497603843919306E1L,
                2.1642788614495947685003E2L,
                3.0909872225312059774938E2L,
                2.2176239823732856465394E2L,
                8.3047565967967209469434E1L,
                1.5062909083469192043167E1L,
                1.0000000000000000000000E0L,
            ];

            // Coefficients for log(1 + x) = x - x^^2/2 + x^^3 P(x)/Q(x)
            // Theoretical peak relative error = 6.2e-22
            static immutable real[7] log2P = [
                1.0747524399916215149070E2L,
                3.4258224542413922935104E2L,
                4.2401812743503691187826E2L,
                2.5620629828144409632571E2L,
                7.7671073698359539859595E1L,
                1.0767376367209449010438E1L,
                4.9962495940332550844739E-1L,
            ];
            static immutable real[8] log2Q = [
                3.2242573199748645407652E2L,
                1.2695660352705325274404E3L,
                2.0307734695595183428202E3L,
                1.6911722418503949084863E3L,
                7.7952888181207260646090E2L,
                1.9444210022760132894510E2L,
                2.3479774160285863271658E1L,
                1.0000000000000000000000E0,
            ];

            // log10 uses the same coefficients as log2.
            alias log10P = log2P;
            alias log10Q = log2Q;

            // Coefficients for log(x) = z + z^^3 R(z^^2)/S(z^^2)
            // where z = 2(x-1)/(x+1)
            // Theoretical peak relative error = 6.16e-22
            static immutable real[4] logR = [
               -3.5717684488096787370998E1L,
                1.0777257190312272158094E1L,
               -7.1990767473014147232598E-1L,
                1.9757429581415468984296E-3L,
            ];
            static immutable real[4] logS = [
               -4.2861221385716144629696E2L,
                1.9361891836232102174846E2L,
               -2.6201045551331104417768E1L,
                1.0000000000000000000000E0L,
            ];
        }
        else static if (floatTraits!T.realFormat == RealFormat.ieeeDouble)
        {
            // Coefficients for log(1 + x) = x - x^^2/2 + x^^3 P(x)/Q(x)
            static immutable double[6] logP = [
                7.70838733755885391666E0,
                1.79368678507819816313E1,
                1.44989225341610930846E1,
                4.70579119878881725854E0,
                4.97494994976747001425E-1,
                1.01875663804580931796E-4,
            ];
            static immutable double[6] logQ = [
                2.31251620126765340583E1,
                7.11544750618563894466E1,
                8.29875266912776603211E1,
                4.52279145837532221105E1,
                1.12873587189167450590E1,
                1.00000000000000000000E0,
            ];

            // log2 uses the same coefficients as log.
            alias log2P = logP;
            alias log2Q = logQ;

            // Coefficients for log(1 + x) = x - x^^2/2 + x^^3 P(x)/Q(x)
            static immutable double[7] logp1P = [
                2.0039553499201281259648E1,
                5.7112963590585538103336E1,
                6.0949667980987787057556E1,
                2.9911919328553073277375E1,
                6.5787325942061044846969E0,
                4.9854102823193375972212E-1,
                4.5270000862445199635215E-5,
            ];
            static immutable double[7] logp1Q = [
                6.0118660497603843919306E1,
                2.1642788614495947685003E2,
                3.0909872225312059774938E2,
                2.2176239823732856465394E2,
                8.3047565967967209469434E1,
                1.5062909083469192043167E1,
                1.0000000000000000000000E0,
            ];

            static immutable double[7] log10P = [
                1.98892446572874072159E1,
                5.67349287391754285487E1,
                6.06127134467767258030E1,
                2.97877425097986925891E1,
                6.56312093769992875930E0,
                4.98531067254050724270E-1,
                4.58482948458143443514E-5,
            ];
            static immutable double[7] log10Q = [
                5.96677339718622216300E1,
                2.14955586696422947765E2,
                3.07254189979530058263E2,
                2.20664384982121929218E2,
                8.27410449222435217021E1,
                1.50314182634250003249E1,
                1.00000000000000000000E0,
            ];

            // Coefficients for log(x) = z + z^^3 R(z)/S(z)
            // where z = 2(x-1)/(x+1)
            static immutable double[3] logR = [
                -6.41409952958715622951E1,
                1.63866645699558079767E1,
                -7.89580278884799154124E-1,
            ];
            static immutable double[4] logS = [
                -7.69691943550460008604E2,
                3.12093766372244180303E2,
                -3.56722798256324312549E1,
                1.00000000000000000000E0,
            ];
        }
        else static if (floatTraits!T.realFormat == RealFormat.ieeeSingle)
        {
            // Coefficients for log(1 + x) = x - x^^2/2 + x^^3 P(x)
            static immutable float[9] logP = [
                 3.3333331174E-1,
                -2.4999993993E-1,
                 2.0000714765E-1,
                -1.6668057665E-1,
                 1.4249322787E-1,
                -1.2420140846E-1,
                 1.1676998740E-1,
                -1.1514610310E-1,
                 7.0376836292E-2,
            ];

            // Coefficients for log(1 + x) = x - x^^2/2 + x^^3 P(x)/Q(x)
            static immutable float[7] logp1P = [
                 2.0039553499E1,
                 5.7112963590E1,
                 6.0949667980E1,
                 2.9911919328E1,
                 6.5787325942E0,
                 4.9854102823E-1,
                 4.5270000862E-5,
            ];
            static immutable float[7] logp1Q = [
                6.01186604976E1,
                2.16427886144E2,
                3.09098722253E2,
                2.21762398237E2,
                8.30475659679E1,
                1.50629090834E1,
                1.00000000000E0,
            ];

            // log2 and log10 uses the same coefficients as log.
            alias log2P = logP;
            alias log10P = logP;
        }
        else
            static assert(0, "no coefficients for log()");
    }
}

/**************************************
 * Calculate the natural logarithm of x.
 *
 *    $(TABLE_SV
 *    $(TR $(TH x)            $(TH log(x))    $(TH divide by 0?) $(TH invalid?))
 *    $(TR $(TD $(PLUSMN)0.0) $(TD -$(INFIN)) $(TD yes)          $(TD no))
 *    $(TR $(TD $(LT)0.0)     $(TD $(NAN))    $(TD no)           $(TD yes))
 *    $(TR $(TD +$(INFIN))    $(TD +$(INFIN)) $(TD no)           $(TD no))
 *    )
 */
pragma(inline, true)
real log(real x) @safe pure nothrow @nogc
{
    version (INLINE_YL2X)
    {
        import std.math.constants : LN2;
        return core.math.yl2x(x, LN2);
    }
    else
        return logImpl(x);
}

/// ditto
pragma(inline, true)
double log(double x) @safe pure nothrow @nogc { return __ctfe ? cast(double) log(cast(real) x) : logImpl(x); }

/// ditto
pragma(inline, true)
float log(float x) @safe pure nothrow @nogc { return __ctfe ? cast(float) log(cast(real) x) : logImpl(x); }

// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log` called with argument types `(int)` matches both "
           ~ "`log(real)`, `log(double)`, and `log(float)`. Cast argument to floating point type instead.")
real log(int x) @safe pure nothrow @nogc { return log(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log` called with argument types `(uint)` matches both "
           ~ "`log(real)`, `log(double)`, and `log(float)`. Cast argument to floating point type instead.")
real log(uint x) @safe pure nothrow @nogc { return log(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log` called with argument types `(long)` matches both "
           ~ "`log(real)`, `log(double)`, and `log(float)`. Cast argument to floating point type instead.")
real log(long x) @safe pure nothrow @nogc { return log(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log` called with argument types `(ulong)` matches both "
           ~ "`log(real)`, `log(double)`, and `log(float)`. Cast argument to floating point type instead.")
real log(ulong x) @safe pure nothrow @nogc { return log(cast(real) x); }

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : feqrel;
    import std.math.constants : E;

    assert(feqrel(log(E), 1) >= real.mant_dig - 1);
}

private T logImpl(T, bool LOG1P = false)(T x) @safe pure nothrow @nogc
{
    import std.math.constants : SQRT1_2;
    import std.math.algebraic : poly;
    import std.math.traits : isInfinity, isNaN, signbit, floatTraits, RealFormat;

    alias coeffs = LogCoeffs!T;
    alias F = floatTraits!T;

    static if (LOG1P)
    {
        const T xm1 = x;
        x = x + 1.0;
    }

    static if (F.realFormat == RealFormat.ieeeExtended ||
               F.realFormat == RealFormat.ieeeExtended53 ||
               F.realFormat == RealFormat.ieeeQuadruple)
    {
        // C1 + C2 = LN2.
        enum T C1 = 6.93145751953125E-1L;
        enum T C2 = 1.428606820309417232121458176568075500134E-6L;
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        enum T C1 = 0.693359375;
        enum T C2 = -2.121944400546905827679e-4;
    }
    else static if (F.realFormat == RealFormat.ieeeSingle)
    {
        enum T C1 = 0.693359375;
        enum T C2 = -2.12194440e-4;
    }
    else
        static assert(0, "Not implemented for this architecture");

    // Special cases.
    if (isNaN(x))
        return x;
    if (isInfinity(x) && !signbit(x))
        return x;
    if (x == 0.0)
        return -T.infinity;
    if (x < 0.0)
        return T.nan;

    // Separate mantissa from exponent.
    // Note, frexp is used so that denormal numbers will be handled properly.
    T y, z;
    int exp;

    x = frexp(x, exp);

    static if (F.realFormat == RealFormat.ieeeDouble ||
               F.realFormat == RealFormat.ieeeExtended ||
               F.realFormat == RealFormat.ieeeExtended53 ||
               F.realFormat == RealFormat.ieeeQuadruple)
    {
        // Logarithm using log(x) = z + z^^3 R(z) / S(z),
        // where z = 2(x - 1)/(x + 1)
        if ((exp > 2) || (exp < -2))
        {
            if (x < SQRT1_2)
            {   // 2(2x - 1)/(2x + 1)
                exp -= 1;
                z = x - 0.5;
                y = 0.5 * z + 0.5;
            }
            else
            {   // 2(x - 1)/(x + 1)
                z = x - 0.5;
                z -= 0.5;
                y = 0.5 * x  + 0.5;
            }
            x = z / y;
            z = x * x;
            z = x * (z * poly(z, coeffs.logR) / poly(z, coeffs.logS));
            z += exp * C2;
            z += x;
            z += exp * C1;

            return z;
        }
    }

    // Logarithm using log(1 + x) = x - .5x^^2 + x^^3 P(x) / Q(x)
    if (x < SQRT1_2)
    {
        exp -= 1;
        static if (LOG1P)
        {
            if (exp != 0)
                x = 2.0 * x - 1.0;
            else
                x = xm1;
        }
        else
            x = 2.0 * x - 1.0;

    }
    else
    {
        static if (LOG1P)
        {
            if (exp != 0)
                x = x - 1.0;
            else
                x = xm1;
        }
        else
            x = x - 1.0;
    }
    z = x * x;
    static if (F.realFormat == RealFormat.ieeeSingle)
        y = x * (z * poly(x, coeffs.logP));
    else
        y = x * (z * poly(x, coeffs.logP) / poly(x, coeffs.logQ));
    y += exp * C2;
    z = y - 0.5 * z;

    // Note, the sum of above terms does not exceed x/4,
    // so it contributes at most about 1/4 lsb to the error.
    z += x;
    z += exp * C1;

    return z;
}

@safe @nogc nothrow unittest
{
    import std.math.traits : floatTraits, RealFormat;
    import std.meta : AliasSeq;

    static void testLog(T)(T[2][] vals)
    {
        import std.math.operations : isClose;
        import std.math.traits : isNaN;
        foreach (ref pair; vals)
        {
            if (isNaN(pair[1]))
                assert(isNaN(log(pair[0])));
            else
                assert(isClose(log(pair[0]), pair[1]));
        }
    }
    static foreach (F; AliasSeq!(float, double, real))
    {{
        scope F[2][] vals = [
            [F(1), F(0x0p+0)], [F(2), F(0x1.62e42fefa39ef358p-1)],
            [F(4), F(0x1.62e42fefa39ef358p+0)], [F(8), F(0x1.0a2b23f3bab73682p+1)],
            [F(16), F(0x1.62e42fefa39ef358p+1)], [F(32), F(0x1.bb9d3beb8c86b02ep+1)],
            [F(64), F(0x1.0a2b23f3bab73682p+2)], [F(128), F(0x1.3687a9f1af2b14ecp+2)],
            [F(256), F(0x1.62e42fefa39ef358p+2)], [F(512), F(0x1.8f40b5ed9812d1c2p+2)],
            [F(1024), F(0x1.bb9d3beb8c86b02ep+2)], [F(2048), F(0x1.e7f9c1e980fa8e98p+2)],
            [F(3), F(0x1.193ea7aad030a976p+0)], [F(5), F(0x1.9c041f7ed8d336bp+0)],
            [F(7), F(0x1.f2272ae325a57546p+0)], [F(15), F(0x1.5aa16394d481f014p+1)],
            [F(17), F(0x1.6aa6bc1fa7f79cfp+1)], [F(31), F(0x1.b78ce48912b59f12p+1)],
            [F(33), F(0x1.bf8d8f4d5b8d1038p+1)], [F(63), F(0x1.09291e8e3181b20ep+2)],
            [F(65), F(0x1.0b292939429755ap+2)], [F(-0), -F.infinity], [F(0), -F.infinity],
            [F(0.1), F(-0x1.26bb1bbb5551582ep+1)], [F(0.25), F(-0x1.62e42fefa39ef358p+0)],
            [F(0.75), F(-0x1.269621134db92784p-2)], [F(0.875), F(-0x1.1178e8227e47bde4p-3)],
            [F(10), F(0x1.26bb1bbb5551582ep+1)], [F(100), F(0x1.26bb1bbb5551582ep+2)],
            [F(10000), F(0x1.26bb1bbb5551582ep+3)],
        ];
        testLog(vals);
    }}
    {
        float[2][16] vals = [
            [float.nan, float.nan],[-float.nan, float.nan],
            [float.infinity, float.infinity], [-float.infinity, float.nan],
            [float.min_normal, -0x1.5d58ap+6f], [-float.min_normal, float.nan],
            [float.max, 0x1.62e43p+6f], [-float.max, float.nan],
            [float.min_normal / 2, -0x1.601e68p+6f], [-float.min_normal / 2, float.nan],
            [float.max / 2, 0x1.601e68p+6f], [-float.max / 2, float.nan],
            [float.min_normal / 3, -0x1.61bd9ap+6f], [-float.min_normal / 3, float.nan],
            [float.max / 3, 0x1.5e7f36p+6f], [-float.max / 3, float.nan],
        ];
        testLog(vals);
    }
    {
        double[2][16] vals = [
            [double.nan, double.nan],[-double.nan, double.nan],
            [double.infinity, double.infinity], [-double.infinity, double.nan],
            [double.min_normal, -0x1.6232bdd7abcd2p+9], [-double.min_normal, double.nan],
            [double.max, 0x1.62e42fefa39efp+9], [-double.max, double.nan],
            [double.min_normal / 2, -0x1.628b76e3a7b61p+9], [-double.min_normal / 2, double.nan],
            [double.max / 2, 0x1.628b76e3a7b61p+9], [-double.max / 2, double.nan],
            [double.min_normal / 3, -0x1.62bf5d2b81354p+9], [-double.min_normal / 3, double.nan],
            [double.max / 3, 0x1.6257909bce36ep+9], [-double.max / 3, double.nan],
        ];
        testLog(vals);
    }
    alias F = floatTraits!real;
    static if (F.realFormat == RealFormat.ieeeExtended || F.realFormat == RealFormat.ieeeQuadruple)
    {{
        real[2][16] vals = [
            [real.nan, real.nan],[-real.nan, real.nan],
            [real.infinity, real.infinity], [-real.infinity, real.nan],
            [real.min_normal, -0x1.62d918ce2421d66p+13L], [-real.min_normal, real.nan],
            [real.max, 0x1.62e42fefa39ef358p+13L], [-real.max, real.nan],
            [real.min_normal / 2, -0x1.62dea45ee3e064dcp+13L], [-real.min_normal / 2, real.nan],
            [real.max / 2, 0x1.62dea45ee3e064dcp+13L], [-real.max / 2, real.nan],
            [real.min_normal / 3, -0x1.62e1e2c3617857e6p+13L], [-real.min_normal / 3, real.nan],
            [real.max / 3, 0x1.62db65fa664871d2p+13L], [-real.max / 3, real.nan],
        ];
        testLog(vals);
    }}
}

/**************************************
 * Calculate the base-10 logarithm of x.
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)            $(TH log10(x))  $(TH divide by 0?) $(TH invalid?))
 *      $(TR $(TD $(PLUSMN)0.0) $(TD -$(INFIN)) $(TD yes)          $(TD no))
 *      $(TR $(TD $(LT)0.0)     $(TD $(NAN))    $(TD no)           $(TD yes))
 *      $(TR $(TD +$(INFIN))    $(TD +$(INFIN)) $(TD no)           $(TD no))
 *      )
 */
pragma(inline, true)
real log10(real x) @safe pure nothrow @nogc
{
    version (INLINE_YL2X)
    {
        import std.math.constants : LOG2;
        return core.math.yl2x(x, LOG2);
    }
    else
        return log10Impl(x);
}

/// ditto
pragma(inline, true)
double log10(double x) @safe pure nothrow @nogc { return __ctfe ? cast(double) log10(cast(real) x) : log10Impl(x); }

/// ditto
pragma(inline, true)
float log10(float x) @safe pure nothrow @nogc { return __ctfe ? cast(float) log10(cast(real) x) : log10Impl(x); }

// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log10` called with argument types `(int)` matches both "
           ~ "`log10(real)`, `log10(double)`, and `log10(float)`. Cast argument to floating point type instead.")
real log10(int x) @safe pure nothrow @nogc { return log10(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log10` called with argument types `(uint)` matches both "
           ~ "`log10(real)`, `log10(double)`, and `log10(float)`. Cast argument to floating point type instead.")
real log10(uint x) @safe pure nothrow @nogc { return log10(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log10` called with argument types `(long)` matches both "
           ~ "`log10(real)`, `log10(double)`, and `log10(float)`. Cast argument to floating point type instead.")
real log10(long x) @safe pure nothrow @nogc { return log10(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log10` called with argument types `(ulong)` matches both "
           ~ "`log10(real)`, `log10(double)`, and `log10(float)`. Cast argument to floating point type instead.")
real log10(ulong x) @safe pure nothrow @nogc { return log10(cast(real) x); }

///
@safe pure nothrow @nogc unittest
{
    import std.math.algebraic : fabs;

    assert(fabs(log10(1000.0L) - 3) < .000001);
}

@safe pure nothrow @nogc unittest
{
    import std.math.algebraic : fabs;

    assert(fabs(log10(1000.0) - 3) < .000001);
    assert(fabs(log10(1000.0f) - 3) < .000001);
}

private T log10Impl(T)(T x) @safe pure nothrow @nogc
{
    import std.math.constants : SQRT1_2;
    import std.math.algebraic : poly;
    import std.math.traits : isNaN, isInfinity, signbit, floatTraits, RealFormat;

    alias coeffs = LogCoeffs!T;
    alias F = floatTraits!T;

    static if (F.realFormat == RealFormat.ieeeExtended ||
               F.realFormat == RealFormat.ieeeExtended53 ||
               F.realFormat == RealFormat.ieeeQuadruple)
    {
        // log10(2) split into two parts.
        enum T L102A =  0.3125L;
        enum T L102B = -1.14700043360188047862611052755069732318101185E-2L;

        // log10(e) split into two parts.
        enum T L10EA =  0.5L;
        enum T L10EB = -6.570551809674817234887108108339491770560299E-2L;
    }
    else static if (F.realFormat == RealFormat.ieeeDouble ||
                    F.realFormat == RealFormat.ieeeSingle)
    {
        enum T L102A =  3.0078125E-1;
        enum T L102B = 2.48745663981195213739E-4;

        enum T L10EA =  4.3359375E-1;
        enum T L10EB = 7.00731903251827651129E-4;
    }
    else
        static assert(0, "Not implemented for this architecture");

    // Special cases are the same as for log.
    if (isNaN(x))
        return x;
    if (isInfinity(x) && !signbit(x))
        return x;
    if (x == 0.0)
        return -T.infinity;
    if (x < 0.0)
        return T.nan;

    // Separate mantissa from exponent.
    // Note, frexp is used so that denormal numbers will be handled properly.
    T y, z;
    int exp;

    x = frexp(x, exp);

    static if (F.realFormat == RealFormat.ieeeExtended ||
               F.realFormat == RealFormat.ieeeExtended53 ||
               F.realFormat == RealFormat.ieeeQuadruple)
    {
        // Logarithm using log(x) = z + z^^3 R(z) / S(z),
        // where z = 2(x - 1)/(x + 1)
        if ((exp > 2) || (exp < -2))
        {
            if (x < SQRT1_2)
            {   // 2(2x - 1)/(2x + 1)
                exp -= 1;
                z = x - 0.5;
                y = 0.5 * z + 0.5;
            }
            else
            {   // 2(x - 1)/(x + 1)
                z = x - 0.5;
                z -= 0.5;
                y = 0.5 * x  + 0.5;
            }
            x = z / y;
            z = x * x;
            y = x * (z * poly(z, coeffs.logR) / poly(z, coeffs.logS));
            goto Ldone;
        }
    }

    // Logarithm using log(1 + x) = x - .5x^^2 + x^^3 P(x) / Q(x)
    if (x < SQRT1_2)
    {
        exp -= 1;
        x = 2.0 * x - 1.0;
    }
    else
        x = x - 1.0;

    z = x * x;
    static if (F.realFormat == RealFormat.ieeeSingle)
        y = x * (z * poly(x, coeffs.log10P));
    else
        y = x * (z * poly(x, coeffs.log10P) / poly(x, coeffs.log10Q));
    y = y - 0.5 * z;

    // Multiply log of fraction by log10(e) and base 2 exponent by log10(2).
    // This sequence of operations is critical and it may be horribly
    // defeated by some compiler optimizers.
Ldone:
    z = y * L10EB;
    z += x * L10EB;
    z += exp * L102B;
    z += y * L10EA;
    z += x * L10EA;
    z += exp * L102A;

    return z;
}

@safe @nogc nothrow unittest
{
    import std.math.traits : floatTraits, RealFormat;
    import std.meta : AliasSeq;

    static void testLog10(T)(T[2][] vals)
    {
        import std.math.operations : isClose;
        import std.math.traits : isNaN;
        foreach (ref pair; vals)
        {
            if (isNaN(pair[1]))
                assert(isNaN(log10(pair[0])));
            else
                assert(isClose(log10(pair[0]), pair[1]));
        }
    }
    static foreach (F; AliasSeq!(float, double, real))
    {{
        scope F[2][] vals = [
            [F(1), F(0x0p+0)], [F(2), F(0x1.34413509f79fef32p-2)],
            [F(4), F(0x1.34413509f79fef32p-1)], [F(8), F(0x1.ce61cf8ef36fe6cap-1)],
            [F(16), F(0x1.34413509f79fef32p+0)], [F(32), F(0x1.8151824c7587eafep+0)],
            [F(64), F(0x1.ce61cf8ef36fe6cap+0)], [F(128), F(0x1.0db90e68b8abf14cp+1)],
            [F(256), F(0x1.34413509f79fef32p+1)], [F(512), F(0x1.5ac95bab3693ed18p+1)],
            [F(1024), F(0x1.8151824c7587eafep+1)], [F(2048), F(0x1.a7d9a8edb47be8e4p+1)],
            [F(3), F(0x1.e8927964fd5fd08cp-2)], [F(5), F(0x1.65df657b04300868p-1)],
            [F(7), F(0x1.b0b0b0b78cc3f296p-1)], [F(15), F(0x1.2d145116c16ff856p+0)],
            [F(17), F(0x1.3afeb354b7d9731ap+0)], [F(31), F(0x1.7dc9e145867e62eap+0)],
            [F(33), F(0x1.84bd545e4baeddp+0)], [F(63), F(0x1.cca1950e4511e192p+0)],
            [F(65), F(0x1.d01b16f9433cf7b8p+0)], [F(-0), -F.infinity], [F(0), -F.infinity],
            [F(0.1), F(-0x1p+0)], [F(0.25), F(-0x1.34413509f79fef32p-1)],
            [F(0.75), F(-0x1.ffbfc2bbc7803758p-4)], [F(0.875), F(-0x1.db11ed766abf432ep-5)],
            [F(10), F(0x1p+0)], [F(100), F(0x1p+1)], [F(10000), F(0x1p+2)],
        ];
        testLog10(vals);
    }}
    {
        float[2][16] vals = [
            [float.nan, float.nan],[-float.nan, float.nan],
            [float.infinity, float.infinity], [-float.infinity, float.nan],
            [float.min_normal, -0x1.2f703p+5f], [-float.min_normal, float.nan],
            [float.max, 0x1.344136p+5f], [-float.max, float.nan],
            [float.min_normal / 2, -0x1.31d8b2p+5f], [-float.min_normal / 2, float.nan],
            [float.max / 2, 0x1.31d8b2p+5f], [-float.max / 2, float.nan],
            [float.min_normal / 3, -0x1.334156p+5f], [-float.min_normal / 3, float.nan],
            [float.max / 3, 0x1.30701p+5f], [-float.max / 3, float.nan],
        ];
        testLog10(vals);
    }
    {
        double[2][16] vals = [
            [double.nan, double.nan],[-double.nan, double.nan],
            [double.infinity, double.infinity], [-double.infinity, double.nan],
            [double.min_normal, -0x1.33a7146f72a42p+8], [-double.min_normal, double.nan],
            [double.max, 0x1.34413509f79ffp+8], [-double.max, double.nan],
            [double.min_normal / 2, -0x1.33f424bcb522p+8], [-double.min_normal / 2, double.nan],
            [double.max / 2, 0x1.33f424bcb522p+8], [-double.max / 2, double.nan],
            [double.min_normal / 3, -0x1.3421390dcbe37p+8], [-double.min_normal / 3, double.nan],
            [double.max / 3, 0x1.33c7106b9e609p+8], [-double.max / 3, double.nan],
        ];
        testLog10(vals);
    }
    alias F = floatTraits!real;
    static if (F.realFormat == RealFormat.ieeeExtended || F.realFormat == RealFormat.ieeeQuadruple)
    {{
        real[2][16] vals = [
            [real.nan, real.nan],[-real.nan, real.nan],
            [real.infinity, real.infinity], [-real.infinity, real.nan],
            [real.min_normal, -0x1.343793004f503232p+12L], [-real.min_normal, real.nan],
            [real.max, 0x1.34413509f79fef32p+12L], [-real.max, real.nan],
            [real.min_normal / 2, -0x1.343c6405237810b2p+12L], [-real.min_normal / 2, real.nan],
            [real.max / 2, 0x1.343c6405237810b2p+12L], [-real.max / 2, real.nan],
            [real.min_normal / 3, -0x1.343f354a34e427bp+12L], [-real.min_normal / 3, real.nan],
            [real.max / 3, 0x1.343992c0120bf9b2p+12L], [-real.max / 3, real.nan],
        ];
        testLog10(vals);
    }}
}

/**
 * Calculates the natural logarithm of 1 + x.
 *
 * For very small x, log1p(x) will be more accurate than
 * log(1 + x).
 *
 *  $(TABLE_SV
 *  $(TR $(TH x)            $(TH log1p(x))     $(TH divide by 0?) $(TH invalid?))
 *  $(TR $(TD $(PLUSMN)0.0) $(TD $(PLUSMN)0.0) $(TD no)           $(TD no))
 *  $(TR $(TD -1.0)         $(TD -$(INFIN))    $(TD yes)          $(TD no))
 *  $(TR $(TD $(LT)-1.0)    $(TD -$(NAN))      $(TD no)           $(TD yes))
 *  $(TR $(TD +$(INFIN))    $(TD +$(INFIN))    $(TD no)           $(TD no))
 *  )
 */
pragma(inline, true)
real log1p(real x) @safe pure nothrow @nogc
{
    version (INLINE_YL2X)
    {
        // On x87, yl2xp1 is valid if and only if -0.5 <= lg(x) <= 0.5,
        //    ie if -0.29 <= x <= 0.414
        import std.math.constants : LN2;
        return (core.math.fabs(x) <= 0.25)  ? core.math.yl2xp1(x, LN2) : core.math.yl2x(x+1, LN2);
    }
    else
        return log1pImpl(x);
}

/// ditto
pragma(inline, true)
double log1p(double x) @safe pure nothrow @nogc { return __ctfe ? cast(double) log1p(cast(real) x) : log1pImpl(x); }

/// ditto
pragma(inline, true)
float log1p(float x) @safe pure nothrow @nogc { return __ctfe ? cast(float) log1p(cast(real) x) : log1pImpl(x); }

// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log1p` called with argument types `(int)` matches both "
           ~ "`log1p(real)`, `log1p(double)`, and `log1p(float)`. Cast argument to floating point type instead.")
real log1p(int x) @safe pure nothrow @nogc { return log1p(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log1p` called with argument types `(uint)` matches both "
           ~ "`log1p(real)`, `log1p(double)`, and `log1p(float)`. Cast argument to floating point type instead.")
real log1p(uint x) @safe pure nothrow @nogc { return log1p(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log1p` called with argument types `(long)` matches both "
           ~ "`log1p(real)`, `log1p(double)`, and `log1p(float)`. Cast argument to floating point type instead.")
real log1p(long x) @safe pure nothrow @nogc { return log1p(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log1p` called with argument types `(ulong)` matches both "
           ~ "`log1p(real)`, `log1p(double)`, and `log1p(float)`. Cast argument to floating point type instead.")
real log1p(ulong x) @safe pure nothrow @nogc { return log1p(cast(real) x); }

///
@safe pure unittest
{
    import std.math.traits : isIdentical, isNaN;
    import std.math.operations : feqrel;

    assert(isIdentical(log1p(0.0), 0.0));
    assert(log1p(1.0).feqrel(0.69314) > 16);

    assert(log1p(-1.0) == -real.infinity);
    assert(isNaN(log1p(-2.0)));
    assert(log1p(real.nan) is real.nan);
    assert(log1p(-real.nan) is -real.nan);
    assert(log1p(real.infinity) == real.infinity);
}

private T log1pImpl(T)(T x) @safe pure nothrow @nogc
{
    import std.math.traits : isNaN, isInfinity, signbit;
    import std.math.algebraic : poly;
    import std.math.constants : SQRT1_2, SQRT2;
    import std.math.traits : floatTraits, RealFormat;

    // Special cases.
    if (isNaN(x) || x == 0.0)
        return x;
    if (isInfinity(x) && !signbit(x))
        return x;
    if (x == -1.0)
        return -T.infinity;
    if (x < -1.0)
        return T.nan;

    alias F = floatTraits!T;
    static if (F.realFormat == RealFormat.ieeeSingle ||
               F.realFormat == RealFormat.ieeeDouble)
    {
        // When the input is within the range 1/sqrt(2) <= x+1 <= sqrt(2), compute
        // log1p inline. Forwarding to log() would otherwise result in inaccuracies.
        const T xp1 = x + 1.0;
        if (xp1 >= SQRT1_2 && xp1 <= SQRT2)
        {
            alias coeffs = LogCoeffs!T;

            T px = poly(x, coeffs.logp1P);
            T qx = poly(x, coeffs.logp1Q);
            const T xx = x * x;
            qx = x + ((cast(T) -0.5) * xx + x * (xx * px / qx));
            return qx;
        }
    }

    return logImpl!(T, true)(x);
}

@safe @nogc nothrow unittest
{
    import std.math.traits : floatTraits, RealFormat;
    import std.meta : AliasSeq;

    static void testLog1p(T)(T[2][] vals)
    {
        import std.math.operations : isClose;
        import std.math.traits : isNaN;
        foreach (ref pair; vals)
        {
            if (isNaN(pair[1]))
                assert(isNaN(log1p(pair[0])));
            else
                assert(isClose(log1p(pair[0]), pair[1]));
        }
    }
    static foreach (F; AliasSeq!(float, double, real))
    {{
        scope F[2][] vals = [
            [F(1), F(0x1.62e42fefa39ef358p-1)], [F(2), F(0x1.193ea7aad030a976p+0)],
            [F(4), F(0x1.9c041f7ed8d336bp+0)], [F(8), F(0x1.193ea7aad030a976p+1)],
            [F(16), F(0x1.6aa6bc1fa7f79cfp+1)], [F(32), F(0x1.bf8d8f4d5b8d1038p+1)],
            [F(64), F(0x1.0b292939429755ap+2)], [F(128), F(0x1.37072a9b5b6cb31p+2)],
            [F(256), F(0x1.63241004e9010ad8p+2)], [F(512), F(0x1.8f60adf041bde2a8p+2)],
            [F(1024), F(0x1.bbad39ebe1cc08b6p+2)], [F(2048), F(0x1.e801c1698ba4395cp+2)],
            [F(3), F(0x1.62e42fefa39ef358p+0)], [F(5), F(0x1.cab0bfa2a2002322p+0)],
            [F(7), F(0x1.0a2b23f3bab73682p+1)], [F(15), F(0x1.62e42fefa39ef358p+1)],
            [F(17), F(0x1.71f7b3a6b918664cp+1)], [F(31), F(0x1.bb9d3beb8c86b02ep+1)],
            [F(33), F(0x1.c35fc81b90df59c6p+1)], [F(63), F(0x1.0a2b23f3bab73682p+2)],
            [F(65), F(0x1.0c234da4a23a6686p+2)], [F(-0), F(-0x0p+0)], [F(0), F(0x0p+0)],
            [F(0.1), F(0x1.8663f793c46c69cp-4)], [F(0.25), F(0x1.c8ff7c79a9a21ac4p-3)],
            [F(0.75), F(0x1.1e85f5e7040d03ep-1)], [F(0.875), F(0x1.41d8fe84672ae646p-1)],
            [F(10), F(0x1.32ee3b77f374bb7cp+1)], [F(100), F(0x1.275e2271bba30be4p+2)],
            [F(10000), F(0x1.26bbed6fbd84182ep+3)],
        ];
        testLog1p(vals);
    }}
    {
        float[2][16] vals = [
            [float.nan, float.nan],[-float.nan, float.nan],
            [float.infinity, float.infinity], [-float.infinity, float.nan],
            [float.min_normal, 0x1p-126f], [-float.min_normal, -0x1p-126f],
            [float.max, 0x1.62e43p+6f], [-float.max, float.nan],
            [float.min_normal / 2, 0x0.8p-126f], [-float.min_normal / 2, -0x0.8p-126f],
            [float.max / 2, 0x1.601e68p+6f], [-float.max / 2, float.nan],
            [float.min_normal / 3, 0x0.555556p-126f], [-float.min_normal / 3, -0x0.555556p-126f],
            [float.max / 3, 0x1.5e7f36p+6f], [-float.max / 3, float.nan],
        ];
        testLog1p(vals);
    }
    {
        double[2][16] vals = [
            [double.nan, double.nan],[-double.nan, double.nan],
            [double.infinity, double.infinity], [-double.infinity, double.nan],
            [double.min_normal, 0x1p-1022], [-double.min_normal, -0x1p-1022],
            [double.max, 0x1.62e42fefa39efp+9], [-double.max, double.nan],
            [double.min_normal / 2, 0x0.8p-1022], [-double.min_normal / 2, -0x0.8p-1022],
            [double.max / 2, 0x1.628b76e3a7b61p+9], [-double.max / 2, double.nan],
            [double.min_normal / 3, 0x0.5555555555555p-1022], [-double.min_normal / 3, -0x0.5555555555555p-1022],
            [double.max / 3, 0x1.6257909bce36ep+9], [-double.max / 3, double.nan],
        ];
        testLog1p(vals);
    }
    alias F = floatTraits!real;
    static if (F.realFormat == RealFormat.ieeeExtended || F.realFormat == RealFormat.ieeeQuadruple)
    {{
        real[2][16] vals = [
            [real.nan, real.nan],[-real.nan, real.nan],
            [real.infinity, real.infinity], [-real.infinity, real.nan],
            [real.min_normal, 0x1p-16382L], [-real.min_normal, -0x1p-16382L],
            [real.max, 0x1.62e42fefa39ef358p+13L], [-real.max, real.nan],
            [real.min_normal / 2, 0x0.8p-16382L], [-real.min_normal / 2, -0x0.8p-16382L],
            [real.max / 2, 0x1.62dea45ee3e064dcp+13L], [-real.max / 2, real.nan],
            [real.min_normal / 3, 0x0.5555555555555556p-16382L], [-real.min_normal / 3, -0x0.5555555555555556p-16382L],
            [real.max / 3, 0x1.62db65fa664871d2p+13L], [-real.max / 3, real.nan],
        ];
        testLog1p(vals);
    }}
}

/***************************************
 * Calculates the base-2 logarithm of x:
 * $(SUB log, 2)x
 *
 *  $(TABLE_SV
 *  $(TR $(TH x)            $(TH log2(x))   $(TH divide by 0?) $(TH invalid?))
 *  $(TR $(TD $(PLUSMN)0.0) $(TD -$(INFIN)) $(TD yes)          $(TD no) )
 *  $(TR $(TD $(LT)0.0)     $(TD $(NAN))    $(TD no)           $(TD yes) )
 *  $(TR $(TD +$(INFIN))    $(TD +$(INFIN)) $(TD no)           $(TD no) )
 *  )
 */
pragma(inline, true)
real log2(real x) @safe pure nothrow @nogc
{
    version (INLINE_YL2X)
        return core.math.yl2x(x, 1.0L);
    else
        return log2Impl(x);
}

/// ditto
pragma(inline, true)
double log2(double x) @safe pure nothrow @nogc { return __ctfe ? cast(double) log2(cast(real) x) : log2Impl(x); }

/// ditto
pragma(inline, true)
float log2(float x) @safe pure nothrow @nogc { return __ctfe ? cast(float) log2(cast(real) x) : log2Impl(x); }

// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log2` called with argument types `(int)` matches both "
           ~ "`log2(real)`, `log2(double)`, and `log2(float)`. Cast argument to floating point type instead.")
real log2(int x) @safe pure nothrow @nogc { return log2(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log2` called with argument types `(uint)` matches both "
           ~ "`log2(real)`, `log2(double)`, and `log2(float)`. Cast argument to floating point type instead.")
real log2(uint x) @safe pure nothrow @nogc { return log2(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log2` called with argument types `(long)` matches both "
           ~ "`log2(real)`, `log2(double)`, and `log2(float)`. Cast argument to floating point type instead.")
real log2(long x) @safe pure nothrow @nogc { return log2(cast(real) x); }
// @@@DEPRECATED_[2.112.0]@@@
deprecated("`std.math.exponential.log2` called with argument types `(ulong)` matches both "
           ~ "`log2(real)`, `log2(double)`, and `log2(float)`. Cast argument to floating point type instead.")
real log2(ulong x) @safe pure nothrow @nogc { return log2(cast(real) x); }

///
@safe unittest
{
    import std.math.operations : isClose;

    assert(isClose(log2(1024.0L), 10));
}

@safe @nogc nothrow unittest
{
    import std.math.operations : isClose;

    // check if values are equal to 19 decimal digits of precision
    assert(isClose(log2(1024.0L), 10, 1e-18));
}

private T log2Impl(T)(T x) @safe pure nothrow @nogc
{
    import std.math.traits : isNaN, isInfinity, signbit;
    import std.math.constants : SQRT1_2, LOG2E;
    import std.math.algebraic : poly;
    import std.math.traits : floatTraits, RealFormat;

    alias coeffs = LogCoeffs!T;
    alias F = floatTraits!T;

    // Special cases are the same as for log.
    if (isNaN(x))
        return x;
    if (isInfinity(x) && !signbit(x))
        return x;
    if (x == 0.0)
        return -T.infinity;
    if (x < 0.0)
        return T.nan;

    // Separate mantissa from exponent.
    // Note, frexp is used so that denormal numbers will be handled properly.
    T y, z;
    int exp;

    x = frexp(x, exp);

    static if (F.realFormat == RealFormat.ieeeDouble ||
               F.realFormat == RealFormat.ieeeExtended ||
               F.realFormat == RealFormat.ieeeExtended53 ||
               F.realFormat == RealFormat.ieeeQuadruple)
    {
        // Logarithm using log(x) = z + z^^3 R(z) / S(z),
        // where z = 2(x - 1)/(x + 1)
        if ((exp > 2) || (exp < -2))
        {
            if (x < SQRT1_2)
            {   // 2(2x - 1)/(2x + 1)
                exp -= 1;
                z = x - 0.5;
                y = 0.5 * z + 0.5;
            }
            else
            {   // 2(x - 1)/(x + 1)
                z = x - 0.5;
                z -= 0.5;
                y = 0.5 * x  + 0.5;
            }
            x = z / y;
            z = x * x;
            y = x * (z * poly(z, coeffs.logR) / poly(z, coeffs.logS));
            goto Ldone;
        }
    }

    // Logarithm using log(1 + x) = x - .5x^^2 + x^^3 P(x) / Q(x)
    if (x < SQRT1_2)
    {
        exp -= 1;
        x = 2.0 * x - 1.0;
    }
    else
        x = x - 1.0;

    z = x * x;
    static if (F.realFormat == RealFormat.ieeeSingle)
        y = x * (z * poly(x, coeffs.log2P));
    else
        y = x * (z * poly(x, coeffs.log2P) / poly(x, coeffs.log2Q));
    y = y - 0.5 * z;

    // Multiply log of fraction by log10(e) and base 2 exponent by log10(2).
    // This sequence of operations is critical and it may be horribly
    // defeated by some compiler optimizers.
Ldone:
    z = y * (LOG2E - 1.0);
    z += x * (LOG2E - 1.0);
    z += y;
    z += x;
    z += exp;

    return z;
}

@safe @nogc nothrow unittest
{
    import std.math.traits : floatTraits, RealFormat;
    import std.meta : AliasSeq;

    static void testLog2(T)(T[2][] vals)
    {
        import std.math.operations : isClose;
        import std.math.traits : isNaN;
        foreach (ref pair; vals)
        {
            if (isNaN(pair[1]))
                assert(isNaN(log2(pair[0])));
            else
                assert(isClose(log2(pair[0]), pair[1]));
        }
    }
    static foreach (F; AliasSeq!(float, double, real))
    {{
        scope F[2][] vals = [
            [F(1), F(0x0p+0)], [F(2), F(0x1p+0)],
            [F(4), F(0x1p+1)], [F(8), F(0x1.8p+1)],
            [F(16), F(0x1p+2)], [F(32), F(0x1.4p+2)],
            [F(64), F(0x1.8p+2)], [F(128), F(0x1.cp+2)],
            [F(256), F(0x1p+3)], [F(512), F(0x1.2p+3)],
            [F(1024), F(0x1.4p+3)], [F(2048), F(0x1.6p+3)],
            [F(3), F(0x1.95c01a39fbd687ap+0)], [F(5), F(0x1.2934f0979a3715fcp+1)],
            [F(7), F(0x1.675767f54042cd9ap+1)], [F(15), F(0x1.f414fdb4982259ccp+1)],
            [F(17), F(0x1.0598fdbeb244c5ap+2)], [F(31), F(0x1.3d118d66c4d4e554p+2)],
            [F(33), F(0x1.42d75a6eb1dfb0e6p+2)], [F(63), F(0x1.7e8bc1179e0caa9cp+2)],
            [F(65), F(0x1.816e79685c2d2298p+2)], [F(-0), -F.infinity], [F(0), -F.infinity],
            [F(0.1), F(-0x1.a934f0979a3715fcp+1)], [F(0.25), F(-0x1p+1)],
            [F(0.75), F(-0x1.a8ff971810a5e182p-2)], [F(0.875), F(-0x1.8a8980abfbd32668p-3)],
            [F(10), F(0x1.a934f0979a3715fcp+1)], [F(100), F(0x1.a934f0979a3715fcp+2)],
            [F(10000), F(0x1.a934f0979a3715fcp+3)],
        ];
        testLog2(vals);
    }}
    {
        float[2][16] vals = [
            [float.nan, float.nan],[-float.nan, float.nan],
            [float.infinity, float.infinity], [-float.infinity, float.nan],
            [float.min_normal, -0x1.f8p+6f], [-float.min_normal, float.nan],
            [float.max, 0x1p+7f], [-float.max, float.nan],
            [float.min_normal / 2, -0x1.fcp+6f], [-float.min_normal / 2, float.nan],
            [float.max / 2, 0x1.fcp+6f], [-float.max / 2, float.nan],
            [float.min_normal / 3, -0x1.fe57p+6f], [-float.min_normal / 3, float.nan],
            [float.max / 3, 0x1.f9a9p+6f], [-float.max / 3, float.nan],
        ];
        testLog2(vals);
    }
    {
        double[2][16] vals = [
            [double.nan, double.nan],[-double.nan, double.nan],
            [double.infinity, double.infinity], [-double.infinity, double.nan],
            [double.min_normal, -0x1.ffp+9], [-double.min_normal, double.nan],
            [double.max, 0x1p+10], [-double.max, double.nan],
            [double.min_normal / 2, -0x1.ff8p+9], [-double.min_normal / 2, double.nan],
            [double.max / 2, 0x1.ff8p+9], [-double.max / 2, double.nan],
            [double.min_normal / 3, -0x1.ffcae00d1cfdfp+9], [-double.min_normal / 3, double.nan],
            [double.max / 3, 0x1.ff351ff2e3021p+9], [-double.max / 3, double.nan],
        ];
        testLog2(vals);
    }
    alias F = floatTraits!real;
    static if (F.realFormat == RealFormat.ieeeExtended || F.realFormat == RealFormat.ieeeQuadruple)
    {{
        real[2][16] vals = [
            [real.nan, real.nan],[-real.nan, real.nan],
            [real.infinity, real.infinity], [-real.infinity, real.nan],
            [real.min_normal, -0x1.fffp+13L], [-real.min_normal, real.nan],
            [real.max, 0x1p+14L], [-real.max, real.nan],
            [real.min_normal / 2, -0x1.fff8p+13L], [-real.min_normal / 2, real.nan],
            [real.max / 2, 0x1.fff8p+13L], [-real.max / 2, real.nan],
            [real.min_normal / 3, -0x1.fffcae00d1cfdeb4p+13L], [-real.min_normal / 3, real.nan],
            [real.max / 3, 0x1.fff351ff2e30214cp+13L], [-real.max / 3, real.nan],
        ];
        testLog2(vals);
    }}
}

/*****************************************
 * Extracts the exponent of x as a signed integral value.
 *
 * If x is subnormal, it is treated as if it were normalized.
 * For a positive, finite x:
 *
 * 1 $(LT)= $(I x) * FLT_RADIX$(SUPERSCRIPT -logb(x)) $(LT) FLT_RADIX
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                 $(TH logb(x))   $(TH divide by 0?) )
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD +$(INFIN)) $(TD no))
 *      $(TR $(TD $(PLUSMN)0.0)      $(TD -$(INFIN)) $(TD yes) )
 *      )
 */
pragma(inline, true)
real logb(real x) @trusted pure nothrow @nogc
{
    version (InlineAsm_X87_MSVC)
        return logbAsm(x);
    else
        return logbImpl(x);
}

/// ditto
pragma(inline, true)
double logb(double x) @trusted pure nothrow @nogc { return logbImpl(x); }

/// ditto
pragma(inline, true)
float logb(float x) @trusted pure nothrow @nogc { return logbImpl(x); }

///
@safe @nogc nothrow unittest
{
    assert(logb(1.0) == 0);
    assert(logb(100.0) == 6);

    assert(logb(0.0) == -real.infinity);
    assert(logb(real.infinity) == real.infinity);
    assert(logb(-real.infinity) == real.infinity);
}

@safe @nogc nothrow unittest
{
    import std.meta : AliasSeq;
    import std.typecons : Tuple;
    import std.math.traits : isNaN;
    static foreach (F; AliasSeq!(float, double, real))
    {{
        alias T = Tuple!(F, F);
        T[17] vals =   // x, logb(x)
        [
            T(1.0          , 0          ),
            T(100.0        , 6          ),
            T(0.0          , -F.infinity),
            T(-0.0         , -F.infinity),
            T(1024         , 10         ),
            T(-2000        , 10         ),
            T(0x0.1p-127   , -131       ),
            T(0x0.01p-127  , -135       ),
            T(0x0.011p-127 , -135       ),
            T(F.nan        , F.nan      ),
            T(-F.nan       , F.nan      ),
            T(F.infinity   , F.infinity ),
            T(-F.infinity  , F.infinity ),
            T(F.min_normal , F.min_exp-1),
            T(-F.min_normal, F.min_exp-1),
            T(F.max        , F.max_exp-1),
            T(-F.max       , F.max_exp-1),
        ];

        foreach (elem; vals)
        {
            if (isNaN(elem[1]))
                assert(isNaN(logb(elem[1])));
            else
                assert(logb(elem[0]) == elem[1]);
        }
    }}
}

version (InlineAsm_X87_MSVC)
private T logbAsm(T)(T x) @trusted pure nothrow @nogc
{
    version (X86_64)
    {
        asm pure nothrow @nogc
        {
            naked                       ;
            fld     real ptr [RCX]      ;
            fxtract                     ;
            fstp    ST(0)               ;
            ret                         ;
        }
    }
    else
    {
        asm pure nothrow @nogc
        {
            fld     x                   ;
            fxtract                     ;
            fstp    ST(0)               ;
        }
    }
}

private T logbImpl(T)(T x) @trusted pure nothrow @nogc
{
    import std.math.traits : isFinite;

    // Handle special cases.
    if (!isFinite(x))
        return x * x;
    if (x == 0)
        return -1 / (x * x);

    return ilogb(x);
}

@safe @nogc nothrow unittest
{
    import std.math.traits : floatTraits, RealFormat;
    import std.meta : AliasSeq;

    static void testLogb(T)(T[2][] vals)
    {
        import std.math.operations : isClose;
        import std.math.traits : isNaN;
        foreach (ref pair; vals)
        {
            if (isNaN(pair[1]))
                assert(isNaN(logb(pair[0])));
            else
                assert(isClose(logb(pair[0]), pair[1]));
        }
    }
    static foreach (F; AliasSeq!(float, double, real))
    {{
        scope F[2][] vals = [
            [F(1), F(0x0p+0)], [F(2), F(0x1p+0)],
            [F(4), F(0x1p+1)], [F(8), F(0x1.8p+1)],
            [F(16), F(0x1p+2)], [F(32), F(0x1.4p+2)],
            [F(64), F(0x1.8p+2)], [F(128), F(0x1.cp+2)],
            [F(256), F(0x1p+3)], [F(512), F(0x1.2p+3)],
            [F(1024), F(0x1.4p+3)], [F(2048), F(0x1.6p+3)],
            [F(3), F(0x1p+0)], [F(5), F(0x1p+1)],
            [F(7), F(0x1p+1)], [F(15), F(0x1.8p+1)],
            [F(17), F(0x1p+2)], [F(31), F(0x1p+2)],
            [F(33), F(0x1.4p+2)], [F(63), F(0x1.4p+2)],
            [F(65), F(0x1.8p+2)], [F(-0), -F.infinity], [F(0), -F.infinity],
            [F(0.1), F(-0x1p+2)], [F(0.25), F(-0x1p+1)],
            [F(0.75), F(-0x1p+0)], [F(0.875), F(-0x1p+0)],
            [F(10), F(0x1.8p+1)], [F(100), F(0x1.8p+2)],
            [F(10000), F(0x1.ap+3)],
        ];
        testLogb(vals);
    }}
    {
        float[2][16] vals = [
            [float.nan, float.nan],[-float.nan, float.nan],
            [float.infinity, float.infinity], [-float.infinity, float.infinity],
            [float.min_normal, -0x1.f8p+6f], [-float.min_normal, -0x1.f8p+6f],
            [float.max, 0x1.fcp+6f], [-float.max, 0x1.fcp+6f],
            [float.min_normal / 2, -0x1.fcp+6f], [-float.min_normal / 2, -0x1.fcp+6f],
            [float.max / 2, 0x1.f8p+6f], [-float.max / 2, 0x1.f8p+6f],
            [float.min_normal / 3, -0x1p+7f], [-float.min_normal / 3, -0x1p+7f],
            [float.max / 3, 0x1.f8p+6f], [-float.max / 3, 0x1.f8p+6f],
        ];
        testLogb(vals);
    }
    {
        double[2][16] vals = [
            [double.nan, double.nan],[-double.nan, double.nan],
            [double.infinity, double.infinity], [-double.infinity, double.infinity],
            [double.min_normal, -0x1.ffp+9], [-double.min_normal, -0x1.ffp+9],
            [double.max, 0x1.ff8p+9], [-double.max, 0x1.ff8p+9],
            [double.min_normal / 2, -0x1.ff8p+9], [-double.min_normal / 2, -0x1.ff8p+9],
            [double.max / 2, 0x1.ffp+9], [-double.max / 2, 0x1.ffp+9],
            [double.min_normal / 3, -0x1p+10], [-double.min_normal / 3, -0x1p+10],
            [double.max / 3, 0x1.ffp+9], [-double.max / 3, 0x1.ffp+9],
        ];
        testLogb(vals);
    }
    alias F = floatTraits!real;
    static if (F.realFormat == RealFormat.ieeeExtended || F.realFormat == RealFormat.ieeeQuadruple)
    {{
        real[2][16] vals = [
            [real.nan, real.nan],[-real.nan, real.nan],
            [real.infinity, real.infinity], [-real.infinity, real.infinity],
            [real.min_normal, -0x1.fffp+13L], [-real.min_normal, -0x1.fffp+13L],
            [real.max, 0x1.fff8p+13L], [-real.max, 0x1.fff8p+13L],
            [real.min_normal / 2, -0x1.fff8p+13L], [-real.min_normal / 2, -0x1.fff8p+13L],
            [real.max / 2, 0x1.fffp+13L], [-real.max / 2, 0x1.fffp+13L],
            [real.min_normal / 3, -0x1p+14L], [-real.min_normal / 3, -0x1p+14L],
            [real.max / 3, 0x1.fffp+13L], [-real.max / 3, 0x1.fffp+13L],
        ];
        testLogb(vals);
    }}
}

/*************************************
 * Efficiently calculates x * 2$(SUPERSCRIPT n).
 *
 * scalbn handles underflow and overflow in
 * the same fashion as the basic arithmetic operators.
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                 $(TH scalb(x)))
 *      $(TR $(TD $(PLUSMNINF))      $(TD $(PLUSMNINF)) )
 *      $(TR $(TD $(PLUSMN)0.0)      $(TD $(PLUSMN)0.0) )
 *      )
 */
pragma(inline, true)
real scalbn(real x, int n) @safe pure nothrow @nogc { return _scalbn(x,n); }

/// ditto
pragma(inline, true)
double scalbn(double x, int n) @safe pure nothrow @nogc { return _scalbn(x,n); }

/// ditto
pragma(inline, true)
float scalbn(float x, int n) @safe pure nothrow @nogc { return _scalbn(x,n); }

///
@safe pure nothrow @nogc unittest
{
    assert(scalbn(0x1.2345678abcdefp0L, 999) == 0x1.2345678abcdefp999L);
    assert(scalbn(-real.infinity, 5) == -real.infinity);
    assert(scalbn(2.0,10) == 2048.0);
    assert(scalbn(2048.0f,-10) == 2.0f);
}

pragma(inline, true)
private F _scalbn(F)(F x, int n)
{
    import std.math.traits : isInfinity;

    if (__ctfe)
    {
        // Handle special cases.
        if (x == F(0.0) || isInfinity(x))
            return x;
    }
    return core.math.ldexp(x, n);
}

@safe pure nothrow @nogc unittest
{
    // CTFE-able test
    static assert(scalbn(0x1.2345678abcdefp0L, 999) == 0x1.2345678abcdefp999L);
    static assert(scalbn(-real.infinity, 5) == -real.infinity);
    // Test with large exponent delta n where the result is in bounds but 2.0L ^^ n is not.
    enum initialExponent = real.min_exp + 2, resultExponent = real.max_exp - 2;
    enum int n = resultExponent - initialExponent;
    enum real x = 0x1.2345678abcdefp0L * (2.0L ^^ initialExponent);
    enum staticResult = scalbn(x, n);
    static assert(staticResult == 0x1.2345678abcdefp0L * (2.0L ^^ resultExponent));
    assert(scalbn(x, n) == staticResult);
}

