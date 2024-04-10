// Written in the D programming language.

/**
This is a submodule of $(MREF std, math).

It contains several functions for introspection on numerical values.

Copyright: Copyright The D Language Foundation 2000 - 2011.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright), Don Clugston,
           Conversion of CEPHES math library to D by Iain Buclaw and David Nadlinger
Source: $(PHOBOSSRC std/math/traits.d)

Macros:
    NAN = $(RED NAN)
    PLUSMN = &plusmn;
    INFIN = &infin;
 */

module std.math.traits;

import std.traits : isFloatingPoint, isIntegral, isNumeric, isSigned;

/*********************************
 * Determines if $(D_PARAM x) is NaN.
 * Params:
 *  x = a floating point number.
 * Returns:
 *  `true` if $(D_PARAM x) is Nan.
 */
bool isNaN(X)(X x) @nogc @trusted pure nothrow
if (isFloatingPoint!(X))
{
    version (all)
    {
        return x != x;
    }
    else
    {
        /*
        Code kept for historical context. At least on Intel, the simple test
        x != x uses one dedicated instruction (ucomiss/ucomisd) that runs in one
        cycle. Code for 80- and 128-bits is larger but still smaller than the
        integrals-based solutions below. Future revisions may enable the code
        below conditionally depending on hardware.
        */
        alias F = floatTraits!(X);
        static if (F.realFormat == RealFormat.ieeeSingle)
        {
            const uint p = *cast(uint *)&x;
            // Sign bit (MSB) is irrelevant so mask it out.
            // Next 8 bits should be all set.
            // At least one bit among the least significant 23 bits should be set.
            return (p & 0x7FFF_FFFF) > 0x7F80_0000;
        }
        else static if (F.realFormat == RealFormat.ieeeDouble)
        {
            const ulong  p = *cast(ulong *)&x;
            // Sign bit (MSB) is irrelevant so mask it out.
            // Next 11 bits should be all set.
            // At least one bit among the least significant 52 bits should be set.
            return (p & 0x7FFF_FFFF_FFFF_FFFF) > 0x7FF0_0000_0000_0000;
        }
        else static if (F.realFormat == RealFormat.ieeeExtended ||
                        F.realFormat == RealFormat.ieeeExtended53)
        {
            const ushort e = F.EXPMASK & (cast(ushort *)&x)[F.EXPPOS_SHORT];
            const ulong ps = *cast(ulong *)&x;
            return e == F.EXPMASK &&
                ps & 0x7FFF_FFFF_FFFF_FFFF; // not infinity
        }
        else static if (F.realFormat == RealFormat.ieeeQuadruple)
        {
            const ushort e = F.EXPMASK & (cast(ushort *)&x)[F.EXPPOS_SHORT];
            const ulong psLsb = (cast(ulong *)&x)[MANTISSA_LSB];
            const ulong psMsb = (cast(ulong *)&x)[MANTISSA_MSB];
            return e == F.EXPMASK &&
                (psLsb | (psMsb& 0x0000_FFFF_FFFF_FFFF)) != 0;
        }
        else
        {
            return x != x;
        }
    }
}

///
@safe pure nothrow @nogc unittest
{
    assert( isNaN(float.init));
    assert( isNaN(-double.init));
    assert( isNaN(real.nan));
    assert( isNaN(-real.nan));
    assert(!isNaN(cast(float) 53.6));
    assert(!isNaN(cast(real)-53.6));
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;

    static foreach (T; AliasSeq!(float, double, real))
    {{
        // CTFE-able tests
        assert(isNaN(T.init));
        assert(isNaN(-T.init));
        assert(isNaN(T.nan));
        assert(isNaN(-T.nan));
        assert(!isNaN(T.infinity));
        assert(!isNaN(-T.infinity));
        assert(!isNaN(cast(T) 53.6));
        assert(!isNaN(cast(T)-53.6));

        // Runtime tests
        shared T f;
        f = T.init;
        assert(isNaN(f));
        assert(isNaN(-f));
        f = T.nan;
        assert(isNaN(f));
        assert(isNaN(-f));
        f = T.infinity;
        assert(!isNaN(f));
        assert(!isNaN(-f));
        f = cast(T) 53.6;
        assert(!isNaN(f));
        assert(!isNaN(-f));
    }}
}

/*********************************
 * Determines if $(D_PARAM x) is finite.
 * Params:
 *  x = a floating point number.
 * Returns:
 *  `true` if $(D_PARAM x) is finite.
 */
bool isFinite(X)(X x) @trusted pure nothrow @nogc
{
    import std.math.traits : floatTraits, RealFormat;

    static if (__traits(isFloating, X))
        if (__ctfe)
            return x == x && x != X.infinity && x != -X.infinity;
    alias F = floatTraits!(X);
    ushort* pe = cast(ushort *)&x;
    return (pe[F.EXPPOS_SHORT] & F.EXPMASK) != F.EXPMASK;
}

///
@safe pure nothrow @nogc unittest
{
    assert( isFinite(1.23f));
    assert( isFinite(float.max));
    assert( isFinite(float.min_normal));
    assert(!isFinite(float.nan));
    assert(!isFinite(float.infinity));
}

@safe pure nothrow @nogc unittest
{
    assert(isFinite(1.23));
    assert(isFinite(double.max));
    assert(isFinite(double.min_normal));
    assert(!isFinite(double.nan));
    assert(!isFinite(double.infinity));

    assert(isFinite(1.23L));
    assert(isFinite(real.max));
    assert(isFinite(real.min_normal));
    assert(!isFinite(real.nan));
    assert(!isFinite(real.infinity));

    //CTFE
    static assert(isFinite(1.23));
    static assert(isFinite(double.max));
    static assert(isFinite(double.min_normal));
    static assert(!isFinite(double.nan));
    static assert(!isFinite(double.infinity));

    static assert(isFinite(1.23L));
    static assert(isFinite(real.max));
    static assert(isFinite(real.min_normal));
    static assert(!isFinite(real.nan));
    static assert(!isFinite(real.infinity));
}


/*********************************
 * Determines if $(D_PARAM x) is normalized.
 *
 * A normalized number must not be zero, subnormal, infinite nor $(NAN).
 *
 * Params:
 *  x = a floating point number.
 * Returns:
 *  `true` if $(D_PARAM x) is normalized.
 */

/* Need one for each format because subnormal floats might
 * be converted to normal reals.
 */
bool isNormal(X)(X x) @trusted pure nothrow @nogc
{
    import std.math.traits : floatTraits, RealFormat;

    static if (__traits(isFloating, X))
        if (__ctfe)
            return (x <= -X.min_normal && x != -X.infinity) || (x >= X.min_normal && x != X.infinity);
    alias F = floatTraits!(X);
    ushort e = F.EXPMASK & (cast(ushort *)&x)[F.EXPPOS_SHORT];
    return (e != F.EXPMASK && e != 0);
}

///
@safe pure nothrow @nogc unittest
{
    float f = 3;
    double d = 500;
    real e = 10e+48;

    assert(isNormal(f));
    assert(isNormal(d));
    assert(isNormal(e));
    f = d = e = 0;
    assert(!isNormal(f));
    assert(!isNormal(d));
    assert(!isNormal(e));
    assert(!isNormal(real.infinity));
    assert(isNormal(-real.max));
    assert(!isNormal(real.min_normal/4));

}

@safe pure nothrow @nogc unittest
{
    // CTFE
    enum float f = 3;
    enum double d = 500;
    enum real e = 10e+48;

    static assert(isNormal(f));
    static assert(isNormal(d));
    static assert(isNormal(e));

    static assert(!isNormal(0.0f));
    static assert(!isNormal(0.0));
    static assert(!isNormal(0.0L));
    static assert(!isNormal(real.infinity));
    static assert(isNormal(-real.max));
    static assert(!isNormal(real.min_normal/4));
}

/*********************************
 * Determines if $(D_PARAM x) is subnormal.
 *
 * Subnormals (also known as "denormal number"), have a 0 exponent
 * and a 0 most significant mantissa bit.
 *
 * Params:
 *  x = a floating point number.
 * Returns:
 *  `true` if $(D_PARAM x) is a denormal number.
 */
bool isSubnormal(X)(X x) @trusted pure nothrow @nogc
{
    import std.math.traits : floatTraits, RealFormat, MANTISSA_MSB, MANTISSA_LSB;

    static if (__traits(isFloating, X))
        if (__ctfe)
            return -X.min_normal < x && x < X.min_normal;
    /*
        Need one for each format because subnormal floats might
        be converted to normal reals.
    */
    alias F = floatTraits!(X);
    static if (F.realFormat == RealFormat.ieeeSingle)
    {
        uint *p = cast(uint *)&x;
        return (*p & F.EXPMASK_INT) == 0 && *p & F.MANTISSAMASK_INT;
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        uint *p = cast(uint *)&x;
        return (p[MANTISSA_MSB] & F.EXPMASK_INT) == 0
            && (p[MANTISSA_LSB] || p[MANTISSA_MSB] & F.MANTISSAMASK_INT);
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple)
    {
        ushort e = F.EXPMASK & (cast(ushort *)&x)[F.EXPPOS_SHORT];
        long*   ps = cast(long *)&x;
        return (e == 0 &&
          ((ps[MANTISSA_LSB]|(ps[MANTISSA_MSB]& 0x0000_FFFF_FFFF_FFFF)) != 0));
    }
    else static if (F.realFormat == RealFormat.ieeeExtended ||
                    F.realFormat == RealFormat.ieeeExtended53)
    {
        ushort* pe = cast(ushort *)&x;
        long*   ps = cast(long *)&x;

        return (pe[F.EXPPOS_SHORT] & F.EXPMASK) == 0 && *ps > 0;
    }
    else
    {
        static assert(false, "Not implemented for this architecture");
    }
}

///
@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;

    static foreach (T; AliasSeq!(float, double, real))
    {{
        T f;
        for (f = 1.0; !isSubnormal(f); f /= 2)
            assert(f != 0);
    }}
}

@safe pure nothrow @nogc unittest
{
    static bool subnormalTest(T)()
    {
        T f;
        for (f = 1.0; !isSubnormal(f); f /= 2)
            if (f == 0)
                return false;
        return true;
    }
    static assert(subnormalTest!float());
    static assert(subnormalTest!double());
    static assert(subnormalTest!real());
}

/*********************************
 * Determines if $(D_PARAM x) is $(PLUSMN)$(INFIN).
 * Params:
 *  x = a floating point number.
 * Returns:
 *  `true` if $(D_PARAM x) is $(PLUSMN)$(INFIN).
 */
bool isInfinity(X)(X x) @nogc @trusted pure nothrow
if (isFloatingPoint!(X))
{
    import std.math.traits : floatTraits, RealFormat, MANTISSA_MSB, MANTISSA_LSB;

    alias F = floatTraits!(X);
    static if (F.realFormat == RealFormat.ieeeSingle)
    {
        return ((*cast(uint *)&x) & 0x7FFF_FFFF) == 0x7F80_0000;
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        return ((*cast(ulong *)&x) & 0x7FFF_FFFF_FFFF_FFFF)
            == 0x7FF0_0000_0000_0000;
    }
    else static if (F.realFormat == RealFormat.ieeeExtended ||
                    F.realFormat == RealFormat.ieeeExtended53)
    {
        const ushort e = cast(ushort)(F.EXPMASK & (cast(ushort *)&x)[F.EXPPOS_SHORT]);
        const ulong ps = *cast(ulong *)&x;

        // On Motorola 68K, infinity can have hidden bit = 1 or 0. On x86, it is always 1.
        return e == F.EXPMASK && (ps & 0x7FFF_FFFF_FFFF_FFFF) == 0;
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple)
    {
        const long psLsb = (cast(long *)&x)[MANTISSA_LSB];
        const long psMsb = (cast(long *)&x)[MANTISSA_MSB];
        return (psLsb == 0)
            && (psMsb & 0x7FFF_FFFF_FFFF_FFFF) == 0x7FFF_0000_0000_0000;
    }
    else
    {
        return (x < -X.max) || (X.max < x);
    }
}

///
@nogc @safe pure nothrow unittest
{
    assert(!isInfinity(float.init));
    assert(!isInfinity(-float.init));
    assert(!isInfinity(float.nan));
    assert(!isInfinity(-float.nan));
    assert(isInfinity(float.infinity));
    assert(isInfinity(-float.infinity));
    assert(isInfinity(-1.0f / 0.0f));
}

@safe pure nothrow @nogc unittest
{
    // CTFE-able tests
    assert(!isInfinity(double.init));
    assert(!isInfinity(-double.init));
    assert(!isInfinity(double.nan));
    assert(!isInfinity(-double.nan));
    assert(isInfinity(double.infinity));
    assert(isInfinity(-double.infinity));
    assert(isInfinity(-1.0 / 0.0));

    assert(!isInfinity(real.init));
    assert(!isInfinity(-real.init));
    assert(!isInfinity(real.nan));
    assert(!isInfinity(-real.nan));
    assert(isInfinity(real.infinity));
    assert(isInfinity(-real.infinity));
    assert(isInfinity(-1.0L / 0.0L));

    // Runtime tests
    shared float f;
    f = float.init;
    assert(!isInfinity(f));
    assert(!isInfinity(-f));
    f = float.nan;
    assert(!isInfinity(f));
    assert(!isInfinity(-f));
    f = float.infinity;
    assert(isInfinity(f));
    assert(isInfinity(-f));
    f = (-1.0f / 0.0f);
    assert(isInfinity(f));

    shared double d;
    d = double.init;
    assert(!isInfinity(d));
    assert(!isInfinity(-d));
    d = double.nan;
    assert(!isInfinity(d));
    assert(!isInfinity(-d));
    d = double.infinity;
    assert(isInfinity(d));
    assert(isInfinity(-d));
    d = (-1.0 / 0.0);
    assert(isInfinity(d));

    shared real e;
    e = real.init;
    assert(!isInfinity(e));
    assert(!isInfinity(-e));
    e = real.nan;
    assert(!isInfinity(e));
    assert(!isInfinity(-e));
    e = real.infinity;
    assert(isInfinity(e));
    assert(isInfinity(-e));
    e = (-1.0L / 0.0L);
    assert(isInfinity(e));
}

@nogc @safe pure nothrow unittest
{
    import std.meta : AliasSeq;
    static bool foo(T)(inout T x) { return isInfinity(x); }
    foreach (T; AliasSeq!(float, double, real))
    {
        assert(!foo(T(3.14f)));
        assert(foo(T.infinity));
    }
}

/*********************************
 * Is the binary representation of x identical to y?
 */
bool isIdentical(real x, real y) @trusted pure nothrow @nogc
{
    import std.math.traits : floatTraits, RealFormat;

    // We're doing a bitwise comparison so the endianness is irrelevant.
    long*   pxs = cast(long *)&x;
    long*   pys = cast(long *)&y;
    alias F = floatTraits!(real);
    static if (F.realFormat == RealFormat.ieeeDouble)
    {
        return pxs[0] == pys[0];
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple)
    {
        return pxs[0] == pys[0] && pxs[1] == pys[1];
    }
    else static if (F.realFormat == RealFormat.ieeeExtended)
    {
        ushort* pxe = cast(ushort *)&x;
        ushort* pye = cast(ushort *)&y;
        return pxe[4] == pye[4] && pxs[0] == pys[0];
    }
    else
    {
        assert(0, "isIdentical not implemented");
    }
}

///
@safe @nogc pure nothrow unittest
{
    assert( isIdentical(0.0, 0.0));
    assert( isIdentical(1.0, 1.0));
    assert( isIdentical(real.infinity, real.infinity));
    assert( isIdentical(-real.infinity, -real.infinity));

    assert(!isIdentical(0.0, -0.0));
    assert(!isIdentical(real.nan, -real.nan));
    assert(!isIdentical(real.infinity, -real.infinity));
}

/*********************************
 * Return 1 if sign bit of e is set, 0 if not.
 */
int signbit(X)(X x) @nogc @trusted pure nothrow
{
    import std.math.traits : floatTraits, RealFormat;

    if (__ctfe)
    {
        double dval = cast(double) x; // Precision can increase or decrease but sign won't change (even NaN).
        return 0 > *cast(long*) &dval;
    }

    alias F = floatTraits!(X);
    return ((cast(ubyte *)&x)[F.SIGNPOS_BYTE] & 0x80) != 0;
}

///
@nogc @safe pure nothrow unittest
{
    assert(!signbit(float.nan));
    assert(signbit(-float.nan));
    assert(!signbit(168.1234f));
    assert(signbit(-168.1234f));
    assert(!signbit(0.0f));
    assert(signbit(-0.0f));
    assert(signbit(-float.max));
    assert(!signbit(float.max));

    assert(!signbit(double.nan));
    assert(signbit(-double.nan));
    assert(!signbit(168.1234));
    assert(signbit(-168.1234));
    assert(!signbit(0.0));
    assert(signbit(-0.0));
    assert(signbit(-double.max));
    assert(!signbit(double.max));

    assert(!signbit(real.nan));
    assert(signbit(-real.nan));
    assert(!signbit(168.1234L));
    assert(signbit(-168.1234L));
    assert(!signbit(0.0L));
    assert(signbit(-0.0L));
    assert(signbit(-real.max));
    assert(!signbit(real.max));
}

@nogc @safe pure nothrow unittest
{
    // CTFE
    static assert(!signbit(float.nan));
    static assert(signbit(-float.nan));
    static assert(!signbit(168.1234f));
    static assert(signbit(-168.1234f));
    static assert(!signbit(0.0f));
    static assert(signbit(-0.0f));
    static assert(signbit(-float.max));
    static assert(!signbit(float.max));

    static assert(!signbit(double.nan));
    static assert(signbit(-double.nan));
    static assert(!signbit(168.1234));
    static assert(signbit(-168.1234));
    static assert(!signbit(0.0));
    static assert(signbit(-0.0));
    static assert(signbit(-double.max));
    static assert(!signbit(double.max));

    static assert(!signbit(real.nan));
    static assert(signbit(-real.nan));
    static assert(!signbit(168.1234L));
    static assert(signbit(-168.1234L));
    static assert(!signbit(0.0L));
    static assert(signbit(-0.0L));
    static assert(signbit(-real.max));
    static assert(!signbit(real.max));
}

/**
Params:
    to = the numeric value to use
    from = the sign value to use
Returns:
    a value composed of to with from's sign bit.
 */
R copysign(R, X)(R to, X from) @trusted pure nothrow @nogc
if (isFloatingPoint!(R) && isFloatingPoint!(X))
{
    import std.math.traits : floatTraits, RealFormat;

    if (__ctfe)
    {
        return signbit(to) == signbit(from) ? to : -to;
    }
    ubyte* pto   = cast(ubyte *)&to;
    const ubyte* pfrom = cast(ubyte *)&from;

    alias T = floatTraits!(R);
    alias F = floatTraits!(X);
    pto[T.SIGNPOS_BYTE] &= 0x7F;
    pto[T.SIGNPOS_BYTE] |= pfrom[F.SIGNPOS_BYTE] & 0x80;
    return to;
}

/// ditto
R copysign(R, X)(X to, R from) @trusted pure nothrow @nogc
if (isIntegral!(X) && isFloatingPoint!(R))
{
    return copysign(cast(R) to, from);
}

///
@safe pure nothrow @nogc unittest
{
    assert(copysign(1.0, 1.0) == 1.0);
    assert(copysign(1.0, -0.0) == -1.0);
    assert(copysign(1UL, -1.0) == -1.0);
    assert(copysign(-1.0, -1.0) == -1.0);

    assert(copysign(real.infinity, -1.0) == -real.infinity);
    assert(copysign(real.nan, 1.0) is real.nan);
    assert(copysign(-real.nan, 1.0) is real.nan);
    assert(copysign(real.nan, -1.0) is -real.nan);
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;

    static foreach (X; AliasSeq!(float, double, real, int, long))
    {
        static foreach (Y; AliasSeq!(float, double, real))
        {{
            X x = 21;
            Y y = 23.8;
            Y e = void;

            e = copysign(x, y);
            assert(e == 21.0);

            e = copysign(-x, y);
            assert(e == 21.0);

            e = copysign(x, -y);
            assert(e == -21.0);

            e = copysign(-x, -y);
            assert(e == -21.0);

            static if (isFloatingPoint!X)
            {
                e = copysign(X.nan, y);
                assert(isNaN(e) && !signbit(e));

                e = copysign(X.nan, -y);
                assert(isNaN(e) && signbit(e));
            }
        }}
    }
    // CTFE
    static foreach (X; AliasSeq!(float, double, real, int, long))
    {
        static foreach (Y; AliasSeq!(float, double, real))
        {{
            enum X x = 21;
            enum Y y = 23.8;

            assert(21.0 == copysign(x, y));
            assert(21.0 == copysign(-x, y));
            assert(-21.0 == copysign(x, -y));
            assert(-21.0 == copysign(-x, -y));

            static if (isFloatingPoint!X)
            {
                static assert(isNaN(copysign(X.nan, y)) && !signbit(copysign(X.nan, y)));
                assert(isNaN(copysign(X.nan, -y)) && signbit(copysign(X.nan, -y)));
            }
        }}
    }
}

/*********************************
Returns `-1` if $(D x < 0), `x` if $(D x == 0), `1` if
$(D x > 0), and $(NAN) if x==$(NAN).
 */
F sgn(F)(F x) @safe pure nothrow @nogc
if (isFloatingPoint!F || isIntegral!F)
{
    // @@@TODO@@@: make this faster
    return x > 0 ? 1 : x < 0 ? -1 : x;
}

///
@safe pure nothrow @nogc unittest
{
    assert(sgn(168.1234) == 1);
    assert(sgn(-168.1234) == -1);
    assert(sgn(0.0) == 0);
    assert(sgn(-0.0) == 0);
}

/**
Check whether a number is an integer power of two.

Note that only positive numbers can be integer powers of two. This
function always return `false` if `x` is negative or zero.

Params:
    x = the number to test

Returns:
    `true` if `x` is an integer power of two.
*/
bool isPowerOf2(X)(const X x) pure @safe nothrow @nogc
if (isNumeric!X)
{
    import std.math.exponential : frexp;

    static if (isFloatingPoint!X)
    {
        int exp;
        const X sig = frexp(x, exp);

        return (exp != int.min) && (sig is cast(X) 0.5L);
    }
    else
    {
        static if (isSigned!X)
        {
            auto y = cast(typeof(x + 0))x;
            return y > 0 && !(y & (y - 1));
        }
        else
        {
            auto y = cast(typeof(x + 0u))x;
            return (y & -y) > (y - 1);
        }
    }
}
///
@safe unittest
{
    import std.math.exponential : pow;

    assert( isPowerOf2(1.0L));
    assert( isPowerOf2(2.0L));
    assert( isPowerOf2(0.5L));
    assert( isPowerOf2(pow(2.0L, 96)));
    assert( isPowerOf2(pow(2.0L, -77)));

    assert(!isPowerOf2(-2.0L));
    assert(!isPowerOf2(-0.5L));
    assert(!isPowerOf2(0.0L));
    assert(!isPowerOf2(4.315));
    assert(!isPowerOf2(1.0L / 3.0L));

    assert(!isPowerOf2(real.nan));
    assert(!isPowerOf2(real.infinity));
}
///
@safe unittest
{
    assert( isPowerOf2(1));
    assert( isPowerOf2(2));
    assert( isPowerOf2(1uL << 63));

    assert(!isPowerOf2(-4));
    assert(!isPowerOf2(0));
    assert(!isPowerOf2(1337u));
}

@safe unittest
{
    import std.math.exponential : pow;
    import std.meta : AliasSeq;

    enum smallP2 = pow(2.0L, -62);
    enum bigP2 = pow(2.0L, 50);
    enum smallP7 = pow(7.0L, -35);
    enum bigP7 = pow(7.0L, 30);

    static foreach (X; AliasSeq!(float, double, real))
    {{
        immutable min_sub = X.min_normal * X.epsilon;

        foreach (x; [smallP2, min_sub, X.min_normal, .25L, 0.5L, 1.0L,
                              2.0L, 8.0L, pow(2.0L, X.max_exp - 1), bigP2])
        {
            assert( isPowerOf2(cast(X) x));
            assert(!isPowerOf2(cast(X)-x));
        }

        foreach (x; [0.0L, 3 * min_sub, smallP7, 0.1L, 1337.0L, bigP7, X.max, real.nan, real.infinity])
        {
            assert(!isPowerOf2(cast(X) x));
            assert(!isPowerOf2(cast(X)-x));
        }
    }}

    static foreach (X; AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong))
    {{
        foreach (x; [1, 2, 4, 8, (X.max >>> 1) + 1])
        {
            assert( isPowerOf2(cast(X) x));
            static if (isSigned!X)
                assert(!isPowerOf2(cast(X)-x));
        }

        foreach (x; [0, 3, 5, 13, 77, X.min, X.max])
            assert(!isPowerOf2(cast(X) x));
    }}

    // CTFE
    static foreach (X; AliasSeq!(float, double, real))
    {{
        enum min_sub = X.min_normal * X.epsilon;

        static foreach (x; [smallP2, min_sub, X.min_normal, .25L, 0.5L, 1.0L,
                              2.0L, 8.0L, pow(2.0L, X.max_exp - 1), bigP2])
        {
            static assert( isPowerOf2(cast(X) x));
            static assert(!isPowerOf2(cast(X)-x));
        }

        static foreach (x; [0.0L, 3 * min_sub, smallP7, 0.1L, 1337.0L, bigP7, X.max, real.nan, real.infinity])
        {
            static assert(!isPowerOf2(cast(X) x));
            static assert(!isPowerOf2(cast(X)-x));
        }
    }}

    static foreach (X; AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong))
    {{
        static foreach (x; [1, 2, 4, 8, (X.max >>> 1) + 1])
        {
            static assert( isPowerOf2(cast(X) x));
            static if (isSigned!X)
                static assert(!isPowerOf2(cast(X)-x));
        }

        static foreach (x; [0, 3, 5, 13, 77, X.min, X.max])
            static assert(!isPowerOf2(cast(X) x));
    }}
}

// Underlying format exposed through floatTraits
enum RealFormat
{
    ieeeHalf,
    ieeeSingle,
    ieeeDouble,
    ieeeExtended,   // x87 80-bit real
    ieeeExtended53, // x87 real rounded to precision of double.
    ibmExtended,    // IBM 128-bit extended
    ieeeQuadruple,
}

// Constants used for extracting the components of the representation.
// They supplement the built-in floating point properties.
template floatTraits(T)
{
    import std.traits : Unqual;

    // EXPMASK is a ushort mask to select the exponent portion (without sign)
    // EXPSHIFT is the number of bits the exponent is left-shifted by in its ushort
    // EXPBIAS is the exponent bias - 1 (exp == EXPBIAS yields ×2^-1).
    // EXPPOS_SHORT is the index of the exponent when represented as a ushort array.
    // SIGNPOS_BYTE is the index of the sign when represented as a ubyte array.
    // RECIP_EPSILON is the value such that (smallest_subnormal) * RECIP_EPSILON == T.min_normal
    enum Unqual!T RECIP_EPSILON = (1/T.epsilon);
    static if (T.mant_dig == 24)
    {
        // Single precision float
        enum ushort EXPMASK = 0x7F80;
        enum ushort EXPSHIFT = 7;
        enum ushort EXPBIAS = 0x3F00;
        enum uint EXPMASK_INT = 0x7F80_0000;
        enum uint MANTISSAMASK_INT = 0x007F_FFFF;
        enum realFormat = RealFormat.ieeeSingle;
        version (LittleEndian)
        {
            enum EXPPOS_SHORT = 1;
            enum SIGNPOS_BYTE = 3;
        }
        else
        {
            enum EXPPOS_SHORT = 0;
            enum SIGNPOS_BYTE = 0;
        }
    }
    else static if (T.mant_dig == 53)
    {
        static if (T.sizeof == 8)
        {
            // Double precision float, or real == double
            enum ushort EXPMASK = 0x7FF0;
            enum ushort EXPSHIFT = 4;
            enum ushort EXPBIAS = 0x3FE0;
            enum uint EXPMASK_INT = 0x7FF0_0000;
            enum uint MANTISSAMASK_INT = 0x000F_FFFF; // for the MSB only
            enum ulong MANTISSAMASK_LONG = 0x000F_FFFF_FFFF_FFFF;
            enum realFormat = RealFormat.ieeeDouble;
            version (LittleEndian)
            {
                enum EXPPOS_SHORT = 3;
                enum SIGNPOS_BYTE = 7;
            }
            else
            {
                enum EXPPOS_SHORT = 0;
                enum SIGNPOS_BYTE = 0;
            }
        }
        else static if (T.sizeof == 12)
        {
            // Intel extended real80 rounded to double
            enum ushort EXPMASK = 0x7FFF;
            enum ushort EXPSHIFT = 0;
            enum ushort EXPBIAS = 0x3FFE;
            enum realFormat = RealFormat.ieeeExtended53;
            version (LittleEndian)
            {
                enum EXPPOS_SHORT = 4;
                enum SIGNPOS_BYTE = 9;
            }
            else
            {
                enum EXPPOS_SHORT = 0;
                enum SIGNPOS_BYTE = 0;
            }
        }
        else
            static assert(false, "No traits support for " ~ T.stringof);
    }
    else static if (T.mant_dig == 64)
    {
        // Intel extended real80
        enum ushort EXPMASK = 0x7FFF;
        enum ushort EXPSHIFT = 0;
        enum ushort EXPBIAS = 0x3FFE;
        enum realFormat = RealFormat.ieeeExtended;
        version (LittleEndian)
        {
            enum EXPPOS_SHORT = 4;
            enum SIGNPOS_BYTE = 9;
        }
        else
        {
            enum EXPPOS_SHORT = 0;
            enum SIGNPOS_BYTE = 0;
        }
    }
    else static if (T.mant_dig == 113)
    {
        // Quadruple precision float
        enum ushort EXPMASK = 0x7FFF;
        enum ushort EXPSHIFT = 0;
        enum ushort EXPBIAS = 0x3FFE;
        enum realFormat = RealFormat.ieeeQuadruple;
        version (LittleEndian)
        {
            enum EXPPOS_SHORT = 7;
            enum SIGNPOS_BYTE = 15;
        }
        else
        {
            enum EXPPOS_SHORT = 0;
            enum SIGNPOS_BYTE = 0;
        }
    }
    else static if (T.mant_dig == 106)
    {
        // IBM Extended doubledouble
        enum ushort EXPMASK = 0x7FF0;
        enum ushort EXPSHIFT = 4;
        enum realFormat = RealFormat.ibmExtended;

        // For IBM doubledouble the larger magnitude double comes first.
        // It's really a double[2] and arrays don't index differently
        // between little and big-endian targets.
        enum DOUBLEPAIR_MSB = 0;
        enum DOUBLEPAIR_LSB = 1;

        // The exponent/sign byte is for most significant part.
        version (LittleEndian)
        {
            enum EXPPOS_SHORT = 3;
            enum SIGNPOS_BYTE = 7;
        }
        else
        {
            enum EXPPOS_SHORT = 0;
            enum SIGNPOS_BYTE = 0;
        }
    }
    else
        static assert(false, "No traits support for " ~ T.stringof);
}

// These apply to all floating-point types
version (LittleEndian)
{
    enum MANTISSA_LSB = 0;
    enum MANTISSA_MSB = 1;
}
else
{
    enum MANTISSA_LSB = 1;
    enum MANTISSA_MSB = 0;
}
