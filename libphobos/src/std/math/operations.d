// Written in the D programming language.

/**
This is a submodule of $(MREF std, math).

It contains several functions for work with floating point numbers.

Copyright: Copyright The D Language Foundation 2000 - 2011.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright), Don Clugston,
           Conversion of CEPHES math library to D by Iain Buclaw and David Nadlinger
Source: $(PHOBOSSRC std/math/operations.d)

Macros:
    TABLE_SV = <table border="1" cellpadding="4" cellspacing="0">
               <caption>Special Values</caption>
               $0</table>
    SVH = $(TR $(TH $1) $(TH $2))
    SV  = $(TR $(TD $1) $(TD $2))
    NAN = $(RED NAN)
    PLUSMN = &plusmn;
    INFIN = &infin;
    LT = &lt;
    GT = &gt;
 */

module std.math.operations;

import std.traits : CommonType, isFloatingPoint, isIntegral, Unqual;

// Functions for NaN payloads
/*
 * A 'payload' can be stored in the significand of a $(NAN). One bit is required
 * to distinguish between a quiet and a signalling $(NAN). This leaves 22 bits
 * of payload for a float; 51 bits for a double; 62 bits for an 80-bit real;
 * and 111 bits for a 128-bit quad.
*/
/**
 * Create a quiet $(NAN), storing an integer inside the payload.
 *
 * For floats, the largest possible payload is 0x3F_FFFF.
 * For doubles, it is 0x3_FFFF_FFFF_FFFF.
 * For 80-bit or 128-bit reals, it is 0x3FFF_FFFF_FFFF_FFFF.
 */
real NaN(ulong payload) @trusted pure nothrow @nogc
{
    import std.math.traits : floatTraits, RealFormat;

    alias F = floatTraits!(real);
    static if (F.realFormat == RealFormat.ieeeExtended ||
               F.realFormat == RealFormat.ieeeExtended53)
    {
        // real80 (in x86 real format, the implied bit is actually
        // not implied but a real bit which is stored in the real)
        ulong v = 3; // implied bit = 1, quiet bit = 1
    }
    else
    {
        ulong v = 1; // no implied bit. quiet bit = 1
    }
    if (__ctfe)
    {
        v = 1; // We use a double in CTFE.
        assert(payload >>> 51 == 0,
            "Cannot set more than 51 bits of NaN payload in CTFE.");
    }


    ulong a = payload;

    // 22 Float bits
    ulong w = a & 0x3F_FFFF;
    a -= w;

    v <<=22;
    v |= w;
    a >>=22;

    // 29 Double bits
    v <<=29;
    w = a & 0xFFF_FFFF;
    v |= w;
    a -= w;
    a >>=29;

    if (__ctfe)
    {
        v |= 0x7FF0_0000_0000_0000;
        return *cast(double*) &v;
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        v |= 0x7FF0_0000_0000_0000;
        real x;
        * cast(ulong *)(&x) = v;
        return x;
    }
    else
    {
        v <<=11;
        a &= 0x7FF;
        v |= a;
        real x = real.nan;

        // Extended real bits
        static if (F.realFormat == RealFormat.ieeeQuadruple)
        {
            v <<= 1; // there's no implicit bit

            version (LittleEndian)
            {
                *cast(ulong*)(6+cast(ubyte*)(&x)) = v;
            }
            else
            {
                *cast(ulong*)(2+cast(ubyte*)(&x)) = v;
            }
        }
        else
        {
            *cast(ulong *)(&x) = v;
        }
        return x;
    }
}

///
@safe @nogc pure nothrow unittest
{
    import std.math.traits : isNaN;

    real a = NaN(1_000_000);
    assert(isNaN(a));
    assert(getNaNPayload(a) == 1_000_000);
}

@system pure nothrow @nogc unittest // not @safe because taking address of local.
{
    import std.math.traits : floatTraits, RealFormat;

    static if (floatTraits!(real).realFormat == RealFormat.ieeeDouble)
    {
        auto x = NaN(1);
        auto xl = *cast(ulong*)&x;
        assert(xl & 0x8_0000_0000_0000UL); //non-signaling bit, bit 52
        assert((xl & 0x7FF0_0000_0000_0000UL) == 0x7FF0_0000_0000_0000UL); //all exp bits set
    }
}

/**
 * Extract an integral payload from a $(NAN).
 *
 * Returns:
 * the integer payload as a ulong.
 *
 * For floats, the largest possible payload is 0x3F_FFFF.
 * For doubles, it is 0x3_FFFF_FFFF_FFFF.
 * For 80-bit or 128-bit reals, it is 0x3FFF_FFFF_FFFF_FFFF.
 */
ulong getNaNPayload(real x) @trusted pure nothrow @nogc
{
    import std.math.traits : floatTraits, RealFormat;

    //  assert(isNaN(x));
    alias F = floatTraits!(real);
    ulong m = void;
    if (__ctfe)
    {
        double y = x;
        m = *cast(ulong*) &y;
        // Make it look like an 80-bit significand.
        // Skip exponent, and quiet bit
        m &= 0x0007_FFFF_FFFF_FFFF;
        m <<= 11;
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        m = *cast(ulong*)(&x);
        // Make it look like an 80-bit significand.
        // Skip exponent, and quiet bit
        m &= 0x0007_FFFF_FFFF_FFFF;
        m <<= 11;
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple)
    {
        version (LittleEndian)
        {
            m = *cast(ulong*)(6+cast(ubyte*)(&x));
        }
        else
        {
            m = *cast(ulong*)(2+cast(ubyte*)(&x));
        }

        m >>= 1; // there's no implicit bit
    }
    else
    {
        m = *cast(ulong*)(&x);
    }

    // ignore implicit bit and quiet bit

    const ulong f = m & 0x3FFF_FF00_0000_0000L;

    ulong w = f >>> 40;
            w |= (m & 0x00FF_FFFF_F800L) << (22 - 11);
            w |= (m & 0x7FF) << 51;
            return w;
}

///
@safe @nogc pure nothrow unittest
{
    import std.math.traits : isNaN;

    real a = NaN(1_000_000);
    assert(isNaN(a));
    assert(getNaNPayload(a) == 1_000_000);
}

@safe @nogc pure nothrow unittest
{
    import std.math.traits : isIdentical, isNaN;

    enum real a = NaN(1_000_000);
    static assert(isNaN(a));
    static assert(getNaNPayload(a) == 1_000_000);
    real b = NaN(1_000_000);
    assert(isIdentical(b, a));
    // The CTFE version of getNaNPayload relies on it being impossible
    // for a CTFE-constructed NaN to have more than 51 bits of payload.
    enum nanNaN = NaN(getNaNPayload(real.nan));
    assert(isIdentical(real.nan, nanNaN));
    static if (real.init != real.init)
    {
        enum initNaN = NaN(getNaNPayload(real.init));
        assert(isIdentical(real.init, initNaN));
    }
}

debug(UnitTest)
{
    @safe pure nothrow @nogc unittest
    {
        real nan4 = NaN(0x789_ABCD_EF12_3456);
        static if (floatTraits!(real).realFormat == RealFormat.ieeeExtended
                || floatTraits!(real).realFormat == RealFormat.ieeeQuadruple)
        {
            assert(getNaNPayload(nan4) == 0x789_ABCD_EF12_3456);
        }
        else
        {
            assert(getNaNPayload(nan4) == 0x1_ABCD_EF12_3456);
        }
        double nan5 = nan4;
        assert(getNaNPayload(nan5) == 0x1_ABCD_EF12_3456);
        float nan6 = nan4;
        assert(getNaNPayload(nan6) == 0x12_3456);
        nan4 = NaN(0xFABCD);
        assert(getNaNPayload(nan4) == 0xFABCD);
        nan6 = nan4;
        assert(getNaNPayload(nan6) == 0xFABCD);
        nan5 = NaN(0x100_0000_0000_3456);
        assert(getNaNPayload(nan5) == 0x0000_0000_3456);
    }
}

/**
 * Calculate the next largest floating point value after x.
 *
 * Return the least number greater than x that is representable as a real;
 * thus, it gives the next point on the IEEE number line.
 *
 *  $(TABLE_SV
 *    $(SVH x,            nextUp(x)   )
 *    $(SV  -$(INFIN),    -real.max   )
 *    $(SV  $(PLUSMN)0.0, real.min_normal*real.epsilon )
 *    $(SV  real.max,     $(INFIN) )
 *    $(SV  $(INFIN),     $(INFIN) )
 *    $(SV  $(NAN),       $(NAN)   )
 * )
 */
real nextUp(real x) @trusted pure nothrow @nogc
{
    import std.math.traits : floatTraits, RealFormat, MANTISSA_MSB, MANTISSA_LSB;

    alias F = floatTraits!(real);
    static if (F.realFormat != RealFormat.ieeeDouble)
    {
        if (__ctfe)
        {
            if (x == -real.infinity)
                return -real.max;
            if (!(x < real.infinity)) // Infinity or NaN.
                return x;
            real delta;
            // Start with a decent estimate of delta.
            if (x <= 0x1.ffffffffffffep+1023 && x >= -double.max)
            {
                const double d = cast(double) x;
                delta = (cast(real) nextUp(d) - cast(real) d) * 0x1p-11L;
                while (x + (delta * 0x1p-100L) > x)
                    delta *= 0x1p-100L;
            }
            else
            {
                delta = 0x1p960L;
                while (!(x + delta > x) && delta < real.max * 0x1p-100L)
                    delta *= 0x1p100L;
            }
            if (x + delta > x)
            {
                while (x + (delta / 2) > x)
                    delta /= 2;
            }
            else
            {
                do { delta += delta; } while (!(x + delta > x));
            }
            if (x < 0 && x + delta == 0)
                return -0.0L;
            return x + delta;
        }
    }
    static if (F.realFormat == RealFormat.ieeeDouble)
    {
        return nextUp(cast(double) x);
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple)
    {
        ushort e = F.EXPMASK & (cast(ushort *)&x)[F.EXPPOS_SHORT];
        if (e == F.EXPMASK)
        {
            // NaN or Infinity
            if (x == -real.infinity) return -real.max;
            return x; // +Inf and NaN are unchanged.
        }

        auto ps = cast(ulong *)&x;
        if (ps[MANTISSA_MSB] & 0x8000_0000_0000_0000)
        {
            // Negative number
            if (ps[MANTISSA_LSB] == 0 && ps[MANTISSA_MSB] == 0x8000_0000_0000_0000)
            {
                // it was negative zero, change to smallest subnormal
                ps[MANTISSA_LSB] = 1;
                ps[MANTISSA_MSB] = 0;
                return x;
            }
            if (ps[MANTISSA_LSB] == 0) --ps[MANTISSA_MSB];
            --ps[MANTISSA_LSB];
        }
        else
        {
            // Positive number
            ++ps[MANTISSA_LSB];
            if (ps[MANTISSA_LSB] == 0) ++ps[MANTISSA_MSB];
        }
        return x;
    }
    else static if (F.realFormat == RealFormat.ieeeExtended ||
                    F.realFormat == RealFormat.ieeeExtended53)
    {
        // For 80-bit reals, the "implied bit" is a nuisance...
        ushort *pe = cast(ushort *)&x;
        ulong  *ps = cast(ulong  *)&x;
        // EPSILON is 1 for 64-bit, and 2048 for 53-bit precision reals.
        enum ulong EPSILON = 2UL ^^ (64 - real.mant_dig);

        if ((pe[F.EXPPOS_SHORT] & F.EXPMASK) == F.EXPMASK)
        {
            // First, deal with NANs and infinity
            if (x == -real.infinity) return -real.max;
            return x; // +Inf and NaN are unchanged.
        }
        if (pe[F.EXPPOS_SHORT] & 0x8000)
        {
            // Negative number -- need to decrease the significand
            *ps -= EPSILON;
            // Need to mask with 0x7FFF... so subnormals are treated correctly.
            if ((*ps & 0x7FFF_FFFF_FFFF_FFFF) == 0x7FFF_FFFF_FFFF_FFFF)
            {
                if (pe[F.EXPPOS_SHORT] == 0x8000)   // it was negative zero
                {
                    *ps = 1;
                    pe[F.EXPPOS_SHORT] = 0; // smallest subnormal.
                    return x;
                }

                --pe[F.EXPPOS_SHORT];

                if (pe[F.EXPPOS_SHORT] == 0x8000)
                    return x; // it's become a subnormal, implied bit stays low.

                *ps = 0xFFFF_FFFF_FFFF_FFFF; // set the implied bit
                return x;
            }
            return x;
        }
        else
        {
            // Positive number -- need to increase the significand.
            // Works automatically for positive zero.
            *ps += EPSILON;
            if ((*ps & 0x7FFF_FFFF_FFFF_FFFF) == 0)
            {
                // change in exponent
                ++pe[F.EXPPOS_SHORT];
                *ps = 0x8000_0000_0000_0000; // set the high bit
            }
        }
        return x;
    }
    else // static if (F.realFormat == RealFormat.ibmExtended)
    {
        assert(0, "nextUp not implemented");
    }
}

/** ditto */
double nextUp(double x) @trusted pure nothrow @nogc
{
    ulong s = *cast(ulong *)&x;

    if ((s & 0x7FF0_0000_0000_0000) == 0x7FF0_0000_0000_0000)
    {
        // First, deal with NANs and infinity
        if (x == -x.infinity) return -x.max;
        return x; // +INF and NAN are unchanged.
    }
    if (s & 0x8000_0000_0000_0000)    // Negative number
    {
        if (s == 0x8000_0000_0000_0000) // it was negative zero
        {
            s = 0x0000_0000_0000_0001; // change to smallest subnormal
            return *cast(double*) &s;
        }
        --s;
    }
    else
    {   // Positive number
        ++s;
    }
    return *cast(double*) &s;
}

/** ditto */
float nextUp(float x) @trusted pure nothrow @nogc
{
    uint s = *cast(uint *)&x;

    if ((s & 0x7F80_0000) == 0x7F80_0000)
    {
        // First, deal with NANs and infinity
        if (x == -x.infinity) return -x.max;

        return x; // +INF and NAN are unchanged.
    }
    if (s & 0x8000_0000)   // Negative number
    {
        if (s == 0x8000_0000) // it was negative zero
        {
            s = 0x0000_0001; // change to smallest subnormal
            return *cast(float*) &s;
        }

        --s;
    }
    else
    {
        // Positive number
        ++s;
    }
    return *cast(float*) &s;
}

///
@safe @nogc pure nothrow unittest
{
    assert(nextUp(1.0 - 1.0e-6).feqrel(0.999999) > 16);
    assert(nextUp(1.0 - real.epsilon).feqrel(1.0) > 16);
}

/**
 * Calculate the next smallest floating point value before x.
 *
 * Return the greatest number less than x that is representable as a real;
 * thus, it gives the previous point on the IEEE number line.
 *
 *  $(TABLE_SV
 *    $(SVH x,            nextDown(x)   )
 *    $(SV  $(INFIN),     real.max  )
 *    $(SV  $(PLUSMN)0.0, -real.min_normal*real.epsilon )
 *    $(SV  -real.max,    -$(INFIN) )
 *    $(SV  -$(INFIN),    -$(INFIN) )
 *    $(SV  $(NAN),       $(NAN)    )
 * )
 */
real nextDown(real x) @safe pure nothrow @nogc
{
    return -nextUp(-x);
}

/** ditto */
double nextDown(double x) @safe pure nothrow @nogc
{
    return -nextUp(-x);
}

/** ditto */
float nextDown(float x) @safe pure nothrow @nogc
{
    return -nextUp(-x);
}

///
@safe pure nothrow @nogc unittest
{
    assert( nextDown(1.0 + real.epsilon) == 1.0);
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : floatTraits, RealFormat, isIdentical;

    static if (floatTraits!(real).realFormat == RealFormat.ieeeExtended ||
               floatTraits!(real).realFormat == RealFormat.ieeeDouble ||
               floatTraits!(real).realFormat == RealFormat.ieeeExtended53 ||
               floatTraits!(real).realFormat == RealFormat.ieeeQuadruple)
    {
        // Tests for reals
        assert(isIdentical(nextUp(NaN(0xABC)), NaN(0xABC)));
        //static assert(isIdentical(nextUp(NaN(0xABC)), NaN(0xABC)));
        // negative numbers
        assert( nextUp(-real.infinity) == -real.max );
        assert( nextUp(-1.0L-real.epsilon) == -1.0 );
        assert( nextUp(-2.0L) == -2.0 + real.epsilon);
        static assert( nextUp(-real.infinity) == -real.max );
        static assert( nextUp(-1.0L-real.epsilon) == -1.0 );
        static assert( nextUp(-2.0L) == -2.0 + real.epsilon);
        // subnormals and zero
        assert( nextUp(-real.min_normal) == -real.min_normal*(1-real.epsilon) );
        assert( nextUp(-real.min_normal*(1-real.epsilon)) == -real.min_normal*(1-2*real.epsilon) );
        assert( isIdentical(-0.0L, nextUp(-real.min_normal*real.epsilon)) );
        assert( nextUp(-0.0L) == real.min_normal*real.epsilon );
        assert( nextUp(0.0L) == real.min_normal*real.epsilon );
        assert( nextUp(real.min_normal*(1-real.epsilon)) == real.min_normal );
        assert( nextUp(real.min_normal) == real.min_normal*(1+real.epsilon) );
        static assert( nextUp(-real.min_normal) == -real.min_normal*(1-real.epsilon) );
        static assert( nextUp(-real.min_normal*(1-real.epsilon)) == -real.min_normal*(1-2*real.epsilon) );
        static assert( -0.0L is nextUp(-real.min_normal*real.epsilon) );
        static assert( nextUp(-0.0L) == real.min_normal*real.epsilon );
        static assert( nextUp(0.0L) == real.min_normal*real.epsilon );
        static assert( nextUp(real.min_normal*(1-real.epsilon)) == real.min_normal );
        static assert( nextUp(real.min_normal) == real.min_normal*(1+real.epsilon) );
        // positive numbers
        assert( nextUp(1.0L) == 1.0 + real.epsilon );
        assert( nextUp(2.0L-real.epsilon) == 2.0 );
        assert( nextUp(real.max) == real.infinity );
        assert( nextUp(real.infinity)==real.infinity );
        static assert( nextUp(1.0L) == 1.0 + real.epsilon );
        static assert( nextUp(2.0L-real.epsilon) == 2.0 );
        static assert( nextUp(real.max) == real.infinity );
        static assert( nextUp(real.infinity)==real.infinity );
        // ctfe near double.max boundary
        static assert(nextUp(nextDown(cast(real) double.max)) == cast(real) double.max);
    }

    double n = NaN(0xABC);
    assert(isIdentical(nextUp(n), n));
    // negative numbers
    assert( nextUp(-double.infinity) == -double.max );
    assert( nextUp(-1-double.epsilon) == -1.0 );
    assert( nextUp(-2.0) == -2.0 + double.epsilon);
    // subnormals and zero

    assert( nextUp(-double.min_normal) == -double.min_normal*(1-double.epsilon) );
    assert( nextUp(-double.min_normal*(1-double.epsilon)) == -double.min_normal*(1-2*double.epsilon) );
    assert( isIdentical(-0.0, nextUp(-double.min_normal*double.epsilon)) );
    assert( nextUp(0.0) == double.min_normal*double.epsilon );
    assert( nextUp(-0.0) == double.min_normal*double.epsilon );
    assert( nextUp(double.min_normal*(1-double.epsilon)) == double.min_normal );
    assert( nextUp(double.min_normal) == double.min_normal*(1+double.epsilon) );
    // positive numbers
    assert( nextUp(1.0) == 1.0 + double.epsilon );
    assert( nextUp(2.0-double.epsilon) == 2.0 );
    assert( nextUp(double.max) == double.infinity );

    float fn = NaN(0xABC);
    assert(isIdentical(nextUp(fn), fn));
    float f = -float.min_normal*(1-float.epsilon);
    float f1 = -float.min_normal;
    assert( nextUp(f1) ==  f);
    f = 1.0f+float.epsilon;
    f1 = 1.0f;
    assert( nextUp(f1) == f );
    f1 = -0.0f;
    assert( nextUp(f1) == float.min_normal*float.epsilon);
    assert( nextUp(float.infinity)==float.infinity );

    assert(nextDown(1.0L+real.epsilon)==1.0);
    assert(nextDown(1.0+double.epsilon)==1.0);
    f = 1.0f+float.epsilon;
    assert(nextDown(f)==1.0);
    assert(nextafter(1.0+real.epsilon, -real.infinity)==1.0);

    // CTFE

    enum double ctfe_n = NaN(0xABC);
    //static assert(isIdentical(nextUp(ctfe_n), ctfe_n)); // FIXME: https://issues.dlang.org/show_bug.cgi?id=20197
    static assert(nextUp(double.nan) is double.nan);
    // negative numbers
    static assert( nextUp(-double.infinity) == -double.max );
    static assert( nextUp(-1-double.epsilon) == -1.0 );
    static assert( nextUp(-2.0) == -2.0 + double.epsilon);
    // subnormals and zero

    static assert( nextUp(-double.min_normal) == -double.min_normal*(1-double.epsilon) );
    static assert( nextUp(-double.min_normal*(1-double.epsilon)) == -double.min_normal*(1-2*double.epsilon) );
    static assert( -0.0 is nextUp(-double.min_normal*double.epsilon) );
    static assert( nextUp(0.0) == double.min_normal*double.epsilon );
    static assert( nextUp(-0.0) == double.min_normal*double.epsilon );
    static assert( nextUp(double.min_normal*(1-double.epsilon)) == double.min_normal );
    static assert( nextUp(double.min_normal) == double.min_normal*(1+double.epsilon) );
    // positive numbers
    static assert( nextUp(1.0) == 1.0 + double.epsilon );
    static assert( nextUp(2.0-double.epsilon) == 2.0 );
    static assert( nextUp(double.max) == double.infinity );

    enum float ctfe_fn = NaN(0xABC);
    //static assert(isIdentical(nextUp(ctfe_fn), ctfe_fn)); // FIXME: https://issues.dlang.org/show_bug.cgi?id=20197
    static assert(nextUp(float.nan) is float.nan);
    static assert(nextUp(-float.min_normal) == -float.min_normal*(1-float.epsilon));
    static assert(nextUp(1.0f) == 1.0f+float.epsilon);
    static assert(nextUp(-0.0f) == float.min_normal*float.epsilon);
    static assert(nextUp(float.infinity)==float.infinity);
    static assert(nextDown(1.0L+real.epsilon)==1.0);
    static assert(nextDown(1.0+double.epsilon)==1.0);
    static assert(nextDown(1.0f+float.epsilon)==1.0);
    static assert(nextafter(1.0+real.epsilon, -real.infinity)==1.0);
}



/******************************************
 * Calculates the next representable value after x in the direction of y.
 *
 * If y > x, the result will be the next largest floating-point value;
 * if y < x, the result will be the next smallest value.
 * If x == y, the result is y.
 * If x or y is a NaN, the result is a NaN.
 *
 * Remarks:
 * This function is not generally very useful; it's almost always better to use
 * the faster functions nextUp() or nextDown() instead.
 *
 * The FE_INEXACT and FE_OVERFLOW exceptions will be raised if x is finite and
 * the function result is infinite. The FE_INEXACT and FE_UNDERFLOW
 * exceptions will be raised if the function value is subnormal, and x is
 * not equal to y.
 */
T nextafter(T)(const T x, const T y) @safe pure nothrow @nogc
{
    import std.math.traits : isNaN;

    if (x == y || isNaN(y))
    {
        return y;
    }

    if (isNaN(x))
    {
        return x;
    }

    return ((y>x) ? nextUp(x) :  nextDown(x));
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN;

    float a = 1;
    assert(is(typeof(nextafter(a, a)) == float));
    assert(nextafter(a, a.infinity) > a);
    assert(isNaN(nextafter(a, a.nan)));
    assert(isNaN(nextafter(a.nan, a)));

    double b = 2;
    assert(is(typeof(nextafter(b, b)) == double));
    assert(nextafter(b, b.infinity) > b);
    assert(isNaN(nextafter(b, b.nan)));
    assert(isNaN(nextafter(b.nan, b)));

    real c = 3;
    assert(is(typeof(nextafter(c, c)) == real));
    assert(nextafter(c, c.infinity) > c);
    assert(isNaN(nextafter(c, c.nan)));
    assert(isNaN(nextafter(c.nan, c)));
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN, signbit;

    // CTFE
    enum float a = 1;
    static assert(is(typeof(nextafter(a, a)) == float));
    static assert(nextafter(a, a.infinity) > a);
    static assert(isNaN(nextafter(a, a.nan)));
    static assert(isNaN(nextafter(a.nan, a)));

    enum double b = 2;
    static assert(is(typeof(nextafter(b, b)) == double));
    static assert(nextafter(b, b.infinity) > b);
    static assert(isNaN(nextafter(b, b.nan)));
    static assert(isNaN(nextafter(b.nan, b)));

    enum real c = 3;
    static assert(is(typeof(nextafter(c, c)) == real));
    static assert(nextafter(c, c.infinity) > c);
    static assert(isNaN(nextafter(c, c.nan)));
    static assert(isNaN(nextafter(c.nan, c)));

    enum real negZero = nextafter(+0.0L, -0.0L);
    static assert(negZero == -0.0L);
    static assert(signbit(negZero));

    static assert(nextafter(c, c) == c);
}

//real nexttoward(real x, real y) { return core.stdc.math.nexttowardl(x, y); }

/**
 * Returns the positive difference between x and y.
 *
 * Equivalent to `fmax(x-y, 0)`.
 *
 * Returns:
 *      $(TABLE_SV
 *      $(TR $(TH x, y)       $(TH fdim(x, y)))
 *      $(TR $(TD x $(GT) y)  $(TD x - y))
 *      $(TR $(TD x $(LT)= y) $(TD +0.0))
 *      )
 */
real fdim(real x, real y) @safe pure nothrow @nogc
{
    return (x < y) ? +0.0 : x - y;
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN;

    assert(fdim(2.0, 0.0) == 2.0);
    assert(fdim(-2.0, 0.0) == 0.0);
    assert(fdim(real.infinity, 2.0) == real.infinity);
    assert(isNaN(fdim(real.nan, 2.0)));
    assert(isNaN(fdim(2.0, real.nan)));
    assert(isNaN(fdim(real.nan, real.nan)));
}

/**
 * Returns the larger of `x` and `y`.
 *
 * If one of the arguments is a `NaN`, the other is returned.
 *
 * See_Also: $(REF max, std,algorithm,comparison) is faster because it does not perform the `isNaN` test.
 */
F fmax(F)(const F x, const F y) @safe pure nothrow @nogc
if (__traits(isFloating, F))
{
    import std.math.traits : isNaN;

    // Do the more predictable test first. Generates 0 branches with ldc and 1 branch with gdc.
    // See https://godbolt.org/z/erxrW9
    if (isNaN(x)) return y;
    return y > x ? y : x;
}

///
@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    static foreach (F; AliasSeq!(float, double, real))
    {
        assert(fmax(F(0.0), F(2.0)) == 2.0);
        assert(fmax(F(-2.0), 0.0) == F(0.0));
        assert(fmax(F.infinity, F(2.0)) == F.infinity);
        assert(fmax(F.nan, F(2.0)) == F(2.0));
        assert(fmax(F(2.0), F.nan) == F(2.0));
    }
}

/**
 * Returns the smaller of `x` and `y`.
 *
 * If one of the arguments is a `NaN`, the other is returned.
 *
 * See_Also: $(REF min, std,algorithm,comparison) is faster because it does not perform the `isNaN` test.
 */
F fmin(F)(const F x, const F y) @safe pure nothrow @nogc
if (__traits(isFloating, F))
{
    import std.math.traits : isNaN;

    // Do the more predictable test first. Generates 0 branches with ldc and 1 branch with gdc.
    // See https://godbolt.org/z/erxrW9
    if (isNaN(x)) return y;
    return y < x ? y : x;
}

///
@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    static foreach (F; AliasSeq!(float, double, real))
    {
        assert(fmin(F(0.0), F(2.0)) == 0.0);
        assert(fmin(F(-2.0), F(0.0)) == -2.0);
        assert(fmin(F.infinity, F(2.0)) == 2.0);
        assert(fmin(F.nan, F(2.0)) == 2.0);
        assert(fmin(F(2.0), F.nan) == 2.0);
    }
}

/**************************************
 * Returns (x * y) + z, rounding only once according to the
 * current rounding mode.
 *
 * BUGS: Not currently implemented - rounds twice.
 */
pragma(inline, true)
real fma(real x, real y, real z) @safe pure nothrow @nogc { return (x * y) + z; }

///
@safe pure nothrow @nogc unittest
{
    assert(fma(0.0, 2.0, 2.0) == 2.0);
    assert(fma(2.0, 2.0, 2.0) == 6.0);
    assert(fma(real.infinity, 2.0, 2.0) == real.infinity);
    assert(fma(real.nan, 2.0, 2.0) is real.nan);
    assert(fma(2.0, 2.0, real.nan) is real.nan);
}

/**************************************
 * To what precision is x equal to y?
 *
 * Returns: the number of mantissa bits which are equal in x and y.
 * eg, 0x1.F8p+60 and 0x1.F1p+60 are equal to 5 bits of precision.
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)      $(TH y)          $(TH feqrel(x, y)))
 *      $(TR $(TD x)      $(TD x)          $(TD real.mant_dig))
 *      $(TR $(TD x)      $(TD $(GT)= 2*x) $(TD 0))
 *      $(TR $(TD x)      $(TD $(LT)= x/2) $(TD 0))
 *      $(TR $(TD $(NAN)) $(TD any)        $(TD 0))
 *      $(TR $(TD any)    $(TD $(NAN))     $(TD 0))
 *      )
 */
int feqrel(X)(const X x, const X y) @trusted pure nothrow @nogc
if (isFloatingPoint!(X))
{
    import std.math.traits : floatTraits, RealFormat;
    import core.math : fabs;

    /* Public Domain. Author: Don Clugston, 18 Aug 2005.
     */
    alias F = floatTraits!(X);
    static if (F.realFormat == RealFormat.ieeeSingle
            || F.realFormat == RealFormat.ieeeDouble
            || F.realFormat == RealFormat.ieeeExtended
            || F.realFormat == RealFormat.ieeeExtended53
            || F.realFormat == RealFormat.ieeeQuadruple)
    {
        if (x == y)
            return X.mant_dig; // ensure diff != 0, cope with INF.

        Unqual!X diff = fabs(x - y);

        ushort *pa = cast(ushort *)(&x);
        ushort *pb = cast(ushort *)(&y);
        ushort *pd = cast(ushort *)(&diff);


        // The difference in abs(exponent) between x or y and abs(x-y)
        // is equal to the number of significand bits of x which are
        // equal to y. If negative, x and y have different exponents.
        // If positive, x and y are equal to 'bitsdiff' bits.
        // AND with 0x7FFF to form the absolute value.
        // To avoid out-by-1 errors, we subtract 1 so it rounds down
        // if the exponents were different. This means 'bitsdiff' is
        // always 1 lower than we want, except that if bitsdiff == 0,
        // they could have 0 or 1 bits in common.

        int bitsdiff = (((  (pa[F.EXPPOS_SHORT] & F.EXPMASK)
                          + (pb[F.EXPPOS_SHORT] & F.EXPMASK)
                          - (1 << F.EXPSHIFT)) >> 1)
                        - (pd[F.EXPPOS_SHORT] & F.EXPMASK)) >> F.EXPSHIFT;
        if ( (pd[F.EXPPOS_SHORT] & F.EXPMASK) == 0)
        {   // Difference is subnormal
            // For subnormals, we need to add the number of zeros that
            // lie at the start of diff's significand.
            // We do this by multiplying by 2^^real.mant_dig
            diff *= F.RECIP_EPSILON;
            return bitsdiff + X.mant_dig - ((pd[F.EXPPOS_SHORT] & F.EXPMASK) >> F.EXPSHIFT);
        }

        if (bitsdiff > 0)
            return bitsdiff + 1; // add the 1 we subtracted before

        // Avoid out-by-1 errors when factor is almost 2.
        if (bitsdiff == 0
            && ((pa[F.EXPPOS_SHORT] ^ pb[F.EXPPOS_SHORT]) & F.EXPMASK) == 0)
        {
            return 1;
        } else return 0;
    }
    else
    {
        static assert(false, "Not implemented for this architecture");
    }
}

///
@safe pure unittest
{
    assert(feqrel(2.0, 2.0) == 53);
    assert(feqrel(2.0f, 2.0f) == 24);
    assert(feqrel(2.0, double.nan) == 0);

    // Test that numbers are within n digits of each
    // other by testing if feqrel > n * log2(10)

    // five digits
    assert(feqrel(2.0, 2.00001) > 16);
    // ten digits
    assert(feqrel(2.0, 2.00000000001) > 33);
}

@safe pure nothrow @nogc unittest
{
    void testFeqrel(F)()
    {
       // Exact equality
       assert(feqrel(F.max, F.max) == F.mant_dig);
       assert(feqrel!(F)(0.0, 0.0) == F.mant_dig);
       assert(feqrel(F.infinity, F.infinity) == F.mant_dig);

       // a few bits away from exact equality
       F w=1;
       for (int i = 1; i < F.mant_dig - 1; ++i)
       {
          assert(feqrel!(F)(1.0 + w * F.epsilon, 1.0) == F.mant_dig-i);
          assert(feqrel!(F)(1.0 - w * F.epsilon, 1.0) == F.mant_dig-i);
          assert(feqrel!(F)(1.0, 1 + (w-1) * F.epsilon) == F.mant_dig - i + 1);
          w*=2;
       }

       assert(feqrel!(F)(1.5+F.epsilon, 1.5) == F.mant_dig-1);
       assert(feqrel!(F)(1.5-F.epsilon, 1.5) == F.mant_dig-1);
       assert(feqrel!(F)(1.5-F.epsilon, 1.5+F.epsilon) == F.mant_dig-2);


       // Numbers that are close
       assert(feqrel!(F)(0x1.Bp+84, 0x1.B8p+84) == 5);
       assert(feqrel!(F)(0x1.8p+10, 0x1.Cp+10) == 2);
       assert(feqrel!(F)(1.5 * (1 - F.epsilon), 1.0L) == 2);
       assert(feqrel!(F)(1.5, 1.0) == 1);
       assert(feqrel!(F)(2 * (1 - F.epsilon), 1.0L) == 1);

       // Factors of 2
       assert(feqrel(F.max, F.infinity) == 0);
       assert(feqrel!(F)(2 * (1 - F.epsilon), 1.0L) == 1);
       assert(feqrel!(F)(1.0, 2.0) == 0);
       assert(feqrel!(F)(4.0, 1.0) == 0);

       // Extreme inequality
       assert(feqrel(F.nan, F.nan) == 0);
       assert(feqrel!(F)(0.0L, -F.nan) == 0);
       assert(feqrel(F.nan, F.infinity) == 0);
       assert(feqrel(F.infinity, -F.infinity) == 0);
       assert(feqrel(F.max, -F.max) == 0);

       assert(feqrel(F.min_normal / 8, F.min_normal / 17) == 3);

       const F Const = 2;
       immutable F Immutable = 2;
       auto Compiles = feqrel(Const, Immutable);
    }

    assert(feqrel(7.1824L, 7.1824L) == real.mant_dig);

    testFeqrel!(real)();
    testFeqrel!(double)();
    testFeqrel!(float)();
}

/**
   Computes whether a values is approximately equal to a reference value,
   admitting a maximum relative difference, and a maximum absolute difference.

   Warning:
        This template is considered out-dated. It will be removed from
        Phobos in 2.106.0. Please use $(LREF isClose) instead. To achieve
        a similar behaviour to `approxEqual(a, b)` use
        `isClose(a, b, 1e-2, 1e-5)`. In case of comparing to 0.0,
        `isClose(a, b, 0.0, eps)` should be used, where `eps`
        represents the accepted deviation from 0.0."

   Params:
        value = Value to compare.
        reference = Reference value.
        maxRelDiff = Maximum allowable difference relative to `reference`.
        Setting to 0.0 disables this check. Defaults to `1e-2`.
        maxAbsDiff = Maximum absolute difference. This is mainly usefull
        for comparing values to zero. Setting to 0.0 disables this check.
        Defaults to `1e-5`.

   Returns:
       `true` if `value` is approximately equal to `reference` under
       either criterium. It is sufficient, when `value ` satisfies
       one of the two criteria.

       If one item is a range, and the other is a single value, then
       the result is the logical and-ing of calling `approxEqual` on
       each element of the ranged item against the single item. If
       both items are ranges, then `approxEqual` returns `true` if
       and only if the ranges have the same number of elements and if
       `approxEqual` evaluates to `true` for each pair of elements.

    See_Also:
        Use $(LREF feqrel) to get the number of equal bits in the mantissa.
 */
deprecated("approxEqual will be removed in 2.106.0. Please use isClose instead.")
bool approxEqual(T, U, V)(T value, U reference, V maxRelDiff = 1e-2, V maxAbsDiff = 1e-5)
{
    import core.math : fabs;
    import std.range.primitives : empty, front, isInputRange, popFront;
    static if (isInputRange!T)
    {
        static if (isInputRange!U)
        {
            // Two ranges
            for (;; value.popFront(), reference.popFront())
            {
                if (value.empty) return reference.empty;
                if (reference.empty) return value.empty;
                if (!approxEqual(value.front, reference.front, maxRelDiff, maxAbsDiff))
                    return false;
            }
        }
        else static if (isIntegral!U)
        {
            // convert reference to real
            return approxEqual(value, real(reference), maxRelDiff, maxAbsDiff);
        }
        else
        {
            // value is range, reference is number
            for (; !value.empty; value.popFront())
            {
                if (!approxEqual(value.front, reference, maxRelDiff, maxAbsDiff))
                    return false;
            }
            return true;
        }
    }
    else
    {
        static if (isInputRange!U)
        {
            // value is number, reference is range
            for (; !reference.empty; reference.popFront())
            {
                if (!approxEqual(value, reference.front, maxRelDiff, maxAbsDiff))
                    return false;
            }
            return true;
        }
        else static if (isIntegral!T || isIntegral!U)
        {
            // convert both value and reference to real
            return approxEqual(real(value), real(reference), maxRelDiff, maxAbsDiff);
        }
        else
        {
            // two numbers
            //static assert(is(T : real) && is(U : real));
            if (reference == 0)
            {
                return fabs(value) <= maxAbsDiff;
            }
            static if (is(typeof(value.infinity)) && is(typeof(reference.infinity)))
            {
                if (value == value.infinity && reference == reference.infinity ||
                    value == -value.infinity && reference == -reference.infinity) return true;
            }
            return fabs((value - reference) / reference) <= maxRelDiff
                || maxAbsDiff != 0 && fabs(value - reference) <= maxAbsDiff;
        }
    }
}

deprecated @safe pure nothrow unittest
{
    assert(approxEqual(1.0, 1.0099));
    assert(!approxEqual(1.0, 1.011));
    assert(approxEqual(0.00001, 0.0));
    assert(!approxEqual(0.00002, 0.0));

    assert(approxEqual(3.0, [3, 3.01, 2.99])); // several reference values is strange
    assert(approxEqual([3, 3.01, 2.99], 3.0)); // better

    float[] arr1 = [ 1.0, 2.0, 3.0 ];
    double[] arr2 = [ 1.001, 1.999, 3 ];
    assert(approxEqual(arr1, arr2));
}

deprecated @safe pure nothrow unittest
{
    // relative comparison depends on reference, make sure proper
    // side is used when comparing range to single value. Based on
    // https://issues.dlang.org/show_bug.cgi?id=15763
    auto a = [2e-3 - 1e-5];
    auto b = 2e-3 + 1e-5;
    assert(a[0].approxEqual(b));
    assert(!b.approxEqual(a[0]));
    assert(a.approxEqual(b));
    assert(!b.approxEqual(a));
}

deprecated @safe pure nothrow @nogc unittest
{
    assert(!approxEqual(0.0,1e-15,1e-9,0.0));
    assert(approxEqual(0.0,1e-15,1e-9,1e-9));
    assert(!approxEqual(1.0,3.0,0.0,1.0));

    assert(approxEqual(1.00000000099,1.0,1e-9,0.0));
    assert(!approxEqual(1.0000000011,1.0,1e-9,0.0));
}

deprecated @safe pure nothrow @nogc unittest
{
    // maybe unintuitive behavior
    assert(approxEqual(1000.0,1010.0));
    assert(approxEqual(9_090_000_000.0,9_000_000_000.0));
    assert(approxEqual(0.0,1e30,1.0));
    assert(approxEqual(0.00001,1e-30));
    assert(!approxEqual(-1e-30,1e-30,1e-2,0.0));
}

deprecated @safe pure nothrow @nogc unittest
{
    int a = 10;
    assert(approxEqual(10, a));

    assert(!approxEqual(3, 0));
    assert(approxEqual(3, 3));
    assert(approxEqual(3.0, 3));
    assert(approxEqual(3, 3.0));

    assert(approxEqual(0.0,0.0));
    assert(approxEqual(-0.0,0.0));
    assert(approxEqual(0.0f,0.0));
}

deprecated @safe pure nothrow @nogc unittest
{
    real num = real.infinity;
    assert(num == real.infinity);
    assert(approxEqual(num, real.infinity));
    num = -real.infinity;
    assert(num == -real.infinity);
    assert(approxEqual(num, -real.infinity));

    assert(!approxEqual(1,real.nan));
    assert(!approxEqual(real.nan,real.max));
    assert(!approxEqual(real.nan,real.nan));
}

deprecated @safe pure nothrow unittest
{
    assert(!approxEqual([1.0,2.0,3.0],[1.0,2.0]));
    assert(!approxEqual([1.0,2.0],[1.0,2.0,3.0]));

    assert(approxEqual!(real[],real[])([],[]));
    assert(approxEqual(cast(real[])[],cast(real[])[]));
}


/**
   Computes whether two values are approximately equal, admitting a maximum
   relative difference, and a maximum absolute difference.

   Params:
        lhs = First item to compare.
        rhs = Second item to compare.
        maxRelDiff = Maximum allowable relative difference.
        Setting to 0.0 disables this check. Default depends on the type of
        `lhs` and `rhs`: It is approximately half the number of decimal digits of
        precision of the smaller type.
        maxAbsDiff = Maximum absolute difference. This is mainly usefull
        for comparing values to zero. Setting to 0.0 disables this check.
        Defaults to `0.0`.

   Returns:
       `true` if the two items are approximately equal under either criterium.
       It is sufficient, when `value ` satisfies one of the two criteria.

       If one item is a range, and the other is a single value, then
       the result is the logical and-ing of calling `isClose` on
       each element of the ranged item against the single item. If
       both items are ranges, then `isClose` returns `true` if
       and only if the ranges have the same number of elements and if
       `isClose` evaluates to `true` for each pair of elements.

    See_Also:
        Use $(LREF feqrel) to get the number of equal bits in the mantissa.
 */
bool isClose(T, U, V = CommonType!(FloatingPointBaseType!T,FloatingPointBaseType!U))
    (T lhs, U rhs, V maxRelDiff = CommonDefaultFor!(T,U), V maxAbsDiff = 0.0)
{
    import std.range.primitives : empty, front, isInputRange, popFront;
    import std.complex : Complex;
    static if (isInputRange!T)
    {
        static if (isInputRange!U)
        {
            // Two ranges
            for (;; lhs.popFront(), rhs.popFront())
            {
                if (lhs.empty) return rhs.empty;
                if (rhs.empty) return lhs.empty;
                if (!isClose(lhs.front, rhs.front, maxRelDiff, maxAbsDiff))
                    return false;
            }
        }
        else
        {
            // lhs is range, rhs is number
            for (; !lhs.empty; lhs.popFront())
            {
                if (!isClose(lhs.front, rhs, maxRelDiff, maxAbsDiff))
                    return false;
            }
            return true;
        }
    }
    else static if (isInputRange!U)
    {
        // lhs is number, rhs is range
        for (; !rhs.empty; rhs.popFront())
        {
            if (!isClose(lhs, rhs.front, maxRelDiff, maxAbsDiff))
                return false;
        }
        return true;
    }
    else static if (is(T TE == Complex!TE))
    {
        static if (is(U UE == Complex!UE))
        {
            // Two complex numbers
            return isClose(lhs.re, rhs.re, maxRelDiff, maxAbsDiff)
                && isClose(lhs.im, rhs.im, maxRelDiff, maxAbsDiff);
        }
        else
        {
            // lhs is complex, rhs is number
            return isClose(lhs.re, rhs, maxRelDiff, maxAbsDiff)
                && isClose(lhs.im, 0.0, maxRelDiff, maxAbsDiff);
        }
    }
    else static if (is(U UE == Complex!UE))
    {
        // lhs is number, rhs is complex
        return isClose(lhs, rhs.re, maxRelDiff, maxAbsDiff)
            && isClose(0.0, rhs.im, maxRelDiff, maxAbsDiff);
    }
    else
    {
        // two numbers
        if (lhs == rhs) return true;

        static if (is(typeof(lhs.infinity)))
            if (lhs == lhs.infinity || lhs == -lhs.infinity)
                 return false;
        static if (is(typeof(rhs.infinity)))
            if (rhs == rhs.infinity || rhs == -rhs.infinity)
                return false;

        import std.math.algebraic : abs;

        auto diff = abs(lhs - rhs);

        return diff <= maxRelDiff*abs(lhs)
            || diff <= maxRelDiff*abs(rhs)
            || diff <= maxAbsDiff;
    }
}

///
@safe pure nothrow @nogc unittest
{
    assert(isClose(1.0,0.999_999_999));
    assert(isClose(0.001, 0.000_999_999_999));
    assert(isClose(1_000_000_000.0,999_999_999.0));

    assert(isClose(17.123_456_789, 17.123_456_78));
    assert(!isClose(17.123_456_789, 17.123_45));

    // use explicit 3rd parameter for less (or more) accuracy
    assert(isClose(17.123_456_789, 17.123_45, 1e-6));
    assert(!isClose(17.123_456_789, 17.123_45, 1e-7));

    // use 4th parameter when comparing close to zero
    assert(!isClose(1e-100, 0.0));
    assert(isClose(1e-100, 0.0, 0.0, 1e-90));
    assert(!isClose(1e-10, -1e-10));
    assert(isClose(1e-10, -1e-10, 0.0, 1e-9));
    assert(!isClose(1e-300, 1e-298));
    assert(isClose(1e-300, 1e-298, 0.0, 1e-200));

    // different default limits for different floating point types
    assert(isClose(1.0f, 0.999_99f));
    assert(!isClose(1.0, 0.999_99));
    static if (real.sizeof > double.sizeof)
        assert(!isClose(1.0L, 0.999_999_999L));
}

///
@safe pure nothrow unittest
{
    assert(isClose([1.0, 2.0, 3.0], [0.999_999_999, 2.000_000_001, 3.0]));
    assert(!isClose([1.0, 2.0], [0.999_999_999, 2.000_000_001, 3.0]));
    assert(!isClose([1.0, 2.0, 3.0], [0.999_999_999, 2.000_000_001]));

    assert(isClose([2.0, 1.999_999_999, 2.000_000_001], 2.0));
    assert(isClose(2.0, [2.0, 1.999_999_999, 2.000_000_001]));
}

@safe pure nothrow unittest
{
    assert(!isClose([1.0, 2.0, 3.0], [0.999_999_999, 3.0, 3.0]));
    assert(!isClose([2.0, 1.999_999, 2.000_000_001], 2.0));
    assert(!isClose(2.0, [2.0, 1.999_999_999, 2.000_000_999]));
}

@safe pure nothrow @nogc unittest
{
    immutable a = 1.00001f;
    const b = 1.000019;
    assert(isClose(a,b));

    assert(isClose(1.00001f,1.000019f));
    assert(isClose(1.00001f,1.000019));
    assert(isClose(1.00001,1.000019f));
    assert(!isClose(1.00001,1.000019));

    real a1 = 1e-300L;
    real a2 = a1.nextUp;
    assert(isClose(a1,a2));
}

@safe pure nothrow unittest
{
    float[] arr1 = [ 1.0, 2.0, 3.0 ];
    double[] arr2 = [ 1.00001, 1.99999, 3 ];
    assert(isClose(arr1, arr2));
}

@safe pure nothrow @nogc unittest
{
    assert(!isClose(1000.0,1010.0));
    assert(!isClose(9_090_000_000.0,9_000_000_000.0));
    assert(isClose(0.0,1e30,1.0));
    assert(!isClose(0.00001,1e-30));
    assert(!isClose(-1e-30,1e-30,1e-2,0.0));
}

@safe pure nothrow @nogc unittest
{
    assert(!isClose(3, 0));
    assert(isClose(3, 3));
    assert(isClose(3.0, 3));
    assert(isClose(3, 3.0));

    assert(isClose(0.0,0.0));
    assert(isClose(-0.0,0.0));
    assert(isClose(0.0f,0.0));
}

@safe pure nothrow @nogc unittest
{
    real num = real.infinity;
    assert(num == real.infinity);
    assert(isClose(num, real.infinity));
    num = -real.infinity;
    assert(num == -real.infinity);
    assert(isClose(num, -real.infinity));

    assert(!isClose(1,real.nan));
    assert(!isClose(real.nan,real.max));
    assert(!isClose(real.nan,real.nan));

    assert(!isClose(-double.infinity, 1));
}

@safe pure nothrow @nogc unittest
{
    assert(isClose!(real[],real[],real)([],[]));
    assert(isClose(cast(real[])[],cast(real[])[]));
}

@safe pure nothrow @nogc unittest
{
    import std.conv : to;

    float f = 31.79f;
    double d = 31.79;
    double f2d = f.to!double;

    assert(isClose(f,f2d));
    assert(!isClose(d,f2d));
}

@safe pure nothrow @nogc unittest
{
    import std.conv : to;

    double d = 31.79;
    float f = d.to!float;
    double f2d = f.to!double;

    assert(isClose(f,f2d));
    assert(!isClose(d,f2d));
    assert(isClose(d,f2d,1e-4));
}

package(std.math) template CommonDefaultFor(T,U)
{
    import std.algorithm.comparison : min;

    alias baseT = FloatingPointBaseType!T;
    alias baseU = FloatingPointBaseType!U;

    enum CommonType!(baseT, baseU) CommonDefaultFor = 10.0L ^^ -((min(baseT.dig, baseU.dig) + 1) / 2 + 1);
}

private template FloatingPointBaseType(T)
{
    import std.range.primitives : ElementType;
    static if (isFloatingPoint!T)
    {
        alias FloatingPointBaseType = Unqual!T;
    }
    else static if (isFloatingPoint!(ElementType!(Unqual!T)))
    {
        alias FloatingPointBaseType = Unqual!(ElementType!(Unqual!T));
    }
    else
    {
        alias FloatingPointBaseType = real;
    }
}

/***********************************
 * Defines a total order on all floating-point numbers.
 *
 * The order is defined as follows:
 * $(UL
 *      $(LI All numbers in [-$(INFIN), +$(INFIN)] are ordered
 *          the same way as by built-in comparison, with the exception of
 *          -0.0, which is less than +0.0;)
 *      $(LI If the sign bit is set (that is, it's 'negative'), $(NAN) is less
 *          than any number; if the sign bit is not set (it is 'positive'),
 *          $(NAN) is greater than any number;)
 *      $(LI $(NAN)s of the same sign are ordered by the payload ('negative'
 *          ones - in reverse order).)
 * )
 *
 * Returns:
 *      negative value if `x` precedes `y` in the order specified above;
 *      0 if `x` and `y` are identical, and positive value otherwise.
 *
 * See_Also:
 *      $(MYREF isIdentical)
 * Standards: Conforms to IEEE 754-2008
 */
int cmp(T)(const(T) x, const(T) y) @nogc @trusted pure nothrow
if (isFloatingPoint!T)
{
    import std.math.traits : floatTraits, RealFormat;

    alias F = floatTraits!T;

    static if (F.realFormat == RealFormat.ieeeSingle
               || F.realFormat == RealFormat.ieeeDouble)
    {
        static if (T.sizeof == 4)
            alias UInt = uint;
        else
            alias UInt = ulong;

        union Repainter
        {
            T number;
            UInt bits;
        }

        enum msb = ~(UInt.max >>> 1);

        import std.typecons : Tuple;
        Tuple!(Repainter, Repainter) vars = void;
        vars[0].number = x;
        vars[1].number = y;

        foreach (ref var; vars)
            if (var.bits & msb)
                var.bits = ~var.bits;
            else
                var.bits |= msb;

        if (vars[0].bits < vars[1].bits)
            return -1;
        else if (vars[0].bits > vars[1].bits)
            return 1;
        else
            return 0;
    }
    else static if (F.realFormat == RealFormat.ieeeExtended53
                    || F.realFormat == RealFormat.ieeeExtended
                    || F.realFormat == RealFormat.ieeeQuadruple)
    {
        static if (F.realFormat == RealFormat.ieeeQuadruple)
            alias RemT = ulong;
        else
            alias RemT = ushort;

        struct Bits
        {
            ulong bulk;
            RemT rem;
        }

        union Repainter
        {
            T number;
            Bits bits;
            ubyte[T.sizeof] bytes;
        }

        import std.typecons : Tuple;
        Tuple!(Repainter, Repainter) vars = void;
        vars[0].number = x;
        vars[1].number = y;

        foreach (ref var; vars)
            if (var.bytes[F.SIGNPOS_BYTE] & 0x80)
            {
                var.bits.bulk = ~var.bits.bulk;
                var.bits.rem = cast(typeof(var.bits.rem))(-1 - var.bits.rem); // ~var.bits.rem
            }
            else
            {
                var.bytes[F.SIGNPOS_BYTE] |= 0x80;
            }

        version (LittleEndian)
        {
            if (vars[0].bits.rem < vars[1].bits.rem)
                return -1;
            else if (vars[0].bits.rem > vars[1].bits.rem)
                return 1;
            else if (vars[0].bits.bulk < vars[1].bits.bulk)
                return -1;
            else if (vars[0].bits.bulk > vars[1].bits.bulk)
                return 1;
            else
                return 0;
        }
        else
        {
            if (vars[0].bits.bulk < vars[1].bits.bulk)
                return -1;
            else if (vars[0].bits.bulk > vars[1].bits.bulk)
                return 1;
            else if (vars[0].bits.rem < vars[1].bits.rem)
                return -1;
            else if (vars[0].bits.rem > vars[1].bits.rem)
                return 1;
            else
                return 0;
        }
    }
    else
    {
        // IBM Extended doubledouble does not follow the general
        // sign-exponent-significand layout, so has to be handled generically

        import std.math.traits : signbit, isNaN;

        const int xSign = signbit(x),
            ySign = signbit(y);

        if (xSign == 1 && ySign == 1)
            return cmp(-y, -x);
        else if (xSign == 1)
            return -1;
        else if (ySign == 1)
            return 1;
        else if (x < y)
            return -1;
        else if (x == y)
            return 0;
        else if (x > y)
            return 1;
        else if (isNaN(x) && !isNaN(y))
            return 1;
        else if (isNaN(y) && !isNaN(x))
            return -1;
        else if (getNaNPayload(x) < getNaNPayload(y))
            return -1;
        else if (getNaNPayload(x) > getNaNPayload(y))
            return 1;
        else
            return 0;
    }
}

/// Most numbers are ordered naturally.
@safe unittest
{
    assert(cmp(-double.infinity, -double.max) < 0);
    assert(cmp(-double.max, -100.0) < 0);
    assert(cmp(-100.0, -0.5) < 0);
    assert(cmp(-0.5, 0.0) < 0);
    assert(cmp(0.0, 0.5) < 0);
    assert(cmp(0.5, 100.0) < 0);
    assert(cmp(100.0, double.max) < 0);
    assert(cmp(double.max, double.infinity) < 0);

    assert(cmp(1.0, 1.0) == 0);
}

/// Positive and negative zeroes are distinct.
@safe unittest
{
    assert(cmp(-0.0, +0.0) < 0);
    assert(cmp(+0.0, -0.0) > 0);
}

/// Depending on the sign, $(NAN)s go to either end of the spectrum.
@safe unittest
{
    assert(cmp(-double.nan, -double.infinity) < 0);
    assert(cmp(double.infinity, double.nan) < 0);
    assert(cmp(-double.nan, double.nan) < 0);
}

/// $(NAN)s of the same sign are ordered by the payload.
@safe unittest
{
    assert(cmp(NaN(10), NaN(20)) < 0);
    assert(cmp(-NaN(20), -NaN(10)) < 0);
}

@safe unittest
{
    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(float, double, real))
    {{
        T[] values = [-cast(T) NaN(20), -cast(T) NaN(10), -T.nan, -T.infinity,
                      -T.max, -T.max / 2, T(-16.0), T(-1.0).nextDown,
                      T(-1.0), T(-1.0).nextUp,
                      T(-0.5), -T.min_normal, (-T.min_normal).nextUp,
                      -2 * T.min_normal * T.epsilon,
                      -T.min_normal * T.epsilon,
                      T(-0.0), T(0.0),
                      T.min_normal * T.epsilon,
                      2 * T.min_normal * T.epsilon,
                      T.min_normal.nextDown, T.min_normal, T(0.5),
                      T(1.0).nextDown, T(1.0),
                      T(1.0).nextUp, T(16.0), T.max / 2, T.max,
                      T.infinity, T.nan, cast(T) NaN(10), cast(T) NaN(20)];

        foreach (i, x; values)
        {
            foreach (y; values[i + 1 .. $])
            {
                assert(cmp(x, y) < 0);
                assert(cmp(y, x) > 0);
            }
            assert(cmp(x, x) == 0);
        }
    }}
}

package(std): // not yet public

struct FloatingPointBitpattern(T)
if (isFloatingPoint!T)
{
    static if (T.mant_dig <= 64)
    {
        ulong mantissa;
    }
    else
    {
        ulong mantissa_lsb;
        ulong mantissa_msb;
    }

    int exponent;
    bool negative;
}

FloatingPointBitpattern!T extractBitpattern(T)(const(T) value) @trusted
if (isFloatingPoint!T)
{
    import std.math.traits : floatTraits, RealFormat;

    T val = value;
    FloatingPointBitpattern!T ret;

    alias F = floatTraits!T;
    static if (F.realFormat == RealFormat.ieeeExtended)
    {
        if (__ctfe)
        {
            import core.math : fabs, ldexp;
            import std.math.rounding : floor;
            import std.math.traits : isInfinity, isNaN, signbit;
            import std.math.exponential : log2;

            if (isNaN(val) || isInfinity(val))
                ret.exponent = 32767;
            else if (fabs(val) < real.min_normal)
                ret.exponent = 0;
            else if (fabs(val) >= nextUp(real.max / 2))
                ret.exponent = 32766;
            else
                ret.exponent = cast(int) (val.fabs.log2.floor() + 16383);

            if (ret.exponent == 32767)
            {
                // NaN or infinity
                ret.mantissa = isNaN(val) ? ((1L << 63) - 1) : 0;
            }
            else
            {
                auto delta = 16382 + 64 // bias + bits of ulong
                             - (ret.exponent == 0 ? 1 : ret.exponent); // -1 in case of subnormals
                val = ldexp(val, delta); // val *= 2^^delta

                ulong tmp = cast(ulong) fabs(val);
                if (ret.exponent != 32767 && ret.exponent > 0 && tmp <= ulong.max / 2)
                {
                    // correction, due to log2(val) being rounded up:
                    ret.exponent--;
                    val *= 2;
                    tmp = cast(ulong) fabs(val);
                }

                ret.mantissa = tmp & long.max;
            }

            ret.negative = (signbit(val) == 1);
        }
        else
        {
            ushort* vs = cast(ushort*) &val;
            ret.mantissa = (cast(ulong*) vs)[0] & long.max;
            ret.exponent = vs[4] & short.max;
            ret.negative = (vs[4] >> 15) & 1;
        }
    }
    else
    {
        static if (F.realFormat == RealFormat.ieeeSingle)
        {
            ulong ival = *cast(uint*) &val;
        }
        else static if (F.realFormat == RealFormat.ieeeDouble)
        {
            ulong ival = *cast(ulong*) &val;
        }
        else
        {
            static assert(false, "Floating point type `" ~ F.realFormat ~ "` not supported.");
        }

        import std.math.exponential : log2;
        enum log2_max_exp = cast(int) log2(T(T.max_exp));

        ret.mantissa = ival & ((1L << (T.mant_dig - 1)) - 1);
        ret.exponent = (ival >> (T.mant_dig - 1)) & ((1L << (log2_max_exp + 1)) - 1);
        ret.negative = (ival >> (T.mant_dig + log2_max_exp)) & 1;
    }

    // add leading 1 for normalized values and correct exponent for denormalied values
    if (ret.exponent != 0 && ret.exponent != 2 * T.max_exp - 1)
        ret.mantissa |= 1L << (T.mant_dig - 1);
    else if (ret.exponent == 0)
        ret.exponent = 1;

    ret.exponent -= T.max_exp - 1;

    return ret;
}

@safe pure unittest
{
    float f = 1.0f;
    auto bp = extractBitpattern(f);
    assert(bp.mantissa == 0x80_0000);
    assert(bp.exponent == 0);
    assert(bp.negative == false);

    f = float.max;
    bp = extractBitpattern(f);
    assert(bp.mantissa == 0xff_ffff);
    assert(bp.exponent == 127);
    assert(bp.negative == false);

    f = -1.5432e-17f;
    bp = extractBitpattern(f);
    assert(bp.mantissa == 0x8e_55c8);
    assert(bp.exponent == -56);
    assert(bp.negative == true);

    // using double literal due to https://issues.dlang.org/show_bug.cgi?id=20361
    f = 2.3822073893521890206e-44;
    bp = extractBitpattern(f);
    assert(bp.mantissa == 0x00_0011);
    assert(bp.exponent == -126);
    assert(bp.negative == false);

    f = -float.infinity;
    bp = extractBitpattern(f);
    assert(bp.mantissa == 0);
    assert(bp.exponent == 128);
    assert(bp.negative == true);

    f = float.nan;
    bp = extractBitpattern(f);
    assert(bp.mantissa != 0); // we don't guarantee payloads
    assert(bp.exponent == 128);
    assert(bp.negative == false);
}

@safe pure unittest
{
    double d = 1.0;
    auto bp = extractBitpattern(d);
    assert(bp.mantissa == 0x10_0000_0000_0000L);
    assert(bp.exponent == 0);
    assert(bp.negative == false);

    d = double.max;
    bp = extractBitpattern(d);
    assert(bp.mantissa == 0x1f_ffff_ffff_ffffL);
    assert(bp.exponent == 1023);
    assert(bp.negative == false);

    d = -1.5432e-222;
    bp = extractBitpattern(d);
    assert(bp.mantissa == 0x11_d9b6_a401_3b04L);
    assert(bp.exponent == -737);
    assert(bp.negative == true);

    d = 0.0.nextUp;
    bp = extractBitpattern(d);
    assert(bp.mantissa == 0x00_0000_0000_0001L);
    assert(bp.exponent == -1022);
    assert(bp.negative == false);

    d = -double.infinity;
    bp = extractBitpattern(d);
    assert(bp.mantissa == 0);
    assert(bp.exponent == 1024);
    assert(bp.negative == true);

    d = double.nan;
    bp = extractBitpattern(d);
    assert(bp.mantissa != 0); // we don't guarantee payloads
    assert(bp.exponent == 1024);
    assert(bp.negative == false);
}

@safe pure unittest
{
    import std.math.traits : floatTraits, RealFormat;

    alias F = floatTraits!real;
    static if (F.realFormat == RealFormat.ieeeExtended)
    {
        real r = 1.0L;
        auto bp = extractBitpattern(r);
        assert(bp.mantissa == 0x8000_0000_0000_0000L);
        assert(bp.exponent == 0);
        assert(bp.negative == false);

        r = real.max;
        bp = extractBitpattern(r);
        assert(bp.mantissa == 0xffff_ffff_ffff_ffffL);
        assert(bp.exponent == 16383);
        assert(bp.negative == false);

        r = -1.5432e-3333L;
        bp = extractBitpattern(r);
        assert(bp.mantissa == 0xc768_a2c7_a616_cc22L);
        assert(bp.exponent == -11072);
        assert(bp.negative == true);

        r = 0.0L.nextUp;
        bp = extractBitpattern(r);
        assert(bp.mantissa == 0x0000_0000_0000_0001L);
        assert(bp.exponent == -16382);
        assert(bp.negative == false);

        r = -float.infinity;
        bp = extractBitpattern(r);
        assert(bp.mantissa == 0);
        assert(bp.exponent == 16384);
        assert(bp.negative == true);

        r = float.nan;
        bp = extractBitpattern(r);
        assert(bp.mantissa != 0); // we don't guarantee payloads
        assert(bp.exponent == 16384);
        assert(bp.negative == false);

        r = nextDown(0x1p+16383L);
        bp = extractBitpattern(r);
        assert(bp.mantissa == 0xffff_ffff_ffff_ffffL);
        assert(bp.exponent == 16382);
        assert(bp.negative == false);
    }
}

@safe pure unittest
{
    import std.math.traits : floatTraits, RealFormat;
    import std.math.exponential : log2;

    alias F = floatTraits!real;

    // log2 is broken for x87-reals on some computers in CTFE
    // the following test excludes these computers from the test
    // (https://issues.dlang.org/show_bug.cgi?id=21757)
    enum test = cast(int) log2(3.05e2312L);
    static if (F.realFormat == RealFormat.ieeeExtended && test == 7681)
    {
        enum r1 = 1.0L;
        enum bp1 = extractBitpattern(r1);
        static assert(bp1.mantissa == 0x8000_0000_0000_0000L);
        static assert(bp1.exponent == 0);
        static assert(bp1.negative == false);

        enum r2 = real.max;
        enum bp2 = extractBitpattern(r2);
        static assert(bp2.mantissa == 0xffff_ffff_ffff_ffffL);
        static assert(bp2.exponent == 16383);
        static assert(bp2.negative == false);

        enum r3 = -1.5432e-3333L;
        enum bp3 = extractBitpattern(r3);
        static assert(bp3.mantissa == 0xc768_a2c7_a616_cc22L);
        static assert(bp3.exponent == -11072);
        static assert(bp3.negative == true);

        enum r4 = 0.0L.nextUp;
        enum bp4 = extractBitpattern(r4);
        static assert(bp4.mantissa == 0x0000_0000_0000_0001L);
        static assert(bp4.exponent == -16382);
        static assert(bp4.negative == false);

        enum r5 = -real.infinity;
        enum bp5 = extractBitpattern(r5);
        static assert(bp5.mantissa == 0);
        static assert(bp5.exponent == 16384);
        static assert(bp5.negative == true);

        enum r6 = real.nan;
        enum bp6 = extractBitpattern(r6);
        static assert(bp6.mantissa != 0); // we don't guarantee payloads
        static assert(bp6.exponent == 16384);
        static assert(bp6.negative == false);

        enum r7 = nextDown(0x1p+16383L);
        enum bp7 = extractBitpattern(r7);
        static assert(bp7.mantissa == 0xffff_ffff_ffff_ffffL);
        static assert(bp7.exponent == 16382);
        static assert(bp7.negative == false);
    }
}
