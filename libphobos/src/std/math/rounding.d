// Written in the D programming language.

/**
This is a submodule of $(MREF std, math).

It contains several functions for rounding floating point numbers.

Copyright: Copyright The D Language Foundation 2000 - 2011.
           D implementations of floor, ceil, and lrint functions are based on the
           CEPHES math library, which is Copyright (C) 2001 Stephen L. Moshier
           $(LT)steve@moshier.net$(GT) and are incorporated herein by permission
           of the author. The author reserves the right to distribute this
           material elsewhere under different copying permissions.
           These modifications are distributed here under the following terms:
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright), Don Clugston,
           Conversion of CEPHES math library to D by Iain Buclaw and David Nadlinger
Source: $(PHOBOSSRC std/math/rounding.d)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module std.math.rounding;

static import core.math;
static import core.stdc.math;

import std.traits : isFloatingPoint, isIntegral, Unqual;

version (D_InlineAsm_X86)    version = InlineAsm_X86_Any;
version (D_InlineAsm_X86_64) version = InlineAsm_X86_Any;

version (InlineAsm_X86_Any) version = InlineAsm_X87;
version (InlineAsm_X87)
{
    static assert(real.mant_dig == 64);
    version (CRuntime_Microsoft) version = InlineAsm_X87_MSVC;
}

/**************************************
 * Returns the value of x rounded upward to the next integer
 * (toward positive infinity).
 */
real ceil(real x) @trusted pure nothrow @nogc
{
    version (InlineAsm_X87_MSVC)
    {
        version (X86_64)
        {
            asm pure nothrow @nogc
            {
                naked                       ;
                fld     real ptr [RCX]      ;
                fstcw   8[RSP]              ;
                mov     AL,9[RSP]           ;
                mov     DL,AL               ;
                and     AL,0xC3             ;
                or      AL,0x08             ; // round to +infinity
                mov     9[RSP],AL           ;
                fldcw   8[RSP]              ;
                frndint                     ;
                mov     9[RSP],DL           ;
                fldcw   8[RSP]              ;
                ret                         ;
            }
        }
        else
        {
            short cw;
            asm pure nothrow @nogc
            {
                fld     x                   ;
                fstcw   cw                  ;
                mov     AL,byte ptr cw+1    ;
                mov     DL,AL               ;
                and     AL,0xC3             ;
                or      AL,0x08             ; // round to +infinity
                mov     byte ptr cw+1,AL    ;
                fldcw   cw                  ;
                frndint                     ;
                mov     byte ptr cw+1,DL    ;
                fldcw   cw                  ;
            }
        }
    }
    else
    {
        import std.math.traits : isInfinity, isNaN;

        // Special cases.
        if (isNaN(x) || isInfinity(x))
            return x;

        real y = floorImpl(x);
        if (y < x)
            y += 1.0;

        return y;
    }
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN;

    assert(ceil(+123.456L) == +124);
    assert(ceil(-123.456L) == -123);
    assert(ceil(-1.234L) == -1);
    assert(ceil(-0.123L) == 0);
    assert(ceil(0.0L) == 0);
    assert(ceil(+0.123L) == 1);
    assert(ceil(+1.234L) == 2);
    assert(ceil(real.infinity) == real.infinity);
    assert(isNaN(ceil(real.nan)));
    assert(isNaN(ceil(real.init)));
}

/// ditto
double ceil(double x) @trusted pure nothrow @nogc
{
    import std.math.traits : isInfinity, isNaN;

    // Special cases.
    if (isNaN(x) || isInfinity(x))
        return x;

    double y = floorImpl(x);
    if (y < x)
        y += 1.0;

    return y;
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN;

    assert(ceil(+123.456) == +124);
    assert(ceil(-123.456) == -123);
    assert(ceil(-1.234) == -1);
    assert(ceil(-0.123) == 0);
    assert(ceil(0.0) == 0);
    assert(ceil(+0.123) == 1);
    assert(ceil(+1.234) == 2);
    assert(ceil(double.infinity) == double.infinity);
    assert(isNaN(ceil(double.nan)));
    assert(isNaN(ceil(double.init)));
}

/// ditto
float ceil(float x) @trusted pure nothrow @nogc
{
    import std.math.traits : isInfinity, isNaN;

    // Special cases.
    if (isNaN(x) || isInfinity(x))
        return x;

    float y = floorImpl(x);
    if (y < x)
        y += 1.0;

    return y;
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN;

    assert(ceil(+123.456f) == +124);
    assert(ceil(-123.456f) == -123);
    assert(ceil(-1.234f) == -1);
    assert(ceil(-0.123f) == 0);
    assert(ceil(0.0f) == 0);
    assert(ceil(+0.123f) == 1);
    assert(ceil(+1.234f) == 2);
    assert(ceil(float.infinity) == float.infinity);
    assert(isNaN(ceil(float.nan)));
    assert(isNaN(ceil(float.init)));
}

/**************************************
 * Returns the value of x rounded downward to the next integer
 * (toward negative infinity).
 */
real floor(real x) @trusted pure nothrow @nogc
{
    version (InlineAsm_X87_MSVC)
    {
        version (X86_64)
        {
            asm pure nothrow @nogc
            {
                naked                       ;
                fld     real ptr [RCX]      ;
                fstcw   8[RSP]              ;
                mov     AL,9[RSP]           ;
                mov     DL,AL               ;
                and     AL,0xC3             ;
                or      AL,0x04             ; // round to -infinity
                mov     9[RSP],AL           ;
                fldcw   8[RSP]              ;
                frndint                     ;
                mov     9[RSP],DL           ;
                fldcw   8[RSP]              ;
                ret                         ;
            }
        }
        else
        {
            short cw;
            asm pure nothrow @nogc
            {
                fld     x                   ;
                fstcw   cw                  ;
                mov     AL,byte ptr cw+1    ;
                mov     DL,AL               ;
                and     AL,0xC3             ;
                or      AL,0x04             ; // round to -infinity
                mov     byte ptr cw+1,AL    ;
                fldcw   cw                  ;
                frndint                     ;
                mov     byte ptr cw+1,DL    ;
                fldcw   cw                  ;
            }
        }
    }
    else
    {
        import std.math.traits : isInfinity, isNaN;

        // Special cases.
        if (isNaN(x) || isInfinity(x) || x == 0.0)
            return x;

        return floorImpl(x);
    }
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN;

    assert(floor(+123.456L) == +123);
    assert(floor(-123.456L) == -124);
    assert(floor(+123.0L) == +123);
    assert(floor(-124.0L) == -124);
    assert(floor(-1.234L) == -2);
    assert(floor(-0.123L) == -1);
    assert(floor(0.0L) == 0);
    assert(floor(+0.123L) == 0);
    assert(floor(+1.234L) == 1);
    assert(floor(real.infinity) == real.infinity);
    assert(isNaN(floor(real.nan)));
    assert(isNaN(floor(real.init)));
}

/// ditto
double floor(double x) @trusted pure nothrow @nogc
{
    import std.math.traits : isInfinity, isNaN;

    // Special cases.
    if (isNaN(x) || isInfinity(x) || x == 0.0)
        return x;

    return floorImpl(x);
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN;

    assert(floor(+123.456) == +123);
    assert(floor(-123.456) == -124);
    assert(floor(+123.0) == +123);
    assert(floor(-124.0) == -124);
    assert(floor(-1.234) == -2);
    assert(floor(-0.123) == -1);
    assert(floor(0.0) == 0);
    assert(floor(+0.123) == 0);
    assert(floor(+1.234) == 1);
    assert(floor(double.infinity) == double.infinity);
    assert(isNaN(floor(double.nan)));
    assert(isNaN(floor(double.init)));
}

/// ditto
float floor(float x) @trusted pure nothrow @nogc
{
    import std.math.traits : isInfinity, isNaN;

    // Special cases.
    if (isNaN(x) || isInfinity(x) || x == 0.0)
        return x;

    return floorImpl(x);
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN;

    assert(floor(+123.456f) == +123);
    assert(floor(-123.456f) == -124);
    assert(floor(+123.0f) == +123);
    assert(floor(-124.0f) == -124);
    assert(floor(-1.234f) == -2);
    assert(floor(-0.123f) == -1);
    assert(floor(0.0f) == 0);
    assert(floor(+0.123f) == 0);
    assert(floor(+1.234f) == 1);
    assert(floor(float.infinity) == float.infinity);
    assert(isNaN(floor(float.nan)));
    assert(isNaN(floor(float.init)));
}

// https://issues.dlang.org/show_bug.cgi?id=6381
// floor/ceil should be usable in pure function.
@safe pure nothrow unittest
{
    auto x = floor(1.2);
    auto y = ceil(1.2);
}

/**
 * Round `val` to a multiple of `unit`. `rfunc` specifies the rounding
 * function to use; by default this is `rint`, which uses the current
 * rounding mode.
 */
Unqual!F quantize(alias rfunc = rint, F)(const F val, const F unit)
if (is(typeof(rfunc(F.init)) : F) && isFloatingPoint!F)
{
    import std.math.traits : isInfinity;

    typeof(return) ret = val;
    if (unit != 0)
    {
        const scaled = val / unit;
        if (!scaled.isInfinity)
            ret = rfunc(scaled) * unit;
    }
    return ret;
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;

    assert(isClose(12345.6789L.quantize(0.01L), 12345.68L));
    assert(isClose(12345.6789L.quantize!floor(0.01L), 12345.67L));
    assert(isClose(12345.6789L.quantize(22.0L), 12342.0L));
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isNaN;

    assert(isClose(12345.6789L.quantize(0), 12345.6789L));
    assert(12345.6789L.quantize(real.infinity).isNaN);
    assert(12345.6789L.quantize(real.nan).isNaN);
    assert(real.infinity.quantize(0.01L) == real.infinity);
    assert(real.infinity.quantize(real.nan).isNaN);
    assert(real.nan.quantize(0.01L).isNaN);
    assert(real.nan.quantize(real.infinity).isNaN);
    assert(real.nan.quantize(real.nan).isNaN);
}

/**
 * Round `val` to a multiple of `pow(base, exp)`. `rfunc` specifies the
 * rounding function to use; by default this is `rint`, which uses the
 * current rounding mode.
 */
Unqual!F quantize(real base, alias rfunc = rint, F, E)(const F val, const E exp)
if (is(typeof(rfunc(F.init)) : F) && isFloatingPoint!F && isIntegral!E)
{
    import std.math.exponential : pow;

    // TODO: Compile-time optimization for power-of-two bases?
    return quantize!rfunc(val, pow(cast(F) base, exp));
}

/// ditto
Unqual!F quantize(real base, long exp = 1, alias rfunc = rint, F)(const F val)
if (is(typeof(rfunc(F.init)) : F) && isFloatingPoint!F)
{
    import std.math.exponential : pow;

    enum unit = cast(F) pow(base, exp);
    return quantize!rfunc(val, unit);
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;

    assert(isClose(12345.6789L.quantize!10(-2), 12345.68L));
    assert(isClose(12345.6789L.quantize!(10, -2), 12345.68L));
    assert(isClose(12345.6789L.quantize!(10, floor)(-2), 12345.67L));
    assert(isClose(12345.6789L.quantize!(10, -2, floor), 12345.67L));

    assert(isClose(12345.6789L.quantize!22(1), 12342.0L));
    assert(isClose(12345.6789L.quantize!22, 12342.0L));
}

@safe pure nothrow @nogc unittest
{
    import std.math.exponential : log10, pow;
    import std.math.operations : isClose;
    import std.meta : AliasSeq;

    static foreach (F; AliasSeq!(real, double, float))
    {{
        const maxL10 = cast(int) F.max.log10.floor;
        const maxR10 = pow(cast(F) 10, maxL10);
        assert(isClose((cast(F) 0.9L * maxR10).quantize!10(maxL10), maxR10));
        assert(isClose((cast(F)-0.9L * maxR10).quantize!10(maxL10), -maxR10));

        assert(F.max.quantize(F.min_normal) == F.max);
        assert((-F.max).quantize(F.min_normal) == -F.max);
        assert(F.min_normal.quantize(F.max) == 0);
        assert((-F.min_normal).quantize(F.max) == 0);
        assert(F.min_normal.quantize(F.min_normal) == F.min_normal);
        assert((-F.min_normal).quantize(F.min_normal) == -F.min_normal);
    }}
}

/******************************************
 * Rounds x to the nearest integer value, using the current rounding
 * mode.
 *
 * Unlike the rint functions, nearbyint does not raise the
 * FE_INEXACT exception.
 */
pragma(inline, true)
real nearbyint(real x) @safe pure nothrow @nogc
{
    return core.stdc.math.nearbyintl(x);
}

///
@safe pure unittest
{
    import std.math.traits : isNaN;

    assert(nearbyint(0.4) == 0);
    assert(nearbyint(0.5) == 0);
    assert(nearbyint(0.6) == 1);
    assert(nearbyint(100.0) == 100);

    assert(isNaN(nearbyint(real.nan)));
    assert(nearbyint(real.infinity) == real.infinity);
    assert(nearbyint(-real.infinity) == -real.infinity);
}

/**********************************
 * Rounds x to the nearest integer value, using the current rounding
 * mode.
 *
 * If the return value is not equal to x, the FE_INEXACT
 * exception is raised.
 *
 * $(LREF nearbyint) performs the same operation, but does
 * not set the FE_INEXACT exception.
 */
pragma(inline, true)
real rint(real x) @safe pure nothrow @nogc
{
    return core.math.rint(x);
}
///ditto
pragma(inline, true)
double rint(double x) @safe pure nothrow @nogc
{
    return core.math.rint(x);
}
///ditto
pragma(inline, true)
float rint(float x) @safe pure nothrow @nogc
{
    return core.math.rint(x);
}

///
@safe unittest
{
    import std.math.traits : isNaN;

    version (IeeeFlagsSupport) resetIeeeFlags();
    assert(rint(0.4) == 0);
    version (GNU) { /* inexact bit not set with enabled optimizations */ } else
    version (IeeeFlagsSupport) assert(ieeeFlags.inexact);

    assert(rint(0.5) == 0);
    assert(rint(0.6) == 1);
    assert(rint(100.0) == 100);

    assert(isNaN(rint(real.nan)));
    assert(rint(real.infinity) == real.infinity);
    assert(rint(-real.infinity) == -real.infinity);
}

@safe unittest
{
    real function(real) print = &rint;
    assert(print != null);
}

/***************************************
 * Rounds x to the nearest integer value, using the current rounding
 * mode.
 *
 * This is generally the fastest method to convert a floating-point number
 * to an integer. Note that the results from this function
 * depend on the rounding mode, if the fractional part of x is exactly 0.5.
 * If using the default rounding mode (ties round to even integers)
 * lrint(4.5) == 4, lrint(5.5)==6.
 */
long lrint(real x) @trusted pure nothrow @nogc
{
    version (InlineAsm_X87)
    {
        version (Win64)
        {
            asm pure nothrow @nogc
            {
                naked;
                fld     real ptr [RCX];
                fistp   qword ptr 8[RSP];
                mov     RAX,8[RSP];
                ret;
            }
        }
        else
        {
            long n;
            asm pure nothrow @nogc
            {
                fld x;
                fistp n;
            }
            return n;
        }
    }
    else
    {
        import std.math : floatTraits, RealFormat, MANTISSA_MSB, MANTISSA_LSB;

        alias F = floatTraits!(real);
        static if (F.realFormat == RealFormat.ieeeDouble)
        {
            long result;

            // Rounding limit when casting from real(double) to ulong.
            enum real OF = 4.50359962737049600000E15L;

            uint* vi = cast(uint*)(&x);

            // Find the exponent and sign
            uint msb = vi[MANTISSA_MSB];
            uint lsb = vi[MANTISSA_LSB];
            int exp = ((msb >> 20) & 0x7ff) - 0x3ff;
            const int sign = msb >> 31;
            msb &= 0xfffff;
            msb |= 0x100000;

            if (exp < 63)
            {
                if (exp >= 52)
                    result = (cast(long) msb << (exp - 20)) | (lsb << (exp - 52));
                else
                {
                    // Adjust x and check result.
                    const real j = sign ? -OF : OF;
                    x = (j + x) - j;
                    msb = vi[MANTISSA_MSB];
                    lsb = vi[MANTISSA_LSB];
                    exp = ((msb >> 20) & 0x7ff) - 0x3ff;
                    msb &= 0xfffff;
                    msb |= 0x100000;

                    if (exp < 0)
                        result = 0;
                    else if (exp < 20)
                        result = cast(long) msb >> (20 - exp);
                    else if (exp == 20)
                        result = cast(long) msb;
                    else
                        result = (cast(long) msb << (exp - 20)) | (lsb >> (52 - exp));
                }
            }
            else
            {
                // It is left implementation defined when the number is too large.
                return cast(long) x;
            }

            return sign ? -result : result;
        }
        else static if (F.realFormat == RealFormat.ieeeExtended ||
                        F.realFormat == RealFormat.ieeeExtended53)
        {
            long result;

            // Rounding limit when casting from real(80-bit) to ulong.
            static if (F.realFormat == RealFormat.ieeeExtended)
                enum real OF = 9.22337203685477580800E18L;
            else
                enum real OF = 4.50359962737049600000E15L;

            ushort* vu = cast(ushort*)(&x);
            uint* vi = cast(uint*)(&x);

            // Find the exponent and sign
            int exp = (vu[F.EXPPOS_SHORT] & 0x7fff) - 0x3fff;
            const int sign = (vu[F.EXPPOS_SHORT] >> 15) & 1;

            if (exp < 63)
            {
                // Adjust x and check result.
                const real j = sign ? -OF : OF;
                x = (j + x) - j;
                exp = (vu[F.EXPPOS_SHORT] & 0x7fff) - 0x3fff;

                version (LittleEndian)
                {
                    if (exp < 0)
                        result = 0;
                    else if (exp <= 31)
                        result = vi[1] >> (31 - exp);
                    else
                        result = (cast(long) vi[1] << (exp - 31)) | (vi[0] >> (63 - exp));
                }
                else
                {
                    if (exp < 0)
                        result = 0;
                    else if (exp <= 31)
                        result = vi[1] >> (31 - exp);
                    else
                        result = (cast(long) vi[1] << (exp - 31)) | (vi[2] >> (63 - exp));
                }
            }
            else
            {
                // It is left implementation defined when the number is too large
                // to fit in a 64bit long.
                return cast(long) x;
            }

            return sign ? -result : result;
        }
        else static if (F.realFormat == RealFormat.ieeeQuadruple)
        {
            const vu = cast(ushort*)(&x);

            // Find the exponent and sign
            const sign = (vu[F.EXPPOS_SHORT] >> 15) & 1;
            if ((vu[F.EXPPOS_SHORT] & F.EXPMASK) - (F.EXPBIAS + 1) > 63)
            {
                // The result is left implementation defined when the number is
                // too large to fit in a 64 bit long.
                return cast(long) x;
            }

            // Force rounding of lower bits according to current rounding
            // mode by adding ±2^-112 and subtracting it again.
            enum OF = 5.19229685853482762853049632922009600E33L;
            const j = sign ? -OF : OF;
            x = (j + x) - j;

            const exp = (vu[F.EXPPOS_SHORT] & F.EXPMASK) - (F.EXPBIAS + 1);
            const implicitOne = 1UL << 48;
            auto vl = cast(ulong*)(&x);
            vl[MANTISSA_MSB] &= implicitOne - 1;
            vl[MANTISSA_MSB] |= implicitOne;

            long result;

            if (exp < 0)
                result = 0;
            else if (exp <= 48)
                result = vl[MANTISSA_MSB] >> (48 - exp);
            else
                result = (vl[MANTISSA_MSB] << (exp - 48)) | (vl[MANTISSA_LSB] >> (112 - exp));

            return sign ? -result : result;
        }
        else
        {
            static assert(false, "real type not supported by lrint()");
        }
    }
}

///
@safe pure nothrow @nogc unittest
{
    assert(lrint(4.5) == 4);
    assert(lrint(5.5) == 6);
    assert(lrint(-4.5) == -4);
    assert(lrint(-5.5) == -6);

    assert(lrint(int.max - 0.5) == 2147483646L);
    assert(lrint(int.max + 0.5) == 2147483648L);
    assert(lrint(int.min - 0.5) == -2147483648L);
    assert(lrint(int.min + 0.5) == -2147483648L);
}

static if (real.mant_dig >= long.sizeof * 8)
{
    @safe pure nothrow @nogc unittest
    {
        assert(lrint(long.max - 1.5L) == long.max - 1);
        assert(lrint(long.max - 0.5L) == long.max - 1);
        assert(lrint(long.min + 0.5L) == long.min);
        assert(lrint(long.min + 1.5L) == long.min + 2);
    }
}

/*******************************************
 * Return the value of x rounded to the nearest integer.
 * If the fractional part of x is exactly 0.5, the return value is
 * rounded away from zero.
 *
 * Returns:
 *     A `real`.
 */
auto round(real x) @trusted nothrow @nogc
{
    version (CRuntime_Microsoft)
    {
        import std.math.hardware : FloatingPointControl;

        auto old = FloatingPointControl.getControlState();
        FloatingPointControl.setControlState(
            (old & (-1 - FloatingPointControl.roundingMask)) | FloatingPointControl.roundToZero
        );
        x = core.math.rint((x >= 0) ? x + 0.5 : x - 0.5);
        FloatingPointControl.setControlState(old);
        return x;
    }
    else
    {
        return core.stdc.math.roundl(x);
    }
}

///
@safe nothrow @nogc unittest
{
    assert(round(4.5) == 5);
    assert(round(5.4) == 5);
    assert(round(-4.5) == -5);
    assert(round(-5.1) == -5);
}

// assure purity on Posix
version (Posix)
{
    @safe pure nothrow @nogc unittest
    {
        assert(round(4.5) == 5);
    }
}

/**********************************************
 * Return the value of x rounded to the nearest integer.
 *
 * If the fractional part of x is exactly 0.5, the return value is rounded
 * away from zero.
 *
 * $(BLUE This function is not implemented for Digital Mars C runtime.)
 */
long lround(real x) @trusted nothrow @nogc
{
    version (CRuntime_DigitalMars)
        assert(0, "lround not implemented");
    else
        return core.stdc.math.llroundl(x);
}

///
@safe nothrow @nogc unittest
{
    version (CRuntime_DigitalMars) {}
    else
    {
        assert(lround(0.49) == 0);
        assert(lround(0.5) == 1);
        assert(lround(1.5) == 2);
    }
}

/**
 Returns the integer portion of x, dropping the fractional portion.
 This is also known as "chop" rounding.
 `pure` on all platforms.
 */
real trunc(real x) @trusted nothrow @nogc pure
{
    version (InlineAsm_X87_MSVC)
    {
        version (X86_64)
        {
            asm pure nothrow @nogc
            {
                naked                       ;
                fld     real ptr [RCX]      ;
                fstcw   8[RSP]              ;
                mov     AL,9[RSP]           ;
                mov     DL,AL               ;
                and     AL,0xC3             ;
                or      AL,0x0C             ; // round to 0
                mov     9[RSP],AL           ;
                fldcw   8[RSP]              ;
                frndint                     ;
                mov     9[RSP],DL           ;
                fldcw   8[RSP]              ;
                ret                         ;
            }
        }
        else
        {
            short cw;
            asm pure nothrow @nogc
            {
                fld     x                   ;
                fstcw   cw                  ;
                mov     AL,byte ptr cw+1    ;
                mov     DL,AL               ;
                and     AL,0xC3             ;
                or      AL,0x0C             ; // round to 0
                mov     byte ptr cw+1,AL    ;
                fldcw   cw                  ;
                frndint                     ;
                mov     byte ptr cw+1,DL    ;
                fldcw   cw                  ;
            }
        }
    }
    else
    {
        return core.stdc.math.truncl(x);
    }
}

///
@safe pure unittest
{
    assert(trunc(0.01) == 0);
    assert(trunc(0.49) == 0);
    assert(trunc(0.5) == 0);
    assert(trunc(1.5) == 1);
}

/*****************************************
 * Returns x rounded to a long value using the current rounding mode.
 * If the integer value of x is
 * greater than long.max, the result is
 * indeterminate.
 */
pragma(inline, true)
long rndtol(real x) @nogc @safe pure nothrow { return core.math.rndtol(x); }
//FIXME
///ditto
pragma(inline, true)
long rndtol(double x) @safe pure nothrow @nogc { return rndtol(cast(real) x); }
//FIXME
///ditto
pragma(inline, true)
long rndtol(float x) @safe pure nothrow @nogc { return rndtol(cast(real) x); }

///
@safe unittest
{
    assert(rndtol(1.0) == 1L);
    assert(rndtol(1.2) == 1L);
    assert(rndtol(1.7) == 2L);
    assert(rndtol(1.0001) == 1L);
}

@safe unittest
{
    long function(real) prndtol = &rndtol;
    assert(prndtol != null);
}

// Helper for floor/ceil
T floorImpl(T)(const T x) @trusted pure nothrow @nogc
{
    import std.math : floatTraits, RealFormat;

    alias F = floatTraits!(T);
    // Take care not to trigger library calls from the compiler,
    // while ensuring that we don't get defeated by some optimizers.
    union floatBits
    {
        T rv;
        ushort[T.sizeof/2] vu;

        // Other kinds of extractors for real formats.
        static if (F.realFormat == RealFormat.ieeeSingle)
            uint vi;
        else static if (F.realFormat == RealFormat.ieeeDouble)
            ulong vi;
    }
    floatBits y = void;
    y.rv = x;

    // Find the exponent (power of 2)
    // Do this by shifting the raw value so that the exponent lies in the low bits,
    // then mask out the sign bit, and subtract the bias.
    static if (F.realFormat == RealFormat.ieeeSingle)
    {
        int exp = ((y.vi >> (T.mant_dig - 1)) & 0xff) - 0x7f;
        enum mantissa_mask = F.MANTISSAMASK_INT;
        enum sign_shift = 31;
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        long exp = ((y.vi >> (T.mant_dig - 1)) & 0x7ff) - 0x3ff;
        enum mantissa_mask = F.MANTISSAMASK_LONG;
        enum sign_shift = 63;
    }
    else static if (F.realFormat == RealFormat.ieeeExtended ||
                    F.realFormat == RealFormat.ieeeExtended53)
    {
        int exp = (y.vu[F.EXPPOS_SHORT] & 0x7fff) - 0x3fff;

        version (LittleEndian)
            int pos = 0;
        else
            int pos = 4;
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple)
    {
        int exp = (y.vu[F.EXPPOS_SHORT] & 0x7fff) - 0x3fff;

        version (LittleEndian)
            int pos = 0;
        else
            int pos = 7;
    }
    else
        static assert(false, "Not implemented for this architecture");

    if (exp < 0)
    {
        if (x < 0.0)
            return -1.0;
        else
            return 0.0;
    }

    static if (F.realFormat == RealFormat.ieeeSingle ||
               F.realFormat == RealFormat.ieeeDouble)
    {
        if (exp < (T.mant_dig - 1))
        {
            // Clear all bits representing the fraction part.
            // Note: the fraction mask represents the floating point number 0.999999...
            // i.e: `2.0 ^^ (exp - T.mant_dig + 1) * (fraction_mask + 1) == 1.0`
            const fraction_mask = mantissa_mask >> exp;

            if ((y.vi & fraction_mask) != 0)
            {
                // If 'x' is negative, then first substract (1.0 - T.epsilon) from the value.
                if (y.vi >> sign_shift)
                    y.vi += fraction_mask;
                y.vi &= ~fraction_mask;
            }
        }
    }
    else
    {
        static if (F.realFormat == RealFormat.ieeeExtended53)
            exp = (T.mant_dig + 11 - 1) - exp; // mant_dig is really 64
        else
            exp = (T.mant_dig - 1) - exp;

        // Zero 16 bits at a time.
        while (exp >= 16)
        {
            version (LittleEndian)
                y.vu[pos++] = 0;
            else
                y.vu[pos--] = 0;
            exp -= 16;
        }

        // Clear the remaining bits.
        if (exp > 0)
            y.vu[pos] &= 0xffff ^ ((1 << exp) - 1);

        if ((x < 0.0) && (x != y.rv))
            y.rv -= 1.0;
    }

    return y.rv;
}
