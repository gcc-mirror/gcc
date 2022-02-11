// Written in the D programming language.

/**
This is a submodule of $(MREF std, math).

It contains several versions of remainder calculation.

Copyright: Copyright The D Language Foundation 2000 - 2011.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright), Don Clugston,
           Conversion of CEPHES math library to D by Iain Buclaw and David Nadlinger
Source: $(PHOBOSSRC std/math/remainder.d)

Macros:
     TABLE_SV = <table border="1" cellpadding="4" cellspacing="0">
                <caption>Special Values</caption>
                $0</table>
     NAN = $(RED NAN)
     PLUSMN = &plusmn;
     PLUSMNINF = &plusmn;&infin;
 */

module std.math.remainder;

static import core.stdc.math;

/************************************
 * Calculates the remainder from the calculation x/y.
 * Returns:
 * The value of x - i * y, where i is the number of times that y can
 * be completely subtracted from x. The result has the same sign as x.
 *
 * $(TABLE_SV
 *  $(TR $(TH x)              $(TH y)             $(TH fmod(x, y))   $(TH invalid?))
 *  $(TR $(TD $(PLUSMN)0.0)   $(TD not 0.0)       $(TD $(PLUSMN)0.0) $(TD no))
 *  $(TR $(TD $(PLUSMNINF))   $(TD anything)      $(TD $(NAN))       $(TD yes))
 *  $(TR $(TD anything)       $(TD $(PLUSMN)0.0)  $(TD $(NAN))       $(TD yes))
 *  $(TR $(TD !=$(PLUSMNINF)) $(TD $(PLUSMNINF))  $(TD x)            $(TD no))
 * )
 */
real fmod(real x, real y) @trusted nothrow @nogc
{
    version (CRuntime_Microsoft)
    {
        return x % y;
    }
    else
        return core.stdc.math.fmodl(x, y);
}

///
@safe unittest
{
    import std.math.operations : feqrel;
    import std.math.traits : isIdentical, isNaN;

    assert(isIdentical(fmod(0.0, 1.0), 0.0));
    assert(fmod(5.0, 3.0).feqrel(2.0) > 16);
    assert(isNaN(fmod(5.0, 0.0)));
}

/************************************
 * Breaks x into an integral part and a fractional part, each of which has
 * the same sign as x. The integral part is stored in i.
 * Returns:
 * The fractional part of x.
 *
 * $(TABLE_SV
 *  $(TR $(TH x)              $(TH i (on input))  $(TH modf(x, i))   $(TH i (on return)))
 *  $(TR $(TD $(PLUSMNINF))   $(TD anything)      $(TD $(PLUSMN)0.0) $(TD $(PLUSMNINF)))
 * )
 */
real modf(real x, ref real i) @trusted nothrow @nogc
{
    version (CRuntime_Microsoft)
    {
        import std.math.traits : copysign, isInfinity;
        import std.math.rounding : trunc;

        i = trunc(x);
        return copysign(isInfinity(x) ? 0.0 : x - i, x);
    }
    else
        return core.stdc.math.modfl(x,&i);
}

///
@safe unittest
{
    import std.math.operations : feqrel;

    real frac;
    real intpart;

    frac = modf(3.14159, intpart);
    assert(intpart.feqrel(3.0) > 16);
    assert(frac.feqrel(0.14159) > 16);
}

/****************************************************
 * Calculate the remainder x REM y, following IEC 60559.
 *
 * REM is the value of x - y * n, where n is the integer nearest the exact
 * value of x / y.
 * If |n - x / y| == 0.5, n is even.
 * If the result is zero, it has the same sign as x.
 * Otherwise, the sign of the result is the sign of x / y.
 * Precision mode has no effect on the remainder functions.
 *
 * remquo returns `n` in the parameter `n`.
 *
 * $(TABLE_SV
 *  $(TR $(TH x)               $(TH y)            $(TH remainder(x, y)) $(TH n)   $(TH invalid?))
 *  $(TR $(TD $(PLUSMN)0.0)    $(TD not 0.0)      $(TD $(PLUSMN)0.0)    $(TD 0.0) $(TD no))
 *  $(TR $(TD $(PLUSMNINF))    $(TD anything)     $(TD -$(NAN))         $(TD ?)   $(TD yes))
 *  $(TR $(TD anything)        $(TD $(PLUSMN)0.0) $(TD $(PLUSMN)$(NAN)) $(TD ?)   $(TD yes))
 *  $(TR $(TD != $(PLUSMNINF)) $(TD $(PLUSMNINF)) $(TD x)               $(TD ?)   $(TD no))
 * )
 */
real remainder(real x, real y) @trusted nothrow @nogc
{
    return core.stdc.math.remainderl(x, y);
}

/// ditto
real remquo(real x, real y, out int n) @trusted nothrow @nogc  /// ditto
{
    return core.stdc.math.remquol(x, y, &n);
}

///
@safe @nogc nothrow unittest
{
    import std.math.operations : feqrel;
    import std.math.traits : isNaN;

    assert(remainder(5.1, 3.0).feqrel(-0.9) > 16);
    assert(remainder(-5.1, 3.0).feqrel(0.9) > 16);
    assert(remainder(0.0, 3.0) == 0.0);

    assert(isNaN(remainder(1.0, 0.0)));
    assert(isNaN(remainder(-1.0, 0.0)));
}

///
@safe @nogc nothrow unittest
{
    import std.math.operations : feqrel;

    int n;

    assert(remquo(5.1, 3.0, n).feqrel(-0.9) > 16 && n == 2);
    assert(remquo(-5.1, 3.0, n).feqrel(0.9) > 16 && n == -2);
    assert(remquo(0.0, 3.0, n) == 0.0 && n == 0);
}
