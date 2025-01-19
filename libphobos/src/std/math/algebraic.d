// Written in the D programming language.

/**
This is a submodule of $(MREF std, math).

It contains classical algebraic functions like `abs`, `sqrt`, and `poly`.

Copyright: Copyright The D Language Foundation 2000 - 2011.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright), Don Clugston,
           Conversion of CEPHES math library to D by Iain Buclaw and David Nadlinger
Source: $(PHOBOSSRC std/math/algebraic.d)

Macros:
    TABLE_SV = <table border="1" cellpadding="4" cellspacing="0">
               <caption>Special Values</caption>
               $0</table>
    NAN = $(RED NAN)
    POWER = $1<sup>$2</sup>
    SUB = $1<sub>$2</sub>
    PLUSMN = &plusmn;
    INFIN = &infin;
    PLUSMNINF = &plusmn;&infin;
    LT = &lt;

 */

module std.math.algebraic;

static import core.math;
static import core.stdc.math;
import std.traits : CommonType, isFloatingPoint, isIntegral, isSigned, Unqual;

/***********************************
 * Calculates the absolute value of a number.
 *
 * Params:
 *     Num = (template parameter) type of number
 *       x = real number value
 *
 * Returns:
 *     The absolute value of the number. If floating-point or integral,
 *     the return type will be the same as the input.
 *
 * Limitations:
 *     When x is a signed integral equal to `Num.min` the value of x will be returned instead.
 *     Note for 2's complement; `-Num.min` (= `Num.max + 1`) is not representable due to overflow.
 */
auto abs(Num)(Num x) @nogc nothrow pure
if (isIntegral!Num || (is(typeof(Num.init >= 0)) && is(typeof(-Num.init))))
{
    static if (isFloatingPoint!(Num))
        return fabs(x);
    else
    {
        static if (isIntegral!Num)
            return x >= 0 ? x : cast(Num) -x;
        else
            return x >= 0 ? x : -x;
    }
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.traits : isIdentical, isNaN;

    assert(isIdentical(abs(-0.0L), 0.0L));
    assert(isNaN(abs(real.nan)));
    assert(abs(-real.infinity) == real.infinity);
    assert(abs(-56) == 56);
    assert(abs(2321312L)  == 2321312L);
    assert(abs(23u) == 23u);
}

@safe pure nothrow @nogc unittest
{
    assert(abs(byte(-8)) == 8);
    assert(abs(ubyte(8u)) == 8);
    assert(abs(short(-8)) == 8);
    assert(abs(ushort(8u)) == 8);
    assert(abs(int(-8)) == 8);
    assert(abs(uint(8u)) == 8);
    assert(abs(long(-8)) == 8);
    assert(abs(ulong(8u)) == 8);
    assert(is(typeof(abs(byte(-8))) == byte));
    assert(is(typeof(abs(ubyte(8u))) == ubyte));
    assert(is(typeof(abs(short(-8))) == short));
    assert(is(typeof(abs(ushort(8u))) == ushort));
    assert(is(typeof(abs(int(-8))) == int));
    assert(is(typeof(abs(uint(8u))) == uint));
    assert(is(typeof(abs(long(-8))) == long));
    assert(is(typeof(abs(ulong(8u))) == ulong));
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(float, double, real))
    {{
        T f = 3;
        assert(abs(f) == f);
        assert(abs(-f) == f);
    }}
}

// see https://issues.dlang.org/show_bug.cgi?id=20205
// to avoid falling into the trap again
@safe pure nothrow @nogc unittest
{
    assert(50 - abs(-100) == -50);
}

// https://issues.dlang.org/show_bug.cgi?id=19162
@safe unittest
{
    struct Vector(T, int size)
    {
        T x, y, z;
    }

    static auto abs(T, int size)(auto ref const Vector!(T, size) v)
    {
        return v;
    }
    Vector!(int, 3) v;
    assert(abs(v) == v);
}

/*******************************
 * Returns |x|
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                 $(TH fabs(x)))
 *      $(TR $(TD $(PLUSMN)0.0)      $(TD +0.0) )
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD +$(INFIN)) )
 *      )
 */
pragma(inline, true)
real fabs(real x) @safe pure nothrow @nogc { return core.math.fabs(x); }

///ditto
pragma(inline, true)
double fabs(double x) @safe pure nothrow @nogc { return core.math.fabs(x); }

///ditto
pragma(inline, true)
float fabs(float x) @safe pure nothrow @nogc { return core.math.fabs(x); }

///
@safe unittest
{
    import std.math.traits : isIdentical;

    assert(isIdentical(fabs(0.0f), 0.0f));
    assert(isIdentical(fabs(-0.0f), 0.0f));
    assert(fabs(-10.0f) == 10.0f);

    assert(isIdentical(fabs(0.0), 0.0));
    assert(isIdentical(fabs(-0.0), 0.0));
    assert(fabs(-10.0) == 10.0);

    assert(isIdentical(fabs(0.0L), 0.0L));
    assert(isIdentical(fabs(-0.0L), 0.0L));
    assert(fabs(-10.0L) == 10.0L);
}

@safe unittest
{
    real function(real) pfabs = &fabs;
    assert(pfabs != null);
}

@safe pure nothrow @nogc unittest
{
    float f = fabs(-2.0f);
    assert(f == 2);

    double d = fabs(-2.0);
    assert(d == 2);

    real r = fabs(-2.0L);
    assert(r == 2);
}

/***************************************
 * Compute square root of x.
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)         $(TH sqrt(x))   $(TH invalid?))
 *      $(TR $(TD -0.0)      $(TD -0.0)      $(TD no))
 *      $(TR $(TD $(LT)0.0)  $(TD $(NAN))    $(TD yes))
 *      $(TR $(TD +$(INFIN)) $(TD +$(INFIN)) $(TD no))
 *      )
 */
pragma(inline, true)
float sqrt(float x) @nogc @safe pure nothrow { return core.math.sqrt(x); }

/// ditto
pragma(inline, true)
double sqrt(double x) @nogc @safe pure nothrow { return core.math.sqrt(x); }

/// ditto
pragma(inline, true)
real sqrt(real x) @nogc @safe pure nothrow { return core.math.sqrt(x); }

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : feqrel;
    import std.math.traits : isNaN;

    assert(sqrt(2.0).feqrel(1.4142) > 16);
    assert(sqrt(9.0).feqrel(3.0) > 16);

    assert(isNaN(sqrt(-1.0f)));
    assert(isNaN(sqrt(-1.0)));
    assert(isNaN(sqrt(-1.0L)));
}

@safe unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=5305
    float function(float) psqrtf = &sqrt;
    assert(psqrtf != null);
    double function(double) psqrtd = &sqrt;
    assert(psqrtd != null);
    real function(real) psqrtr = &sqrt;
    assert(psqrtr != null);

    //ctfe
    enum ZX80 = sqrt(7.0f);
    enum ZX81 = sqrt(7.0);
    enum ZX82 = sqrt(7.0L);
}

@safe pure nothrow @nogc unittest
{
    float f = sqrt(2.0f);
    assert(fabs(f * f - 2.0f) < .00001);

    double d = sqrt(2.0);
    assert(fabs(d * d - 2.0) < .00001);

    real r = sqrt(2.0L);
    assert(fabs(r * r - 2.0) < .00001);
}

/***************
 * Calculates the cube root of x.
 *
 *      $(TABLE_SV
 *      $(TR $(TH $(I x))            $(TH cbrt(x))           $(TH invalid?))
 *      $(TR $(TD $(PLUSMN)0.0)      $(TD $(PLUSMN)0.0)      $(TD no) )
 *      $(TR $(TD $(NAN))            $(TD $(NAN))            $(TD yes) )
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD $(PLUSMN)$(INFIN)) $(TD no) )
 *      )
 */
real cbrt(real x) @trusted nothrow @nogc
{
    version (CRuntime_Microsoft)
    {
        import std.math.traits : copysign;
        import std.math.exponential : exp2;

        version (INLINE_YL2X)
            return copysign(exp2(core.math.yl2x(fabs(x), 1.0L/3.0L)), x);
        else
            return core.stdc.math.cbrtl(x);
    }
    else
        return core.stdc.math.cbrtl(x);
}

///
@safe unittest
{
    import std.math.operations : feqrel;

    assert(cbrt(1.0).feqrel(1.0) > 16);
    assert(cbrt(27.0).feqrel(3.0) > 16);
    assert(cbrt(15.625).feqrel(2.5) > 16);
}

/***********************************************************************
 * Calculates the length of the
 * hypotenuse of a right-angled triangle with sides of length x and y.
 * The hypotenuse is the value of the square root of
 * the sums of the squares of x and y:
 *
 *      sqrt($(POWER x, 2) + $(POWER y, 2))
 *
 * Note that hypot(x, y), hypot(y, x) and
 * hypot(x, -y) are equivalent.
 *
 *  $(TABLE_SV
 *  $(TR $(TH x)            $(TH y)            $(TH hypot(x, y)) $(TH invalid?))
 *  $(TR $(TD x)            $(TD $(PLUSMN)0.0) $(TD |x|)         $(TD no))
 *  $(TR $(TD $(PLUSMNINF)) $(TD y)            $(TD +$(INFIN))   $(TD no))
 *  $(TR $(TD $(PLUSMNINF)) $(TD $(NAN))       $(TD +$(INFIN))   $(TD no))
 *  )
 */
T hypot(T)(const T x, const T y) @safe pure nothrow @nogc
if (isFloatingPoint!T)
{
    // Scale x and y to avoid underflow and overflow.
    // If one is huge and the other tiny, return the larger.
    // If both are huge, avoid overflow by scaling by 2^^-N.
    // If both are tiny, avoid underflow by scaling by 2^^N.
    import core.math : fabs, sqrt;
    import std.math.traits : floatTraits, RealFormat;

    alias F = floatTraits!T;

    T u = fabs(x);
    T v = fabs(y);
    if (!(u >= v))  // check for NaN as well.
    {
        v = u;
        u = fabs(y);
        if (u == T.infinity) return u; // hypot(inf, nan) == inf
        if (v == T.infinity) return v; // hypot(nan, inf) == inf
    }

    static if (F.realFormat == RealFormat.ieeeSingle)
    {
        enum SQRTMIN = 0x1p-60f;
        enum SQRTMAX = 0x1p+60f;
        enum SCALE_UNDERFLOW = 0x1p+90f;
        enum SCALE_OVERFLOW = 0x1p-90f;
    }
    else static if (F.realFormat == RealFormat.ieeeDouble ||
                    F.realFormat == RealFormat.ieeeExtended53 ||
                    F.realFormat == RealFormat.ibmExtended)
    {
        enum SQRTMIN = 0x1p-450L;
        enum SQRTMAX = 0x1p+500L;
        enum SCALE_UNDERFLOW = 0x1p+600L;
        enum SCALE_OVERFLOW = 0x1p-600L;
    }
    else static if (F.realFormat == RealFormat.ieeeExtended ||
                    F.realFormat == RealFormat.ieeeQuadruple)
    {
        enum SQRTMIN = 0x1p-8000L;
        enum SQRTMAX = 0x1p+8000L;
        enum SCALE_UNDERFLOW = 0x1p+10000L;
        enum SCALE_OVERFLOW = 0x1p-10000L;
    }
    else
        assert(0, "hypot not implemented");

    // Now u >= v, or else one is NaN.
    T ratio = 1.0;
    if (v >= SQRTMAX)
    {
        // hypot(huge, huge) -- avoid overflow
        ratio = SCALE_UNDERFLOW;
        u *= SCALE_OVERFLOW;
        v *= SCALE_OVERFLOW;
    }
    else if (u <= SQRTMIN)
    {
        // hypot (tiny, tiny) -- avoid underflow
        // This is only necessary to avoid setting the underflow
        // flag.
        ratio = SCALE_OVERFLOW;
        u *= SCALE_UNDERFLOW;
        v *= SCALE_UNDERFLOW;
    }

    if (u * T.epsilon > v)
    {
        // hypot (huge, tiny) = huge
        return u;
    }

    // both are in the normal range
    return ratio * sqrt(u*u + v*v);
}

///
@safe unittest
{
    import std.math.operations : feqrel;

    assert(hypot(1.0, 1.0).feqrel(1.4142) > 16);
    assert(hypot(3.0, 4.0).feqrel(5.0) > 16);
    assert(hypot(real.infinity, 1.0L) == real.infinity);
    assert(hypot(real.infinity, real.nan) == real.infinity);
}

@safe unittest
{
    import std.math.operations : feqrel;

    assert(hypot(1.0f, 1.0f).feqrel(1.4142f) > 16);
    assert(hypot(3.0f, 4.0f).feqrel(5.0f) > 16);
    assert(hypot(float.infinity, 1.0f) == float.infinity);
    assert(hypot(float.infinity, float.nan) == float.infinity);

    assert(hypot(1.0L, 1.0L).feqrel(1.4142L) > 16);
    assert(hypot(3.0L, 4.0L).feqrel(5.0L) > 16);
    assert(hypot(double.infinity, 1.0) == double.infinity);
    assert(hypot(double.infinity, double.nan) == double.infinity);
}

@safe unittest
{
    import std.math.operations : feqrel;
    import std.math.traits : isIdentical;
    import std.meta : AliasSeq;

    static foreach (T; AliasSeq!(float, double, real))
    {{
        static T[3][] vals =     // x,y,hypot
        [
            [ 0.0,     0.0,   0.0],
            [ 0.0,    -0.0,   0.0],
            [ -0.0,   -0.0,   0.0],
            [ 3.0,     4.0,   5.0],
            [ -300,   -400,   500],
            [0.0,      7.0,   7.0],
            [9.0,   9*T.epsilon,   9.0],
            [88/(64*sqrt(T.min_normal)), 105/(64*sqrt(T.min_normal)), 137/(64*sqrt(T.min_normal))],
            [88/(128*sqrt(T.min_normal)), 105/(128*sqrt(T.min_normal)), 137/(128*sqrt(T.min_normal))],
            [3*T.min_normal*T.epsilon, 4*T.min_normal*T.epsilon, 5*T.min_normal*T.epsilon],
            [ T.min_normal, T.min_normal, sqrt(2.0L)*T.min_normal],
            [ T.max/sqrt(2.0L), T.max/sqrt(2.0L), T.max],
            [ T.infinity, T.nan, T.infinity],
            [ T.nan, T.infinity, T.infinity],
            [ T.nan, T.nan, T.nan],
            [ T.nan, T.max, T.nan],
            [ T.max, T.nan, T.nan],
        ];
        for (int i = 0; i < vals.length; i++)
        {
            T x = vals[i][0];
            T y = vals[i][1];
            T z = vals[i][2];
            T h = hypot(x, y);
            assert(isIdentical(z,h) || feqrel(z, h) >= T.mant_dig - 1);
        }
     }}
}

/***********************************************************************
 * Calculates the distance of the point (x, y, z) from the origin (0, 0, 0)
 * in three-dimensional space.
 * The distance is the value of the square root of the sums of the squares
 * of x, y, and z:
 *
 *      sqrt($(POWER x, 2) + $(POWER y, 2) + $(POWER z, 2))
 *
 * Note that the distance between two points (x1, y1, z1) and (x2, y2, z2)
 * in three-dimensional space can be calculated as hypot(x2-x1, y2-y1, z2-z1).
 *
 * Params:
 *     x = floating point value
 *     y = floating point value
 *     z = floating point value
 *
 * Returns:
 *     The square root of the sum of the squares of the given arguments.
 */
T hypot(T)(const T x, const T y, const T z) @safe pure nothrow @nogc
if (isFloatingPoint!T)
{
    import core.math : fabs, sqrt;
    import std.math.operations : fmax;
    const absx = fabs(x);
    const absy = fabs(y);
    const absz = fabs(z);

    // Scale all parameters to avoid overflow.
    const ratio = fmax(absx, fmax(absy, absz));
    if (ratio == 0.0)
        return ratio;

    return ratio * sqrt((absx / ratio) * (absx / ratio)
                        + (absy / ratio) * (absy / ratio)
                        + (absz / ratio) * (absz / ratio));
}

///
@safe unittest
{
    import std.math.operations : isClose;

    assert(isClose(hypot(1.0, 2.0, 2.0), 3.0));
    assert(isClose(hypot(2.0, 3.0, 6.0), 7.0));
    assert(isClose(hypot(1.0, 4.0, 8.0), 9.0));
}

@safe unittest
{
    import std.meta : AliasSeq;
    import std.math.traits : isIdentical;
    import std.math.operations : isClose;
    static foreach (T; AliasSeq!(float, double, real))
    {{
        static T[4][] vals = [
            [ 0.0L, 0.0L, 0.0L, 0.0L ],
            [ 0.0L, 1.0L, 1.0L, sqrt(2.0L) ],
            [ 1.0L, 1.0L, 1.0L, sqrt(3.0L) ],
            [ 1.0L, 2.0L, 2.0L, 3.0L ],
            [ 2.0L, 3.0L, 6.0L, 7.0L ],
            [ 1.0L, 4.0L, 8.0L, 9.0L ],
            [ 4.0L, 4.0L, 7.0L, 9.0L ],
            [ 12.0L, 16.0L, 21.0L, 29.0L ],
            [ 1e+8L, 1.0L, 1e-8L, 1e+8L+5e-9L ],
            [ 1.0L, 1e+8L, 1e-8L, 1e+8L+5e-9L ],
            [ 1e-8L, 1.0L, 1e+8L, 1e+8L+5e-9L ],
            [ 1e-2L, 1e-4L, 1e-4L, 0.010000999950004999375L ],
            [ 2147483647.0L, 2147483647.0L, 2147483647.0L, 3719550785.027307813987L ]
        ];
        for (int i = 0; i < vals.length; i++)
        {
            T x = vals[i][0];
            T y = vals[i][1];
            T z = vals[i][2];
            T r = vals[i][3];
            T a = hypot(x, y, z);
            assert(isIdentical(r, a) || isClose(r, a));
        }
    }}
}

/***********************************
 * Evaluate polynomial A(x) = $(SUB a, 0) + $(SUB a, 1)x + $(SUB a, 2)$(POWER x,2) +
 *                          $(SUB a,3)$(POWER x,3); ...
 *
 * Uses Horner's rule A(x) = $(SUB a, 0) + x($(SUB a, 1) + x($(SUB a, 2) +
 *                         x($(SUB a, 3) + ...)))
 * Params:
 *      x =     the value to evaluate.
 *      A =     array of coefficients $(SUB a, 0), $(SUB a, 1), etc.
 */
Unqual!(CommonType!(T1, T2)) poly(T1, T2)(T1 x, in T2[] A) @trusted pure nothrow @nogc
if (isFloatingPoint!T1 && isFloatingPoint!T2)
in
{
    assert(A.length > 0);
}
do
{
    static if (is(immutable T2 == immutable real))
    {
        return polyImpl(x, A);
    }
    else
    {
        return polyImplBase(x, A);
    }
}

/// ditto
Unqual!(CommonType!(T1, T2)) poly(T1, T2, int N)(T1 x, ref const T2[N] A) @safe pure nothrow @nogc
if (isFloatingPoint!T1 && isFloatingPoint!T2 && N > 0 && N <= 10)
{
    // statically unrolled version for up to 10 coefficients
    typeof(return) r = A[N - 1];
    static foreach (i; 1 .. N)
    {
        r *= x;
        r += A[N - 1 - i];
    }
    return r;
}

///
@safe nothrow @nogc unittest
{
    real x = 3.1L;
    static real[] pp = [56.1L, 32.7L, 6];

    assert(poly(x, pp) == (56.1L + (32.7L + 6.0L * x) * x));
}

@safe nothrow @nogc unittest
{
    double x = 3.1;
    static double[] pp = [56.1, 32.7, 6];
    double y = x;
    y *= 6.0;
    y += 32.7;
    y *= x;
    y += 56.1;
    assert(poly(x, pp) == y);
}

@safe unittest
{
    static assert(poly(3.0, [1.0, 2.0, 3.0]) == 34);
}

private Unqual!(CommonType!(T1, T2)) polyImplBase(T1, T2)(T1 x, in T2[] A) @trusted pure nothrow @nogc
if (isFloatingPoint!T1 && isFloatingPoint!T2)
{
    ptrdiff_t i = A.length - 1;
    typeof(return) r = A[i];
    while (--i >= 0)
    {
        r *= x;
        r += A[i];
    }
    return r;
}

version (linux)             version = GenericPosixVersion;
else version (FreeBSD)      version = GenericPosixVersion;
else version (OpenBSD)      version = GenericPosixVersion;
else version (Solaris)      version = GenericPosixVersion;
else version (DragonFlyBSD) version = GenericPosixVersion;

private real polyImpl(real x, in real[] A) @trusted pure nothrow @nogc
{
    version (D_InlineAsm_X86)
    {
        if (__ctfe)
        {
            return polyImplBase(x, A);
        }
        version (Windows)
        {
        // BUG: This code assumes a frame pointer in EBP.
            asm pure nothrow @nogc // assembler by W. Bright
            {
                // EDX = (A.length - 1) * real.sizeof
                mov     ECX,A[EBP]              ; // ECX = A.length
                dec     ECX                     ;
                lea     EDX,[ECX][ECX*8]        ;
                add     EDX,ECX                 ;
                add     EDX,A+4[EBP]            ;
                fld     real ptr [EDX]          ; // ST0 = coeff[ECX]
                jecxz   return_ST               ;
                fld     x[EBP]                  ; // ST0 = x
                fxch    ST(1)                   ; // ST1 = x, ST0 = r
                align   4                       ;
        L2:     fmul    ST,ST(1)                ; // r *= x
                fld     real ptr -10[EDX]       ;
                sub     EDX,10                  ; // deg--
                faddp   ST(1),ST                ;
                dec     ECX                     ;
                jne     L2                      ;
                fxch    ST(1)                   ; // ST1 = r, ST0 = x
                fstp    ST(0)                   ; // dump x
                align   4                       ;
        return_ST:                              ;
            }
        }
        else version (GenericPosixVersion)
        {
            asm pure nothrow @nogc // assembler by W. Bright
            {
                // EDX = (A.length - 1) * real.sizeof
                mov     ECX,A[EBP]              ; // ECX = A.length
                dec     ECX                     ;
                lea     EDX,[ECX*8]             ;
                lea     EDX,[EDX][ECX*4]        ;
                add     EDX,A+4[EBP]            ;
                fld     real ptr [EDX]          ; // ST0 = coeff[ECX]
                jecxz   return_ST               ;
                fld     x[EBP]                  ; // ST0 = x
                fxch    ST(1)                   ; // ST1 = x, ST0 = r
                align   4                       ;
        L2:     fmul    ST,ST(1)                ; // r *= x
                fld     real ptr -12[EDX]       ;
                sub     EDX,12                  ; // deg--
                faddp   ST(1),ST                ;
                dec     ECX                     ;
                jne     L2                      ;
                fxch    ST(1)                   ; // ST1 = r, ST0 = x
                fstp    ST(0)                   ; // dump x
                align   4                       ;
        return_ST:                              ;
            }
        }
        else version (OSX)
        {
            asm pure nothrow @nogc // assembler by W. Bright
            {
                // EDX = (A.length - 1) * real.sizeof
                mov     ECX,A[EBP]              ; // ECX = A.length
                dec     ECX                     ;
                lea     EDX,[ECX*8]             ;
                add     EDX,EDX                 ;
                add     EDX,A+4[EBP]            ;
                fld     real ptr [EDX]          ; // ST0 = coeff[ECX]
                jecxz   return_ST               ;
                fld     x[EBP]                  ; // ST0 = x
                fxch    ST(1)                   ; // ST1 = x, ST0 = r
                align   4                       ;
        L2:     fmul    ST,ST(1)                ; // r *= x
                fld     real ptr -16[EDX]       ;
                sub     EDX,16                  ; // deg--
                faddp   ST(1),ST                ;
                dec     ECX                     ;
                jne     L2                      ;
                fxch    ST(1)                   ; // ST1 = r, ST0 = x
                fstp    ST(0)                   ; // dump x
                align   4                       ;
        return_ST:                              ;
            }
        }
        else
        {
            static assert(0);
        }
    }
    else
    {
        return polyImplBase(x, A);
    }
}

/**
 * Gives the next power of two after `val`. `T` can be any built-in
 * numerical type.
 *
 * If the operation would lead to an over/underflow, this function will
 * return `0`.
 *
 * Params:
 *     val = any number
 *
 * Returns:
 *     the next power of two after `val`
 */
T nextPow2(T)(const T val)
if (isIntegral!T)
{
    return powIntegralImpl!(PowType.ceil)(val);
}

/// ditto
T nextPow2(T)(const T val)
if (isFloatingPoint!T)
{
    return powFloatingPointImpl!(PowType.ceil)(val);
}

///
@safe @nogc pure nothrow unittest
{
    assert(nextPow2(2) == 4);
    assert(nextPow2(10) == 16);
    assert(nextPow2(4000) == 4096);

    assert(nextPow2(-2) == -4);
    assert(nextPow2(-10) == -16);

    assert(nextPow2(uint.max) == 0);
    assert(nextPow2(uint.min) == 0);
    assert(nextPow2(size_t.max) == 0);
    assert(nextPow2(size_t.min) == 0);

    assert(nextPow2(int.max) == 0);
    assert(nextPow2(int.min) == 0);
    assert(nextPow2(long.max) == 0);
    assert(nextPow2(long.min) == 0);
}

///
@safe @nogc pure nothrow unittest
{
    assert(nextPow2(2.1) == 4.0);
    assert(nextPow2(-2.0) == -4.0);
    assert(nextPow2(0.25) == 0.5);
    assert(nextPow2(-4.0) == -8.0);

    assert(nextPow2(double.max) == 0.0);
    assert(nextPow2(double.infinity) == double.infinity);
}

@safe @nogc pure nothrow unittest
{
    assert(nextPow2(ubyte(2)) == 4);
    assert(nextPow2(ubyte(10)) == 16);

    assert(nextPow2(byte(2)) == 4);
    assert(nextPow2(byte(10)) == 16);

    assert(nextPow2(short(2)) == 4);
    assert(nextPow2(short(10)) == 16);
    assert(nextPow2(short(4000)) == 4096);

    assert(nextPow2(ushort(2)) == 4);
    assert(nextPow2(ushort(10)) == 16);
    assert(nextPow2(ushort(4000)) == 4096);
}

@safe @nogc pure nothrow unittest
{
    foreach (ulong i; 1 .. 62)
    {
        assert(nextPow2(1UL << i) == 2UL << i);
        assert(nextPow2((1UL << i) - 1) == 1UL << i);
        assert(nextPow2((1UL << i) + 1) == 2UL << i);
        assert(nextPow2((1UL << i) + (1UL<<(i-1))) == 2UL << i);
    }
}

@safe @nogc pure nothrow unittest
{
    import std.math.traits : isNaN;
    import std.meta : AliasSeq;

    static foreach (T; AliasSeq!(float, double, real))
    {{
        enum T subNormal = T.min_normal / 2;

        static if (subNormal) assert(nextPow2(subNormal) == T.min_normal);

        assert(nextPow2(T(0.0)) == 0.0);

        assert(nextPow2(T(2.0)) == 4.0);
        assert(nextPow2(T(2.1)) == 4.0);
        assert(nextPow2(T(3.1)) == 4.0);
        assert(nextPow2(T(4.0)) == 8.0);
        assert(nextPow2(T(0.25)) == 0.5);

        assert(nextPow2(T(-2.0)) == -4.0);
        assert(nextPow2(T(-2.1)) == -4.0);
        assert(nextPow2(T(-3.1)) == -4.0);
        assert(nextPow2(T(-4.0)) == -8.0);
        assert(nextPow2(T(-0.25)) == -0.5);

        assert(nextPow2(T.max) == 0);
        assert(nextPow2(-T.max) == 0);

        assert(nextPow2(T.infinity) == T.infinity);
        assert(nextPow2(T.init).isNaN);
    }}
}

// https://issues.dlang.org/show_bug.cgi?id=15973
@safe @nogc pure nothrow unittest
{
    assert(nextPow2(uint.max / 2) == uint.max / 2 + 1);
    assert(nextPow2(uint.max / 2 + 2) == 0);
    assert(nextPow2(int.max / 2) == int.max / 2 + 1);
    assert(nextPow2(int.max / 2 + 2) == 0);
    assert(nextPow2(int.min + 1) == int.min);
}

/**
 * Gives the last power of two before `val`. $(T) can be any built-in
 * numerical type.
 *
 * Params:
 *     val = any number
 *
 * Returns:
 *     the last power of two before `val`
 */
T truncPow2(T)(const T val)
if (isIntegral!T)
{
    return powIntegralImpl!(PowType.floor)(val);
}

/// ditto
T truncPow2(T)(const T val)
if (isFloatingPoint!T)
{
    return powFloatingPointImpl!(PowType.floor)(val);
}

///
@safe @nogc pure nothrow unittest
{
    assert(truncPow2(3) == 2);
    assert(truncPow2(4) == 4);
    assert(truncPow2(10) == 8);
    assert(truncPow2(4000) == 2048);

    assert(truncPow2(-5) == -4);
    assert(truncPow2(-20) == -16);

    assert(truncPow2(uint.max) == int.max + 1);
    assert(truncPow2(uint.min) == 0);
    assert(truncPow2(ulong.max) == long.max + 1);
    assert(truncPow2(ulong.min) == 0);

    assert(truncPow2(int.max) == (int.max / 2) + 1);
    assert(truncPow2(int.min) == int.min);
    assert(truncPow2(long.max) == (long.max / 2) + 1);
    assert(truncPow2(long.min) == long.min);
}

///
@safe @nogc pure nothrow unittest
{
    assert(truncPow2(2.1) == 2.0);
    assert(truncPow2(7.0) == 4.0);
    assert(truncPow2(-1.9) == -1.0);
    assert(truncPow2(0.24) == 0.125);
    assert(truncPow2(-7.0) == -4.0);

    assert(truncPow2(double.infinity) == double.infinity);
}

@safe @nogc pure nothrow unittest
{
    assert(truncPow2(ubyte(3)) == 2);
    assert(truncPow2(ubyte(4)) == 4);
    assert(truncPow2(ubyte(10)) == 8);

    assert(truncPow2(byte(3)) == 2);
    assert(truncPow2(byte(4)) == 4);
    assert(truncPow2(byte(10)) == 8);

    assert(truncPow2(ushort(3)) == 2);
    assert(truncPow2(ushort(4)) == 4);
    assert(truncPow2(ushort(10)) == 8);
    assert(truncPow2(ushort(4000)) == 2048);

    assert(truncPow2(short(3)) == 2);
    assert(truncPow2(short(4)) == 4);
    assert(truncPow2(short(10)) == 8);
    assert(truncPow2(short(4000)) == 2048);
}

@safe @nogc pure nothrow unittest
{
    foreach (ulong i; 1 .. 62)
    {
        assert(truncPow2(2UL << i) == 2UL << i);
        assert(truncPow2((2UL << i) + 1) == 2UL << i);
        assert(truncPow2((2UL << i) - 1) == 1UL << i);
        assert(truncPow2((2UL << i) - (2UL<<(i-1))) == 1UL << i);
    }
}

@safe @nogc pure nothrow unittest
{
    import std.math.traits : isNaN;
    import std.meta : AliasSeq;

    static foreach (T; AliasSeq!(float, double, real))
    {
        assert(truncPow2(T(0.0)) == 0.0);

        assert(truncPow2(T(4.0)) == 4.0);
        assert(truncPow2(T(2.1)) == 2.0);
        assert(truncPow2(T(3.5)) == 2.0);
        assert(truncPow2(T(7.0)) == 4.0);
        assert(truncPow2(T(0.24)) == 0.125);

        assert(truncPow2(T(-2.0)) == -2.0);
        assert(truncPow2(T(-2.1)) == -2.0);
        assert(truncPow2(T(-3.1)) == -2.0);
        assert(truncPow2(T(-7.0)) == -4.0);
        assert(truncPow2(T(-0.24)) == -0.125);

        assert(truncPow2(T.infinity) == T.infinity);
        assert(truncPow2(T.init).isNaN);
    }
}

private enum PowType
{
    floor,
    ceil
}

pragma(inline, true)
private T powIntegralImpl(PowType type, T)(T val)
{
    import core.bitop : bsr;

    if (val == 0 || (type == PowType.ceil && (val > T.max / 2 || val == T.min)))
        return 0;
    else
    {
        static if (isSigned!T)
            return cast() cast(T) (val < 0 ? -(T(1) << bsr(0 - val) + type) : T(1) << bsr(val) + type);
        else
            return cast() cast(T) (T(1) << bsr(val) + type);
    }
}

private T powFloatingPointImpl(PowType type, T)(T x)
{
    import std.math.traits : copysign, isFinite;
    import std.math.exponential : frexp;

    if (!x.isFinite)
        return x;

    if (!x)
        return x;

    int exp;
    auto y = frexp(x, exp);

    static if (type == PowType.ceil)
        y = core.math.ldexp(cast(T) 0.5, exp + 1);
    else
        y = core.math.ldexp(cast(T) 0.5, exp);

    if (!y.isFinite)
        return cast(T) 0.0;

    y = copysign(y, x);

    return y;
}
