// Written in the D programming language.

/**
This is a submodule of $(MREF std, math).

It contains several trigonometric functions.

Copyright: Copyright The D Language Foundation 2000 - 2011.
           D implementations of tan, atan, and atan2 functions are based on the
           CEPHES math library, which is Copyright (C) 2001 Stephen L. Moshier
           $(LT)steve@moshier.net$(GT) and are incorporated herein by permission
           of the author. The author reserves the right to distribute this
           material elsewhere under different copying permissions.
           These modifications are distributed here under the following terms:
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP digitalmars.com, Walter Bright), Don Clugston,
           Conversion of CEPHES math library to D by Iain Buclaw and David Nadlinger
Source: $(PHOBOSSRC std/math/trigonometry.d)

Macros:
    TABLE_SV = <table border="1" cellpadding="4" cellspacing="0">
               <caption>Special Values</caption>
               $0</table>
    SVH = $(TR $(TH $1) $(TH $2))
    SV  = $(TR $(TD $1) $(TD $2))
    TH3 = $(TR $(TH $1) $(TH $2) $(TH $3))
    TD3 = $(TR $(TD $1) $(TD $2) $(TD $3))
    TABLE_DOMRG = <table border="1" cellpadding="4" cellspacing="0">
                  $(SVH Domain X, Range Y)
                  $(SV $1, $2)
                  </table>
    DOMAIN=$1
    RANGE=$1
    POWER = $1<sup>$2</sup>
    NAN = $(RED NAN)
    PLUSMN = &plusmn;
    INFIN = &infin;
    PLUSMNINF = &plusmn;&infin;
 */

module std.math.trigonometry;

static import core.math;

version (D_InlineAsm_X86)    version = InlineAsm_X86_Any;
version (D_InlineAsm_X86_64) version = InlineAsm_X86_Any;

version (InlineAsm_X86_Any) version = InlineAsm_X87;
version (InlineAsm_X87)
{
    static assert(real.mant_dig == 64);
    version (CRuntime_Microsoft) version = InlineAsm_X87_MSVC;
}

/***********************************
 * Returns cosine of x. x is in radians.
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                 $(TH cos(x)) $(TH invalid?))
 *      $(TR $(TD $(NAN))            $(TD $(NAN)) $(TD yes)     )
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD $(NAN)) $(TD yes)     )
 *      )
 * Bugs:
 *      Results are undefined if |x| >= $(POWER 2,64).
 */
pragma(inline, true)
real cos(real x) @safe pure nothrow @nogc { return core.math.cos(x); }
///ditto
pragma(inline, true)
double cos(double x) @safe pure nothrow @nogc { return core.math.cos(x); }
///ditto
pragma(inline, true)
float cos(float x) @safe pure nothrow @nogc { return core.math.cos(x); }

///
@safe unittest
{
    import std.math.operations : isClose;

    assert(cos(0.0) == 1.0);
    assert(cos(1.0).isClose(0.5403023059));
    assert(cos(3.0).isClose(-0.9899924966));
}

@safe unittest
{
    real function(real) pcos = &cos;
    assert(pcos != null);
}

@safe pure nothrow @nogc unittest
{
    import std.math.algebraic : fabs;

    float f = cos(-2.0f);
    assert(fabs(f - -0.416147f) < .00001);

    double d = cos(-2.0);
    assert(fabs(d - -0.416147f) < .00001);

    real r = cos(-2.0L);
    assert(fabs(r - -0.416147f) < .00001);
}

/***********************************
 * Returns $(HTTP en.wikipedia.org/wiki/Sine, sine) of x. x is in $(HTTP en.wikipedia.org/wiki/Radian, radians).
 *
 *      $(TABLE_SV
 *      $(TH3 x           ,  sin(x)      ,  invalid?)
 *      $(TD3 $(NAN)      ,  $(NAN)      ,  yes     )
 *      $(TD3 $(PLUSMN)0.0,  $(PLUSMN)0.0,  no      )
 *      $(TD3 $(PLUSMNINF),  $(NAN)      ,  yes     )
 *      )
 *
 * Params:
 *      x = angle in radians (not degrees)
 * Returns:
 *      sine of x
 * See_Also:
 *      $(MYREF cos), $(MYREF tan), $(MYREF asin)
 * Bugs:
 *      Results are undefined if |x| >= $(POWER 2,64).
 */
pragma(inline, true)
real sin(real x) @safe pure nothrow @nogc { return core.math.sin(x); }
///ditto
pragma(inline, true)
double sin(double x) @safe pure nothrow @nogc { return core.math.sin(x); }
///ditto
pragma(inline, true)
float sin(float x) @safe pure nothrow @nogc { return core.math.sin(x); }

///
@safe unittest
{
    import std.math.constants : PI;
    import std.stdio : writefln;

    void someFunc()
    {
      real x = 30.0;
      auto result = sin(x * (PI / 180)); // convert degrees to radians
      writefln("The sine of %s degrees is %s", x, result);
    }
}

@safe unittest
{
    real function(real) psin = &sin;
    assert(psin != null);
}

@safe pure nothrow @nogc unittest
{
    import std.math.algebraic : fabs;

    float f = sin(-2.0f);
    assert(fabs(f - -0.909297f) < .00001);

    double d = sin(-2.0);
    assert(fabs(d - -0.909297f) < .00001);

    real r = sin(-2.0L);
    assert(fabs(r - -0.909297f) < .00001);
}

/****************************************************************************
 * Returns tangent of x. x is in radians.
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)             $(TH tan(x))       $(TH invalid?))
 *      $(TR $(TD $(NAN))        $(TD $(NAN))       $(TD yes))
 *      $(TR $(TD $(PLUSMN)0.0)  $(TD $(PLUSMN)0.0) $(TD no))
 *      $(TR $(TD $(PLUSMNINF))  $(TD $(NAN))       $(TD yes))
 *      )
 */
pragma(inline, true)
real tan(real x) @safe pure nothrow @nogc
{
    version (InlineAsm_X87)
    {
        if (!__ctfe)
            return tanAsm(x);
    }
    return tanImpl(x);
}

/// ditto
pragma(inline, true)
double tan(double x) @safe pure nothrow @nogc { return __ctfe ? cast(double) tan(cast(real) x) : tanImpl(x); }

/// ditto
pragma(inline, true)
float tan(float x) @safe pure nothrow @nogc { return __ctfe ? cast(float) tan(cast(real) x) : tanImpl(x); }

///
@safe unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isIdentical;
    import std.math.constants : PI;
    import std.math.algebraic : sqrt;

    assert(isIdentical(tan(0.0), 0.0));
    assert(tan(PI).isClose(0, 0.0, 1e-10));
    assert(tan(PI / 3).isClose(sqrt(3.0)));
}

version (InlineAsm_X87)
private real tanAsm(real x) @trusted pure nothrow @nogc
{
    // Separating `return real.nan` from the asm block on LDC produces unintended
    // behaviour as additional instructions are generated, invalidating the asm
    // logic inside the previous block. To circumvent this, we can push rnan
    // manually by creating an immutable variable in the stack.
    immutable rnan = real.nan;

    version (X86)
    {
    asm pure nothrow @nogc
    {
        fld     x[EBP]                  ; // load theta
        fxam                            ; // test for oddball values
        fstsw   AX                      ;
        sahf                            ;
        jc      trigerr                 ; // x is NAN, infinity, or empty
                                          // 387's can handle subnormals
SC18:   fptan                           ;
        fstsw   AX                      ;
        sahf                            ;
        jnp     Clear1                  ; // C2 = 1 (x is out of range)

        // Do argument reduction to bring x into range
        fldpi                           ;
        fxch                            ;
SC17:   fprem1                          ;
        fstsw   AX                      ;
        sahf                            ;
        jp      SC17                    ;
        fstp    ST(1)                   ; // remove pi from stack
        jmp     SC18                    ;

trigerr:
        jnp     Lret                    ; // if theta is NAN, return theta
        fstp    ST(0)                   ; // dump theta
        fld     rnan                    ; // return rnan
        jmp     Lret                    ;
Clear1:
        fstp    ST(0)                   ; // dump X, which is always 1
Lret:
        ;
    }
    }
    else version (X86_64)
    {
        version (Win64)
        {
            asm pure nothrow @nogc
            {
                fld     real ptr [RCX]  ; // load theta
            }
        }
        else
        {
            asm pure nothrow @nogc
            {
                fld     x[RBP]          ; // load theta
            }
        }
    asm pure nothrow @nogc
    {
        fxam                            ; // test for oddball values
        fstsw   AX                      ;
        test    AH,1                    ;
        jnz     trigerr                 ; // x is NAN, infinity, or empty
                                          // 387's can handle subnormals
SC18:   fptan                           ;
        fstsw   AX                      ;
        test    AH,4                    ;
        jz      Clear1                  ; // C2 = 1 (x is out of range)

        // Do argument reduction to bring x into range
        fldpi                           ;
        fxch                            ;
SC17:   fprem1                          ;
        fstsw   AX                      ;
        test    AH,4                    ;
        jnz     SC17                    ;
        fstp    ST(1)                   ; // remove pi from stack
        jmp     SC18                    ;

trigerr:
        test    AH,4                    ;
        jz      Lret                    ; // if theta is NAN, return theta
        fstp    ST(0)                   ; // dump theta
        fld     rnan                    ; // return rnan
        jmp     Lret                    ;
Clear1:
        fstp    ST(0)                   ; // dump X, which is always 1
Lret:
        ;
    }
    }
    else
        static assert(0);
}

private T tanImpl(T)(T x) @safe pure nothrow @nogc
{
    import std.math.traits : floatTraits, RealFormat;
    import std.math.constants : PI, PI_4;
    import std.math.rounding : floor;
    import std.math.algebraic : poly;
    import std.math.traits : isInfinity, isNaN, signbit;

    // Coefficients for tan(x) and PI/4 split into three parts.
    enum realFormat = floatTraits!T.realFormat;
    static if (realFormat == RealFormat.ieeeQuadruple)
    {
        static immutable T[6] P = [
            2.883414728874239697964612246732416606301E10L,
            -2.307030822693734879744223131873392503321E9L,
            5.160188250214037865511600561074819366815E7L,
            -4.249691853501233575668486667664718192660E5L,
            1.272297782199996882828849455156962260810E3L,
            -9.889929415807650724957118893791829849557E-1L
        ];
        static immutable T[7] Q = [
            8.650244186622719093893836740197250197602E10L,
            -4.152206921457208101480801635640958361612E10L,
            2.758476078803232151774723646710890525496E9L,
            -5.733709132766856723608447733926138506824E7L,
            4.529422062441341616231663543669583527923E5L,
            -1.317243702830553658702531997959756728291E3L,
            1.0
        ];

        enum T P1 =
            7.853981633974483067550664827649598009884357452392578125E-1L;
        enum T P2 =
            2.8605943630549158983813312792950660807511260829685741796657E-18L;
        enum T P3 =
            2.1679525325309452561992610065108379921905808E-35L;
    }
    else static if (realFormat == RealFormat.ieeeExtended ||
                    realFormat == RealFormat.ieeeDouble)
    {
        static immutable T[3] P = [
           -1.7956525197648487798769E7L,
            1.1535166483858741613983E6L,
           -1.3093693918138377764608E4L,
        ];
        static immutable T[5] Q = [
           -5.3869575592945462988123E7L,
            2.5008380182335791583922E7L,
           -1.3208923444021096744731E6L,
            1.3681296347069295467845E4L,
            1.0000000000000000000000E0L,
        ];

        enum T P1 = 7.853981554508209228515625E-1L;
        enum T P2 = 7.946627356147928367136046290398E-9L;
        enum T P3 = 3.061616997868382943065164830688E-17L;
    }
    else static if (realFormat == RealFormat.ieeeSingle)
    {
        static immutable T[6] P = [
            3.33331568548E-1,
            1.33387994085E-1,
            5.34112807005E-2,
            2.44301354525E-2,
            3.11992232697E-3,
            9.38540185543E-3,
        ];

        enum T P1 = 0.78515625;
        enum T P2 = 2.4187564849853515625E-4;
        enum T P3 = 3.77489497744594108E-8;
    }
    else
        static assert(0, "no coefficients for tan()");

    // Special cases.
    if (x == cast(T) 0.0 || isNaN(x))
        return x;
    if (isInfinity(x))
        return T.nan;

    // Make argument positive but save the sign.
    bool sign = false;
    if (signbit(x))
    {
        sign = true;
        x = -x;
    }

    // Compute x mod PI/4.
    static if (realFormat == RealFormat.ieeeSingle)
    {
        enum T FOPI = 4 / PI;
        int j = cast(int) (FOPI * x);
        T y = j;
        T z;
    }
    else
    {
        T y = floor(x / cast(T) PI_4);
        // Strip high bits of integer part.
        enum T highBitsFactor = (realFormat == RealFormat.ieeeDouble ? 0x1p3 : 0x1p4);
        enum T highBitsInv = 1.0 / highBitsFactor;
        T z = y * highBitsInv;
        // Compute y - 2^numHighBits * (y / 2^numHighBits).
        z = y - highBitsFactor * floor(z);

        // Integer and fraction part modulo one octant.
        int j = cast(int)(z);
    }

    // Map zeros and singularities to origin.
    if (j & 1)
    {
        j += 1;
        y += cast(T) 1.0;
    }

    z = ((x - y * P1) - y * P2) - y * P3;
    const T zz = z * z;

    enum T zzThreshold = (realFormat == RealFormat.ieeeSingle ? 1.0e-4L :
                          realFormat == RealFormat.ieeeDouble ? 1.0e-14L : 1.0e-20L);
    if (zz > zzThreshold)
    {
        static if (realFormat == RealFormat.ieeeSingle)
            y = z + z * (zz * poly(zz, P));
        else
            y = z + z * (zz * poly(zz, P) / poly(zz, Q));
    }
    else
        y = z;

    if (j & 2)
        y = (cast(T) -1.0) / y;

    return (sign) ? -y : y;
}

@safe @nogc nothrow unittest
{
    static void testTan(T)()
    {
        import std.math.operations : CommonDefaultFor, isClose, NaN;
        import std.math.traits : isIdentical, isNaN;
        import std.math.constants : PI, PI_4;

        // ±0
        const T zero = 0.0;
        assert(isIdentical(tan(zero), zero));
        assert(isIdentical(tan(-zero), -zero));
        // ±∞
        const T inf = T.infinity;
        assert(isNaN(tan(inf)));
        assert(isNaN(tan(-inf)));
        // NaN
        const T specialNaN = NaN(0x0123L);
        assert(isIdentical(tan(specialNaN), specialNaN));

        static immutable T[2][] vals =
        [
            // angle, tan
            [   .5,  .546302489843790513255L],
            [   1,   1.55740772465490223050L],
            [   1.5, 14.1014199471717193876L],
            [   2,  -2.18503986326151899164L],
            [   2.5,-.747022297238660279355L],
            [   3,  -.142546543074277805295L],
            [   3.5, .374585640158594666330L],
            [   4,   1.15782128234957758313L],
            [   4.5, 4.63733205455118446831L],
            [   5,  -3.38051500624658563698L],
            [   5.5,-.995584052213885017701L],
            [   6,  -.291006191384749157053L],
            [   6.5, .220277200345896811825L],
            [   10,  .648360827459086671259L],

            // special angles
            [   PI_4,   1],
            //[   PI_2,   T.infinity], // PI_2 is not _exactly_ pi/2.
            [   3*PI_4, -1],
            [   PI,     0],
            [   5*PI_4, 1],
            //[   3*PI_2, -T.infinity],
            [   7*PI_4, -1],
            [   2*PI,   0],
         ];

        foreach (ref val; vals)
        {
            T x = val[0];
            T r = val[1];
            T t = tan(x);

            //printf("tan(%Lg) = %Lg, should be %Lg\n", cast(real) x, cast(real) t, cast(real) r);
            assert(isClose(r, t, CommonDefaultFor!(T,T), CommonDefaultFor!(T,T)));

            x = -x;
            r = -r;
            t = tan(x);
            //printf("tan(%Lg) = %Lg, should be %Lg\n", cast(real) x, cast(real) t, cast(real) r);
            assert(isClose(r, t, CommonDefaultFor!(T,T), CommonDefaultFor!(T,T)));
        }
    }

    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(real, double, float))
        testTan!T();

    import std.math.operations : isClose;
    import std.math.constants : PI;
    import std.math.algebraic : sqrt;
    assert(isClose(tan(PI / 3), sqrt(3.0L), real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}

@safe pure nothrow @nogc unittest
{
    import std.math.algebraic : fabs;
    import std.math.traits : isNaN;

    float f = tan(-2.0f);
    assert(fabs(f - 2.18504f) < .00001);

    double d = tan(-2.0);
    assert(fabs(d - 2.18504f) < .00001);

    real r = tan(-2.0L);
    assert(fabs(r - 2.18504f) < .00001);

    // Verify correct behavior for large inputs
    assert(!isNaN(tan(0x1p63)));
    assert(!isNaN(tan(-0x1p63)));
    static if (real.mant_dig >= 64)
    {
        assert(!isNaN(tan(0x1p300L)));
        assert(!isNaN(tan(-0x1p300L)));
    }
}

/***************
 * Calculates the arc cosine of x,
 * returning a value ranging from 0 to $(PI).
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)         $(TH acos(x)) $(TH invalid?))
 *      $(TR $(TD $(GT)1.0)  $(TD $(NAN))  $(TD yes))
 *      $(TR $(TD $(LT)-1.0) $(TD $(NAN))  $(TD yes))
 *      $(TR $(TD $(NAN))    $(TD $(NAN))  $(TD yes))
 *  )
 */
real acos(real x) @safe pure nothrow @nogc
{
    import core.math : sqrt;

    return atan2(sqrt(1-x*x), x);
}

/// ditto
double acos(double x) @safe pure nothrow @nogc { return acos(cast(real) x); }

/// ditto
float acos(float x) @safe pure nothrow @nogc  { return acos(cast(real) x); }

///
@safe unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isNaN;
    import std.math.constants : PI;

    assert(acos(0.0).isClose(1.570796327));
    assert(acos(0.5).isClose(PI / 3));
    assert(acos(PI).isNaN);
}

@safe @nogc nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;

    assert(isClose(acos(0.5), PI / 3, real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}

/***************
 * Calculates the arc sine of x,
 * returning a value ranging from -$(PI)/2 to $(PI)/2.
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)            $(TH asin(x))      $(TH invalid?))
 *      $(TR $(TD $(PLUSMN)0.0) $(TD $(PLUSMN)0.0) $(TD no))
 *      $(TR $(TD $(GT)1.0)     $(TD $(NAN))       $(TD yes))
 *      $(TR $(TD $(LT)-1.0)    $(TD $(NAN))       $(TD yes))
 *  )
 */
real asin(real x) @safe pure nothrow @nogc
{
    import core.math : sqrt;

    return atan2(x, sqrt(1-x*x));
}

/// ditto
double asin(double x) @safe pure nothrow @nogc { return asin(cast(real) x); }

/// ditto
float asin(float x) @safe pure nothrow @nogc  { return asin(cast(real) x); }

///
@safe unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isIdentical, isNaN;
    import std.math.constants : PI;

    assert(isIdentical(asin(0.0), 0.0));
    assert(asin(0.5).isClose(PI / 6));
    assert(asin(PI).isNaN);
}

@safe @nogc nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;

    assert(isClose(asin(0.5), PI / 6, real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}

/***************
 * Calculates the arc tangent of x,
 * returning a value ranging from -$(PI)/2 to $(PI)/2.
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                 $(TH atan(x))      $(TH invalid?))
 *      $(TR $(TD $(PLUSMN)0.0)      $(TD $(PLUSMN)0.0) $(TD no))
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD $(NAN))       $(TD yes))
 *  )
 */
pragma(inline, true)
real atan(real x) @safe pure nothrow @nogc
{
    version (InlineAsm_X87)
    {
        if (!__ctfe)
            return atan2Asm(x, 1.0L);
    }
    return atanImpl(x);
}

/// ditto
pragma(inline, true)
double atan(double x) @safe pure nothrow @nogc { return __ctfe ? cast(double) atan(cast(real) x) : atanImpl(x); }

/// ditto
pragma(inline, true)
float atan(float x) @safe pure nothrow @nogc { return __ctfe ? cast(float) atan(cast(real) x) : atanImpl(x); }

///
@safe unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isIdentical;
    import std.math.constants : PI;
    import std.math.algebraic : sqrt;

    assert(isIdentical(atan(0.0), 0.0));
    assert(atan(sqrt(3.0)).isClose(PI / 3));
}

private T atanImpl(T)(T x) @safe pure nothrow @nogc
{
    import std.math.traits : floatTraits, RealFormat, copysign, isInfinity, signbit;
    import std.math.constants : PI_2, PI_4;
    import std.math.algebraic : poly;

    // Coefficients for atan(x)
    enum realFormat = floatTraits!T.realFormat;
    static if (realFormat == RealFormat.ieeeQuadruple)
    {
        static immutable T[9] P = [
            -6.880597774405940432145577545328795037141E2L,
            -2.514829758941713674909996882101723647996E3L,
            -3.696264445691821235400930243493001671932E3L,
            -2.792272753241044941703278827346430350236E3L,
            -1.148164399808514330375280133523543970854E3L,
            -2.497759878476618348858065206895055957104E2L,
            -2.548067867495502632615671450650071218995E1L,
            -8.768423468036849091777415076702113400070E-1L,
            -6.635810778635296712545011270011752799963E-4L
        ];
        static immutable T[9] Q = [
            2.064179332321782129643673263598686441900E3L,
            8.782996876218210302516194604424986107121E3L,
            1.547394317752562611786521896296215170819E4L,
            1.458510242529987155225086911411015961174E4L,
            7.928572347062145288093560392463784743935E3L,
            2.494680540950601626662048893678584497900E3L,
            4.308348370818927353321556740027020068897E2L,
            3.566239794444800849656497338030115886153E1L,
            1.0
        ];
    }
    else static if (realFormat == RealFormat.ieeeExtended)
    {
        static immutable T[5] P = [
           -5.0894116899623603312185E1L,
           -9.9988763777265819915721E1L,
           -6.3976888655834347413154E1L,
           -1.4683508633175792446076E1L,
           -8.6863818178092187535440E-1L,
        ];
        static immutable T[6] Q = [
            1.5268235069887081006606E2L,
            3.9157570175111990631099E2L,
            3.6144079386152023162701E2L,
            1.4399096122250781605352E2L,
            2.2981886733594175366172E1L,
            1.0000000000000000000000E0L,
        ];
    }
    else static if (realFormat == RealFormat.ieeeDouble)
    {
        static immutable T[5] P = [
           -6.485021904942025371773E1L,
           -1.228866684490136173410E2L,
           -7.500855792314704667340E1L,
           -1.615753718733365076637E1L,
           -8.750608600031904122785E-1L,
        ];
        static immutable T[6] Q = [
            1.945506571482613964425E2L,
            4.853903996359136964868E2L,
            4.328810604912902668951E2L,
            1.650270098316988542046E2L,
            2.485846490142306297962E1L,
            1.000000000000000000000E0L,
        ];

        enum T MOREBITS = 6.123233995736765886130E-17L;
    }
    else static if (realFormat == RealFormat.ieeeSingle)
    {
        static immutable T[4] P = [
           -3.33329491539E-1,
            1.99777106478E-1,
           -1.38776856032E-1,
            8.05374449538E-2,
        ];
    }
    else
        static assert(0, "no coefficients for atan()");

    // tan(PI/8)
    enum T TAN_PI_8 = 0.414213562373095048801688724209698078569672L;
    // tan(3 * PI/8)
    enum T TAN3_PI_8 = 2.414213562373095048801688724209698078569672L;

    // Special cases.
    if (x == cast(T) 0.0)
        return x;
    if (isInfinity(x))
        return copysign(cast(T) PI_2, x);

    // Make argument positive but save the sign.
    bool sign = false;
    if (signbit(x))
    {
        sign = true;
        x = -x;
    }

    static if (realFormat == RealFormat.ieeeDouble) // special case for double precision
    {
        short flag = 0;
        T y;
        if (x > TAN3_PI_8)
        {
            y = PI_2;
            flag = 1;
            x = -(1.0 / x);
        }
        else if (x <= 0.66)
        {
            y = 0.0;
        }
        else
        {
            y = PI_4;
            flag = 2;
            x = (x - 1.0)/(x + 1.0);
        }

        T z = x * x;
        z = z * poly(z, P) / poly(z, Q);
        z = x * z + x;
        if (flag == 2)
            z += 0.5 * MOREBITS;
        else if (flag == 1)
            z += MOREBITS;
        y = y + z;
    }
    else
    {
        // Range reduction.
        T y;
        if (x > TAN3_PI_8)
        {
            y = PI_2;
            x = -((cast(T) 1.0) / x);
        }
        else if (x > TAN_PI_8)
        {
            y = PI_4;
            x = (x - cast(T) 1.0)/(x + cast(T) 1.0);
        }
        else
            y = 0.0;

        // Rational form in x^^2.
        const T z = x * x;
        static if (realFormat == RealFormat.ieeeSingle)
            y += poly(z, P) * z * x + x;
        else
            y = y + (poly(z, P) / poly(z, Q)) * z * x + x;
    }

    return (sign) ? -y : y;
}

@safe @nogc nothrow unittest
{
    static void testAtan(T)()
    {
        import std.math.operations : CommonDefaultFor, isClose, NaN;
        import std.math.traits : isIdentical;
        import std.math.constants : PI_2, PI_4;

        // ±0
        const T zero = 0.0;
        assert(isIdentical(atan(zero), zero));
        assert(isIdentical(atan(-zero), -zero));
        // ±∞
        const T inf = T.infinity;
        assert(isClose(atan(inf), cast(T) PI_2));
        assert(isClose(atan(-inf), cast(T) -PI_2));
        // NaN
        const T specialNaN = NaN(0x0123L);
        assert(isIdentical(atan(specialNaN), specialNaN));

        static immutable T[2][] vals =
        [
            // x, atan(x)
            [ 0.25, 0.244978663126864154172L ],
            [ 0.5,  0.463647609000806116214L ],
            [ 1,    PI_4                     ],
            [ 1.5,  0.982793723247329067985L ],
            [ 10,   1.471127674303734591852L ],
        ];

        foreach (ref val; vals)
        {
            T x = val[0];
            T r = val[1];
            T a = atan(x);

            //printf("atan(%Lg) = %Lg, should be %Lg\n", cast(real) x, cast(real) a, cast(real) r);
            assert(isClose(r, a, CommonDefaultFor!(T,T), CommonDefaultFor!(T,T)));

            x = -x;
            r = -r;
            a = atan(x);
            //printf("atan(%Lg) = %Lg, should be %Lg\n", cast(real) x, cast(real) a, cast(real) r);
            assert(isClose(r, a, CommonDefaultFor!(T,T), CommonDefaultFor!(T,T)));
        }
    }

    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(real, double, float))
        testAtan!T();

    import std.math.operations : isClose;
    import std.math.algebraic : sqrt;
    import std.math.constants : PI;
    assert(isClose(atan(sqrt(3.0L)), PI / 3, real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}

/***************
 * Calculates the arc tangent of y / x,
 * returning a value ranging from -$(PI) to $(PI).
 *
 *      $(TABLE_SV
 *      $(TR $(TH y)                 $(TH x)            $(TH atan(y, x)))
 *      $(TR $(TD $(NAN))            $(TD anything)     $(TD $(NAN)) )
 *      $(TR $(TD anything)          $(TD $(NAN))       $(TD $(NAN)) )
 *      $(TR $(TD $(PLUSMN)0.0)      $(TD $(GT)0.0)     $(TD $(PLUSMN)0.0) )
 *      $(TR $(TD $(PLUSMN)0.0)      $(TD +0.0)         $(TD $(PLUSMN)0.0) )
 *      $(TR $(TD $(PLUSMN)0.0)      $(TD $(LT)0.0)     $(TD $(PLUSMN)$(PI)))
 *      $(TR $(TD $(PLUSMN)0.0)      $(TD -0.0)         $(TD $(PLUSMN)$(PI)))
 *      $(TR $(TD $(GT)0.0)          $(TD $(PLUSMN)0.0) $(TD $(PI)/2) )
 *      $(TR $(TD $(LT)0.0)          $(TD $(PLUSMN)0.0) $(TD -$(PI)/2) )
 *      $(TR $(TD $(GT)0.0)          $(TD $(INFIN))     $(TD $(PLUSMN)0.0) )
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD anything)     $(TD $(PLUSMN)$(PI)/2))
 *      $(TR $(TD $(GT)0.0)          $(TD -$(INFIN))    $(TD $(PLUSMN)$(PI)) )
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD $(INFIN))     $(TD $(PLUSMN)$(PI)/4))
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD -$(INFIN))    $(TD $(PLUSMN)3$(PI)/4))
 *      )
 */
pragma(inline, true)
real atan2(real y, real x) @trusted pure nothrow @nogc // TODO: @safe
{
    version (InlineAsm_X87)
    {
        if (!__ctfe)
            return atan2Asm(y, x);
    }
    return atan2Impl(y, x);
}

/// ditto
pragma(inline, true)
double atan2(double y, double x) @safe pure nothrow @nogc
{
    return __ctfe ? cast(double) atan2(cast(real) y, cast(real) x) : atan2Impl(y, x);
}

/// ditto
pragma(inline, true)
float atan2(float y, float x) @safe pure nothrow @nogc
{
    return __ctfe ? cast(float) atan2(cast(real) y, cast(real) x) : atan2Impl(y, x);
}

///
@safe unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    import std.math.algebraic : sqrt;

    assert(atan2(1.0, sqrt(3.0)).isClose(PI / 6));
}

version (InlineAsm_X87)
private real atan2Asm(real y, real x) @trusted pure nothrow @nogc
{
    version (Win64)
    {
        asm pure nothrow @nogc {
            naked;
            fld real ptr [RDX]; // y
            fld real ptr [RCX]; // x
            fpatan;
            ret;
        }
    }
    else
    {
        asm pure nothrow @nogc {
            fld y;
            fld x;
            fpatan;
        }
    }
}

private T atan2Impl(T)(T y, T x) @safe pure nothrow @nogc
{
    import std.math.traits : copysign, isInfinity, isNaN, signbit;
    import std.math.constants : PI, PI_2, PI_4;

    // Special cases.
    if (isNaN(x) || isNaN(y))
        return T.nan;
    if (y == cast(T) 0.0)
    {
        if (x >= 0 && !signbit(x))
            return copysign(0, y);
        else
            return copysign(cast(T) PI, y);
    }
    if (x == cast(T) 0.0)
        return copysign(cast(T) PI_2, y);
    if (isInfinity(x))
    {
        if (signbit(x))
        {
            if (isInfinity(y))
                return copysign(3 * cast(T) PI_4, y);
            else
                return copysign(cast(T) PI, y);
        }
        else
        {
            if (isInfinity(y))
                return copysign(cast(T) PI_4, y);
            else
                return copysign(cast(T) 0.0, y);
        }
    }
    if (isInfinity(y))
        return copysign(cast(T) PI_2, y);

    // Call atan and determine the quadrant.
    T z = atan(y / x);

    if (signbit(x))
    {
        if (signbit(y))
            z = z - cast(T) PI;
        else
            z = z + cast(T) PI;
    }

    if (z == cast(T) 0.0)
        return copysign(z, y);

    return z;
}

@safe @nogc nothrow unittest
{
    static void testAtan2(T)()
    {
        import std.math.operations : isClose;
        import std.math.traits : isIdentical, isNaN;
        import std.math.constants : PI, PI_2, PI_4;

        // NaN
        const T nan = T.nan;
        assert(isNaN(atan2(nan, cast(T) 1)));
        assert(isNaN(atan2(cast(T) 1, nan)));

        const T inf = T.infinity;
        static immutable T[3][] vals =
        [
            // y, x, atan2(y, x)

            // ±0
            [  0.0,  1.0,  0.0 ],
            [ -0.0,  1.0, -0.0 ],
            [  0.0,  0.0,  0.0 ],
            [ -0.0,  0.0, -0.0 ],
            [  0.0, -1.0,  PI ],
            [ -0.0, -1.0, -PI ],
            [  0.0, -0.0,  PI ],
            [ -0.0, -0.0, -PI ],
            [  1.0,  0.0,  PI_2 ],
            [  1.0, -0.0,  PI_2 ],
            [ -1.0,  0.0, -PI_2 ],
            [ -1.0, -0.0, -PI_2 ],

            // ±∞
            [  1.0,  inf,  0.0 ],
            [ -1.0,  inf, -0.0 ],
            [  1.0, -inf,  PI ],
            [ -1.0, -inf, -PI ],
            [  inf,  1.0,  PI_2 ],
            [  inf, -1.0,  PI_2 ],
            [ -inf,  1.0, -PI_2 ],
            [ -inf, -1.0, -PI_2 ],
            [  inf,  inf,  PI_4 ],
            [ -inf,  inf, -PI_4 ],
            [  inf, -inf,  3 * PI_4 ],
            [ -inf, -inf, -3 * PI_4 ],

            [  1.0,  1.0,  PI_4 ],
            [ -2.0,  2.0, -PI_4 ],
            [  3.0, -3.0,  3 * PI_4 ],
            [ -4.0, -4.0, -3 * PI_4 ],

            [  0.75,  0.25,   1.2490457723982544258299L ],
            [ -0.5,   0.375, -0.9272952180016122324285L ],
            [  0.5,  -0.125,  1.8157749899217607734034L ],
            [ -0.75, -0.5,   -2.1587989303424641704769L ],
        ];

        foreach (ref val; vals)
        {
            const T y = val[0];
            const T x = val[1];
            const T r = val[2];
            const T a = atan2(y, x);

            //printf("atan2(%Lg, %Lg) = %Lg, should be %Lg\n", cast(real) y, cast(real) x, cast(real) a, cast(real) r);
            if (r == 0)
                assert(isIdentical(r, a)); // check sign
            else
                assert(isClose(r, a));
        }
    }

    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(real, double, float))
        testAtan2!T();

    import std.math.operations : isClose;
    import std.math.algebraic : sqrt;
    import std.math.constants : PI;
    assert(isClose(atan2(1.0L, sqrt(3.0L)), PI / 6, real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}

/***********************************
 * Calculates the hyperbolic cosine of x.
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                 $(TH cosh(x))      $(TH invalid?))
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD $(PLUSMN)0.0) $(TD no) )
 *      )
 */
real cosh(real x) @safe pure nothrow @nogc
{
    import std.math.exponential : exp;

    //  cosh = (exp(x)+exp(-x))/2.
    // The naive implementation works correctly.
    const real y = exp(x);
    return (y + 1.0/y) * 0.5;
}

/// ditto
double cosh(double x) @safe pure nothrow @nogc { return cosh(cast(real) x); }

/// ditto
float cosh(float x) @safe pure nothrow @nogc  { return cosh(cast(real) x); }

///
@safe unittest
{
    import std.math.constants : E;
    import std.math.operations : isClose;

    assert(cosh(0.0) == 1.0);
    assert(cosh(1.0).isClose((E + 1.0 / E) / 2));
}

@safe @nogc nothrow unittest
{
    import std.math.constants : E;
    import std.math.operations : isClose;

    assert(isClose(cosh(1.0), (E + 1.0 / E) / 2, real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}

/***********************************
 * Calculates the hyperbolic sine of x.
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                 $(TH sinh(x))           $(TH invalid?))
 *      $(TR $(TD $(PLUSMN)0.0)      $(TD $(PLUSMN)0.0)      $(TD no))
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD $(PLUSMN)$(INFIN)) $(TD no))
 *      )
 */
real sinh(real x) @safe pure nothrow @nogc { return _sinh(x); }

/// ditto
double sinh(double x) @safe pure nothrow @nogc { return _sinh(x); }

/// ditto
float sinh(float x) @safe pure nothrow @nogc { return _sinh(x); }

///
@safe unittest
{
    import std.math.constants : E;
    import std.math.operations : isClose;
    import std.math.traits : isIdentical;

    enum sinh1 = (E - 1.0 / E) / 2;
    import std.meta : AliasSeq;
    static foreach (F; AliasSeq!(float, double, real))
    {
        assert(isIdentical(sinh(F(0.0)), F(0.0)));
        assert(sinh(F(1.0)).isClose(F(sinh1)));
    }
}

private F _sinh(F)(F x)
{
    import std.math.traits : copysign;
    import std.math.exponential : exp, expm1;
    import core.math : fabs;
    import std.math.constants : LN2;

    //  sinh(x) =  (exp(x)-exp(-x))/2;
    // Very large arguments could cause an overflow, but
    // the maximum value of x for which exp(x) + exp(-x)) != exp(x)
    // is x = 0.5 * (real.mant_dig) * LN2. // = 22.1807 for real80.
    if (fabs(x) > F.mant_dig * F(LN2))
    {
        return copysign(F(0.5) * exp(fabs(x)), x);
    }

    const y = expm1(x);
    return F(0.5) * y / (y+1) * (y+2);
}

@safe @nogc nothrow unittest
{
    import std.math.constants : E;
    import std.math.operations : isClose;

    assert(isClose(sinh(1.0L), real((E - 1.0 / E) / 2), real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}
/***********************************
 * Calculates the hyperbolic tangent of x.
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                 $(TH tanh(x))      $(TH invalid?))
 *      $(TR $(TD $(PLUSMN)0.0)      $(TD $(PLUSMN)0.0) $(TD no) )
 *      $(TR $(TD $(PLUSMN)$(INFIN)) $(TD $(PLUSMN)1.0) $(TD no))
 *      )
 */
real tanh(real x) @safe pure nothrow @nogc { return _tanh(x); }

/// ditto
double tanh(double x) @safe pure nothrow @nogc { return _tanh(x); }

/// ditto
float tanh(float x) @safe pure nothrow @nogc { return _tanh(x); }

///
@safe unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isIdentical;

    assert(isIdentical(tanh(0.0), 0.0));
    assert(tanh(1.0).isClose(sinh(1.0) / cosh(1.0)));
}

private F _tanh(F)(F x)
{
    import std.math.traits : copysign;
    import std.math.exponential : expm1;
    import core.math : fabs;
    import std.math.constants : LN2;

    //  tanh(x) = (exp(x) - exp(-x))/(exp(x)+exp(-x))
    if (fabs(x) > F.mant_dig * F(LN2))
    {
        return copysign(1, x);
    }

    const y = expm1(2*x);
    return y / (y + 2);
}

@safe @nogc nothrow unittest
{
    import std.math.operations : isClose;

    assert(isClose(tanh(1.0L), sinh(1.0L) / cosh(1.0L), real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}

/***********************************
 * Calculates the inverse hyperbolic cosine of x.
 *
 *  Mathematically, acosh(x) = log(x + sqrt( x*x - 1))
 *
 * $(TABLE_DOMRG
 *    $(DOMAIN 1..$(INFIN)),
 *    $(RANGE  0..$(INFIN))
 * )
 *
 *  $(TABLE_SV
 *    $(SVH  x,     acosh(x) )
 *    $(SV  $(NAN), $(NAN) )
 *    $(SV  $(LT)1,     $(NAN) )
 *    $(SV  1,      0       )
 *    $(SV  +$(INFIN),+$(INFIN))
 *  )
 */
real acosh(real x) @safe pure nothrow @nogc { return _acosh(x); }

/// ditto
double acosh(double x) @safe pure nothrow @nogc { return _acosh(x); }

/// ditto
float acosh(float x) @safe pure nothrow @nogc { return _acosh(x); }

///
@safe @nogc nothrow unittest
{
    import std.math.traits : isIdentical, isNaN;

    assert(isNaN(acosh(0.9)));
    assert(isNaN(acosh(real.nan)));
    assert(isIdentical(acosh(1.0), 0.0));
    assert(acosh(real.infinity) == real.infinity);
    assert(isNaN(acosh(0.5)));
}

private F _acosh(F)(F x) @safe pure nothrow @nogc
{
    import std.math.constants : LN2;
    import std.math.exponential : log;
    import core.math : sqrt;

    if (x > 1/F.epsilon)
        return F(LN2) + log(x);
    else
        return log(x + sqrt(x*x - 1));
}

@safe @nogc nothrow unittest
{
    import std.math.operations : isClose;

    assert(isClose(acosh(cosh(3.0L)), 3.0L, real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}

/***********************************
 * Calculates the inverse hyperbolic sine of x.
 *
 *  Mathematically,
 *  ---------------
 *  asinh(x) =  log( x + sqrt( x*x + 1 )) // if x >= +0
 *  asinh(x) = -log(-x + sqrt( x*x + 1 )) // if x <= -0
 *  -------------
 *
 *    $(TABLE_SV
 *    $(SVH x,                asinh(x)       )
 *    $(SV  $(NAN),           $(NAN)         )
 *    $(SV  $(PLUSMN)0,       $(PLUSMN)0      )
 *    $(SV  $(PLUSMN)$(INFIN),$(PLUSMN)$(INFIN))
 *    )
 */
real asinh(real x) @safe pure nothrow @nogc { return _asinh(x); }

/// ditto
double asinh(double x) @safe pure nothrow @nogc { return _asinh(x); }

/// ditto
float asinh(float x) @safe pure nothrow @nogc { return _asinh(x); }

///
@safe @nogc nothrow unittest
{
    import std.math.traits : isIdentical, isNaN;

    assert(isIdentical(asinh(0.0), 0.0));
    assert(isIdentical(asinh(-0.0), -0.0));
    assert(asinh(real.infinity) == real.infinity);
    assert(asinh(-real.infinity) == -real.infinity);
    assert(isNaN(asinh(real.nan)));
}

private F _asinh(F)(F x)
{
    import std.math.traits : copysign;
    import core.math : fabs, sqrt;
    import std.math.exponential : log, log1p;
    import std.math.constants : LN2;

    return (fabs(x) > 1 / F.epsilon)
        // beyond this point, x*x + 1 == x*x
        ? copysign(F(LN2) + log(fabs(x)), x)
        // sqrt(x*x + 1) ==  1 + x * x / ( 1 + sqrt(x*x + 1) )
        : copysign(log1p(fabs(x) + x*x / (1 + sqrt(x*x + 1)) ), x);
}

@safe unittest
{
    import std.math.operations : isClose;

    assert(isClose(asinh(sinh(3.0L)), 3.0L, real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}

/***********************************
 * Calculates the inverse hyperbolic tangent of x,
 * returning a value from ranging from -1 to 1.
 *
 * Mathematically, atanh(x) = log( (1+x)/(1-x) ) / 2
 *
 * $(TABLE_DOMRG
 *    $(DOMAIN -$(INFIN)..$(INFIN)),
 *    $(RANGE  -1 .. 1)
 * )
 * $(BR)
 * $(TABLE_SV
 *    $(SVH  x,     acosh(x) )
 *    $(SV  $(NAN), $(NAN) )
 *    $(SV  $(PLUSMN)0, $(PLUSMN)0)
 *    $(SV  -$(INFIN), -0)
 * )
 */
real atanh(real x) @safe pure nothrow @nogc
{
    import std.math.exponential : log1p;

    // log( (1+x)/(1-x) ) == log ( 1 + (2*x)/(1-x) )
    return  0.5 * log1p( 2 * x / (1 - x) );
}

/// ditto
double atanh(double x) @safe pure nothrow @nogc { return atanh(cast(real) x); }

/// ditto
float atanh(float x) @safe pure nothrow @nogc { return atanh(cast(real) x); }

///
@safe @nogc nothrow unittest
{
    import std.math.traits : isIdentical, isNaN;

    assert(isIdentical(atanh(0.0), 0.0));
    assert(isIdentical(atanh(-0.0),-0.0));
    assert(isNaN(atanh(real.nan)));
    assert(isNaN(atanh(-real.infinity)));
    assert(atanh(0.0) == 0);
}

@safe unittest
{
    import std.math.operations : isClose;

    assert(isClose(atanh(tanh(0.5L)), 0.5, real.sizeof > double.sizeof ? 1e-15 : 1e-14));
}
