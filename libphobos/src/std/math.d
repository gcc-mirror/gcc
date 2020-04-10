// Written in the D programming language.

/**
 * Contains the elementary mathematical functions (powers, roots,
 * and trigonometric functions), and low-level floating-point operations.
 * Mathematical special functions are available in $(D std.mathspecial).
 *
$(SCRIPT inhibitQuickIndex = 1;)

$(DIVC quickindex,
$(BOOKTABLE ,
$(TR $(TH Category) $(TH Members) )
$(TR $(TDNW Constants) $(TD
    $(MYREF E) $(MYREF PI) $(MYREF PI_2) $(MYREF PI_4) $(MYREF M_1_PI)
    $(MYREF M_2_PI) $(MYREF M_2_SQRTPI) $(MYREF LN10) $(MYREF LN2)
    $(MYREF LOG2) $(MYREF LOG2E) $(MYREF LOG2T) $(MYREF LOG10E)
    $(MYREF SQRT2) $(MYREF SQRT1_2)
))
$(TR $(TDNW Classics) $(TD
    $(MYREF abs) $(MYREF fabs) $(MYREF sqrt) $(MYREF cbrt) $(MYREF hypot)
    $(MYREF poly) $(MYREF nextPow2) $(MYREF truncPow2)
))
$(TR $(TDNW Trigonometry) $(TD
    $(MYREF sin) $(MYREF cos) $(MYREF tan) $(MYREF asin) $(MYREF acos)
    $(MYREF atan) $(MYREF atan2) $(MYREF sinh) $(MYREF cosh) $(MYREF tanh)
    $(MYREF asinh) $(MYREF acosh) $(MYREF atanh) $(MYREF expi)
))
$(TR $(TDNW Rounding) $(TD
    $(MYREF ceil) $(MYREF floor) $(MYREF round) $(MYREF lround)
    $(MYREF trunc) $(MYREF rint) $(MYREF lrint) $(MYREF nearbyint)
    $(MYREF rndtol) $(MYREF quantize)
))
$(TR $(TDNW Exponentiation & Logarithms) $(TD
    $(MYREF pow) $(MYREF exp) $(MYREF exp2) $(MYREF expm1) $(MYREF ldexp)
    $(MYREF frexp) $(MYREF log) $(MYREF log2) $(MYREF log10) $(MYREF logb)
    $(MYREF ilogb) $(MYREF log1p) $(MYREF scalbn)
))
$(TR $(TDNW Modulus) $(TD
    $(MYREF fmod) $(MYREF modf) $(MYREF remainder)
))
$(TR $(TDNW Floating-point operations) $(TD
    $(MYREF approxEqual) $(MYREF feqrel) $(MYREF fdim) $(MYREF fmax)
    $(MYREF fmin) $(MYREF fma) $(MYREF nextDown) $(MYREF nextUp)
    $(MYREF nextafter) $(MYREF NaN) $(MYREF getNaNPayload)
    $(MYREF cmp)
))
$(TR $(TDNW Introspection) $(TD
    $(MYREF isFinite) $(MYREF isIdentical) $(MYREF isInfinity) $(MYREF isNaN)
    $(MYREF isNormal) $(MYREF isSubnormal) $(MYREF signbit) $(MYREF sgn)
    $(MYREF copysign) $(MYREF isPowerOf2)
))
$(TR $(TDNW Complex Numbers) $(TD
  $(MYREF abs) $(MYREF conj) $(MYREF sin) $(MYREF cos) $(MYREF expi)
))
$(TR $(TDNW Hardware Control) $(TD
    $(MYREF IeeeFlags) $(MYREF FloatingPointControl)
))
)
)

 * The functionality closely follows the IEEE754-2008 standard for
 * floating-point arithmetic, including the use of camelCase names rather
 * than C99-style lower case names. All of these functions behave correctly
 * when presented with an infinity or NaN.
 *
 * The following IEEE 'real' formats are currently supported:
 * $(UL
 * $(LI 64 bit Big-endian  'double' (eg PowerPC))
 * $(LI 128 bit Big-endian 'quadruple' (eg SPARC))
 * $(LI 64 bit Little-endian 'double' (eg x86-SSE2))
 * $(LI 80 bit Little-endian, with implied bit 'real80' (eg x87, Itanium))
 * $(LI 128 bit Little-endian 'quadruple' (not implemented on any known processor!))
 * $(LI Non-IEEE 128 bit Big-endian 'doubledouble' (eg PowerPC) has partial support)
 * )
 * Unlike C, there is no global 'errno' variable. Consequently, almost all of
 * these functions are pure nothrow.
 *
 * Status:
 * The semantics and names of feqrel and approxEqual will be revised.
 *
 * Macros:
 *      TABLE_SV = <table border="1" cellpadding="4" cellspacing="0">
 *              <caption>Special Values</caption>
 *              $0</table>
 *      SVH = $(TR $(TH $1) $(TH $2))
 *      SV  = $(TR $(TD $1) $(TD $2))
 *      TH3 = $(TR $(TH $1) $(TH $2) $(TH $3))
 *      TD3 = $(TR $(TD $1) $(TD $2) $(TD $3))
 *      TABLE_DOMRG = <table border="1" cellpadding="4" cellspacing="0">
 *              $(SVH Domain X, Range Y)
                $(SV $1, $2)
 *              </table>
 *      DOMAIN=$1
 *      RANGE=$1

 *      NAN = $(RED NAN)
 *      SUP = <span style="vertical-align:super;font-size:smaller">$0</span>
 *      GAMMA = &#915;
 *      THETA = &theta;
 *      INTEGRAL = &#8747;
 *      INTEGRATE = $(BIG &#8747;<sub>$(SMALL $1)</sub><sup>$2</sup>)
 *      POWER = $1<sup>$2</sup>
 *      SUB = $1<sub>$2</sub>
 *      BIGSUM = $(BIG &Sigma; <sup>$2</sup><sub>$(SMALL $1)</sub>)
 *      CHOOSE = $(BIG &#40;) <sup>$(SMALL $1)</sup><sub>$(SMALL $2)</sub> $(BIG &#41;)
 *      PLUSMN = &plusmn;
 *      INFIN = &infin;
 *      PLUSMNINF = &plusmn;&infin;
 *      PI = &pi;
 *      LT = &lt;
 *      GT = &gt;
 *      SQRT = &radic;
 *      HALF = &frac12;
 *
 * Copyright: Copyright Digital Mars 2000 - 2011.
 *            D implementations of tan, atan, atan2, exp, expm1, exp2, log, log10, log1p,
 *            log2, floor, ceil and lrint functions are based on the CEPHES math library,
 *            which is Copyright (C) 2001 Stephen L. Moshier $(LT)steve@moshier.net$(GT)
 *            and are incorporated herein by permission of the author.  The author
 *            reserves the right to distribute this material elsewhere under different
 *            copying permissions.  These modifications are distributed here under
 *            the following terms:
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright), Don Clugston,
 *            Conversion of CEPHES math library to D by Iain Buclaw and David Nadlinger
 * Source: $(PHOBOSSRC std/_math.d)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module std.math;

version (Win64)
{
    version (D_InlineAsm_X86_64)
        version = Win64_DMD_InlineAsm;
}

static import core.math;
static import core.stdc.math;
static import core.stdc.fenv;
import std.traits; // CommonType, isFloatingPoint, isIntegral, isSigned, isUnsigned, Largest, Unqual

version (LDC)
{
    import ldc.intrinsics;
}

version (DigitalMars)
{
    version = INLINE_YL2X;        // x87 has opcodes for these
}

version (X86)       version = X86_Any;
version (X86_64)    version = X86_Any;
version (PPC)       version = PPC_Any;
version (PPC64)     version = PPC_Any;
version (MIPS32)    version = MIPS_Any;
version (MIPS64)    version = MIPS_Any;
version (AArch64)   version = ARM_Any;
version (ARM)       version = ARM_Any;
version (S390)      version = IBMZ_Any;
version (SPARC)     version = SPARC_Any;
version (SPARC64)   version = SPARC_Any;
version (SystemZ)   version = IBMZ_Any;
version (RISCV32)   version = RISCV_Any;
version (RISCV64)   version = RISCV_Any;

version (D_InlineAsm_X86)
{
    version = InlineAsm_X86_Any;
}
else version (D_InlineAsm_X86_64)
{
    version = InlineAsm_X86_Any;
}

version (CRuntime_Microsoft)
{
    version (InlineAsm_X86_Any)
        version = MSVC_InlineAsm;
}

version (X86_64) version = StaticallyHaveSSE;
version (X86) version (OSX) version = StaticallyHaveSSE;

version (StaticallyHaveSSE)
{
    private enum bool haveSSE = true;
}
else version (X86)
{
    static import core.cpuid;
    private alias haveSSE = core.cpuid.sse;
}

version (D_SoftFloat)
{
    // Some soft float implementations may support IEEE floating flags.
    // The implementation here supports hardware flags only and is so currently
    // only available for supported targets.
}
else version (X86_Any)   version = IeeeFlagsSupport;
else version (PPC_Any)   version = IeeeFlagsSupport;
else version (RISCV_Any) version = IeeeFlagsSupport;
else version (MIPS_Any)  version = IeeeFlagsSupport;
else version (ARM_Any)   version = IeeeFlagsSupport;

// Struct FloatingPointControl is only available if hardware FP units are available.
version (D_HardFloat)
{
    // FloatingPointControl.clearExceptions() depends on version IeeeFlagsSupport
    version (IeeeFlagsSupport) version = FloatingPointControlSupport;
}

version (GNU)
{
    // The compiler can unexpectedly rearrange floating point operations and
    // access to the floating point status flags when optimizing. This means
    // ieeeFlags tests cannot be reliably checked in optimized code.
    // See https://github.com/ldc-developers/ldc/issues/888
}
else
{
    version = IeeeFlagsUnittest;
    version = FloatingPointControlUnittest;
}

version (unittest)
{
    import core.stdc.stdio; // : sprintf;

    static if (real.sizeof > double.sizeof)
        enum uint useDigits = 16;
    else
        enum uint useDigits = 15;

    /******************************************
     * Compare floating point numbers to n decimal digits of precision.
     * Returns:
     *  1       match
     *  0       nomatch
     */

    private bool equalsDigit(real x, real y, uint ndigits)
    {
        if (signbit(x) != signbit(y))
            return 0;

        if (isInfinity(x) && isInfinity(y))
            return 1;
        if (isInfinity(x) || isInfinity(y))
            return 0;

        if (isNaN(x) && isNaN(y))
            return 1;
        if (isNaN(x) || isNaN(y))
            return 0;

        char[30] bufx;
        char[30] bufy;
        assert(ndigits < bufx.length);

        int ix;
        int iy;
        version (CRuntime_Microsoft)
            alias real_t = double;
        else
            alias real_t = real;
        ix = sprintf(bufx.ptr, "%.*Lg", ndigits, cast(real_t) x);
        iy = sprintf(bufy.ptr, "%.*Lg", ndigits, cast(real_t) y);
        assert(ix < bufx.length && ix > 0);
        assert(ix < bufy.length && ix > 0);

        return bufx[0 .. ix] == bufy[0 .. iy];
    }
}



package:
// The following IEEE 'real' formats are currently supported.
version (LittleEndian)
{
    static assert(real.mant_dig == 53 || real.mant_dig == 64
               || real.mant_dig == 113,
      "Only 64-bit, 80-bit, and 128-bit reals"~
      " are supported for LittleEndian CPUs");
}
else
{
    static assert(real.mant_dig == 53 || real.mant_dig == 106
               || real.mant_dig == 113,
    "Only 64-bit and 128-bit reals are supported for BigEndian CPUs."~
    " double-double reals have partial support");
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
    // EXPMASK is a ushort mask to select the exponent portion (without sign)
    // EXPSHIFT is the number of bits the exponent is left-shifted by in its ushort
    // EXPBIAS is the exponent bias - 1 (exp == EXPBIAS yields ×2^-1).
    // EXPPOS_SHORT is the index of the exponent when represented as a ushort array.
    // SIGNPOS_BYTE is the index of the sign when represented as a ubyte array.
    // RECIP_EPSILON is the value such that (smallest_subnormal) * RECIP_EPSILON == T.min_normal
    enum T RECIP_EPSILON = (1/T.epsilon);
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

// Common code for math implementations.

// Helper for floor/ceil
T floorImpl(T)(const T x) @trusted pure nothrow @nogc
{
    alias F = floatTraits!(T);
    // Take care not to trigger library calls from the compiler,
    // while ensuring that we don't get defeated by some optimizers.
    union floatBits
    {
        T rv;
        ushort[T.sizeof/2] vu;

        // Other kinds of extractors for real formats.
        static if (F.realFormat == RealFormat.ieeeSingle)
            int vi;
    }
    floatBits y = void;
    y.rv = x;

    // Find the exponent (power of 2)
    // Do this by shifting the raw value so that the exponent lies in the low bits,
    // then mask out the sign bit, and subtract the bias.
    static if (F.realFormat == RealFormat.ieeeSingle)
    {
        int exp = ((y.vi >> (T.mant_dig - 1)) & 0xff) - 0x7f;
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        int exp = ((y.vu[F.EXPPOS_SHORT] >> 4) & 0x7ff) - 0x3ff;

        version (LittleEndian)
            int pos = 0;
        else
            int pos = 3;
    }
    else static if (F.realFormat == RealFormat.ieeeExtended)
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

    static if (F.realFormat == RealFormat.ieeeSingle)
    {
        if (exp < (T.mant_dig - 1))
        {
            // Clear all bits representing the fraction part.
            const uint fraction_mask = F.MANTISSAMASK_INT >> exp;

            if ((y.vi & fraction_mask) != 0)
            {
                // If 'x' is negative, then first substract 1.0 from the value.
                if (y.vi < 0)
                    y.vi += 0x00800000 >> exp;
                y.vi &= ~fraction_mask;
            }
        }
    }
    else
    {
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

public:

// Values obtained from Wolfram Alpha. 116 bits ought to be enough for anybody.
// Wolfram Alpha LLC. 2011. Wolfram|Alpha. http://www.wolframalpha.com/input/?i=e+in+base+16 (access July 6, 2011).
enum real E =          0x1.5bf0a8b1457695355fb8ac404e7a8p+1L; /** e = 2.718281... */
enum real LOG2T =      0x1.a934f0979a3715fc9257edfe9b5fbp+1L; /** $(SUB log, 2)10 = 3.321928... */
enum real LOG2E =      0x1.71547652b82fe1777d0ffda0d23a8p+0L; /** $(SUB log, 2)e = 1.442695... */
enum real LOG2 =       0x1.34413509f79fef311f12b35816f92p-2L; /** $(SUB log, 10)2 = 0.301029... */
enum real LOG10E =     0x1.bcb7b1526e50e32a6ab7555f5a67cp-2L; /** $(SUB log, 10)e = 0.434294... */
enum real LN2 =        0x1.62e42fefa39ef35793c7673007e5fp-1L; /** ln 2  = 0.693147... */
enum real LN10 =       0x1.26bb1bbb5551582dd4adac5705a61p+1L; /** ln 10 = 2.302585... */
enum real PI =         0x1.921fb54442d18469898cc51701b84p+1L; /** $(_PI) = 3.141592... */
enum real PI_2 =       PI/2;                                  /** $(PI) / 2 = 1.570796... */
enum real PI_4 =       PI/4;                                  /** $(PI) / 4 = 0.785398... */
enum real M_1_PI =     0x1.45f306dc9c882a53f84eafa3ea69cp-2L; /** 1 / $(PI) = 0.318309... */
enum real M_2_PI =     2*M_1_PI;                              /** 2 / $(PI) = 0.636619... */
enum real M_2_SQRTPI = 0x1.20dd750429b6d11ae3a914fed7fd8p+0L; /** 2 / $(SQRT)$(PI) = 1.128379... */
enum real SQRT2 =      0x1.6a09e667f3bcc908b2fb1366ea958p+0L; /** $(SQRT)2 = 1.414213... */
enum real SQRT1_2 =    SQRT2/2;                               /** $(SQRT)$(HALF) = 0.707106... */
// Note: Make sure the magic numbers in compiler backend for x87 match these.


/***********************************
 * Calculates the absolute value of a number
 *
 * Params:
 *     Num = (template parameter) type of number
 *       x = real number value
 *       z = complex number value
 *       y = imaginary number value
 *
 * Returns:
 *     The absolute value of the number.  If floating-point or integral,
 *     the return type will be the same as the input; if complex or
 *     imaginary, the returned value will be the corresponding floating
 *     point type.
 *
 * For complex numbers, abs(z) = sqrt( $(POWER z.re, 2) + $(POWER z.im, 2) )
 * = hypot(z.re, z.im).
 */
Num abs(Num)(Num x) @safe pure nothrow
if (is(typeof(Num.init >= 0)) && is(typeof(-Num.init)) &&
    !(is(Num* : const(ifloat*)) || is(Num* : const(idouble*))
    || is(Num* : const(ireal*))))
{
    static if (isFloatingPoint!(Num))
        return fabs(x);
    else
        return x >= 0 ? x : -x;
}

/// ditto
auto abs(Num)(Num z) @safe pure nothrow @nogc
if (is(Num* : const(cfloat*)) || is(Num* : const(cdouble*))
    || is(Num* : const(creal*)))
{
    return hypot(z.re, z.im);
}

/// ditto
auto abs(Num)(Num y) @safe pure nothrow @nogc
if (is(Num* : const(ifloat*)) || is(Num* : const(idouble*))
    || is(Num* : const(ireal*)))
{
    return fabs(y.im);
}

/// ditto
@safe pure nothrow @nogc unittest
{
    assert(isIdentical(abs(-0.0L), 0.0L));
    assert(isNaN(abs(real.nan)));
    assert(abs(-real.infinity) == real.infinity);
    assert(abs(-3.2Li) == 3.2L);
    assert(abs(71.6Li) == 71.6L);
    assert(abs(-56) == 56);
    assert(abs(2321312L)  == 2321312L);
    assert(abs(-1L+1i) == sqrt(2.0L));
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(float, double, real))
    {
        T f = 3;
        assert(abs(f) == f);
        assert(abs(-f) == f);
    }
    foreach (T; AliasSeq!(cfloat, cdouble, creal))
    {
        T f = -12+3i;
        assert(abs(f) == hypot(f.re, f.im));
        assert(abs(-f) == hypot(f.re, f.im));
    }
}

/***********************************
 * Complex conjugate
 *
 *  conj(x + iy) = x - iy
 *
 * Note that z * conj(z) = $(POWER z.re, 2) - $(POWER z.im, 2)
 * is always a real number
 */
auto conj(Num)(Num z) @safe pure nothrow @nogc
if (is(Num* : const(cfloat*)) || is(Num* : const(cdouble*))
    || is(Num* : const(creal*)))
{
    //FIXME
    //Issue 14206
    static if (is(Num* : const(cdouble*)))
        return cast(cdouble) conj(cast(creal) z);
    else
        return z.re - z.im*1fi;
}

/** ditto */
auto conj(Num)(Num y) @safe pure nothrow @nogc
if (is(Num* : const(ifloat*)) || is(Num* : const(idouble*))
    || is(Num* : const(ireal*)))
{
    return -y;
}

///
@safe pure nothrow @nogc unittest
{
    creal c = 7 + 3Li;
    assert(conj(c) == 7-3Li);
    ireal z = -3.2Li;
    assert(conj(z) == -z);
}
//Issue 14206
@safe pure nothrow @nogc unittest
{
    cdouble c = 7 + 3i;
    assert(conj(c) == 7-3i);
    idouble z = -3.2i;
    assert(conj(z) == -z);
}
//Issue 14206
@safe pure nothrow @nogc unittest
{
    cfloat c = 7f + 3fi;
    assert(conj(c) == 7f-3fi);
    ifloat z = -3.2fi;
    assert(conj(z) == -z);
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

real cos(real x) @safe pure nothrow @nogc { pragma(inline, true); return core.math.cos(x); }
//FIXME
///ditto
double cos(double x) @safe pure nothrow @nogc { return cos(cast(real) x); }
//FIXME
///ditto
float cos(float x) @safe pure nothrow @nogc { return cos(cast(real) x); }

@safe unittest
{
    real function(real) pcos = &cos;
    assert(pcos != null);
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

real sin(real x) @safe pure nothrow @nogc { pragma(inline, true); return core.math.sin(x); }
//FIXME
///ditto
double sin(double x) @safe pure nothrow @nogc { return sin(cast(real) x); }
//FIXME
///ditto
float sin(float x) @safe pure nothrow @nogc { return sin(cast(real) x); }

///
@safe unittest
{
    import std.math : sin, PI;
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

/***********************************
 *  Returns sine for complex and imaginary arguments.
 *
 *  sin(z) = sin(z.re)*cosh(z.im) + cos(z.re)*sinh(z.im)i
 *
 * If both sin($(THETA)) and cos($(THETA)) are required,
 * it is most efficient to use expi($(THETA)).
 */
creal sin(creal z) @safe pure nothrow @nogc
{
    const creal cs = expi(z.re);
    const creal csh = coshisinh(z.im);
    return cs.im * csh.re + cs.re * csh.im * 1i;
}

/** ditto */
ireal sin(ireal y) @safe pure nothrow @nogc
{
    return cosh(y.im)*1i;
}

///
@safe pure nothrow @nogc unittest
{
  assert(sin(0.0+0.0i) == 0.0);
  assert(sin(2.0+0.0i) == sin(2.0L) );
}

/***********************************
 *  cosine, complex and imaginary
 *
 *  cos(z) = cos(z.re)*cosh(z.im) - sin(z.re)*sinh(z.im)i
 */
creal cos(creal z) @safe pure nothrow @nogc
{
    const creal cs = expi(z.re);
    const creal csh = coshisinh(z.im);
    return cs.re * csh.re - cs.im * csh.im * 1i;
}

/** ditto */
real cos(ireal y) @safe pure nothrow @nogc
{
    return cosh(y.im);
}

///
@safe pure nothrow @nogc unittest
{
    assert(cos(0.0+0.0i)==1.0);
    assert(cos(1.3L+0.0i)==cos(1.3L));
    assert(cos(5.2Li)== cosh(5.2L));
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

real tan(real x) @trusted pure nothrow @nogc
{
    version (D_InlineAsm_X86)
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
    }
    return real.nan;

Clear1: asm pure nothrow @nogc{
        fstp    ST(0)                   ; // dump X, which is always 1
    }

Lret: {}
    }
    else version (D_InlineAsm_X86_64)
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
    }
    return real.nan;

Clear1: asm pure nothrow @nogc{
        fstp    ST(0)                   ; // dump X, which is always 1
    }

Lret: {}
    }
    else
    {
        // Coefficients for tan(x) and PI/4 split into three parts.
        static if (floatTraits!real.realFormat == RealFormat.ieeeQuadruple)
        {
            static immutable real[6] P = [
                2.883414728874239697964612246732416606301E10L,
                -2.307030822693734879744223131873392503321E9L,
                5.160188250214037865511600561074819366815E7L,
                -4.249691853501233575668486667664718192660E5L,
                1.272297782199996882828849455156962260810E3L,
                -9.889929415807650724957118893791829849557E-1L
            ];
            static immutable real[7] Q = [
                8.650244186622719093893836740197250197602E10L,
                -4.152206921457208101480801635640958361612E10L,
                2.758476078803232151774723646710890525496E9L,
                -5.733709132766856723608447733926138506824E7L,
                4.529422062441341616231663543669583527923E5L,
                -1.317243702830553658702531997959756728291E3L,
                1.0
            ];

            enum real P1 =
                7.853981633974483067550664827649598009884357452392578125E-1L;
            enum real P2 =
                2.8605943630549158983813312792950660807511260829685741796657E-18L;
            enum real P3 =
                2.1679525325309452561992610065108379921905808E-35L;
        }
        else
        {
            static immutable real[3] P = [
               -1.7956525197648487798769E7L,
                1.1535166483858741613983E6L,
               -1.3093693918138377764608E4L,
            ];
            static immutable real[5] Q = [
               -5.3869575592945462988123E7L,
                2.5008380182335791583922E7L,
               -1.3208923444021096744731E6L,
                1.3681296347069295467845E4L,
                1.0000000000000000000000E0L,
            ];

            enum real P1 = 7.853981554508209228515625E-1L;
            enum real P2 = 7.946627356147928367136046290398E-9L;
            enum real P3 = 3.061616997868382943065164830688E-17L;
        }

        // Special cases.
        if (x == 0.0 || isNaN(x))
            return x;
        if (isInfinity(x))
            return real.nan;

        // Make argument positive but save the sign.
        bool sign = false;
        if (signbit(x))
        {
            sign = true;
            x = -x;
        }

        // Compute x mod PI/4.
        real y = floor(x / PI_4);
        // Strip high bits of integer part.
        real z = ldexp(y, -4);
        // Compute y - 16 * (y / 16).
        z = y - ldexp(floor(z), 4);

        // Integer and fraction part modulo one octant.
        int j = cast(int)(z);

        // Map zeros and singularities to origin.
        if (j & 1)
        {
            j += 1;
            y += 1.0;
        }

        z = ((x - y * P1) - y * P2) - y * P3;
        const real zz = z * z;

        if (zz > 1.0e-20L)
            y = z + z * (zz * poly(zz, P) / poly(zz, Q));
        else
            y = z;

        if (j & 2)
            y = -1.0 / y;

        return (sign) ? -y : y;
    }
}

@safe nothrow @nogc unittest
{
    static real[2][] vals =     // angle,tan
        [
         [   0,   0],
         [   .5,  .5463024898],
         [   1,   1.557407725],
         [   1.5, 14.10141995],
         [   2,  -2.185039863],
         [   2.5,-.7470222972],
         [   3,  -.1425465431],
         [   3.5, .3745856402],
         [   4,   1.157821282],
         [   4.5, 4.637332055],
         [   5,  -3.380515006],
         [   5.5,-.9955840522],
         [   6,  -.2910061914],
         [   6.5, .2202772003],
         [   10,  .6483608275],

         // special angles
         [   PI_4,   1],
         //[   PI_2,   real.infinity], // PI_2 is not _exactly_ pi/2.
         [   3*PI_4, -1],
         [   PI,     0],
         [   5*PI_4, 1],
         //[   3*PI_2, -real.infinity],
         [   7*PI_4, -1],
         [   2*PI,   0],
         ];
    int i;

    for (i = 0; i < vals.length; i++)
    {
        real x = vals[i][0];
        real r = vals[i][1];
        real t = tan(x);

        //printf("tan(%Lg) = %Lg, should be %Lg\n", x, t, r);
        if (!isIdentical(r, t)) assert(fabs(r-t) <= .0000001);

        x = -x;
        r = -r;
        t = tan(x);
        //printf("tan(%Lg) = %Lg, should be %Lg\n", x, t, r);
        if (!isIdentical(r, t) && !(r != r && t != t)) assert(fabs(r-t) <= .0000001);
    }
    // overflow
    assert(isNaN(tan(real.infinity)));
    assert(isNaN(tan(-real.infinity)));
    // NaN propagation
    assert(isIdentical( tan(NaN(0x0123L)), NaN(0x0123L) ));
}

@system unittest
{
    assert(equalsDigit(tan(PI / 3), std.math.sqrt(3.0), useDigits));
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
    return atan2(sqrt(1-x*x), x);
}

/// ditto
double acos(double x) @safe pure nothrow @nogc { return acos(cast(real) x); }

/// ditto
float acos(float x) @safe pure nothrow @nogc  { return acos(cast(real) x); }

@system unittest
{
    assert(equalsDigit(acos(0.5), std.math.PI / 3, useDigits));
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
    return atan2(x, sqrt(1-x*x));
}

/// ditto
double asin(double x) @safe pure nothrow @nogc { return asin(cast(real) x); }

/// ditto
float asin(float x) @safe pure nothrow @nogc  { return asin(cast(real) x); }

@system unittest
{
    assert(equalsDigit(asin(0.5), PI / 6, useDigits));
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
real atan(real x) @safe pure nothrow @nogc
{
    version (InlineAsm_X86_Any)
    {
        return atan2(x, 1.0L);
    }
    else
    {
        // Coefficients for atan(x)
        static if (floatTraits!real.realFormat == RealFormat.ieeeQuadruple)
        {
            static immutable real[9] P = [
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
            static immutable real[9] Q = [
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
        else
        {
            static immutable real[5] P = [
               -5.0894116899623603312185E1L,
               -9.9988763777265819915721E1L,
               -6.3976888655834347413154E1L,
               -1.4683508633175792446076E1L,
               -8.6863818178092187535440E-1L,
            ];
            static immutable real[6] Q = [
                1.5268235069887081006606E2L,
                3.9157570175111990631099E2L,
                3.6144079386152023162701E2L,
                1.4399096122250781605352E2L,
                2.2981886733594175366172E1L,
                1.0000000000000000000000E0L,
            ];
        }

        // tan(PI/8)
        enum real TAN_PI_8 = 0.414213562373095048801688724209698078569672L;
        // tan(3 * PI/8)
        enum real TAN3_PI_8 = 2.414213562373095048801688724209698078569672L;

        // Special cases.
        if (x == 0.0)
            return x;
        if (isInfinity(x))
            return copysign(PI_2, x);

        // Make argument positive but save the sign.
        bool sign = false;
        if (signbit(x))
        {
            sign = true;
            x = -x;
        }

        // Range reduction.
        real y;
        if (x > TAN3_PI_8)
        {
            y = PI_2;
            x = -(1.0 / x);
        }
        else if (x > TAN_PI_8)
        {
            y = PI_4;
            x = (x - 1.0)/(x + 1.0);
        }
        else
            y = 0.0;

        // Rational form in x^^2.
        const real z = x * x;
        y = y + (poly(z, P) / poly(z, Q)) * z * x + x;

        return (sign) ? -y : y;
    }
}

/// ditto
double atan(double x) @safe pure nothrow @nogc { return atan(cast(real) x); }

/// ditto
float atan(float x)  @safe pure nothrow @nogc { return atan(cast(real) x); }

@system unittest
{
    assert(equalsDigit(atan(std.math.sqrt(3.0)), PI / 3, useDigits));
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
real atan2(real y, real x) @trusted pure nothrow @nogc
{
    version (InlineAsm_X86_Any)
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
    else
    {
        // Special cases.
        if (isNaN(x) || isNaN(y))
            return real.nan;
        if (y == 0.0)
        {
            if (x >= 0 && !signbit(x))
                return copysign(0, y);
            else
                return copysign(PI, y);
        }
        if (x == 0.0)
            return copysign(PI_2, y);
        if (isInfinity(x))
        {
            if (signbit(x))
            {
                if (isInfinity(y))
                    return copysign(3*PI_4, y);
                else
                    return copysign(PI, y);
            }
            else
            {
                if (isInfinity(y))
                    return copysign(PI_4, y);
                else
                    return copysign(0.0, y);
            }
        }
        if (isInfinity(y))
            return copysign(PI_2, y);

        // Call atan and determine the quadrant.
        real z = atan(y / x);

        if (signbit(x))
        {
            if (signbit(y))
                z = z - PI;
            else
                z = z + PI;
        }

        if (z == 0.0)
            return copysign(z, y);

        return z;
    }
}

/// ditto
double atan2(double y, double x) @safe pure nothrow @nogc
{
    return atan2(cast(real) y, cast(real) x);
}

/// ditto
float atan2(float y, float x) @safe pure nothrow @nogc
{
    return atan2(cast(real) y, cast(real) x);
}

@system unittest
{
    assert(equalsDigit(atan2(1.0L, std.math.sqrt(3.0L)), PI / 6, useDigits));
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
    //  cosh = (exp(x)+exp(-x))/2.
    // The naive implementation works correctly.
    const real y = exp(x);
    return (y + 1.0/y) * 0.5;
}

/// ditto
double cosh(double x) @safe pure nothrow @nogc { return cosh(cast(real) x); }

/// ditto
float cosh(float x) @safe pure nothrow @nogc  { return cosh(cast(real) x); }

@system unittest
{
    assert(equalsDigit(cosh(1.0), (E + 1.0 / E) / 2, useDigits));
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
real sinh(real x) @safe pure nothrow @nogc
{
    //  sinh(x) =  (exp(x)-exp(-x))/2;
    // Very large arguments could cause an overflow, but
    // the maximum value of x for which exp(x) + exp(-x)) != exp(x)
    // is x = 0.5 * (real.mant_dig) * LN2. // = 22.1807 for real80.
    if (fabs(x) > real.mant_dig * LN2)
    {
        return copysign(0.5 * exp(fabs(x)), x);
    }

    const real y = expm1(x);
    return 0.5 * y / (y+1) * (y+2);
}

/// ditto
double sinh(double x) @safe pure nothrow @nogc { return sinh(cast(real) x); }

/// ditto
float sinh(float x) @safe pure nothrow @nogc  { return sinh(cast(real) x); }

@system unittest
{
    assert(equalsDigit(sinh(1.0), (E - 1.0 / E) / 2, useDigits));
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
real tanh(real x) @safe pure nothrow @nogc
{
    //  tanh(x) = (exp(x) - exp(-x))/(exp(x)+exp(-x))
    if (fabs(x) > real.mant_dig * LN2)
    {
        return copysign(1, x);
    }

    const real y = expm1(2*x);
    return y / (y + 2);
}

/// ditto
double tanh(double x) @safe pure nothrow @nogc { return tanh(cast(real) x); }

/// ditto
float tanh(float x) @safe pure nothrow @nogc { return tanh(cast(real) x); }

@system unittest
{
    assert(equalsDigit(tanh(1.0), sinh(1.0) / cosh(1.0), 15));
}

package:

/* Returns cosh(x) + I * sinh(x)
 * Only one call to exp() is performed.
 */
creal coshisinh(real x) @safe pure nothrow @nogc
{
    // See comments for cosh, sinh.
    if (fabs(x) > real.mant_dig * LN2)
    {
        const real y = exp(fabs(x));
        return y * 0.5 + 0.5i * copysign(y, x);
    }
    else
    {
        const real y = expm1(x);
        return (y + 1.0 + 1.0/(y + 1.0)) * 0.5 + 0.5i * y / (y+1) * (y+2);
    }
}

@safe pure nothrow @nogc unittest
{
    creal c = coshisinh(3.0L);
    assert(c.re == cosh(3.0L));
    assert(c.im == sinh(3.0L));
}

public:

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
real acosh(real x) @safe pure nothrow @nogc
{
    if (x > 1/real.epsilon)
        return LN2 + log(x);
    else
        return log(x + sqrt(x*x - 1));
}

/// ditto
double acosh(double x) @safe pure nothrow @nogc { return acosh(cast(real) x); }

/// ditto
float acosh(float x) @safe pure nothrow @nogc  { return acosh(cast(real) x); }


@system unittest
{
    assert(isNaN(acosh(0.9)));
    assert(isNaN(acosh(real.nan)));
    assert(acosh(1.0)==0.0);
    assert(acosh(real.infinity) == real.infinity);
    assert(isNaN(acosh(0.5)));
    assert(equalsDigit(acosh(cosh(3.0)), 3, useDigits));
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
real asinh(real x) @safe pure nothrow @nogc
{
    return (fabs(x) > 1 / real.epsilon)
       // beyond this point, x*x + 1 == x*x
       ?  copysign(LN2 + log(fabs(x)), x)
       // sqrt(x*x + 1) ==  1 + x * x / ( 1 + sqrt(x*x + 1) )
       : copysign(log1p(fabs(x) + x*x / (1 + sqrt(x*x + 1)) ), x);
}

/// ditto
double asinh(double x) @safe pure nothrow @nogc { return asinh(cast(real) x); }

/// ditto
float asinh(float x) @safe pure nothrow @nogc { return asinh(cast(real) x); }

@system  unittest
{
    assert(isIdentical(asinh(0.0), 0.0));
    assert(isIdentical(asinh(-0.0), -0.0));
    assert(asinh(real.infinity) == real.infinity);
    assert(asinh(-real.infinity) == -real.infinity);
    assert(isNaN(asinh(real.nan)));
    assert(equalsDigit(asinh(sinh(3.0)), 3, useDigits));
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
    // log( (1+x)/(1-x) ) == log ( 1 + (2*x)/(1-x) )
    return  0.5 * log1p( 2 * x / (1 - x) );
}

/// ditto
double atanh(double x) @safe pure nothrow @nogc { return atanh(cast(real) x); }

/// ditto
float atanh(float x) @safe pure nothrow @nogc { return atanh(cast(real) x); }


@system unittest
{
    assert(isIdentical(atanh(0.0), 0.0));
    assert(isIdentical(atanh(-0.0),-0.0));
    assert(isNaN(atanh(real.nan)));
    assert(isNaN(atanh(-real.infinity)));
    assert(atanh(0.0) == 0);
    assert(equalsDigit(atanh(tanh(0.5L)), 0.5, useDigits));
}

/*****************************************
 * Returns x rounded to a long value using the current rounding mode.
 * If the integer value of x is
 * greater than long.max, the result is
 * indeterminate.
 */
long rndtol(real x) @nogc @safe pure nothrow { pragma(inline, true); return core.math.rndtol(x); }
//FIXME
///ditto
long rndtol(double x) @safe pure nothrow @nogc { return rndtol(cast(real) x); }
//FIXME
///ditto
long rndtol(float x) @safe pure nothrow @nogc { return rndtol(cast(real) x); }

@safe unittest
{
    long function(real) prndtol = &rndtol;
    assert(prndtol != null);
}

/*****************************************
 * Returns x rounded to a long value using the FE_TONEAREST rounding mode.
 * If the integer value of x is
 * greater than long.max, the result is
 * indeterminate.
 */
extern (C) real rndtonl(real x);

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
float sqrt(float x) @nogc @safe pure nothrow { pragma(inline, true); return core.math.sqrt(x); }

/// ditto
double sqrt(double x) @nogc @safe pure nothrow { pragma(inline, true); return core.math.sqrt(x); }

/// ditto
real sqrt(real x) @nogc @safe pure nothrow { pragma(inline, true); return core.math.sqrt(x); }

@safe pure nothrow @nogc unittest
{
    //ctfe
    enum ZX80 = sqrt(7.0f);
    enum ZX81 = sqrt(7.0);
    enum ZX82 = sqrt(7.0L);

    assert(isNaN(sqrt(-1.0f)));
    assert(isNaN(sqrt(-1.0)));
    assert(isNaN(sqrt(-1.0L)));
}

@safe unittest
{
    float function(float) psqrtf = &sqrt;
    assert(psqrtf != null);
    double function(double) psqrtd = &sqrt;
    assert(psqrtd != null);
    real function(real) psqrtr = &sqrt;
    assert(psqrtr != null);
}

creal sqrt(creal z) @nogc @safe pure nothrow
{
    creal c;
    real x,y,w,r;

    if (z == 0)
    {
        c = 0 + 0i;
    }
    else
    {
        const real z_re = z.re;
        const real z_im = z.im;

        x = fabs(z_re);
        y = fabs(z_im);
        if (x >= y)
        {
            r = y / x;
            w = sqrt(x) * sqrt(0.5 * (1 + sqrt(1 + r * r)));
        }
        else
        {
            r = x / y;
            w = sqrt(y) * sqrt(0.5 * (r + sqrt(1 + r * r)));
        }

        if (z_re >= 0)
        {
            c = w + (z_im / (w + w)) * 1.0i;
        }
        else
        {
            if (z_im < 0)
                w = -w;
            c = z_im / (w + w) + w * 1.0i;
        }
    }
    return c;
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
real exp(real x) @trusted pure nothrow @nogc
{
    version (D_InlineAsm_X86)
    {
        //  e^^x = 2^^(LOG2E*x)
        // (This is valid because the overflow & underflow limits for exp
        // and exp2 are so similar).
        return exp2(LOG2E*x);
    }
    else version (D_InlineAsm_X86_64)
    {
        //  e^^x = 2^^(LOG2E*x)
        // (This is valid because the overflow & underflow limits for exp
        // and exp2 are so similar).
        return exp2(LOG2E*x);
    }
    else
    {
        alias F = floatTraits!real;
        static if (F.realFormat == RealFormat.ieeeDouble)
        {
            // Coefficients for exp(x)
            static immutable real[3] P = [
                9.99999999999999999910E-1L,
                3.02994407707441961300E-2L,
                1.26177193074810590878E-4L,
            ];
            static immutable real[4] Q = [
                2.00000000000000000009E0L,
                2.27265548208155028766E-1L,
                2.52448340349684104192E-3L,
                3.00198505138664455042E-6L,
            ];

            // C1 + C2 = LN2.
            enum real C1 = 6.93145751953125E-1;
            enum real C2 = 1.42860682030941723212E-6;

            // Overflow and Underflow limits.
            enum real OF =  7.09782712893383996732E2;  // ln((1-2^-53) * 2^1024)
            enum real UF = -7.451332191019412076235E2; // ln(2^-1075)
        }
        else static if (F.realFormat == RealFormat.ieeeExtended)
        {
            // Coefficients for exp(x)
            static immutable real[3] P = [
                9.9999999999999999991025E-1L,
                3.0299440770744196129956E-2L,
                1.2617719307481059087798E-4L,
            ];
            static immutable real[4] Q = [
                2.0000000000000000000897E0L,
                2.2726554820815502876593E-1L,
                2.5244834034968410419224E-3L,
                3.0019850513866445504159E-6L,
            ];

            // C1 + C2 = LN2.
            enum real C1 = 6.9314575195312500000000E-1L;
            enum real C2 = 1.4286068203094172321215E-6L;

            // Overflow and Underflow limits.
            enum real OF =  1.1356523406294143949492E4L;  // ln((1-2^-64) * 2^16384)
            enum real UF = -1.13994985314888605586758E4L; // ln(2^-16446)
        }
        else static if (F.realFormat == RealFormat.ieeeQuadruple)
        {
            // Coefficients for exp(x) - 1
            static immutable real[5] P = [
                9.999999999999999999999999999999999998502E-1L,
                3.508710990737834361215404761139478627390E-2L,
                2.708775201978218837374512615596512792224E-4L,
                6.141506007208645008909088812338454698548E-7L,
                3.279723985560247033712687707263393506266E-10L
            ];
            static immutable real[6] Q = [
                2.000000000000000000000000000000000000150E0,
                2.368408864814233538909747618894558968880E-1L,
                3.611828913847589925056132680618007270344E-3L,
                1.504792651814944826817779302637284053660E-5L,
                1.771372078166251484503904874657985291164E-8L,
                2.980756652081995192255342779918052538681E-12L
            ];

            // C1 + C2 = LN2.
            enum real C1 = 6.93145751953125E-1L;
            enum real C2 = 1.428606820309417232121458176568075500134E-6L;

            // Overflow and Underflow limits.
            enum real OF =  1.135583025911358400418251384584930671458833e4L;
            enum real UF = -1.143276959615573793352782661133116431383730e4L;
        }
        else
            static assert(0, "Not implemented for this architecture");

        // Special cases. Raises an overflow or underflow flag accordingly,
        // except in the case for CTFE, where there are no hardware controls.
        if (isNaN(x))
            return x;
        if (x > OF)
            return real.infinity;
        if (x < UF)
            return 0.0;

        // Express: e^^x = e^^g * 2^^n
        //   = e^^g * e^^(n * LOG2E)
        //   = e^^(g + n * LOG2E)
        int n = cast(int) floor(LOG2E * x + 0.5);
        x -= n * C1;
        x -= n * C2;

        // Rational approximation for exponential of the fractional part:
        //  e^^x = 1 + 2x P(x^^2) / (Q(x^^2) - P(x^^2))
        const real xx = x * x;
        const real px = x * poly(xx, P);
        x = px / (poly(xx, Q) - px);
        x = 1.0 + ldexp(x, 1);

        // Scale by power of 2.
        x = ldexp(x, n);

        return x;
    }
}

/// ditto
double exp(double x) @safe pure nothrow @nogc  { return exp(cast(real) x); }

/// ditto
float exp(float x)  @safe pure nothrow @nogc   { return exp(cast(real) x); }

@system unittest
{
    assert(equalsDigit(exp(3.0L), E * E * E, useDigits));
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
real expm1(real x) @trusted pure nothrow @nogc
{
    version (D_InlineAsm_X86)
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
    else version (D_InlineAsm_X86_64)
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
    {
        // Coefficients for exp(x) - 1 and overflow/underflow limits.
        static if (floatTraits!real.realFormat == RealFormat.ieeeQuadruple)
        {
            static immutable real[8] P = [
                2.943520915569954073888921213330863757240E8L,
                -5.722847283900608941516165725053359168840E7L,
                8.944630806357575461578107295909719817253E6L,
                -7.212432713558031519943281748462837065308E5L,
                4.578962475841642634225390068461943438441E4L,
                -1.716772506388927649032068540558788106762E3L,
                4.401308817383362136048032038528753151144E1L,
                -4.888737542888633647784737721812546636240E-1L
            ];

            static immutable real[9] Q = [
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

            enum real OF = 1.1356523406294143949491931077970764891253E4L;
            enum real UF = -1.143276959615573793352782661133116431383730e4L;
        }
        else
        {
            static immutable real[5] P = [
               -1.586135578666346600772998894928250240826E4L,
                2.642771505685952966904660652518429479531E3L,
               -3.423199068835684263987132888286791620673E2L,
                1.800826371455042224581246202420972737840E1L,
               -5.238523121205561042771939008061958820811E-1L,
            ];
            static immutable real[6] Q = [
               -9.516813471998079611319047060563358064497E4L,
                3.964866271411091674556850458227710004570E4L,
               -7.207678383830091850230366618190187434796E3L,
                7.206038318724600171970199625081491823079E2L,
               -4.002027679107076077238836622982900945173E1L,
                1.0
            ];

            enum real OF =  1.1356523406294143949492E4L;
            enum real UF = -4.5054566736396445112120088E1L;
        }


        // C1 + C2 = LN2.
        enum real C1 = 6.9314575195312500000000E-1L;
        enum real C2 = 1.428606820309417232121458176568075500134E-6L;

        // Special cases. Raises an overflow flag, except in the case
        // for CTFE, where there are no hardware controls.
        if (x > OF)
            return real.infinity;
        if (x == 0.0)
            return x;
        if (x < UF)
            return -1.0;

        // Express x = LN2 (n + remainder), remainder not exceeding 1/2.
        int n = cast(int) floor(0.5 + x / LN2);
        x -= n * C1;
        x -= n * C2;

        // Rational approximation:
        //  exp(x) - 1 = x + 0.5 x^^2 + x^^3 P(x) / Q(x)
        real px = x * poly(x, P);
        real qx = poly(x, Q);
        const real xx = x * x;
        qx = x + (0.5 * xx + xx * px / qx);

        // We have qx = exp(remainder LN2) - 1, so:
        //  exp(x) - 1 = 2^^n (qx + 1) - 1 = 2^^n qx + 2^^n - 1.
        px = ldexp(1.0, n);
        x = px * qx + (px - 1.0);

        return x;
    }
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
real exp2(real x) @nogc @trusted pure nothrow
{
    version (InlineAsm_X86_Any)
    {
        if (!__ctfe)
            return exp2Asm(x);
        else
            return exp2Impl(x);
    }
    else
    {
        return exp2Impl(x);
    }
}

version (InlineAsm_X86_Any)
private real exp2Asm(real x) @nogc @trusted pure nothrow
{
    version (D_InlineAsm_X86)
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
    else version (D_InlineAsm_X86_64)
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

private real exp2Impl(real x) @nogc @trusted pure nothrow
{
    // Coefficients for exp2(x)
    static if (floatTraits!real.realFormat == RealFormat.ieeeQuadruple)
    {
        static immutable real[5] P = [
            9.079594442980146270952372234833529694788E12L,
            1.530625323728429161131811299626419117557E11L,
            5.677513871931844661829755443994214173883E8L,
            6.185032670011643762127954396427045467506E5L,
            1.587171580015525194694938306936721666031E2L
        ];

        static immutable real[6] Q = [
            2.619817175234089411411070339065679229869E13L,
            1.490560994263653042761789432690793026977E12L,
            1.092141473886177435056423606755843616331E10L,
            2.186249607051644894762167991800811827835E7L,
            1.236602014442099053716561665053645270207E4L,
            1.0
        ];
    }
    else
    {
        static immutable real[3] P = [
            2.0803843631901852422887E6L,
            3.0286971917562792508623E4L,
            6.0614853552242266094567E1L,
        ];
        static immutable real[4] Q = [
            6.0027204078348487957118E6L,
            3.2772515434906797273099E5L,
            1.7492876999891839021063E3L,
            1.0000000000000000000000E0L,
        ];
    }

    // Overflow and Underflow limits.
    enum real OF =  16_384.0L;
    enum real UF = -16_382.0L;

    // Special cases. Raises an overflow or underflow flag accordingly,
    // except in the case for CTFE, where there are no hardware controls.
    if (isNaN(x))
        return x;
    if (x > OF)
        return real.infinity;
    if (x < UF)
        return 0.0;

    // Separate into integer and fractional parts.
    int n = cast(int) floor(x + 0.5);
    x -= n;

    // Rational approximation:
    //  exp2(x) = 1.0 + 2x P(x^^2) / (Q(x^^2) - P(x^^2))
    const real xx = x * x;
    const real px = x * poly(xx, P);
    x = px / (poly(xx, Q) - px);
    x = 1.0 + ldexp(x, 1);

    // Scale by power of 2.
    x = ldexp(x, n);

    return x;
}

///
@safe unittest
{
    assert(feqrel(exp2(0.5L), SQRT2) >= real.mant_dig -1);
    assert(exp2(8.0L) == 256.0);
    assert(exp2(-9.0L)== 1.0L/512.0);
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

@system unittest
{
    version (FloatingPointControlSupport)
    {
        FloatingPointControl ctrl;
        if (FloatingPointControl.hasExceptionTraps)
            ctrl.disableExceptions(FloatingPointControl.allExceptions);
        ctrl.rounding = FloatingPointControl.roundToNearest;
    }

    static if (real.mant_dig == 113)
    {
        static immutable real[2][] exptestpoints =
        [ //  x               exp(x)
            [ 1.0L,           E                                        ],
            [ 0.5L,           0x1.a61298e1e069bc972dfefab6df34p+0L     ],
            [ 3.0L,           E*E*E                                    ],
            [ 0x1.6p+13L,     0x1.6e509d45728655cdb4840542acb5p+16250L ], // near overflow
            [ 0x1.7p+13L,     real.infinity                            ], // close overflow
            [ 0x1p+80L,       real.infinity                            ], // far overflow
            [ real.infinity,  real.infinity                            ],
            [-0x1.18p+13L,    0x1.5e4bf54b4807034ea97fef0059a6p-12927L ], // near underflow
            [-0x1.625p+13L,   0x1.a6bd68a39d11fec3a250cd97f524p-16358L ], // ditto
            [-0x1.62dafp+13L, 0x0.cb629e9813b80ed4d639e875be6cp-16382L ], // near underflow - subnormal
            [-0x1.6549p+13L,  0x0.0000000000000000000000000001p-16382L ], // ditto
            [-0x1.655p+13L,   0                                        ], // close underflow
            [-0x1p+30L,       0                                        ], // far underflow
        ];
    }
    else static if (real.mant_dig == 64) // 80-bit reals
    {
        static immutable real[2][] exptestpoints =
        [ //  x               exp(x)
            [ 1.0L,           E                            ],
            [ 0.5L,           0x1.a61298e1e069bc97p+0L     ],
            [ 3.0L,           E*E*E                        ],
            [ 0x1.1p+13L,     0x1.29aeffefc8ec645p+12557L  ], // near overflow
            [ 0x1.7p+13L,     real.infinity                ], // close overflow
            [ 0x1p+80L,       real.infinity                ], // far overflow
            [ real.infinity,  real.infinity                ],
            [-0x1.18p+13L,    0x1.5e4bf54b4806db9p-12927L  ], // near underflow
            [-0x1.625p+13L,   0x1.a6bd68a39d11f35cp-16358L ], // ditto
            [-0x1.62dafp+13L, 0x1.96c53d30277021dp-16383L  ], // near underflow - subnormal
            [-0x1.643p+13L,   0x1p-16444L                  ], // ditto
            [-0x1.645p+13L,   0                            ], // close underflow
            [-0x1p+30L,       0                            ], // far underflow
        ];
    }
    else static if (real.mant_dig == 53) // 64-bit reals
    {
        static immutable real[2][] exptestpoints =
        [ //  x,             exp(x)
            [ 1.0L,          E                        ],
            [ 0.5L,          0x1.a61298e1e069cp+0L    ],
            [ 3.0L,          E*E*E                    ],
            [ 0x1.6p+9L,     0x1.93bf4ec282efbp+1015L ], // near overflow
            [ 0x1.7p+9L,     real.infinity            ], // close overflow
            [ 0x1p+80L,      real.infinity            ], // far overflow
            [ real.infinity, real.infinity            ],
            [-0x1.6p+9L,     0x1.44a3824e5285fp-1016L ], // near underflow
            [-0x1.64p+9L,    0x0.06f84920bb2d3p-1022L ], // near underflow - subnormal
            [-0x1.743p+9L,   0x0.0000000000001p-1022L ], // ditto
            [-0x1.8p+9L,     0                        ], // close underflow
            [-0x1p30L,       0                        ], // far underflow
        ];
    }
    else
        static assert(0, "No exp() tests for real type!");

    const minEqualDecimalDigits = real.dig - 3;
    real x;
    version (IeeeFlagsSupport) IeeeFlags f;
    foreach (ref pair; exptestpoints)
    {
        version (IeeeFlagsSupport) resetIeeeFlags();
        x = exp(pair[0]);
        assert(equalsDigit(x, pair[1], minEqualDecimalDigits));
    }

    // Ideally, exp(0) would not set the inexact flag.
    // Unfortunately, fldl2e sets it!
    // So it's not realistic to avoid setting it.
    assert(exp(0.0L) == 1.0);

    // NaN propagation. Doesn't set flags, bcos was already NaN.
    version (IeeeFlagsSupport)
    {
        resetIeeeFlags();
        x = exp(real.nan);
        f = ieeeFlags;
        assert(isIdentical(abs(x), real.nan));
        assert(f.flags == 0);

        resetIeeeFlags();
        x = exp(-real.nan);
        f = ieeeFlags;
        assert(isIdentical(abs(x), real.nan));
        assert(f.flags == 0);
    }
    else
    {
        x = exp(real.nan);
        assert(isIdentical(abs(x), real.nan));

        x = exp(-real.nan);
        assert(isIdentical(abs(x), real.nan));
    }

    x = exp(NaN(0x123));
    assert(isIdentical(x, NaN(0x123)));

    // High resolution test (verified against GNU MPFR/Mathematica).
    assert(exp(0.5L) == 0x1.A612_98E1_E069_BC97_2DFE_FAB6_DF34p+0L);
}


/**
 * Calculate cos(y) + i sin(y).
 *
 * On many CPUs (such as x86), this is a very efficient operation;
 * almost twice as fast as calculating sin(y) and cos(y) separately,
 * and is the preferred method when both are required.
 */
creal expi(real y) @trusted pure nothrow @nogc
{
    version (InlineAsm_X86_Any)
    {
        version (Win64)
        {
            asm pure nothrow @nogc
            {
                naked;
                fld     real ptr [ECX];
                fsincos;
                fxch    ST(1), ST(0);
                ret;
            }
        }
        else
        {
            asm pure nothrow @nogc
            {
                fld y;
                fsincos;
                fxch ST(1), ST(0);
            }
        }
    }
    else
    {
        return cos(y) + sin(y)*1i;
    }
}

///
@safe pure nothrow @nogc unittest
{
    assert(expi(1.3e5L) == cos(1.3e5L) + sin(1.3e5L) * 1i);
    assert(expi(0.0L) == 1L + 0.0Li);
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
    Unqual!T vf = value;
    ushort* vu = cast(ushort*)&vf;
    static if (is(Unqual!T == float))
        int* vi = cast(int*)&vf;
    else
        long* vl = cast(long*)&vf;
    int ex;
    alias F = floatTraits!T;

    ex = vu[F.EXPPOS_SHORT] & F.EXPMASK;
    static if (F.realFormat == RealFormat.ieeeExtended)
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
@system unittest
{
    int exp;
    real mantissa = frexp(123.456L, exp);

    // check if values are equal to 19 decimal digits of precision
    assert(equalsDigit(mantissa * pow(2.0L, cast(real) exp), 123.456L, 19));

    assert(frexp(-real.nan, exp) && exp == int.min);
    assert(frexp(real.nan, exp) && exp == int.min);
    assert(frexp(-real.infinity, exp) == -real.infinity && exp == int.min);
    assert(frexp(real.infinity, exp) == real.infinity && exp == int.max);
    assert(frexp(-0.0, exp) == -0.0 && exp == 0);
    assert(frexp(0.0, exp) == 0.0 && exp == 0);
}

@safe unittest
{
    import std.meta : AliasSeq;
    import std.typecons : tuple, Tuple;

    foreach (T; AliasSeq!(real, double, float))
    {
        Tuple!(T, T, int)[] vals =     // x,frexp,exp
            [
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

             // Phobos issue #16026:
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
    }
}

@safe unittest
{
    import std.meta : AliasSeq;
    void foo() {
        foreach (T; AliasSeq!(real, double, float))
        {
            int exp;
            const T a = 1;
            immutable T b = 2;
            auto c = frexp(a, exp);
            auto d = frexp(b, exp);
        }
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
    static if (F.realFormat == RealFormat.ieeeExtended)
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
        core.stdc.math.ilogbl(x);
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

alias FP_ILOGB0   = core.stdc.math.FP_ILOGB0;
alias FP_ILOGBNAN = core.stdc.math.FP_ILOGBNAN;

@system nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    import std.typecons : Tuple;
    foreach (F; AliasSeq!(float, double, real))
    {
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
    }

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

real ldexp(real n, int exp) @nogc @safe pure nothrow { pragma(inline, true); return core.math.ldexp(n, exp); }
//FIXME
///ditto
double ldexp(double n, int exp) @safe pure nothrow @nogc { return ldexp(cast(real) n, exp); }
//FIXME
///ditto
float ldexp(float n, int exp) @safe pure nothrow @nogc { return ldexp(cast(real) n, exp); }

///
@nogc @safe pure nothrow unittest
{
    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(float, double, real))
    {
        T r;

        r = ldexp(3.0L, 3);
        assert(r == 24);

        r = ldexp(cast(T) 3.0, cast(int) 3);
        assert(r == 24);

        T n = 3.0;
        int exp = 3;
        r = ldexp(n, exp);
        assert(r == 24);
    }
}

@safe pure nothrow @nogc unittest
{
    static if (floatTraits!(real).realFormat == RealFormat.ieeeExtended ||
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

/* workaround Issue 14718, float parsing depends on platform strtold
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

@system unittest
{
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

        assert(equalsDigit(z, l, 7));
    }

    real function(real, int) pldexp = &ldexp;
    assert(pldexp != null);
}

private
{
    version (INLINE_YL2X) {} else
    {
        static if (floatTraits!real.realFormat == RealFormat.ieeeQuadruple)
        {
            // Coefficients for log(1 + x) = x - x**2/2 + x**3 P(x)/Q(x)
            static immutable real[13] logCoeffsP = [
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
            static immutable real[13] logCoeffsQ = [
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

            // Coefficients for log(x) = z + z^3 P(z^2)/Q(z^2)
            // where z = 2(x-1)/(x+1)
            static immutable real[6] logCoeffsR = [
                -8.828896441624934385266096344596648080902E-1L,
                8.057002716646055371965756206836056074715E1L,
                -2.024301798136027039250415126250455056397E3L,
                2.048819892795278657810231591630928516206E4L,
                -8.977257995689735303686582344659576526998E4L,
                1.418134209872192732479751274970992665513E5L
            ];
            static immutable real[6] logCoeffsS = [
                1.701761051846631278975701529965589676574E6L
                -1.332535117259762928288745111081235577029E6L,
                4.001557694070773974936904547424676279307E5L,
                -5.748542087379434595104154610899551484314E4L,
                3.998526750980007367835804959888064681098E3L,
                -1.186359407982897997337150403816839480438E2L,
                1.0
            ];
        }
        else
        {
            // Coefficients for log(1 + x) = x - x**2/2 + x**3 P(x)/Q(x)
            static immutable real[7] logCoeffsP = [
                2.0039553499201281259648E1L,
                5.7112963590585538103336E1L,
                6.0949667980987787057556E1L,
                2.9911919328553073277375E1L,
                6.5787325942061044846969E0L,
                4.9854102823193375972212E-1L,
                4.5270000862445199635215E-5L,
            ];
            static immutable real[7] logCoeffsQ = [
                6.0118660497603843919306E1L,
                2.1642788614495947685003E2L,
                3.0909872225312059774938E2L,
                2.2176239823732856465394E2L,
                8.3047565967967209469434E1L,
                1.5062909083469192043167E1L,
                1.0000000000000000000000E0L,
            ];

            // Coefficients for log(x) = z + z^3 P(z^2)/Q(z^2)
            // where z = 2(x-1)/(x+1)
            static immutable real[4] logCoeffsR = [
               -3.5717684488096787370998E1L,
                1.0777257190312272158094E1L,
               -7.1990767473014147232598E-1L,
                1.9757429581415468984296E-3L,
            ];
            static immutable real[4] logCoeffsS = [
               -4.2861221385716144629696E2L,
                1.9361891836232102174846E2L,
               -2.6201045551331104417768E1L,
                1.0000000000000000000000E0L,
            ];
        }
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
real log(real x) @safe pure nothrow @nogc
{
    version (INLINE_YL2X)
        return core.math.yl2x(x, LN2);
    else
    {
        // C1 + C2 = LN2.
        enum real C1 = 6.93145751953125E-1L;
        enum real C2 = 1.428606820309417232121458176568075500134E-6L;

        // Special cases.
        if (isNaN(x))
            return x;
        if (isInfinity(x) && !signbit(x))
            return x;
        if (x == 0.0)
            return -real.infinity;
        if (x < 0.0)
            return real.nan;

        // Separate mantissa from exponent.
        // Note, frexp is used so that denormal numbers will be handled properly.
        real y, z;
        int exp;

        x = frexp(x, exp);

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
            z = x * (z * poly(z, logCoeffsR) / poly(z, logCoeffsS));
            z += exp * C2;
            z += x;
            z += exp * C1;

            return z;
        }

        // Logarithm using log(1 + x) = x - .5x^^2 + x^^3 P(x) / Q(x)
        if (x < SQRT1_2)
        {   // 2x - 1
            exp -= 1;
            x = ldexp(x, 1) - 1.0;
        }
        else
        {
            x = x - 1.0;
        }
        z = x * x;
        y = x * (z * poly(x, logCoeffsP) / poly(x, logCoeffsQ));
        y += exp * C2;
        z = y - ldexp(z, -1);

        // Note, the sum of above terms does not exceed x/4,
        // so it contributes at most about 1/4 lsb to the error.
        z += x;
        z += exp * C1;

        return z;
    }
}

///
@safe pure nothrow @nogc unittest
{
    assert(log(E) == 1);
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
real log10(real x) @safe pure nothrow @nogc
{
    version (INLINE_YL2X)
        return core.math.yl2x(x, LOG2);
    else
    {
        // log10(2) split into two parts.
        enum real L102A =  0.3125L;
        enum real L102B = -1.14700043360188047862611052755069732318101185E-2L;

        // log10(e) split into two parts.
        enum real L10EA =  0.5L;
        enum real L10EB = -6.570551809674817234887108108339491770560299E-2L;

        // Special cases are the same as for log.
        if (isNaN(x))
            return x;
        if (isInfinity(x) && !signbit(x))
            return x;
        if (x == 0.0)
            return -real.infinity;
        if (x < 0.0)
            return real.nan;

        // Separate mantissa from exponent.
        // Note, frexp is used so that denormal numbers will be handled properly.
        real y, z;
        int exp;

        x = frexp(x, exp);

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
            y = x * (z * poly(z, logCoeffsR) / poly(z, logCoeffsS));
            goto Ldone;
        }

        // Logarithm using log(1 + x) = x - .5x^^2 + x^^3 P(x) / Q(x)
        if (x < SQRT1_2)
        {   // 2x - 1
            exp -= 1;
            x = ldexp(x, 1) - 1.0;
        }
        else
            x = x - 1.0;

        z = x * x;
        y = x * (z * poly(x, logCoeffsP) / poly(x, logCoeffsQ));
        y = y - ldexp(z, -1);

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
}

///
@safe pure nothrow @nogc unittest
{
    assert(fabs(log10(1000) - 3) < .000001);
}

/******************************************
 *      Calculates the natural logarithm of 1 + x.
 *
 *      For very small x, log1p(x) will be more accurate than
 *      log(1 + x).
 *
 *  $(TABLE_SV
 *  $(TR $(TH x)            $(TH log1p(x))     $(TH divide by 0?) $(TH invalid?))
 *  $(TR $(TD $(PLUSMN)0.0) $(TD $(PLUSMN)0.0) $(TD no)           $(TD no))
 *  $(TR $(TD -1.0)         $(TD -$(INFIN))    $(TD yes)          $(TD no))
 *  $(TR $(TD $(LT)-1.0)    $(TD $(NAN))       $(TD no)           $(TD yes))
 *  $(TR $(TD +$(INFIN))    $(TD -$(INFIN))    $(TD no)           $(TD no))
 *  )
 */
real log1p(real x) @safe pure nothrow @nogc
{
    version (INLINE_YL2X)
    {
        // On x87, yl2xp1 is valid if and only if -0.5 <= lg(x) <= 0.5,
        //    ie if -0.29 <= x <= 0.414
        return (fabs(x) <= 0.25)  ? core.math.yl2xp1(x, LN2) : core.math.yl2x(x+1, LN2);
    }
    else
    {
        // Special cases.
        if (isNaN(x) || x == 0.0)
            return x;
        if (isInfinity(x) && !signbit(x))
            return x;
        if (x == -1.0)
            return -real.infinity;
        if (x < -1.0)
            return real.nan;

        return log(x + 1.0);
    }
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
real log2(real x) @safe pure nothrow @nogc
{
    version (INLINE_YL2X)
        return core.math.yl2x(x, 1);
    else
    {
        // Special cases are the same as for log.
        if (isNaN(x))
            return x;
        if (isInfinity(x) && !signbit(x))
            return x;
        if (x == 0.0)
            return -real.infinity;
        if (x < 0.0)
            return real.nan;

        // Separate mantissa from exponent.
        // Note, frexp is used so that denormal numbers will be handled properly.
        real y, z;
        int exp;

        x = frexp(x, exp);

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
            y = x * (z * poly(z, logCoeffsR) / poly(z, logCoeffsS));
            goto Ldone;
        }

        // Logarithm using log(1 + x) = x - .5x^^2 + x^^3 P(x) / Q(x)
        if (x < SQRT1_2)
        {   // 2x - 1
            exp -= 1;
            x = ldexp(x, 1) - 1.0;
        }
        else
            x = x - 1.0;

        z = x * x;
        y = x * (z * poly(x, logCoeffsP) / poly(x, logCoeffsQ));
        y = y - ldexp(z, -1);

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
}

///
@system unittest
{
    // check if values are equal to 19 decimal digits of precision
    assert(equalsDigit(log2(1024.0L), 10, 19));
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
real logb(real x) @trusted nothrow @nogc
{
    version (Win64_DMD_InlineAsm)
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
    else version (MSVC_InlineAsm)
    {
        asm pure nothrow @nogc
        {
            fld     x                   ;
            fxtract                     ;
            fstp    ST(0)               ;
        }
    }
    else
        return core.stdc.math.logbl(x);
}

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
        i = trunc(x);
        return copysign(isInfinity(x) ? 0.0 : x - i, x);
    }
    else
        return core.stdc.math.modfl(x,&i);
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
real scalbn(real x, int n) @trusted nothrow @nogc
{
    version (InlineAsm_X86_Any)
    {
        // scalbnl is not supported on DMD-Windows, so use asm pure nothrow @nogc.
        version (Win64)
        {
            asm pure nothrow @nogc {
                naked                           ;
                mov     16[RSP],RCX             ;
                fild    word ptr 16[RSP]        ;
                fld     real ptr [RDX]          ;
                fscale                          ;
                fstp    ST(1)                   ;
                ret                             ;
            }
        }
        else
        {
            asm pure nothrow @nogc {
                fild n;
                fld x;
                fscale;
                fstp ST(1);
            }
        }
    }
    else
    {
        return core.stdc.math.scalbnl(x, n);
    }
}

///
@safe nothrow @nogc unittest
{
    assert(scalbn(-real.infinity, 5) == -real.infinity);
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
        version (INLINE_YL2X)
            return copysign(exp2(core.math.yl2x(fabs(x), 1.0L/3.0L)), x);
        else
            return core.stdc.math.cbrtl(x);
    }
    else
        return core.stdc.math.cbrtl(x);
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
real fabs(real x) @safe pure nothrow @nogc { pragma(inline, true); return core.math.fabs(x); }
//FIXME
///ditto
double fabs(double x) @safe pure nothrow @nogc { return fabs(cast(real) x); }
//FIXME
///ditto
float fabs(float x) @safe pure nothrow @nogc { return fabs(cast(real) x); }

@safe unittest
{
    real function(real) pfabs = &fabs;
    assert(pfabs != null);
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

real hypot(real x, real y) @safe pure nothrow @nogc
{
    // Scale x and y to avoid underflow and overflow.
    // If one is huge and the other tiny, return the larger.
    // If both are huge, avoid overflow by scaling by 1/sqrt(real.max/2).
    // If both are tiny, avoid underflow by scaling by sqrt(real.min_normal*real.epsilon).

    enum real SQRTMIN = 0.5 * sqrt(real.min_normal); // This is a power of 2.
    enum real SQRTMAX = 1.0L / SQRTMIN; // 2^^((max_exp)/2) = nextUp(sqrt(real.max))

    static assert(2*(SQRTMAX/2)*(SQRTMAX/2) <= real.max);

    // Proves that sqrt(real.max) ~~  0.5/sqrt(real.min_normal)
    static assert(real.min_normal*real.max > 2 && real.min_normal*real.max <= 4);

    real u = fabs(x);
    real v = fabs(y);
    if (!(u >= v))  // check for NaN as well.
    {
        v = u;
        u = fabs(y);
        if (u == real.infinity) return u; // hypot(inf, nan) == inf
        if (v == real.infinity) return v; // hypot(nan, inf) == inf
    }

    // Now u >= v, or else one is NaN.
    if (v >= SQRTMAX*0.5)
    {
            // hypot(huge, huge) -- avoid overflow
        u *= SQRTMIN*0.5;
        v *= SQRTMIN*0.5;
        return sqrt(u*u + v*v) * SQRTMAX * 2.0;
    }

    if (u <= SQRTMIN)
    {
        // hypot (tiny, tiny) -- avoid underflow
        // This is only necessary to avoid setting the underflow
        // flag.
        u *= SQRTMAX / real.epsilon;
        v *= SQRTMAX / real.epsilon;
        return sqrt(u*u + v*v) * SQRTMIN * real.epsilon;
    }

    if (u * real.epsilon > v)
    {
        // hypot (huge, tiny) = huge
        return u;
    }

    // both are in the normal range
    return sqrt(u*u + v*v);
}

@safe unittest
{
    static real[3][] vals =     // x,y,hypot
        [
            [ 0.0,     0.0,   0.0],
            [ 0.0,    -0.0,   0.0],
            [ -0.0,   -0.0,   0.0],
            [ 3.0,     4.0,   5.0],
            [ -300,   -400,   500],
            [0.0,      7.0,   7.0],
            [9.0,   9*real.epsilon,   9.0],
            [88/(64*sqrt(real.min_normal)), 105/(64*sqrt(real.min_normal)), 137/(64*sqrt(real.min_normal))],
            [88/(128*sqrt(real.min_normal)), 105/(128*sqrt(real.min_normal)), 137/(128*sqrt(real.min_normal))],
            [3*real.min_normal*real.epsilon, 4*real.min_normal*real.epsilon, 5*real.min_normal*real.epsilon],
            [ real.min_normal, real.min_normal, sqrt(2.0L)*real.min_normal],
            [ real.max/sqrt(2.0L), real.max/sqrt(2.0L), real.max],
            [ real.infinity, real.nan, real.infinity],
            [ real.nan, real.infinity, real.infinity],
            [ real.nan, real.nan, real.nan],
            [ real.nan, real.max, real.nan],
            [ real.max, real.nan, real.nan],
        ];
        for (int i = 0; i < vals.length; i++)
        {
            real x = vals[i][0];
            real y = vals[i][1];
            real z = vals[i][2];
            real h = hypot(x, y);
            assert(isIdentical(z,h) || feqrel(z, h) >= real.mant_dig - 1);
        }
}

/**************************************
 * Returns the value of x rounded upward to the next integer
 * (toward positive infinity).
 */
real ceil(real x) @trusted pure nothrow @nogc
{
    version (Win64_DMD_InlineAsm)
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
    else version (MSVC_InlineAsm)
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
    else
    {
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

// ditto
double ceil(double x) @trusted pure nothrow @nogc
{
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

// ditto
float ceil(float x) @trusted pure nothrow @nogc
{
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
    version (Win64_DMD_InlineAsm)
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
    else version (MSVC_InlineAsm)
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
    else
    {
        // Special cases.
        if (isNaN(x) || isInfinity(x) || x == 0.0)
            return x;

        return floorImpl(x);
    }
}

///
@safe pure nothrow @nogc unittest
{
    assert(floor(+123.456L) == +123);
    assert(floor(-123.456L) == -124);
    assert(floor(-1.234L) == -2);
    assert(floor(-0.123L) == -1);
    assert(floor(0.0L) == 0);
    assert(floor(+0.123L) == 0);
    assert(floor(+1.234L) == 1);
    assert(floor(real.infinity) == real.infinity);
    assert(isNaN(floor(real.nan)));
    assert(isNaN(floor(real.init)));
}

// ditto
double floor(double x) @trusted pure nothrow @nogc
{
    // Special cases.
    if (isNaN(x) || isInfinity(x) || x == 0.0)
        return x;

    return floorImpl(x);
}

@safe pure nothrow @nogc unittest
{
    assert(floor(+123.456) == +123);
    assert(floor(-123.456) == -124);
    assert(floor(-1.234) == -2);
    assert(floor(-0.123) == -1);
    assert(floor(0.0) == 0);
    assert(floor(+0.123) == 0);
    assert(floor(+1.234) == 1);
    assert(floor(double.infinity) == double.infinity);
    assert(isNaN(floor(double.nan)));
    assert(isNaN(floor(double.init)));
}

// ditto
float floor(float x) @trusted pure nothrow @nogc
{
    // Special cases.
    if (isNaN(x) || isInfinity(x) || x == 0.0)
        return x;

    return floorImpl(x);
}

@safe pure nothrow @nogc unittest
{
    assert(floor(+123.456f) == +123);
    assert(floor(-123.456f) == -124);
    assert(floor(-1.234f) == -2);
    assert(floor(-0.123f) == -1);
    assert(floor(0.0f) == 0);
    assert(floor(+0.123f) == 0);
    assert(floor(+1.234f) == 1);
    assert(floor(float.infinity) == float.infinity);
    assert(isNaN(floor(float.nan)));
    assert(isNaN(floor(float.init)));
}

/**
 * Round `val` to a multiple of `unit`. `rfunc` specifies the rounding
 * function to use; by default this is `rint`, which uses the current
 * rounding mode.
 */
Unqual!F quantize(alias rfunc = rint, F)(const F val, const F unit)
if (is(typeof(rfunc(F.init)) : F) && isFloatingPoint!F)
{
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
    assert(12345.6789L.quantize(0.01L) == 12345.68L);
    assert(12345.6789L.quantize!floor(0.01L) == 12345.67L);
    assert(12345.6789L.quantize(22.0L) == 12342.0L);
}

///
@safe pure nothrow @nogc unittest
{
    assert(12345.6789L.quantize(0) == 12345.6789L);
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
    // TODO: Compile-time optimization for power-of-two bases?
    return quantize!rfunc(val, pow(cast(F) base, exp));
}

/// ditto
Unqual!F quantize(real base, long exp = 1, alias rfunc = rint, F)(const F val)
if (is(typeof(rfunc(F.init)) : F) && isFloatingPoint!F)
{
    enum unit = cast(F) pow(base, exp);
    return quantize!rfunc(val, unit);
}

///
@safe pure nothrow @nogc unittest
{
    assert(12345.6789L.quantize!10(-2) == 12345.68L);
    assert(12345.6789L.quantize!(10, -2) == 12345.68L);
    assert(12345.6789L.quantize!(10, floor)(-2) == 12345.67L);
    assert(12345.6789L.quantize!(10, -2, floor) == 12345.67L);

    assert(12345.6789L.quantize!22(1) == 12342.0L);
    assert(12345.6789L.quantize!22 == 12342.0L);
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;

    foreach (F; AliasSeq!(real, double, float))
    {
        const maxL10 = cast(int) F.max.log10.floor;
        const maxR10 = pow(cast(F) 10, maxL10);
        assert((cast(F) 0.9L * maxR10).quantize!10(maxL10) ==  maxR10);
        assert((cast(F)-0.9L * maxR10).quantize!10(maxL10) == -maxR10);

        assert(F.max.quantize(F.min_normal) == F.max);
        assert((-F.max).quantize(F.min_normal) == -F.max);
        assert(F.min_normal.quantize(F.max) == 0);
        assert((-F.min_normal).quantize(F.max) == 0);
        assert(F.min_normal.quantize(F.min_normal) == F.min_normal);
        assert((-F.min_normal).quantize(F.min_normal) == -F.min_normal);
    }
}

/******************************************
 * Rounds x to the nearest integer value, using the current rounding
 * mode.
 *
 * Unlike the rint functions, nearbyint does not raise the
 * FE_INEXACT exception.
 */
real nearbyint(real x) @trusted nothrow @nogc
{
    version (CRuntime_Microsoft)
    {
        assert(0);      // not implemented in C library
    }
    else
        return core.stdc.math.nearbyintl(x);
}

/**********************************
 * Rounds x to the nearest integer value, using the current rounding
 * mode.
 * If the return value is not equal to x, the FE_INEXACT
 * exception is raised.
 * $(B nearbyint) performs
 * the same operation, but does not set the FE_INEXACT exception.
 */
real rint(real x) @safe pure nothrow @nogc { pragma(inline, true); return core.math.rint(x); }
//FIXME
///ditto
double rint(double x) @safe pure nothrow @nogc { return rint(cast(real) x); }
//FIXME
///ditto
float rint(float x) @safe pure nothrow @nogc { return rint(cast(real) x); }

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
    version (InlineAsm_X86_Any)
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
        else static if (F.realFormat == RealFormat.ieeeExtended)
        {
            long result;

            // Rounding limit when casting from real(80-bit) to ulong.
            enum real OF = 9.22337203685477580800E18L;

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
 */
real round(real x) @trusted nothrow @nogc
{
    version (CRuntime_Microsoft)
    {
        auto old = FloatingPointControl.getControlState();
        FloatingPointControl.setControlState(
            (old & ~FloatingPointControl.roundingMask) | FloatingPointControl.roundToZero
        );
        x = rint((x >= 0) ? x + 0.5 : x - 0.5);
        FloatingPointControl.setControlState(old);
        return x;
    }
    else
        return core.stdc.math.roundl(x);
}

/**********************************************
 * Return the value of x rounded to the nearest integer.
 *
 * If the fractional part of x is exactly 0.5, the return value is rounded
 * away from zero.
 *
 * $(BLUE This function is Posix-Only.)
 */
long lround(real x) @trusted nothrow @nogc
{
    version (Posix)
        return core.stdc.math.llroundl(x);
    else
        assert(0, "lround not implemented");
}

version (Posix)
{
    @safe nothrow @nogc unittest
    {
        assert(lround(0.49) == 0);
        assert(lround(0.5) == 1);
        assert(lround(1.5) == 2);
    }
}

/****************************************************
 * Returns the integer portion of x, dropping the fractional portion.
 *
 * This is also known as "chop" rounding.
 */
real trunc(real x) @trusted nothrow @nogc
{
    version (Win64_DMD_InlineAsm)
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
    else version (MSVC_InlineAsm)
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
    else
        return core.stdc.math.truncl(x);
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
 * remquo returns n in the parameter n.
 *
 * $(TABLE_SV
 *  $(TR $(TH x)               $(TH y)            $(TH remainder(x, y)) $(TH n)   $(TH invalid?))
 *  $(TR $(TD $(PLUSMN)0.0)    $(TD not 0.0)      $(TD $(PLUSMN)0.0)    $(TD 0.0) $(TD no))
 *  $(TR $(TD $(PLUSMNINF))    $(TD anything)     $(TD $(NAN))          $(TD ?)   $(TD yes))
 *  $(TR $(TD anything)        $(TD $(PLUSMN)0.0) $(TD $(NAN))          $(TD ?)   $(TD yes))
 *  $(TR $(TD != $(PLUSMNINF)) $(TD $(PLUSMNINF)) $(TD x)               $(TD ?)   $(TD no))
 * )
 *
 * $(BLUE `remquo` and `remainder` not supported on Windows.)
 */
real remainder(real x, real y) @trusted nothrow @nogc
{
    version (CRuntime_Microsoft)
    {
        int n;
        return remquo(x, y, n);
    }
    else
        return core.stdc.math.remainderl(x, y);
}

real remquo(real x, real y, out int n) @trusted nothrow @nogc  /// ditto
{
    version (Posix)
        return core.stdc.math.remquol(x, y, &n);
    else
        assert(0, "remquo not implemented");
}


version (IeeeFlagsSupport)
{

/** IEEE exception status flags ('sticky bits')

 These flags indicate that an exceptional floating-point condition has occurred.
 They indicate that a NaN or an infinity has been generated, that a result
 is inexact, or that a signalling NaN has been encountered. If floating-point
 exceptions are enabled (unmasked), a hardware exception will be generated
 instead of setting these flags.
 */
struct IeeeFlags
{
private:
    // The x87 FPU status register is 16 bits.
    // The Pentium SSE2 status register is 32 bits.
    // The ARM and PowerPC FPSCR is a 32-bit register.
    // The SPARC FSR is a 32bit register (64 bits for SPARC 7 & 8, but high bits are uninteresting).
    // The RISC-V (32 & 64 bit) fcsr is 32-bit register.
    uint flags;

    version (CRuntime_Microsoft)
    {
        // Microsoft uses hardware-incompatible custom constants in fenv.h (core.stdc.fenv).
        // Applies to both x87 status word (16 bits) and SSE2 status word(32 bits).
        enum : int
        {
            INEXACT_MASK   = 0x20,
            UNDERFLOW_MASK = 0x10,
            OVERFLOW_MASK  = 0x08,
            DIVBYZERO_MASK = 0x04,
            INVALID_MASK   = 0x01,

            EXCEPTIONS_MASK = 0b11_1111
        }
        // Don't bother about subnormals, they are not supported on most CPUs.
        //  SUBNORMAL_MASK = 0x02;
    }
    else
    {
        enum : int
        {
            INEXACT_MASK    = core.stdc.fenv.FE_INEXACT,
            UNDERFLOW_MASK  = core.stdc.fenv.FE_UNDERFLOW,
            OVERFLOW_MASK   = core.stdc.fenv.FE_OVERFLOW,
            DIVBYZERO_MASK  = core.stdc.fenv.FE_DIVBYZERO,
            INVALID_MASK    = core.stdc.fenv.FE_INVALID,
            EXCEPTIONS_MASK = core.stdc.fenv.FE_ALL_EXCEPT,
        }
    }

private:
    static uint getIeeeFlags()
    {
        version (GNU)
        {
            version (X86_Any)
            {
                ushort sw;
                asm pure nothrow @nogc
                {
                    "fstsw %0" : "=a" (sw);
                }
                // OR the result with the SSE2 status register (MXCSR).
                if (haveSSE)
                {
                    uint mxcsr;
                    asm pure nothrow @nogc
                    {
                        "stmxcsr %0" : "=m" (mxcsr);
                    }
                    return (sw | mxcsr) & EXCEPTIONS_MASK;
                }
                else
                    return sw & EXCEPTIONS_MASK;
            }
            else version (ARM)
            {
                version (ARM_SoftFloat)
                    return 0;
                else
                {
                    uint result = void;
                    asm pure nothrow @nogc
                    {
                        "vmrs %0, FPSCR; and %0, %0, #0x1F;" : "=r" (result);
                    }
                    return result;
                }
            }
            else version (RISCV_Any)
            {
                version (D_SoftFloat)
                    return 0;
                else
                {
                    uint result = void;
                    asm pure nothrow @nogc
                    {
                        "frflags %0" : "=r" (result);
                    }
                    return result;
                }
            }
            else
                assert(0, "Not yet supported");
        }
        else
        version (InlineAsm_X86_Any)
        {
            ushort sw;
            asm pure nothrow @nogc { fstsw sw; }

            // OR the result with the SSE2 status register (MXCSR).
            if (haveSSE)
            {
                uint mxcsr;
                asm pure nothrow @nogc { stmxcsr mxcsr; }
                return (sw | mxcsr) & EXCEPTIONS_MASK;
            }
            else return sw & EXCEPTIONS_MASK;
        }
        else version (SPARC)
        {
           /*
               int retval;
               asm pure nothrow @nogc { st %fsr, retval; }
               return retval;
            */
           assert(0, "Not yet supported");
        }
        else version (ARM)
        {
            assert(false, "Not yet supported.");
        }
        else
            assert(0, "Not yet supported");
    }

    static void resetIeeeFlags() @nogc
    {
        version (GNU)
        {
            version (X86_Any)
            {
                asm nothrow @nogc
                {
                    "fnclex";
                }

                // Also clear exception flags in MXCSR, SSE's control register.
                if (haveSSE)
                {
                    uint mxcsr;
                    asm nothrow @nogc
                    {
                        "stmxcsr %0" : "=m" (mxcsr);
                    }
                    mxcsr &= ~EXCEPTIONS_MASK;
                    asm nothrow @nogc
                    {
                        "ldmxcsr %0" : : "m" (mxcsr);
                    }
                }
            }
            else version (ARM)
            {
                version (ARM_SoftFloat)
                    return;
                else
                {
                    uint old = FloatingPointControl.getControlState();
                    old &= ~0b11111; // http://infocenter.arm.com/help/topic/com.arm.doc.ddi0408i/Chdfifdc.html
                    asm nothrow @nogc
                    {
                        "vmsr FPSCR, %0" : : "r" (old);
                    }
                }
            }
            else version (RISCV_Any)
            {
                version (D_SoftFloat)
                    return;
                else
                {
                    uint newValues = 0x0;
                    asm nothrow @nogc
                    {
                        "fsflags %0" : : "r" (newValues);
                    }
                }
            }
            else
                assert(0, "Not yet supported");
        }
        else
        version (InlineAsm_X86_Any)
        {
            asm nothrow @nogc
            {
                fnclex;
            }

            // Also clear exception flags in MXCSR, SSE's control register.
            if (haveSSE)
            {
                uint mxcsr;
                asm nothrow @nogc { stmxcsr mxcsr; }
                mxcsr &= ~EXCEPTIONS_MASK;
                asm nothrow @nogc { ldmxcsr mxcsr; }
            }
        }
        else
        {
            /* SPARC:
              int tmpval;
              asm pure nothrow @nogc { st %fsr, tmpval; }
              tmpval &=0xFFFF_FC00;
              asm pure nothrow @nogc { ld tmpval, %fsr; }
            */
           assert(0, "Not yet supported");
        }
    }
public:
    version (IeeeFlagsSupport)
    {

     /**
      * The result cannot be represented exactly, so rounding occurred.
      * Example: `x = sin(0.1);`
      */
     @property bool inexact() const { return (flags & INEXACT_MASK) != 0; }

     /**
      * A zero was generated by underflow
      * Example: `x = real.min*real.epsilon/2;`
      */
     @property bool underflow() const { return (flags & UNDERFLOW_MASK) != 0; }

     /**
      * An infinity was generated by overflow
      * Example: `x = real.max*2;`
      */
     @property bool overflow() const { return (flags & OVERFLOW_MASK) != 0; }

     /**
      * An infinity was generated by division by zero
      * Example: `x = 3/0.0;`
      */
     @property bool divByZero() const { return (flags & DIVBYZERO_MASK) != 0; }

     /**
      * A machine NaN was generated.
      * Example: `x = real.infinity * 0.0;`
      */
     @property bool invalid() const { return (flags & INVALID_MASK) != 0; }

     }
}

///
version (IeeeFlagsUnittest)
@system unittest
{
    static void func() {
        int a = 10 * 10;
    }
    pragma(inline, false) static void blockopt(ref real x) {}
    real a = 3.5;
    // Set all the flags to zero
    resetIeeeFlags();
    assert(!ieeeFlags.divByZero);
    blockopt(a); // avoid constant propagation by the optimizer
    // Perform a division by zero.
    a /= 0.0L;
    assert(a == real.infinity);
    assert(ieeeFlags.divByZero);
    blockopt(a); // avoid constant propagation by the optimizer
    // Create a NaN
    a *= 0.0L;
    assert(ieeeFlags.invalid);
    assert(isNaN(a));

    // Check that calling func() has no effect on the
    // status flags.
    IeeeFlags f = ieeeFlags;
    func();
    assert(ieeeFlags == f);
}

version (IeeeFlagsUnittest)
@system unittest
{
    import std.meta : AliasSeq;

    static struct Test
    {
        void delegate() action;
        bool function() ieeeCheck;
    }

    foreach (T; AliasSeq!(float, double, real))
    {
        T x; /* Needs to be here to trick -O. It would optimize away the
            calculations if x were local to the function literals. */
        auto tests = [
            Test(
                () { x = 1; x += 0.1; },
                () => ieeeFlags.inexact
            ),
            Test(
                () { x = T.min_normal; x /= T.max; },
                () => ieeeFlags.underflow
            ),
            Test(
                () { x = T.max; x += T.max; },
                () => ieeeFlags.overflow
            ),
            Test(
                () { x = 1; x /= 0; },
                () => ieeeFlags.divByZero
            ),
            Test(
                () { x = 0; x /= 0; },
                () => ieeeFlags.invalid
            )
        ];
        foreach (test; tests)
        {
            resetIeeeFlags();
            assert(!test.ieeeCheck());
            test.action();
            assert(test.ieeeCheck());
        }
    }
}

/// Set all of the floating-point status flags to false.
void resetIeeeFlags() @nogc { IeeeFlags.resetIeeeFlags(); }

/// Returns: snapshot of the current state of the floating-point status flags
@property IeeeFlags ieeeFlags()
{
   return IeeeFlags(IeeeFlags.getIeeeFlags());
}

} // IeeeFlagsSupport


version (FloatingPointControlSupport)
{

/** Control the Floating point hardware

  Change the IEEE754 floating-point rounding mode and the floating-point
  hardware exceptions.

  By default, the rounding mode is roundToNearest and all hardware exceptions
  are disabled. For most applications, debugging is easier if the $(I division
  by zero), $(I overflow), and $(I invalid operation) exceptions are enabled.
  These three are combined into a $(I severeExceptions) value for convenience.
  Note in particular that if $(I invalidException) is enabled, a hardware trap
  will be generated whenever an uninitialized floating-point variable is used.

  All changes are temporary. The previous state is restored at the
  end of the scope.


Example:
----
{
    FloatingPointControl fpctrl;

    // Enable hardware exceptions for division by zero, overflow to infinity,
    // invalid operations, and uninitialized floating-point variables.
    fpctrl.enableExceptions(FloatingPointControl.severeExceptions);

    // This will generate a hardware exception, if x is a
    // default-initialized floating point variable:
    real x; // Add `= 0` or even `= real.nan` to not throw the exception.
    real y = x * 3.0;

    // The exception is only thrown for default-uninitialized NaN-s.
    // NaN-s with other payload are valid:
    real z = y * real.nan; // ok

    // Changing the rounding mode:
    fpctrl.rounding = FloatingPointControl.roundUp;
    assert(rint(1.1) == 2);

    // The set hardware exceptions will be disabled when leaving this scope.
    // The original rounding mode will also be restored.
}

// Ensure previous values are returned:
assert(!FloatingPointControl.enabledExceptions);
assert(FloatingPointControl.rounding == FloatingPointControl.roundToNearest);
assert(rint(1.1) == 1);
----

 */
struct FloatingPointControl
{
    alias RoundingMode = uint; ///

    version (StdDdoc)
    {
        enum : RoundingMode
        {
            /** IEEE rounding modes.
             * The default mode is roundToNearest.
             *
             *  roundingMask = A mask of all rounding modes.
             */
            roundToNearest,
            roundDown, /// ditto
            roundUp, /// ditto
            roundToZero, /// ditto
            roundingMask, /// ditto
        }
    }
    else version (CRuntime_Microsoft)
    {
        // Microsoft uses hardware-incompatible custom constants in fenv.h (core.stdc.fenv).
        enum : RoundingMode
        {
            roundToNearest = 0x0000,
            roundDown      = 0x0400,
            roundUp        = 0x0800,
            roundToZero    = 0x0C00,
            roundingMask   = roundToNearest | roundDown
                             | roundUp | roundToZero,
        }
    }
    else
    {
        enum : RoundingMode
        {
            roundToNearest = core.stdc.fenv.FE_TONEAREST,
            roundDown      = core.stdc.fenv.FE_DOWNWARD,
            roundUp        = core.stdc.fenv.FE_UPWARD,
            roundToZero    = core.stdc.fenv.FE_TOWARDZERO,
            roundingMask   = roundToNearest | roundDown
                             | roundUp | roundToZero,
        }
    }

    //// Change the floating-point hardware rounding mode
    @property void rounding(RoundingMode newMode) @nogc
    {
        initialize();
        setControlState(cast(ushort)((getControlState() & (-1 - roundingMask)) | (newMode & roundingMask)));
    }

    /// Returns: the currently active rounding mode
    @property static RoundingMode rounding() @nogc
    {
        return cast(RoundingMode)(getControlState() & roundingMask);
    }

    alias ExceptionMask = uint; ///

    version (StdDdoc)
    {
        enum : ExceptionMask
        {
            /** IEEE hardware exceptions.
             *  By default, all exceptions are masked (disabled).
             *
             *  severeExceptions = The overflow, division by zero, and invalid
             *  exceptions.
             */
            subnormalException,
            inexactException, /// ditto
            underflowException, /// ditto
            overflowException, /// ditto
            divByZeroException, /// ditto
            invalidException, /// ditto
            severeExceptions, /// ditto
            allExceptions, /// ditto
        }
    }
    else version (ARM_Any)
    {
        enum : ExceptionMask
        {
            subnormalException    = 0x8000,
            inexactException      = 0x1000,
            underflowException    = 0x0800,
            overflowException     = 0x0400,
            divByZeroException    = 0x0200,
            invalidException      = 0x0100,
            severeExceptions   = overflowException | divByZeroException
                                 | invalidException,
            allExceptions      = severeExceptions | underflowException
                                 | inexactException | subnormalException,
        }
    }
    else version (PPC_Any)
    {
        enum : ExceptionMask
        {
            inexactException      = 0x0008,
            divByZeroException    = 0x0010,
            underflowException    = 0x0020,
            overflowException     = 0x0040,
            invalidException      = 0x0080,
            severeExceptions   = overflowException | divByZeroException
                                 | invalidException,
            allExceptions      = severeExceptions | underflowException
                                 | inexactException,
        }
    }
    else version (HPPA)
    {
        enum : ExceptionMask
        {
            inexactException      = 0x01,
            underflowException    = 0x02,
            overflowException     = 0x04,
            divByZeroException    = 0x08,
            invalidException      = 0x10,
            severeExceptions   = overflowException | divByZeroException
                                 | invalidException,
            allExceptions      = severeExceptions | underflowException
                                 | inexactException,
        }
    }
    else version (MIPS_Any)
    {
        enum : ExceptionMask
        {
            inexactException      = 0x0080,
            divByZeroException    = 0x0400,
            overflowException     = 0x0200,
            underflowException    = 0x0100,
            invalidException      = 0x0800,
            severeExceptions   = overflowException | divByZeroException
                                 | invalidException,
            allExceptions      = severeExceptions | underflowException
                                 | inexactException,
        }
    }
    else version (SPARC_Any)
    {
        enum : ExceptionMask
        {
            inexactException      = 0x0800000,
            divByZeroException    = 0x1000000,
            overflowException     = 0x4000000,
            underflowException    = 0x2000000,
            invalidException      = 0x8000000,
            severeExceptions   = overflowException | divByZeroException
                                 | invalidException,
            allExceptions      = severeExceptions | underflowException
                                 | inexactException,
        }
    }
    else version (IBMZ_Any)
    {
        enum : ExceptionMask
        {
            inexactException      = 0x08000000,
            divByZeroException    = 0x40000000,
            overflowException     = 0x20000000,
            underflowException    = 0x10000000,
            invalidException      = 0x80000000,
            severeExceptions   = overflowException | divByZeroException
                                 | invalidException,
            allExceptions      = severeExceptions | underflowException
                                 | inexactException,
        }
    }
    else version (RISCV_Any)
    {
        enum : ExceptionMask
        {
            inexactException      = 0x01,
            divByZeroException    = 0x02,
            underflowException    = 0x04,
            overflowException     = 0x08,
            invalidException      = 0x10,
            severeExceptions   = overflowException | divByZeroException
                                 | invalidException,
            allExceptions      = severeExceptions | underflowException
                                 | inexactException,
        }
    }
    else version (X86_Any)
    {
        enum : ExceptionMask
        {
            inexactException      = 0x20,
            underflowException    = 0x10,
            overflowException     = 0x08,
            divByZeroException    = 0x04,
            subnormalException    = 0x02,
            invalidException      = 0x01,
            severeExceptions   = overflowException | divByZeroException
                                 | invalidException,
            allExceptions      = severeExceptions | underflowException
                                 | inexactException | subnormalException,
        }
    }
    else
        static assert(false, "Not implemented for this architecture");

public:
    /// Returns: true if the current FPU supports exception trapping
    @property static bool hasExceptionTraps() @safe nothrow @nogc
    {
        version (X86_Any)
            return true;
        else version (PPC_Any)
            return true;
        else version (MIPS_Any)
            return true;
        else version (ARM_Any)
        {
            auto oldState = getControlState();
            // If exceptions are not supported, we set the bit but read it back as zero
            // https://sourceware.org/ml/libc-ports/2012-06/msg00091.html
            setControlState(oldState | divByZeroException);
            immutable result = (getControlState() & allExceptions) != 0;
            setControlState(oldState);
            return result;
        }
        else
            assert(0, "Not yet supported");
    }

    /// Enable (unmask) specific hardware exceptions. Multiple exceptions may be ORed together.
    void enableExceptions(ExceptionMask exceptions) @nogc
    {
        assert(hasExceptionTraps);
        initialize();
        version (X86_Any)
            setControlState(getControlState() & ~(exceptions & allExceptions));
        else
            setControlState(getControlState() | (exceptions & allExceptions));
    }

    /// Disable (mask) specific hardware exceptions. Multiple exceptions may be ORed together.
    void disableExceptions(ExceptionMask exceptions) @nogc
    {
        assert(hasExceptionTraps);
        initialize();
        version (X86_Any)
            setControlState(getControlState() | (exceptions & allExceptions));
        else
            setControlState(getControlState() & ~(exceptions & allExceptions));
    }

    /// Returns: the exceptions which are currently enabled (unmasked)
    @property static ExceptionMask enabledExceptions() @nogc
    {
        assert(hasExceptionTraps);
        version (X86_Any)
            return (getControlState() & allExceptions) ^ allExceptions;
        else
            return (getControlState() & allExceptions);
    }

    ///  Clear all pending exceptions, then restore the original exception state and rounding mode.
    ~this() @nogc
    {
        clearExceptions();
        if (initialized)
            setControlState(savedState);
    }

private:
    ControlState savedState;

    bool initialized = false;

    version (ARM_Any)
    {
        alias ControlState = uint;
    }
    else version (HPPA)
    {
        alias ControlState = uint;
    }
    else version (PPC_Any)
    {
        alias ControlState = uint;
    }
    else version (MIPS_Any)
    {
        alias ControlState = uint;
    }
    else version (SPARC_Any)
    {
        alias ControlState = ulong;
    }
    else version (IBMZ_Any)
    {
        alias ControlState = uint;
    }
    else version (RISCV_Any)
    {
        alias ControlState = uint;
    }
    else version (X86_Any)
    {
        alias ControlState = ushort;
    }
    else
        static assert(false, "Not implemented for this architecture");

    void initialize() @nogc
    {
        // BUG: This works around the absence of this() constructors.
        if (initialized) return;
        clearExceptions();
        savedState = getControlState();
        initialized = true;
    }

    // Clear all pending exceptions
    static void clearExceptions() @nogc
    {
        version (IeeeFlagsSupport)
            resetIeeeFlags();
        else
            static assert(false, "Not implemented for this architecture");
    }

    // Read from the control register
    static ControlState getControlState() @trusted nothrow @nogc
    {
        version (GNU)
        {
            version (X86_Any)
            {
                ControlState cont;
                asm pure nothrow @nogc
                {
                    "fstcw %0" : "=m" (cont);
                }
                return cont;
            }
            else version (AArch64)
            {
                ControlState cont;
                asm pure nothrow @nogc
                {
                    "mrs %0, FPCR;" : "=r" (cont);
                }
                return cont;
            }
            else version (ARM)
            {
                ControlState cont;
                version (ARM_SoftFloat)
                   cont = 0;
                else
                {
                    asm pure nothrow @nogc
                    {
                        "vmrs %0, FPSCR" : "=r" (cont);
                    }
                }
                return cont;
            }
            else version (RISCV_Any)
            {
                version (D_SoftFloat)
                    return 0;
                else
                {
                    ControlState cont;
                    asm pure nothrow @nogc
                    {
                        "frcsr %0" : "=r" (cont);
                    }
                    return cont;
                }
            }
            else
                assert(0, "Not yet supported");
        }
        else
        version (D_InlineAsm_X86)
        {
            short cont;
            asm pure nothrow @nogc
            {
                xor EAX, EAX;
                fstcw cont;
            }
            return cont;
        }
        else
        version (D_InlineAsm_X86_64)
        {
            short cont;
            asm pure nothrow @nogc
            {
                xor RAX, RAX;
                fstcw cont;
            }
            return cont;
        }
        else
            assert(0, "Not yet supported");
    }

    // Set the control register
    static void setControlState(ControlState newState) @trusted nothrow @nogc
    {
        version (GNU)
        {
            version (X86_Any)
            {
                asm nothrow @nogc
                {
                    "fclex; fldcw %0" : : "m" (newState);
                }

                // Also update MXCSR, SSE's control register.
                if (haveSSE)
                {
                    uint mxcsr;
                    asm nothrow @nogc
                    {
                        "stmxcsr %0" : "=m" (mxcsr);
                    }

                    /* In the FPU control register, rounding mode is in bits 10 and
                       11. In MXCSR it's in bits 13 and 14. */
                    mxcsr &= ~(roundingMask << 3);             // delete old rounding mode
                    mxcsr |= (newState & roundingMask) << 3;   // write new rounding mode

                    /* In the FPU control register, masks are bits 0 through 5.
                       In MXCSR they're 7 through 12. */
                    mxcsr &= ~(allExceptions << 7);            // delete old masks
                    mxcsr |= (newState & allExceptions) << 7;  // write new exception masks

                    asm nothrow @nogc
                    {
                        "ldmxcsr %0" : : "m" (mxcsr);
                    }
                }
            }
            else version (AArch64)
            {
                asm nothrow @nogc
                {
                    "msr FPCR, %0;" : : "r" (newState);
                }
            }
            else version (ARM)
            {
                version (ARM_SoftFloat)
                   return;
                else
                {
                    asm nothrow @nogc
                    {
                        "vmsr FPSCR, %0" : : "r" (newState);
                    }
                }
            }
            else version (RISCV_Any)
            {
                version (D_SoftFloat)
                    return;
                else
                {
                    asm nothrow @nogc
                    {
                        "fscsr %0" : : "r" (newState);
                    }
                }
            }
            else
                assert(0, "Not yet supported");
        }
        else
        version (InlineAsm_X86_Any)
        {
            asm nothrow @nogc
            {
                fclex;
                fldcw newState;
            }

            // Also update MXCSR, SSE's control register.
            if (haveSSE)
            {
                uint mxcsr;
                asm nothrow @nogc { stmxcsr mxcsr; }

                /* In the FPU control register, rounding mode is in bits 10 and
                11. In MXCSR it's in bits 13 and 14. */
                mxcsr &= ~(roundingMask << 3);             // delete old rounding mode
                mxcsr |= (newState & roundingMask) << 3;   // write new rounding mode

                /* In the FPU control register, masks are bits 0 through 5.
                In MXCSR they're 7 through 12. */
                mxcsr &= ~(allExceptions << 7);            // delete old masks
                mxcsr |= (newState & allExceptions) << 7;  // write new exception masks

                asm nothrow @nogc { ldmxcsr mxcsr; }
            }
        }
        else
            assert(0, "Not yet supported");
    }
}

@system unittest
{
    void ensureDefaults()
    {
        assert(FloatingPointControl.rounding
               == FloatingPointControl.roundToNearest);
        if (FloatingPointControl.hasExceptionTraps)
            assert(FloatingPointControl.enabledExceptions == 0);
    }

    {
        FloatingPointControl ctrl;
    }
    ensureDefaults();

    {
        FloatingPointControl ctrl;
        ctrl.rounding = FloatingPointControl.roundDown;
        assert(FloatingPointControl.rounding == FloatingPointControl.roundDown);
    }
    ensureDefaults();

    if (FloatingPointControl.hasExceptionTraps)
    {
        FloatingPointControl ctrl;
        ctrl.enableExceptions(FloatingPointControl.divByZeroException
                              | FloatingPointControl.overflowException);
        assert(ctrl.enabledExceptions ==
               (FloatingPointControl.divByZeroException
                | FloatingPointControl.overflowException));

        ctrl.rounding = FloatingPointControl.roundUp;
        assert(FloatingPointControl.rounding == FloatingPointControl.roundUp);
    }
    ensureDefaults();
}

version (FloatingPointControlUnittest)
@system unittest // rounding
{
    import std.meta : AliasSeq;

    foreach (T; AliasSeq!(float, double, real))
    {
        /* Be careful with changing the rounding mode, it interferes
         * with common subexpressions. Changing rounding modes should
         * be done with separate functions that are not inlined.
         */

        {
            static T addRound(T)(uint rm)
            {
                pragma(inline, false) static void blockopt(ref T x) {}
                pragma(inline, false);
                FloatingPointControl fpctrl;
                fpctrl.rounding = rm;
                T x = 1;
                blockopt(x); // avoid constant propagation by the optimizer
                x += 0.1;
                return x;
            }

            T u = addRound!(T)(FloatingPointControl.roundUp);
            T d = addRound!(T)(FloatingPointControl.roundDown);
            T z = addRound!(T)(FloatingPointControl.roundToZero);

            assert(u > d);
            assert(z == d);
        }

        {
            static T subRound(T)(uint rm)
            {
                pragma(inline, false) static void blockopt(ref T x) {}
                pragma(inline, false);
                FloatingPointControl fpctrl;
                fpctrl.rounding = rm;
                T x = -1;
                blockopt(x); // avoid constant propagation by the optimizer
                x -= 0.1;
                return x;
            }

            T u = subRound!(T)(FloatingPointControl.roundUp);
            T d = subRound!(T)(FloatingPointControl.roundDown);
            T z = subRound!(T)(FloatingPointControl.roundToZero);

            assert(u > d);
            assert(z == u);
        }
    }
}

} // FloatingPointControlSupport


/*********************************
 * Determines if $(D_PARAM x) is NaN.
 * Params:
 *  x = a floating point number.
 * Returns:
 *  $(D true) if $(D_PARAM x) is Nan.
 */
bool isNaN(X)(X x) @nogc @trusted pure nothrow
if (isFloatingPoint!(X))
{
    alias F = floatTraits!(X);
    static if (F.realFormat == RealFormat.ieeeSingle)
    {
        const uint p = *cast(uint *)&x;
        return ((p & 0x7F80_0000) == 0x7F80_0000)
            && p & 0x007F_FFFF; // not infinity
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        const ulong  p = *cast(ulong *)&x;
        return ((p & 0x7FF0_0000_0000_0000) == 0x7FF0_0000_0000_0000)
            && p & 0x000F_FFFF_FFFF_FFFF; // not infinity
    }
    else static if (F.realFormat == RealFormat.ieeeExtended)
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

    foreach (T; AliasSeq!(float, double, real))
    {
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
    }
}

/*********************************
 * Determines if $(D_PARAM x) is finite.
 * Params:
 *  x = a floating point number.
 * Returns:
 *  $(D true) if $(D_PARAM x) is finite.
 */
bool isFinite(X)(X x) @trusted pure nothrow @nogc
{
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
}


/*********************************
 * Determines if $(D_PARAM x) is normalized.
 *
 * A normalized number must not be zero, subnormal, infinite nor $(NAN).
 *
 * Params:
 *  x = a floating point number.
 * Returns:
 *  $(D true) if $(D_PARAM x) is normalized.
 */

/* Need one for each format because subnormal floats might
 * be converted to normal reals.
 */
bool isNormal(X)(X x) @trusted pure nothrow @nogc
{
    alias F = floatTraits!(X);
    static if (F.realFormat == RealFormat.ibmExtended)
    {
        // doubledouble is normal if the least significant part is normal.
        return isNormal((cast(double*)&x)[MANTISSA_LSB]);
    }
    else
    {
        ushort e = F.EXPMASK & (cast(ushort *)&x)[F.EXPPOS_SHORT];
        return (e != F.EXPMASK && e != 0);
    }
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

/*********************************
 * Determines if $(D_PARAM x) is subnormal.
 *
 * Subnormals (also known as "denormal number"), have a 0 exponent
 * and a 0 most significant mantissa bit.
 *
 * Params:
 *  x = a floating point number.
 * Returns:
 *  $(D true) if $(D_PARAM x) is a denormal number.
 */
bool isSubnormal(X)(X x) @trusted pure nothrow @nogc
{
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
    else static if (F.realFormat == RealFormat.ieeeExtended)
    {
        ushort* pe = cast(ushort *)&x;
        long*   ps = cast(long *)&x;

        return (pe[F.EXPPOS_SHORT] & F.EXPMASK) == 0 && *ps > 0;
    }
    else static if (F.realFormat == RealFormat.ibmExtended)
    {
        return isSubnormal((cast(double*)&x)[MANTISSA_MSB]);
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

    foreach (T; AliasSeq!(float, double, real))
    {
        T f;
        for (f = 1.0; !isSubnormal(f); f /= 2)
            assert(f != 0);
    }
}

/*********************************
 * Determines if $(D_PARAM x) is $(PLUSMN)$(INFIN).
 * Params:
 *  x = a floating point number.
 * Returns:
 *  $(D true) if $(D_PARAM x) is $(PLUSMN)$(INFIN).
 */
bool isInfinity(X)(X x) @nogc @trusted pure nothrow
if (isFloatingPoint!(X))
{
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
    else static if (F.realFormat == RealFormat.ieeeExtended)
    {
        const ushort e = cast(ushort)(F.EXPMASK & (cast(ushort *)&x)[F.EXPPOS_SHORT]);
        const ulong ps = *cast(ulong *)&x;

        // On Motorola 68K, infinity can have hidden bit = 1 or 0. On x86, it is always 1.
        return e == F.EXPMASK && (ps & 0x7FFF_FFFF_FFFF_FFFF) == 0;
    }
    else static if (F.realFormat == RealFormat.ibmExtended)
    {
        return (((cast(ulong *)&x)[MANTISSA_MSB]) & 0x7FFF_FFFF_FFFF_FFFF)
            == 0x7FF8_0000_0000_0000;
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

/*********************************
 * Is the binary representation of x identical to y?
 *
 * Same as ==, except that positive and negative zero are not identical,
 * and two $(NAN)s are identical if they have the same 'payload'.
 */
bool isIdentical(real x, real y) @trusted pure nothrow @nogc
{
    // We're doing a bitwise comparison so the endianness is irrelevant.
    long*   pxs = cast(long *)&x;
    long*   pys = cast(long *)&y;
    alias F = floatTraits!(real);
    static if (F.realFormat == RealFormat.ieeeDouble)
    {
        return pxs[0] == pys[0];
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple
                 || F.realFormat == RealFormat.ibmExtended)
    {
        return pxs[0] == pys[0] && pxs[1] == pys[1];
    }
    else
    {
        ushort* pxe = cast(ushort *)&x;
        ushort* pye = cast(ushort *)&y;
        return pxe[4] == pye[4] && pxs[0] == pys[0];
    }
}

/*********************************
 * Return 1 if sign bit of e is set, 0 if not.
 */
int signbit(X)(X x) @nogc @trusted pure nothrow
{
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


/*********************************
 * Return a value composed of to with from's sign bit.
 */
R copysign(R, X)(R to, X from) @trusted pure nothrow @nogc
if (isFloatingPoint!(R) && isFloatingPoint!(X))
{
    ubyte* pto   = cast(ubyte *)&to;
    const ubyte* pfrom = cast(ubyte *)&from;

    alias T = floatTraits!(R);
    alias F = floatTraits!(X);
    pto[T.SIGNPOS_BYTE] &= 0x7F;
    pto[T.SIGNPOS_BYTE] |= pfrom[F.SIGNPOS_BYTE] & 0x80;
    return to;
}

// ditto
R copysign(R, X)(X to, R from) @trusted pure nothrow @nogc
if (isIntegral!(X) && isFloatingPoint!(R))
{
    return copysign(cast(R) to, from);
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;

    foreach (X; AliasSeq!(float, double, real, int, long))
    {
        foreach (Y; AliasSeq!(float, double, real))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
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
        }();
    }
}

/*********************************
Returns $(D -1) if $(D x < 0), $(D x) if $(D x == 0), $(D 1) if
$(D x > 0), and $(NAN) if x==$(NAN).
 */
F sgn(F)(F x) @safe pure nothrow @nogc
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
    alias F = floatTraits!(real);
    static if (F.realFormat == RealFormat.ieeeExtended)
    {
        // real80 (in x86 real format, the implied bit is actually
        // not implied but a real bit which is stored in the real)
        ulong v = 3; // implied bit = 1, quiet bit = 1
    }
    else
    {
        ulong v = 1; // no implied bit. quiet bit = 1
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

    static if (F.realFormat == RealFormat.ieeeDouble)
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

@system pure nothrow @nogc unittest // not @safe because taking address of local.
{
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
    //  assert(isNaN(x));
    alias F = floatTraits!(real);
    static if (F.realFormat == RealFormat.ieeeDouble)
    {
        ulong m = *cast(ulong *)(&x);
        // Make it look like an 80-bit significand.
        // Skip exponent, and quiet bit
        m &= 0x0007_FFFF_FFFF_FFFF;
        m <<= 11;
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple)
    {
        version (LittleEndian)
        {
            ulong m = *cast(ulong*)(6+cast(ubyte*)(&x));
        }
        else
        {
            ulong m = *cast(ulong*)(2+cast(ubyte*)(&x));
        }

        m >>= 1; // there's no implicit bit
    }
    else
    {
        ulong m = *cast(ulong *)(&x);
    }

    // ignore implicit bit and quiet bit

    const ulong f = m & 0x3FFF_FF00_0000_0000L;

    ulong w = f >>> 40;
            w |= (m & 0x00FF_FFFF_F800L) << (22 - 11);
            w |= (m & 0x7FF) << 51;
            return w;
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
    alias F = floatTraits!(real);
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
    else static if (F.realFormat == RealFormat.ieeeExtended)
    {
        // For 80-bit reals, the "implied bit" is a nuisance...
        ushort *pe = cast(ushort *)&x;
        ulong  *ps = cast(ulong  *)&x;

        if ((pe[F.EXPPOS_SHORT] & F.EXPMASK) == F.EXPMASK)
        {
            // First, deal with NANs and infinity
            if (x == -real.infinity) return -real.max;
            return x; // +Inf and NaN are unchanged.
        }
        if (pe[F.EXPPOS_SHORT] & 0x8000)
        {
            // Negative number -- need to decrease the significand
            --*ps;
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
            ++*ps;
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
    ulong *ps = cast(ulong *)&x;

    if ((*ps & 0x7FF0_0000_0000_0000) == 0x7FF0_0000_0000_0000)
    {
        // First, deal with NANs and infinity
        if (x == -x.infinity) return -x.max;
        return x; // +INF and NAN are unchanged.
    }
    if (*ps & 0x8000_0000_0000_0000)    // Negative number
    {
        if (*ps == 0x8000_0000_0000_0000) // it was negative zero
        {
            *ps = 0x0000_0000_0000_0001; // change to smallest subnormal
            return x;
        }
        --*ps;
    }
    else
    {   // Positive number
        ++*ps;
    }
    return x;
}

/** ditto */
float nextUp(float x) @trusted pure nothrow @nogc
{
    uint *ps = cast(uint *)&x;

    if ((*ps & 0x7F80_0000) == 0x7F80_0000)
    {
        // First, deal with NANs and infinity
        if (x == -x.infinity) return -x.max;

        return x; // +INF and NAN are unchanged.
    }
    if (*ps & 0x8000_0000)   // Negative number
    {
        if (*ps == 0x8000_0000) // it was negative zero
        {
            *ps = 0x0000_0001; // change to smallest subnormal
            return x;
        }

        --*ps;
    }
    else
    {
        // Positive number
        ++*ps;
    }
    return x;
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
    static if (floatTraits!(real).realFormat == RealFormat.ieeeExtended)
    {

        // Tests for 80-bit reals
        assert(isIdentical(nextUp(NaN(0xABC)), NaN(0xABC)));
        // negative numbers
        assert( nextUp(-real.infinity) == -real.max );
        assert( nextUp(-1.0L-real.epsilon) == -1.0 );
        assert( nextUp(-2.0L) == -2.0 + real.epsilon);
        // subnormals and zero
        assert( nextUp(-real.min_normal) == -real.min_normal*(1-real.epsilon) );
        assert( nextUp(-real.min_normal*(1-real.epsilon)) == -real.min_normal*(1-2*real.epsilon) );
        assert( isIdentical(-0.0L, nextUp(-real.min_normal*real.epsilon)) );
        assert( nextUp(-0.0L) == real.min_normal*real.epsilon );
        assert( nextUp(0.0L) == real.min_normal*real.epsilon );
        assert( nextUp(real.min_normal*(1-real.epsilon)) == real.min_normal );
        assert( nextUp(real.min_normal) == real.min_normal*(1+real.epsilon) );
        // positive numbers
        assert( nextUp(1.0L) == 1.0 + real.epsilon );
        assert( nextUp(2.0L-real.epsilon) == 2.0 );
        assert( nextUp(real.max) == real.infinity );
        assert( nextUp(real.infinity)==real.infinity );
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
}



/******************************************
 * Calculates the next representable value after x in the direction of y.
 *
 * If y > x, the result will be the next largest floating-point value;
 * if y < x, the result will be the next smallest value.
 * If x == y, the result is y.
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
    if (x == y) return y;
    return ((y>x) ? nextUp(x) :  nextDown(x));
}

///
@safe pure nothrow @nogc unittest
{
    float a = 1;
    assert(is(typeof(nextafter(a, a)) == float));
    assert(nextafter(a, a.infinity) > a);

    double b = 2;
    assert(is(typeof(nextafter(b, b)) == double));
    assert(nextafter(b, b.infinity) > b);

    real c = 3;
    assert(is(typeof(nextafter(c, c)) == real));
    assert(nextafter(c, c.infinity) > c);
}

//real nexttoward(real x, real y) { return core.stdc.math.nexttowardl(x, y); }

/*******************************************
 * Returns the positive difference between x and y.
 * Returns:
 *      $(TABLE_SV
 *      $(TR $(TH x, y)       $(TH fdim(x, y)))
 *      $(TR $(TD x $(GT) y)  $(TD x - y))
 *      $(TR $(TD x $(LT)= y) $(TD +0.0))
 *      )
 */
real fdim(real x, real y) @safe pure nothrow @nogc { return (x > y) ? x - y : +0.0; }

/****************************************
 * Returns the larger of x and y.
 */
real fmax(real x, real y) @safe pure nothrow @nogc { return x > y ? x : y; }

/****************************************
 * Returns the smaller of x and y.
 */
real fmin(real x, real y) @safe pure nothrow @nogc { return x < y ? x : y; }

/**************************************
 * Returns (x * y) + z, rounding only once according to the
 * current rounding mode.
 *
 * BUGS: Not currently implemented - rounds twice.
 */
real fma(real x, real y, real z) @safe pure nothrow @nogc { return (x * y) + z; }

/*******************************************************************
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
        switch (n)
        {
        case -1:
            return 1 / x;
        case -2:
            return 1 / (x * x);
        default:
        }

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

@safe pure nothrow @nogc unittest
{
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

    // Test disabled on most targets.
    // See https://issues.dlang.org/show_bug.cgi?id=5628
    version (X86_64)   enum BUG5628 = false;
    else version (ARM) enum BUG5628 = false;
    else version (GNU) enum BUG5628 = false;
    else               enum BUG5628 = true;

    static if (BUG5628)
    {
        assert(pow(xd, neg2) == 1 / (x * x));
        assert(pow(xf, neg8) == 1 / ((x * x) * (x * x) * (x * x) * (x * x)));
    }

    assert(feqrel(pow(x, neg3),  1 / (x * x * x)) >= real.mant_dig - 1);
}

@system unittest
{
    assert(equalsDigit(pow(2.0L, 10.0L), 1024, 19));
}

/** Compute the value of an integer x, raised to the power of a positive
 * integer n.
 *
 *  If both x and n are 0, the result is 1.
 *  If n is negative, an integer divide error will occur at runtime,
 * regardless of the value of x.
 */
typeof(Unqual!(F).init * Unqual!(G).init) pow(F, G)(F x, G n) @nogc @trusted pure nothrow
if (isIntegral!(F) && isIntegral!(G))
{
    if (n<0) return x/0; // Only support positive powers
    typeof(return) p, v = void;
    Unqual!G m = n;

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

/**Computes integer to floating point powers.*/
real pow(I, F)(I x, F y) @nogc @trusted pure nothrow
if (isIntegral!I && isFloatingPoint!F)
{
    return pow(cast(real) x, cast(Unqual!F) y);
}

/*********************************************
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
 * $(TR $(TD $(PLUSMN)1.0)   $(TD $(PLUSMN)$(INFIN))          $(TD $(NAN))
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
    alias Float = typeof(return);

    static real impl(real x, real y) @nogc pure nothrow
    {
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
            if (fabs(x) > 1)
            {
                if (signbit(y))
                    return +0.0;
                else
                    return F.infinity;
            }
            else if (fabs(x) == 1)
            {
                return y * 0; // generate NaN.
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
                if (floor(y) != y)
                    return sqrt(x); // Complex result -- create a NaN

                const hy = ldexp(y, -1);
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
            Float w = exp2(y * log2(x));
            return sign * w;
        }
    }
    return impl(x, y);
}

@safe pure nothrow @nogc unittest
{
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

    // Issue #14786 fixed
    immutable real maxOdd = pow(2.0L, real.mant_dig) - 1.0L;
    assert(pow(-1.0L,  maxOdd) == -1.0L);
    assert(pow(-1.0L, -maxOdd) == -1.0L);
    assert(pow(-1.0L, maxOdd + 1.0L) == 1.0L);
    assert(pow(-1.0L, -maxOdd + 1.0L) == 1.0L);
    assert(pow(-1.0L, maxOdd - 1.0L) == 1.0L);
    assert(pow(-1.0L, -maxOdd - 1.0L) == 1.0L);

    // Now, actual numbers.
    assert(approxEqual(pow(two, three), 8.0));
    assert(approxEqual(pow(two, -2.5), 0.1767767));

    // Test integer to float power.
    immutable uint twoI = 2;
    assert(approxEqual(pow(twoI, three), 8.0));
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
    /* Public Domain. Author: Don Clugston, 18 Aug 2005.
     */
    alias F = floatTraits!(X);
    static if (F.realFormat == RealFormat.ibmExtended)
    {
        if (cast(double*)(&x)[MANTISSA_MSB] == cast(double*)(&y)[MANTISSA_MSB])
        {
            return double.mant_dig
            + feqrel(cast(double*)(&x)[MANTISSA_LSB],
                    cast(double*)(&y)[MANTISSA_LSB]);
        }
        else
        {
            return feqrel(cast(double*)(&x)[MANTISSA_MSB],
                    cast(double*)(&y)[MANTISSA_MSB]);
        }
    }
    else
    {
        static assert(F.realFormat == RealFormat.ieeeSingle
                    || F.realFormat == RealFormat.ieeeDouble
                    || F.realFormat == RealFormat.ieeeExtended
                    || F.realFormat == RealFormat.ieeeQuadruple);

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

package: // Not public yet
/* Return the value that lies halfway between x and y on the IEEE number line.
 *
 * Formally, the result is the arithmetic mean of the binary significands of x
 * and y, multiplied by the geometric mean of the binary exponents of x and y.
 * x and y must have the same sign, and must not be NaN.
 * Note: this function is useful for ensuring O(log n) behaviour in algorithms
 * involving a 'binary chop'.
 *
 * Special cases:
 * If x and y are within a factor of 2, (ie, feqrel(x, y) > 0), the return value
 * is the arithmetic mean (x + y) / 2.
 * If x and y are even powers of 2, the return value is the geometric mean,
 *   ieeeMean(x, y) = sqrt(x * y).
 *
 */
T ieeeMean(T)(const T x, const T y)  @trusted pure nothrow @nogc
in
{
    // both x and y must have the same sign, and must not be NaN.
    assert(signbit(x) == signbit(y));
    assert(x == x && y == y);
}
body
{
    // Runtime behaviour for contract violation:
    // If signs are opposite, or one is a NaN, return 0.
    if (!((x >= 0 && y >= 0) || (x <= 0 && y <= 0))) return 0.0;

    // The implementation is simple: cast x and y to integers,
    // average them (avoiding overflow), and cast the result back to a floating-point number.

    alias F = floatTraits!(T);
    T u;
    static if (F.realFormat == RealFormat.ieeeExtended)
    {
        // There's slight additional complexity because they are actually
        // 79-bit reals...
        ushort *ue = cast(ushort *)&u;
        ulong *ul = cast(ulong *)&u;
        ushort *xe = cast(ushort *)&x;
        ulong *xl = cast(ulong *)&x;
        ushort *ye = cast(ushort *)&y;
        ulong *yl = cast(ulong *)&y;

        // Ignore the useless implicit bit. (Bonus: this prevents overflows)
        ulong m = ((*xl) & 0x7FFF_FFFF_FFFF_FFFFL) + ((*yl) & 0x7FFF_FFFF_FFFF_FFFFL);

        // @@@ BUG? @@@
        // Cast shouldn't be here
        ushort e = cast(ushort) ((xe[F.EXPPOS_SHORT] & F.EXPMASK)
                                 + (ye[F.EXPPOS_SHORT] & F.EXPMASK));
        if (m & 0x8000_0000_0000_0000L)
        {
            ++e;
            m &= 0x7FFF_FFFF_FFFF_FFFFL;
        }
        // Now do a multi-byte right shift
        const uint c = e & 1; // carry
        e >>= 1;
        m >>>= 1;
        if (c)
            m |= 0x4000_0000_0000_0000L; // shift carry into significand
        if (e)
            *ul = m | 0x8000_0000_0000_0000L; // set implicit bit...
        else
            *ul = m; // ... unless exponent is 0 (subnormal or zero).

        ue[4]= e | (xe[F.EXPPOS_SHORT]& 0x8000); // restore sign bit
    }
    else static if (F.realFormat == RealFormat.ieeeQuadruple)
    {
        // This would be trivial if 'ucent' were implemented...
        ulong *ul = cast(ulong *)&u;
        ulong *xl = cast(ulong *)&x;
        ulong *yl = cast(ulong *)&y;

        // Multi-byte add, then multi-byte right shift.
        import core.checkedint : addu;
        bool carry;
        ulong ml = addu(xl[MANTISSA_LSB], yl[MANTISSA_LSB], carry);

        ulong mh = carry + (xl[MANTISSA_MSB] & 0x7FFF_FFFF_FFFF_FFFFL) +
            (yl[MANTISSA_MSB] & 0x7FFF_FFFF_FFFF_FFFFL);

        ul[MANTISSA_MSB] = (mh >>> 1) | (xl[MANTISSA_MSB] & 0x8000_0000_0000_0000);
        ul[MANTISSA_LSB] = (ml >>> 1) | (mh & 1) << 63;
    }
    else static if (F.realFormat == RealFormat.ieeeDouble)
    {
        ulong *ul = cast(ulong *)&u;
        ulong *xl = cast(ulong *)&x;
        ulong *yl = cast(ulong *)&y;
        ulong m = (((*xl) & 0x7FFF_FFFF_FFFF_FFFFL)
                   + ((*yl) & 0x7FFF_FFFF_FFFF_FFFFL)) >>> 1;
        m |= ((*xl) & 0x8000_0000_0000_0000L);
        *ul = m;
    }
    else static if (F.realFormat == RealFormat.ieeeSingle)
    {
        uint *ul = cast(uint *)&u;
        uint *xl = cast(uint *)&x;
        uint *yl = cast(uint *)&y;
        uint m = (((*xl) & 0x7FFF_FFFF) + ((*yl) & 0x7FFF_FFFF)) >>> 1;
        m |= ((*xl) & 0x8000_0000);
        *ul = m;
    }
    else
    {
        assert(0, "Not implemented");
    }
    return u;
}

@safe pure nothrow @nogc unittest
{
    assert(ieeeMean(-0.0,-1e-20)<0);
    assert(ieeeMean(0.0,1e-20)>0);

    assert(ieeeMean(1.0L,4.0L)==2L);
    assert(ieeeMean(2.0*1.013,8.0*1.013)==4*1.013);
    assert(ieeeMean(-1.0L,-4.0L)==-2L);
    assert(ieeeMean(-1.0,-4.0)==-2);
    assert(ieeeMean(-1.0f,-4.0f)==-2f);
    assert(ieeeMean(-1.0,-2.0)==-1.5);
    assert(ieeeMean(-1*(1+8*real.epsilon),-2*(1+8*real.epsilon))
                 ==-1.5*(1+5*real.epsilon));
    assert(ieeeMean(0x1p60,0x1p-10)==0x1p25);

    static if (floatTraits!(real).realFormat == RealFormat.ieeeExtended)
    {
      assert(ieeeMean(1.0L,real.infinity)==0x1p8192L);
      assert(ieeeMean(0.0L,real.infinity)==1.5);
    }
    assert(ieeeMean(0.5*real.min_normal*(1-4*real.epsilon),0.5*real.min_normal)
           == 0.5*real.min_normal*(1-2*real.epsilon));
}

public:


/***********************************
 * Evaluate polynomial A(x) = $(SUB a, 0) + $(SUB a, 1)x + $(SUB a, 2)$(POWER x,2)
 *                          + $(SUB a,3)$(POWER x,3); ...
 *
 * Uses Horner's rule A(x) = $(SUB a, 0) + x($(SUB a, 1) + x($(SUB a, 2)
 *                         + x($(SUB a, 3) + ...)))
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
body
{
    static if (is(Unqual!T2 == real))
    {
        return polyImpl(x, A);
    }
    else
    {
        return polyImplBase(x, A);
    }
}

///
@safe nothrow @nogc unittest
{
    real x = 3.1;
    static real[] pp = [56.1, 32.7, 6];

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
                ;
            }
        }
        else version (linux)
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
                ;
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
                ;
            }
        }
        else version (FreeBSD)
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
                ;
            }
        }
        else version (Solaris)
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
                ;
            }
        }
        else version (DragonFlyBSD)
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
                ;
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
   Computes whether two values are approximately equal, admitting a maximum
   relative difference, and a maximum absolute difference.

   Params:
        lhs = First item to compare.
        rhs = Second item to compare.
        maxRelDiff = Maximum allowable difference relative to `rhs`.
        maxAbsDiff = Maximum absolute difference.

   Returns:
       `true` if the two items are approximately equal under either criterium.
       If one item is a range, and the other is a single value, then the result
       is the logical and-ing of calling `approxEqual` on each element of the
       ranged item against the single item. If both items are ranges, then
       `approxEqual` returns `true` if and only if the ranges have the same
       number of elements and if `approxEqual` evaluates to `true` for each
       pair of elements.
 */
bool approxEqual(T, U, V)(T lhs, U rhs, V maxRelDiff, V maxAbsDiff = 1e-5)
{
    import std.range.primitives : empty, front, isInputRange, popFront;
    static if (isInputRange!T)
    {
        static if (isInputRange!U)
        {
            // Two ranges
            for (;; lhs.popFront(), rhs.popFront())
            {
                if (lhs.empty) return rhs.empty;
                if (rhs.empty) return lhs.empty;
                if (!approxEqual(lhs.front, rhs.front, maxRelDiff, maxAbsDiff))
                    return false;
            }
        }
        else static if (isIntegral!U)
        {
            // convert rhs to real
            return approxEqual(lhs, real(rhs), maxRelDiff, maxAbsDiff);
        }
        else
        {
            // lhs is range, rhs is number
            for (; !lhs.empty; lhs.popFront())
            {
                if (!approxEqual(lhs.front, rhs, maxRelDiff, maxAbsDiff))
                    return false;
            }
            return true;
        }
    }
    else
    {
        static if (isInputRange!U)
        {
            // lhs is number, rhs is range
            for (; !rhs.empty; rhs.popFront())
            {
                if (!approxEqual(lhs, rhs.front, maxRelDiff, maxAbsDiff))
                    return false;
            }
            return true;
        }
        else static if (isIntegral!T || isIntegral!U)
        {
            // convert both lhs and rhs to real
            return approxEqual(real(lhs), real(rhs), maxRelDiff, maxAbsDiff);
        }
        else
        {
            // two numbers
            //static assert(is(T : real) && is(U : real));
            if (rhs == 0)
            {
                return fabs(lhs) <= maxAbsDiff;
            }
            static if (is(typeof(lhs.infinity)) && is(typeof(rhs.infinity)))
            {
                if (lhs == lhs.infinity && rhs == rhs.infinity ||
                    lhs == -lhs.infinity && rhs == -rhs.infinity) return true;
            }
            return fabs((lhs - rhs) / rhs) <= maxRelDiff
                || maxAbsDiff != 0 && fabs(lhs - rhs) <= maxAbsDiff;
        }
    }
}

/**
   Returns $(D approxEqual(lhs, rhs, 1e-2, 1e-5)).
 */
bool approxEqual(T, U)(T lhs, U rhs)
{
    return approxEqual(lhs, rhs, 1e-2, 1e-5);
}

///
@safe pure nothrow unittest
{
    assert(approxEqual(1.0, 1.0099));
    assert(!approxEqual(1.0, 1.011));
    float[] arr1 = [ 1.0, 2.0, 3.0 ];
    double[] arr2 = [ 1.001, 1.999, 3 ];
    assert(approxEqual(arr1, arr2));

    real num = real.infinity;
    assert(num == real.infinity);  // Passes.
    assert(approxEqual(num, real.infinity));  // Fails.
    num = -real.infinity;
    assert(num == -real.infinity);  // Passes.
    assert(approxEqual(num, -real.infinity));  // Fails.

    assert(!approxEqual(3, 0));
    assert(approxEqual(3, 3));
    assert(approxEqual(3.0, 3));
    assert(approxEqual([3, 3, 3], 3.0));
    assert(approxEqual([3.0, 3.0, 3.0], 3));
    int a = 10;
    assert(approxEqual(10, a));
}

@safe pure nothrow @nogc unittest
{
    real num = real.infinity;
    assert(num == real.infinity);  // Passes.
    assert(approxEqual(num, real.infinity));  // Fails.
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

@safe pure nothrow @nogc unittest
{
    float f = fabs(-2.0f);
    assert(f == 2);

    double d = fabs(-2.0);
    assert(d == 2);

    real r = fabs(-2.0L);
    assert(r == 2);
}

@safe pure nothrow @nogc unittest
{
    float f = sin(-2.0f);
    assert(fabs(f - -0.909297f) < .00001);

    double d = sin(-2.0);
    assert(fabs(d - -0.909297f) < .00001);

    real r = sin(-2.0L);
    assert(fabs(r - -0.909297f) < .00001);
}

@safe pure nothrow @nogc unittest
{
    float f = cos(-2.0f);
    assert(fabs(f - -0.416147f) < .00001);

    double d = cos(-2.0);
    assert(fabs(d - -0.416147f) < .00001);

    real r = cos(-2.0L);
    assert(fabs(r - -0.416147f) < .00001);
}

@safe pure nothrow @nogc unittest
{
    float f = tan(-2.0f);
    assert(fabs(f - 2.18504f) < .00001);

    double d = tan(-2.0);
    assert(fabs(d - 2.18504f) < .00001);

    real r = tan(-2.0L);
    assert(fabs(r - 2.18504f) < .00001);

    // Verify correct behavior for large inputs
    assert(!isNaN(tan(0x1p63)));
    assert(!isNaN(tan(0x1p300L)));
    assert(!isNaN(tan(-0x1p63)));
    assert(!isNaN(tan(-0x1p300L)));
}

@safe pure nothrow unittest
{
    // issue 6381: floor/ceil should be usable in pure function.
    auto x = floor(1.2);
    auto y = ceil(1.2);
}

@safe pure nothrow unittest
{
    // relative comparison depends on rhs, make sure proper side is used when
    // comparing range to single value. Based on bugzilla issue 15763
    auto a = [2e-3 - 1e-5];
    auto b = 2e-3 + 1e-5;
    assert(a[0].approxEqual(b));
    assert(!b.approxEqual(a[0]));
    assert(a.approxEqual(b));
    assert(!b.approxEqual(a));
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
 *      negative value if $(D x) precedes $(D y) in the order specified above;
 *      0 if $(D x) and $(D y) are identical, and positive value otherwise.
 *
 * See_Also:
 *      $(MYREF isIdentical)
 * Standards: Conforms to IEEE 754-2008
 */
int cmp(T)(const(T) x, const(T) y) @nogc @trusted pure nothrow
if (isFloatingPoint!T)
{
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
    foreach (T; AliasSeq!(float, double, real))
    {
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
            return cast(Unqual!T) (val < 0 ? -(T(1) << bsr(0 - val) + type) : T(1) << bsr(val) + type);
        else
            return cast(Unqual!T) (T(1) << bsr(val) + type);
    }
}

private T powFloatingPointImpl(PowType type, T)(T x)
{
    if (!x.isFinite)
        return x;

    if (!x)
        return x;

    int exp;
    auto y = frexp(x, exp);

    static if (type == PowType.ceil)
        y = ldexp(cast(T) 0.5, exp + 1);
    else
        y = ldexp(cast(T) 0.5, exp);

    if (!y.isFinite)
        return cast(T) 0.0;

    y = copysign(y, x);

    return y;
}

/**
 * Gives the next power of two after $(D val). `T` can be any built-in
 * numerical type.
 *
 * If the operation would lead to an over/underflow, this function will
 * return `0`.
 *
 * Params:
 *     val = any number
 *
 * Returns:
 *     the next power of two after $(D val)
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
    import std.meta : AliasSeq;

    foreach (T; AliasSeq!(float, double, real))
    {
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
    }
}

@safe @nogc pure nothrow unittest // Issue 15973
{
    assert(nextPow2(uint.max / 2) == uint.max / 2 + 1);
    assert(nextPow2(uint.max / 2 + 2) == 0);
    assert(nextPow2(int.max / 2) == int.max / 2 + 1);
    assert(nextPow2(int.max / 2 + 2) == 0);
    assert(nextPow2(int.min + 1) == int.min);
}

/**
 * Gives the last power of two before $(D val). $(T) can be any built-in
 * numerical type.
 *
 * Params:
 *     val = any number
 *
 * Returns:
 *     the last power of two before $(D val)
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
    import std.meta : AliasSeq;

    foreach (T; AliasSeq!(float, double, real))
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
    import std.meta : AliasSeq;

    immutable smallP2 = pow(2.0L, -62);
    immutable bigP2 = pow(2.0L, 50);
    immutable smallP7 = pow(7.0L, -35);
    immutable bigP7 = pow(7.0L, 30);

    foreach (X; AliasSeq!(float, double, real))
    {
        immutable min_sub = X.min_normal * X.epsilon;

        foreach (x; AliasSeq!(smallP2, min_sub, X.min_normal, .25L, 0.5L, 1.0L,
                              2.0L, 8.0L, pow(2.0L, X.max_exp - 1), bigP2))
        {
            assert( isPowerOf2(cast(X) x));
            assert(!isPowerOf2(cast(X)-x));
        }

        foreach (x; AliasSeq!(0.0L, 3 * min_sub, smallP7, 0.1L, 1337.0L, bigP7, X.max, real.nan, real.infinity))
        {
            assert(!isPowerOf2(cast(X) x));
            assert(!isPowerOf2(cast(X)-x));
        }
    }

    foreach (X; AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong))
    {
        foreach (x; [1, 2, 4, 8, (X.max >>> 1) + 1])
        {
            assert( isPowerOf2(cast(X) x));
            static if (isSigned!X)
                assert(!isPowerOf2(cast(X)-x));
        }

        foreach (x; [0, 3, 5, 13, 77, X.min, X.max])
            assert(!isPowerOf2(cast(X) x));
    }
}
