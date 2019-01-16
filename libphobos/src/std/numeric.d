// Written in the D programming language.

/**
This module is a port of a growing fragment of the $(D_PARAM numeric)
header in Alexander Stepanov's $(LINK2 http://sgi.com/tech/stl,
Standard Template Library), with a few additions.

Macros:
Copyright: Copyright Andrei Alexandrescu 2008 - 2009.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP erdani.org, Andrei Alexandrescu),
                   Don Clugston, Robert Jacques, Ilya Yaroshenko
Source:    $(PHOBOSSRC std/_numeric.d)
*/
/*
         Copyright Andrei Alexandrescu 2008 - 2009.
Distributed under the Boost Software License, Version 1.0.
   (See accompanying file LICENSE_1_0.txt or copy at
         http://www.boost.org/LICENSE_1_0.txt)
*/
module std.numeric;

import std.complex;
import std.math;
import std.range.primitives;
import std.traits;
import std.typecons;

version (unittest)
{
    import std.stdio;
}
/// Format flags for CustomFloat.
public enum CustomFloatFlags
{
    /// Adds a sign bit to allow for signed numbers.
    signed = 1,

    /**
     * Store values in normalized form by default. The actual precision of the
     * significand is extended by 1 bit by assuming an implicit leading bit of 1
     * instead of 0. i.e. $(D 1.nnnn) instead of $(D 0.nnnn).
     * True for all $(LINK2 https://en.wikipedia.org/wiki/IEEE_floating_point, IEE754) types
     */
    storeNormalized = 2,

    /**
     * Stores the significand in $(LINK2 https://en.wikipedia.org/wiki/IEEE_754-1985#Denormalized_numbers,
     * IEEE754 denormalized) form when the exponent is 0. Required to express the value 0.
     */
    allowDenorm = 4,

    /**
      * Allows the storage of $(LINK2 https://en.wikipedia.org/wiki/IEEE_754-1985#Positive_and_negative_infinity,
      * IEEE754 _infinity) values.
      */
    infinity = 8,

    /// Allows the storage of $(LINK2 https://en.wikipedia.org/wiki/NaN, IEEE754 Not a Number) values.
    nan = 16,

    /**
     * If set, select an exponent bias such that max_exp = 1.
     * i.e. so that the maximum value is >= 1.0 and < 2.0.
     * Ignored if the exponent bias is manually specified.
     */
    probability = 32,

    /// If set, unsigned custom floats are assumed to be negative.
    negativeUnsigned = 64,

    /**If set, 0 is the only allowed $(LINK2 https://en.wikipedia.org/wiki/IEEE_754-1985#Denormalized_numbers,
     * IEEE754 denormalized) number.
     * Requires allowDenorm and storeNormalized.
     */
    allowDenormZeroOnly = 128 | allowDenorm | storeNormalized,

    /// Include _all of the $(LINK2 https://en.wikipedia.org/wiki/IEEE_floating_point, IEEE754) options.
    ieee = signed | storeNormalized | allowDenorm | infinity | nan ,

    /// Include none of the above options.
    none = 0
}

private template CustomFloatParams(uint bits)
{
    enum CustomFloatFlags flags = CustomFloatFlags.ieee
                ^ ((bits == 80) ? CustomFloatFlags.storeNormalized : CustomFloatFlags.none);
    static if (bits ==  8) alias CustomFloatParams = CustomFloatParams!( 4,  3, flags);
    static if (bits == 16) alias CustomFloatParams = CustomFloatParams!(10,  5, flags);
    static if (bits == 32) alias CustomFloatParams = CustomFloatParams!(23,  8, flags);
    static if (bits == 64) alias CustomFloatParams = CustomFloatParams!(52, 11, flags);
    static if (bits == 80) alias CustomFloatParams = CustomFloatParams!(64, 15, flags);
}

private template CustomFloatParams(uint precision, uint exponentWidth, CustomFloatFlags flags)
{
    import std.meta : AliasSeq;
    alias CustomFloatParams =
        AliasSeq!(
            precision,
            exponentWidth,
            flags,
            (1 << (exponentWidth - ((flags & flags.probability) == 0)))
             - ((flags & (flags.nan | flags.infinity)) != 0) - ((flags & flags.probability) != 0)
        ); // ((flags & CustomFloatFlags.probability) == 0)
}

/**
 * Allows user code to define custom floating-point formats. These formats are
 * for storage only; all operations on them are performed by first implicitly
 * extracting them to $(D real) first. After the operation is completed the
 * result can be stored in a custom floating-point value via assignment.
 */
template CustomFloat(uint bits)
if (bits == 8 || bits == 16 || bits == 32 || bits == 64 || bits == 80)
{
    alias CustomFloat = CustomFloat!(CustomFloatParams!(bits));
}

/// ditto
template CustomFloat(uint precision, uint exponentWidth, CustomFloatFlags flags = CustomFloatFlags.ieee)
if (((flags & flags.signed) + precision + exponentWidth) % 8 == 0 && precision + exponentWidth > 0)
{
    alias CustomFloat = CustomFloat!(CustomFloatParams!(precision, exponentWidth, flags));
}

///
@safe unittest
{
    import std.math : sin, cos;

    // Define a 16-bit floating point values
    CustomFloat!16                                x;     // Using the number of bits
    CustomFloat!(10, 5)                           y;     // Using the precision and exponent width
    CustomFloat!(10, 5,CustomFloatFlags.ieee)     z;     // Using the precision, exponent width and format flags
    CustomFloat!(10, 5,CustomFloatFlags.ieee, 15) w;     // Using the precision, exponent width, format flags and exponent offset bias

    // Use the 16-bit floats mostly like normal numbers
    w = x*y - 1;

    // Functions calls require conversion
    z = sin(+x)           + cos(+y);                     // Use unary plus to concisely convert to a real
    z = sin(x.get!float)  + cos(y.get!float);            // Or use get!T
    z = sin(cast(float) x) + cos(cast(float) y);           // Or use cast(T) to explicitly convert

    // Define a 8-bit custom float for storing probabilities
    alias Probability = CustomFloat!(4, 4, CustomFloatFlags.ieee^CustomFloatFlags.probability^CustomFloatFlags.signed );
    auto p = Probability(0.5);
}

/// ditto
struct CustomFloat(uint             precision,  // fraction bits (23 for float)
                   uint             exponentWidth,  // exponent bits (8 for float)  Exponent width
                   CustomFloatFlags flags,
                   uint             bias)
if (((flags & flags.signed)  + precision + exponentWidth) % 8 == 0 &&
    precision + exponentWidth > 0)
{
    import std.bitmanip : bitfields;
    import std.meta : staticIndexOf;
private:
    // get the correct unsigned bitfield type to support > 32 bits
    template uType(uint bits)
    {
        static if (bits <= size_t.sizeof*8)  alias uType = size_t;
        else                                alias uType = ulong ;
    }

    // get the correct signed   bitfield type to support > 32 bits
    template sType(uint bits)
    {
        static if (bits <= ptrdiff_t.sizeof*8-1) alias sType = ptrdiff_t;
        else                                    alias sType = long;
    }

    alias T_sig = uType!precision;
    alias T_exp = uType!exponentWidth;
    alias T_signed_exp = sType!exponentWidth;

    alias Flags = CustomFloatFlags;

    // Facilitate converting numeric types to custom float
    union ToBinary(F)
        if (is(typeof(CustomFloatParams!(F.sizeof*8))) || is(F == real))
    {
        F set;

        // If on Linux or Mac, where 80-bit reals are padded, ignore the
        // padding.
        import std.algorithm.comparison : min;
        CustomFloat!(CustomFloatParams!(min(F.sizeof*8, 80))) get;

        // Convert F to the correct binary type.
        static typeof(get) opCall(F value)
        {
            ToBinary r;
            r.set = value;
            return r.get;
        }
        alias get this;
    }

    // Perform IEEE rounding with round to nearest detection
    void roundedShift(T,U)(ref T sig, U shift)
    {
        if (sig << (T.sizeof*8 - shift) == cast(T) 1uL << (T.sizeof*8 - 1))
        {
            // round to even
            sig >>= shift;
            sig  += sig & 1;
        }
        else
        {
            sig >>= shift - 1;
            sig  += sig & 1;
            // Perform standard rounding
            sig >>= 1;
        }
    }

    // Convert the current value to signed exponent, normalized form
    void toNormalized(T,U)(ref T sig, ref U exp)
    {
        sig = significand;
        auto shift = (T.sizeof*8) - precision;
        exp = exponent;
        static if (flags&(Flags.infinity|Flags.nan))
        {
            // Handle inf or nan
            if (exp == exponent_max)
            {
                exp = exp.max;
                sig <<= shift;
                static if (flags&Flags.storeNormalized)
                {
                    // Save inf/nan in denormalized format
                    sig >>= 1;
                    sig  += cast(T) 1uL << (T.sizeof*8 - 1);
                }
                return;
            }
        }
        if ((~flags&Flags.storeNormalized) ||
            // Convert denormalized form to normalized form
            ((flags&Flags.allowDenorm) && exp == 0))
        {
            if (sig > 0)
            {
                import core.bitop : bsr;
                auto shift2 = precision - bsr(sig);
                exp  -= shift2-1;
                shift += shift2;
            }
            else                                // value = 0.0
            {
                exp = exp.min;
                return;
            }
        }
        sig <<= shift;
        exp -= bias;
    }

    // Set the current value from signed exponent, normalized form
    void fromNormalized(T,U)(ref T sig, ref U exp)
    {
        auto shift = (T.sizeof*8) - precision;
        if (exp == exp.max)
        {
            // infinity or nan
            exp = exponent_max;
            static if (flags & Flags.storeNormalized)
                sig <<= 1;

            // convert back to normalized form
            static if (~flags & Flags.infinity)
                // No infinity support?
                assert(sig != 0, "Infinity floating point value assigned to a "
                        ~ typeof(this).stringof ~ " (no infinity support).");

            static if (~flags & Flags.nan)  // No NaN support?
                assert(sig == 0, "NaN floating point value assigned to a " ~
                        typeof(this).stringof ~ " (no nan support).");
            sig >>= shift;
            return;
        }
        if (exp == exp.min)     // 0.0
        {
             exp = 0;
             sig = 0;
             return;
        }

        exp += bias;
        if (exp <= 0)
        {
            static if ((flags&Flags.allowDenorm) ||
                       // Convert from normalized form to denormalized
                       (~flags&Flags.storeNormalized))
            {
                shift += -exp;
                roundedShift(sig,1);
                sig   += cast(T) 1uL << (T.sizeof*8 - 1);
                // Add the leading 1
                exp    = 0;
            }
            else
                assert((flags&Flags.storeNormalized) && exp == 0,
                    "Underflow occured assigning to a " ~
                    typeof(this).stringof ~ " (no denormal support).");
        }
        else
        {
            static if (~flags&Flags.storeNormalized)
            {
                // Convert from normalized form to denormalized
                roundedShift(sig,1);
                sig  += cast(T) 1uL << (T.sizeof*8 - 1);
                // Add the leading 1
            }
        }

        if (shift > 0)
            roundedShift(sig,shift);
        if (sig > significand_max)
        {
            // handle significand overflow (should only be 1 bit)
            static if (~flags&Flags.storeNormalized)
            {
                sig >>= 1;
            }
            else
                sig &= significand_max;
            exp++;
        }
        static if ((flags&Flags.allowDenormZeroOnly)==Flags.allowDenormZeroOnly)
        {
            // disallow non-zero denormals
            if (exp == 0)
            {
                sig <<= 1;
                if (sig > significand_max && (sig&significand_max) > 0)
                    // Check and round to even
                    exp++;
                sig = 0;
            }
        }

        if (exp >= exponent_max)
        {
            static if (flags&(Flags.infinity|Flags.nan))
            {
                sig         = 0;
                exp         = exponent_max;
                static if (~flags&(Flags.infinity))
                    assert(0, "Overflow occured assigning to a " ~
                        typeof(this).stringof ~ " (no infinity support).");
            }
            else
                assert(exp == exponent_max, "Overflow occured assigning to a "
                    ~ typeof(this).stringof ~ " (no infinity support).");
        }
    }

public:
    static if (precision == 64) // CustomFloat!80 support hack
    {
        ulong significand;
        enum ulong significand_max = ulong.max;
        mixin(bitfields!(
            T_exp , "exponent", exponentWidth,
            bool  , "sign"    , flags & flags.signed ));
    }
    else
    {
        mixin(bitfields!(
            T_sig, "significand", precision,
            T_exp, "exponent"   , exponentWidth,
            bool , "sign"       , flags & flags.signed ));
    }

    /// Returns: infinity value
    static if (flags & Flags.infinity)
        static @property CustomFloat infinity()
        {
            CustomFloat value;
            static if (flags & Flags.signed)
            value.sign          = 0;
            value.significand   = 0;
            value.exponent      = exponent_max;
            return value;
        }

    /// Returns: NaN value
    static if (flags & Flags.nan)
        static @property CustomFloat nan()
        {
            CustomFloat value;
            static if (flags & Flags.signed)
            value.sign          = 0;
            value.significand   = cast(typeof(significand_max)) 1L << (precision-1);
            value.exponent      = exponent_max;
            return value;
        }

    /// Returns: number of decimal digits of precision
    static @property size_t dig()
    {
        auto shiftcnt =  precision - ((flags&Flags.storeNormalized) != 0);
        immutable x = (shiftcnt == 64) ? 0 : 1uL << shiftcnt;
        return cast(size_t) log10(x);
    }

    /// Returns: smallest increment to the value 1
    static @property CustomFloat epsilon()
    {
        CustomFloat value;
        static if (flags & Flags.signed)
        value.sign       = 0;
        T_signed_exp exp = -precision;
        T_sig        sig = 0;

        value.fromNormalized(sig,exp);
        if (exp == 0 && sig == 0) // underflowed to zero
        {
            static if ((flags&Flags.allowDenorm) ||
                       (~flags&Flags.storeNormalized))
                sig = 1;
            else
                sig = cast(T) 1uL << (precision - 1);
        }
        value.exponent     = cast(value.T_exp) exp;
        value.significand  = cast(value.T_sig) sig;
        return value;
    }

    /// the number of bits in mantissa
    enum mant_dig = precision + ((flags&Flags.storeNormalized) != 0);

    /// Returns: maximum int value such that 10<sup>max_10_exp</sup> is representable
    static @property int max_10_exp(){ return cast(int) log10( +max ); }

    /// maximum int value such that 2<sup>max_exp-1</sup> is representable
    enum max_exp = exponent_max-bias+((~flags&(Flags.infinity|flags.nan))!=0);

    /// Returns: minimum int value such that 10<sup>min_10_exp</sup> is representable
    static @property int min_10_exp(){ return cast(int) log10( +min_normal ); }

    /// minimum int value such that 2<sup>min_exp-1</sup> is representable as a normalized value
    enum min_exp = cast(T_signed_exp)-bias +1+ ((flags&Flags.allowDenorm)!=0);

    /// Returns: largest representable value that's not infinity
    static @property CustomFloat max()
    {
        CustomFloat value;
        static if (flags & Flags.signed)
        value.sign        = 0;
        value.exponent    = exponent_max - ((flags&(flags.infinity|flags.nan)) != 0);
        value.significand = significand_max;
        return value;
    }

    /// Returns: smallest representable normalized value that's not 0
    static @property CustomFloat min_normal() {
        CustomFloat value;
        static if (flags & Flags.signed)
        value.sign        = 0;
        value.exponent    = 1;
        static if (flags&Flags.storeNormalized)
            value.significand = 0;
        else
            value.significand = cast(T_sig) 1uL << (precision - 1);
        return value;
    }

    /// Returns: real part
    @property CustomFloat re() { return this; }

    /// Returns: imaginary part
    static @property CustomFloat im() { return CustomFloat(0.0f); }

    /// Initialize from any $(D real) compatible type.
    this(F)(F input) if (__traits(compiles, cast(real) input ))
    {
        this = input;
    }

    /// Self assignment
    void opAssign(F:CustomFloat)(F input)
    {
        static if (flags & Flags.signed)
        sign        = input.sign;
        exponent    = input.exponent;
        significand = input.significand;
    }

    /// Assigns from any $(D real) compatible type.
    void opAssign(F)(F input)
        if (__traits(compiles, cast(real) input))
    {
        import std.conv : text;

        static if (staticIndexOf!(Unqual!F, float, double, real) >= 0)
            auto value = ToBinary!(Unqual!F)(input);
        else
            auto value = ToBinary!(real    )(input);

        // Assign the sign bit
        static if (~flags & Flags.signed)
            assert((!value.sign) ^ ((flags&flags.negativeUnsigned) > 0),
                "Incorrectly signed floating point value assigned to a " ~
                typeof(this).stringof ~ " (no sign support).");
        else
            sign = value.sign;

        CommonType!(T_signed_exp ,value.T_signed_exp) exp = value.exponent;
        CommonType!(T_sig,        value.T_sig       ) sig = value.significand;

        value.toNormalized(sig,exp);
        fromNormalized(sig,exp);

        assert(exp <= exponent_max,    text(typeof(this).stringof ~
            " exponent too large: "   ,exp," > ",exponent_max,   "\t",input,"\t",sig));
        assert(sig <= significand_max, text(typeof(this).stringof ~
            " significand too large: ",sig," > ",significand_max,
            "\t",input,"\t",exp," ",exponent_max));
        exponent    = cast(T_exp) exp;
        significand = cast(T_sig) sig;
    }

    /// Fetches the stored value either as a $(D float), $(D double) or $(D real).
    @property F get(F)()
        if (staticIndexOf!(Unqual!F, float, double, real) >= 0)
    {
        import std.conv : text;

        ToBinary!F result;

        static if (flags&Flags.signed)
            result.sign = sign;
        else
            result.sign = (flags&flags.negativeUnsigned) > 0;

        CommonType!(T_signed_exp ,result.get.T_signed_exp ) exp = exponent; // Assign the exponent and fraction
        CommonType!(T_sig,        result.get.T_sig        ) sig = significand;

        toNormalized(sig,exp);
        result.fromNormalized(sig,exp);
        assert(exp <= result.exponent_max,    text("get exponent too large: "   ,exp," > ",result.exponent_max) );
        assert(sig <= result.significand_max, text("get significand too large: ",sig," > ",result.significand_max) );
        result.exponent     = cast(result.get.T_exp) exp;
        result.significand  = cast(result.get.T_sig) sig;
        return result.set;
    }

    ///ditto
    T opCast(T)() if (__traits(compiles, get!T )) { return get!T; }

    /// Convert the CustomFloat to a real and perform the relavent operator on the result
    real opUnary(string op)()
        if (__traits(compiles, mixin(op~`(get!real)`)) || op=="++" || op=="--")
    {
        static if (op=="++" || op=="--")
        {
            auto result = get!real;
            this = mixin(op~`result`);
            return result;
        }
        else
            return mixin(op~`get!real`);
    }

    /// ditto
    real opBinary(string op,T)(T b)
        if (__traits(compiles, mixin(`get!real`~op~`b`)))
    {
        return mixin(`get!real`~op~`b`);
    }

    /// ditto
    real opBinaryRight(string op,T)(T a)
        if ( __traits(compiles, mixin(`a`~op~`get!real`)) &&
            !__traits(compiles, mixin(`get!real`~op~`b`)))
    {
        return mixin(`a`~op~`get!real`);
    }

    /// ditto
    int opCmp(T)(auto ref T b)
        if (__traits(compiles, cast(real) b))
    {
        auto x = get!real;
        auto y = cast(real) b;
        return  (x >= y)-(x <= y);
    }

    /// ditto
    void opOpAssign(string op, T)(auto ref T b)
        if (__traits(compiles, mixin(`get!real`~op~`cast(real) b`)))
    {
        return mixin(`this = this `~op~` cast(real) b`);
    }

    /// ditto
    template toString()
    {
        import std.format : FormatSpec, formatValue;
        // Needs to be a template because of DMD @@BUG@@ 13737.
        void toString()(scope void delegate(const(char)[]) sink, FormatSpec!char fmt)
        {
            sink.formatValue(get!real, fmt);
        }
    }
}

@safe unittest
{
    import std.meta;
    alias FPTypes =
        AliasSeq!(
            CustomFloat!(5, 10),
            CustomFloat!(5, 11, CustomFloatFlags.ieee ^ CustomFloatFlags.signed),
            CustomFloat!(1, 15, CustomFloatFlags.ieee ^ CustomFloatFlags.signed),
            CustomFloat!(4, 3, CustomFloatFlags.ieee | CustomFloatFlags.probability ^ CustomFloatFlags.signed)
        );

    foreach (F; FPTypes)
    {
        auto x = F(0.125);
        assert(x.get!float == 0.125F);
        assert(x.get!double == 0.125);

        x -= 0.0625;
        assert(x.get!float == 0.0625F);
        assert(x.get!double == 0.0625);

        x *= 2;
        assert(x.get!float == 0.125F);
        assert(x.get!double == 0.125);

        x /= 4;
        assert(x.get!float == 0.03125);
        assert(x.get!double == 0.03125);

        x = 0.5;
        x ^^= 4;
        assert(x.get!float == 1 / 16.0F);
        assert(x.get!double == 1 / 16.0);
    }
}

@system unittest
{
    // @system due to to!string(CustomFloat)
    import std.conv;
    CustomFloat!(5, 10) y = CustomFloat!(5, 10)(0.125);
    assert(y.to!string == "0.125");
}

/**
Defines the fastest type to use when storing temporaries of a
calculation intended to ultimately yield a result of type $(D F)
(where $(D F) must be one of $(D float), $(D double), or $(D
real)). When doing a multi-step computation, you may want to store
intermediate results as $(D FPTemporary!F).

The necessity of $(D FPTemporary) stems from the optimized
floating-point operations and registers present in virtually all
processors. When adding numbers in the example above, the addition may
in fact be done in $(D real) precision internally. In that case,
storing the intermediate $(D result) in $(D double format) is not only
less precise, it is also (surprisingly) slower, because a conversion
from $(D real) to $(D double) is performed every pass through the
loop. This being a lose-lose situation, $(D FPTemporary!F) has been
defined as the $(I fastest) type to use for calculations at precision
$(D F). There is no need to define a type for the $(I most accurate)
calculations, as that is always $(D real).

Finally, there is no guarantee that using $(D FPTemporary!F) will
always be fastest, as the speed of floating-point calculations depends
on very many factors.
 */
template FPTemporary(F)
if (isFloatingPoint!F)
{
    version (X86)
        alias FPTemporary = real;
    else
        alias FPTemporary = Unqual!F;
}

///
@safe unittest
{
    import std.math : approxEqual;

    // Average numbers in an array
    double avg(in double[] a)
    {
        if (a.length == 0) return 0;
        FPTemporary!double result = 0;
        foreach (e; a) result += e;
        return result / a.length;
    }

    auto a = [1.0, 2.0, 3.0];
    assert(approxEqual(avg(a), 2));
}

/**
Implements the $(HTTP tinyurl.com/2zb9yr, secant method) for finding a
root of the function $(D fun) starting from points $(D [xn_1, x_n])
(ideally close to the root). $(D Num) may be $(D float), $(D double),
or $(D real).
*/
template secantMethod(alias fun)
{
    import std.functional : unaryFun;
    Num secantMethod(Num)(Num xn_1, Num xn)
    {
        auto fxn = unaryFun!(fun)(xn_1), d = xn_1 - xn;
        typeof(fxn) fxn_1;

        xn = xn_1;
        while (!approxEqual(d, 0) && isFinite(d))
        {
            xn_1 = xn;
            xn -= d;
            fxn_1 = fxn;
            fxn = unaryFun!(fun)(xn);
            d *= -fxn / (fxn - fxn_1);
        }
        return xn;
    }
}

///
@safe unittest
{
    import std.math : approxEqual, cos;

    float f(float x)
    {
        return cos(x) - x*x*x;
    }
    auto x = secantMethod!(f)(0f, 1f);
    assert(approxEqual(x, 0.865474));
}

@system unittest
{
    // @system because of __gshared stderr
    scope(failure) stderr.writeln("Failure testing secantMethod");
    float f(float x)
    {
        return cos(x) - x*x*x;
    }
    immutable x = secantMethod!(f)(0f, 1f);
    assert(approxEqual(x, 0.865474));
    auto d = &f;
    immutable y = secantMethod!(d)(0f, 1f);
    assert(approxEqual(y, 0.865474));
}


/**
 * Return true if a and b have opposite sign.
 */
private bool oppositeSigns(T1, T2)(T1 a, T2 b)
{
    return signbit(a) != signbit(b);
}

public:

/**  Find a real root of a real function f(x) via bracketing.
 *
 * Given a function `f` and a range `[a .. b]` such that `f(a)`
 * and `f(b)` have opposite signs or at least one of them equals ±0,
 * returns the value of `x` in
 * the range which is closest to a root of `f(x)`.  If `f(x)`
 * has more than one root in the range, one will be chosen
 * arbitrarily.  If `f(x)` returns NaN, NaN will be returned;
 * otherwise, this algorithm is guaranteed to succeed.
 *
 * Uses an algorithm based on TOMS748, which uses inverse cubic
 * interpolation whenever possible, otherwise reverting to parabolic
 * or secant interpolation. Compared to TOMS748, this implementation
 * improves worst-case performance by a factor of more than 100, and
 * typical performance by a factor of 2. For 80-bit reals, most
 * problems require 8 to 15 calls to `f(x)` to achieve full machine
 * precision. The worst-case performance (pathological cases) is
 * approximately twice the number of bits.
 *
 * References: "On Enclosing Simple Roots of Nonlinear Equations",
 * G. Alefeld, F.A. Potra, Yixun Shi, Mathematics of Computation 61,
 * pp733-744 (1993).  Fortran code available from $(HTTP
 * www.netlib.org,www.netlib.org) as algorithm TOMS478.
 *
 */
T findRoot(T, DF, DT)(scope DF f, in T a, in T b,
    scope DT tolerance) //= (T a, T b) => false)
if (
    isFloatingPoint!T &&
    is(typeof(tolerance(T.init, T.init)) : bool) &&
    is(typeof(f(T.init)) == R, R) && isFloatingPoint!R
    )
{
    immutable fa = f(a);
    if (fa == 0)
        return a;
    immutable fb = f(b);
    if (fb == 0)
        return b;
    immutable r = findRoot(f, a, b, fa, fb, tolerance);
    // Return the first value if it is smaller or NaN
    return !(fabs(r[2]) > fabs(r[3])) ? r[0] : r[1];
}

///ditto
T findRoot(T, DF)(scope DF f, in T a, in T b)
{
    return findRoot(f, a, b, (T a, T b) => false);
}

/** Find root of a real function f(x) by bracketing, allowing the
 * termination condition to be specified.
 *
 * Params:
 *
 * f = Function to be analyzed
 *
 * ax = Left bound of initial range of `f` known to contain the
 * root.
 *
 * bx = Right bound of initial range of `f` known to contain the
 * root.
 *
 * fax = Value of $(D f(ax)).
 *
 * fbx = Value of $(D f(bx)). $(D fax) and $(D fbx) should have opposite signs.
 * ($(D f(ax)) and $(D f(bx)) are commonly known in advance.)
 *
 *
 * tolerance = Defines an early termination condition. Receives the
 *             current upper and lower bounds on the root. The
 *             delegate must return $(D true) when these bounds are
 *             acceptable. If this function always returns $(D false),
 *             full machine precision will be achieved.
 *
 * Returns:
 *
 * A tuple consisting of two ranges. The first two elements are the
 * range (in `x`) of the root, while the second pair of elements
 * are the corresponding function values at those points. If an exact
 * root was found, both of the first two elements will contain the
 * root, and the second pair of elements will be 0.
 */
Tuple!(T, T, R, R) findRoot(T, R, DF, DT)(scope DF f, in T ax, in T bx, in R fax, in R fbx,
    scope DT tolerance) // = (T a, T b) => false)
if (
    isFloatingPoint!T &&
    is(typeof(tolerance(T.init, T.init)) : bool) &&
    is(typeof(f(T.init)) == R) && isFloatingPoint!R
    )
in
{
    assert(!ax.isNaN() && !bx.isNaN(), "Limits must not be NaN");
    assert(signbit(fax) != signbit(fbx), "Parameters must bracket the root.");
}
body
{
    // Author: Don Clugston. This code is (heavily) modified from TOMS748
    // (www.netlib.org).  The changes to improve the worst-cast performance are
    // entirely original.

    T a, b, d;  // [a .. b] is our current bracket. d is the third best guess.
    R fa, fb, fd; // Values of f at a, b, d.
    bool done = false; // Has a root been found?

    // Allow ax and bx to be provided in reverse order
    if (ax <= bx)
    {
        a = ax; fa = fax;
        b = bx; fb = fbx;
    }
    else
    {
        a = bx; fa = fbx;
        b = ax; fb = fax;
    }

    // Test the function at point c; update brackets accordingly
    void bracket(T c)
    {
        R fc = f(c);
        if (fc == 0 || fc.isNaN()) // Exact solution, or NaN
        {
            a = c;
            fa = fc;
            d = c;
            fd = fc;
            done = true;
            return;
        }

        // Determine new enclosing interval
        if (signbit(fa) != signbit(fc))
        {
            d = b;
            fd = fb;
            b = c;
            fb = fc;
        }
        else
        {
            d = a;
            fd = fa;
            a = c;
            fa = fc;
        }
    }

   /* Perform a secant interpolation. If the result would lie on a or b, or if
     a and b differ so wildly in magnitude that the result would be meaningless,
     perform a bisection instead.
    */
    static T secant_interpolate(T a, T b, R fa, R fb)
    {
        if (( ((a - b) == a) && b != 0) || (a != 0 && ((b - a) == b)))
        {
            // Catastrophic cancellation
            if (a == 0)
                a = copysign(T(0), b);
            else if (b == 0)
                b = copysign(T(0), a);
            else if (signbit(a) != signbit(b))
                return 0;
            T c = ieeeMean(a, b);
            return c;
        }
        // avoid overflow
        if (b - a > T.max)
            return b / 2 + a / 2;
        if (fb - fa > R.max)
            return a - (b - a) / 2;
        T c = a - (fa / (fb - fa)) * (b - a);
        if (c == a || c == b)
            return (a + b) / 2;
        return c;
    }

    /* Uses 'numsteps' newton steps to approximate the zero in [a .. b] of the
       quadratic polynomial interpolating f(x) at a, b, and d.
       Returns:
         The approximate zero in [a .. b] of the quadratic polynomial.
    */
    T newtonQuadratic(int numsteps)
    {
        // Find the coefficients of the quadratic polynomial.
        immutable T a0 = fa;
        immutable T a1 = (fb - fa)/(b - a);
        immutable T a2 = ((fd - fb)/(d - b) - a1)/(d - a);

        // Determine the starting point of newton steps.
        T c = oppositeSigns(a2, fa) ? a  : b;

        // start the safeguarded newton steps.
        foreach (int i; 0 .. numsteps)
        {
            immutable T pc = a0 + (a1 + a2 * (c - b))*(c - a);
            immutable T pdc = a1 + a2*((2 * c) - (a + b));
            if (pdc == 0)
                return a - a0 / a1;
            else
                c = c - pc / pdc;
        }
        return c;
    }

    // On the first iteration we take a secant step:
    if (fa == 0 || fa.isNaN())
    {
        done = true;
        b = a;
        fb = fa;
    }
    else if (fb == 0 || fb.isNaN())
    {
        done = true;
        a = b;
        fa = fb;
    }
    else
    {
        bracket(secant_interpolate(a, b, fa, fb));
    }

    // Starting with the second iteration, higher-order interpolation can
    // be used.
    int itnum = 1;   // Iteration number
    int baditer = 1; // Num bisections to take if an iteration is bad.
    T c, e;  // e is our fourth best guess
    R fe;

whileloop:
    while (!done && (b != nextUp(a)) && !tolerance(a, b))
    {
        T a0 = a, b0 = b; // record the brackets

        // Do two higher-order (cubic or parabolic) interpolation steps.
        foreach (int QQ; 0 .. 2)
        {
            // Cubic inverse interpolation requires that
            // all four function values fa, fb, fd, and fe are distinct;
            // otherwise use quadratic interpolation.
            bool distinct = (fa != fb) && (fa != fd) && (fa != fe)
                         && (fb != fd) && (fb != fe) && (fd != fe);
            // The first time, cubic interpolation is impossible.
            if (itnum<2) distinct = false;
            bool ok = distinct;
            if (distinct)
            {
                // Cubic inverse interpolation of f(x) at a, b, d, and e
                immutable q11 = (d - e) * fd / (fe - fd);
                immutable q21 = (b - d) * fb / (fd - fb);
                immutable q31 = (a - b) * fa / (fb - fa);
                immutable d21 = (b - d) * fd / (fd - fb);
                immutable d31 = (a - b) * fb / (fb - fa);

                immutable q22 = (d21 - q11) * fb / (fe - fb);
                immutable q32 = (d31 - q21) * fa / (fd - fa);
                immutable d32 = (d31 - q21) * fd / (fd - fa);
                immutable q33 = (d32 - q22) * fa / (fe - fa);
                c = a + (q31 + q32 + q33);
                if (c.isNaN() || (c <= a) || (c >= b))
                {
                    // DAC: If the interpolation predicts a or b, it's
                    // probable that it's the actual root. Only allow this if
                    // we're already close to the root.
                    if (c == a && a - b != a)
                    {
                        c = nextUp(a);
                    }
                    else if (c == b && a - b != -b)
                    {
                        c = nextDown(b);
                    }
                    else
                    {
                        ok = false;
                    }
                }
            }
            if (!ok)
            {
                // DAC: Alefeld doesn't explain why the number of newton steps
                // should vary.
                c = newtonQuadratic(distinct ? 3 : 2);
                if (c.isNaN() || (c <= a) || (c >= b))
                {
                    // Failure, try a secant step:
                    c = secant_interpolate(a, b, fa, fb);
                }
            }
            ++itnum;
            e = d;
            fe = fd;
            bracket(c);
            if (done || ( b == nextUp(a)) || tolerance(a, b))
                break whileloop;
            if (itnum == 2)
                continue whileloop;
        }

        // Now we take a double-length secant step:
        T u;
        R fu;
        if (fabs(fa) < fabs(fb))
        {
            u = a;
            fu = fa;
        }
        else
        {
            u = b;
            fu = fb;
        }
        c = u - 2 * (fu / (fb - fa)) * (b - a);

        // DAC: If the secant predicts a value equal to an endpoint, it's
        // probably false.
        if (c == a || c == b || c.isNaN() || fabs(c - u) > (b - a) / 2)
        {
            if ((a-b) == a || (b-a) == b)
            {
                if ((a>0 && b<0) || (a<0 && b>0))
                    c = 0;
                else
                {
                    if (a == 0)
                        c = ieeeMean(copysign(T(0), b), b);
                    else if (b == 0)
                        c = ieeeMean(copysign(T(0), a), a);
                    else
                        c = ieeeMean(a, b);
                }
            }
            else
            {
                c = a + (b - a) / 2;
            }
        }
        e = d;
        fe = fd;
        bracket(c);
        if (done || (b == nextUp(a)) || tolerance(a, b))
            break;

        // IMPROVE THE WORST-CASE PERFORMANCE
        // We must ensure that the bounds reduce by a factor of 2
        // in binary space! every iteration. If we haven't achieved this
        // yet, or if we don't yet know what the exponent is,
        // perform a binary chop.

        if ((a == 0 || b == 0 ||
            (fabs(a) >= T(0.5) * fabs(b) && fabs(b) >= T(0.5) * fabs(a)))
            &&  (b - a) < T(0.25) * (b0 - a0))
        {
            baditer = 1;
            continue;
        }

        // DAC: If this happens on consecutive iterations, we probably have a
        // pathological function. Perform a number of bisections equal to the
        // total number of consecutive bad iterations.

        if ((b - a) < T(0.25) * (b0 - a0))
            baditer = 1;
        foreach (int QQ; 0 .. baditer)
        {
            e = d;
            fe = fd;

            T w;
            if ((a>0 && b<0) || (a<0 && b>0))
                w = 0;
            else
            {
                T usea = a;
                T useb = b;
                if (a == 0)
                    usea = copysign(T(0), b);
                else if (b == 0)
                    useb = copysign(T(0), a);
                w = ieeeMean(usea, useb);
            }
            bracket(w);
        }
        ++baditer;
    }
    return Tuple!(T, T, R, R)(a, b, fa, fb);
}

///ditto
Tuple!(T, T, R, R) findRoot(T, R, DF)(scope DF f, in T ax, in T bx, in R fax, in R fbx)
{
    return findRoot(f, ax, bx, fax, fbx, (T a, T b) => false);
}

///ditto
T findRoot(T, R)(scope R delegate(T) f, in T a, in T b,
    scope bool delegate(T lo, T hi) tolerance = (T a, T b) => false)
{
    return findRoot!(T, R delegate(T), bool delegate(T lo, T hi))(f, a, b, tolerance);
}

@safe nothrow unittest
{
    int numProblems = 0;
    int numCalls;

    void testFindRoot(real delegate(real) @nogc @safe nothrow pure f , real x1, real x2) @nogc @safe nothrow pure
    {
        //numCalls=0;
        //++numProblems;
        assert(!x1.isNaN() && !x2.isNaN());
        assert(signbit(x1) != signbit(x2));
        auto result = findRoot(f, x1, x2, f(x1), f(x2),
          (real lo, real hi) { return false; });

        auto flo = f(result[0]);
        auto fhi = f(result[1]);
        if (flo != 0)
        {
            assert(oppositeSigns(flo, fhi));
        }
    }

    // Test functions
    real cubicfn(real x) @nogc @safe nothrow pure
    {
        //++numCalls;
        if (x>float.max)
            x = float.max;
        if (x<-double.max)
            x = -double.max;
        // This has a single real root at -59.286543284815
        return 0.386*x*x*x + 23*x*x + 15.7*x + 525.2;
    }
    // Test a function with more than one root.
    real multisine(real x) { ++numCalls; return sin(x); }
    //testFindRoot( &multisine, 6, 90);
    //testFindRoot(&cubicfn, -100, 100);
    //testFindRoot( &cubicfn, -double.max, real.max);


/* Tests from the paper:
 * "On Enclosing Simple Roots of Nonlinear Equations", G. Alefeld, F.A. Potra,
 *   Yixun Shi, Mathematics of Computation 61, pp733-744 (1993).
 */
    // Parameters common to many alefeld tests.
    int n;
    real ale_a, ale_b;

    int powercalls = 0;

    real power(real x)
    {
        ++powercalls;
        ++numCalls;
        return pow(x, n) + double.min_normal;
    }
    int [] power_nvals = [3, 5, 7, 9, 19, 25];
    // Alefeld paper states that pow(x,n) is a very poor case, where bisection
    // outperforms his method, and gives total numcalls =
    // 921 for bisection (2.4 calls per bit), 1830 for Alefeld (4.76/bit),
    // 2624 for brent (6.8/bit)
    // ... but that is for double, not real80.
    // This poor performance seems mainly due to catastrophic cancellation,
    // which is avoided here by the use of ieeeMean().
    // I get: 231 (0.48/bit).
    // IE this is 10X faster in Alefeld's worst case
    numProblems=0;
    foreach (k; power_nvals)
    {
        n = k;
        //testFindRoot(&power, -1, 10);
    }

    int powerProblems = numProblems;

    // Tests from Alefeld paper

    int [9] alefeldSums;
    real alefeld0(real x)
    {
        ++alefeldSums[0];
        ++numCalls;
        real q =  sin(x) - x/2;
        for (int i=1; i<20; ++i)
            q+=(2*i-5.0)*(2*i-5.0)/((x-i*i)*(x-i*i)*(x-i*i));
        return q;
    }
    real alefeld1(real x)
    {
        ++numCalls;
        ++alefeldSums[1];
        return ale_a*x + exp(ale_b * x);
    }
    real alefeld2(real x)
    {
        ++numCalls;
        ++alefeldSums[2];
        return pow(x, n) - ale_a;
    }
    real alefeld3(real x)
    {
        ++numCalls;
        ++alefeldSums[3];
        return (1.0 +pow(1.0L-n, 2))*x - pow(1.0L-n*x, 2);
    }
    real alefeld4(real x)
    {
        ++numCalls;
        ++alefeldSums[4];
        return x*x - pow(1-x, n);
    }
    real alefeld5(real x)
    {
        ++numCalls;
        ++alefeldSums[5];
        return (1+pow(1.0L-n, 4))*x - pow(1.0L-n*x, 4);
    }
    real alefeld6(real x)
    {
        ++numCalls;
        ++alefeldSums[6];
        return exp(-n*x)*(x-1.01L) + pow(x, n);
    }
    real alefeld7(real x)
    {
        ++numCalls;
        ++alefeldSums[7];
        return (n*x-1)/((n-1)*x);
    }

    numProblems=0;
    //testFindRoot(&alefeld0, PI_2, PI);
    for (n=1; n <= 10; ++n)
    {
        //testFindRoot(&alefeld0, n*n+1e-9L, (n+1)*(n+1)-1e-9L);
    }
    ale_a = -40; ale_b = -1;
    //testFindRoot(&alefeld1, -9, 31);
    ale_a = -100; ale_b = -2;
    //testFindRoot(&alefeld1, -9, 31);
    ale_a = -200; ale_b = -3;
    //testFindRoot(&alefeld1, -9, 31);
    int [] nvals_3 = [1, 2, 5, 10, 15, 20];
    int [] nvals_5 = [1, 2, 4, 5, 8, 15, 20];
    int [] nvals_6 = [1, 5, 10, 15, 20];
    int [] nvals_7 = [2, 5, 15, 20];

    for (int i=4; i<12; i+=2)
    {
       n = i;
       ale_a = 0.2;
       //testFindRoot(&alefeld2, 0, 5);
       ale_a=1;
       //testFindRoot(&alefeld2, 0.95, 4.05);
       //testFindRoot(&alefeld2, 0, 1.5);
    }
    foreach (i; nvals_3)
    {
        n=i;
        //testFindRoot(&alefeld3, 0, 1);
    }
    foreach (i; nvals_3)
    {
        n=i;
        //testFindRoot(&alefeld4, 0, 1);
    }
    foreach (i; nvals_5)
    {
        n=i;
        //testFindRoot(&alefeld5, 0, 1);
    }
    foreach (i; nvals_6)
    {
        n=i;
        //testFindRoot(&alefeld6, 0, 1);
    }
    foreach (i; nvals_7)
    {
        n=i;
        //testFindRoot(&alefeld7, 0.01L, 1);
    }
    real worstcase(real x)
    {
        ++numCalls;
        return x<0.3*real.max? -0.999e-3 : 1.0;
    }
    //testFindRoot(&worstcase, -real.max, real.max);

    // just check that the double + float cases compile
    //findRoot((double x){ return 0.0; }, -double.max, double.max);
    //findRoot((float x){ return 0.0f; }, -float.max, float.max);

/*
   int grandtotal=0;
   foreach (calls; alefeldSums)
   {
       grandtotal+=calls;
   }
   grandtotal-=2*numProblems;
   printf("\nALEFELD TOTAL = %d avg = %f (alefeld avg=19.3 for double)\n",
   grandtotal, (1.0*grandtotal)/numProblems);
   powercalls -= 2*powerProblems;
   printf("POWER TOTAL = %d avg = %f ", powercalls,
        (1.0*powercalls)/powerProblems);
*/
    //Issue 14231
    auto xp = findRoot((float x) => x, 0f, 1f);
    auto xn = findRoot((float x) => x, -1f, -0f);
}

//regression control
@system unittest
{
    // @system due to the case in the 2nd line
    static assert(__traits(compiles, findRoot((float x)=>cast(real) x, float.init, float.init)));
    static assert(__traits(compiles, findRoot!real((x)=>cast(double) x, real.init, real.init)));
    static assert(__traits(compiles, findRoot((real x)=>cast(double) x, real.init, real.init)));
}

/++
Find a real minimum of a real function `f(x)` via bracketing.
Given a function `f` and a range `(ax .. bx)`,
returns the value of `x` in the range which is closest to a minimum of `f(x)`.
`f` is never evaluted at the endpoints of `ax` and `bx`.
If `f(x)` has more than one minimum in the range, one will be chosen arbitrarily.
If `f(x)` returns NaN or -Infinity, `(x, f(x), NaN)` will be returned;
otherwise, this algorithm is guaranteed to succeed.

Params:
    f = Function to be analyzed
    ax = Left bound of initial range of f known to contain the minimum.
    bx = Right bound of initial range of f known to contain the minimum.
    relTolerance = Relative tolerance.
    absTolerance = Absolute tolerance.

Preconditions:
    `ax` and `bx` shall be finite reals. $(BR)
    $(D relTolerance) shall be normal positive real. $(BR)
    $(D absTolerance) shall be normal positive real no less then $(D T.epsilon*2).

Returns:
    A tuple consisting of `x`, `y = f(x)` and `error = 3 * (absTolerance * fabs(x) + relTolerance)`.

    The method used is a combination of golden section search and
successive parabolic interpolation. Convergence is never much slower
than that for a Fibonacci search.

References:
    "Algorithms for Minimization without Derivatives", Richard Brent, Prentice-Hall, Inc. (1973)

See_Also: $(LREF findRoot), $(REF isNormal, std,math)
+/
Tuple!(T, "x", Unqual!(ReturnType!DF), "y", T, "error")
findLocalMin(T, DF)(
        scope DF f,
        in T ax,
        in T bx,
        in T relTolerance = sqrt(T.epsilon),
        in T absTolerance = sqrt(T.epsilon),
        )
if (isFloatingPoint!T
    && __traits(compiles, {T _ = DF.init(T.init);}))
in
{
    assert(isFinite(ax), "ax is not finite");
    assert(isFinite(bx), "bx is not finite");
    assert(isNormal(relTolerance), "relTolerance is not normal floating point number");
    assert(isNormal(absTolerance), "absTolerance is not normal floating point number");
    assert(relTolerance >= 0, "absTolerance is not positive");
    assert(absTolerance >= T.epsilon*2, "absTolerance is not greater then `2*T.epsilon`");
}
out (result)
{
    assert(isFinite(result.x));
}
body
{
    alias R = Unqual!(CommonType!(ReturnType!DF, T));
    // c is the squared inverse of the golden ratio
    // (3 - sqrt(5))/2
    // Value obtained from Wolfram Alpha.
    enum T c = 0x0.61c8864680b583ea0c633f9fa31237p+0L;
    enum T cm1 = 0x0.9e3779b97f4a7c15f39cc0605cedc8p+0L;
    R tolerance;
    T a = ax > bx ? bx : ax;
    T b = ax > bx ? ax : bx;
    // sequence of declarations suitable for SIMD instructions
    T  v = a * cm1 + b * c;
    assert(isFinite(v));
    R fv = f(v);
    if (isNaN(fv) || fv == -T.infinity)
    {
        return typeof(return)(v, fv, T.init);
    }
    T  w = v;
    R fw = fv;
    T  x = v;
    R fx = fv;
    size_t i;
    for (R d = 0, e = 0;;)
    {
        i++;
        T m = (a + b) / 2;
        // This fix is not part of the original algorithm
        if (!isFinite(m)) // fix infinity loop. Issue can be reproduced in R.
        {
            m = a / 2 + b / 2;
            if (!isFinite(m)) // fast-math compiler switch is enabled
            {
                //SIMD instructions can be used by compiler, do not reduce declarations
                int a_exp = void;
                int b_exp = void;
                immutable an = frexp(a, a_exp);
                immutable bn = frexp(b, b_exp);
                immutable am = ldexp(an, a_exp-1);
                immutable bm = ldexp(bn, b_exp-1);
                m = am + bm;
                if (!isFinite(m)) // wrong input: constraints are disabled in release mode
                {
                    return typeof(return).init;
                }
            }
        }
        tolerance = absTolerance * fabs(x) + relTolerance;
        immutable t2 = tolerance * 2;
        // check stopping criterion
        if (!(fabs(x - m) > t2 - (b - a) / 2))
        {
            break;
        }
        R p = 0;
        R q = 0;
        R r = 0;
        // fit parabola
        if (fabs(e) > tolerance)
        {
            immutable  xw =  x -  w;
            immutable fxw = fx - fw;
            immutable  xv =  x -  v;
            immutable fxv = fx - fv;
            immutable xwfxv = xw * fxv;
            immutable xvfxw = xv * fxw;
            p = xv * xvfxw - xw * xwfxv;
            q = (xvfxw - xwfxv) * 2;
            if (q > 0)
                p = -p;
            else
                q = -q;
            r = e;
            e = d;
        }
        T u;
        // a parabolic-interpolation step
        if (fabs(p) < fabs(q * r / 2) && p > q * (a - x) && p < q * (b - x))
        {
            d = p / q;
            u = x + d;
            // f must not be evaluated too close to a or b
            if (u - a < t2 || b - u < t2)
                d = x < m ? tolerance : -tolerance;
        }
        // a golden-section step
        else
        {
            e = (x < m ? b : a) - x;
            d = c * e;
        }
        // f must not be evaluated too close to x
        u = x + (fabs(d) >= tolerance ? d : d > 0 ? tolerance : -tolerance);
        immutable fu = f(u);
        if (isNaN(fu) || fu == -T.infinity)
        {
            return typeof(return)(u, fu, T.init);
        }
        //  update  a, b, v, w, and x
        if (fu <= fx)
        {
            u < x ? b : a = x;
            v = w; fv = fw;
            w = x; fw = fx;
            x = u; fx = fu;
        }
        else
        {
            u < x ? a : b = u;
            if (fu <= fw || w == x)
            {
                v = w; fv = fw;
                w = u; fw = fu;
            }
            else if (fu <= fv || v == x || v == w)
            { // do not remove this braces
                v = u; fv = fu;
            }
        }
    }
    return typeof(return)(x, fx, tolerance * 3);
}

///
@safe unittest
{
    import std.math : approxEqual;

    auto ret = findLocalMin((double x) => (x-4)^^2, -1e7, 1e7);
    assert(ret.x.approxEqual(4.0));
    assert(ret.y.approxEqual(0.0));
}

@safe unittest
{
    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(double, float, real))
    {
        {
            auto ret = findLocalMin!T((T x) => (x-4)^^2, T.min_normal, 1e7);
            assert(ret.x.approxEqual(T(4)));
            assert(ret.y.approxEqual(T(0)));
        }
        {
            auto ret = findLocalMin!T((T x) => fabs(x-1), -T.max/4, T.max/4, T.min_normal, 2*T.epsilon);
            assert(approxEqual(ret.x, T(1)));
            assert(approxEqual(ret.y, T(0)));
            assert(ret.error <= 10 * T.epsilon);
        }
        {
            auto ret = findLocalMin!T((T x) => T.init, 0, 1, T.min_normal, 2*T.epsilon);
            assert(!ret.x.isNaN);
            assert(ret.y.isNaN);
            assert(ret.error.isNaN);
        }
        {
            auto ret = findLocalMin!T((T x) => log(x), 0, 1, T.min_normal, 2*T.epsilon);
            assert(ret.error < 3.00001 * ((2*T.epsilon)*fabs(ret.x)+ T.min_normal));
            assert(ret.x >= 0 && ret.x <= ret.error);
        }
        {
            auto ret = findLocalMin!T((T x) => log(x), 0, T.max, T.min_normal, 2*T.epsilon);
            assert(ret.y < -18);
            assert(ret.error < 5e-08);
            assert(ret.x >= 0 && ret.x <= ret.error);
        }
        {
            auto ret = findLocalMin!T((T x) => -fabs(x), -1, 1, T.min_normal, 2*T.epsilon);
            assert(ret.x.fabs.approxEqual(T(1)));
            assert(ret.y.fabs.approxEqual(T(1)));
            assert(ret.error.approxEqual(T(0)));
        }
    }
}

/**
Computes $(LINK2 https://en.wikipedia.org/wiki/Euclidean_distance,
Euclidean distance) between input ranges $(D a) and
$(D b). The two ranges must have the same length. The three-parameter
version stops computation as soon as the distance is greater than or
equal to $(D limit) (this is useful to save computation if a small
distance is sought).
 */
CommonType!(ElementType!(Range1), ElementType!(Range2))
euclideanDistance(Range1, Range2)(Range1 a, Range2 b)
if (isInputRange!(Range1) && isInputRange!(Range2))
{
    enum bool haveLen = hasLength!(Range1) && hasLength!(Range2);
    static if (haveLen) assert(a.length == b.length);
    Unqual!(typeof(return)) result = 0;
    for (; !a.empty; a.popFront(), b.popFront())
    {
        immutable t = a.front - b.front;
        result += t * t;
    }
    static if (!haveLen) assert(b.empty);
    return sqrt(result);
}

/// Ditto
CommonType!(ElementType!(Range1), ElementType!(Range2))
euclideanDistance(Range1, Range2, F)(Range1 a, Range2 b, F limit)
if (isInputRange!(Range1) && isInputRange!(Range2))
{
    limit *= limit;
    enum bool haveLen = hasLength!(Range1) && hasLength!(Range2);
    static if (haveLen) assert(a.length == b.length);
    Unqual!(typeof(return)) result = 0;
    for (; ; a.popFront(), b.popFront())
    {
        if (a.empty)
        {
            static if (!haveLen) assert(b.empty);
            break;
        }
        immutable t = a.front - b.front;
        result += t * t;
        if (result >= limit) break;
    }
    return sqrt(result);
}

@safe unittest
{
    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(double, const double, immutable double))
    {
        T[] a = [ 1.0, 2.0, ];
        T[] b = [ 4.0, 6.0, ];
        assert(euclideanDistance(a, b) == 5);
        assert(euclideanDistance(a, b, 5) == 5);
        assert(euclideanDistance(a, b, 4) == 5);
        assert(euclideanDistance(a, b, 2) == 3);
    }
}

/**
Computes the $(LINK2 https://en.wikipedia.org/wiki/Dot_product,
dot product) of input ranges $(D a) and $(D
b). The two ranges must have the same length. If both ranges define
length, the check is done once; otherwise, it is done at each
iteration.
 */
CommonType!(ElementType!(Range1), ElementType!(Range2))
dotProduct(Range1, Range2)(Range1 a, Range2 b)
if (isInputRange!(Range1) && isInputRange!(Range2) &&
    !(isArray!(Range1) && isArray!(Range2)))
{
    enum bool haveLen = hasLength!(Range1) && hasLength!(Range2);
    static if (haveLen) assert(a.length == b.length);
    Unqual!(typeof(return)) result = 0;
    for (; !a.empty; a.popFront(), b.popFront())
    {
        result += a.front * b.front;
    }
    static if (!haveLen) assert(b.empty);
    return result;
}

/// Ditto
CommonType!(F1, F2)
dotProduct(F1, F2)(in F1[] avector, in F2[] bvector)
{
    immutable n = avector.length;
    assert(n == bvector.length);
    auto avec = avector.ptr, bvec = bvector.ptr;
    Unqual!(typeof(return)) sum0 = 0, sum1 = 0;

    const all_endp = avec + n;
    const smallblock_endp = avec + (n & ~3);
    const bigblock_endp = avec + (n & ~15);

    for (; avec != bigblock_endp; avec += 16, bvec += 16)
    {
        sum0 += avec[0] * bvec[0];
        sum1 += avec[1] * bvec[1];
        sum0 += avec[2] * bvec[2];
        sum1 += avec[3] * bvec[3];
        sum0 += avec[4] * bvec[4];
        sum1 += avec[5] * bvec[5];
        sum0 += avec[6] * bvec[6];
        sum1 += avec[7] * bvec[7];
        sum0 += avec[8] * bvec[8];
        sum1 += avec[9] * bvec[9];
        sum0 += avec[10] * bvec[10];
        sum1 += avec[11] * bvec[11];
        sum0 += avec[12] * bvec[12];
        sum1 += avec[13] * bvec[13];
        sum0 += avec[14] * bvec[14];
        sum1 += avec[15] * bvec[15];
    }

    for (; avec != smallblock_endp; avec += 4, bvec += 4)
    {
        sum0 += avec[0] * bvec[0];
        sum1 += avec[1] * bvec[1];
        sum0 += avec[2] * bvec[2];
        sum1 += avec[3] * bvec[3];
    }

    sum0 += sum1;

    /* Do trailing portion in naive loop. */
    while (avec != all_endp)
    {
        sum0 += *avec * *bvec;
        ++avec;
        ++bvec;
    }

    return sum0;
}

@system unittest
{
    // @system due to dotProduct and assertCTFEable
    import std.exception : assertCTFEable;
    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(double, const double, immutable double))
    {
        T[] a = [ 1.0, 2.0, ];
        T[] b = [ 4.0, 6.0, ];
        assert(dotProduct(a, b) == 16);
        assert(dotProduct([1, 3, -5], [4, -2, -1]) == 3);
    }

    // Make sure the unrolled loop codepath gets tested.
    static const x =
        [1.0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18];
    static const y =
        [2.0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19];
    assertCTFEable!({ assert(dotProduct(x, y) == 2280); });
}

/**
Computes the $(LINK2 https://en.wikipedia.org/wiki/Cosine_similarity,
cosine similarity) of input ranges $(D a) and $(D
b). The two ranges must have the same length. If both ranges define
length, the check is done once; otherwise, it is done at each
iteration. If either range has all-zero elements, return 0.
 */
CommonType!(ElementType!(Range1), ElementType!(Range2))
cosineSimilarity(Range1, Range2)(Range1 a, Range2 b)
if (isInputRange!(Range1) && isInputRange!(Range2))
{
    enum bool haveLen = hasLength!(Range1) && hasLength!(Range2);
    static if (haveLen) assert(a.length == b.length);
    Unqual!(typeof(return)) norma = 0, normb = 0, dotprod = 0;
    for (; !a.empty; a.popFront(), b.popFront())
    {
        immutable t1 = a.front, t2 = b.front;
        norma += t1 * t1;
        normb += t2 * t2;
        dotprod += t1 * t2;
    }
    static if (!haveLen) assert(b.empty);
    if (norma == 0 || normb == 0) return 0;
    return dotprod / sqrt(norma * normb);
}

@safe unittest
{
    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(double, const double, immutable double))
    {
        T[] a = [ 1.0, 2.0, ];
        T[] b = [ 4.0, 3.0, ];
        assert(approxEqual(
                    cosineSimilarity(a, b), 10.0 / sqrt(5.0 * 25),
                    0.01));
    }
}

/**
Normalizes values in $(D range) by multiplying each element with a
number chosen such that values sum up to $(D sum). If elements in $(D
range) sum to zero, assigns $(D sum / range.length) to
all. Normalization makes sense only if all elements in $(D range) are
positive. $(D normalize) assumes that is the case without checking it.

Returns: $(D true) if normalization completed normally, $(D false) if
all elements in $(D range) were zero or if $(D range) is empty.
 */
bool normalize(R)(R range, ElementType!(R) sum = 1)
if (isForwardRange!(R))
{
    ElementType!(R) s = 0;
    // Step 1: Compute sum and length of the range
    static if (hasLength!(R))
    {
        const length = range.length;
        foreach (e; range)
        {
            s += e;
        }
    }
    else
    {
        uint length = 0;
        foreach (e; range)
        {
            s += e;
            ++length;
        }
    }
    // Step 2: perform normalization
    if (s == 0)
    {
        if (length)
        {
            immutable f = sum / range.length;
            foreach (ref e; range) e = f;
        }
        return false;
    }
    // The path most traveled
    assert(s >= 0);
    immutable f = sum / s;
    foreach (ref e; range)
        e *= f;
    return true;
}

///
@safe unittest
{
    double[] a = [];
    assert(!normalize(a));
    a = [ 1.0, 3.0 ];
    assert(normalize(a));
    assert(a == [ 0.25, 0.75 ]);
    a = [ 0.0, 0.0 ];
    assert(!normalize(a));
    assert(a == [ 0.5, 0.5 ]);
}

/**
Compute the sum of binary logarithms of the input range $(D r).
The error of this method is much smaller than with a naive sum of log2.
 */
ElementType!Range sumOfLog2s(Range)(Range r)
if (isInputRange!Range && isFloatingPoint!(ElementType!Range))
{
    long exp = 0;
    Unqual!(typeof(return)) x = 1;
    foreach (e; r)
    {
        if (e < 0)
            return typeof(return).nan;
        int lexp = void;
        x *= frexp(e, lexp);
        exp += lexp;
        if (x < 0.5)
        {
            x *= 2;
            exp--;
        }
    }
    return exp + log2(x);
}

///
@safe unittest
{
    import std.math : isNaN;

    assert(sumOfLog2s(new double[0]) == 0);
    assert(sumOfLog2s([0.0L]) == -real.infinity);
    assert(sumOfLog2s([-0.0L]) == -real.infinity);
    assert(sumOfLog2s([2.0L]) == 1);
    assert(sumOfLog2s([-2.0L]).isNaN());
    assert(sumOfLog2s([real.nan]).isNaN());
    assert(sumOfLog2s([-real.nan]).isNaN());
    assert(sumOfLog2s([real.infinity]) == real.infinity);
    assert(sumOfLog2s([-real.infinity]).isNaN());
    assert(sumOfLog2s([ 0.25, 0.25, 0.25, 0.125 ]) == -9);
}

/**
Computes $(LINK2 https://en.wikipedia.org/wiki/Entropy_(information_theory),
_entropy) of input range $(D r) in bits. This
function assumes (without checking) that the values in $(D r) are all
in $(D [0, 1]). For the entropy to be meaningful, often $(D r) should
be normalized too (i.e., its values should sum to 1). The
two-parameter version stops evaluating as soon as the intermediate
result is greater than or equal to $(D max).
 */
ElementType!Range entropy(Range)(Range r)
if (isInputRange!Range)
{
    Unqual!(typeof(return)) result = 0.0;
    for (;!r.empty; r.popFront)
    {
        if (!r.front) continue;
        result -= r.front * log2(r.front);
    }
    return result;
}

/// Ditto
ElementType!Range entropy(Range, F)(Range r, F max)
if (isInputRange!Range &&
    !is(CommonType!(ElementType!Range, F) == void))
{
    Unqual!(typeof(return)) result = 0.0;
    for (;!r.empty; r.popFront)
    {
        if (!r.front) continue;
        result -= r.front * log2(r.front);
        if (result >= max) break;
    }
    return result;
}

@safe unittest
{
    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(double, const double, immutable double))
    {
        T[] p = [ 0.0, 0, 0, 1 ];
        assert(entropy(p) == 0);
        p = [ 0.25, 0.25, 0.25, 0.25 ];
        assert(entropy(p) == 2);
        assert(entropy(p, 1) == 1);
    }
}

/**
Computes the $(LINK2 https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence,
Kullback-Leibler divergence) between input ranges
$(D a) and $(D b), which is the sum $(D ai * log(ai / bi)). The base
of logarithm is 2. The ranges are assumed to contain elements in $(D
[0, 1]). Usually the ranges are normalized probability distributions,
but this is not required or checked by $(D
kullbackLeiblerDivergence). If any element $(D bi) is zero and the
corresponding element $(D ai) nonzero, returns infinity. (Otherwise,
if $(D ai == 0 && bi == 0), the term $(D ai * log(ai / bi)) is
considered zero.) If the inputs are normalized, the result is
positive.
 */
CommonType!(ElementType!Range1, ElementType!Range2)
kullbackLeiblerDivergence(Range1, Range2)(Range1 a, Range2 b)
if (isInputRange!(Range1) && isInputRange!(Range2))
{
    enum bool haveLen = hasLength!(Range1) && hasLength!(Range2);
    static if (haveLen) assert(a.length == b.length);
    Unqual!(typeof(return)) result = 0;
    for (; !a.empty; a.popFront(), b.popFront())
    {
        immutable t1 = a.front;
        if (t1 == 0) continue;
        immutable t2 = b.front;
        if (t2 == 0) return result.infinity;
        assert(t1 > 0 && t2 > 0);
        result += t1 * log2(t1 / t2);
    }
    static if (!haveLen) assert(b.empty);
    return result;
}

///
@safe unittest
{
    import std.math : approxEqual;

    double[] p = [ 0.0, 0, 0, 1 ];
    assert(kullbackLeiblerDivergence(p, p) == 0);
    double[] p1 = [ 0.25, 0.25, 0.25, 0.25 ];
    assert(kullbackLeiblerDivergence(p1, p1) == 0);
    assert(kullbackLeiblerDivergence(p, p1) == 2);
    assert(kullbackLeiblerDivergence(p1, p) == double.infinity);
    double[] p2 = [ 0.2, 0.2, 0.2, 0.4 ];
    assert(approxEqual(kullbackLeiblerDivergence(p1, p2), 0.0719281));
    assert(approxEqual(kullbackLeiblerDivergence(p2, p1), 0.0780719));
}

/**
Computes the $(LINK2 https://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence,
Jensen-Shannon divergence) between $(D a) and $(D
b), which is the sum $(D (ai * log(2 * ai / (ai + bi)) + bi * log(2 *
bi / (ai + bi))) / 2). The base of logarithm is 2. The ranges are
assumed to contain elements in $(D [0, 1]). Usually the ranges are
normalized probability distributions, but this is not required or
checked by $(D jensenShannonDivergence). If the inputs are normalized,
the result is bounded within $(D [0, 1]). The three-parameter version
stops evaluations as soon as the intermediate result is greater than
or equal to $(D limit).
 */
CommonType!(ElementType!Range1, ElementType!Range2)
jensenShannonDivergence(Range1, Range2)(Range1 a, Range2 b)
if (isInputRange!Range1 && isInputRange!Range2 &&
    is(CommonType!(ElementType!Range1, ElementType!Range2)))
{
    enum bool haveLen = hasLength!(Range1) && hasLength!(Range2);
    static if (haveLen) assert(a.length == b.length);
    Unqual!(typeof(return)) result = 0;
    for (; !a.empty; a.popFront(), b.popFront())
    {
        immutable t1 = a.front;
        immutable t2 = b.front;
        immutable avg = (t1 + t2) / 2;
        if (t1 != 0)
        {
            result += t1 * log2(t1 / avg);
        }
        if (t2 != 0)
        {
            result += t2 * log2(t2 / avg);
        }
    }
    static if (!haveLen) assert(b.empty);
    return result / 2;
}

/// Ditto
CommonType!(ElementType!Range1, ElementType!Range2)
jensenShannonDivergence(Range1, Range2, F)(Range1 a, Range2 b, F limit)
if (isInputRange!Range1 && isInputRange!Range2 &&
    is(typeof(CommonType!(ElementType!Range1, ElementType!Range2).init
    >= F.init) : bool))
{
    enum bool haveLen = hasLength!(Range1) && hasLength!(Range2);
    static if (haveLen) assert(a.length == b.length);
    Unqual!(typeof(return)) result = 0;
    limit *= 2;
    for (; !a.empty; a.popFront(), b.popFront())
    {
        immutable t1 = a.front;
        immutable t2 = b.front;
        immutable avg = (t1 + t2) / 2;
        if (t1 != 0)
        {
            result += t1 * log2(t1 / avg);
        }
        if (t2 != 0)
        {
            result += t2 * log2(t2 / avg);
        }
        if (result >= limit) break;
    }
    static if (!haveLen) assert(b.empty);
    return result / 2;
}

///
@safe unittest
{
    import std.math : approxEqual;

    double[] p = [ 0.0, 0, 0, 1 ];
    assert(jensenShannonDivergence(p, p) == 0);
    double[] p1 = [ 0.25, 0.25, 0.25, 0.25 ];
    assert(jensenShannonDivergence(p1, p1) == 0);
    assert(approxEqual(jensenShannonDivergence(p1, p), 0.548795));
    double[] p2 = [ 0.2, 0.2, 0.2, 0.4 ];
    assert(approxEqual(jensenShannonDivergence(p1, p2), 0.0186218));
    assert(approxEqual(jensenShannonDivergence(p2, p1), 0.0186218));
    assert(approxEqual(jensenShannonDivergence(p2, p1, 0.005), 0.00602366));
}

/**
The so-called "all-lengths gap-weighted string kernel" computes a
similarity measure between $(D s) and $(D t) based on all of their
common subsequences of all lengths. Gapped subsequences are also
included.

To understand what $(D gapWeightedSimilarity(s, t, lambda)) computes,
consider first the case $(D lambda = 1) and the strings $(D s =
["Hello", "brave", "new", "world"]) and $(D t = ["Hello", "new",
"world"]). In that case, $(D gapWeightedSimilarity) counts the
following matches:

$(OL $(LI three matches of length 1, namely $(D "Hello"), $(D "new"),
and $(D "world");) $(LI three matches of length 2, namely ($(D
"Hello", "new")), ($(D "Hello", "world")), and ($(D "new", "world"));)
$(LI one match of length 3, namely ($(D "Hello", "new", "world")).))

The call $(D gapWeightedSimilarity(s, t, 1)) simply counts all of
these matches and adds them up, returning 7.

----
string[] s = ["Hello", "brave", "new", "world"];
string[] t = ["Hello", "new", "world"];
assert(gapWeightedSimilarity(s, t, 1) == 7);
----

Note how the gaps in matching are simply ignored, for example ($(D
"Hello", "new")) is deemed as good a match as ($(D "new",
"world")). This may be too permissive for some applications. To
eliminate gapped matches entirely, use $(D lambda = 0):

----
string[] s = ["Hello", "brave", "new", "world"];
string[] t = ["Hello", "new", "world"];
assert(gapWeightedSimilarity(s, t, 0) == 4);
----

The call above eliminated the gapped matches ($(D "Hello", "new")),
($(D "Hello", "world")), and ($(D "Hello", "new", "world")) from the
tally. That leaves only 4 matches.

The most interesting case is when gapped matches still participate in
the result, but not as strongly as ungapped matches. The result will
be a smooth, fine-grained similarity measure between the input
strings. This is where values of $(D lambda) between 0 and 1 enter
into play: gapped matches are $(I exponentially penalized with the
number of gaps) with base $(D lambda). This means that an ungapped
match adds 1 to the return value; a match with one gap in either
string adds $(D lambda) to the return value; ...; a match with a total
of $(D n) gaps in both strings adds $(D pow(lambda, n)) to the return
value. In the example above, we have 4 matches without gaps, 2 matches
with one gap, and 1 match with three gaps. The latter match is ($(D
"Hello", "world")), which has two gaps in the first string and one gap
in the second string, totaling to three gaps. Summing these up we get
$(D 4 + 2 * lambda + pow(lambda, 3)).

----
string[] s = ["Hello", "brave", "new", "world"];
string[] t = ["Hello", "new", "world"];
assert(gapWeightedSimilarity(s, t, 0.5) == 4 + 0.5 * 2 + 0.125);
----

$(D gapWeightedSimilarity) is useful wherever a smooth similarity
measure between sequences allowing for approximate matches is
needed. The examples above are given with words, but any sequences
with elements comparable for equality are allowed, e.g. characters or
numbers. $(D gapWeightedSimilarity) uses a highly optimized dynamic
programming implementation that needs $(D 16 * min(s.length,
t.length)) extra bytes of memory and $(BIGOH s.length * t.length) time
to complete.
 */
F gapWeightedSimilarity(alias comp = "a == b", R1, R2, F)(R1 s, R2 t, F lambda)
if (isRandomAccessRange!(R1) && hasLength!(R1) &&
    isRandomAccessRange!(R2) && hasLength!(R2))
{
    import core.exception : onOutOfMemoryError;
    import core.stdc.stdlib : malloc, free;
    import std.algorithm.mutation : swap;
    import std.functional : binaryFun;

    if (s.length < t.length) return gapWeightedSimilarity(t, s, lambda);
    if (!t.length) return 0;

    auto dpvi = cast(F*) malloc(F.sizeof * 2 * t.length);
    if (!dpvi)
        onOutOfMemoryError();

    auto dpvi1 = dpvi + t.length;
    scope(exit) free(dpvi < dpvi1 ? dpvi : dpvi1);
    dpvi[0 .. t.length] = 0;
    dpvi1[0] = 0;
    immutable lambda2 = lambda * lambda;

    F result = 0;
    foreach (i; 0 .. s.length)
    {
        const si = s[i];
        for (size_t j = 0;;)
        {
            F dpsij = void;
            if (binaryFun!(comp)(si, t[j]))
            {
                dpsij = 1 + dpvi[j];
                result += dpsij;
            }
            else
            {
                dpsij = 0;
            }
            immutable j1 = j + 1;
            if (j1 == t.length) break;
            dpvi1[j1] = dpsij + lambda * (dpvi1[j] + dpvi[j1]) -
                        lambda2 * dpvi[j];
            j = j1;
        }
        swap(dpvi, dpvi1);
    }
    return result;
}

@system unittest
{
    string[] s = ["Hello", "brave", "new", "world"];
    string[] t = ["Hello", "new", "world"];
    assert(gapWeightedSimilarity(s, t, 1) == 7);
    assert(gapWeightedSimilarity(s, t, 0) == 4);
    assert(gapWeightedSimilarity(s, t, 0.5) == 4 + 2 * 0.5 + 0.125);
}

/**
The similarity per $(D gapWeightedSimilarity) has an issue in that it
grows with the lengths of the two strings, even though the strings are
not actually very similar. For example, the range $(D ["Hello",
"world"]) is increasingly similar with the range $(D ["Hello",
"world", "world", "world",...]) as more instances of $(D "world") are
appended. To prevent that, $(D gapWeightedSimilarityNormalized)
computes a normalized version of the similarity that is computed as
$(D gapWeightedSimilarity(s, t, lambda) /
sqrt(gapWeightedSimilarity(s, t, lambda) * gapWeightedSimilarity(s, t,
lambda))). The function $(D gapWeightedSimilarityNormalized) (a
so-called normalized kernel) is bounded in $(D [0, 1]), reaches $(D 0)
only for ranges that don't match in any position, and $(D 1) only for
identical ranges.

The optional parameters $(D sSelfSim) and $(D tSelfSim) are meant for
avoiding duplicate computation. Many applications may have already
computed $(D gapWeightedSimilarity(s, s, lambda)) and/or $(D
gapWeightedSimilarity(t, t, lambda)). In that case, they can be passed
as $(D sSelfSim) and $(D tSelfSim), respectively.
 */
Select!(isFloatingPoint!(F), F, double)
gapWeightedSimilarityNormalized(alias comp = "a == b", R1, R2, F)
        (R1 s, R2 t, F lambda, F sSelfSim = F.init, F tSelfSim = F.init)
if (isRandomAccessRange!(R1) && hasLength!(R1) &&
    isRandomAccessRange!(R2) && hasLength!(R2))
{
    static bool uncomputed(F n)
    {
        static if (isFloatingPoint!(F))
            return isNaN(n);
        else
            return n == n.init;
    }
    if (uncomputed(sSelfSim))
        sSelfSim = gapWeightedSimilarity!(comp)(s, s, lambda);
    if (sSelfSim == 0) return 0;
    if (uncomputed(tSelfSim))
        tSelfSim = gapWeightedSimilarity!(comp)(t, t, lambda);
    if (tSelfSim == 0) return 0;

    return gapWeightedSimilarity!(comp)(s, t, lambda) /
           sqrt(cast(typeof(return)) sSelfSim * tSelfSim);
}

///
@system unittest
{
    import std.math : approxEqual, sqrt;

    string[] s = ["Hello", "brave", "new", "world"];
    string[] t = ["Hello", "new", "world"];
    assert(gapWeightedSimilarity(s, s, 1) == 15);
    assert(gapWeightedSimilarity(t, t, 1) == 7);
    assert(gapWeightedSimilarity(s, t, 1) == 7);
    assert(approxEqual(gapWeightedSimilarityNormalized(s, t, 1),
                    7.0 / sqrt(15.0 * 7), 0.01));
}

/**
Similar to $(D gapWeightedSimilarity), just works in an incremental
manner by first revealing the matches of length 1, then gapped matches
of length 2, and so on. The memory requirement is $(BIGOH s.length *
t.length). The time complexity is $(BIGOH s.length * t.length) time
for computing each step. Continuing on the previous example:

The implementation is based on the pseudocode in Fig. 4 of the paper
$(HTTP jmlr.csail.mit.edu/papers/volume6/rousu05a/rousu05a.pdf,
"Efﬁcient Computation of Gapped Substring Kernels on Large Alphabets")
by Rousu et al., with additional algorithmic and systems-level
optimizations.
 */
struct GapWeightedSimilarityIncremental(Range, F = double)
if (isRandomAccessRange!(Range) && hasLength!(Range))
{
    import core.stdc.stdlib : malloc, realloc, alloca, free;

private:
    Range s, t;
    F currentValue = 0;
    F* kl;
    size_t gram = void;
    F lambda = void, lambda2 = void;

public:
/**
Constructs an object given two ranges $(D s) and $(D t) and a penalty
$(D lambda). Constructor completes in $(BIGOH s.length * t.length)
time and computes all matches of length 1.
 */
    this(Range s, Range t, F lambda)
    {
        import core.exception : onOutOfMemoryError;

        assert(lambda > 0);
        this.gram = 0;
        this.lambda = lambda;
        this.lambda2 = lambda * lambda; // for efficiency only

        size_t iMin = size_t.max, jMin = size_t.max,
            iMax = 0, jMax = 0;
        /* initialize */
        Tuple!(size_t, size_t) * k0;
        size_t k0len;
        scope(exit) free(k0);
        currentValue = 0;
        foreach (i, si; s)
        {
            foreach (j; 0 .. t.length)
            {
                if (si != t[j]) continue;
                k0 = cast(typeof(k0)) realloc(k0, ++k0len * (*k0).sizeof);
                with (k0[k0len - 1])
                {
                    field[0] = i;
                    field[1] = j;
                }
                // Maintain the minimum and maximum i and j
                if (iMin > i) iMin = i;
                if (iMax < i) iMax = i;
                if (jMin > j) jMin = j;
                if (jMax < j) jMax = j;
            }
        }

        if (iMin > iMax) return;
        assert(k0len);

        currentValue = k0len;
        // Chop strings down to the useful sizes
        s = s[iMin .. iMax + 1];
        t = t[jMin .. jMax + 1];
        this.s = s;
        this.t = t;

        kl = cast(F*) malloc(s.length * t.length * F.sizeof);
        if (!kl)
            onOutOfMemoryError();

        kl[0 .. s.length * t.length] = 0;
        foreach (pos; 0 .. k0len)
        {
            with (k0[pos])
            {
                kl[(field[0] - iMin) * t.length + field[1] -jMin] = lambda2;
            }
        }
    }

    /**
    Returns: $(D this).
     */
    ref GapWeightedSimilarityIncremental opSlice()
    {
        return this;
    }

    /**
    Computes the match of the popFront length. Completes in $(BIGOH s.length *
    t.length) time.
     */
    void popFront()
    {
        import std.algorithm.mutation : swap;

        // This is a large source of optimization: if similarity at
        // the gram-1 level was 0, then we can safely assume
        // similarity at the gram level is 0 as well.
        if (empty) return;

        // Now attempt to match gapped substrings of length `gram'
        ++gram;
        currentValue = 0;

        auto Si = cast(F*) alloca(t.length * F.sizeof);
        Si[0 .. t.length] = 0;
        foreach (i; 0 .. s.length)
        {
            const si = s[i];
            F Sij_1 = 0;
            F Si_1j_1 = 0;
            auto kli = kl + i * t.length;
            for (size_t j = 0;;)
            {
                const klij = kli[j];
                const Si_1j = Si[j];
                const tmp = klij + lambda * (Si_1j + Sij_1) - lambda2 * Si_1j_1;
                // now update kl and currentValue
                if (si == t[j])
                    currentValue += kli[j] = lambda2 * Si_1j_1;
                else
                    kli[j] = 0;
                // commit to Si
                Si[j] = tmp;
                if (++j == t.length) break;
                // get ready for the popFront step; virtually increment j,
                // so essentially stuffj_1 <-- stuffj
                Si_1j_1 = Si_1j;
                Sij_1 = tmp;
            }
        }
        currentValue /= pow(lambda, 2 * (gram + 1));

        version (none)
        {
            Si_1[0 .. t.length] = 0;
            kl[0 .. min(t.length, maxPerimeter + 1)] = 0;
            foreach (i; 1 .. min(s.length, maxPerimeter + 1))
            {
                auto kli = kl + i * t.length;
                assert(s.length > i);
                const si = s[i];
                auto kl_1i_1 = kl_1 + (i - 1) * t.length;
                kli[0] = 0;
                F lastS = 0;
                foreach (j; 1 .. min(maxPerimeter - i + 1, t.length))
                {
                    immutable j_1 = j - 1;
                    immutable tmp = kl_1i_1[j_1]
                        + lambda * (Si_1[j] + lastS)
                        - lambda2 * Si_1[j_1];
                    kl_1i_1[j_1] = float.nan;
                    Si_1[j_1] = lastS;
                    lastS = tmp;
                    if (si == t[j])
                    {
                        currentValue += kli[j] = lambda2 * lastS;
                    }
                    else
                    {
                        kli[j] = 0;
                    }
                }
                Si_1[t.length - 1] = lastS;
            }
            currentValue /= pow(lambda, 2 * (gram + 1));
            // get ready for the popFront computation
            swap(kl, kl_1);
        }
    }

    /**
    Returns: The gapped similarity at the current match length (initially
    1, grows with each call to $(D popFront)).
    */
    @property F front() { return currentValue; }

    /**
    Returns: Whether there are more matches.
     */
    @property bool empty()
    {
        if (currentValue) return false;
        if (kl)
        {
            free(kl);
            kl = null;
        }
        return true;
    }
}

/**
Ditto
 */
GapWeightedSimilarityIncremental!(R, F) gapWeightedSimilarityIncremental(R, F)
(R r1, R r2, F penalty)
{
    return typeof(return)(r1, r2, penalty);
}

///
@system unittest
{
    string[] s = ["Hello", "brave", "new", "world"];
    string[] t = ["Hello", "new", "world"];
    auto simIter = gapWeightedSimilarityIncremental(s, t, 1.0);
    assert(simIter.front == 3); // three 1-length matches
    simIter.popFront();
    assert(simIter.front == 3); // three 2-length matches
    simIter.popFront();
    assert(simIter.front == 1); // one 3-length match
    simIter.popFront();
    assert(simIter.empty);     // no more match
}

@system unittest
{
    import std.conv : text;
    string[] s = ["Hello", "brave", "new", "world"];
    string[] t = ["Hello", "new", "world"];
    auto simIter = gapWeightedSimilarityIncremental(s, t, 1.0);
    //foreach (e; simIter) writeln(e);
    assert(simIter.front == 3); // three 1-length matches
    simIter.popFront();
    assert(simIter.front == 3, text(simIter.front)); // three 2-length matches
    simIter.popFront();
    assert(simIter.front == 1); // one 3-length matches
    simIter.popFront();
    assert(simIter.empty);     // no more match

    s = ["Hello"];
    t = ["bye"];
    simIter = gapWeightedSimilarityIncremental(s, t, 0.5);
    assert(simIter.empty);

    s = ["Hello"];
    t = ["Hello"];
    simIter = gapWeightedSimilarityIncremental(s, t, 0.5);
    assert(simIter.front == 1); // one match
    simIter.popFront();
    assert(simIter.empty);

    s = ["Hello", "world"];
    t = ["Hello"];
    simIter = gapWeightedSimilarityIncremental(s, t, 0.5);
    assert(simIter.front == 1); // one match
    simIter.popFront();
    assert(simIter.empty);

    s = ["Hello", "world"];
    t = ["Hello", "yah", "world"];
    simIter = gapWeightedSimilarityIncremental(s, t, 0.5);
    assert(simIter.front == 2); // two 1-gram matches
    simIter.popFront();
    assert(simIter.front == 0.5, text(simIter.front)); // one 2-gram match, 1 gap
}

@system unittest
{
    GapWeightedSimilarityIncremental!(string[]) sim =
        GapWeightedSimilarityIncremental!(string[])(
            ["nyuk", "I", "have", "no", "chocolate", "giba"],
            ["wyda", "I", "have", "I", "have", "have", "I", "have", "hehe"],
            0.5);
    double[] witness = [ 7.0, 4.03125, 0, 0 ];
    foreach (e; sim)
    {
        //writeln(e);
        assert(e == witness.front);
        witness.popFront();
    }
    witness = [ 3.0, 1.3125, 0.25 ];
    sim = GapWeightedSimilarityIncremental!(string[])(
        ["I", "have", "no", "chocolate"],
        ["I", "have", "some", "chocolate"],
        0.5);
    foreach (e; sim)
    {
        //writeln(e);
        assert(e == witness.front);
        witness.popFront();
    }
    assert(witness.empty);
}

/**
Computes the greatest common divisor of $(D a) and $(D b) by using
an efficient algorithm such as $(HTTPS en.wikipedia.org/wiki/Euclidean_algorithm, Euclid's)
or $(HTTPS en.wikipedia.org/wiki/Binary_GCD_algorithm, Stein's) algorithm.

Params:
    T = Any numerical type that supports the modulo operator `%`. If
        bit-shifting `<<` and `>>` are also supported, Stein's algorithm will
        be used; otherwise, Euclid's algorithm is used as _a fallback.
Returns:
    The greatest common divisor of the given arguments.
 */
T gcd(T)(T a, T b)
    if (isIntegral!T)
{
    static if (is(T == const) || is(T == immutable))
    {
        return gcd!(Unqual!T)(a, b);
    }
    else version (DigitalMars)
    {
        static if (T.min < 0)
        {
            assert(a >= 0 && b >= 0);
        }
        while (b)
        {
            immutable t = b;
            b = a % b;
            a = t;
        }
        return a;
    }
    else
    {
        if (a == 0)
            return b;
        if (b == 0)
            return a;

        import core.bitop : bsf;
        import std.algorithm.mutation : swap;

        immutable uint shift = bsf(a | b);
        a >>= a.bsf;

        do
        {
            b >>= b.bsf;
            if (a > b)
                swap(a, b);
            b -= a;
        } while (b);

        return a << shift;
    }
}

///
@safe unittest
{
    assert(gcd(2 * 5 * 7 * 7, 5 * 7 * 11) == 5 * 7);
    const int a = 5 * 13 * 23 * 23, b = 13 * 59;
    assert(gcd(a, b) == 13);
}

// This overload is for non-builtin numerical types like BigInt or
// user-defined types.
/// ditto
T gcd(T)(T a, T b)
    if (!isIntegral!T &&
        is(typeof(T.init % T.init)) &&
        is(typeof(T.init == 0 || T.init > 0)))
{
    import std.algorithm.mutation : swap;

    enum canUseBinaryGcd = is(typeof(() {
        T t, u;
        t <<= 1;
        t >>= 1;
        t -= u;
        bool b = (t & 1) == 0;
        swap(t, u);
    }));

    assert(a >= 0 && b >= 0);

    static if (canUseBinaryGcd)
    {
        uint shift = 0;
        while ((a & 1) == 0 && (b & 1) == 0)
        {
            a >>= 1;
            b >>= 1;
            shift++;
        }

        do
        {
            assert((a & 1) != 0);
            while ((b & 1) == 0)
                b >>= 1;
            if (a > b)
                swap(a, b);
            b -= a;
        } while (b);

        return a << shift;
    }
    else
    {
        // The only thing we have is %; fallback to Euclidean algorithm.
        while (b != 0)
        {
            auto t = b;
            b = a % b;
            a = t;
        }
        return a;
    }
}

// Issue 7102
@system pure unittest
{
    import std.bigint : BigInt;
    assert(gcd(BigInt("71_000_000_000_000_000_000"),
               BigInt("31_000_000_000_000_000_000")) ==
           BigInt("1_000_000_000_000_000_000"));
}

@safe pure nothrow unittest
{
    // A numerical type that only supports % and - (to force gcd implementation
    // to use Euclidean algorithm).
    struct CrippledInt
    {
        int impl;
        CrippledInt opBinary(string op : "%")(CrippledInt i)
        {
            return CrippledInt(impl % i.impl);
        }
        int opEquals(CrippledInt i) { return impl == i.impl; }
        int opEquals(int i) { return impl == i; }
        int opCmp(int i) { return (impl < i) ? -1 : (impl > i) ? 1 : 0; }
    }
    assert(gcd(CrippledInt(2310), CrippledInt(1309)) == CrippledInt(77));
}

// This is to make tweaking the speed/size vs. accuracy tradeoff easy,
// though floats seem accurate enough for all practical purposes, since
// they pass the "approxEqual(inverseFft(fft(arr)), arr)" test even for
// size 2 ^^ 22.
private alias lookup_t = float;

/**A class for performing fast Fourier transforms of power of two sizes.
 * This class encapsulates a large amount of state that is reusable when
 * performing multiple FFTs of sizes smaller than or equal to that specified
 * in the constructor.  This results in substantial speedups when performing
 * multiple FFTs with a known maximum size.  However,
 * a free function API is provided for convenience if you need to perform a
 * one-off FFT.
 *
 * References:
 * $(HTTP en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm)
 */
final class Fft
{
    import core.bitop : bsf;
    import std.algorithm.iteration : map;
    import std.array : uninitializedArray;

private:
    immutable lookup_t[][] negSinLookup;

    void enforceSize(R)(R range) const
    {
        import std.conv : text;
        assert(range.length <= size, text(
            "FFT size mismatch.  Expected ", size, ", got ", range.length));
    }

    void fftImpl(Ret, R)(Stride!R range, Ret buf) const
    in
    {
        assert(range.length >= 4);
        assert(isPowerOf2(range.length));
    }
    body
    {
        auto recurseRange = range;
        recurseRange.doubleSteps();

        if (buf.length > 4)
        {
            fftImpl(recurseRange, buf[0..$ / 2]);
            recurseRange.popHalf();
            fftImpl(recurseRange, buf[$ / 2..$]);
        }
        else
        {
            // Do this here instead of in another recursion to save on
            // recursion overhead.
            slowFourier2(recurseRange, buf[0..$ / 2]);
            recurseRange.popHalf();
            slowFourier2(recurseRange, buf[$ / 2..$]);
        }

        butterfly(buf);
    }

    // This algorithm works by performing the even and odd parts of our FFT
    // using the "two for the price of one" method mentioned at
    // http://www.engineeringproductivitytools.com/stuff/T0001/PT10.HTM#Head521
    // by making the odd terms into the imaginary components of our new FFT,
    // and then using symmetry to recombine them.
    void fftImplPureReal(Ret, R)(R range, Ret buf) const
    in
    {
        assert(range.length >= 4);
        assert(isPowerOf2(range.length));
    }
    body
    {
        alias E = ElementType!R;

        // Converts odd indices of range to the imaginary components of
        // a range half the size.  The even indices become the real components.
        static if (isArray!R && isFloatingPoint!E)
        {
            // Then the memory layout of complex numbers provides a dirt
            // cheap way to convert.  This is a common case, so take advantage.
            auto oddsImag = cast(Complex!E[]) range;
        }
        else
        {
            // General case:  Use a higher order range.  We can assume
            // source.length is even because it has to be a power of 2.
            static struct OddToImaginary
            {
                R source;
                alias C = Complex!(CommonType!(E, typeof(buf[0].re)));

                @property
                {
                    C front()
                    {
                        return C(source[0], source[1]);
                    }

                    C back()
                    {
                        immutable n = source.length;
                        return C(source[n - 2], source[n - 1]);
                    }

                    typeof(this) save()
                    {
                        return typeof(this)(source.save);
                    }

                    bool empty()
                    {
                        return source.empty;
                    }

                    size_t length()
                    {
                        return source.length / 2;
                    }
                }

                void popFront()
                {
                    source.popFront();
                    source.popFront();
                }

                void popBack()
                {
                    source.popBack();
                    source.popBack();
                }

                C opIndex(size_t index)
                {
                    return C(source[index * 2], source[index * 2 + 1]);
                }

                typeof(this) opSlice(size_t lower, size_t upper)
                {
                    return typeof(this)(source[lower * 2 .. upper * 2]);
                }
            }

            auto oddsImag = OddToImaginary(range);
        }

        fft(oddsImag, buf[0..$ / 2]);
        auto evenFft = buf[0..$ / 2];
        auto oddFft = buf[$ / 2..$];
        immutable halfN = evenFft.length;
        oddFft[0].re = buf[0].im;
        oddFft[0].im = 0;
        evenFft[0].im = 0;
        // evenFft[0].re is already right b/c it's aliased with buf[0].re.

        foreach (k; 1 .. halfN / 2 + 1)
        {
            immutable bufk = buf[k];
            immutable bufnk = buf[buf.length / 2 - k];
            evenFft[k].re = 0.5 * (bufk.re + bufnk.re);
            evenFft[halfN - k].re = evenFft[k].re;
            evenFft[k].im = 0.5 * (bufk.im - bufnk.im);
            evenFft[halfN - k].im = -evenFft[k].im;

            oddFft[k].re = 0.5 * (bufk.im + bufnk.im);
            oddFft[halfN - k].re = oddFft[k].re;
            oddFft[k].im = 0.5 * (bufnk.re - bufk.re);
            oddFft[halfN - k].im = -oddFft[k].im;
        }

        butterfly(buf);
    }

    void butterfly(R)(R buf) const
    in
    {
        assert(isPowerOf2(buf.length));
    }
    body
    {
        immutable n = buf.length;
        immutable localLookup = negSinLookup[bsf(n)];
        assert(localLookup.length == n);

        immutable cosMask = n - 1;
        immutable cosAdd = n / 4 * 3;

        lookup_t negSinFromLookup(size_t index) pure nothrow
        {
            return localLookup[index];
        }

        lookup_t cosFromLookup(size_t index) pure nothrow
        {
            // cos is just -sin shifted by PI * 3 / 2.
            return localLookup[(index + cosAdd) & cosMask];
        }

        immutable halfLen = n / 2;

        // This loop is unrolled and the two iterations are interleaved
        // relative to the textbook FFT to increase ILP.  This gives roughly 5%
        // speedups on DMD.
        for (size_t k = 0; k < halfLen; k += 2)
        {
            immutable cosTwiddle1 = cosFromLookup(k);
            immutable sinTwiddle1 = negSinFromLookup(k);
            immutable cosTwiddle2 = cosFromLookup(k + 1);
            immutable sinTwiddle2 = negSinFromLookup(k + 1);

            immutable realLower1 = buf[k].re;
            immutable imagLower1 = buf[k].im;
            immutable realLower2 = buf[k + 1].re;
            immutable imagLower2 = buf[k + 1].im;

            immutable upperIndex1 = k + halfLen;
            immutable upperIndex2 = upperIndex1 + 1;
            immutable realUpper1 = buf[upperIndex1].re;
            immutable imagUpper1 = buf[upperIndex1].im;
            immutable realUpper2 = buf[upperIndex2].re;
            immutable imagUpper2 = buf[upperIndex2].im;

            immutable realAdd1 = cosTwiddle1 * realUpper1
                               - sinTwiddle1 * imagUpper1;
            immutable imagAdd1 = sinTwiddle1 * realUpper1
                               + cosTwiddle1 * imagUpper1;
            immutable realAdd2 = cosTwiddle2 * realUpper2
                               - sinTwiddle2 * imagUpper2;
            immutable imagAdd2 = sinTwiddle2 * realUpper2
                               + cosTwiddle2 * imagUpper2;

            buf[k].re += realAdd1;
            buf[k].im += imagAdd1;
            buf[k + 1].re += realAdd2;
            buf[k + 1].im += imagAdd2;

            buf[upperIndex1].re = realLower1 - realAdd1;
            buf[upperIndex1].im = imagLower1 - imagAdd1;
            buf[upperIndex2].re = realLower2 - realAdd2;
            buf[upperIndex2].im = imagLower2 - imagAdd2;
        }
    }

    // This constructor is used within this module for allocating the
    // buffer space elsewhere besides the GC heap.  It's definitely **NOT**
    // part of the public API and definitely **IS** subject to change.
    //
    // Also, this is unsafe because the memSpace buffer will be cast
    // to immutable.
    public this(lookup_t[] memSpace)  // Public b/c of bug 4636.
    {
        immutable size = memSpace.length / 2;

        /* Create a lookup table of all negative sine values at a resolution of
         * size and all smaller power of two resolutions.  This may seem
         * inefficient, but having all the lookups be next to each other in
         * memory at every level of iteration is a huge win performance-wise.
         */
        if (size == 0)
        {
            return;
        }

        assert(isPowerOf2(size),
            "Can only do FFTs on ranges with a size that is a power of two.");

        auto table = new lookup_t[][bsf(size) + 1];

        table[$ - 1] = memSpace[$ - size..$];
        memSpace = memSpace[0 .. size];

        auto lastRow = table[$ - 1];
        lastRow[0] = 0;  // -sin(0) == 0.
        foreach (ptrdiff_t i; 1 .. size)
        {
            // The hard coded cases are for improved accuracy and to prevent
            // annoying non-zeroness when stuff should be zero.

            if (i == size / 4)
                lastRow[i] = -1;  // -sin(pi / 2) == -1.
            else if (i == size / 2)
                lastRow[i] = 0;   // -sin(pi) == 0.
            else if (i == size * 3 / 4)
                lastRow[i] = 1;  // -sin(pi * 3 / 2) == 1
            else
                lastRow[i] = -sin(i * 2.0L * PI / size);
        }

        // Fill in all the other rows with strided versions.
        foreach (i; 1 .. table.length - 1)
        {
            immutable strideLength = size / (2 ^^ i);
            auto strided = Stride!(lookup_t[])(lastRow, strideLength);
            table[i] = memSpace[$ - strided.length..$];
            memSpace = memSpace[0..$ - strided.length];

            size_t copyIndex;
            foreach (elem; strided)
            {
                table[i][copyIndex++] = elem;
            }
        }

        negSinLookup = cast(immutable) table;
    }

public:
    /**Create an $(D Fft) object for computing fast Fourier transforms of
     * power of two sizes of $(D size) or smaller.  $(D size) must be a
     * power of two.
     */
    this(size_t size)
    {
        // Allocate all twiddle factor buffers in one contiguous block so that,
        // when one is done being used, the next one is next in cache.
        auto memSpace = uninitializedArray!(lookup_t[])(2 * size);
        this(memSpace);
    }

    @property size_t size() const
    {
        return (negSinLookup is null) ? 0 : negSinLookup[$ - 1].length;
    }

    /**Compute the Fourier transform of range using the $(BIGOH N log N)
     * Cooley-Tukey Algorithm.  $(D range) must be a random-access range with
     * slicing and a length equal to $(D size) as provided at the construction of
     * this object.  The contents of range can be either  numeric types,
     * which will be interpreted as pure real values, or complex types with
     * properties or members $(D .re) and $(D .im) that can be read.
     *
     * Note:  Pure real FFTs are automatically detected and the relevant
     *        optimizations are performed.
     *
     * Returns:  An array of complex numbers representing the transformed data in
     *           the frequency domain.
     *
     * Conventions: The exponent is negative and the factor is one,
     *              i.e., output[j] := sum[ exp(-2 PI i j k / N) input[k] ].
     */
    Complex!F[] fft(F = double, R)(R range) const
        if (isFloatingPoint!F && isRandomAccessRange!R)
    {
        enforceSize(range);
        Complex!F[] ret;
        if (range.length == 0)
        {
            return ret;
        }

        // Don't waste time initializing the memory for ret.
        ret = uninitializedArray!(Complex!F[])(range.length);

        fft(range,  ret);
        return ret;
    }

    /**Same as the overload, but allows for the results to be stored in a user-
     * provided buffer.  The buffer must be of the same length as range, must be
     * a random-access range, must have slicing, and must contain elements that are
     * complex-like.  This means that they must have a .re and a .im member or
     * property that can be both read and written and are floating point numbers.
     */
    void fft(Ret, R)(R range, Ret buf) const
        if (isRandomAccessRange!Ret && isComplexLike!(ElementType!Ret) && hasSlicing!Ret)
    {
        assert(buf.length == range.length);
        enforceSize(range);

        if (range.length == 0)
        {
            return;
        }
        else if (range.length == 1)
        {
            buf[0] = range[0];
            return;
        }
        else if (range.length == 2)
        {
            slowFourier2(range, buf);
            return;
        }
        else
        {
            alias E = ElementType!R;
            static if (is(E : real))
            {
                return fftImplPureReal(range, buf);
            }
            else
            {
                static if (is(R : Stride!R))
                    return fftImpl(range, buf);
                else
                    return fftImpl(Stride!R(range, 1), buf);
            }
        }
    }

    /**
     * Computes the inverse Fourier transform of a range.  The range must be a
     * random access range with slicing, have a length equal to the size
     * provided at construction of this object, and contain elements that are
     * either of type std.complex.Complex or have essentially
     * the same compile-time interface.
     *
     * Returns:  The time-domain signal.
     *
     * Conventions: The exponent is positive and the factor is 1/N, i.e.,
     *              output[j] := (1 / N) sum[ exp(+2 PI i j k / N) input[k] ].
     */
    Complex!F[] inverseFft(F = double, R)(R range) const
        if (isRandomAccessRange!R && isComplexLike!(ElementType!R) && isFloatingPoint!F)
    {
        enforceSize(range);
        Complex!F[] ret;
        if (range.length == 0)
        {
            return ret;
        }

        // Don't waste time initializing the memory for ret.
        ret = uninitializedArray!(Complex!F[])(range.length);

        inverseFft(range, ret);
        return ret;
    }

    /**
     * Inverse FFT that allows a user-supplied buffer to be provided.  The buffer
     * must be a random access range with slicing, and its elements
     * must be some complex-like type.
     */
    void inverseFft(Ret, R)(R range, Ret buf) const
        if (isRandomAccessRange!Ret && isComplexLike!(ElementType!Ret) && hasSlicing!Ret)
    {
        enforceSize(range);

        auto swapped = map!swapRealImag(range);
        fft(swapped,  buf);

        immutable lenNeg1 = 1.0 / buf.length;
        foreach (ref elem; buf)
        {
            immutable temp = elem.re * lenNeg1;
            elem.re = elem.im * lenNeg1;
            elem.im = temp;
        }
    }
}

// This mixin creates an Fft object in the scope it's mixed into such that all
// memory owned by the object is deterministically destroyed at the end of that
// scope.
private enum string MakeLocalFft = q{
    import core.stdc.stdlib;
    import core.exception : onOutOfMemoryError;

    auto lookupBuf = (cast(lookup_t*) malloc(range.length * 2 * lookup_t.sizeof))
                     [0 .. 2 * range.length];
    if (!lookupBuf.ptr)
        onOutOfMemoryError();

    scope(exit) free(cast(void*) lookupBuf.ptr);
    auto fftObj = scoped!Fft(lookupBuf);
};

/**Convenience functions that create an $(D Fft) object, run the FFT or inverse
 * FFT and return the result.  Useful for one-off FFTs.
 *
 * Note:  In addition to convenience, these functions are slightly more
 *        efficient than manually creating an Fft object for a single use,
 *        as the Fft object is deterministically destroyed before these
 *        functions return.
 */
Complex!F[] fft(F = double, R)(R range)
{
    mixin(MakeLocalFft);
    return fftObj.fft!(F, R)(range);
}

/// ditto
void fft(Ret, R)(R range, Ret buf)
{
    mixin(MakeLocalFft);
    return fftObj.fft!(Ret, R)(range, buf);
}

/// ditto
Complex!F[] inverseFft(F = double, R)(R range)
{
    mixin(MakeLocalFft);
    return fftObj.inverseFft!(F, R)(range);
}

/// ditto
void inverseFft(Ret, R)(R range, Ret buf)
{
    mixin(MakeLocalFft);
    return fftObj.inverseFft!(Ret, R)(range, buf);
}

@system unittest
{
    import std.algorithm;
    import std.conv;
    import std.range;
    // Test values from R and Octave.
    auto arr = [1,2,3,4,5,6,7,8];
    auto fft1 = fft(arr);
    assert(approxEqual(map!"a.re"(fft1),
        [36.0, -4, -4, -4, -4, -4, -4, -4]));
    assert(approxEqual(map!"a.im"(fft1),
        [0, 9.6568, 4, 1.6568, 0, -1.6568, -4, -9.6568]));

    auto fft1Retro = fft(retro(arr));
    assert(approxEqual(map!"a.re"(fft1Retro),
        [36.0, 4, 4, 4, 4, 4, 4, 4]));
    assert(approxEqual(map!"a.im"(fft1Retro),
        [0, -9.6568, -4, -1.6568, 0, 1.6568, 4, 9.6568]));

    auto fft1Float = fft(to!(float[])(arr));
    assert(approxEqual(map!"a.re"(fft1), map!"a.re"(fft1Float)));
    assert(approxEqual(map!"a.im"(fft1), map!"a.im"(fft1Float)));

    alias C = Complex!float;
    auto arr2 = [C(1,2), C(3,4), C(5,6), C(7,8), C(9,10),
        C(11,12), C(13,14), C(15,16)];
    auto fft2 = fft(arr2);
    assert(approxEqual(map!"a.re"(fft2),
        [64.0, -27.3137, -16, -11.3137, -8, -4.6862, 0, 11.3137]));
    assert(approxEqual(map!"a.im"(fft2),
        [72, 11.3137, 0, -4.686, -8, -11.3137, -16, -27.3137]));

    auto inv1 = inverseFft(fft1);
    assert(approxEqual(map!"a.re"(inv1), arr));
    assert(reduce!max(map!"a.im"(inv1)) < 1e-10);

    auto inv2 = inverseFft(fft2);
    assert(approxEqual(map!"a.re"(inv2), map!"a.re"(arr2)));
    assert(approxEqual(map!"a.im"(inv2), map!"a.im"(arr2)));

    // FFTs of size 0, 1 and 2 are handled as special cases.  Test them here.
    ushort[] empty;
    assert(fft(empty) == null);
    assert(inverseFft(fft(empty)) == null);

    real[] oneElem = [4.5L];
    auto oneFft = fft(oneElem);
    assert(oneFft.length == 1);
    assert(oneFft[0].re == 4.5L);
    assert(oneFft[0].im == 0);

    auto oneInv = inverseFft(oneFft);
    assert(oneInv.length == 1);
    assert(approxEqual(oneInv[0].re, 4.5));
    assert(approxEqual(oneInv[0].im, 0));

    long[2] twoElems = [8, 4];
    auto twoFft = fft(twoElems[]);
    assert(twoFft.length == 2);
    assert(approxEqual(twoFft[0].re, 12));
    assert(approxEqual(twoFft[0].im, 0));
    assert(approxEqual(twoFft[1].re, 4));
    assert(approxEqual(twoFft[1].im, 0));
    auto twoInv = inverseFft(twoFft);
    assert(approxEqual(twoInv[0].re, 8));
    assert(approxEqual(twoInv[0].im, 0));
    assert(approxEqual(twoInv[1].re, 4));
    assert(approxEqual(twoInv[1].im, 0));
}

// Swaps the real and imaginary parts of a complex number.  This is useful
// for inverse FFTs.
C swapRealImag(C)(C input)
{
    return C(input.im, input.re);
}

private:
// The reasons I couldn't use std.algorithm were b/c its stride length isn't
// modifiable on the fly and because range has grown some performance hacks
// for powers of 2.
struct Stride(R)
{
    import core.bitop : bsf;
    Unqual!R range;
    size_t _nSteps;
    size_t _length;
    alias E = ElementType!(R);

    this(R range, size_t nStepsIn)
    {
        this.range = range;
       _nSteps = nStepsIn;
       _length = (range.length + _nSteps - 1) / nSteps;
    }

    size_t length() const @property
    {
        return _length;
    }

    typeof(this) save() @property
    {
        auto ret = this;
        ret.range = ret.range.save;
        return ret;
    }

    E opIndex(size_t index)
    {
        return range[index * _nSteps];
    }

    E front() @property
    {
        return range[0];
    }

    void popFront()
    {
        if (range.length >= _nSteps)
        {
            range = range[_nSteps .. range.length];
            _length--;
        }
        else
        {
            range = range[0 .. 0];
            _length = 0;
        }
    }

    // Pops half the range's stride.
    void popHalf()
    {
        range = range[_nSteps / 2 .. range.length];
    }

    bool empty() const @property
    {
        return length == 0;
    }

    size_t nSteps() const @property
    {
        return _nSteps;
    }

    void doubleSteps()
    {
        _nSteps *= 2;
        _length /= 2;
    }

    size_t nSteps(size_t newVal) @property
    {
        _nSteps = newVal;

        // Using >> bsf(nSteps) is a few cycles faster than / nSteps.
        _length = (range.length + _nSteps - 1)  >> bsf(nSteps);
        return newVal;
    }
}

// Hard-coded base case for FFT of size 2.  This is actually a TON faster than
// using a generic slow DFT.  This seems to be the best base case.  (Size 1
// can be coded inline as buf[0] = range[0]).
void slowFourier2(Ret, R)(R range, Ret buf)
{
    assert(range.length == 2);
    assert(buf.length == 2);
    buf[0] = range[0] + range[1];
    buf[1] = range[0] - range[1];
}

// Hard-coded base case for FFT of size 4.  Doesn't work as well as the size
// 2 case.
void slowFourier4(Ret, R)(R range, Ret buf)
{
    alias C = ElementType!Ret;

    assert(range.length == 4);
    assert(buf.length == 4);
    buf[0] = range[0] + range[1] + range[2] + range[3];
    buf[1] = range[0] - range[1] * C(0, 1) - range[2] + range[3] * C(0, 1);
    buf[2] = range[0] - range[1] + range[2] - range[3];
    buf[3] = range[0] + range[1] * C(0, 1) - range[2] - range[3] * C(0, 1);
}

N roundDownToPowerOf2(N)(N num)
if (isScalarType!N && !isFloatingPoint!N)
{
    import core.bitop : bsr;
    return num & (cast(N) 1 << bsr(num));
}

@safe unittest
{
    assert(roundDownToPowerOf2(7) == 4);
    assert(roundDownToPowerOf2(4) == 4);
}

template isComplexLike(T)
{
    enum bool isComplexLike = is(typeof(T.init.re)) &&
        is(typeof(T.init.im));
}

@safe unittest
{
    static assert(isComplexLike!(Complex!double));
    static assert(!isComplexLike!(uint));
}
