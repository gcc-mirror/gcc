/**
 * Written in the D programming language.
 * This module provides functions to converting different values to const(ubyte)[]
 *
 * Copyright: Copyright Igor Stepanov 2013-2013.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Igor Stepanov
 * Source: $(DRUNTIMESRC core/internal/_convert.d)
 */
module core.internal.convert;
import core.internal.traits : Unqual;

/+
A @nogc function can allocate memory during CTFE.
+/
@nogc nothrow pure @trusted
private ubyte[] ctfe_alloc()(size_t n)
{
    if (!__ctfe)
    {
        assert(0, "CTFE only");
    }
    else
    {
        static ubyte[] alloc(size_t x) nothrow pure
        {
            if (__ctfe) // Needed to prevent _d_newarray from appearing in compiled prorgam.
                return new ubyte[x];
            else
                assert(0);
        }
        return (cast(ubyte[] function(size_t) @nogc nothrow pure) &alloc)(n);
    }
}

@trusted pure nothrow @nogc
const(ubyte)[] toUbyte(T)(const ref T val) if (is(Unqual!T == float) || is(Unqual!T == double) || is(Unqual!T == real) ||
                                        is(Unqual!T == ifloat) || is(Unqual!T == idouble) || is(Unqual!T == ireal))
{
    if (__ctfe)
    {
        static if (floatFormat!T == FloatFormat.Float || floatFormat!T == FloatFormat.Double)
        {
            static if (is(T : ireal)) // https://issues.dlang.org/show_bug.cgi?id=19932
                const f = val.im;
            else
                alias f = val;
            static if (T.sizeof == uint.sizeof)
                uint bits = *cast(const uint*) &f;
            else static if (T.sizeof == ulong.sizeof)
                ulong bits = *cast(const ulong*) &f;
            ubyte[] result = ctfe_alloc(T.sizeof);
            version (BigEndian)
            {
                foreach_reverse (ref b; result)
                {
                    b = cast(ubyte) bits;
                    bits >>= 8;
                }
            }
            else
            {
                foreach (ref b; result)
                {
                    b = cast(ubyte) bits;
                    bits >>= 8;
                }
            }
            return result;
        }
        else static if (floatFormat!T == FloatFormat.DoubleDouble)
        {
            // Parse DoubleDoubles as a pair of doubles.
            // The layout of the type is:
            //
            //   [1|    11    |       52      ][1|    11    |       52       ]
            //   [S| Exponent | Fraction (hi) ][S| Exponent | Fraction (low) ]
            //
            // We can get the least significant bits by subtracting the IEEE
            // double precision portion from the real value.

            import core.math : toPrec;

            ubyte[] buff = ctfe_alloc(T.sizeof);
            enum msbSize = double.sizeof;

            static if (is(Unqual!T == ireal))
                double hi = toPrec!double(val.im);
            else
                double hi = toPrec!double(val);
            buff[0 .. msbSize] = toUbyte(hi)[];

            if (val is cast(T)0.0 || val is cast(T)-0.0 ||
                val is T.nan || val is -T.nan ||
                val is T.infinity || val > T.max ||
                val is -T.infinity || val < -T.max)
            {
                // Zero, NaN, and Inf are all representable as doubles, so the
                // least significant part can be 0.0.
                buff[msbSize .. $] = 0;
            }
            else
            {
                static if (is(Unqual!T == ireal))
                    double low = toPrec!double(val.im - hi);
                else
                    double low = toPrec!double(val - hi);
                buff[msbSize .. $] = toUbyte(low)[];
            }

            // Arrays don't index differently between little and big-endian targets.
            return buff;
        }
        else
        {
            auto parsed = parse(val);

            ulong mantissa = parsed.mantissa;
            uint exp = parsed.exponent;
            uint sign = parsed.sign;

            ubyte[] buff = ctfe_alloc(T.sizeof);
            size_t off_bytes = 0;
            size_t off_bits  = 0;
            // Quadruples won't fit in one ulong, so check for that.
            enum mantissaMax = FloatTraits!T.MANTISSA < ulong.sizeof*8 ?
                               FloatTraits!T.MANTISSA : ulong.sizeof*8;

            for (; off_bytes < mantissaMax/8; ++off_bytes)
            {
                buff[off_bytes] = cast(ubyte)mantissa;
                mantissa >>= 8;
            }

            static if (floatFormat!T == FloatFormat.Quadruple)
            {
                ulong mantissa2 = parsed.mantissa2;
                off_bytes--; // go back one, since mantissa only stored data in 56
                             // bits, ie 7 bytes
                for (; off_bytes < FloatTraits!T.MANTISSA/8; ++off_bytes)
                {
                    buff[off_bytes] = cast(ubyte)mantissa2;
                    mantissa2 >>= 8;
                }
            }
            else
            {
                off_bits = FloatTraits!T.MANTISSA%8;
                buff[off_bytes] = cast(ubyte)mantissa;
            }

            for (size_t i=0; i<FloatTraits!T.EXPONENT/8; ++i)
            {
                ubyte cur_exp = cast(ubyte)exp;
                exp >>= 8;
                buff[off_bytes] |= (cur_exp << off_bits);
                ++off_bytes;
                buff[off_bytes] |= cur_exp >> 8 - off_bits;
            }


            exp <<= 8 - FloatTraits!T.EXPONENT%8 - 1;
            buff[off_bytes] |= exp;
            sign <<= 7;
            buff[off_bytes] |= sign;

            version (BigEndian)
            {
                for (size_t left = 0, right = buff.length - 1; left < right; left++, right--)
                {
                    const swap = buff[left];
                    buff[left] = buff[right];
                    buff[right] = swap;
                }
            }
            return buff;
        }
    }
    else
    {
        return (cast(const(ubyte)*)&val)[0 .. T.sizeof];
    }
}

@safe pure nothrow @nogc
private Float parse(bool is_denormalized = false, T)(T x) if (is(Unqual!T == ifloat) || is(Unqual!T == idouble) || is(Unqual!T == ireal))
{
    return parse(x.im);
}

@safe pure nothrow @nogc
private Float parse(bool is_denormalized = false, T:real)(T x_) if (floatFormat!T != FloatFormat.Real80)
{
    Unqual!T x = x_;
    static assert(floatFormat!T != FloatFormat.DoubleDouble,
           "doubledouble float format not supported in CTFE");
    if (x is cast(T)0.0) return FloatTraits!T.ZERO;
    if (x is cast(T)-0.0) return FloatTraits!T.NZERO;
    if (x is T.nan) return FloatTraits!T.NAN;
    if (x is -T.nan) return FloatTraits!T.NNAN;
    if (x is T.infinity || x > T.max) return FloatTraits!T.INF;
    if (x is -T.infinity || x < -T.max) return FloatTraits!T.NINF;

    uint sign = x < 0;
    x = sign ? -x : x;
    int e = binLog2(x);
    real x2 = x;
    uint exp = cast(uint)(e + (2^^(FloatTraits!T.EXPONENT-1) - 1));

    if (!exp)
    {
        if (is_denormalized)
            return Float(0, 0, sign);
        else
            return denormalizedMantissa(x, sign);
    }

    x2 /= binPow2(e);

    static if (!is_denormalized)
        x2 -= 1.0;

    static if (floatFormat!T == FloatFormat.Quadruple)
    {
        // Store the 112-bit mantissa in two ulongs, specifically the lower 56
        // bits of each, with the most significant bits in mantissa2. There's
        // an edge case exposed by the labeled test below, where only a subnormal
        // with the highest bit set being the 57th bit will "overflow" to the
        // 57th bit in mantissa2 with the following logic, but that special case
        // is handled by an additional check in denormalizedMantissa for
        // Quadruples below.

        x2 *=  2UL<<(FloatTraits!T.MANTISSA - (ulong.sizeof - 1)*8 - 1);
        ulong mant2 = cast(ulong) x2;
        x2 -= mant2;

        x2 *=  2UL<<((ulong.sizeof - 1)*8 - 1);
        ulong mant = cast(ulong) x2;
        return Float(mant, exp, sign, mant2);
    }
    else
    {
        x2 *=  2UL<<(FloatTraits!T.MANTISSA);
        ulong mant = shiftrRound(cast(ulong)x2);
        return Float(mant, exp, sign);
    }
}

@safe pure nothrow @nogc
private Float parse(bool _ = false, T:real)(T x_) if (floatFormat!T == FloatFormat.Real80)
{
    Unqual!T x = x_;
    //HACK @@@3632@@@

    if (x == 0.0L)
    {
        real y = 1.0L/x;
        if (y == real.infinity) // -0.0
            return FloatTraits!T.ZERO;
        else
            return FloatTraits!T.NZERO; //0.0
    }

    if (x != x) //HACK: should be if (x is real.nan) and if (x is -real.nan)
    {
        auto y = cast(double)x;
        if (y is double.nan)
            return FloatTraits!T.NAN;
        else
            return FloatTraits!T.NNAN;
    }

    if (x == real.infinity) return FloatTraits!T.INF;
    if (x == -real.infinity) return FloatTraits!T.NINF;

    enum EXPONENT_MED = (2^^(FloatTraits!T.EXPONENT-1) - 1);
    uint sign = x < 0;
    x = sign ? -x : x;

    int e = binLog2(x);
    uint exp = cast(uint)(e + EXPONENT_MED);
    if (!exp)
    {
        return denormalizedMantissa(x, sign);
    }
    int pow = (FloatTraits!T.MANTISSA-1-e);
    x *=  binPow2((pow / EXPONENT_MED)*EXPONENT_MED); //To avoid overflow in 2.0L ^^ pow
    x *=  binPow2(pow % EXPONENT_MED);
    ulong mant = cast(ulong)x;
    return Float(mant, exp, sign);
}

private struct Float
{
    ulong mantissa;
    uint exponent;
    uint sign;
    ulong mantissa2;
}

private template FloatTraits(T) if (floatFormat!T == FloatFormat.Float)
{
    enum DATASIZE = 4;
    enum EXPONENT = 8;
    enum MANTISSA = 23;
    enum ZERO     = Float(0, 0, 0);
    enum NZERO    = Float(0, 0, 1);
    enum NAN      = Float(0x400000UL, 0xff, 0);
    enum NNAN     = Float(0x400000UL, 0xff, 1);
    enum INF      = Float(0, 255, 0);
    enum NINF     = Float(0, 255, 1);
}

private template FloatTraits(T) if (floatFormat!T == FloatFormat.Double)
{
    enum DATASIZE = 8;
    enum EXPONENT = 11;
    enum MANTISSA = 52;
    enum ZERO     = Float(0, 0, 0);
    enum NZERO    = Float(0, 0, 1);
    enum NAN      = Float(0x8000000000000UL, 0x7ff, 0);
    enum NNAN     = Float(0x8000000000000UL, 0x7ff, 1);
    enum INF      = Float(0, 0x7ff, 0);
    enum NINF     = Float(0, 0x7ff, 1);
}

private template FloatTraits(T) if (floatFormat!T == FloatFormat.Real80)
{
    enum DATASIZE = 10;
    enum EXPONENT = 15;
    enum MANTISSA = 64;
    enum ZERO     = Float(0, 0, 0);
    enum NZERO    = Float(0, 0, 1);
    enum NAN      = Float(0xC000000000000000UL, 0x7fff, 0);
    enum NNAN     = Float(0xC000000000000000UL, 0x7fff, 1);
    enum INF      = Float(0x8000000000000000UL, 0x7fff, 0);
    enum NINF     = Float(0x8000000000000000UL, 0x7fff, 1);
}

private template FloatTraits(T) if (floatFormat!T == FloatFormat.DoubleDouble) //Unsupported in CTFE
{
    enum DATASIZE = 16;
    enum EXPONENT = 11;
    enum MANTISSA = 106;
    enum ZERO     = Float(0, 0, 0);
    enum NZERO    = Float(0, 0, 1);
    enum NAN      = Float(0x8000000000000UL, 0x7ff, 0);
    enum NNAN     = Float(0x8000000000000UL, 0x7ff, 1);
    enum INF      = Float(0, 0x7ff, 0);
    enum NINF     = Float(0, 0x7ff, 1);
}

private template FloatTraits(T) if (floatFormat!T == FloatFormat.Quadruple)
{
    enum DATASIZE = 16;
    enum EXPONENT = 15;
    enum MANTISSA = 112;
    enum ZERO     = Float(0, 0, 0);
    enum NZERO    = Float(0, 0, 1);
    enum NAN      = Float(0, 0x7fff, 0, 0x80000000000000UL);
    enum NNAN     = Float(0, 0x7fff, 1, 0x80000000000000UL);
    enum INF      = Float(0, 0x7fff, 0);
    enum NINF     = Float(0, 0x7fff, 1);
}


@safe pure nothrow @nogc
private real binPow2(int pow)
{
    static real binPosPow2(int pow) @safe pure nothrow @nogc
    {
        assert(pow > 0);

        if (pow == 1) return 2.0L;

        int subpow = pow/2;
        real p = binPosPow2(subpow);
        real ret = p*p;

        if (pow%2)
        {
            ret *= 2.0L;
        }

        return ret;
    }

    if (!pow) return 1.0L;
    if (pow > 0) return binPosPow2(pow);
    return 1.0L/binPosPow2(-pow);
}


//Need in CTFE, because CTFE float and double expressions computed more precisely that run-time expressions.
@safe pure nothrow @nogc
private ulong shiftrRound(ulong x)
{
    return (x >> 1) + (x & 1);
}

@safe pure nothrow @nogc
private uint binLog2(T)(const T x)
{
    assert(x > 0);
    int max = 2 ^^ (FloatTraits!T.EXPONENT-1)-1;
    int min = -max+1;
    int med = (min + max) / 2;

    if (x < T.min_normal) return -max;

    while ((max - min) > 1)
    {
        if (binPow2(med) > x)
        {
            max = med;
        }
        else
        {
            min = med;
        }
        med = (min + max) / 2;
    }

    if (x < binPow2(max))
        return min;
    return max;
}

@safe pure nothrow @nogc
private Float denormalizedMantissa(T)(T x, uint sign) if (floatFormat!T == FloatFormat.Real80)
{
    x *= 2.0L^^FloatTraits!T.MANTISSA;
    auto fl = parse(x);
    uint pow = FloatTraits!T.MANTISSA - fl.exponent + 1;
    return Float(fl.mantissa >> pow, 0, sign);
}

@safe pure nothrow @nogc
private Float denormalizedMantissa(T)(T x, uint sign)
    if (floatFormat!T == FloatFormat.Float || floatFormat!T == FloatFormat.Double)
{
    x *= 2.0L^^FloatTraits!T.MANTISSA;
    auto fl = parse!true(x);
    ulong mant = fl.mantissa >> (FloatTraits!T.MANTISSA - fl.exponent);
    return Float(shiftrRound(mant), 0, sign);
}

@safe pure nothrow @nogc
private Float denormalizedMantissa(T)(T x, uint sign) if (floatFormat!T == FloatFormat.Quadruple)
{
    x *= 2.0L^^FloatTraits!T.MANTISSA;
    auto fl = parse!true(x);
    uint offset = FloatTraits!T.MANTISSA - fl.exponent + 1;
    enum mantissaSize = (ulong.sizeof - 1) * 8;

    if (offset < mantissaSize)
    {   // Create a new mantissa ulong with the trailing mantissa2 bits that
        // need to be shifted into mantissa, by shifting the needed bits left,
        // zeroing out the first byte, and then ORing it with mantissa shifted
        // right by offset.

        ulong shiftedMantissa = ((fl.mantissa2 << (mantissaSize - offset)) &
                                 0x00FFFFFFFFFFFFFFUL) | fl.mantissa >> offset;
        return Float(shiftedMantissa, 0, sign, fl.mantissa2 >> offset);
    }
    else if (offset > mantissaSize)
        return Float(fl.mantissa2 >> offset - mantissaSize , 0, sign, 0);
    else
        // Handle special case mentioned in parse() above by zeroing out the
        // 57'th bit of mantissa2, "shifting" it into mantissa, and setting the
        // first bit of mantissa2.
        return Float(fl.mantissa2 & 0x00FFFFFFFFFFFFFFUL , 0, sign, 1);
}

version (unittest)
{
    private const(ubyte)[] toUbyte2(T)(T val)
    {
        return toUbyte(val).dup;
    }

    private void testNumberConvert(string v)()
    {
        enum ctval = mixin(v);

        alias TYPE = typeof(ctval);
        auto rtval = ctval;
        auto rtbytes = *cast(ubyte[TYPE.sizeof]*)&rtval;

        enum ctbytes = toUbyte2(ctval);

        // don't test pad bytes because can be anything
        enum testsize =
            (FloatTraits!TYPE.EXPONENT + FloatTraits!TYPE.MANTISSA + 1)/8;
        assert(rtbytes[0..testsize] == ctbytes[0..testsize]);
    }

    private void testConvert()
    {
        /**Test special values*/
        testNumberConvert!("-float.infinity");
        testNumberConvert!("float.infinity");
        testNumberConvert!("-0.0F");
        testNumberConvert!("0.0F");
        //testNumberConvert!("-float.nan"); //BUG @@@3632@@@
        testNumberConvert!("float.nan");

        testNumberConvert!("-double.infinity");
        testNumberConvert!("double.infinity");
        testNumberConvert!("-0.0");
        testNumberConvert!("0.0");
        //testNumberConvert!("-double.nan"); //BUG @@@3632@@@
        testNumberConvert!("double.nan");

        testNumberConvert!("-real.infinity");
        testNumberConvert!("real.infinity");
        testNumberConvert!("-0.0L");
        testNumberConvert!("0.0L");
        //testNumberConvert!("-real.nan"); //BUG @@@3632@@@
        testNumberConvert!("real.nan");

        /**
            Test min and max values values: min value has an '1' mantissa and minimal exponent,
            Max value has an all '1' bits mantissa and max exponent.
        */
        testNumberConvert!("float.min_normal");
        testNumberConvert!("float.max");

        /**Test common values*/
        testNumberConvert!("-0.17F");
        testNumberConvert!("3.14F");

        /**Test immutable and const*/
        testNumberConvert!("cast(const)3.14F");
        testNumberConvert!("cast(immutable)3.14F");

        /**The same tests for double and real*/
        testNumberConvert!("double.min_normal");
        testNumberConvert!("double.max");
        testNumberConvert!("-0.17");
        testNumberConvert!("3.14");
        testNumberConvert!("cast(const)3.14");
        testNumberConvert!("cast(immutable)3.14");

        testNumberConvert!("real.min_normal");
        testNumberConvert!("real.max");
        testNumberConvert!("-0.17L");
        testNumberConvert!("3.14L");
        testNumberConvert!("cast(const)3.14L");
        testNumberConvert!("cast(immutable)3.14L");

        /**Test denormalized values*/

        /**Max denormalized value, first bit is 1*/
        testNumberConvert!("float.min_normal/2");
        /**Min denormalized value, last bit is 1*/
        testNumberConvert!("float.min_normal/2UL^^23");

        /**Denormalized values with round*/
        testNumberConvert!("float.min_normal/19");
        testNumberConvert!("float.min_normal/17");

        testNumberConvert!("double.min_normal/2");
        testNumberConvert!("double.min_normal/2UL^^52");
        testNumberConvert!("double.min_normal/19");
        testNumberConvert!("double.min_normal/17");

        testNumberConvert!("real.min_normal/2");
        testNumberConvert!("real.min_normal/2UL^^63");
        // check subnormal storage edge case for Quadruple
        testNumberConvert!("real.min_normal/2UL^^56");
        testNumberConvert!("real.min_normal/19");
        testNumberConvert!("real.min_normal/17");

        /**Test imaginary values: convert algorithm is same with real values*/
        testNumberConvert!("0.0Fi");
        testNumberConvert!("0.0i");
        testNumberConvert!("0.0Li");

        /**True random values*/
        testNumberConvert!("-0x9.0f7ee55df77618fp-13829L");
        testNumberConvert!("0x7.36e6e2640120d28p+8797L");
        testNumberConvert!("-0x1.05df6ce4702ccf8p+15835L");
        testNumberConvert!("0x9.54bb0d88806f714p-7088L");

        testNumberConvert!("-0x9.0f7ee55df7ffp-338");
        testNumberConvert!("0x7.36e6e264012dp+879");
        testNumberConvert!("-0x1.05df6ce4708ep+658");
        testNumberConvert!("0x9.54bb0d888061p-708");

        testNumberConvert!("-0x9.0f7eefp-101F");
        testNumberConvert!("0x7.36e6ep+87F");
        testNumberConvert!("-0x1.05df6p+112F");
        testNumberConvert!("0x9.54bb0p-70F");

        /**Big overflow or underflow*/
        testNumberConvert!("cast(double)-0x9.0f7ee55df77618fp-13829L");
        testNumberConvert!("cast(double)0x7.36e6e2640120d28p+8797L");
        testNumberConvert!("cast(double)-0x1.05df6ce4702ccf8p+15835L");
        testNumberConvert!("cast(double)0x9.54bb0d88806f714p-7088L");

        testNumberConvert!("cast(float)-0x9.0f7ee55df77618fp-13829L");
        testNumberConvert!("cast(float)0x7.36e6e2640120d28p+8797L");
        testNumberConvert!("cast(float)-0x1.05df6ce4702ccf8p+15835L");
        testNumberConvert!("cast(float)0x9.54bb0d88806f714p-7088L");
    }


    unittest
    {
        testConvert();
    }
}



private enum FloatFormat
{
    Float,
    Double,
    Real80,
    DoubleDouble,
    Quadruple
}

template floatFormat(T) if (is(T:real) || is(T:ireal))
{
    static if (T.mant_dig == 24)
        enum floatFormat = FloatFormat.Float;
    else static if (T.mant_dig == 53)
    {
        // Double precision, or real == double
        static if (T.sizeof == double.sizeof)
            enum floatFormat = FloatFormat.Double;
        // 80-bit real with rounding precision set to 53 bits.
        else static if (T.sizeof == real.sizeof)
            enum floatFormat = FloatFormat.Real80;
    }
    else static if (T.mant_dig == 64)
        enum floatFormat = FloatFormat.Real80;
    else static if (T.mant_dig == 106)
        enum floatFormat = FloatFormat.DoubleDouble;
    else static if (T.mant_dig == 113)
        enum floatFormat = FloatFormat.Quadruple;
    else
        static assert(0);

}

package template floatSize(T) if (is(T:real) || is(T:ireal))
{
    enum floatSize = FloatTraits!(T).DATASIZE;
}

//  all toUbyte functions must be evaluable at compile time
@trusted pure nothrow @nogc
const(ubyte)[] toUbyte(T)(const T[] arr) if (T.sizeof == 1)
{
    return cast(const(ubyte)[])arr;
}

@trusted pure nothrow @nogc
const(ubyte)[] toUbyte(T)(const T[] arr) if (T.sizeof > 1)
{
    if (__ctfe)
    {
        ubyte[] ret = ctfe_alloc(T.sizeof * arr.length);
        static if (is(T EType == enum)) // Odd style is to avoid template instantiation in most cases.
            alias E = OriginalType!EType;
        else
            alias E = T;
        static if (is(E == struct) || is(E == union) || __traits(isStaticArray, E) || !is(typeof(arr[0] is null)))
        {
            size_t offset = 0;
            foreach (ref cur; arr)
            {
                ret[offset .. offset + T.sizeof] = toUbyte(cur)[0 .. T.sizeof];
                offset += T.sizeof;
            }
        }
        else
        {
            foreach (cur; arr)
                assert(cur is null, "Unable to compute byte representation of non-null pointer at compile time");
        }
        return ret;
    }
    else
    {
        return (cast(const(ubyte)*)(arr.ptr))[0 .. T.sizeof*arr.length];
    }
}

@trusted pure nothrow @nogc
const(ubyte)[] toUbyte(T)(const ref T val) if (__traits(isIntegral, T) && !is(T == enum) && !is(T == __vector))
{
    static if (T.sizeof == 1)
    {
        if (__ctfe)
        {
            ubyte[] result = ctfe_alloc(1);
            result[0] = cast(ubyte) val;
            return result;
        }
        else
        {
            return (cast(const(ubyte)*)(&val))[0 .. T.sizeof];
        }
    }
    else if (__ctfe)
    {
        ubyte[] tmp = ctfe_alloc(T.sizeof);
        Unqual!T val_ = val;
        for (size_t i = 0; i < T.sizeof; ++i)
        {
            size_t idx;
            version (LittleEndian) idx = i;
            else idx = T.sizeof-i-1;
            tmp[idx] = cast(ubyte)(val_&0xff);
            val_ >>= 8;
        }
        return tmp;
    }
    else
    {
        return (cast(const(ubyte)*)(&val))[0 .. T.sizeof];
    }
}

@trusted pure nothrow @nogc
const(ubyte)[] toUbyte(T)(const ref T val) if (is(T == __vector))
{
    if (!__ctfe)
        return (cast(const ubyte*) &val)[0 .. T.sizeof];
    else static if (is(typeof(val[0]) : void))
        assert(0, "Unable to compute byte representation of " ~ T.stringof ~ " at compile time.");
    else
    {
        // This code looks like it should work in CTFE but it segfaults:
        //    auto a = val.array;
        //    return toUbyte(a);
        alias E = typeof(val[0]);
        ubyte[] result = ctfe_alloc(T.sizeof);
        for (size_t i = 0, j = 0; i < T.sizeof; i += E.sizeof, ++j)
        {
            result[i .. i + E.sizeof] = toUbyte(val[j]);
        }
        return result;
    }
}

@trusted pure nothrow @nogc
const(ubyte)[] toUbyte(T)(const ref T val) if (is(Unqual!T == cfloat) || is(Unqual!T == cdouble) ||is(Unqual!T == creal))
{
    if (__ctfe)
    {
        auto re = val.re;
        auto im = val.im;
        auto a = re.toUbyte();
        auto b = im.toUbyte();
        ubyte[] result = ctfe_alloc(a.length + b.length);
        result[0 .. a.length] = a[0 .. a.length];
        result[a.length .. $] = b[0 .. b.length];
        return result;
    }
    else
    {
        return (cast(const(ubyte)*)&val)[0 .. T.sizeof];
    }
}

@trusted pure nothrow @nogc
const(ubyte)[] toUbyte(T)(const ref T val) if (is(T == enum))
{
    if (__ctfe)
    {
        static if (is(T V == enum)){}
        return toUbyte(cast(const V) val);
    }
    else
    {
        return (cast(const(ubyte)*)&val)[0 .. T.sizeof];
    }
}

nothrow pure @safe unittest
{
    // Issue 19008 - check toUbyte works on enums.
    enum Month : uint { jan = 1}
    Month m = Month.jan;
    const bytes = toUbyte(m);
    enum ctfe_works = (() => { Month x = Month.jan; return toUbyte(x).length > 0; })();
}

@trusted pure nothrow @nogc
const(ubyte)[] toUbyte(T)(const ref T val) if (is(T == delegate) || is(T : V*, V) && __traits(getAliasThis, T).length == 0)
{
    if (__ctfe)
    {
        if (val !is null) assert(0, "Unable to compute byte representation of non-null pointer at compile time");
        return ctfe_alloc(T.sizeof);
    }
    else
    {
        return (cast(const(ubyte)*)&val)[0 .. T.sizeof];
    }
}

@trusted pure nothrow @nogc
const(ubyte)[] toUbyte(T)(const ref T val) if (is(T == struct) || is(T == union))
{
    if (__ctfe)
    {
        ubyte[] bytes = ctfe_alloc(T.sizeof);
        foreach (key, ref cur; val.tupleof)
        {
            static if (is(typeof(cur) EType == enum)) // Odd style is to avoid template instantiation in most cases.
                alias CurType = OriginalType!EType;
            else
                alias CurType = typeof(cur);
            static if (is(CurType == struct) || is(CurType == union) || __traits(isStaticArray, CurType) || !is(typeof(cur is null)))
            {
                bytes[val.tupleof[key].offsetof .. val.tupleof[key].offsetof + CurType.sizeof] = toUbyte(cur)[];
            }
            else
            {
                assert(cur is null, "Unable to compute byte representation of non-null reference field at compile time");
                //skip, because val bytes are zeros
            }
        }
        return bytes;
    }
    else
    {
        return (cast(const(ubyte)*)&val)[0 .. T.sizeof];
    }
}

// Strips off all `enum`s from type `T`.
// Perhaps move to core.internal.types.
private template OriginalType(T)
{
    static if (is(T EType == enum))
        alias OriginalType = .OriginalType!EType;
    else
        alias OriginalType = T;
}
