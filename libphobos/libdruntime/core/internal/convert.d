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

@trusted pure nothrow
const(ubyte)[] toUbyte(T)(const ref T val) if (is(Unqual!T == float) || is(Unqual!T == double) || is(Unqual!T == real) ||
                                        is(Unqual!T == ifloat) || is(Unqual!T == idouble) || is(Unqual!T == ireal))
{
    static const(ubyte)[] reverse_(const(ubyte)[] arr)
    {
        ubyte[] buff = ctfe_alloc(arr.length);
        foreach (k, v; arr)
        {
            buff[$-k-1] = v;
        }
        return buff;
    }
    if (__ctfe)
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
            for(; off_bytes < FloatTraits!T.MANTISSA/8; ++off_bytes)
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

        version (LittleEndian)
        {
            return buff;
        }
        else
        {
            return reverse_(buff);
        }
    }
    else
    {
        return (cast(const(ubyte)*)&val)[0 .. T.sizeof];
    }
}

@safe pure nothrow
private Float parse(bool is_denormalized = false, T)(T x) if (is(Unqual!T == ifloat) || is(Unqual!T == idouble) || is(Unqual!T == ireal))
{
    return parse(x.im);
}

@safe pure nothrow
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

@safe pure nothrow
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
    enum EXPONENT = 15;
    enum MANTISSA = 112;
    enum ZERO     = Float(0, 0, 0);
    enum NZERO    = Float(0, 0, 1);
    enum NAN      = Float(0, 0x7fff, 0, 0x80000000000000UL);
    enum NNAN     = Float(0, 0x7fff, 1, 0x80000000000000UL);
    enum INF      = Float(0, 0x7fff, 0);
    enum NINF     = Float(0, 0x7fff, 1);
}


@safe pure nothrow
private real binPow2(int pow)
{
    static real binPosPow2(int pow) @safe pure nothrow
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
@safe pure nothrow
private ulong shiftrRound(ulong x)
{
    return (x >> 1) + (x & 1);
}

@safe pure nothrow
private uint binLog2(T)(T x)
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

@safe pure nothrow
private Float denormalizedMantissa(T)(T x, uint sign) if (floatFormat!T == FloatFormat.Real80)
{
    x *= 2.0L^^FloatTraits!T.MANTISSA;
    auto fl = parse(x);
    uint pow = FloatTraits!T.MANTISSA - fl.exponent + 1;
    return Float(fl.mantissa >> pow, 0, sign);
}

@safe pure nothrow
private Float denormalizedMantissa(T)(T x, uint sign)
    if (floatFormat!T == FloatFormat.Float || floatFormat!T == FloatFormat.Double)
{
    x *= 2.0L^^FloatTraits!T.MANTISSA;
    auto fl = parse!true(x);
    ulong mant = fl.mantissa >> (FloatTraits!T.MANTISSA - fl.exponent);
    return Float(shiftrRound(mant), 0, sign);
}

@safe pure nothrow
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
        //testNumberConvert!("real.min_normal/19"); // XGDC: ct[0] == 0, rt[0] == 27
        //testNumberConvert!("real.min_normal/17"); // XGDC: ct[0= == 128, rt[0] == 136

        /**Test imaginary values: convert algorithm is same with real values*/
        testNumberConvert!("0.0Fi");
        testNumberConvert!("0.0i");
        testNumberConvert!("0.0Li");

        /**True random values*/
        //testNumberConvert!("-0x9.0f7ee55df77618fp-13829L"); //XGDC: ct[0,1] == [0,96], rt[0,1] == [143,97]
        //testNumberConvert!("0x7.36e6e2640120d28p+8797L"); // XGDC: ct[0,1] == [0,24], rt[0,1] == [80,26]
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
        enum floatFormat = FloatFormat.Double;
    else static if (T.mant_dig == 64)
        enum floatFormat = FloatFormat.Real80;
    else static if (T.mant_dig == 106)
        enum floatFormat = FloatFormat.DoubleDouble;
    else static if (T.mant_dig == 113)
        enum floatFormat = FloatFormat.Quadruple;
    else
        static assert(0);

}

//  all toUbyte functions must be evaluable at compile time
@trusted pure nothrow
const(ubyte)[] toUbyte(T)(T[] arr) if (T.sizeof == 1)
{
    return cast(const(ubyte)[])arr;
}

@trusted pure nothrow
const(ubyte)[] toUbyte(T)(T[] arr) if ((is(typeof(toUbyte(arr[0])) == const(ubyte)[])) && (T.sizeof > 1))
{
    if (__ctfe)
    {
        const(ubyte)[] ret;
        foreach (cur; arr)
        {
            ret ~= toUbyte(cur);
        }
        return ret;
    }
    else
    {
        return (cast(const(ubyte)*)(arr.ptr))[0 .. T.sizeof*arr.length];
    }
}

@trusted pure nothrow
const(ubyte)[] toUbyte(T)(ref T val) if (__traits(isIntegral, T) && !is(T == enum))
{
    static if (T.sizeof == 1)
    {
        if (__ctfe)
        {
            return cast(const(ubyte)[])[val];
        }
        else
        {
            return (cast(const(ubyte)*)(&val))[0 .. T.sizeof];
        }
    }
    else if (__ctfe)
    {
        ubyte[T.sizeof] tmp;
        Unqual!T val_ = val;
        for (size_t i = 0; i < T.sizeof; ++i)
        {
            size_t idx;
            version (LittleEndian) idx = i;
            else idx = T.sizeof-i-1;
            tmp[idx] = cast(ubyte)(val_&0xff);
            val_ >>= 8;
        }
        return tmp[].dup;
    }
    else
    {
        return (cast(const(ubyte)*)(&val))[0 .. T.sizeof];
    }
}

@trusted pure nothrow
const(ubyte)[] toUbyte(T)(ref T val) if (is(Unqual!T == cfloat) || is(Unqual!T == cdouble) ||is(Unqual!T == creal))
{
    if (__ctfe)
    {
        auto re = val.re;
        auto im = val.im;
        return (re.toUbyte() ~ im.toUbyte());
    }
    else
    {
        return (cast(const(ubyte)*)&val)[0 .. T.sizeof];
    }
}

@trusted pure nothrow
const(ubyte)[] toUbyte(T)(ref T val) if (is(T == enum) && is(typeof(toUbyte(cast(V)val)) == const(ubyte)[]))
{
    if (__ctfe)
    {
        static if (is(T V == enum)){}
        V e_val = val;
        return toUbyte(e_val);
    }
    else
    {
        return (cast(const(ubyte)*)&val)[0 .. T.sizeof];
    }
}

private bool isNonReference(T)()
{
    static if (is(T == struct) || is(T == union))
    {
        return isNonReferenceStruct!T();
    }
    else static if (__traits(isStaticArray, T))
    {
      return isNonReference!(typeof(T.init[0]))();
    }
    else static if (is(T E == enum))
    {
      return isNonReference!(E)();
    }
    else static if (!__traits(isScalar, T))
    {
        return false;
    }
    else static if (is(T V : V*))
    {
        return false;
    }
    else static if (is(T == function))
    {
        return false;
    }
    else
    {
        return true;
    }
}

private bool isNonReferenceStruct(T)() if (is(T == struct) || is(T == union))
{
    foreach (cur; T.init.tupleof)
    {
        static if (!isNonReference!(typeof(cur))()) return false;
    }

    return true;
}

@trusted pure nothrow
const(ubyte)[] toUbyte(T)(ref T val) if (is(T == struct) || is(T == union))
{
    if (__ctfe)
    {
        ubyte[T.sizeof] bytes;
        foreach (key, cur; val.tupleof)
        {
            alias CUR_TYPE = typeof(cur);
            static if (isNonReference!(CUR_TYPE)())
            {
                bytes[val.tupleof[key].offsetof .. val.tupleof[key].offsetof + cur.sizeof] = toUbyte(cur)[];
            }
            else static if (is(typeof(val.tupleof[key] is null)))
            {
                assert(val.tupleof[key] is null, "Unable to compute byte representation of non-null reference field at compile time");
                //skip, because val bytes are zeros
            }
            else
            {
                //pragma(msg, "is null: ", typeof(CUR_TYPE).stringof);
                assert(0, "Unable to compute byte representation of "~typeof(CUR_TYPE).stringof~" field at compile time");
            }
        }
        return bytes[].dup;
    }
    else
    {
        return (cast(const(ubyte)*)&val)[0 .. T.sizeof];
    }
}
