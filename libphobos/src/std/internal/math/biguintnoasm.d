/** Arbitrary precision arithmetic ('bignum') for processors with no asm support
 *
 * All functions operate on arrays of uints, stored LSB first.
 * If there is a destination array, it will be the first parameter.
 * Currently, all of these functions are subject to change, and are
 * intended for internal use only.
 * This module is intended only to assist development of high-speed routines
 * on currently unsupported processors.
 * The X86 asm version is about 30 times faster than the D version (DMD).
 */

/*          Copyright Don Clugston 2008 - 2010.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */

module std.internal.math.biguintnoasm;

nothrow:
@safe:

public:
alias BigDigit = uint; // A Bignum is an array of BigDigits.

    // Limits for when to switch between multiplication algorithms.
enum int KARATSUBALIMIT = 10; // Minimum value for which Karatsuba is worthwhile.
enum int KARATSUBASQUARELIMIT = 12; // Minimum value for which square Karatsuba is worthwhile


/** Multi-byte addition or subtraction
 *    dest[] = src1[] + src2[] + carry (0 or 1).
 * or dest[] = src1[] - src2[] - carry (0 or 1).
 * Returns carry or borrow (0 or 1).
 * Set op == '+' for addition, '-' for subtraction.
 */
uint multibyteAddSub(char op)(uint[] dest, const(uint) [] src1,
    const (uint) [] src2, uint carry) pure @nogc @safe
{
    ulong c = carry;
    for (size_t i = 0; i < src2.length; ++i)
    {
        static if (op=='+') c = c  + src1[i] + src2[i];
             else           c = cast(ulong) src1[i] - src2[i] - c;
        dest[i] = cast(uint) c;
        c = (c > 0xFFFF_FFFF);
    }
    return cast(uint) c;
}

@safe unittest
{
    uint [] a = new uint[40];
    uint [] b = new uint[40];
    uint [] c = new uint[40];
    for (size_t i = 0; i < a.length; ++i)
    {
        if (i&1) a[i]=cast(uint)(0x8000_0000 + i);
        else a[i]=cast(uint) i;
        b[i]= 0x8000_0003;
    }
    c[19]=0x3333_3333;
    uint carry = multibyteAddSub!('+')(c[0 .. 18], b[0 .. 18], a[0 .. 18], 0);
    assert(c[0]==0x8000_0003);
    assert(c[1]==4);
    assert(c[19]==0x3333_3333); // check for overrun
    assert(carry == 1);
    for (size_t i = 0; i < a.length; ++i)
    {
        a[i] = b[i] = c[i] = 0;
    }
    a[8]=0x048D159E;
    b[8]=0x048D159E;
    a[10]=0x1D950C84;
    b[10]=0x1D950C84;
    a[5] =0x44444444;
    carry = multibyteAddSub!('-')(a[0 .. 12], a[0 .. 12], b[0 .. 12], 0);
    assert(a[11] == 0);
    for (size_t i = 0; i < 10; ++i)
        if (i != 5)
            assert(a[i] == 0);

    for (size_t q = 3; q < 36; ++q)
    {
        for (size_t i = 0; i< a.length; ++i)
        {
            a[i] = b[i] = c[i] = 0;
        }
        a[q-2]=0x040000;
        b[q-2]=0x040000;
       carry = multibyteAddSub!('-')(a[0 .. q], a[0 .. q], b[0 .. q], 0);
       assert(a[q-2]==0);
    }
}



/** dest[] += carry, or dest[] -= carry.
 *  op must be '+' or '-'
 *  Returns final carry or borrow (0 or 1)
 */
uint multibyteIncrementAssign(char op)(uint[] dest, uint carry)
    pure @nogc @safe
{
    static if (op=='+')
    {
        ulong c = carry;
        c += dest[0];
        dest[0] = cast(uint) c;
        if (c <= 0xFFFF_FFFF)
            return 0;

        for (size_t i = 1; i < dest.length; ++i)
        {
            ++dest[i];
            if (dest[i] != 0)
                return 0;
        }
        return 1;
    }
    else
    {
        ulong c = carry;
        c = dest[0] - c;
        dest[0] = cast(uint) c;
        if (c <= 0xFFFF_FFFF)
            return 0;
        for (size_t i = 1; i < dest.length; ++i)
        {
            --dest[i];
            if (dest[i] != 0xFFFF_FFFF)
                return 0;
        }
        return 1;
    }
}

/** dest[] = src[] << numbits
 *  numbits must be in the range 1 .. 31
 */
uint multibyteShl(uint [] dest, const(uint) [] src, uint numbits)
    pure @nogc @safe
{
    ulong c = 0;
    for (size_t i = 0; i < dest.length; ++i)
    {
        c += (cast(ulong)(src[i]) << numbits);
        dest[i] = cast(uint) c;
        c >>>= 32;
   }
   return cast(uint) c;
}


/** dest[] = src[] >> numbits
 *  numbits must be in the range 1 .. 31
 */
void multibyteShr(uint [] dest, const(uint) [] src, uint numbits)
    pure @nogc @safe
{
    ulong c = 0;
    for (ptrdiff_t i = dest.length; i != 0; --i)
    {
        c += (src[i-1] >>numbits) + (cast(ulong)(src[i-1]) << (64 - numbits));
        dest[i-1] = cast(uint) c;
        c >>>= 32;
   }
}

@safe unittest
{

    uint [] aa = [0x1222_2223, 0x4555_5556, 0x8999_999A, 0xBCCC_CCCD, 0xEEEE_EEEE];
    multibyteShr(aa[0..$-2], aa, 4);
    assert(aa[0] == 0x6122_2222 && aa[1] == 0xA455_5555 && aa[2] == 0x0899_9999);
    assert(aa[3] == 0xBCCC_CCCD);

    aa = [0x1222_2223, 0x4555_5556, 0x8999_999A, 0xBCCC_CCCD, 0xEEEE_EEEE];
    multibyteShr(aa[0..$-1], aa, 4);
    assert(aa[0] == 0x6122_2222 && aa[1] == 0xA455_5555
        && aa[2] == 0xD899_9999 && aa[3] == 0x0BCC_CCCC);

    aa = [0xF0FF_FFFF, 0x1222_2223, 0x4555_5556, 0x8999_999A, 0xBCCC_CCCD,
        0xEEEE_EEEE];
    multibyteShl(aa[1 .. 4], aa[1..$], 4);
    assert(aa[0] == 0xF0FF_FFFF && aa[1] == 0x2222_2230
        && aa[2]==0x5555_5561 && aa[3]==0x9999_99A4 && aa[4]==0x0BCCC_CCCD);
}

/** dest[] = src[] * multiplier + carry.
 * Returns carry.
 */
uint multibyteMul(uint[] dest, const(uint)[] src, uint multiplier, uint carry)
    pure @nogc @safe
{
    assert(dest.length == src.length);
    ulong c = carry;
    for (size_t i = 0; i < src.length; ++i)
    {
        c += cast(ulong)(src[i]) * multiplier;
        dest[i] = cast(uint) c;
        c>>=32;
    }
    return cast(uint) c;
}

@safe unittest
{
    uint [] aa = [0xF0FF_FFFF, 0x1222_2223, 0x4555_5556, 0x8999_999A,
        0xBCCC_CCCD, 0xEEEE_EEEE];
    multibyteMul(aa[1 .. 4], aa[1 .. 4], 16, 0);
    assert(aa[0] == 0xF0FF_FFFF && aa[1] == 0x2222_2230 && aa[2]==0x5555_5561
        && aa[3]==0x9999_99A4 && aa[4]==0x0BCCC_CCCD);
}

/**
 * dest[] += src[] * multiplier + carry(0 .. FFFF_FFFF).
 * Returns carry out of MSB (0 .. FFFF_FFFF).
 */
uint multibyteMulAdd(char op)(uint [] dest, const(uint)[] src,
    uint multiplier, uint carry) pure @nogc @safe
{
    assert(dest.length == src.length);
    ulong c = carry;
    for (size_t i = 0; i < src.length; ++i)
    {
        static if (op=='+')
        {
            c += cast(ulong)(multiplier) * src[i]  + dest[i];
            dest[i] = cast(uint) c;
            c >>= 32;
        }
        else
        {
            c += cast(ulong) multiplier * src[i];
            ulong t = cast(ulong) dest[i] - cast(uint) c;
            dest[i] = cast(uint) t;
            c = cast(uint)((c >> 32) - (t >> 32));
        }
    }
    return cast(uint) c;
}

@safe unittest
{

    uint [] aa = [0xF0FF_FFFF, 0x1222_2223, 0x4555_5556, 0x8999_999A,
        0xBCCC_CCCD, 0xEEEE_EEEE];
    uint [] bb = [0x1234_1234, 0xF0F0_F0F0, 0x00C0_C0C0, 0xF0F0_F0F0,
        0xC0C0_C0C0];
    multibyteMulAdd!('+')(bb[1..$-1], aa[1..$-2], 16, 5);
    assert(bb[0] == 0x1234_1234 && bb[4] == 0xC0C0_C0C0);
    assert(bb[1] == 0x2222_2230 + 0xF0F0_F0F0 + 5
        && bb[2] == 0x5555_5561 + 0x00C0_C0C0 + 1
        && bb[3] == 0x9999_99A4 + 0xF0F0_F0F0 );
}


/**
   Sets result = result[0 .. left.length] + left * right

   It is defined in this way to allow cache-efficient multiplication.
   This function is equivalent to:
    ----
    for (size_t i = 0; i< right.length; ++i)
    {
        dest[left.length + i] = multibyteMulAdd(dest[i .. left.length+i],
                left, right[i], 0);
    }
    ----
 */
void multibyteMultiplyAccumulate(uint [] dest, const(uint)[] left, const(uint)
        [] right) pure @nogc @safe
{
    for (size_t i = 0; i < right.length; ++i)
    {
        dest[left.length + i] = multibyteMulAdd!('+')(dest[i .. left.length+i],
                left, right[i], 0);
    }
}

/**  dest[] /= divisor.
 * overflow is the initial remainder, and must be in the range 0 .. divisor-1.
 */
uint multibyteDivAssign(uint [] dest, uint divisor, uint overflow)
    pure @nogc @safe
{
    ulong c = cast(ulong) overflow;
    for (ptrdiff_t i = dest.length-1; i >= 0; --i)
    {
        c = (c << 32) + cast(ulong)(dest[i]);
        uint q = cast(uint)(c/divisor);
        c -= divisor * q;
        dest[i] = q;
    }
    return cast(uint) c;
}

@safe unittest
{
    uint [] aa = new uint[101];
    for (uint i = 0; i < aa.length; ++i)
        aa[i] = 0x8765_4321 * (i+3);
    uint overflow = multibyteMul(aa, aa, 0x8EFD_FCFB, 0x33FF_7461);
    uint r = multibyteDivAssign(aa, 0x8EFD_FCFB, overflow);
    for (uint i=0; i<aa.length; ++i)
    {
        assert(aa[i] == 0x8765_4321 * (i+3));
    }
    assert(r == 0x33FF_7461);

}
// Set dest[2*i .. 2*i+1]+=src[i]*src[i]
void multibyteAddDiagonalSquares(uint[] dest, const(uint)[] src)
    pure @nogc @safe
{
    ulong c = 0;
    for (size_t i = 0; i < src.length; ++i)
    {
        // At this point, c is 0 or 1, since FFFF*FFFF+FFFF_FFFF = 1_0000_0000.
        c += cast(ulong)(src[i]) * src[i] + dest[2*i];
        dest[2*i] = cast(uint) c;
        c = (c>>=32) + dest[2*i+1];
        dest[2*i+1] = cast(uint) c;
        c >>= 32;
    }
}

// Does half a square multiply. (square = diagonal + 2*triangle)
void multibyteTriangleAccumulate(uint[] dest, const(uint)[] x)
    pure @nogc @safe
{
    // x[0]*x[1...$] + x[1]*x[2..$] + ... + x[$-2]x[$-1..$]
    dest[x.length] = multibyteMul(dest[1 .. x.length], x[1..$], x[0], 0);
    if (x.length < 4)
    {
        if (x.length == 3)
        {
            ulong c = cast(ulong)(x[$-1]) * x[$-2]  + dest[2*x.length-3];
            dest[2*x.length - 3] = cast(uint) c;
            c >>= 32;
            dest[2*x.length - 2] = cast(uint) c;
        }
        return;
    }
    for (size_t i = 2; i < x.length - 2; ++i)
    {
        dest[i-1+ x.length] = multibyteMulAdd!('+')(
             dest[i+i-1 .. i+x.length-1], x[i..$], x[i-1], 0);
    }
        // Unroll the last two entries, to reduce loop overhead:
    ulong  c = cast(ulong)(x[$-3]) * x[$-2] + dest[2*x.length-5];
    dest[2*x.length-5] = cast(uint) c;
    c >>= 32;
    c += cast(ulong)(x[$-3]) * x[$-1] + dest[2*x.length-4];
    dest[2*x.length-4] = cast(uint) c;
    c >>= 32;
    c += cast(ulong)(x[$-1]) * x[$-2];
    dest[2*x.length-3] = cast(uint) c;
    c >>= 32;
    dest[2*x.length-2] = cast(uint) c;
}

void multibyteSquare(BigDigit[] result, const(BigDigit) [] x) pure @nogc @safe
{
    multibyteTriangleAccumulate(result, x);
    result[$-1] = multibyteShl(result[1..$-1], result[1..$-1], 1); // mul by 2
    result[0] = 0;
    multibyteAddDiagonalSquares(result, x);
}
