/* 128 bit integer arithmetic.
 *
 * Not optimized for speed.
 *
 * Copyright: Copyright D Language Foundation 2022.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Walter Bright
 * Source:    $(DRUNTIMESRC core/_int128.d)
 */

module core.int128;

nothrow:
@safe:
@nogc:

alias I = long;
alias U = ulong;
enum Ubits = uint(U.sizeof * 8);

align(16) struct Cent
{
    U lo;      // low 64 bits
    U hi;      // high 64 bits
}

enum One = Cent(1);
enum Zero = Cent();
enum MinusOne = neg(One);

/*****************************
 * Test against 0
 * Params:
 *      c = Cent to test
 * Returns:
 *      true if != 0
 */
pure
bool tst(Cent c)
{
    return c.hi || c.lo;
}


/*****************************
 * Complement
 * Params:
 *      c = Cent to complement
 * Returns:
 *      complemented value
 */
pure
Cent com(Cent c)
{
    c.lo = ~c.lo;
    c.hi = ~c.hi;
    return c;
}

/*****************************
 * Negate
 * Params:
 *      c = Cent to negate
 * Returns:
 *      negated value
 */
pure
Cent neg(Cent c)
{
    if (c.lo == 0)
        c.hi = -c.hi;
    else
    {
        c.lo = -c.lo;
        c.hi = ~c.hi;
    }
    return c;
}

/*****************************
 * Increment
 * Params:
 *      c = Cent to increment
 * Returns:
 *      incremented value
 */
pure
Cent inc(Cent c)
{
    return add(c, One);
}

/*****************************
 * Decrement
 * Params:
 *      c = Cent to decrement
 * Returns:
 *      incremented value
 */
pure
Cent dec(Cent c)
{
    return sub(c, One);
}

/*****************************
 * Shift left one bit
 * Params:
 *      c = Cent to shift
 * Returns:
 *      shifted value
 */
pure
Cent shl1(Cent c)
{
    c.hi = (c.hi << 1) | (cast(I)c.lo < 0);
    c.lo <<= 1;
    return c;
}

/*****************************
 * Unsigned shift right one bit
 * Params:
 *      c = Cent to shift
 * Returns:
 *      shifted value
 */
pure
Cent shr1(Cent c)
{
    c.lo = (c.lo >> 1) | ((c.hi & 1) << (Ubits - 1));
    c.hi >>= 1;
    return c;
}


/*****************************
 * Arithmetic shift right one bit
 * Params:
 *      c = Cent to shift
 * Returns:
 *      shifted value
 */
pure
Cent sar1(Cent c)
{
    c.lo = (c.lo >> 1) | ((c.hi & 1) << (Ubits - 1));
    c.hi = cast(I)c.hi >> 1;
    return c;
}

/*****************************
 * Shift left n bits
 * Params:
 *      c = Cent to shift
 *      n = number of bits to shift
 * Returns:
 *      shifted value
 */
pure
Cent shl(Cent c, uint n)
{
    if (n >= Ubits * 2)
        return Zero;

    if (n >= Ubits)
    {
        c.hi = c.lo << (n - Ubits);
        c.lo = 0;
    }
    else
    {
        c.hi = ((c.hi << n) | (c.lo >> (Ubits - n - 1) >> 1));
        c.lo = c.lo << n;
    }
    return c;
}

/*****************************
 * Unsigned shift right n bits
 * Params:
 *      c = Cent to shift
 *      n = number of bits to shift
 * Returns:
 *      shifted value
 */
pure
Cent shr(Cent c, uint n)
{
    if (n >= Ubits * 2)
        return Zero;

    if (n >= Ubits)
    {
        c.lo = c.hi >> (n - Ubits);
        c.hi = 0;
    }
    else
    {
        c.lo = ((c.lo >> n) | (c.hi << (Ubits - n - 1) << 1));
        c.hi = c.hi >> n;
    }
    return c;
}

/*****************************
 * Arithmetic shift right n bits
 * Params:
 *      c = Cent to shift
 *      n = number of bits to shift
 * Returns:
 *      shifted value
 */
pure
Cent sar(Cent c, uint n)
{
    const signmask = -(c.hi >> (Ubits - 1));
    const signshift = (Ubits * 2) - n;
    c = shr(c, n);

    // Sign extend all bits beyond the precision of Cent.
    if (n >= Ubits * 2)
    {
        c.hi = signmask;
        c.lo = signmask;
    }
    else if (signshift >= Ubits * 2)
    {
    }
    else if (signshift >= Ubits)
    {
        c.hi &= ~(U.max << (signshift - Ubits));
        c.hi |= signmask << (signshift - Ubits);
    }
    else
    {
        c.hi = signmask;
        c.lo &= ~(U.max << signshift);
        c.lo |= signmask << signshift;
    }
    return c;
}

/*****************************
 * Rotate left one bit
 * Params:
 *      c = Cent to rotate
 * Returns:
 *      rotated value
 */
pure
Cent rol1(Cent c)
{
    int carry = cast(I)c.hi < 0;

    c.hi = (c.hi << 1) | (cast(I)c.lo < 0);
    c.lo = (c.lo << 1) | carry;
    return c;
}

/*****************************
 * Rotate right one bit
 * Params:
 *      c = Cent to rotate
 * Returns:
 *      rotated value
 */
pure
Cent ror1(Cent c)
{
    int carry = c.lo & 1;
    c.lo = (c.lo >> 1) | (cast(U)(c.hi & 1) << (Ubits - 1));
    c.hi = (c.hi >> 1) | (cast(U)carry << (Ubits - 1));
    return c;
}


/*****************************
 * Rotate left n bits
 * Params:
 *      c = Cent to rotate
 *      n = number of bits to rotate
 * Returns:
 *      rotated value
 */
pure
Cent rol(Cent c, uint n)
{
    n &= Ubits * 2 - 1;
    Cent l = shl(c, n);
    Cent r = shr(c, Ubits * 2 - n);
    return or(l, r);
}

/*****************************
 * Rotate right n bits
 * Params:
 *      c = Cent to rotate
 *      n = number of bits to rotate
 * Returns:
 *      rotated value
 */
pure
Cent ror(Cent c, uint n)
{
    n &= Ubits * 2 - 1;
    Cent r = shr(c, n);
    Cent l = shl(c, Ubits * 2 - n);
    return or(r, l);
}

/****************************
 * And c1 & c2.
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      c1 & c2
 */
pure
Cent and(Cent c1, Cent c2)
{
    return Cent(c1.lo & c2.lo, c1.hi & c2.hi);
}

/****************************
 * Or c1 | c2.
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      c1 | c2
 */
pure
Cent or(Cent c1, Cent c2)
{
    return Cent(c1.lo | c2.lo, c1.hi | c2.hi);
}

/****************************
 * Xor c1 ^ c2.
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      c1 ^ c2
 */
pure
Cent xor(Cent c1, Cent c2)
{
    return Cent(c1.lo ^ c2.lo, c1.hi ^ c2.hi);
}

/****************************
 * Add c1 to c2.
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      c1 + c2
 */
pure
Cent add(Cent c1, Cent c2)
{
    U r = cast(U)(c1.lo + c2.lo);
    return Cent(r, cast(U)(c1.hi + c2.hi + (r < c1.lo)));
}

/****************************
 * Subtract c2 from c1.
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      c1 - c2
 */
pure
Cent sub(Cent c1, Cent c2)
{
    return add(c1, neg(c2));
}

/****************************
 * Multiply c1 * c2.
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      c1 * c2
 */
pure
Cent mul(Cent c1, Cent c2)
{
    enum mulmask = (1UL << (Ubits / 2)) - 1;
    enum mulshift = Ubits / 2;

    // This algorithm splits the operands into 4 words, then computes and sums
    // the partial products of each part.
    const c2l0 = c2.lo & mulmask;
    const c2l1 = c2.lo >> mulshift;
    const c2h0 = c2.hi & mulmask;
    const c2h1 = c2.hi >> mulshift;

    const c1l0 = c1.lo & mulmask;
    U r0 = c1l0 * c2l0;
    U r1 = c1l0 * c2l1 + (r0 >> mulshift);
    U r2 = c1l0 * c2h0 + (r1 >> mulshift);
    U r3 = c1l0 * c2h1 + (r2 >> mulshift);

    const c1l1 = c1.lo >> mulshift;
    r1 = c1l1 * c2l0 + (r1 & mulmask);
    r2 = c1l1 * c2l1 + (r2 & mulmask) + (r1 >> mulshift);
    r3 = c1l1 * c2h0 + (r3 & mulmask) + (r2 >> mulshift);

    const c1h0 = c1.hi & mulmask;
    r2 = c1h0 * c2l0 + (r2 & mulmask);
    r3 = c1h0 * c2l1 + (r3 & mulmask) + (r2 >> mulshift);

    const c1h1 = c1.hi >> mulshift;
    r3 = c1h1 * c2l0 + (r3 & mulmask);

    return Cent((r0 & mulmask) + (r1 & mulmask) * (mulmask + 1),
                (r2 & mulmask) + (r3 & mulmask) * (mulmask + 1));

}


/****************************
 * Unsigned divide c1 / c2.
 * Params:
 *      c1 = dividend
 *      c2 = divisor
 * Returns:
 *      quotient c1 / c2
 */
pure
Cent udiv(Cent c1, Cent c2)
{
    Cent modulus;
    return udivmod(c1, c2, modulus);
}

/****************************
 * Unsigned divide c1 / c2. The remainder after division is stored to modulus.
 * Params:
 *      c1 = dividend
 *      c2 = divisor
 *      modulus = set to c1 % c2
 * Returns:
 *      quotient c1 / c2
 */
pure
Cent udivmod(Cent c1, Cent c2, out Cent modulus)
{
    //printf("udiv c1(%llx,%llx) c2(%llx,%llx)\n", c1.lo, c1.hi, c2.lo, c2.hi);
    // Based on "Unsigned Doubleword Division" in Hacker's Delight
    import core.bitop;

    // Divides a 128-bit dividend by a 64-bit divisor.
    // The result must fit in 64 bits.
    static U udivmod128_64(Cent c1, U c2, out U modulus)
    {
        // We work in base 2^^32
        enum base = 1UL << 32;
        enum divmask = (1UL << (Ubits / 2)) - 1;
        enum divshift = Ubits / 2;

        // Check for overflow and divide by 0
        if (c1.hi >= c2)
        {
            modulus = 0UL;
            return ~0UL;
        }

        // Computes [num1 num0] / den
        static uint udiv96_64(U num1, uint num0, U den)
        {
            // Extract both digits of the denominator
            const den1 = cast(uint)(den >> divshift);
            const den0 = cast(uint)(den & divmask);
            // Estimate ret as num1 / den1, and then correct it
            U ret = num1 / den1;
            const t2 = (num1 % den1) * base + num0;
            const t1 = ret * den0;
            if (t1 > t2)
                ret -= (t1 - t2 > den) ? 2 : 1;
            return cast(uint)ret;
        }

        // Determine the normalization factor. We multiply c2 by this, so that its leading
        // digit is at least half base. In binary this means just shifting left by the number
        // of leading zeros, so that there's a 1 in the MSB.
        // We also shift number by the same amount. This cannot overflow because c1.hi < c2.
        const shift = (Ubits - 1) - bsr(c2);
        c2 <<= shift;
        U num2 = c1.hi;
        num2 <<= shift;
        num2 |= (c1.lo >> (-shift & 63)) & (-cast(I)shift >> 63);
        c1.lo <<= shift;

        // Extract the low digits of the numerator (after normalizing)
        const num1 = cast(uint)(c1.lo >> divshift);
        const num0 = cast(uint)(c1.lo & divmask);

        // Compute q1 = [num2 num1] / c2
        const q1 = udiv96_64(num2, num1, c2);
        // Compute the true (partial) remainder
        const rem = num2 * base + num1 - q1 * c2;
        // Compute q0 = [rem num0] / c2
        const q0 = udiv96_64(rem, num0, c2);

        modulus = (rem * base + num0 - q0 * c2) >> shift;
        return (cast(U)q1 << divshift) | q0;
    }

    // Special cases
    if (!tst(c2))
    {
        // Divide by zero
        modulus = Zero;
        return com(modulus);
    }
    if (c1.hi == 0 && c2.hi == 0)
    {
        // Single precision divide
        modulus = Cent(c1.lo % c2.lo);
        return Cent(c1.lo / c2.lo);
    }
    if (c1.hi == 0)
    {
        // Numerator is smaller than the divisor
        modulus = c1;
        return Zero;
    }
    if (c2.hi == 0)
    {
        // Divisor is a 64-bit value, so we just need one 128/64 division.
        // If c1 / c2 would overflow, break c1 up into two halves.
        const q1 = (c1.hi < c2.lo) ? 0 : (c1.hi / c2.lo);
        if (q1)
            c1.hi = c1.hi % c2.lo;
        U rem;
        const q0 = udivmod128_64(c1, c2.lo, rem);
        modulus = Cent(rem);
        return Cent(q0, q1);
    }

    // Full cent precision division.
    // Here c2 >= 2^^64
    // We know that c2.hi != 0, so count leading zeros is OK
    // We have 0 <= shift <= 63
    const shift = (Ubits - 1) - bsr(c2.hi);

    // Normalize the divisor so its MSB is 1
    // v1 = (c2 << shift) >> 64
    U v1 = shl(c2, shift).hi;

    // To ensure no overflow.
    Cent u1 = shr1(c1);

    // Get quotient from divide unsigned operation.
    U rem_ignored;
    const q1 = udivmod128_64(u1, v1, rem_ignored);

    // Undo normalization and division of c1 by 2.
    Cent quotient = shr(shl(Cent(q1), shift), 63);

    // Make quotient correct or too small by 1
    if (tst(quotient))
        quotient = dec(quotient);

    // Now quotient is correct.
    // Compute rem = c1 - (quotient * c2);
    Cent rem = sub(c1, mul(quotient, c2));

    // Check if remainder is larger than the divisor
    if (uge(rem, c2))
    {
        // Increment quotient
        quotient = inc(quotient);
        // Subtract c2 from remainder
        rem = sub(rem, c2);
    }
    modulus = rem;
    //printf("quotient "); print(quotient);
    //printf("modulus  "); print(modulus);
    return quotient;
}


/****************************
 * Signed divide c1 / c2.
 * Params:
 *      c1 = dividend
 *      c2 = divisor
 * Returns:
 *      quotient c1 / c2
 */
pure
Cent div(Cent c1, Cent c2)
{
    Cent modulus;
    return divmod(c1, c2, modulus);
}

/****************************
 * Signed divide c1 / c2. The remainder after division is stored to modulus.
 * Params:
 *      c1 = dividend
 *      c2 = divisor
 *      modulus = set to c1 % c2
 * Returns:
 *      quotient c1 / c2
 */
pure
Cent divmod(Cent c1, Cent c2, out Cent modulus)
{
    /* Muck about with the signs so we can use the unsigned divide
     */
    if (cast(I)c1.hi < 0)
    {
        if (cast(I)c2.hi < 0)
        {
            Cent r = udivmod(neg(c1), neg(c2), modulus);
            modulus = neg(modulus);
            return r;
        }
        Cent r = neg(udivmod(neg(c1), c2, modulus));
        modulus = neg(modulus);
        return r;
    }
    else if (cast(I)c2.hi < 0)
    {
        return neg(udivmod(c1, neg(c2), modulus));
    }
    else
        return udivmod(c1, c2, modulus);
}

/****************************
 * If c1 > c2 unsigned
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      true if c1 > c2
 */
pure
bool ugt(Cent c1, Cent c2)
{
    return (c1.hi == c2.hi) ? (c1.lo > c2.lo) : (c1.hi > c2.hi);
}

/****************************
 * If c1 >= c2 unsigned
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      true if c1 >= c2
 */
pure
bool uge(Cent c1, Cent c2)
{
    return !ugt(c2, c1);
}

/****************************
 * If c1 < c2 unsigned
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      true if c1 < c2
 */
pure
bool ult(Cent c1, Cent c2)
{
    return ugt(c2, c1);
}

/****************************
 * If c1 <= c2 unsigned
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      true if c1 <= c2
 */
pure
bool ule(Cent c1, Cent c2)
{
    return !ugt(c1, c2);
}

/****************************
 * If c1 > c2 signed
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      true if c1 > c2
 */
pure
bool gt(Cent c1, Cent c2)
{
    return (c1.hi == c2.hi)
        ? (c1.lo > c2.lo)
        : (cast(I)c1.hi > cast(I)c2.hi);
}

/****************************
 * If c1 >= c2 signed
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      true if c1 >= c2
 */
pure
bool ge(Cent c1, Cent c2)
{
    return !gt(c2, c1);
}

/****************************
 * If c1 < c2 signed
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      true if c1 < c2
 */
pure
bool lt(Cent c1, Cent c2)
{
    return gt(c2, c1);
}

/****************************
 * If c1 <= c2 signed
 * Params:
 *      c1 = operand 1
 *      c2 = operand 2
 * Returns:
 *      true if c1 <= c2
 */
pure
bool le(Cent c1, Cent c2)
{
    return !gt(c1, c2);
}

/*******************************************************/

version (unittest)
{
    version (none)
    {
        import core.stdc.stdio;

        @trusted
        void print(Cent c)
        {
            printf("%lld, %lld\n", cast(ulong)c.lo, cast(ulong)c.hi);
            printf("x%llx, x%llx\n", cast(ulong)c.lo, cast(ulong)c.hi);
        }
    }
}

unittest
{
    const C0 = Zero;
    const C1 = One;
    const C2 = Cent(2);
    const C3 = Cent(3);
    const C5 = Cent(5);
    const C10 = Cent(10);
    const C20 = Cent(20);
    const C30 = Cent(30);
    const C100 = Cent(100);

    const Cm1 =  neg(One);
    const Cm3 =  neg(C3);
    const Cm10 = neg(C10);

    const C3_1 = Cent(1,3);
    const C3_2 = Cent(2,3);
    const C4_8  = Cent(8, 4);
    const C5_0  = Cent(0, 5);
    const C7_1 = Cent(1,7);
    const C7_9 = Cent(9,7);
    const C9_3 = Cent(3,9);
    const C10_0 = Cent(0,10);
    const C10_1 = Cent(1,10);
    const C10_3 = Cent(3,10);
    const C11_3 = Cent(3,11);
    const C20_0 = Cent(0,20);
    const C90_30 = Cent(30,90);

    const Cm10_0 = inc(com(C10_0)); // Cent(0, -10);
    const Cm10_1 = inc(com(C10_1)); // Cent(-1, -11);
    const Cm10_3 = inc(com(C10_3)); // Cent(-3, -11);

    enum Cs_3 = Cent(3, I.min);

    const Cbig_1 = Cent(0xa3ccac1832952398, 0xc3ac542864f652f8);
    const Cbig_2 = Cent(0x5267b85f8a42fc20, 0);
    const Cbig_3 = Cent(0xf0000000ffffffff, 0);

    /************************/

    assert( ugt(C1, C0) );
    assert( ult(C1, C2) );
    assert( uge(C1, C0) );
    assert( ule(C1, C2) );

    assert( !ugt(C0, C1) );
    assert( !ult(C2, C1) );
    assert( !uge(C0, C1) );
    assert( !ule(C2, C1) );

    assert( !ugt(C1, C1) );
    assert( !ult(C1, C1) );
    assert( uge(C1, C1) );
    assert( ule(C2, C2) );

    assert( ugt(C10_3, C10_1) );
    assert( ugt(C11_3, C10_3) );
    assert( !ugt(C9_3, C10_3) );
    assert( !ugt(C9_3, C9_3) );

    assert( gt(C2, C1) );
    assert( !gt(C1, C2) );
    assert( !gt(C1, C1) );
    assert( gt(C0, Cm1) );
    assert( gt(Cm1, neg(C10)));
    assert( !gt(Cm1, Cm1) );
    assert( !gt(Cm1, C0) );

    assert( !lt(C2, C1) );
    assert( !le(C2, C1) );
    assert( ge(C2, C1) );

    assert(neg(C10_0) == Cm10_0);
    assert(neg(C10_1) == Cm10_1);
    assert(neg(C10_3) == Cm10_3);

    assert(add(C7_1,C3_2) == C10_3);
    assert(sub(C1,C2) == Cm1);

    assert(inc(C3_1) == C3_2);
    assert(dec(C3_2) == C3_1);

    assert(shl(C10,0) == C10);
    assert(shl(C10,Ubits) == C10_0);
    assert(shl(C10,1) == C20);
    assert(shl(C10,Ubits * 2) == C0);
    assert(shr(C10_0,0) == C10_0);
    assert(shr(C10_0,Ubits) == C10);
    assert(shr(C10_0,Ubits - 1) == C20);
    assert(shr(C10_0,Ubits + 1) == C5);
    assert(shr(C10_0,Ubits * 2) == C0);
    assert(sar(C10_0,0) == C10_0);
    assert(sar(C10_0,Ubits) == C10);
    assert(sar(C10_0,Ubits - 1) == C20);
    assert(sar(C10_0,Ubits + 1) == C5);
    assert(sar(C10_0,Ubits * 2) == C0);
    assert(sar(Cm1,Ubits * 2) == Cm1);

    assert(shl1(C10) == C20);
    assert(shr1(C10_0) == C5_0);
    assert(sar1(C10_0) == C5_0);
    assert(sar1(Cm1) == Cm1);

    Cent modulus;

    assert(udiv(C10,C2) == C5);
    assert(udivmod(C10,C2, modulus) ==  C5);   assert(modulus == C0);
    assert(udivmod(C10,C3, modulus) ==  C3);   assert(modulus == C1);
    assert(udivmod(C10,C0, modulus) == Cm1);   assert(modulus == C0);
    assert(udivmod(C2,C90_30, modulus) == C0); assert(modulus == C2);
    assert(udiv(mul(C90_30, C2), C2) == C90_30);
    assert(udiv(mul(C90_30, C2), C90_30) == C2);

    assert(div(C10,C3) == C3);
    assert(divmod( C10,  C3, modulus) ==  C3); assert(modulus ==  C1);
    assert(divmod(Cm10,  C3, modulus) == Cm3); assert(modulus == Cm1);
    assert(divmod( C10, Cm3, modulus) == Cm3); assert(modulus ==  C1);
    assert(divmod(Cm10, Cm3, modulus) ==  C3); assert(modulus == Cm1);
    assert(divmod(C2, C90_30, modulus) == C0); assert(modulus == C2);
    assert(div(mul(C90_30, C2), C2) == C90_30);
    assert(div(mul(C90_30, C2), C90_30) == C2);

    assert(divmod(Cbig_1, Cbig_2, modulus) == Cent(0x4496aa309d4d4a2f, U.max));
    assert(modulus == Cent(0xd83203d0fdc799b8, U.max));
    assert(udivmod(Cbig_1, Cbig_2, modulus) == Cent(0x5fe0e9bace2bedad, 2));
    assert(modulus == Cent(0x2c923125a68721f8, 0));
    assert(div(Cbig_1, Cbig_3) == Cent(0xbfa6c02b5aff8b86, U.max));
    assert(udiv(Cbig_1, Cbig_3) == Cent(0xd0b7d13b48cb350f, 0));

    assert(mul(Cm10, C1) == Cm10);
    assert(mul(C1, Cm10) == Cm10);
    assert(mul(C9_3, C10) == C90_30);
    assert(mul(Cs_3, C10) == C30);
    assert(mul(Cm10, Cm10) == C100);

    assert( or(C4_8, C3_1) == C7_9);
    assert(and(C4_8, C7_9) == C4_8);
    assert(xor(C4_8, C7_9) == C3_1);

    assert(rol(Cm1,  1) == Cm1);
    assert(ror(Cm1, 45) == Cm1);
    assert(rol(ror(C7_9, 5), 5) == C7_9);
    assert(rol(C7_9, 1) == rol1(C7_9));
    assert(ror(C7_9, 1) == ror1(C7_9));
}


