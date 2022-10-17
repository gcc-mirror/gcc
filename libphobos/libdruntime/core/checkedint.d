
/**********************************************
 * This module implements integral arithmetic primitives that check
 * for out-of-range results.
 *
 * Integral arithmetic operators operate on fixed width types.
 * Results that are not representable in those fixed widths are silently
 * truncated to fit.
 * This module offers integral arithmetic primitives that produce the
 * same results, but set an 'overflow' flag when such truncation occurs.
 * The setting is sticky, meaning that numerous operations can be cascaded
 * and then the flag need only be checked at the end.
 * Whether the operation is signed or unsigned is indicated by an 's' or 'u'
 * suffix, respectively. While this could be achieved without such suffixes by
 * using overloading on the signedness of the types, the suffix makes it clear
 * which is happening without needing to examine the types.
 *
 * While the generic versions of these functions are computationally expensive
 * relative to the cost of the operation itself, compiler implementations are free
 * to recognize them and generate equivalent and faster code.
 *
 * The functions here are templates so they can be used with -betterC,
 * as betterC does not link with this library.
 *
 * References: $(LINK2 http://blog.regehr.org/archives/1139, Fast Integer Overflow Checks)
 * Copyright: Copyright (c) Walter Bright 2014.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Walter Bright
 * Source:    $(DRUNTIMESRC core/_checkedint.d)
 */

module core.checkedint;

import core.internal.attributes : betterC;

nothrow:
@safe:
@nogc:
pure:

/*******************************
 * Add two signed integers, checking for overflow.
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the sum
 */

pragma(inline, true)
int adds()(int x, int y, ref bool overflow)
{
    long r = cast(long)x + cast(long)y;
    if (r < int.min || r > int.max)
        overflow = true;
    return cast(int)r;
}

///
@betterC
unittest
{
    bool overflow;
    assert(adds(2, 3, overflow) == 5);
    assert(!overflow);

    assert(adds(1, int.max - 1, overflow) == int.max);
    assert(!overflow);

    assert(adds(int.min + 1, -1, overflow) == int.min);
    assert(!overflow);

    assert(adds(int.max, 1, overflow) == int.min);
    assert(overflow);

    overflow = false;
    assert(adds(int.min, -1, overflow) == int.max);
    assert(overflow);

    assert(adds(0, 0, overflow) == 0);
    assert(overflow);                   // sticky
}

/// ditto
pragma(inline, true)
long adds()(long x, long y, ref bool overflow)
{
    long r = cast(ulong)x + cast(ulong)y;
    if (x <  0 && y <  0 && r >= 0 ||
        x >= 0 && y >= 0 && r <  0)
        overflow = true;
    return r;
}

///
@betterC
unittest
{
    bool overflow;
    assert(adds(2L, 3L, overflow) == 5);
    assert(!overflow);

    assert(adds(1L, long.max - 1, overflow) == long.max);
    assert(!overflow);

    assert(adds(long.min + 1, -1, overflow) == long.min);
    assert(!overflow);

    assert(adds(long.max, 1, overflow) == long.min);
    assert(overflow);

    overflow = false;
    assert(adds(long.min, -1, overflow) == long.max);
    assert(overflow);

    assert(adds(0L, 0L, overflow) == 0);
    assert(overflow);                   // sticky
}

static if (is(cent))
{
/// ditto
pragma(inline, true)
cent adds()(cent x, cent y, ref bool overflow)
{
    cent r = cast(ucent)x + cast(ucent)y;
    if (x <  0 && y <  0 && r >= 0 ||
        x >= 0 && y >= 0 && r <  0)
        overflow = true;
    return r;
}

unittest
{
    bool overflow;
    assert(adds(cast(cent)2L, 3L, overflow) == 5);
    assert(!overflow);
    assert(adds(1L, cent.max - 1, overflow) == cent.max);
    assert(!overflow);
    assert(adds(cent.min + 1, -1, overflow) == cent.min);
    assert(!overflow);
    assert(adds(cent.max, 1, overflow) == cent.min);
    assert(overflow);
    overflow = false;
    assert(adds(cent.min, -1, overflow) == cent.max);
    assert(overflow);
    assert(adds(cast(cent)0L, 0L, overflow) == 0);
    assert(overflow);                   // sticky
}
}


/*******************************
 * Add two unsigned integers, checking for overflow (aka carry).
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the sum
 */

pragma(inline, true)
uint addu()(uint x, uint y, ref bool overflow)
{
    immutable uint r = x + y;
    immutable bool o = r < x;
    assert(o == (r < y));
    if (o)
        overflow = true;
    return r;
}

///
@betterC
unittest
{
    for (uint i = 0; i < 10; ++i)
    {
        bool overflow;
        immutable uint r = addu (uint.max - i, uint.max - i, overflow);
        assert (r == 2 * (uint.max - i));
        assert (overflow);
    }

    bool overflow;
    assert(addu(2, 3, overflow) == 5);
    assert(!overflow);

    assert(addu(1, uint.max - 1, overflow) == uint.max);
    assert(!overflow);

    assert(addu(uint.min, -1, overflow) == uint.max);
    assert(!overflow);

    assert(addu(uint.max, 1, overflow) == uint.min);
    assert(overflow);

    overflow = false;
    assert(addu(uint.min + 1, -1, overflow) == uint.min);
    assert(overflow);

    assert(addu(0, 0, overflow) == 0);
    assert(overflow);                   // sticky
}

/// ditto
pragma(inline, true)
ulong addu()(ulong x, ulong y, ref bool overflow)
{
    immutable ulong r = x + y;
    immutable bool o = r < x;
    assert(o == (r < y));
    if (o)
        overflow = true;
    return r;
}

///
@betterC
unittest
{
    bool overflow;
    assert(addu(2L, 3L, overflow) == 5);
    assert(!overflow);

    assert(addu(1, ulong.max - 1, overflow) == ulong.max);
    assert(!overflow);

    assert(addu(ulong.min, -1L, overflow) == ulong.max);
    assert(!overflow);

    assert(addu(ulong.max, 1, overflow) == ulong.min);
    assert(overflow);

    overflow = false;
    assert(addu(ulong.min + 1, -1L, overflow) == ulong.min);
    assert(overflow);

    assert(addu(0L, 0L, overflow) == 0);
    assert(overflow);                   // sticky
}

static if (is(ucent))
{
/// ditto
pragma(inline, true)
ucent addu()(ucent x, ucent y, ref bool overflow)
{
    immutable ucent r = x + y;
    immutable bool o = r < x;
    assert(o == (r < y));
    if (o)
        overflow = true;
    return r;
}

unittest
{
    bool overflow;
    assert(addu(cast(ucent)2L, 3L, overflow) == 5);
    assert(!overflow);
    assert(addu(1, ucent.max - 1, overflow) == ucent.max);
    assert(!overflow);
    assert(addu(ucent.min, -1L, overflow) == ucent.max);
    assert(!overflow);
    assert(addu(ucent.max, 1, overflow) == ucent.min);
    assert(overflow);
    overflow = false;
    assert(addu(ucent.min + 1, -1L, overflow) == ucent.min);
    assert(overflow);
    assert(addu(cast(ucent)0L, 0L, overflow) == 0);
    assert(overflow);                   // sticky
}
}


/*******************************
 * Subtract two signed integers, checking for overflow.
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the difference
 */

pragma(inline, true)
int subs()(int x, int y, ref bool overflow)
{
    immutable long r = cast(long)x - cast(long)y;
    if (r < int.min || r > int.max)
        overflow = true;
    return cast(int)r;
}

///
@betterC
unittest
{
    bool overflow;
    assert(subs(2, -3, overflow) == 5);
    assert(!overflow);

    assert(subs(1, -int.max + 1, overflow) == int.max);
    assert(!overflow);

    assert(subs(int.min + 1, 1, overflow) == int.min);
    assert(!overflow);

    assert(subs(int.max, -1, overflow) == int.min);
    assert(overflow);

    overflow = false;
    assert(subs(int.min, 1, overflow) == int.max);
    assert(overflow);

    assert(subs(0, 0, overflow) == 0);
    assert(overflow);                   // sticky
}

/// ditto
pragma(inline, true)
long subs()(long x, long y, ref bool overflow)
{
    immutable long r = cast(ulong)x - cast(ulong)y;
    if (x <  0 && y >= 0 && r >= 0 ||
        x >= 0 && y <  0 && (r <  0 || y == long.min))
        overflow = true;
    return r;
}

///
@betterC
unittest
{
    bool overflow;
    assert(subs(2L, -3L, overflow) == 5);
    assert(!overflow);

    assert(subs(1L, -long.max + 1, overflow) == long.max);
    assert(!overflow);

    assert(subs(long.min + 1, 1, overflow) == long.min);
    assert(!overflow);

    assert(subs(-1L, long.min, overflow) == long.max);
    assert(!overflow);

    assert(subs(long.max, -1, overflow) == long.min);
    assert(overflow);

    overflow = false;
    assert(subs(long.min, 1, overflow) == long.max);
    assert(overflow);

    assert(subs(0L, 0L, overflow) == 0);
    assert(overflow);                   // sticky
}

static if (is(cent))
{
/// ditto
pragma(inline, true)
cent subs()(cent x, cent y, ref bool overflow)
{
    immutable cent r = cast(ucent)x - cast(ucent)y;
    if (x <  0 && y >= 0 && r >= 0 ||
        x >= 0 && y <  0 && (r <  0 || y == long.min))
        overflow = true;
    return r;
}

unittest
{
    bool overflow;
    assert(subs(cast(cent)2L, -3L, overflow) == 5);
    assert(!overflow);
    assert(subs(1L, -cent.max + 1, overflow) == cent.max);
    assert(!overflow);
    assert(subs(cent.min + 1, 1, overflow) == cent.min);
    assert(!overflow);
    assert(subs(-1L, cent.min, overflow) == cent.max);
    assert(!overflow);
    assert(subs(cent.max, -1, overflow) == cent.min);
    assert(overflow);
    overflow = false;
    assert(subs(cent.min, 1, overflow) == cent.max);
    assert(overflow);
    assert(subs(cast(cent)0L, 0L, overflow) == 0);
    assert(overflow);                   // sticky
}
}


/*******************************
 * Subtract two unsigned integers, checking for overflow (aka borrow).
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the difference
 */

pragma(inline, true)
uint subu()(uint x, uint y, ref bool overflow)
{
    if (x < y)
        overflow = true;
    return x - y;
}

///
@betterC
unittest
{
    bool overflow;
    assert(subu(3, 2, overflow) == 1);
    assert(!overflow);

    assert(subu(uint.max, 1, overflow) == uint.max - 1);
    assert(!overflow);

    assert(subu(1, 1, overflow) == uint.min);
    assert(!overflow);

    assert(subu(0, 1, overflow) == uint.max);
    assert(overflow);

    overflow = false;
    assert(subu(uint.max - 1, uint.max, overflow) == uint.max);
    assert(overflow);

    assert(subu(0, 0, overflow) == 0);
    assert(overflow);                   // sticky
}


/// ditto
pragma(inline, true)
ulong subu()(ulong x, ulong y, ref bool overflow)
{
    if (x < y)
        overflow = true;
    return x - y;
}

///
@betterC
unittest
{
    bool overflow;
    assert(subu(3UL, 2UL, overflow) == 1);
    assert(!overflow);

    assert(subu(ulong.max, 1, overflow) == ulong.max - 1);
    assert(!overflow);

    assert(subu(1UL, 1UL, overflow) == ulong.min);
    assert(!overflow);

    assert(subu(0UL, 1UL, overflow) == ulong.max);
    assert(overflow);

    overflow = false;
    assert(subu(ulong.max - 1, ulong.max, overflow) == ulong.max);
    assert(overflow);

    assert(subu(0UL, 0UL, overflow) == 0);
    assert(overflow);                   // sticky
}

static if (is(ucent))
{
/// ditto
pragma(inline, true)
ucent subu()(ucent x, ucent y, ref bool overflow)
{
    if (x < y)
        overflow = true;
    return x - y;
}

unittest
{
    bool overflow;
    assert(subu(cast(ucent)3UL, 2UL, overflow) == 1);
    assert(!overflow);
    assert(subu(ucent.max, 1, overflow) == ucent.max - 1);
    assert(!overflow);
    assert(subu(1UL, 1UL, overflow) == ucent.min);
    assert(!overflow);
    assert(subu(cast(ucent)0UL, 1UL, overflow) == ucent.max);
    assert(overflow);
    overflow = false;
    assert(subu(ucent.max - 1, ucent.max, overflow) == ucent.max);
    assert(overflow);
    assert(subu(cast(ucent)0UL, 0UL, overflow) == 0);
    assert(overflow);                   // sticky
}
}


/***********************************************
 * Negate an integer.
 *
 * Params:
 *      x = operand
 *      overflow = set if x cannot be negated, is not affected otherwise
 * Returns:
 *      the negation of x
 */

pragma(inline, true)
int negs()(int x, ref bool overflow)
{
    if (x == int.min)
        overflow = true;
    return -x;
}

///
@betterC
unittest
{
    bool overflow;
    assert(negs(0, overflow) == -0);
    assert(!overflow);

    assert(negs(1234, overflow) == -1234);
    assert(!overflow);

    assert(negs(-5678, overflow) == 5678);
    assert(!overflow);

    assert(negs(int.min, overflow) == -int.min);
    assert(overflow);

    assert(negs(0, overflow) == -0);
    assert(overflow);                   // sticky
}

/// ditto
pragma(inline, true)
long negs()(long x, ref bool overflow)
{
    if (x == long.min)
        overflow = true;
    return -x;
}

///
@betterC
unittest
{
    bool overflow;
    assert(negs(0L, overflow) == -0);
    assert(!overflow);

    assert(negs(1234L, overflow) == -1234);
    assert(!overflow);

    assert(negs(-5678L, overflow) == 5678);
    assert(!overflow);

    assert(negs(long.min, overflow) == -long.min);
    assert(overflow);

    assert(negs(0L, overflow) == -0);
    assert(overflow);                   // sticky
}

static if (is(cent))
{
/// ditto
pragma(inline, true)
cent negs()(cent x, ref bool overflow)
{
    if (x == cent.min)
        overflow = true;
    return -x;
}

unittest
{
    bool overflow;
    assert(negs(cast(cent)0L, overflow) == -0);
    assert(!overflow);
    assert(negs(cast(cent)1234L, overflow) == -1234);
    assert(!overflow);
    assert(negs(cast(cent)-5678L, overflow) == 5678);
    assert(!overflow);
    assert(negs(cent.min, overflow) == -cent.min);
    assert(overflow);
    assert(negs(cast(cent)0L, overflow) == -0);
    assert(overflow);                   // sticky
}
}


/*******************************
 * Multiply two signed integers, checking for overflow.
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the product
 */

pragma(inline, true)
int muls()(int x, int y, ref bool overflow)
{
    long r = cast(long)x * cast(long)y;
    if (r < int.min || r > int.max)
        overflow = true;
    return cast(int)r;
}

///
@betterC
unittest
{
    bool overflow;
    assert(muls(2, 3, overflow) == 6);
    assert(!overflow);

    assert(muls(-200, 300, overflow) == -60_000);
    assert(!overflow);

    assert(muls(1, int.max, overflow) == int.max);
    assert(!overflow);

    assert(muls(int.min, 1, overflow) == int.min);
    assert(!overflow);

    assert(muls(int.max, 2, overflow) == (int.max * 2));
    assert(overflow);

    overflow = false;
    assert(muls(int.min, -1, overflow) == int.min);
    assert(overflow);

    assert(muls(0, 0, overflow) == 0);
    assert(overflow);                   // sticky
}

/// ditto
pragma(inline, true)
long muls()(long x, long y, ref bool overflow)
{
    immutable long r = cast(ulong)x * cast(ulong)y;
    enum not0or1 = ~1L;
    if ((x & not0or1) &&
        ((r == y) ? r != 0
                  : (r == 0x8000_0000_0000_0000 && x == -1L) || ((r / x) != y)))
        overflow = true;
    return r;
}

///
@betterC
unittest
{
    bool overflow;
    assert(muls(2L, 3L, overflow) == 6);
    assert(!overflow);

    assert(muls(-200L, 300L, overflow) == -60_000);
    assert(!overflow);

    assert(muls(1, long.max, overflow) == long.max);
    assert(!overflow);

    assert(muls(long.min, 1L, overflow) == long.min);
    assert(!overflow);

    assert(muls(long.max, 2L, overflow) == (long.max * 2));
    assert(overflow);
    overflow = false;

    assert(muls(-1L, long.min, overflow) == long.min);
    assert(overflow);

    overflow = false;
    assert(muls(long.min, -1L, overflow) == long.min);
    assert(overflow);

    assert(muls(0L, 0L, overflow) == 0);
    assert(overflow);                   // sticky
}

static if (is(cent))
{
/// ditto
pragma(inline, true)
cent muls()(cent x, cent y, ref bool overflow)
{
    immutable cent r = cast(ucent)x * cast(ucent)y;
    enum not0or1 = ~1L;
    if ((x & not0or1) && ((r == y)? r : (r / x) != y))
        overflow = true;
    return r;
}

unittest
{
    bool overflow;
    assert(muls(cast(cent)2L, 3L, overflow) == 6);
    assert(!overflow);
    assert(muls(cast(cent)-200L, 300L, overflow) == -60_000);
    assert(!overflow);
    assert(muls(1, cent.max, overflow) == cent.max);
    assert(!overflow);
    assert(muls(cent.min, 1L, overflow) == cent.min);
    assert(!overflow);
    assert(muls(cent.max, 2L, overflow) == (cent.max * 2));
    assert(overflow);
    overflow = false;
    assert(muls(-1L, cent.min, overflow) == cent.min);
    assert(overflow);
    overflow = false;
    assert(muls(cent.min, -1L, overflow) == cent.min);
    assert(overflow);
    assert(muls(cast(cent)0L, 0L, overflow) == 0);
    assert(overflow);                   // sticky
}
}


/*******************************
 * Multiply two unsigned integers, checking for overflow (aka carry).
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the product
 */
pragma(inline, true)
uint mulu()(uint x, uint y, ref bool overflow)
{
    immutable ulong r = ulong(x) * ulong(y);
    if (r >> 32)
        overflow = true;
    return cast(uint) r;
}

@betterC
unittest
{
    void test(uint x, uint y, uint r, bool overflow) @nogc nothrow
    {
        bool o;
        assert(mulu(x, y, o) == r);
        assert(o == overflow);
    }
    test(2, 3, 6, false);
    test(1, uint.max, uint.max, false);
    test(0, 1, 0, false);
    test(0, uint.max, 0, false);
    test(uint.max, 2, 2 * uint.max, true);
    test(1 << 16, 1U << 16, 0, true);

    bool overflow = true;
    assert(mulu(0, 0, overflow) == 0);
    assert(overflow);                   // sticky
}

/// ditto
pragma(inline, true)
ulong mulu()(ulong x, uint y, ref bool overflow)
{
    ulong r = x * y;
    if (x >> 32 &&
            r / x != y)
        overflow = true;
    return r;
}

/// ditto
pragma(inline, true)
ulong mulu()(ulong x, ulong y, ref bool overflow)
{
    immutable ulong r = x * y;
    if ((x | y) >> 32 &&
            x &&
            r / x != y)
        overflow = true;
    return r;
}

@betterC
unittest
{
    void test(T, U)(T x, U y, ulong r, bool overflow) @nogc nothrow
    {
        bool o;
        assert(mulu(x, y, o) == r);
        assert(o == overflow);
    }
    // One operand is zero
    test(0, 3, 0, false);
    test(0UL, 3, 0, false);
    test(0UL, 3UL, 0, false);
    test(3, 0, 0, false);
    test(3UL, 0, 0, false);
    test(3UL, 0UL, 0, false);
    // Small numbers
    test(2, 3, 6, false);
    test(2UL, 3, 6, false);
    test(2UL, 3UL, 6, false);
    // At the 32/64 border
    test(1, ulong(uint.max), uint.max, false);
    test(1UL, ulong(uint.max), uint.max, false);
    test(ulong(uint.max), 1, uint.max, false);
    test(ulong(uint.max), 1UL, uint.max, false);
    test(1, 1 + ulong(uint.max), 1 + ulong(uint.max), false);
    test(1UL, 1 + ulong(uint.max), 1 + ulong(uint.max), false);
    test(1 + ulong(uint.max), 1, 1 + ulong(uint.max), false);
    test(1 + ulong(uint.max), 1UL, 1 + ulong(uint.max), false);
    // At the limit
    test(1, ulong.max, ulong.max, false);
    test(1UL, ulong.max, ulong.max, false);
    test(ulong.max, 1, ulong.max, false);
    test(ulong.max, 1UL, ulong.max, false);
    // Miscellaneous
    test(0, 1, 0, false);
    test(0, ulong.max, 0, false);
    test(ulong.max, 2, 2 * ulong.max, true);
    test(1UL << 32, 1UL << 32, 0, true);
    // Must be sticky
    bool overflow = true;
    assert(mulu(0UL, 0UL, overflow) == 0);
    assert(overflow);                   // sticky
}

static if (is(ucent))
{
/// ditto
pragma(inline, true)
ucent mulu()(ucent x, ucent y, ref bool overflow)
{
    immutable ucent r = x * y;
    if (x && (r / x) != y)
        overflow = true;
    return r;
}

unittest
{
    void test(ucent x, ucent y, ucent r, bool overflow) @nogc nothrow
    {
        bool o;
        assert(mulu(x, y, o) == r);
        assert(o == overflow);
    }
    test(2, 3, 6, false);
    test(1, ucent.max, ucent.max, false);
    test(0, 1, 0, false);
    test(0, ucent.max, 0, false);
    test(ucent.max, 2, 2 * ucent.max, true);
    test(cast(ucent)1UL << 64, cast(ucent)1UL << 64, 0, true);

    bool overflow = true;
    assert(mulu(0UL, 0UL, overflow) == 0);
    assert(overflow);                   // sticky
}
}
