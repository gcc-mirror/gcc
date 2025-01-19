// Written in the D programming language
/**
 * Implements a signed 128 bit integer type.
 *
    Author:     Walter Bright
    Copyright:  Copyright (c) 2022, D Language Foundation
    License:    $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0)
    Source:     $(PHOBOSSRC std/int128.d)
 */
module std.int128;

private import core.int128;


/***********************************
 * 128 bit signed integer type.
 */

public struct Int128
{
  @safe pure nothrow @nogc
  {
    Cent data;          /// core.int128.Cent

    /****************
     * Construct an `Int128` from a `long` value.
     * The upper 64 bits are formed by sign extension.
     * Params:
     *  lo = signed lower 64 bits
     */
    this(long lo)
    {
        data.lo = lo;
        data.hi = lo < 0 ? ~0L : 0;
    }

    /****************
     * Construct an `Int128` from a `ulong` value.
     * The upper 64 bits are set to zero.
     * Params:
     *  lo = unsigned lower 64 bits
     */
    this(ulong lo)
    {
        data.lo = lo;
        data.hi = 0;
    }

    /****************
     * Construct an `Int128` from a `long` value.
     * Params:
     *  hi = upper 64 bits
     *  lo = lower 64 bits
     */
    this(long hi, long lo)
    {
        data.hi = hi;
        data.lo = lo;
    }

    /********************
     * Construct an `Int128` from a `Cent`.
     * Params:
     *  data = Cent data
     */
    this(Cent data)
    {
        this.data = data;
    }

    /********************
     * Returns: hash value for Int128
     */
    size_t toHash() const
    {
        return cast(size_t)((data.lo & 0xFFFF_FFFF) + (data.hi & 0xFFFF_FFFF) + (data.lo >> 32) + (data.hi >> 32));
    }

    /************************
     * Compare for equality
     * Params: lo = signed value to compare with
     * Returns: true if Int128 equals value
     */
    bool opEquals(long lo) const
    {
        return data.lo == lo && data.hi == (lo >> 63);
    }

    /************************
     * Compare for equality
     * Params: lo = unsigned value to compare with
     * Returns: true if Int128 equals value
     */
    bool opEquals(ulong lo) const
    {
        return data.hi == 0 && data.lo == lo;
    }

    /************************
     * Compare for equality
     * Params: op2 = value to compare with
     * Returns: true if Int128 equals value
     */
    bool opEquals(Int128 op2) const
    {
        return data.hi == op2.data.hi && data.lo == op2.data.lo;
    }

    /** Support unary arithmentic operator +
     * Params: op = "+"
     * Returns: lvalue of result
     */
    Int128 opUnary(string op)() const
    if (op == "+")
    {
        return this;
    }

    /** Support unary arithmentic operator - ~
     * Params: op = "-", "~"
     * Returns: lvalue of result
     */
    Int128 opUnary(string op)() const
    if (op == "-" || op == "~")
    {
        static if (op == "-")
            return Int128(neg(this.data));
        else static if (op == "~")
            return Int128(com(this.data));
    }

    /** Support unary arithmentic operator ++ --
     * Params: op = "++", "--"
     * Returns: lvalue of result
     */
    Int128 opUnary(string op)()
    if (op == "++" || op == "--")
    {
        static if (op == "++")
            this.data = inc(this.data);
        else static if (op == "--")
            this.data = dec(this.data);
        else
            static assert(0, op);
        return this;
    }

    /** Support casting to a bool
     * Params: T = bool
     * Returns: true if value is not zero
     */
    bool opCast(T : bool)() const
    {
        return tst(this.data);
    }
  } // @safe pure nothrow @nogc

    /** Support casting to an integral type
     * Params: T = integral type
     * Returns: low bits of value reinterpreted as T
     */
    T opCast(T : long)() const
    if (is(byte : T))
    {
        return cast(T) this.data.lo;
    }

    ///
    @safe unittest
    {
        const Int128 a = Int128(0xffff_ffff_ffff_ffffL, 0x0123_4567_89ab_cdefL);
        assert(cast(long) a == 0x0123_4567_89ab_cdefL);
        assert(cast(int)  a ==           0x89ab_cdef);
        assert(cast(byte) a == cast(byte) 0xef);
    }

    /** Support casting to floating point type
     * Params: T = floating point type
     * Returns: value cast to floating point with environment-dependent
     * rounding if the value is not exactly representable
     */
    T opCast(T : real)() const
    {
        import core.math : ldexp;
        if (cast(long) this.data.hi >= 0)
            return ldexp(cast(T) this.data.hi, 64) + this.data.lo;
        else
        {
            const absData = neg(this.data);
            return -ldexp(cast(T) absData.hi, 64) - absData.lo;
        }
    }

    ///
    @safe unittest
    {
        const Int128 a = Int128(-1L << 60);
        assert(cast(double) a == -(2.0 ^^ 60));
        assert(cast(double) (a * a) == 2.0 ^^ 120);
    }

    /** Support binary arithmetic operators + - * / % & | ^ << >> >>>
     * Params:
     *   op = one of the arithmetic binary operators
     *   op2 = second operand
     * Returns: value after the operation is applied
     */
    Int128 opBinary(string op)(Int128 op2) const
    if (op == "+" || op == "-" ||
        op == "*" || op == "/" || op == "%" ||
        op == "&" || op == "|" || op == "^")
    {
        static if (op == "+")
            return Int128(add(this.data, op2.data));
        else static if (op == "-")
            return Int128(sub(this.data, op2.data));
        else static if (op == "*")
            return Int128(mul(this.data, op2.data));
        else static if (op == "/")
            return Int128(div(this.data, op2.data));
        else static if (op == "%")
        {
            Cent modulus;
            divmod(this.data, op2.data, modulus);
            return Int128(modulus);
        }
        else static if (op == "&")
            return Int128(and(this.data, op2.data));
        else static if (op == "|")
            return Int128(or(this.data, op2.data));
        else static if (op == "^")
            return Int128(xor(this.data, op2.data));
        else
            static assert(0, "wrong op value");
    }

    /// ditto
    Int128 opBinary(string op, Int)(const Int op2) const
    if ((op == "+" || op == "-" ||
        op == "*" || op == "/" || op == "%" ||
        op == "&" || op == "|" || op == "^") &&
        is(Int : long) && __traits(isIntegral, Int))
    {
        static if (__traits(isUnsigned, Int))
            return mixin("this " ~ op ~ " Int128(0, op2)");
        else
            return mixin("this " ~ op ~ " Int128((cast(long) op2) >> 63 , op2)");
    }

    /// ditto
    Int128 opBinary(string op, IntLike)(auto ref IntLike op2) const
    if ((op == "+" || op == "-" ||
        op == "*" || op == "/" || op == "%" ||
        op == "&" || op == "|" || op == "^") &&
        is(IntLike : long) && !__traits(isIntegral, IntLike))
    {
        return opBinary!(op)(__traits(getMember, op2, __traits(getAliasThis, IntLike)[0]));
    }

    /// ditto
    Int128 opBinaryRight(string op, Int)(const Int op2) const
    if ((op == "+" || op == "-" ||
        op == "*" || op == "/" || op == "%" ||
        op == "&" || op == "|" || op == "^") &&
        is(Int : long) && __traits(isIntegral, Int))
    {
        static if (__traits(isUnsigned, Int))
            mixin("return Int128(0, op2) " ~ op ~ " this;");
        else
            mixin("return Int128((cast(long) op2) >> 63, op2) " ~ op ~ " this;");
    }

    /// ditto
    Int128 opBinaryRight(string op, IntLike)(auto ref IntLike op2) const
    if ((op == "+" || op == "-" ||
        op == "*" || op == "/" || op == "%" ||
        op == "&" || op == "|" || op == "^") &&
        is(IntLike : long) && !__traits(isIntegral, IntLike))
    {
        return opBinaryRight!(op)(__traits(getMember, op2, __traits(getAliasThis, IntLike)[0]));
    }

    /// ditto
    Int128 opBinary(string op)(long op2) const
    if (op == "<<")
    {
        return Int128(shl(this.data, cast(uint) op2));
    }

    /// ditto
    Int128 opBinary(string op)(long op2) const
    if (op == ">>")
    {
        return Int128(sar(this.data, cast(uint) op2));
    }

    /// ditto
    Int128 opBinary(string op)(long op2) const
    if (op == ">>>")
    {
        return Int128(shr(this.data, cast(uint) op2));
    }

    /** arithmetic assignment operators += -= *= /= %= &= |= ^= <<= >>= >>>=
     * Params: op = one of +, -, etc.
     *   op2 = second operand
     * Returns: lvalue of updated left operand
     */
    ref Int128 opOpAssign(string op)(Int128 op2)
    if (op == "+" || op == "-" ||
        op == "*" || op == "/" || op == "%" ||
        op == "&" || op == "|" || op == "^" ||
        op == "<<" || op == ">>" || op == ">>>")
    {
        mixin("this = this " ~ op ~ " op2;");
        return this;
    }

    /// ditto
    ref Int128 opOpAssign(string op, Int)(auto ref Int op2)
    if ((op == "+" || op == "-" ||
        op == "*" || op == "/" || op == "%" ||
        op == "&" || op == "|" || op == "^" ||
        op == "<<" || op == ">>" || op == ">>>")
        && is(Int : long))
    {
        mixin("this = this " ~ op ~ " op2;");
        return this;
    }

    /** support arithmentic comparison operators < <= > >=
     * Params: op2 = right hand operand
     * Returns: -1 for less than, 0 for equals, 1 for greater than
     */
    int opCmp(Int128 op2) const @nogc nothrow pure @safe
    {
        return this == op2 ? 0 : gt(this.data, op2.data) * 2 - 1;
    }

    /// ditto
    int opCmp(Int)(const Int op2) const @nogc nothrow pure @safe
    if (is(Int : long) && __traits(isIntegral, Int))
    {
        static if (__traits(isUnsigned, Int))
            return opCmp(Int128(0, op2));
        else
            return opCmp(Int128((cast(long) op2) >> 63, op2));
    }

    /// ditto
    int opCmp(IntLike)(auto ref IntLike op2) const
    if (is(IntLike : long) && !__traits(isIntegral, IntLike))
    {
        return opCmp(__traits(getMember, op2, __traits(getAliasThis, IntLike)[0]));
    }

    /**
     * Formats `Int128` with either `%d`, `%x`, `%X`, or `%s` (same as `%d`).
     *
     * Params:
     *   sink = $(REF_ALTTEXT Output range, isOutputRange, std, range, primitives)
     *   to write to.
     *   fmt = A $(REF FormatSpec, std,format) which controls how the number
     *   is displayed.
     *
     * Throws:
     *       $(REF FormatException, std,format) if the format specifier is
     *       not one of 'd', 'x', 'X', 's'.
     *
     * See_Also: $(REF formatValue, std,format)
     */
    void toString(Writer, FormatSpec)(scope ref Writer sink, scope const ref FormatSpec fmt) const
    {
        import std.range.primitives : put;
        import std.format : FormatException, Fmt = FormatSpec;

        static if (is(FormatSpec == Fmt!Char, Char))
        {
            // Puts "Char" into scope if the pattern matches.
        }
        static assert(is(Char),
            "Expecting `FormatSpec` to be instantiation of `std.format.FormatSpec`");

        Char[39] buf = void;
        size_t bufStart = void;
        Char signChar = 0;
        if (fmt.spec == 'd' || fmt.spec == 's')
        {
            const bool isNeg = 0 > cast(long) this.data.hi;
            Cent val = isNeg ? neg(this.data) : this.data;
            immutable Cent radix = { lo: 10, hi: 0 };
            Cent modulus;
            bufStart = buf.length;
            do
            {
                uint x = void;
                if (ugt(radix, val))
                {
                    x = cast(uint) val.lo;
                    val = Cent(0, 0);
                }
                else
                {
                    val = udivmod(val, radix, modulus);
                    x = cast(uint) modulus.lo;
                }
                buf[--bufStart] = cast(Char) ('0' + x);
            } while (tst(val));
            if (isNeg)
                signChar = '-';
            else if (fmt.flPlus)
                signChar = '+';
            else if (fmt.flSpace)
                signChar = ' ';
        }
        else if (fmt.spec == 'x' || fmt.spec == 'X')
        {
            immutable hexDigits = fmt.spec == 'X' ? "0123456789ABCDEF" : "0123456789abcdef";
            ulong a = data.lo;
            bufStart = buf.length - 1;
            size_t penPos = buf.length - 1;
            do
            {
                if ((buf[penPos] = hexDigits[0xF & cast(uint) a]) != '0')
                    bufStart = penPos;
                a >>>= 4;
            } while (--penPos >= buf.length - 16);
            a = data.hi;
            do
            {
                if ((buf[penPos] = hexDigits[0xF & cast(uint) a]) != '0')
                    bufStart = penPos;
                a >>>= 4;
            } while (--penPos >= buf.length - 32);
        }
        else
        {
            throw new FormatException("Format specifier not understood: %" ~ fmt.spec);
        }

        const minw = (buf.length - bufStart) + int(signChar != 0);
        const maxw = minw < fmt.width ? fmt.width : minw;
        const difw = maxw - minw;

        static void putRepeatedChars(Char c)(scope ref Writer sink, size_t n)
        {
            static immutable Char[8] array = [c, c, c, c, c, c, c, c];
            foreach (_; 0 .. n / 8)
                put(sink, array[0 .. 8]);
            if (n & 7)
                put(sink, array[0 .. n & 7]);
        }

        if (!fmt.flDash && !fmt.flZero && difw)
            putRepeatedChars!' '(sink, difw);

        if (signChar)
        {
            Char[1] signCharBuf = signChar;
            put(sink, signCharBuf[0 .. 1]);
        }

        if (!fmt.flDash && fmt.flZero && difw)
            putRepeatedChars!'0'(sink, difw);

        put(sink, buf[bufStart .. $]);

        if (fmt.flDash && difw)
            putRepeatedChars!' '(sink, difw);
    }

    /**
        `toString` is rarely directly invoked; the usual way of using it is via
        $(REF format, std, format):
     */
    @safe unittest
    {
        import std.format : format;

        assert(format("%s", Int128.max) == "170141183460469231731687303715884105727");
        assert(format("%s", Int128.min) == "-170141183460469231731687303715884105728");
        assert(format("%x", Int128.max) == "7fffffffffffffffffffffffffffffff");
        assert(format("%X", Int128.max) == "7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
        assert(format("%032X", Int128(123L)) == "0000000000000000000000000000007B");
        assert(format("%+ 40d", Int128(123L)) == "                                    +123");
        assert(format("%+-40d", Int128(123L)) == "+123                                    ");
    }

    /// Also can format as `wchar` or `dchar`.
    @safe unittest
    {
        import std.conv : to;

        assert(to!wstring(Int128.max) == "170141183460469231731687303715884105727"w);
        assert(to!dstring(Int128.max) == "170141183460469231731687303715884105727"d);
    }

    enum min = Int128(long.min, 0);             /// minimum value
    enum max = Int128(long.max, ulong.max);     /// maximum value
}

/********************************************* Tests ************************************/

version (unittest)
{
import core.stdc.stdio;

@trusted void print(Int128 c)
{
    printf("%lld, %lld\n", c.data.hi, c.data.lo);
}

@trusted void printx(Int128 c)
{
    printf("%llx, %llx\n", c.data.hi, c.data.lo);
}
}

/// Int128 tests
@safe pure nothrow @nogc
unittest
{
    Int128 c = Int128(5, 6);
    assert(c == c);
    assert(c == +c);
    assert(c == - -c);
    assert(~c == Int128(~5, ~6));
    ++c;
    assert(c == Int128(5, 7));
    assert(--c == Int128(5, 6));
    assert(!!c);
    assert(!Int128());

    assert(c + Int128(10, 20) == Int128(15, 26));
    assert(c - Int128(1, 2)   == Int128(4, 4));
    assert(c * Int128(100, 2) == Int128(610, 12));
    assert(c / Int128(3, 2)   == Int128(0, 1));
    assert(c % Int128(3, 2)   == Int128(2, 4));
    assert((c & Int128(3, 2)) == Int128(1, 2));
    assert((c | Int128(3, 2)) == Int128(7, 6));
    assert((c ^ Int128(3, 2)) == Int128(6, 4));

    assert(c + 15   == Int128(5, 21));
    assert(c - 15   == Int128(4, -9));
    assert(c * 15   == Int128(75, 90));
    assert(c / 15   == Int128(0, 6148914691236517205));
    assert(c % 15   == Int128(0, 11));
    assert((c & 15) == Int128(0, 6));
    assert((c | 15) == Int128(5, 15));
    assert((c ^ 15) == Int128(5, 9));

    assert(15 + c   == Int128(5, 21));
    assert(15 - c   == Int128(-5, 9));
    assert(15 * c   == Int128(75, 90));
    assert(15 / c   == Int128(0, 0));
    assert(15 % c   == Int128(0, 15));
    assert((15 & c) == Int128(0, 6));
    assert((15 | c) == Int128(5, 15));
    assert((15 ^ c) == Int128(5, 9));

    assert(c << 1 == Int128(10, 12));
    assert(-c >> 1 == Int128(-3, 9223372036854775805));
    assert(-c >>> 1 == Int128(9223372036854775805, 9223372036854775805));

    assert((c += 1) == Int128(5, 7));
    assert((c -= 1) == Int128(5, 6));
    assert((c += Int128(0, 1)) == Int128(5, 7));
    assert((c -= Int128(0, 1)) == Int128(5, 6));
    assert((c *= 2) == Int128(10, 12));
    assert((c /= 2) == Int128(5, 6));
    assert((c %= 2) == Int128());
    c += Int128(5, 6);
    assert((c *= Int128(10, 20)) == Int128(160, 120));
    assert((c /= Int128(10, 20)) == Int128(0, 15));
    c += Int128(72, 0);
    assert((c %= Int128(10, 20)) == Int128(1, -125));
    assert((c &= Int128(3, 20)) == Int128(1, 0));
    assert((c |= Int128(8, 2)) == Int128(9, 2));
    assert((c ^= Int128(8, 2)) == Int128(1, 0));
    c |= Int128(10, 5);
    assert((c <<= 1) == Int128(11 * 2, 5 * 2));
    assert((c >>>= 1) == Int128(11, 5));
    c = Int128(long.min, long.min);
    assert((c >>= 1) == Int128(long.min >> 1, cast(ulong) long.min >> 1));

    assert(-Int128.min == Int128.min);
    assert(Int128.max + 1 == Int128.min);

    c = Int128(5, 6);
    assert(c < Int128(6, 5));
    assert(c > 10);

    c = Int128(-1UL);
    assert(c == -1UL);
    c = Int128(-1L);
    assert(c == -1L);
}

@system unittest
{
    alias Seq(T...) = T;
    Int128 c = Int128(-1L);
    assert(c.opCmp(-1L) == 0);
    // To avoid regression calling opCmp with any integral type needs to
    // work without the compiler complaining "opCmp called with argument
    // X matches both <...>".
    static foreach (Int; Seq!(long, int, short, byte, ulong, uint, ushort, ubyte, dchar, wchar, char))
        assert(c < Int.max);
    static foreach (Int; Seq!(int, short, byte))
        assert(c.opCmp(Int(-1)) == 0);
    assert(c < true);
    // To avoid regression calling opCmp with any type that converts to an
    // integral type through alias this needs to work regardless of whether
    // the alias is safe/pure/nothrow/nogc and regardless of whether the
    // type has a disabled postblit.
    static struct Wrapped(T)
    {
        T value;
        uint count;
        T get() @system { ++count; return value; } // not const
        alias get this;
        @disable this(this); // no implicit copies
    }
    assert(c.opCmp(Wrapped!long(-1)) == 0);
    auto w = Wrapped!ulong(ulong.max);
    w.count++; // avoid invalid D-Scanner message that w could have been declared const
    assert(c < w);

    const zero = Int128(0L);
    const one = Int128(1L);
    const neg_one = Int128(-1L);
    const neg_two = Int128(-2L);
    // Correct result with ulong.max:
    assert(zero + ulong.max == ulong.max);
    assert(one * ulong.max == ulong.max);
    assert((neg_one & ulong.max) == ulong.max);
    assert((zero | ulong.max) == ulong.max);
    assert((zero ^ ulong.max) == ulong.max);
    // Correct result with negative arguments:
    assert(zero + -1L == -1L);
    assert(neg_two * -3L == 6L);
    assert(neg_two / -2L == 1L);
    assert(neg_two % -2L == 0L);
    assert((neg_one & -1L) == -1L);
    assert((zero | -1L) == -1L);
    assert((zero ^ -1L) == -1L);
    // Ensure alias this still works.
    {
        Int128 a = zero;
        assert((a ^= w) == ulong.max);
    }
    assert((Wrapped!long(-1L) + zero) == -1L);
}
