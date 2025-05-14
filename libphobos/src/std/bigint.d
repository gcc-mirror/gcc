/** Arbitrary-precision ('bignum') arithmetic.
 *
 * Performance is optimized for numbers below ~1000 decimal digits.
 * For X86 machines, highly optimised assembly routines are used.
 *
 * The following algorithms are currently implemented:
 * $(UL
 * $(LI Karatsuba multiplication)
 * $(LI Squaring is optimized independently of multiplication)
 * $(LI Divide-and-conquer division)
 * $(LI Binary exponentiation)
 * )
 *
 * For very large numbers, consider using the $(HTTP gmplib.org, GMP library) instead.
 *
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Don Clugston
 * Source: $(PHOBOSSRC std/bigint.d)
 */
/*          Copyright Don Clugston 2008 - 2010.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */

module std.bigint;

import std.conv : ConvException;

import std.format.spec : FormatSpec;
import std.format : FormatException;
import std.internal.math.biguintcore;
import std.internal.math.biguintnoasm : BigDigit;
import std.range.primitives;
import std.traits;

/** A struct representing an arbitrary precision integer.
 *
 * All arithmetic operations are supported, except unsigned shift right (`>>>`).
 * Bitwise operations (`|`, `&`, `^`, `~`) are supported, and behave as if BigInt was
 * an infinite length 2's complement number.
 *
 * BigInt implements value semantics using copy-on-write. This means that
 * assignment is cheap, but operations such as x++ will cause heap
 * allocation. (But note that for most bigint operations, heap allocation is
 * inevitable anyway.)
 */
struct BigInt
{
private:
    BigUint data;     // BigInt adds signed arithmetic to BigUint.
    bool sign = false;
public:
    /**
     * Construct a `BigInt` from a decimal or hexadecimal string. The number must
     * be in the form of a decimal or hex literal. It may have a leading `+`
     * or `-` sign, followed by `0x` or `0X` if hexadecimal. Underscores are
     * permitted in any location after the `0x` and/or the sign of the number.
     *
     * Params:
     *     s = a finite bidirectional range of any character type
     *
     * Throws:
     *     $(REF ConvException, std,conv) if the string doesn't represent a valid number
     */
    this(Range)(Range s)
    if (isBidirectionalRange!Range &&
        isSomeChar!(ElementType!Range) &&
        !isInfinite!Range &&
        !isNarrowString!Range)
    {
        import std.algorithm.iteration : filterBidirectional;
        import std.algorithm.searching : startsWith;
        import std.conv : ConvException;
        import std.exception : enforce;
        import std.utf : byChar;

        enforce!ConvException(!s.empty, "Can't initialize BigInt with an empty range");

        bool neg = false;
        bool ok;

        data = 0UL;

        // check for signs and if the string is a hex value
        if (s.front == '+')
        {
            s.popFront(); // skip '+'
        }
        else if (s.front == '-')
        {
            neg = true;
            s.popFront();
        }

        if (s.save.startsWith("0x".byChar) ||
            s.save.startsWith("0X".byChar))
        {
            s.popFront;
            s.popFront;

            if (!s.empty)
                ok = data.fromHexString(s.filterBidirectional!(a => a != '_'));
            else
                ok = false;
        }
        else
        {
            ok = data.fromDecimalString(s.filterBidirectional!(a => a != '_'));
        }

        enforce!ConvException(ok, "Not a valid numerical string");

        if (isZero())
            neg = false;

        sign = neg;
    }

    /// ditto
    this(Range)(Range s) pure
    if (isNarrowString!Range)
    {
        import std.utf : byCodeUnit;
        this(s.byCodeUnit);
    }

    @safe unittest
    {
        // system because of the dummy ranges eventually call std.array!string
        import std.exception : assertThrown;
        import std.internal.test.dummyrange;

        auto r1 = new ReferenceBidirectionalRange!dchar("101");
        auto big1 = BigInt(r1);
        assert(big1 == BigInt(101));

        auto r2 = new ReferenceBidirectionalRange!dchar("1_000");
        auto big2 = BigInt(r2);
        assert(big2 == BigInt(1000));

        auto r3 = new ReferenceBidirectionalRange!dchar("0x0");
        auto big3 = BigInt(r3);
        assert(big3 == BigInt(0));

        auto r4 = new ReferenceBidirectionalRange!dchar("0x");
        assertThrown!ConvException(BigInt(r4));
    }

    /**
     * Construct a `BigInt` from a sign and a magnitude.
     *
     * The magnitude is an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
     * of unsigned integers that satisfies either $(REF hasLength, std,range,primitives)
     * or $(REF isForwardRange, std,range,primitives). The first (leftmost)
     * element of the magnitude is considered the most significant.
     *
     * Params:
     *     isNegative = true for negative, false for non-negative
     *          (ignored when magnitude is zero)
     *     magnitude = a finite range of unsigned integers
     */
    this(Range)(bool isNegative, Range magnitude)
    if (isInputRange!Range &&
        isUnsigned!(ElementType!Range) &&
        (hasLength!Range || isForwardRange!Range) &&
        !isInfinite!Range)
    {
        data.fromMagnitude(magnitude);
        sign = isNegative && !data.isZero;
    }

    ///
    pure @safe unittest
    {
        ubyte[] magnitude = [1, 2, 3, 4, 5, 6];
        auto b1 = BigInt(false, magnitude);
        assert(cast(long) b1 == 0x01_02_03_04_05_06L);
        auto b2 = BigInt(true, magnitude);
        assert(cast(long) b2 == -0x01_02_03_04_05_06L);
    }

    /// Construct a `BigInt` from a built-in integral type.
    this(T)(T x) pure nothrow @safe
    if (isIntegral!T)
    {
        data = data.init; // @@@: Workaround for compiler bug
        opAssign(x);
    }

    ///
    @safe unittest
    {
        ulong data = 1_000_000_000_000;
        auto bigData = BigInt(data);
        assert(bigData == BigInt("1_000_000_000_000"));
    }

    /// Construct a `BigInt` from another `BigInt`.
    this(T)(T x) pure nothrow @safe
    if (is(immutable T == immutable BigInt))
    {
        opAssign(x);
    }

    ///
    @safe unittest
    {
        const(BigInt) b1 = BigInt("1_234_567_890");
        BigInt b2 = BigInt(b1);
        assert(b2 == BigInt("1_234_567_890"));
    }

    /// Assignment from built-in integer types.
    BigInt opAssign(T)(T x) pure nothrow @safe
    if (isIntegral!T)
    {
        data = cast(ulong) absUnsign(x);
        sign = (x < 0);
        return this;
    }

    ///
    @safe unittest
    {
        auto b = BigInt("123");
        b = 456;
        assert(b == BigInt("456"));
    }

    /// Assignment from another BigInt.
    BigInt opAssign(T:BigInt)(T x) pure @nogc @safe
    {
        data = x.data;
        sign = x.sign;
        return this;
    }

    ///
    @safe unittest
    {
        auto b1 = BigInt("123");
        auto b2 = BigInt("456");
        b2 = b1;
        assert(b2 == BigInt("123"));
    }

    /**
     * Implements assignment operators from built-in integers of the form
     * `BigInt op= integer`.
     */
    BigInt opOpAssign(string op, T)(T y) pure nothrow @safe return scope
    if ((op=="+" || op=="-" || op=="*" || op=="/" || op=="%"
        || op==">>" || op=="<<" || op=="^^" || op=="|" || op=="&" || op=="^") && isIntegral!T)
    {
        ulong u = absUnsign(y);

        static if (op=="+")
        {
            data = BigUint.addOrSubInt!ulong(data, u, wantSub: sign != (y<0), sign);
        }
        else static if (op=="-")
        {
            data = BigUint.addOrSubInt!ulong(data, u, wantSub: sign == (y<0), sign);
        }
        else static if (op=="*")
        {
            if (y == 0 || data.isZero())
            {
                sign = false;
                data = 0UL;
                return this;
            }
            else
            {
                sign = ( sign != (y<0) );
                data = BigUint.mulInt(data, u);
            }
        }
        else static if (op=="/")
        {
            assert(y != 0, "Division by zero");
            static if (T.sizeof <= uint.sizeof)
            {
                data = BigUint.divInt(data, cast(uint) u);
            }
            else
            {
                data = BigUint.divInt(data, u);
            }
            sign = data.isZero() ? false : sign ^ (y < 0);
        }
        else static if (op=="%")
        {
            assert(y != 0, "Division by zero");
            static if (is(immutable(T) == immutable(long)) || is( immutable(T) == immutable(ulong) ))
            {
                this %= BigInt(y);
            }
            else
            {
                data = cast(ulong) BigUint.modInt(data, cast(uint) u);
                if (data.isZero())
                    sign = false;
            }
            // x%y always has the same sign as x.
            // This is not the same as mathematical mod.
        }
        else static if (op==">>" || op=="<<")
        {
            // Do a left shift if y>0 and <<, or
            // if y<0 and >>; else do a right shift.
            if (y == 0)
                return this;
            else if ((y > 0) == (op=="<<"))
            {
                // Sign never changes during left shift
                data = data.opBinary!(op)(u);
            }
            else
            {
                data = data.opBinary!(op)(u);
                if (data.isZero())
                    sign = false;
            }
        }
        else static if (op=="^^")
        {
            sign = (y & 1) ? sign : false;
            if (y < 0)
            {
                checkDivByZero();
                data = cast(ulong) (data == 1);
            }
            else
            {
                data = BigUint.pow(data, u);
            }
        }
        else static if (op=="&")
        {
            if (y >= 0 && (y <= 1 || !sign)) // In these cases we can avoid some allocation.
            {
                static if (T.sizeof <= uint.sizeof && BigDigit.sizeof <= uint.sizeof)
                    data = cast(ulong) data.peekUint(0) & y;
                else
                    data = data.peekUlong(0) & y;
                sign = false;
            }
            else
            {
                BigInt b = y;
                opOpAssign!op(b);
            }
        }
        else static if (op=="|" || op=="^")
        {
            BigInt b = y;
            opOpAssign!op(b);
        }
        else static assert(0, "BigInt " ~ op[0..$-1] ~ "= " ~ T.stringof ~ " is not supported");
        return this;
    }

    // https://issues.dlang.org/show_bug.cgi?id=10565
@safe unittest
{
    // Test cases from the issue
    BigInt a = BigInt("0");
    BigInt b = BigInt("-0");
    BigInt c = BigInt("0") * -1;
    BigInt d = BigInt("0") * -42;
    BigInt e = BigInt("0"); e *= -1;
    BigInt f = BigInt(c);
    BigInt g = BigInt("0") * cast(byte) -1;
    BigInt h = BigInt("0"); h *= BigInt("-1");
    BigInt i = BigInt("0"); i -= 2 * i;
    BigInt j = BigInt("0"); j = -j;
    // All of these should be zero and not negative
    auto values = [a, b, c, d, e, f, g, h, i, j];
    foreach (val; values)
    {
        assert(val == 0, "BigInt value should be equal to zero");
        assert(!(val < 0), "BigInt zero should not be negative");
    }
}

    ///
    @safe unittest
    {
        auto b = BigInt("1_000_000_000");

        b += 12345;
        assert(b == BigInt("1_000_012_345"));

        b /= 5;
        assert(b == BigInt("200_002_469"));
    }

    // https://issues.dlang.org/show_bug.cgi?id=16264
    @safe unittest
    {
        auto a = BigInt(
    `335690982744637013564796917901053301979460129353374296317539383938630086938` ~
    `465898213033510992292836631752875403891802201862860531801760096359705447768` ~
    `957432600293361240407059207520920532482429912948952142341440301429494694368` ~
    `264560802292927144211230021750155988283029753927847924288850436812178022006` ~
    `408597793414273953252832688620479083497367463977081627995406363446761896298` ~
    `967177607401918269561385622811274398143647535024987050366350585544531063531` ~
    `7118554808325723941557169427279911052268935775`);

        auto b = BigInt(
    `207672245542926038535480439528441949928508406405023044025560363701392340829` ~
    `852529131306106648201340460604257466180580583656068555417076345439694125326` ~
    `843947164365500055567495554645796102453565953360564114634705366335703491527` ~
    `429426780005741168078089657359833601261803592920462081364401456331489106355` ~
    `199133982282631108670436696758342051198891939367812305559960349479160308314` ~
    `068518200681530999860641597181672463704794566473241690395901768680673716414` ~
    `243691584391572899147223065906633310537507956952626106509069491302359792769` ~
    `378934570685117202046921464019396759638376362935855896435623442486036961070` ~
    `534574698959398017332214518246531363445309522357827985468581166065335726996` ~
    `711467464306784543112544076165391268106101754253962102479935962248302404638` ~
    `21737237102628470475027851189594709504`);

        BigInt c = a * b;  // Crashes

        assert(c == BigInt(
    `697137001950904057507249234183127244116872349433141878383548259425589716813` ~
    `135440660252012378417669596912108637127036044977634382385990472429604619344` ~
    `738746224291111527200379708978133071390303850450970292020176369525401803474` ~
    `998613408923490273129022167907826017408385746675184651576154302536663744109` ~
    `111018961065316024005076097634601030334948684412785487182572502394847587887` ~
    `507385831062796361152176364659197432600147716058873232435238712648552844428` ~
    `058885217631715287816333209463171932255049134340904981280717725999710525214` ~
    `161541960645335744430049558161514565159449390036287489478108344584188898872` ~
    `434914159748515512161981956372737022393466624249130107254611846175580584736` ~
    `276213025837422102290580044755202968610542057651282410252208599309841499843` ~
    `672251048622223867183370008181364966502137725166782667358559333222947265344` ~
    `524195551978394625568228658697170315141077913403482061673401937141405425042` ~
    `283546509102861986303306729882186190883772633960389974665467972016939172303` ~
    `653623175801495207204880400522581834672918935651426160175413277309985678579` ~
    `830872397214091472424064274864210953551447463312267310436493480881235642109` ~
    `668498742629676513172286703948381906930297135997498416573231570483993847269` ~
    `479552708416124555462530834668011570929850407031109157206202741051573633443` ~
    `58105600`
        ));
    }

    // https://issues.dlang.org/show_bug.cgi?id=24028
    @system unittest
    {
        import std.exception : assertThrown;
        import core.exception : AssertError;

        assert(BigInt(100) ^^ -1 == BigInt(0));
        assert(BigInt(1) ^^ -1 == BigInt(1));
        assert(BigInt(-1) ^^ -1 == BigInt(-1));
        assert(BigInt(-1) ^^ -2 == BigInt(1));
        assertThrown!AssertError(BigInt(0) ^^ -1);
    }

    /**
     * Implements assignment operators of the form `BigInt op= BigInt`.
     */
    BigInt opOpAssign(string op, T)(T y) pure nothrow @safe return scope
    if ((op=="+" || op== "-" || op=="*" || op=="|" || op=="&" || op=="^" || op=="/" || op=="%") && is (T: BigInt))
    {
        static if (op == "+")
        {
            data = BigUint.addOrSub(data, y.data, sign != y.sign, sign);
        }
        else static if (op == "-")
        {
            data = BigUint.addOrSub(data, y.data, sign == y.sign, sign);
        }
        else static if (op == "*")
        {
            data = BigUint.mul(data, y.data);
            sign = isZero() ? false : sign ^ y.sign;
        }
        else static if (op == "/")
        {
            y.checkDivByZero();
            if (!isZero())
            {
                data = BigUint.div(data, y.data);
                sign = isZero() ? false : sign ^ y.sign;
            }
        }
        else static if (op == "%")
        {
            y.checkDivByZero();
            if (!isZero())
            {
                data = BigUint.mod(data, y.data);
                // x%y always has the same sign as x.
                if (isZero())
                    sign = false;
            }
        }
        else static if (op == "|" || op == "&" || op == "^")
        {
            data = BigUint.bitwiseOp!op(data, y.data, sign, y.sign, sign);
        }
        else static assert(0, "BigInt " ~ op[0..$-1] ~ "= " ~
            T.stringof ~ " is not supported");
        return this;
    }

    ///
    @safe unittest
    {
        auto x = BigInt("123");
        auto y = BigInt("321");
        x += y;
        assert(x == BigInt("444"));
    }

    /**
     * Implements binary operators between `BigInt`s.
     */
    BigInt opBinary(string op, T)(T y) pure nothrow @safe const return scope
    if ((op=="+" || op == "*" || op=="-" || op=="|" || op=="&" || op=="^" ||
        op=="/" || op=="%") && is (T: BigInt))
    {
        BigInt r = this;
        return r.opOpAssign!(op)(y);
    }

    ///
    @safe unittest
    {
        auto x = BigInt("123");
        auto y = BigInt("456");
        BigInt z = x * y;
        assert(z == BigInt("56088"));
    }

    /**
     * Implements binary operators between `BigInt`'s and built-in integers.
     */
    BigInt opBinary(string op, T)(T y) pure nothrow @safe const return scope
    if ((op=="+" || op == "*" || op=="-" || op=="/" || op=="|" || op=="&" ||
        op=="^"|| op==">>" || op=="<<" || op=="^^")
        && isIntegral!T)
    {
        BigInt r = this;
        r.opOpAssign!(op)(y);
        return r;
    }

    ///
    @safe unittest
    {
        auto x = BigInt("123");
        x *= 300;
        assert(x == BigInt("36900"));
    }

    /**
        Implements a narrowing remainder operation with built-in integer types.

        This binary operator returns a narrower, built-in integer type
        where applicable, according to the following table.

        $(TABLE ,
        $(TR $(TD `BigInt`) $(TD $(CODE_PERCENT)) $(TD `uint`) $(TD $(RARR)) $(TD `long`))
        $(TR $(TD `BigInt`) $(TD $(CODE_PERCENT)) $(TD `long`) $(TD $(RARR)) $(TD `long`))
        $(TR $(TD `BigInt`) $(TD $(CODE_PERCENT)) $(TD `ulong`) $(TD $(RARR)) $(TD `BigInt`))
        $(TR $(TD `BigInt`) $(TD $(CODE_PERCENT)) $(TD other type) $(TD $(RARR)) $(TD `int`))
        )
     */
    auto opBinary(string op, T)(T y) pure nothrow @safe const
    if (op == "%" && isIntegral!T)
    {
        assert(y != 0, "% 0 not allowed");

        // BigInt % uint => long
        // BigInt % long => long
        // BigInt % ulong => BigInt
        // BigInt % other_type => int
        static if (is(immutable T == immutable long) || is(immutable T == immutable ulong))
        {
            auto r = this % BigInt(y);

            static if (is(immutable T == immutable long))
            {
                return r.toLong();
            }
            else
            {
                // return as-is to avoid overflow
                return r;
            }
        }
        else
        {
            immutable uint u = absUnsign(y);
            static if (is(immutable T == immutable uint))
               alias R = long;
            else
               alias R = int;
            R rem = BigUint.modInt(data, u);
            // x%y always has the same sign as x.
            // This is not the same as mathematical mod.
            return sign ? -rem : rem;
        }
    }

    ///
    @safe unittest
    {
        auto  x  = BigInt("1_000_000_500");
        long  l  = 1_000_000L;
        ulong ul = 2_000_000UL;
        int   i  = 500_000;
        short s  = 30_000;

        assert(is(typeof(x % l)  == long)   && x % l  == 500L);
        assert(is(typeof(x % ul) == BigInt) && x % ul == BigInt(500));
        assert(is(typeof(x % i)  == int)    && x % i  == 500);
        assert(is(typeof(x % s)  == int)    && x % s  == 10500);
    }

    /**
        Implements operators with built-in integers on the left-hand side and
        `BigInt` on the right-hand side.
     */
    BigInt opBinaryRight(string op, T)(T y) pure nothrow @safe const
    if ((op=="+" || op=="*" || op=="|" || op=="&" || op=="^") && isIntegral!T)
    {
        return opBinary!(op)(y);
    }

    ///
    @safe unittest
    {
        auto x = BigInt("100");
        BigInt y = 123 + x;
        assert(y == BigInt("223"));

        BigInt z = 123 - x;
        assert(z == BigInt("23"));

        // Dividing a built-in integer type by BigInt always results in
        // something that fits in a built-in type, so the built-in type is
        // returned, not BigInt.
        assert(is(typeof(1000 / x) == int));
        assert(1000 / x == 10);
    }

    //  BigInt = integer op BigInt
    /// ditto
    BigInt opBinaryRight(string op, T)(T y) pure nothrow @safe const
    if (op == "-" && isIntegral!T)
    {
        ulong u = absUnsign(y);
        BigInt r;
        static if (op == "-")
        {
            r.sign = sign;
            r.data = BigUint.addOrSubInt!ulong(data, u, wantSub: sign == (y<0), r.sign);
            r.negate();
        }
        return r;
    }

    //  integer = integer op BigInt
    /// ditto
    T opBinaryRight(string op, T)(T x) pure nothrow @safe const
    if ((op=="%" || op=="/") && isIntegral!T)
    {
        checkDivByZero();

        static if (op == "%")
        {
            // x%y always has the same sign as x.
            if (data.ulongLength > 1)
                return x;
            immutable u = absUnsign(x);
            immutable rem = u % data.peekUlong(0);
            // x%y always has the same sign as x.
            return cast(T)((x<0) ? -rem : rem);
        }
        else static if (op == "/")
        {
            if (data.ulongLength > 1)
                return 0;
            return cast(T)(x / data.peekUlong(0));
        }
    }

    // const unary operations
    /**
        Implements `BigInt` unary operators.
     */
    BigInt opUnary(string op)() pure nothrow @safe const
    if (op=="+" || op=="-" || op=="~")
    {
       static if (op=="-")
       {
            BigInt r = this;
            r.negate();
            return r;
        }
        else static if (op=="~")
        {
            return -(this+1);
        }
        else static if (op=="+")
           return this;
    }

    // non-const unary operations
    /// ditto
    BigInt opUnary(string op)() pure nothrow @safe
    if (op=="++" || op=="--")
    {
        static if (op=="++")
        {
            data = BigUint.addOrSubInt!ulong(data, 1UL, wantSub: sign, sign);
            return this;
        }
        else static if (op=="--")
        {
            data = BigUint.addOrSubInt!ulong(data, 1UL, wantSub: !sign, sign);
            return this;
        }
    }

    ///
    @safe unittest
    {
        auto x = BigInt("1234");
        assert(-x == BigInt("-1234"));

        ++x;
        assert(x == BigInt("1235"));
    }

    /**
        Implements `BigInt` equality test with other `BigInt`'s and built-in
        numeric types.
     */
    bool opEquals()(auto ref const BigInt y) const pure @nogc @safe
    {
       return sign == y.sign && y.data == data;
    }

    /// ditto
    bool opEquals(T)(const T y) const pure nothrow @nogc @safe
    if (isIntegral!T)
    {
        if (sign != (y<0))
            return 0;
        return data.opEquals(cast(ulong) absUnsign(y));
    }

    /// ditto
    bool opEquals(T)(const T y) const pure nothrow @nogc
    if (isFloatingPoint!T)
    {
        return 0 == opCmp(y);
    }

    ///
    @safe unittest
    {
        // Note that when comparing a BigInt to a float or double the
        // full precision of the BigInt is always considered, unlike
        // when comparing an int to a float or a long to a double.
        assert(BigInt(123456789) != cast(float) 123456789);
    }

    @safe unittest
    {
        auto x = BigInt("12345");
        auto y = BigInt("12340");
        int z = 12345;
        int w = 54321;

        assert(x == x);
        assert(x != y);
        assert(x == y + 5);
        assert(x == z);
        assert(x != w);
    }

    @safe unittest
    {
        import std.math.operations : nextDown, nextUp;

        const x = BigInt("0x1abc_de80_0000_0000_0000_0000_0000_0000");
        BigInt x1 = x + 1;
        BigInt x2 = x - 1;

        const d = 0x1.abcde8p124;
        assert(x == d);
        assert(x1 != d);
        assert(x2 != d);
        assert(x != nextUp(d));
        assert(x != nextDown(d));
        assert(x != double.nan);

        const dL = 0x1.abcde8p124L;
        assert(x == dL);
        assert(x1 != dL);
        assert(x2 != dL);
        assert(x != nextUp(dL));
        assert(x != nextDown(dL));
        assert(x != real.nan);

        assert(BigInt(0) == 0.0f);
        assert(BigInt(0) == 0.0);
        assert(BigInt(0) == 0.0L);
        assert(BigInt(0) == -0.0f);
        assert(BigInt(0) == -0.0);
        assert(BigInt(0) == -0.0L);
        assert(BigInt("999_999_999_999_999_999_999_999_999_999_999_999_999") != float.infinity);
    }

    /**
        Implements casting to `bool`.
     */
    T opCast(T:bool)() pure nothrow @nogc @safe const
    {
        return !isZero();
    }

    ///
    @safe unittest
    {
        // Non-zero values are regarded as true
        auto x = BigInt("1");
        auto y = BigInt("10");
        assert(x);
        assert(y);

        // Zero value is regarded as false
        auto z = BigInt("0");
        assert(!z);
    }

    /**
        Implements casting to integer types.

        Throws: $(REF ConvOverflowException, std,conv) if the number exceeds
        the target type's range.
     */
    T opCast(T:ulong)() pure @safe const
    {
        if (isUnsigned!T && sign)
            { /* throw */ }
        else
        if (data.ulongLength == 1)
        {
            ulong l = data.peekUlong(0);
            if (isUnsigned!T || !sign)
            {
                if (l <= T.max)
                    return cast(T) l;
            }
            else
            {
                if (l <= ulong(T.max)+1)
                    return cast(T)-long(l); // -long.min == long.min
            }
        }

        import std.conv : ConvOverflowException;
        import std.string : format;
        throw new ConvOverflowException(
            "BigInt(%s) cannot be represented as a %s"
            .format(this.toDecimalString, T.stringof));
    }

    ///
    @safe unittest
    {
        import std.conv : to, ConvOverflowException;
        import std.exception : assertThrown;

        assert(BigInt("0").to!int == 0);

        assert(BigInt("0").to!ubyte == 0);
        assert(BigInt("255").to!ubyte == 255);
        assertThrown!ConvOverflowException(BigInt("256").to!ubyte);
        assertThrown!ConvOverflowException(BigInt("-1").to!ubyte);
    }

    @safe unittest
    {
        import std.conv : to, ConvOverflowException;
        import std.exception : assertThrown;

        assert(BigInt("-1").to!byte == -1);
        assert(BigInt("-128").to!byte == -128);
        assert(BigInt("127").to!byte == 127);
        assertThrown!ConvOverflowException(BigInt("-129").to!byte);
        assertThrown!ConvOverflowException(BigInt("128").to!byte);

        assert(BigInt("0").to!uint == 0);
        assert(BigInt("4294967295").to!uint == uint.max);
        assertThrown!ConvOverflowException(BigInt("4294967296").to!uint);
        assertThrown!ConvOverflowException(BigInt("-1").to!uint);

        assert(BigInt("-1").to!int == -1);
        assert(BigInt("-2147483648").to!int == int.min);
        assert(BigInt("2147483647").to!int == int.max);
        assertThrown!ConvOverflowException(BigInt("-2147483649").to!int);
        assertThrown!ConvOverflowException(BigInt("2147483648").to!int);

        assert(BigInt("0").to!ulong == 0);
        assert(BigInt("18446744073709551615").to!ulong == ulong.max);
        assertThrown!ConvOverflowException(BigInt("18446744073709551616").to!ulong);
        assertThrown!ConvOverflowException(BigInt("-1").to!ulong);

        assert(BigInt("-1").to!long == -1);
        assert(BigInt("-9223372036854775808").to!long == long.min);
        assert(BigInt("9223372036854775807").to!long == long.max);
        assertThrown!ConvOverflowException(BigInt("-9223372036854775809").to!long);
        assertThrown!ConvOverflowException(BigInt("9223372036854775808").to!long);
    }

    /**
        Implements casting to floating point types.
     */
    T opCast(T)() @safe nothrow @nogc const
    if (isFloatingPoint!T)
    {
        return toFloat!(T, "nearest");
    }

    ///
    @system unittest
    {
        assert(cast(float)  BigInt("35540592535949172786332045140593475584")
                == 35540592535949172786332045140593475584.0f);
        assert(cast(double) BigInt("35540601499647381470685035515422441472")
                == 35540601499647381470685035515422441472.0);
        assert(cast(real)   BigInt("35540601499647381470685035515422441472")
                == 35540601499647381470685035515422441472.0L);

        assert(cast(float)  BigInt("-0x1345_6780_0000_0000_0000_0000_0000") == -0x1.3456_78p+108f       );
        assert(cast(double) BigInt("-0x1345_678a_bcde_f000_0000_0000_0000") == -0x1.3456_78ab_cdefp+108 );
        assert(cast(real)   BigInt("-0x1345_678a_bcde_f000_0000_0000_0000") == -0x1.3456_78ab_cdefp+108L);
    }

    /// Rounding when casting to floating point
    @system unittest
    {
        // BigInts whose values cannot be exactly represented as float/double/real
        // are rounded when cast to float/double/real. When cast to float or
        // double or 64-bit real the rounding is strictly defined. When cast
        // to extended-precision real the rounding rules vary by environment.

        // BigInts that fall somewhere between two non-infinite floats/doubles
        // are rounded to the closer value when cast to float/double.
        assert(cast(float) BigInt(0x1aaa_aae7) == 0x1.aaa_aaep+28f);
        assert(cast(float) BigInt(0x1aaa_aaff) == 0x1.aaa_ab0p+28f);
        assert(cast(float) BigInt(-0x1aaa_aae7) == -0x1.aaaaaep+28f);
        assert(cast(float) BigInt(-0x1aaa_aaff) == -0x1.aaaab0p+28f);

        assert(cast(double) BigInt(0x1aaa_aaaa_aaaa_aa77) == 0x1.aaa_aaaa_aaaa_aa00p+60);
        assert(cast(double) BigInt(0x1aaa_aaaa_aaaa_aaff) == 0x1.aaa_aaaa_aaaa_ab00p+60);
        assert(cast(double) BigInt(-0x1aaa_aaaa_aaaa_aa77) == -0x1.aaa_aaaa_aaaa_aa00p+60);
        assert(cast(double) BigInt(-0x1aaa_aaaa_aaaa_aaff) == -0x1.aaa_aaaa_aaaa_ab00p+60);

        // BigInts that fall exactly between two non-infinite floats/doubles
        // are rounded away from zero when cast to float/double. (Note that
        // in most environments this is NOT the same rounding rule rule used
        // when casting int/long to float/double.)
        assert(cast(float) BigInt(0x1aaa_aaf0) == 0x1.aaa_ab0p+28f);
        assert(cast(float) BigInt(-0x1aaa_aaf0) == -0x1.aaaab0p+28f);

        assert(cast(double) BigInt(0x1aaa_aaaa_aaaa_aa80) == 0x1.aaa_aaaa_aaaa_ab00p+60);
        assert(cast(double) BigInt(-0x1aaa_aaaa_aaaa_aa80) == -0x1.aaa_aaaa_aaaa_ab00p+60);

        // BigInts that are bounded on one side by the largest positive or
        // most negative finite float/double and on the other side by infinity
        // or -infinity are rounded as if in place of infinity was the value
        // `2^^(T.max_exp)` when cast to float/double.
        assert(cast(float) BigInt("999_999_999_999_999_999_999_999_999_999_999_999_999") == float.infinity);
        assert(cast(float) BigInt("-999_999_999_999_999_999_999_999_999_999_999_999_999") == -float.infinity);

        assert(cast(double) BigInt("999_999_999_999_999_999_999_999_999_999_999_999_999") < double.infinity);
        assert(cast(real) BigInt("999_999_999_999_999_999_999_999_999_999_999_999_999") < real.infinity);
    }

    @safe unittest
    {
        // Test exponent overflow is correct.
        assert(cast(float) BigInt(0x1fffffff) == 0x1.000000p+29f);
        assert(cast(double) BigInt(0x1fff_ffff_ffff_fff0) == 0x1.000000p+61);
    }

    private T toFloat(T, string roundingMode)() @safe nothrow @nogc const
    if (__traits(isFloating, T) && (roundingMode == "nearest" || roundingMode == "truncate"))
    {
        import core.bitop : bsr;
        enum performRounding = (roundingMode == "nearest");
        enum performTruncation = (roundingMode == "truncate");
        static assert(performRounding || performTruncation, "unrecognized rounding mode");
        enum int totalNeededBits = T.mant_dig + int(performRounding);
        static if (totalNeededBits <= 64)
        {
            // We need to examine the top two 64-bit words, not just the top one,
            // since the top word could have just a single significant bit.
            const ulongLength = data.ulongLength;
            const ulong w1 = data.peekUlong(ulongLength - 1);
            if (w1 == 0)
                return T(0); // Special: exponent should be all zero bits, plus bsr(w1) is undefined.
            const ulong w2 = ulongLength < 2 ? 0 : data.peekUlong(ulongLength - 2);
            const uint w1BitCount = bsr(w1) + 1;
            ulong sansExponent = (w1 << (64 - w1BitCount)) | (w2 >>> (w1BitCount));
            size_t exponent = (ulongLength - 1) * 64 + w1BitCount + 1;
            static if (performRounding)
            {
                sansExponent += 1UL << (64 - totalNeededBits);
                if (0 <= cast(long) sansExponent) // Use high bit to detect overflow.
                {
                    // Do not bother filling in the high bit of sansExponent
                    // with 1. It will be discarded by float and double and 80
                    // bit real cannot be on this path with rounding enabled.
                    exponent += 1;
                }
            }
            static if (T.mant_dig == float.mant_dig)
            {
                if (exponent >= T.max_exp)
                    return isNegative ? -T.infinity : T.infinity;
                uint resultBits = (uint(isNegative) << 31) | // sign bit
                    ((0xFF & (exponent - float.min_exp)) << 23) | // exponent
                    cast(uint) ((sansExponent << 1) >>> (64 - 23)); // mantissa.
                // TODO: remove @trusted lambda after DIP 1000 is enabled by default.
                return (() @trusted => *cast(float*) &resultBits)();
            }
            else static if (T.mant_dig == double.mant_dig)
            {
                if (exponent >= T.max_exp)
                    return isNegative ? -T.infinity : T.infinity;
                ulong resultBits = (ulong(isNegative) << 63) | // sign bit
                    ((0x7FFUL & (exponent - double.min_exp)) << 52) | // exponent
                    ((sansExponent << 1) >>> (64 - 52)); // mantissa.
                // TODO: remove @trusted lambda after DIP 1000 is enabled by default.
                return (() @trusted => *cast(double*) &resultBits)();
            }
            else
            {
                import core.math : ldexp;
                return ldexp(isNegative ? -cast(real) sansExponent : cast(real) sansExponent,
                    cast(int) exponent - 65);
            }
        }
        else
        {
            import core.math : ldexp;
            const ulongLength = data.ulongLength;
            if ((ulongLength - 1) * 64L > int.max)
                return isNegative ? -T.infinity : T.infinity;
            int scale = cast(int) ((ulongLength - 1) * 64);
            const ulong w1 = data.peekUlong(ulongLength - 1);
            if (w1 == 0)
                return T(0); // Special: bsr(w1) is undefined.
            int bitsStillNeeded = totalNeededBits - bsr(w1) - 1;
            T acc = ldexp(cast(T) w1, scale);
            for (ptrdiff_t i = ulongLength - 2; i >= 0 && bitsStillNeeded > 0; i--)
            {
                ulong w = data.peekUlong(i);
                // To round towards zero we must make sure not to use too many bits.
                if (bitsStillNeeded >= 64)
                {
                    acc += ldexp(cast(T) w, scale -= 64);
                    bitsStillNeeded -= 64;
                }
                else
                {
                    w = (w >>> (64 - bitsStillNeeded)) << (64 - bitsStillNeeded);
                    acc += ldexp(cast(T) w, scale -= 64);
                    break;
                }
            }
            if (isNegative)
                acc = -acc;
            return cast(T) acc;
        }
    }

    /**
        Implements casting to/from qualified `BigInt`'s.

        Warning: Casting to/from `const` or `immutable` may break type
        system guarantees. Use with care.
     */
    T opCast(T)() pure nothrow @nogc const
    if (is(immutable T == immutable BigInt))
    {
        return this;
    }

    ///
    @safe unittest
    {
        const(BigInt) x = BigInt("123");
        BigInt y = cast() x;    // cast away const
        assert(y == x);
    }

    // Hack to make BigInt's typeinfo.compare work properly.
    // Note that this must appear before the other opCmp overloads, otherwise
    // DMD won't find it.
    /**
        Implements 3-way comparisons of `BigInt` with `BigInt` or `BigInt` with
        built-in numeric types.
     */
    int opCmp(ref const BigInt y) pure nothrow @nogc @safe const
    {
        // Simply redirect to the "real" opCmp implementation.
        return this.opCmp!BigInt(y);
    }

    /// ditto
    int opCmp(T)(const T y) pure nothrow @nogc @safe const
    if (isIntegral!T)
    {
        if (sign != (y<0) )
            return sign ? -1 : 1;
        int cmp = data.opCmp(cast(ulong) absUnsign(y));
        return sign? -cmp: cmp;
    }
    /// ditto
    int opCmp(T)(const T y) nothrow @nogc @safe const
    if (isFloatingPoint!T)
    {
        import core.bitop : bsr;
        import std.math.operations : cmp;
        import std.math.traits : isFinite;

        const asFloat = toFloat!(T, "truncate");
        if (asFloat != y)
            return cmp(asFloat, y); // handles +/- NaN.
        if (!isFinite(y))
            return isNegative ? 1 : -1;
        const ulongLength = data.ulongLength;
        const w1 = data.peekUlong(ulongLength - 1);
        if (w1 == 0)
            return 0; // Special: bsr(w1) is undefined.
        const numSignificantBits = (ulongLength - 1) * 64 + bsr(w1) + 1;
        for (ptrdiff_t bitsRemainingToCheck = numSignificantBits - T.mant_dig, i = 0;
            bitsRemainingToCheck > 0; i++, bitsRemainingToCheck -= 64)
        {
            auto word = data.peekUlong(i);
            if (word == 0)
                continue;
            // Make sure we're only checking digits that are beyond
            // the precision of `y`.
            if (bitsRemainingToCheck < 64 && (word << (64 - bitsRemainingToCheck)) == 0)
                break; // This can only happen on the last loop iteration.
            return isNegative ? -1 : 1;
        }
        return 0;
    }
    /// ditto
    int opCmp(T:BigInt)(const T y) pure nothrow @nogc @safe const
    {
        if (sign != y.sign)
            return sign ? -1 : 1;
        immutable cmp = data.opCmp(y.data);
        return sign? -cmp: cmp;
    }

    ///
    @safe unittest
    {
        auto x = BigInt("100");
        auto y = BigInt("10");
        int z = 50;
        const int w = 200;

        assert(y < x);
        assert(x > z);
        assert(z > y);
        assert(x < w);
    }

    ///
    @safe unittest
    {
        auto x = BigInt("0x1abc_de80_0000_0000_0000_0000_0000_0000");
        BigInt y = x - 1;
        BigInt z = x + 1;

        double d = 0x1.abcde8p124;
        assert(y < d);
        assert(z > d);
        assert(x >= d && x <= d);

        // Note that when comparing a BigInt to a float or double the
        // full precision of the BigInt is always considered, unlike
        // when comparing an int to a float or a long to a double.
        assert(BigInt(123456789) < cast(float) 123456789);
    }

    @safe unittest
    {
        assert(BigInt("999_999_999_999_999_999_999_999_999_999_999_999_999") < float.infinity);

        // Test `real` works.
        auto x = BigInt("0x1abc_de80_0000_0000_0000_0000_0000_0000");
        BigInt y = x - 1;
        BigInt z = x + 1;

        real d = 0x1.abcde8p124;
        assert(y < d);
        assert(z > d);
        assert(x >= d && x <= d);

        // Test comparison for numbers of 64 bits or fewer.
        auto w1 = BigInt(0x1abc_de80_0000_0000);
        auto w2 = w1 - 1;
        auto w3 = w1 + 1;
        assert(w1.ulongLength == 1);
        assert(w2.ulongLength == 1);
        assert(w3.ulongLength == 1);

        double e = 0x1.abcde8p+60;
        assert(w1 >= e && w1 <= e);
        assert(w2 < e);
        assert(w3 > e);

        real eL = 0x1.abcde8p+60;
        assert(w1 >= eL && w1 <= eL);
        assert(w2 < eL);
        assert(w3 > eL);
    }

    /**
        Returns: The value of this `BigInt` as a `long`, or `long.max`/`long.min`
        if outside the representable range.
     */
    long toLong() @safe pure nothrow const @nogc
    {
        return (sign ? -1 : 1) *
          (data.ulongLength == 1  && (data.peekUlong(0) <= sign+cast(ulong)(long.max)) // 1+long.max = |long.min|
          ? cast(long)(data.peekUlong(0))
          : long.max);
    }

    ///
    @safe unittest
    {
        auto b = BigInt("12345");
        long l = b.toLong();
        assert(l == 12345);
    }

    /**
        Returns: The value of this `BigInt` as an `int`, or `int.max`/`int.min` if outside
        the representable range.
     */
    int toInt() @safe pure nothrow @nogc const
    {
        return (sign ? -1 : 1) *
          (data.uintLength == 1  && (data.peekUint(0) <= sign+cast(uint)(int.max)) // 1+int.max = |int.min|
          ? cast(int)(data.peekUint(0))
          : int.max);
    }

    ///
    @safe unittest
    {
        auto big = BigInt("5_000_000");
        auto i = big.toInt();
        assert(i == 5_000_000);

        // Numbers that are too big to fit into an int will be clamped to int.max.
        auto tooBig = BigInt("5_000_000_000");
        i = tooBig.toInt();
        assert(i == int.max);
    }

    /// Number of significant `uint`s which are used in storing this number.
    /// The absolute value of this `BigInt` is always &lt; 2$(SUPERSCRIPT 32*uintLength)
    @property size_t uintLength() @safe pure nothrow @nogc const
    {
        return data.uintLength;
    }

    /// Number of significant `ulong`s which are used in storing this number.
    /// The absolute value of this `BigInt` is always &lt; 2$(SUPERSCRIPT 64*ulongLength)
    @property size_t ulongLength() @safe pure nothrow @nogc const
    {
        return data.ulongLength;
    }

    /** Convert the `BigInt` to `string`, passing it to the given sink.
     *
     * Params:
     *  sink = An OutputRange for accepting possibly piecewise segments of the
     *      formatted string.
     *  formatString = A format string specifying the output format.
     *
     * $(TABLE  Available output formats:,
     * $(TR $(TD "d") $(TD  Decimal))
     * $(TR $(TD "o") $(TD  Octal))
     * $(TR $(TD "x") $(TD  Hexadecimal, lower case))
     * $(TR $(TD "X") $(TD  Hexadecimal, upper case))
     * $(TR $(TD "s") $(TD  Default formatting (same as "d") ))
     * $(TR $(TD null) $(TD Default formatting (same as "d") ))
     * )
     */
    void toString(Writer)(scope ref Writer sink, string formatString) const
    {
        auto f = FormatSpec!char(formatString);
        f.writeUpToNextSpec(sink);
        toString!Writer(sink, f);
    }

    /// ditto
    void toString(Writer)(scope ref Writer sink, scope const ref FormatSpec!char f) const
    {
        import std.range.primitives : put;
        const spec = f.spec;
        immutable hex = (spec == 'x' || spec == 'X');
        if (!(spec == 's' || spec == 'd' || spec =='o' || hex))
            throw new FormatException("Format specifier not understood: %" ~ spec);

        char[] buff;
        if (spec == 'X')
        {
            buff = data.toHexString(0, '_', 0, f.flZero ? '0' : ' ', LetterCase.upper);
        }
        else if (spec == 'x')
        {
            buff = data.toHexString(0, '_', 0, f.flZero ? '0' : ' ', LetterCase.lower);
        }
        else if (spec == 'o')
        {
            buff = data.toOctalString();
        }
        else
        {
            buff = data.toDecimalString(0);
        }
        assert(buff.length > 0, "Invalid buffer length");

        char signChar = isNegative ? '-' : 0;
        auto minw = buff.length + (signChar ? 1 : 0);

        if (!hex && !signChar && (f.width == 0 || minw < f.width))
        {
            if (f.flPlus)
            {
                signChar = '+';
                ++minw;
            }
            else if (f.flSpace)
            {
                signChar = ' ';
                ++minw;
            }
        }

        immutable maxw = minw < f.width ? f.width : minw;
        immutable difw = maxw - minw;

        if (!f.flDash && !f.flZero)
            foreach (i; 0 .. difw)
                put(sink, " ");

        if (signChar)
        {
            scope char[1] buf = signChar;
            put(sink, buf[]);
        }

        if (!f.flDash && f.flZero)
            foreach (i; 0 .. difw)
                put(sink, "0");

        put(sink, buff);

        if (f.flDash)
            foreach (i; 0 .. difw)
                put(sink, " ");
    }

    /**
        `toString` is rarely directly invoked; the usual way of using it is via
        $(REF format, std, format):
     */
    @safe unittest
    {
        import std.format : format;

        auto x = BigInt("1_000_000");
        x *= 12345;

        assert(format("%d", x) == "12345000000");
        assert(format("%x", x) == "2_dfd1c040");
        assert(format("%X", x) == "2_DFD1C040");
        assert(format("%o", x) == "133764340100");
    }

    // for backwards compatibility, see unittest below
    /// ditto
    void toString(scope void delegate(scope const(char)[]) sink, string formatString) const
    {
        toString!(void delegate(scope const(char)[]))(sink, formatString);
    }

    // for backwards compatibility, see unittest below
    /// ditto
    void toString(scope void delegate(scope const(char)[]) sink, scope const ref FormatSpec!char f) const
    {
        toString!(void delegate(scope const(char)[]))(sink, f);
    }

    // Backwards compatibility test
    // BigInt.toString used to only accept a delegate sink function, but this does not work
    // well with attributes such as @safe. A template function toString was added that
    // works on OutputRanges, but when a delegate was passed in the form of an untyped
    // lambda such as `str => dst.put(str)` the parameter type was inferred as `void` and
    // the function failed to instantiate.
    @system unittest
    {
        import std.format.spec : FormatSpec;
        import std.array : appender;
        BigInt num = 503;
        auto dst = appender!string();
        num.toString(str => dst.put(str), null);
        assert(dst[] == "503");
        num = 504;
        auto f = FormatSpec!char("");
        num.toString(str => dst.put(str), f);
        assert(dst[] == "503504");
    }

    // Implement toHash so that BigInt works properly as an AA key.
    /**
        Returns: A unique hash of the `BigInt`'s value suitable for use in a hash
        table.
     */
    size_t toHash() const @safe pure nothrow @nogc
    {
        return data.toHash() + sign;
    }

    /**
        `toHash` is rarely directly invoked; it is implicitly used when
        BigInt is used as the key of an associative array.
     */
    @safe pure unittest
    {
        string[BigInt] aa;
        aa[BigInt(123)] = "abc";
        aa[BigInt(456)] = "def";

        assert(aa[BigInt(123)] == "abc");
        assert(aa[BigInt(456)] == "def");
    }

    /**
     * Gets the nth number in the underlying representation that makes up the whole
     * `BigInt`.
     *
     * Params:
     *     T = the type to view the underlying representation as
     *     n = The nth number to retrieve. Must be less than $(LREF ulongLength) or
     *     $(LREF uintLength) with respect to `T`.
     * Returns:
     *     The nth `ulong` in the representation of this `BigInt`.
     */
    T getDigit(T = ulong)(size_t n) const
    if (is(T == ulong) || is(T == uint))
    {
        static if (is(T == ulong))
        {
            assert(n < ulongLength(), "getDigit index out of bounds");
            return data.peekUlong(n);
        }
        else
        {
            assert(n < uintLength(), "getDigit index out of bounds");
            return data.peekUint(n);
        }
    }

    ///
    @safe pure unittest
    {
        auto a = BigInt("1000");
        assert(a.ulongLength() == 1);
        assert(a.getDigit(0) == 1000);

        assert(a.uintLength() == 1);
        assert(a.getDigit!uint(0) == 1000);

        auto b = BigInt("2_000_000_000_000_000_000_000_000_000");
        assert(b.ulongLength() == 2);
        assert(b.getDigit(0) == 4584946418820579328);
        assert(b.getDigit(1) == 108420217);

        assert(b.uintLength() == 3);
        assert(b.getDigit!uint(0) == 3489660928);
        assert(b.getDigit!uint(1) == 1067516025);
        assert(b.getDigit!uint(2) == 108420217);
    }

private:
    void negate() @safe pure nothrow @nogc scope
    {
        if (!data.isZero())
            sign = !sign;
    }
    bool isZero() pure const nothrow @nogc @safe scope
    {
        return data.isZero();
    }
    alias isNegative = sign;

    // Generate a runtime error if division by zero occurs
    void checkDivByZero() pure const nothrow @safe scope
    {
        assert(!isZero(), "BigInt division by zero");
    }
}

///
@safe unittest
{
    BigInt a = "9588669891916142";
    BigInt b = "7452469135154800";
    auto c = a * b;
    assert(c == BigInt("71459266416693160362545788781600"));
    auto d = b * a;
    assert(d == BigInt("71459266416693160362545788781600"));
    assert(d == c);
    d = c * BigInt("794628672112");
    assert(d == BigInt("56783581982794522489042432639320434378739200"));
    auto e = c + d;
    assert(e == BigInt("56783581982865981755459125799682980167520800"));
    auto f = d + c;
    assert(f == e);
    auto g = f - c;
    assert(g == d);
    g = f - d;
    assert(g == c);
    e = 12345678;
    g = c + e;
    auto h = g / b;
    auto i = g % b;
    assert(h == a);
    assert(i == e);
    BigInt j = "-0x9A56_57f4_7B83_AB78";
    BigInt k = j;
    j ^^= 11;
    assert(k ^^ 11 == j);
}

/**
Params:
    x = The `BigInt` to convert to a decimal `string`.

Returns:
    A `string` that represents the `BigInt` as a decimal number.

*/
string toDecimalString(const(BigInt) x) pure nothrow @safe
{
    auto buff = x.data.toDecimalString(x.isNegative ? 1 : 0);
    if (x.isNegative)
        buff[0] = '-';
    return buff;
}

///
@safe pure unittest
{
    auto x = BigInt("123");
    x *= 1000;
    x += 456;

    auto xstr = x.toDecimalString();
    assert(xstr == "123456");
}

/**
Params:
    x = The `BigInt` to convert to a hexadecimal `string`.

Returns:
    A `string` that represents the `BigInt` as a hexadecimal (base 16)
    number in upper case.

*/
string toHex(const(BigInt) x) pure @safe
{
    import std.array : appender;
    auto outbuff = appender!string();
    x.toString(outbuff, "%X");
    return outbuff[];
}

///
@safe unittest
{
    auto x = BigInt("123");
    x *= 1000;
    x += 456;

    auto xstr = x.toHex();
    assert(xstr == "1E240");
}

/** Returns the absolute value of x converted to the corresponding unsigned
type.

Params:
    x = The integral value to return the absolute value of.

Returns:
    The absolute value of x.

*/
Unsigned!T absUnsign(T)(T x)
if (isIntegral!T)
{
    static if (isSigned!T)
    {
        import std.conv : unsigned;
        /* This returns the correct result even when x = T.min
         * on two's complement machines because unsigned(T.min) = |T.min|
         * even though -T.min = T.min.
         */
        return unsigned((x < 0) ? cast(T)(0-x) : x);
    }
    else
    {
        return x;
    }
}

///
nothrow pure @safe
unittest
{
    assert((-1).absUnsign == 1);
    assert(1.absUnsign == 1);
}

nothrow pure @safe
unittest
{
    BigInt a, b;
    a = 1;
    b = 2;
    auto c = a + b;
    assert(c == 3);
}

nothrow pure @safe
unittest
{
    long a;
    BigInt b;
    auto c = a + b;
    assert(c == 0);
    auto d = b + a;
    assert(d == 0);
}

nothrow pure @safe
unittest
{
    BigInt x = 1, y = 2;
    assert(x <  y);
    assert(x <= y);
    assert(y >= x);
    assert(y >  x);
    assert(x != y);

    long r1 = x.toLong;
    assert(r1 == 1);

    BigInt r2 = 10 % x;
    assert(r2 == 0);

    BigInt r3 = 10 / y;
    assert(r3 == 5);

    BigInt[] arr = [BigInt(1)];
    auto incr = arr[0]++;
    assert(arr == [BigInt(2)]);
    assert(incr == BigInt(1));
}

@safe unittest
{
    // Radix conversion
    assert( toDecimalString(BigInt("-1_234_567_890_123_456_789"))
        == "-1234567890123456789");
    assert( toHex(BigInt("0x1234567890123456789")) == "123_45678901_23456789");
    assert( toHex(BigInt("0x00000000000000000000000000000000000A234567890123456789"))
        == "A23_45678901_23456789");
    assert( toHex(BigInt("0x000_00_000000_000_000_000000000000_000000_")) == "0");

    assert(BigInt(-0x12345678).toInt() == -0x12345678);
    assert(BigInt(-0x12345678).toLong() == -0x12345678);
    assert(BigInt(0x1234_5678_9ABC_5A5AL).ulongLength == 1);
    assert(BigInt(0x1234_5678_9ABC_5A5AL).toLong() == 0x1234_5678_9ABC_5A5AL);
    assert(BigInt(-0x1234_5678_9ABC_5A5AL).toLong() == -0x1234_5678_9ABC_5A5AL);
    assert(BigInt(0xF234_5678_9ABC_5A5AL).toLong() == long.max);
    assert(BigInt(-0x123456789ABCL).toInt() == -int.max);
    char[] s1 = "123".dup; // https://issues.dlang.org/show_bug.cgi?id=8164
    assert(BigInt(s1) == 123);
    char[] s2 = "0xABC".dup;
    assert(BigInt(s2) == 2748);

    assert((BigInt(-2) + BigInt(1)) == BigInt(-1));
    BigInt a = ulong.max - 5;
    auto b = -long.max % a;
    assert( b == -long.max % (ulong.max - 5));
    b = long.max / a;
    assert( b == long.max /(ulong.max - 5));
    assert(BigInt(1) - 1 == 0);
    assert((-4) % BigInt(5) == -4); // https://issues.dlang.org/show_bug.cgi?id=5928
    assert(BigInt(-4) % BigInt(5) == -4);
    assert(BigInt(2)/BigInt(-3) == BigInt(0)); // https://issues.dlang.org/show_bug.cgi?id=8022
    assert(BigInt("-1") > long.min); // https://issues.dlang.org/show_bug.cgi?id=9548

    assert(toDecimalString(BigInt("0000000000000000000000000000000000000000001234567"))
        == "1234567");
}

@safe unittest // Minimum signed value bug tests.
{
    assert(BigInt("-0x8000000000000000") == BigInt(long.min));
    assert(BigInt("-0x8000000000000000")+1 > BigInt(long.min));
    assert(BigInt("-0x80000000") == BigInt(int.min));
    assert(BigInt("-0x80000000")+1 > BigInt(int.min));
    assert(BigInt(long.min).toLong() == long.min); // lossy toLong bug for long.min
    assert(BigInt(int.min).toInt() == int.min); // lossy toInt bug for int.min
    assert(BigInt(long.min).ulongLength == 1);
    assert(BigInt(int.min).uintLength == 1); // cast/sign extend bug in opAssign
    BigInt a;
    a += int.min;
    assert(a == BigInt(int.min));
    a = int.min - BigInt(int.min);
    assert(a == 0);
    a = int.min;
    assert(a == BigInt(int.min));
    assert(int.min % (BigInt(int.min)-1) == int.min);
    assert((BigInt(int.min)-1)%int.min == -1);
}

 // Recursive division (https://issues.dlang.org/show_bug.cgi?id=5568)
@safe unittest
{
    enum Z = 4843;
    BigInt m = (BigInt(1) << (Z*8) ) - 1;
    m -= (BigInt(1) << (Z*6)) - 1;
    BigInt oldm = m;

    BigInt a = (BigInt(1) << (Z*4) )-1;
    BigInt b = m % a;
    m /= a;
    m *= a;
    assert( m + b == oldm);

    m = (BigInt(1) << (4846 + 4843) ) - 1;
    a = (BigInt(1) << 4846 ) - 1;
    b = (BigInt(1) << (4846*2 + 4843)) - 1;
    BigInt c = (BigInt(1) << (4846*2 + 4843*2)) - 1;
    BigInt w =  c - b + a;
    assert(w % m == 0);

    // https://issues.dlang.org/show_bug.cgi?id=6819
    BigInt z1 = BigInt(10)^^64;
    BigInt w1 = BigInt(10)^^128;
    assert(z1^^2 == w1);
    BigInt z2 = BigInt(1)<<64;
    BigInt w2 = BigInt(1)<<128;
    assert(z2^^2 == w2);
    // https://issues.dlang.org/show_bug.cgi?id=7993
    BigInt n7793 = 10;
    assert( n7793 / 1 == 10);
    // https://issues.dlang.org/show_bug.cgi?id=7973
    auto a7973 = 10_000_000_000_000_000;
    const c7973 = 10_000_000_000_000_000;
    immutable i7973 = 10_000_000_000_000_000;
    BigInt v7973 = 2551700137;
    v7973 %= a7973;
    assert(v7973 == 2551700137);
    v7973 %= c7973;
    assert(v7973 == 2551700137);
    v7973 %= i7973;
    assert(v7973 == 2551700137);
    // https://issues.dlang.org/show_bug.cgi?id=8165
    BigInt[2] a8165;
    a8165[0] = a8165[1] = 1;
}

@safe unittest
{
    import std.array;
    import std.format.write : formattedWrite;

    immutable string[][] table = [
    /*  fmt,        +10     -10 */
        ["%d",      "10",   "-10"],
        ["%+d",     "+10",  "-10"],
        ["%-d",     "10",   "-10"],
        ["%+-d",    "+10",  "-10"],

        ["%4d",     "  10", " -10"],
        ["%+4d",    " +10", " -10"],
        ["%-4d",    "10  ", "-10 "],
        ["%+-4d",   "+10 ", "-10 "],

        ["%04d",    "0010", "-010"],
        ["%+04d",   "+010", "-010"],
        ["%-04d",   "10  ", "-10 "],
        ["%+-04d",  "+10 ", "-10 "],

        ["% 04d",   " 010", "-010"],
        ["%+ 04d",  "+010", "-010"],
        ["%- 04d",  " 10 ", "-10 "],
        ["%+- 04d", "+10 ", "-10 "],
    ];

    auto w1 = appender!(char[])();
    auto w2 = appender!(char[])();

    foreach (entry; table)
    {
        immutable fmt = entry[0];

        formattedWrite(w1, fmt, BigInt(10));
        formattedWrite(w2, fmt, 10);
        assert(w1.data == w2.data);
        assert(w1.data == entry[1]);
        w1.clear();
        w2.clear();

        formattedWrite(w1, fmt, BigInt(-10));
        formattedWrite(w2, fmt, -10);
        assert(w1.data == w2.data);
        assert(w1.data == entry[2]);
        w1.clear();
        w2.clear();
    }
}

@safe unittest
{
    import std.array;
    import std.format.write : formattedWrite;

    immutable string[][] table = [
    /*  fmt,        +10     -10 */
        ["%x",      "a",    "-a"],
        ["%+x",     "a",    "-a"],
        ["%-x",     "a",    "-a"],
        ["%+-x",    "a",    "-a"],

        ["%4x",     "   a", "  -a"],
        ["%+4x",    "   a", "  -a"],
        ["%-4x",    "a   ", "-a  "],
        ["%+-4x",   "a   ", "-a  "],

        ["%04x",    "000a", "-00a"],
        ["%+04x",   "000a", "-00a"],
        ["%-04x",   "a   ", "-a  "],
        ["%+-04x",  "a   ", "-a  "],

        ["% 04x",   "000a", "-00a"],
        ["%+ 04x",  "000a", "-00a"],
        ["%- 04x",  "a   ", "-a  "],
        ["%+- 04x", "a   ", "-a  "],
    ];

    auto w1 = appender!(char[])();
    auto w2 = appender!(char[])();

    foreach (entry; table)
    {
        immutable fmt = entry[0];

        formattedWrite(w1, fmt, BigInt(10));
        formattedWrite(w2, fmt, 10);
        assert(w1.data == w2.data);     // Equal only positive BigInt
        assert(w1.data == entry[1]);
        w1.clear();
        w2.clear();

        formattedWrite(w1, fmt, BigInt(-10));
        //formattedWrite(w2, fmt, -10);
        //assert(w1.data == w2.data);
        assert(w1.data == entry[2]);
        w1.clear();
        //w2.clear();
    }
}

@safe unittest
{
    import std.array;
    import std.format.write : formattedWrite;

    immutable string[][] table = [
    /*  fmt,        +10     -10 */
        ["%X",      "A",    "-A"],
        ["%+X",     "A",    "-A"],
        ["%-X",     "A",    "-A"],
        ["%+-X",    "A",    "-A"],

        ["%4X",     "   A", "  -A"],
        ["%+4X",    "   A", "  -A"],
        ["%-4X",    "A   ", "-A  "],
        ["%+-4X",   "A   ", "-A  "],

        ["%04X",    "000A", "-00A"],
        ["%+04X",   "000A", "-00A"],
        ["%-04X",   "A   ", "-A  "],
        ["%+-04X",  "A   ", "-A  "],

        ["% 04X",   "000A", "-00A"],
        ["%+ 04X",  "000A", "-00A"],
        ["%- 04X",  "A   ", "-A  "],
        ["%+- 04X", "A   ", "-A  "],
    ];

    auto w1 = appender!(char[])();
    auto w2 = appender!(char[])();

    foreach (entry; table)
    {
        immutable fmt = entry[0];

        formattedWrite(w1, fmt, BigInt(10));
        formattedWrite(w2, fmt, 10);
        assert(w1.data == w2.data);     // Equal only positive BigInt
        assert(w1.data == entry[1]);
        w1.clear();
        w2.clear();

        formattedWrite(w1, fmt, BigInt(-10));
        //formattedWrite(w2, fmt, -10);
        //assert(w1.data == w2.data);
        assert(w1.data == entry[2]);
        w1.clear();
        //w2.clear();
    }
}

// https://issues.dlang.org/show_bug.cgi?id=6448
@safe unittest
{
    import std.array;
    import std.format.write : formattedWrite;

    auto w1 = appender!string();
    auto w2 = appender!string();

    int x = 100;
    formattedWrite(w1, "%010d", x);
    BigInt bx = x;
    formattedWrite(w2, "%010d", bx);
    assert(w1.data == w2.data);
    // https://issues.dlang.org/show_bug.cgi?id=8011
    BigInt y = -3;
    ++y;
    assert(y.toLong() == -2);
    y = 1;
    --y;
    assert(y.toLong() == 0);
    --y;
    assert(y.toLong() == -1);
    --y;
    assert(y.toLong() == -2);
}

@safe unittest
{
    import std.math.algebraic : abs;
    auto r = abs(BigInt(-1000)); // https://issues.dlang.org/show_bug.cgi?id=6486
    assert(r == 1000);

    auto r2 = abs(const(BigInt)(-500)); // https://issues.dlang.org/show_bug.cgi?id=11188
    assert(r2 == 500);
    auto r3 = abs(immutable(BigInt)(-733)); // https://issues.dlang.org/show_bug.cgi?id=11188
    assert(r3 == 733);

    // opCast!bool
    BigInt one = 1, zero;
    assert(one && !zero);
}

// https://issues.dlang.org/show_bug.cgi?id=6850
@safe unittest
{
    pure long pureTest() {
        BigInt a = 1;
        BigInt b = 1336;
        a += b;
        return a.toLong();
    }

    assert(pureTest() == 1337);
}

// https://issues.dlang.org/show_bug.cgi?id=8435
// https://issues.dlang.org/show_bug.cgi?id=10118
@safe unittest
{
    auto i = BigInt(100);
    auto j = BigInt(100);

    // Two separate BigInt instances representing same value should have same
    // hash.
    assert(typeid(i).getHash(&i) == typeid(j).getHash(&j));
    assert(typeid(i).compare(&i, &j) == 0);

    // BigInt AA keys should behave consistently.
    int[BigInt] aa;
    aa[BigInt(123)] = 123;
    assert(BigInt(123) in aa);

    aa[BigInt(123)] = 321;
    assert(aa[BigInt(123)] == 321);

    auto keys = aa.byKey;
    assert(keys.front == BigInt(123));
    keys.popFront();
    assert(keys.empty);
}

// https://issues.dlang.org/show_bug.cgi?id=11148
@safe unittest
{
    void foo(BigInt) {}
    const BigInt cbi = 3;
    immutable BigInt ibi = 3;

    foo(cbi);
    foo(ibi);

    import std.conv : to;
    import std.meta : AliasSeq;

    static foreach (T1; AliasSeq!(BigInt, const(BigInt), immutable(BigInt)))
    {
        static foreach (T2; AliasSeq!(BigInt, const(BigInt), immutable(BigInt)))
        {{
            T1 t1 = 2;
            T2 t2 = t1;

            T2 t2_1 = to!T2(t1);
            T2 t2_2 = cast(T2) t1;

            assert(t2 == t1);
            assert(t2 == 2);

            assert(t2_1 == t1);
            assert(t2_1 == 2);

            assert(t2_2 == t1);
            assert(t2_2 == 2);
        }}
    }

    BigInt n = 2;
    n *= 2;
    assert(n == 4);
}

// https://issues.dlang.org/show_bug.cgi?id=8167
@safe unittest
{
    BigInt a = BigInt(3);
    BigInt b = BigInt(a);
    assert(b == 3);
}

// https://issues.dlang.org/show_bug.cgi?id=9061
@safe unittest
{
    long l1 = 0x12345678_90ABCDEF;
    long l2 = 0xFEDCBA09_87654321;
    long l3 = l1 | l2;
    long l4 = l1 & l2;
    long l5 = l1 ^ l2;

    BigInt b1 = l1;
    BigInt b2 = l2;
    BigInt b3 = b1 | b2;
    BigInt b4 = b1 & b2;
    BigInt b5 = b1 ^ b2;

    assert(l3 == b3);
    assert(l4 == b4);
    assert(l5 == b5);
}

// https://issues.dlang.org/show_bug.cgi?id=11600
@safe unittest
{
    import std.conv;
    import std.exception : assertThrown;

    // Original bug report
    assertThrown!ConvException(to!BigInt("avadakedavra"));

    // Digit string lookalikes that are actually invalid
    assertThrown!ConvException(to!BigInt("0123hellothere"));
    assertThrown!ConvException(to!BigInt("-hihomarylowe"));
    assertThrown!ConvException(to!BigInt("__reallynow__"));
    assertThrown!ConvException(to!BigInt("-123four"));
}

// https://issues.dlang.org/show_bug.cgi?id=11583
@safe unittest
{
    BigInt x = 0;
    assert((x > 0) == false);
}

// https://issues.dlang.org/show_bug.cgi?id=13391
@safe unittest
{
    BigInt x1 = "123456789";
    BigInt x2 = "123456789123456789";
    BigInt x3 = "123456789123456789123456789";

    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong))
    {
        assert((x1 * T.max) / T.max == x1);
        assert((x2 * T.max) / T.max == x2);
        assert((x3 * T.max) / T.max == x3);
    }

    assert(x1 / -123456789 == -1);
    assert(x1 / 123456789U == 1);
    assert(x1 / -123456789L == -1);
    assert(x1 / 123456789UL == 1);
    assert(x2 / -123456789123456789L == -1);
    assert(x2 / 123456789123456789UL == 1);

    assert(x1 / uint.max == 0);
    assert(x1 / ulong.max == 0);
    assert(x2 / ulong.max == 0);

    x1 /= 123456789UL;
    assert(x1 == 1);
    x2 /= 123456789123456789UL;
    assert(x2 == 1);
}

// https://issues.dlang.org/show_bug.cgi?id=13963
@safe unittest
{
    BigInt x = 1;
    import std.meta : AliasSeq;
    static foreach (Int; AliasSeq!(byte, ubyte, short, ushort, int))
    {
        assert(is(typeof(x % Int(1)) == int));
    }
    assert(is(typeof(x % 1U) == long));
    assert(is(typeof(x % 1L) == long));
    assert(is(typeof(x % 1UL) == BigInt));

    auto x0 = BigInt(uint.max - 1);
    auto x1 = BigInt(8);
    assert(x1 / x == x1);
    auto x2 = -BigInt(long.min) + 1;

    // uint
    assert( x0 % uint.max ==  x0 % BigInt(uint.max));
    assert(-x0 % uint.max == -x0 % BigInt(uint.max));
    assert( x0 % uint.max ==  long(uint.max - 1));
    assert(-x0 % uint.max == -long(uint.max - 1));

    // long
    assert(x1 % 2L == 0L);
    assert(-x1 % 2L == 0L);

    assert(x1 % 3L == 2L);
    assert(x1 % -3L == 2L);
    assert(-x1 % 3L == -2L);
    assert(-x1 % -3L == -2L);

    assert(x1 % 11L == 8L);
    assert(x1 % -11L == 8L);
    assert(-x1 % 11L == -8L);
    assert(-x1 % -11L == -8L);

    // ulong
    assert(x1 % 2UL == BigInt(0));
    assert(-x1 % 2UL == BigInt(0));

    assert(x1 % 3UL == BigInt(2));
    assert(-x1 % 3UL == -BigInt(2));

    assert(x1 % 11UL == BigInt(8));
    assert(-x1 % 11UL == -BigInt(8));

    assert(x2 % ulong.max == x2);
    assert(-x2 % ulong.max == -x2);
}

// https://issues.dlang.org/show_bug.cgi?id=14124
@safe unittest
{
    auto x = BigInt(-3);
    x %= 3;
    assert(!x.isNegative);
    assert(x.isZero);

    x = BigInt(-3);
    x %= cast(ushort) 3;
    assert(!x.isNegative);
    assert(x.isZero);

    x = BigInt(-3);
    x %= 3L;
    assert(!x.isNegative);
    assert(x.isZero);

    x = BigInt(3);
    x %= -3;
    assert(!x.isNegative);
    assert(x.isZero);
}

// https://issues.dlang.org/show_bug.cgi?id=15678
@safe unittest
{
    import std.exception : assertThrown;
    assertThrown!ConvException(BigInt(""));
    assertThrown!ConvException(BigInt("0x1234BARF"));
    assertThrown!ConvException(BigInt("1234PUKE"));
}

// https://issues.dlang.org/show_bug.cgi?id=6447
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota;

    auto s = BigInt(1_000_000_000_000);
    auto e = BigInt(1_000_000_000_003);
    auto r = iota(s, e);
    assert(r.equal([
        BigInt(1_000_000_000_000),
        BigInt(1_000_000_000_001),
        BigInt(1_000_000_000_002)
    ]));
}

// https://issues.dlang.org/show_bug.cgi?id=17330
@safe unittest
{
    auto b = immutable BigInt("123");
    assert(b == 123);
}

// https://issues.dlang.org/show_bug.cgi?id=14767
@safe pure unittest
{
    static immutable a = BigInt("340282366920938463463374607431768211455");
    assert(a == BigInt("340282366920938463463374607431768211455"));

    BigInt plusTwo(in BigInt n)
    {
        return n + 2;
    }

    enum BigInt test1 = BigInt(123);
    enum BigInt test2 = plusTwo(test1);
    assert(test2 == 125);
}

/**
 * Finds the quotient and remainder for the given dividend and divisor in one operation.
 *
 * Params:
 *     dividend = the $(LREF BigInt) to divide
 *     divisor = the $(LREF BigInt) to divide the dividend by
 *     quotient = is set to the result of the division
 *     remainder = is set to the remainder of the division
 */
void divMod(const BigInt dividend, const BigInt divisor, out BigInt quotient, out BigInt remainder) pure nothrow @safe
{
    BigUint q, r;
    BigUint.divMod(dividend.data, divisor.data, q, r);
    quotient.sign = dividend.sign != divisor.sign;
    quotient.data = q;
    remainder.sign = r.isZero() ? false : dividend.sign;
    remainder.data = r;
}

///
@safe pure nothrow unittest
{
    auto a = BigInt(123);
    auto b = BigInt(25);
    BigInt q, r;

    divMod(a, b, q, r);

    assert(q == 4);
    assert(r == 23);
    assert(q * b + r == a);
}

// https://issues.dlang.org/show_bug.cgi?id=18086
@safe pure nothrow unittest
{
    BigInt q = 1;
    BigInt r = 1;
    BigInt c = 1024;
    BigInt d = 100;

    divMod(c, d, q, r);
    assert(q ==  10);
    assert(r ==  24);
    assert((q * d + r) == c);

    divMod(c, -d, q, r);
    assert(q == -10);
    assert(r ==  24);
    assert(q * -d + r == c);

    divMod(-c, -d, q, r);
    assert(q ==  10);
    assert(r == -24);
    assert(q * -d + r == -c);

    divMod(-c, d, q, r);
    assert(q == -10);
    assert(r == -24);
    assert(q * d + r == -c);
}

// https://issues.dlang.org/show_bug.cgi?id=22771
@safe pure nothrow unittest
{
    BigInt quotient, remainder;
    divMod(BigInt(-50), BigInt(1), quotient, remainder);
    assert(remainder == 0);
}

// https://issues.dlang.org/show_bug.cgi?id=19740
@safe unittest
{
    BigInt a = BigInt(
        "241127122100380210001001124020210001001100000200003101000062221012075223052000021042250111300200000000000" ~
        "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
    BigInt b = BigInt(
        "700200000000500418321000401140010110000022007221432000000141020011323301104104060202100200457210001600142" ~
        "000001012245300100001110215200000000120000000000000000000000000000000000000000000000000000000000000000000" ~
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");

    BigInt c = a * b;
    assert(c == BigInt(
        "1688372108948068874722901180228375682334987075822938736581472847151834613694489486296103575639363261807341" ~
        "3910091006778604956808730652275328822700182498926542563654351871390166691461743896850906716336187966456064" ~
        "2702007176328110013356024000000000000000000000000000000000000000000000000000000000000000000000000000000000" ~
        "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" ~
        "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));
}

@safe unittest
{
    auto n = BigInt("1234"d);
}

/**
Fast power modulus calculation for $(LREF BigInt) operands.
Params:
     base = the $(LREF BigInt) is basic operands.
     exponent = the $(LREF BigInt) is power exponent of base.
     modulus = the $(LREF BigInt) is modules to be modular of base ^ exponent.
Returns:
     The power modulus value of (base ^ exponent) % modulus.
*/
BigInt powmod(BigInt base, BigInt exponent, BigInt modulus) pure nothrow @safe
{
    BigInt result = 1;

    while (exponent)
    {
        if (exponent.data.peekUint(0) & 1)
        {
            result = (result * base) % modulus;
        }

        auto tmp = base % modulus;
        base = (tmp * tmp) % modulus;
        exponent >>= 1;
    }

    return result;
}

/// for powmod
@safe unittest
{
    BigInt base = BigInt("123456789012345678901234567890");
    BigInt exponent = BigInt("1234567890123456789012345678901234567");
    BigInt modulus = BigInt("1234567");

    BigInt result = powmod(base, exponent, modulus);
    assert(result == 359079);
}
