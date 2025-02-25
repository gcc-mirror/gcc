pure @safe unittest
{
    import std.bigint;

        ubyte[] magnitude = [1, 2, 3, 4, 5, 6];
        auto b1 = BigInt(false, magnitude);
        assert(cast(long) b1 == 0x01_02_03_04_05_06L);
        auto b2 = BigInt(true, magnitude);
        assert(cast(long) b2 == -0x01_02_03_04_05_06L);
    
}

@safe unittest
{
    import std.bigint;

        ulong data = 1_000_000_000_000;
        auto bigData = BigInt(data);
        assert(bigData == BigInt("1_000_000_000_000"));
    
}

@safe unittest
{
    import std.bigint;

        const(BigInt) b1 = BigInt("1_234_567_890");
        BigInt b2 = BigInt(b1);
        assert(b2 == BigInt("1_234_567_890"));
    
}

@safe unittest
{
    import std.bigint;

        auto b = BigInt("123");
        b = 456;
        assert(b == BigInt("456"));
    
}

@safe unittest
{
    import std.bigint;

        auto b1 = BigInt("123");
        auto b2 = BigInt("456");
        b2 = b1;
        assert(b2 == BigInt("123"));
    
}

@safe unittest
{
    import std.bigint;

        auto b = BigInt("1_000_000_000");

        b += 12345;
        assert(b == BigInt("1_000_012_345"));

        b /= 5;
        assert(b == BigInt("200_002_469"));
    
}

@safe unittest
{
    import std.bigint;

        auto x = BigInt("123");
        auto y = BigInt("321");
        x += y;
        assert(x == BigInt("444"));
    
}

@safe unittest
{
    import std.bigint;

        auto x = BigInt("123");
        auto y = BigInt("456");
        BigInt z = x * y;
        assert(z == BigInt("56088"));
    
}

@safe unittest
{
    import std.bigint;

        auto x = BigInt("123");
        x *= 300;
        assert(x == BigInt("36900"));
    
}

@safe unittest
{
    import std.bigint;

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

@safe unittest
{
    import std.bigint;

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

@safe unittest
{
    import std.bigint;

        auto x = BigInt("1234");
        assert(-x == BigInt("-1234"));

        ++x;
        assert(x == BigInt("1235"));
    
}

@safe unittest
{
    import std.bigint;

        // Note that when comparing a BigInt to a float or double the
        // full precision of the BigInt is always considered, unlike
        // when comparing an int to a float or a long to a double.
        assert(BigInt(123456789) != cast(float) 123456789);
    
}

@safe unittest
{
    import std.bigint;

        // Non-zero values are regarded as true
        auto x = BigInt("1");
        auto y = BigInt("10");
        assert(x);
        assert(y);

        // Zero value is regarded as false
        auto z = BigInt("0");
        assert(!z);
    
}

@safe unittest
{
    import std.bigint;

        import std.conv : to, ConvOverflowException;
        import std.exception : assertThrown;

        assert(BigInt("0").to!int == 0);

        assert(BigInt("0").to!ubyte == 0);
        assert(BigInt("255").to!ubyte == 255);
        assertThrown!ConvOverflowException(BigInt("256").to!ubyte);
        assertThrown!ConvOverflowException(BigInt("-1").to!ubyte);
    
}

@system unittest
{
    import std.bigint;

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

@system unittest
{
    import std.bigint;

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
    import std.bigint;

        const(BigInt) x = BigInt("123");
        BigInt y = cast() x;    // cast away const
        assert(y == x);
    
}

@safe unittest
{
    import std.bigint;

        auto x = BigInt("100");
        auto y = BigInt("10");
        int z = 50;
        const int w = 200;

        assert(y < x);
        assert(x > z);
        assert(z > y);
        assert(x < w);
    
}

@safe unittest
{
    import std.bigint;

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
    import std.bigint;

        auto b = BigInt("12345");
        long l = b.toLong();
        assert(l == 12345);
    
}

@safe unittest
{
    import std.bigint;

        auto big = BigInt("5_000_000");
        auto i = big.toInt();
        assert(i == 5_000_000);

        // Numbers that are too big to fit into an int will be clamped to int.max.
        auto tooBig = BigInt("5_000_000_000");
        i = tooBig.toInt();
        assert(i == int.max);
    
}

@safe unittest
{
    import std.bigint;

        import std.format : format;

        auto x = BigInt("1_000_000");
        x *= 12345;

        assert(format("%d", x) == "12345000000");
        assert(format("%x", x) == "2_dfd1c040");
        assert(format("%X", x) == "2_DFD1C040");
        assert(format("%o", x) == "133764340100");
    
}

@safe pure unittest
{
    import std.bigint;

        string[BigInt] aa;
        aa[BigInt(123)] = "abc";
        aa[BigInt(456)] = "def";

        assert(aa[BigInt(123)] == "abc");
        assert(aa[BigInt(456)] == "def");
    
}

@safe pure unittest
{
    import std.bigint;

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

@safe unittest
{
    import std.bigint;

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

@safe pure unittest
{
    import std.bigint;

    auto x = BigInt("123");
    x *= 1000;
    x += 456;

    auto xstr = x.toDecimalString();
    assert(xstr == "123456");
}

@safe unittest
{
    import std.bigint;

    auto x = BigInt("123");
    x *= 1000;
    x += 456;

    auto xstr = x.toHex();
    assert(xstr == "1E240");
}

nothrow pure @safe unittest
{
    import std.bigint;

    assert((-1).absUnsign == 1);
    assert(1.absUnsign == 1);
}

@safe pure nothrow unittest
{
    import std.bigint;

    auto a = BigInt(123);
    auto b = BigInt(25);
    BigInt q, r;

    divMod(a, b, q, r);

    assert(q == 4);
    assert(r == 23);
    assert(q * b + r == a);
}

@safe unittest
{
    import std.bigint;

    BigInt base = BigInt("123456789012345678901234567890");
    BigInt exponent = BigInt("1234567890123456789012345678901234567");
    BigInt modulus = BigInt("1234567");

    BigInt result = powmod(base, exponent, modulus);
    assert(result == 359079);
}

