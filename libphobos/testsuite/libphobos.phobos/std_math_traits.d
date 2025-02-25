@safe pure nothrow @nogc unittest
{
    import std.math.traits;

    assert( isNaN(float.init));
    assert( isNaN(-double.init));
    assert( isNaN(real.nan));
    assert( isNaN(-real.nan));
    assert(!isNaN(cast(float) 53.6));
    assert(!isNaN(cast(real)-53.6));
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits;

    assert( isFinite(1.23f));
    assert( isFinite(float.max));
    assert( isFinite(float.min_normal));
    assert(!isFinite(float.nan));
    assert(!isFinite(float.infinity));
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits;

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

@safe pure nothrow @nogc unittest
{
    import std.math.traits;

    import std.meta : AliasSeq;

    static foreach (T; AliasSeq!(float, double, real))
    {{
        T f;
        for (f = 1.0; !isSubnormal(f); f /= 2)
            assert(f != 0);
    }}
}

@nogc @safe pure nothrow unittest
{
    import std.math.traits;

    assert(!isInfinity(float.init));
    assert(!isInfinity(-float.init));
    assert(!isInfinity(float.nan));
    assert(!isInfinity(-float.nan));
    assert(isInfinity(float.infinity));
    assert(isInfinity(-float.infinity));
    assert(isInfinity(-1.0f / 0.0f));
}

@safe @nogc pure nothrow unittest
{
    import std.math.traits;

    // We're forcing the CTFE to run by assigning the result of the function to an enum
    enum test1 = isIdentical(1.0,1.0);
    enum test2 = isIdentical(real.nan,real.nan);
    enum test3 = isIdentical(real.infinity, real.infinity);
    enum test4 = isIdentical(real.infinity, real.infinity);
    enum test5 = isIdentical(0.0, 0.0);

    assert(test1);
    assert(test2);
    assert(test3);
    assert(test4);
    assert(test5);

    enum test6 = !isIdentical(0.0, -0.0);
    enum test7 = !isIdentical(real.nan, -real.nan);
    enum test8 = !isIdentical(real.infinity, -real.infinity);

    assert(test6);
    assert(test7);
    assert(test8);
}

@nogc @safe pure nothrow unittest
{
    import std.math.traits;

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

@safe pure nothrow @nogc unittest
{
    import std.math.traits;

    assert(copysign(1.0, 1.0) == 1.0);
    assert(copysign(1.0, -0.0) == -1.0);
    assert(copysign(1UL, -1.0) == -1.0);
    assert(copysign(-1.0, -1.0) == -1.0);

    assert(copysign(real.infinity, -1.0) == -real.infinity);
    assert(copysign(real.nan, 1.0) is real.nan);
    assert(copysign(-real.nan, 1.0) is real.nan);
    assert(copysign(real.nan, -1.0) is -real.nan);
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits;

    assert(sgn(168.1234) == 1);
    assert(sgn(-168.1234) == -1);
    assert(sgn(0.0) == 0);
    assert(sgn(-0.0) == 0);
}

@safe unittest
{
    import std.math.traits;

    import std.math.exponential : pow;

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

@safe unittest
{
    import std.math.traits;

    assert( isPowerOf2(1));
    assert( isPowerOf2(2));
    assert( isPowerOf2(1uL << 63));

    assert(!isPowerOf2(-4));
    assert(!isPowerOf2(0));
    assert(!isPowerOf2(1337u));
}

