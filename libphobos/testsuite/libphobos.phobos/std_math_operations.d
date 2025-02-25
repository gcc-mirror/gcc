@safe @nogc pure nothrow unittest
{
    import std.math.operations;

    import std.math.traits : isNaN;

    real a = NaN(1_000_000);
    assert(isNaN(a));
    assert(getNaNPayload(a) == 1_000_000);
}

@safe @nogc pure nothrow unittest
{
    import std.math.operations;

    import std.math.traits : isNaN;

    real a = NaN(1_000_000);
    assert(isNaN(a));
    assert(getNaNPayload(a) == 1_000_000);
}

@safe @nogc pure nothrow unittest
{
    import std.math.operations;

    assert(nextUp(1.0 - 1.0e-6).feqrel(0.999999) > 16);
    assert(nextUp(1.0 - real.epsilon).feqrel(1.0) > 16);
}

@safe pure nothrow @nogc unittest
{
    import std.math.operations;

    assert( nextDown(1.0 + real.epsilon) == 1.0);
}

@safe pure nothrow @nogc unittest
{
    import std.math.operations;

    import std.math.traits : isNaN;

    float a = 1;
    assert(is(typeof(nextafter(a, a)) == float));
    assert(nextafter(a, a.infinity) > a);
    assert(isNaN(nextafter(a, a.nan)));
    assert(isNaN(nextafter(a.nan, a)));

    double b = 2;
    assert(is(typeof(nextafter(b, b)) == double));
    assert(nextafter(b, b.infinity) > b);
    assert(isNaN(nextafter(b, b.nan)));
    assert(isNaN(nextafter(b.nan, b)));

    real c = 3;
    assert(is(typeof(nextafter(c, c)) == real));
    assert(nextafter(c, c.infinity) > c);
    assert(isNaN(nextafter(c, c.nan)));
    assert(isNaN(nextafter(c.nan, c)));
}

@safe pure nothrow @nogc unittest
{
    import std.math.operations;

    import std.math.traits : isNaN;

    assert(fdim(2.0, 0.0) == 2.0);
    assert(fdim(-2.0, 0.0) == 0.0);
    assert(fdim(real.infinity, 2.0) == real.infinity);
    assert(isNaN(fdim(real.nan, 2.0)));
    assert(isNaN(fdim(2.0, real.nan)));
    assert(isNaN(fdim(real.nan, real.nan)));
}

@safe pure nothrow @nogc unittest
{
    import std.math.operations;

    import std.meta : AliasSeq;
    static foreach (F; AliasSeq!(float, double, real))
    {
        assert(fmax(F(0.0), F(2.0)) == 2.0);
        assert(fmax(F(-2.0), 0.0) == F(0.0));
        assert(fmax(F.infinity, F(2.0)) == F.infinity);
        assert(fmax(F.nan, F(2.0)) == F(2.0));
        assert(fmax(F(2.0), F.nan) == F(2.0));
    }
}

@safe pure nothrow @nogc unittest
{
    import std.math.operations;

    import std.meta : AliasSeq;
    static foreach (F; AliasSeq!(float, double, real))
    {
        assert(fmin(F(0.0), F(2.0)) == 0.0);
        assert(fmin(F(-2.0), F(0.0)) == -2.0);
        assert(fmin(F.infinity, F(2.0)) == 2.0);
        assert(fmin(F.nan, F(2.0)) == 2.0);
        assert(fmin(F(2.0), F.nan) == 2.0);
    }
}

@safe pure nothrow @nogc unittest
{
    import std.math.operations;

    assert(fma(0.0, 2.0, 2.0) == 2.0);
    assert(fma(2.0, 2.0, 2.0) == 6.0);
    assert(fma(real.infinity, 2.0, 2.0) == real.infinity);
    assert(fma(real.nan, 2.0, 2.0) is real.nan);
    assert(fma(2.0, 2.0, real.nan) is real.nan);
}

@safe pure unittest
{
    import std.math.operations;

    assert(feqrel(2.0, 2.0) == 53);
    assert(feqrel(2.0f, 2.0f) == 24);
    assert(feqrel(2.0, double.nan) == 0);

    // Test that numbers are within n digits of each
    // other by testing if feqrel > n * log2(10)

    // five digits
    assert(feqrel(2.0, 2.00001) > 16);
    // ten digits
    assert(feqrel(2.0, 2.00000000001) > 33);
}

@safe pure nothrow @nogc unittest
{
    import std.math.operations;

    assert(isClose(1.0,0.999_999_999));
    assert(isClose(0.001, 0.000_999_999_999));
    assert(isClose(1_000_000_000.0,999_999_999.0));

    assert(isClose(17.123_456_789, 17.123_456_78));
    assert(!isClose(17.123_456_789, 17.123_45));

    // use explicit 3rd parameter for less (or more) accuracy
    assert(isClose(17.123_456_789, 17.123_45, 1e-6));
    assert(!isClose(17.123_456_789, 17.123_45, 1e-7));

    // use 4th parameter when comparing close to zero
    assert(!isClose(1e-100, 0.0));
    assert(isClose(1e-100, 0.0, 0.0, 1e-90));
    assert(!isClose(1e-10, -1e-10));
    assert(isClose(1e-10, -1e-10, 0.0, 1e-9));
    assert(!isClose(1e-300, 1e-298));
    assert(isClose(1e-300, 1e-298, 0.0, 1e-200));

    // different default limits for different floating point types
    assert(isClose(1.0f, 0.999_99f));
    assert(!isClose(1.0, 0.999_99));
    static if (real.sizeof > double.sizeof)
        assert(!isClose(1.0L, 0.999_999_999L));
}

@safe pure nothrow unittest
{
    import std.math.operations;

    assert(isClose([1.0, 2.0, 3.0], [0.999_999_999, 2.000_000_001, 3.0]));
    assert(!isClose([1.0, 2.0], [0.999_999_999, 2.000_000_001, 3.0]));
    assert(!isClose([1.0, 2.0, 3.0], [0.999_999_999, 2.000_000_001]));

    assert(isClose([2.0, 1.999_999_999, 2.000_000_001], 2.0));
    assert(isClose(2.0, [2.0, 1.999_999_999, 2.000_000_001]));
}

@safe unittest
{
    import std.math.operations;

    assert(cmp(-double.infinity, -double.max) < 0);
    assert(cmp(-double.max, -100.0) < 0);
    assert(cmp(-100.0, -0.5) < 0);
    assert(cmp(-0.5, 0.0) < 0);
    assert(cmp(0.0, 0.5) < 0);
    assert(cmp(0.5, 100.0) < 0);
    assert(cmp(100.0, double.max) < 0);
    assert(cmp(double.max, double.infinity) < 0);

    assert(cmp(1.0, 1.0) == 0);
}

@safe unittest
{
    import std.math.operations;

    assert(cmp(-0.0, +0.0) < 0);
    assert(cmp(+0.0, -0.0) > 0);
}

@safe unittest
{
    import std.math.operations;

    assert(cmp(-double.nan, -double.infinity) < 0);
    assert(cmp(double.infinity, double.nan) < 0);
    assert(cmp(-double.nan, double.nan) < 0);
}

@safe unittest
{
    import std.math.operations;

    assert(cmp(NaN(10), NaN(20)) < 0);
    assert(cmp(-NaN(20), -NaN(10)) < 0);
}

