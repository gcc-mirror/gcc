@safe pure nothrow @nogc unittest
{
    import std.math.algebraic;

    import std.math.traits : isIdentical, isNaN;

    assert(isIdentical(abs(-0.0L), 0.0L));
    assert(isNaN(abs(real.nan)));
    assert(abs(-real.infinity) == real.infinity);
    assert(abs(-56) == 56);
    assert(abs(2321312L)  == 2321312L);
    assert(abs(23u) == 23u);
}

@safe unittest
{
    import std.math.algebraic;

    import std.math.traits : isIdentical;

    assert(isIdentical(fabs(0.0f), 0.0f));
    assert(isIdentical(fabs(-0.0f), 0.0f));
    assert(fabs(-10.0f) == 10.0f);

    assert(isIdentical(fabs(0.0), 0.0));
    assert(isIdentical(fabs(-0.0), 0.0));
    assert(fabs(-10.0) == 10.0);

    assert(isIdentical(fabs(0.0L), 0.0L));
    assert(isIdentical(fabs(-0.0L), 0.0L));
    assert(fabs(-10.0L) == 10.0L);
}

@safe pure nothrow @nogc unittest
{
    import std.math.algebraic;

    import std.math.operations : feqrel;
    import std.math.traits : isNaN;

    assert(sqrt(2.0).feqrel(1.4142) > 16);
    assert(sqrt(9.0).feqrel(3.0) > 16);

    assert(isNaN(sqrt(-1.0f)));
    assert(isNaN(sqrt(-1.0)));
    assert(isNaN(sqrt(-1.0L)));
}

@safe unittest
{
    import std.math.algebraic;

    import std.math.operations : feqrel;

    assert(cbrt(1.0).feqrel(1.0) > 16);
    assert(cbrt(27.0).feqrel(3.0) > 16);
    assert(cbrt(15.625).feqrel(2.5) > 16);
}

@safe unittest
{
    import std.math.algebraic;

    import std.math.operations : feqrel;

    assert(hypot(1.0, 1.0).feqrel(1.4142) > 16);
    assert(hypot(3.0, 4.0).feqrel(5.0) > 16);
    assert(hypot(real.infinity, 1.0L) == real.infinity);
    assert(hypot(real.infinity, real.nan) == real.infinity);
}

@safe unittest
{
    import std.math.algebraic;

    import std.math.operations : isClose;

    assert(isClose(hypot(1.0, 2.0, 2.0), 3.0));
    assert(isClose(hypot(2.0, 3.0, 6.0), 7.0));
    assert(isClose(hypot(1.0, 4.0, 8.0), 9.0));
}

@safe nothrow @nogc unittest
{
    import std.math.algebraic;

    real x = 3.1L;
    static real[] pp = [56.1L, 32.7L, 6];

    assert(poly(x, pp) == (56.1L + (32.7L + 6.0L * x) * x));
}

@safe @nogc pure nothrow unittest
{
    import std.math.algebraic;

    assert(nextPow2(2) == 4);
    assert(nextPow2(10) == 16);
    assert(nextPow2(4000) == 4096);

    assert(nextPow2(-2) == -4);
    assert(nextPow2(-10) == -16);

    assert(nextPow2(uint.max) == 0);
    assert(nextPow2(uint.min) == 0);
    assert(nextPow2(size_t.max) == 0);
    assert(nextPow2(size_t.min) == 0);

    assert(nextPow2(int.max) == 0);
    assert(nextPow2(int.min) == 0);
    assert(nextPow2(long.max) == 0);
    assert(nextPow2(long.min) == 0);
}

@safe @nogc pure nothrow unittest
{
    import std.math.algebraic;

    assert(nextPow2(2.1) == 4.0);
    assert(nextPow2(-2.0) == -4.0);
    assert(nextPow2(0.25) == 0.5);
    assert(nextPow2(-4.0) == -8.0);

    assert(nextPow2(double.max) == 0.0);
    assert(nextPow2(double.infinity) == double.infinity);
}

@safe @nogc pure nothrow unittest
{
    import std.math.algebraic;

    assert(truncPow2(3) == 2);
    assert(truncPow2(4) == 4);
    assert(truncPow2(10) == 8);
    assert(truncPow2(4000) == 2048);

    assert(truncPow2(-5) == -4);
    assert(truncPow2(-20) == -16);

    assert(truncPow2(uint.max) == int.max + 1);
    assert(truncPow2(uint.min) == 0);
    assert(truncPow2(ulong.max) == long.max + 1);
    assert(truncPow2(ulong.min) == 0);

    assert(truncPow2(int.max) == (int.max / 2) + 1);
    assert(truncPow2(int.min) == int.min);
    assert(truncPow2(long.max) == (long.max / 2) + 1);
    assert(truncPow2(long.min) == long.min);
}

@safe @nogc pure nothrow unittest
{
    import std.math.algebraic;

    assert(truncPow2(2.1) == 2.0);
    assert(truncPow2(7.0) == 4.0);
    assert(truncPow2(-1.9) == -1.0);
    assert(truncPow2(0.24) == 0.125);
    assert(truncPow2(-7.0) == -4.0);

    assert(truncPow2(double.infinity) == double.infinity);
}

