@safe pure nothrow @nogc unittest
{
    import std.math.exponential;

    import std.math.operations : feqrel;

    assert(pow(2.0, 5) == 32.0);
    assert(pow(1.5, 9).feqrel(38.4433) > 16);
    assert(pow(real.nan, 2) is real.nan);
    assert(pow(real.infinity, 2) == real.infinity);
}

@safe pure nothrow @nogc unittest
{
    import std.math.exponential;

    assert(pow(2, 3) == 8);
    assert(pow(3, 2) == 9);

    assert(pow(2, 10) == 1_024);
    assert(pow(2, 20) == 1_048_576);
    assert(pow(2, 30) == 1_073_741_824);

    assert(pow(0, 0) == 1);

    assert(pow(1, -5) == 1);
    assert(pow(1, -6) == 1);
    assert(pow(-1, -5) == -1);
    assert(pow(-1, -6) == 1);

    assert(pow(-2, 5) == -32);
    assert(pow(-2, -5) == 0);
    assert(pow(cast(double) -2, -5) == -0.03125);
}

@safe pure nothrow @nogc unittest
{
    import std.math.exponential;

    assert(pow(2, 5.0) == 32.0);
    assert(pow(7, 3.0) == 343.0);
    assert(pow(2, real.nan) is real.nan);
    assert(pow(2, real.infinity) == real.infinity);
}

@safe pure nothrow @nogc unittest
{
    import std.math.exponential;

    import std.math.operations : isClose;

    assert(isClose(pow(2.0, 3.0), 8.0));
    assert(isClose(pow(1.5, 10.0), 57.6650390625));

    // square root of 9
    assert(isClose(pow(9.0, 0.5), 3.0));
    // 10th root of 1024
    assert(isClose(pow(1024.0, 0.1), 2.0));

    assert(isClose(pow(-4.0, 3.0), -64.0));

    // reciprocal of 4 ^^ 2
    assert(isClose(pow(4.0, -2.0), 0.0625));
    // reciprocal of (-2) ^^ 3
    assert(isClose(pow(-2.0, -3.0), -0.125));

    assert(isClose(pow(-2.5, 3.0), -15.625));
    // reciprocal of 2.5 ^^ 3
    assert(isClose(pow(2.5, -3.0), 0.064));
    // reciprocal of (-2.5) ^^ 3
    assert(isClose(pow(-2.5, -3.0), -0.064));

    // reciprocal of square root of 4
    assert(isClose(pow(4.0, -0.5), 0.5));

    // per definition
    assert(isClose(pow(0.0, 0.0), 1.0));
}

@safe pure nothrow @nogc unittest
{
    import std.math.exponential;

    import std.math.operations : isClose;

    // the result is a complex number
    // which cannot be represented as floating point number
    import std.math.traits : isNaN;
    assert(isNaN(pow(-2.5, -1.5)));

    // use the ^^-operator of std.complex instead
    import std.complex : complex;
    auto c1 = complex(-2.5, 0.0);
    auto c2 = complex(-1.5, 0.0);
    auto result = c1 ^^ c2;
    // exact result apparently depends on `real` precision => increased tolerance
    assert(isClose(result.re, -4.64705438e-17, 2e-4));
    assert(isClose(result.im, 2.52982e-1, 2e-4));
}

@safe pure nothrow @nogc unittest
{
    import std.math.exponential;

    assert(powmod(1U, 10U, 3U) == 1);
    assert(powmod(3U, 2U, 6U) == 3);
    assert(powmod(5U, 5U, 15U) == 5);
    assert(powmod(2U, 3U, 5U) == 3);
    assert(powmod(2U, 4U, 5U) == 1);
    assert(powmod(2U, 5U, 5U) == 2);
}

@safe unittest
{
    import std.math.exponential;

    import std.math.operations : feqrel;
    import std.math.constants : E;

    assert(exp(0.0) == 1.0);
    assert(exp(3.0).feqrel(E * E * E) > 16);
}

@safe unittest
{
    import std.math.exponential;

    import std.math.traits : isIdentical;
    import std.math.operations : feqrel;

    assert(isIdentical(expm1(0.0), 0.0));
    assert(expm1(1.0).feqrel(1.71828) > 16);
    assert(expm1(2.0).feqrel(6.3890) > 16);
}

@safe unittest
{
    import std.math.exponential;

    import std.math.traits : isIdentical;
    import std.math.operations : feqrel;

    assert(isIdentical(exp2(0.0), 1.0));
    assert(exp2(2.0).feqrel(4.0) > 16);
    assert(exp2(8.0).feqrel(256.0) > 16);
}

@safe unittest
{
    import std.math.exponential;

    import std.math.operations : isClose;

    int exp;
    real mantissa = frexp(123.456L, exp);

    assert(isClose(mantissa * pow(2.0L, cast(real) exp), 123.456L));

    assert(frexp(-real.nan, exp) && exp == int.min);
    assert(frexp(real.nan, exp) && exp == int.min);
    assert(frexp(-real.infinity, exp) == -real.infinity && exp == int.min);
    assert(frexp(real.infinity, exp) == real.infinity && exp == int.max);
    assert(frexp(-0.0, exp) == -0.0 && exp == 0);
    assert(frexp(0.0, exp) == 0.0 && exp == 0);
}

@safe pure unittest
{
    import std.math.exponential;

    assert(ilogb(1) == 0);
    assert(ilogb(3) == 1);
    assert(ilogb(3.0) == 1);
    assert(ilogb(100_000_000) == 26);

    assert(ilogb(0) == FP_ILOGB0);
    assert(ilogb(0.0) == FP_ILOGB0);
    assert(ilogb(double.nan) == FP_ILOGBNAN);
    assert(ilogb(double.infinity) == int.max);
}

@safe pure unittest
{
    import std.math.exponential;

    assert(ilogb(0) == FP_ILOGB0);
    assert(ilogb(0.0) == FP_ILOGB0);
    assert(ilogb(double.nan) == FP_ILOGBNAN);
}

@nogc @safe pure nothrow unittest
{
    import std.math.exponential;

    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(float, double, real))
    {{
        T r;

        r = ldexp(3.0L, 3);
        assert(r == 24);

        r = ldexp(cast(T) 3.0, cast(int) 3);
        assert(r == 24);

        T n = 3.0;
        int exp = 3;
        r = ldexp(n, exp);
        assert(r == 24);
    }}
}

@safe pure nothrow @nogc unittest
{
    import std.math.exponential;

    import std.math.operations : feqrel;
    import std.math.constants : E;

    assert(feqrel(log(E), 1) >= real.mant_dig - 1);
}

@safe pure nothrow @nogc unittest
{
    import std.math.exponential;

    import std.math.algebraic : fabs;

    assert(fabs(log10(1000.0L) - 3) < .000001);
}

@safe pure unittest
{
    import std.math.exponential;

    import std.math.traits : isIdentical, isNaN;
    import std.math.operations : feqrel;

    assert(isIdentical(log1p(0.0), 0.0));
    assert(log1p(1.0).feqrel(0.69314) > 16);

    assert(log1p(-1.0) == -real.infinity);
    assert(isNaN(log1p(-2.0)));
    assert(log1p(real.nan) is real.nan);
    assert(log1p(-real.nan) is -real.nan);
    assert(log1p(real.infinity) == real.infinity);
}

@safe unittest
{
    import std.math.exponential;

    import std.math.operations : isClose;

    assert(isClose(log2(1024.0L), 10));
}

@safe @nogc nothrow unittest
{
    import std.math.exponential;

    assert(logb(1.0) == 0);
    assert(logb(100.0) == 6);

    assert(logb(0.0) == -real.infinity);
    assert(logb(real.infinity) == real.infinity);
    assert(logb(-real.infinity) == real.infinity);
}

@safe pure nothrow @nogc unittest
{
    import std.math.exponential;

    assert(scalbn(0x1.2345678abcdefp0L, 999) == 0x1.2345678abcdefp999L);
    assert(scalbn(-real.infinity, 5) == -real.infinity);
    assert(scalbn(2.0,10) == 2048.0);
    assert(scalbn(2048.0f,-10) == 2.0f);
}

