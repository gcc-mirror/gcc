@safe unittest
{
    import std.math.trigonometry;

    import std.math.operations : isClose;

    assert(cos(0.0) == 1.0);
    assert(cos(1.0).isClose(0.5403023059));
    assert(cos(3.0).isClose(-0.9899924966));
}

@safe unittest
{
    import std.math.trigonometry;

    import std.math.constants : PI;
    import std.stdio : writefln;

    void someFunc()
    {
      real x = 30.0;
      auto result = sin(x * (PI / 180)); // convert degrees to radians
      writefln("The sine of %s degrees is %s", x, result);
    }
}

@safe unittest
{
    import std.math.trigonometry;

    import std.math.operations : isClose;
    import std.math.traits : isIdentical;
    import std.math.constants : PI;
    import std.math.algebraic : sqrt;

    assert(isIdentical(tan(0.0), 0.0));
    assert(tan(PI).isClose(0, 0.0, 1e-10));
    assert(tan(PI / 3).isClose(sqrt(3.0)));
}

@safe unittest
{
    import std.math.trigonometry;

    import std.math.operations : isClose;
    import std.math.traits : isNaN;
    import std.math.constants : PI;

    assert(acos(0.0).isClose(1.570796327));
    assert(acos(0.5).isClose(PI / 3));
    assert(acos(PI).isNaN);
}

@safe unittest
{
    import std.math.trigonometry;

    import std.math.operations : isClose;
    import std.math.traits : isIdentical, isNaN;
    import std.math.constants : PI;

    assert(isIdentical(asin(0.0), 0.0));
    assert(asin(0.5).isClose(PI / 6));
    assert(asin(PI).isNaN);
}

@safe unittest
{
    import std.math.trigonometry;

    import std.math.operations : isClose;
    import std.math.traits : isIdentical;
    import std.math.constants : PI;
    import std.math.algebraic : sqrt;

    assert(isIdentical(atan(0.0), 0.0));
    assert(atan(sqrt(3.0)).isClose(PI / 3));
}

@safe unittest
{
    import std.math.trigonometry;

    import std.math.operations : isClose;
    import std.math.constants : PI;
    import std.math.algebraic : sqrt;

    assert(atan2(1.0, sqrt(3.0)).isClose(PI / 6));
}

@safe unittest
{
    import std.math.trigonometry;

    import std.math.constants : E;
    import std.math.operations : isClose;

    assert(cosh(0.0) == 1.0);
    assert(cosh(1.0).isClose((E + 1.0 / E) / 2));
}

@safe unittest
{
    import std.math.trigonometry;

    import std.math.constants : E;
    import std.math.operations : isClose;
    import std.math.traits : isIdentical;

    enum sinh1 = (E - 1.0 / E) / 2;
    import std.meta : AliasSeq;
    static foreach (F; AliasSeq!(float, double, real))
    {
        assert(isIdentical(sinh(F(0.0)), F(0.0)));
        assert(sinh(F(1.0)).isClose(F(sinh1)));
    }
}

@safe unittest
{
    import std.math.trigonometry;

    import std.math.operations : isClose;
    import std.math.traits : isIdentical;

    assert(isIdentical(tanh(0.0), 0.0));
    assert(tanh(1.0).isClose(sinh(1.0) / cosh(1.0)));
}

@safe @nogc nothrow unittest
{
    import std.math.trigonometry;

    import std.math.traits : isIdentical, isNaN;

    assert(isNaN(acosh(0.9)));
    assert(isNaN(acosh(real.nan)));
    assert(isIdentical(acosh(1.0), 0.0));
    assert(acosh(real.infinity) == real.infinity);
    assert(isNaN(acosh(0.5)));
}

@safe @nogc nothrow unittest
{
    import std.math.trigonometry;

    import std.math.traits : isIdentical, isNaN;

    assert(isIdentical(asinh(0.0), 0.0));
    assert(isIdentical(asinh(-0.0), -0.0));
    assert(asinh(real.infinity) == real.infinity);
    assert(asinh(-real.infinity) == -real.infinity);
    assert(isNaN(asinh(real.nan)));
}

@safe @nogc nothrow unittest
{
    import std.math.trigonometry;

    import std.math.traits : isIdentical, isNaN;

    assert(isIdentical(atanh(0.0), 0.0));
    assert(isIdentical(atanh(-0.0),-0.0));
    assert(isNaN(atanh(real.nan)));
    assert(isNaN(atanh(-real.infinity)));
    assert(atanh(0.0) == 0);
}

