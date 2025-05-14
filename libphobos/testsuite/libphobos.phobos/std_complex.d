@safe pure nothrow unittest
{
    import std.complex;

    auto a = complex(1.0);
    static assert(is(typeof(a) == Complex!double));
    assert(a.re == 1.0);
    assert(a.im == 0.0);

    auto b = complex(2.0L);
    static assert(is(typeof(b) == Complex!real));
    assert(b.re == 2.0L);
    assert(b.im == 0.0L);

    auto c = complex(1.0, 2.0);
    static assert(is(typeof(c) == Complex!double));
    assert(c.re == 1.0);
    assert(c.im == 2.0);

    auto d = complex(3.0, 4.0L);
    static assert(is(typeof(d) == Complex!real));
    assert(d.re == 3.0);
    assert(d.im == 4.0L);

    auto e = complex(1);
    static assert(is(typeof(e) == Complex!double));
    assert(e.re == 1);
    assert(e.im == 0);

    auto f = complex(1L, 2);
    static assert(is(typeof(f) == Complex!double));
    assert(f.re == 1L);
    assert(f.im == 2);

    auto g = complex(3, 4.0L);
    static assert(is(typeof(g) == Complex!real));
    assert(g.re == 3);
    assert(g.im == 4.0L);
}

@safe unittest
{
    import std.complex;

        auto c = complex(1.2, 3.4);

        // Vanilla toString formatting:
        assert(c.toString() == "1.2+3.4i");

        // Formatting with std.string.format specs: the precision and width
        // specifiers apply to both the real and imaginary parts of the
        // complex number.
        import std.format : format;
        assert(format("%.2f", c)  == "1.20+3.40i");
        assert(format("%4.1f", c) == " 1.2+ 3.4i");
    
}

@safe pure nothrow unittest
{
    import std.complex;

    static import core.math;
    assert(abs(complex(1.0)) == 1.0);
    assert(abs(complex(0.0, 1.0)) == 1.0);
    assert(abs(complex(1.0L, -2.0L)) == core.math.sqrt(5.0L));
}

@safe pure nothrow unittest
{
    import std.complex;

    import std.math.operations : isClose;
    assert(sqAbs(complex(0.0)) == 0.0);
    assert(sqAbs(complex(1.0)) == 1.0);
    assert(sqAbs(complex(0.0, 1.0)) == 1.0);
    assert(isClose(sqAbs(complex(1.0L, -2.0L)), 5.0L));
    assert(isClose(sqAbs(complex(-3.0L, 1.0L)), 10.0L));
    assert(isClose(sqAbs(complex(1.0f,-1.0f)), 2.0f));
}

@safe pure nothrow unittest
{
    import std.complex;

    import std.math.constants : PI_2, PI_4;
    assert(arg(complex(1.0)) == 0.0);
    assert(arg(complex(0.0L, 1.0L)) == PI_2);
    assert(arg(complex(1.0L, 1.0L)) == PI_4);
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import std.math.operations : isClose;
    import std.math.constants : PI;
    assert(norm(complex(3.0, 4.0)) == 25.0);
    assert(norm(fromPolar(5.0, 0.0)) == 25.0);
    assert(isClose(norm(fromPolar(5.0L, PI / 6)), 25.0L));
    assert(isClose(norm(fromPolar(5.0L, 13 * PI / 6)), 25.0L));
}

@safe pure nothrow unittest
{
    import std.complex;

    assert(conj(complex(1.0)) == complex(1.0));
    assert(conj(complex(1.0, 2.0)) == complex(1.0, -2.0));
}

@safe pure nothrow unittest
{
    import std.complex;

    assert(proj(complex(1.0)) == complex(1.0));
    assert(proj(complex(double.infinity, 5.0)) == complex(double.infinity, 0.0));
    assert(proj(complex(5.0, -double.infinity)) == complex(double.infinity, -0.0));
}

@safe pure nothrow unittest
{
    import std.complex;

    import core.math;
    import std.math.operations : isClose;
    import std.math.algebraic : sqrt;
    import std.math.constants : PI_4;
    auto z = fromPolar(core.math.sqrt(2.0L), PI_4);
    assert(isClose(z.re, 1.0L));
    assert(isClose(z.im, 1.0L));
}

@safe pure nothrow unittest
{
    import std.complex;

    static import core.math;
    assert(sin(complex(0.0)) == 0.0);
    assert(sin(complex(2.0, 0)) == core.math.sin(2.0));
}

@safe pure nothrow unittest
{
    import std.complex;

    static import core.math;
    static import std.math;
    assert(cos(complex(0.0)) == 1.0);
    assert(cos(complex(1.3, 0.0)) == core.math.cos(1.3));
    assert(cos(complex(0.0, 5.2)) == std.math.cosh(5.2));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    static import std.math;

    int ceqrel(T)(const Complex!T x, const Complex!T y) @safe pure nothrow @nogc
    {
        import std.math.operations : feqrel;
        const r = feqrel(x.re, y.re);
        const i = feqrel(x.im, y.im);
        return r < i ? r : i;
    }
    assert(ceqrel(tan(complex(1.0, 0.0)), complex(std.math.tan(1.0), 0.0)) >= double.mant_dig - 2);
    assert(ceqrel(tan(complex(0.0, 1.0)), complex(0.0, std.math.tanh(1.0))) >= double.mant_dig - 2);
}

@safe pure nothrow unittest
{
    import std.complex;

    import std.math.operations : isClose;
    import std.math.constants : PI;
    assert(asin(complex(0.0)) == 0.0);
    assert(isClose(asin(complex(0.5L)), PI / 6));
}

@safe pure nothrow unittest
{
    import std.complex;

    import std.math.operations : isClose;
    import std.math.constants : PI;
    import std.math.trigonometry : std_math_acos = acos;
    assert(acos(complex(0.0)) == std_math_acos(0.0));
    assert(isClose(acos(complex(0.5L)), PI / 3));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import std.math.operations : isClose;
    import std.math.constants : PI;
    assert(atan(complex(0.0)) == 0.0);
    assert(isClose(atan(sqrt(complex(3.0L))), PI / 3));
    assert(isClose(atan(sqrt(complex(3.0f))), float(PI) / 3));
}

@safe pure nothrow unittest
{
    import std.complex;

    static import std.math;
    assert(sinh(complex(0.0)) == 0.0);
    assert(sinh(complex(1.0L)) == std.math.sinh(1.0L));
    assert(sinh(complex(1.0f)) == std.math.sinh(1.0f));
}

@safe pure nothrow unittest
{
    import std.complex;

    static import std.math;
    assert(cosh(complex(0.0)) == 1.0);
    assert(cosh(complex(1.0L)) == std.math.cosh(1.0L));
    assert(cosh(complex(1.0f)) == std.math.cosh(1.0f));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_tanh = tanh;
    assert(tanh(complex(0.0)) == 0.0);
    assert(isClose(tanh(complex(1.0L)), std_math_tanh(1.0L)));
    assert(isClose(tanh(complex(1.0f)), std_math_tanh(1.0f)));
}

@safe pure nothrow unittest
{
    import std.complex;

    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_asinh = asinh;
    assert(asinh(complex(0.0)) == 0.0);
    assert(isClose(asinh(complex(1.0L)), std_math_asinh(1.0L)));
    assert(isClose(asinh(complex(1.0f)), std_math_asinh(1.0f)));
}

@safe pure nothrow unittest
{
    import std.complex;

    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_acosh = acosh;
    assert(acosh(complex(1.0)) == 0.0);
    assert(isClose(acosh(complex(3.0L)), std_math_acosh(3.0L)));
    assert(isClose(acosh(complex(3.0f)), std_math_acosh(3.0f)));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_atanh = atanh;
    assert(atanh(complex(0.0)) == 0.0);
    assert(isClose(atanh(complex(0.5L)), std_math_atanh(0.5L)));
    assert(isClose(atanh(complex(0.5f)), std_math_atanh(0.5f)));
}

@safe pure nothrow unittest
{
    import std.complex;

    import core.math : cos, sin;
    assert(expi(0.0L) == 1.0L);
    assert(expi(1.3e5L) == complex(cos(1.3e5L), sin(1.3e5L)));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import std.math.trigonometry : cosh, sinh;
    assert(coshisinh(3.0L) == complex(cosh(3.0L), sinh(3.0L)));
}

@safe pure nothrow unittest
{
    import std.complex;

    static import core.math;
    assert(sqrt(complex(0.0)) == 0.0);
    assert(sqrt(complex(1.0L, 0)) == core.math.sqrt(1.0L));
    assert(sqrt(complex(-1.0L, 0)) == complex(0, 1.0L));
    assert(sqrt(complex(-8.0, -6.0)) == complex(1.0, -3.0));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import std.math.operations : isClose;
    import std.math.constants : PI;

    assert(exp(complex(0.0, 0.0)) == complex(1.0, 0.0));

    auto a = complex(2.0, 1.0);
    assert(exp(conj(a)) == conj(exp(a)));

    auto b = exp(complex(0.0L, 1.0L) * PI);
    assert(isClose(b, -1.0L, 0.0, 1e-15));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import core.math : sqrt;
    import std.math.constants : PI;
    import std.math.operations : isClose;

    auto a = complex(2.0, 1.0);
    assert(log(conj(a)) == conj(log(a)));

    auto b = 2.0 * log10(complex(0.0, 1.0));
    auto c = 4.0 * log10(complex(sqrt(2.0) / 2, sqrt(2.0) / 2));
    assert(isClose(b, c, 0.0, 1e-15));

    assert(log(complex(-1.0L, 0.0L)) == complex(0.0L, PI));
    assert(log(complex(-1.0L, -0.0L)) == complex(0.0L, -PI));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import core.math : sqrt;
    import std.math.constants : LN10, PI;
    import std.math.operations : isClose;

    auto a = complex(2.0, 1.0);
    assert(log10(a) == log(a) / log(complex(10.0)));

    auto b = log10(complex(0.0, 1.0)) * 2.0;
    auto c = log10(complex(sqrt(2.0) / 2, sqrt(2.0) / 2)) * 4.0;
    assert(isClose(b, c, 0.0, 1e-15));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import std.math.operations : isClose;

    auto a = complex(1.0, 2.0);
    assert(pow(a, 2) == a * a);
    assert(pow(a, 3) == a * a * a);
    assert(pow(a, -2) == 1.0 / (a * a));
    assert(isClose(pow(a, -3), 1.0 / (a * a * a)));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import std.math.operations : isClose;
    assert(pow(complex(0.0), 2.0) == complex(0.0));
    assert(pow(complex(5.0), 2.0) == complex(25.0));

    auto a = pow(complex(-1.0, 0.0), 0.5);
    assert(isClose(a, complex(0.0, +1.0), 0.0, 1e-16));

    auto b = pow(complex(-1.0, -0.0), 0.5);
    assert(isClose(b, complex(0.0, -1.0), 0.0, 1e-16));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import std.math.operations : isClose;
    import std.math.exponential : exp;
    import std.math.constants : PI;
    auto a = complex(0.0);
    auto b = complex(2.0);
    assert(pow(a, b) == complex(0.0));

    auto c = complex(0.0L, 1.0L);
    assert(isClose(pow(c, c), exp((-PI) / 2)));
}

@safe pure nothrow @nogc unittest
{
    import std.complex;

    import std.math.operations : isClose;
    assert(pow(2.0, complex(0.0)) == complex(1.0));
    assert(pow(2.0, complex(5.0)) == complex(32.0));

    auto a = pow(-2.0, complex(-1.0));
    assert(isClose(a, complex(-0.5), 0.0, 1e-16));

    auto b = pow(-0.5, complex(-1.0));
    assert(isClose(b, complex(-2.0), 0.0, 1e-15));
}

