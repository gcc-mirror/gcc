@safe unittest
{
    import std.math.remainder;

    import std.math.operations : feqrel;
    import std.math.traits : isIdentical, isNaN;

    assert(isIdentical(fmod(0.0, 1.0), 0.0));
    assert(fmod(5.0, 3.0).feqrel(2.0) > 16);
    assert(isNaN(fmod(5.0, 0.0)));
}

@safe unittest
{
    import std.math.remainder;

    import std.math.operations : feqrel;

    real frac;
    real intpart;

    frac = modf(3.14159, intpart);
    assert(intpart.feqrel(3.0) > 16);
    assert(frac.feqrel(0.14159) > 16);
}

@safe @nogc nothrow unittest
{
    import std.math.remainder;

    import std.math.operations : feqrel;
    import std.math.traits : isNaN;

    assert(remainder(5.1, 3.0).feqrel(-0.9) > 16);
    assert(remainder(-5.1, 3.0).feqrel(0.9) > 16);
    assert(remainder(0.0, 3.0) == 0.0);

    assert(isNaN(remainder(1.0, 0.0)));
    assert(isNaN(remainder(-1.0, 0.0)));
}

@safe @nogc nothrow unittest
{
    import std.math.remainder;

    import std.math.operations : feqrel;

    int n;

    assert(remquo(5.1, 3.0, n).feqrel(-0.9) > 16 && n == 2);
    assert(remquo(-5.1, 3.0, n).feqrel(0.9) > 16 && n == -2);
    assert(remquo(0.0, 3.0, n) == 0.0 && n == 0);
}

