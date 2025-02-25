@safe pure nothrow @nogc unittest
{
    import std.math.rounding;

    import std.math.traits : isNaN;

    assert(ceil(+123.456L) == +124);
    assert(ceil(-123.456L) == -123);
    assert(ceil(-1.234L) == -1);
    assert(ceil(-0.123L) == 0);
    assert(ceil(0.0L) == 0);
    assert(ceil(+0.123L) == 1);
    assert(ceil(+1.234L) == 2);
    assert(ceil(real.infinity) == real.infinity);
    assert(isNaN(ceil(real.nan)));
    assert(isNaN(ceil(real.init)));
}

@safe pure nothrow @nogc unittest
{
    import std.math.rounding;

    import std.math.traits : isNaN;

    assert(floor(+123.456L) == +123);
    assert(floor(-123.456L) == -124);
    assert(floor(+123.0L) == +123);
    assert(floor(-124.0L) == -124);
    assert(floor(-1.234L) == -2);
    assert(floor(-0.123L) == -1);
    assert(floor(0.0L) == 0);
    assert(floor(+0.123L) == 0);
    assert(floor(+1.234L) == 1);
    assert(floor(real.infinity) == real.infinity);
    assert(isNaN(floor(real.nan)));
    assert(isNaN(floor(real.init)));
}

@safe pure nothrow @nogc unittest
{
    import std.math.rounding;

    import std.math.operations : isClose;

    assert(isClose(12345.6789L.quantize(0.01L), 12345.68L));
    assert(isClose(12345.6789L.quantize!floor(0.01L), 12345.67L));
    assert(isClose(12345.6789L.quantize(22.0L), 12342.0L));
}

@safe pure nothrow @nogc unittest
{
    import std.math.rounding;

    import std.math.operations : isClose;
    import std.math.traits : isNaN;

    assert(isClose(12345.6789L.quantize(0), 12345.6789L));
    assert(12345.6789L.quantize(real.infinity).isNaN);
    assert(12345.6789L.quantize(real.nan).isNaN);
    assert(real.infinity.quantize(0.01L) == real.infinity);
    assert(real.infinity.quantize(real.nan).isNaN);
    assert(real.nan.quantize(0.01L).isNaN);
    assert(real.nan.quantize(real.infinity).isNaN);
    assert(real.nan.quantize(real.nan).isNaN);
}

@safe pure nothrow @nogc unittest
{
    import std.math.rounding;

    import std.math.operations : isClose;

    assert(isClose(12345.6789L.quantize!10(-2), 12345.68L));
    assert(isClose(12345.6789L.quantize!(10, -2), 12345.68L));
    assert(isClose(12345.6789L.quantize!(10, floor)(-2), 12345.67L));
    assert(isClose(12345.6789L.quantize!(10, -2, floor), 12345.67L));

    assert(isClose(12345.6789L.quantize!22(1), 12342.0L));
    assert(isClose(12345.6789L.quantize!22, 12342.0L));
}

@safe pure unittest
{
    import std.math.rounding;

    import std.math.traits : isNaN;

    assert(nearbyint(0.4) == 0);
    assert(nearbyint(0.5) == 0);
    assert(nearbyint(0.6) == 1);
    assert(nearbyint(100.0) == 100);

    assert(isNaN(nearbyint(real.nan)));
    assert(nearbyint(real.infinity) == real.infinity);
    assert(nearbyint(-real.infinity) == -real.infinity);
}

@safe unittest
{
    import std.math.rounding;

    import std.math.traits : isNaN;

    version (IeeeFlagsSupport) resetIeeeFlags();
    assert(rint(0.4) == 0);
    version (GNU) { /* inexact bit not set with enabled optimizations */ } else
    version (IeeeFlagsSupport) assert(ieeeFlags.inexact);

    assert(rint(0.5) == 0);
    assert(rint(0.6) == 1);
    assert(rint(100.0) == 100);

    assert(isNaN(rint(real.nan)));
    assert(rint(real.infinity) == real.infinity);
    assert(rint(-real.infinity) == -real.infinity);
}

@safe pure nothrow @nogc unittest
{
    import std.math.rounding;

    assert(lrint(4.5) == 4);
    assert(lrint(5.5) == 6);
    assert(lrint(-4.5) == -4);
    assert(lrint(-5.5) == -6);

    assert(lrint(int.max - 0.5) == 2147483646L);
    assert(lrint(int.max + 0.5) == 2147483648L);
    assert(lrint(int.min - 0.5) == -2147483648L);
    assert(lrint(int.min + 0.5) == -2147483648L);
}

@safe nothrow @nogc unittest
{
    import std.math.rounding;

    assert(round(4.5) == 5);
    assert(round(5.4) == 5);
    assert(round(-4.5) == -5);
    assert(round(-5.1) == -5);
}

@safe nothrow @nogc unittest
{
    import std.math.rounding;

    assert(lround(0.49) == 0);
    assert(lround(0.5) == 1);
    assert(lround(1.5) == 2);
}

@safe pure unittest
{
    import std.math.rounding;

    assert(trunc(0.01) == 0);
    assert(trunc(0.49) == 0);
    assert(trunc(0.5) == 0);
    assert(trunc(1.5) == 1);
}

@safe unittest
{
    import std.math.rounding;

    assert(rndtol(1.0) == 1L);
    assert(rndtol(1.2) == 1L);
    assert(rndtol(1.7) == 2L);
    assert(rndtol(1.0001) == 1L);
}

