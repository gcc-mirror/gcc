@safe unittest
{
    import std.numeric;

    import std.math.trigonometry : sin, cos;

    // Define a 16-bit floating point values
    CustomFloat!16                                x;     // Using the number of bits
    CustomFloat!(10, 5)                           y;     // Using the precision and exponent width
    CustomFloat!(10, 5,CustomFloatFlags.ieee)     z;     // Using the precision, exponent width and format flags
    CustomFloat!(10, 5,CustomFloatFlags.ieee, 15) w;     // Using the precision, exponent width, format flags and exponent offset bias

    // Use the 16-bit floats mostly like normal numbers
    w = x*y - 1;

    // Functions calls require conversion
    z = sin(+x)           + cos(+y);                     // Use unary plus to concisely convert to a real
    z = sin(x.get!float)  + cos(y.get!float);            // Or use get!T
    z = sin(cast(float) x) + cos(cast(float) y);           // Or use cast(T) to explicitly convert

    // Define a 8-bit custom float for storing probabilities
    alias Probability = CustomFloat!(4, 4, CustomFloatFlags.ieee^CustomFloatFlags.probability^CustomFloatFlags.signed );
    auto p = Probability(0.5);
}

@safe unittest
{
    import std.numeric;

    import std.math.operations : isClose;

    // Average numbers in an array
    double avg(in double[] a)
    {
        if (a.length == 0) return 0;
        FPTemporary!double result = 0;
        foreach (e; a) result += e;
        return result / a.length;
    }

    auto a = [1.0, 2.0, 3.0];
    assert(isClose(avg(a), 2));
}

@safe unittest
{
    import std.numeric;

    import std.math.operations : isClose;
    import std.math.trigonometry : cos;

    float f(float x)
    {
        return cos(x) - x*x*x;
    }
    auto x = secantMethod!(f)(0f, 1f);
    assert(isClose(x, 0.865474));
}

@safe unittest
{
    import std.numeric;

    import std.math.operations : isClose;

    auto ret = findLocalMin((double x) => (x-4)^^2, -1e7, 1e7);
    assert(ret.x.isClose(4.0));
    assert(ret.y.isClose(0.0, 0.0, 1e-10));
}

@safe unittest
{
    import std.numeric;

    double[] a = [];
    assert(!normalize(a));
    a = [ 1.0, 3.0 ];
    assert(normalize(a));
    assert(a == [ 0.25, 0.75 ]);
    assert(normalize!(typeof(a))(a, 50)); // a = [12.5, 37.5]
    a = [ 0.0, 0.0 ];
    assert(!normalize(a));
    assert(a == [ 0.5, 0.5 ]);
}

@safe unittest
{
    import std.numeric;

    import std.math.traits : isNaN;

    assert(sumOfLog2s(new double[0]) == 0);
    assert(sumOfLog2s([0.0L]) == -real.infinity);
    assert(sumOfLog2s([-0.0L]) == -real.infinity);
    assert(sumOfLog2s([2.0L]) == 1);
    assert(sumOfLog2s([-2.0L]).isNaN());
    assert(sumOfLog2s([real.nan]).isNaN());
    assert(sumOfLog2s([-real.nan]).isNaN());
    assert(sumOfLog2s([real.infinity]) == real.infinity);
    assert(sumOfLog2s([-real.infinity]).isNaN());
    assert(sumOfLog2s([ 0.25, 0.25, 0.25, 0.125 ]) == -9);
}

@safe unittest
{
    import std.numeric;

    import std.math.operations : isClose;

    double[] p = [ 0.0, 0, 0, 1 ];
    assert(kullbackLeiblerDivergence(p, p) == 0);
    double[] p1 = [ 0.25, 0.25, 0.25, 0.25 ];
    assert(kullbackLeiblerDivergence(p1, p1) == 0);
    assert(kullbackLeiblerDivergence(p, p1) == 2);
    assert(kullbackLeiblerDivergence(p1, p) == double.infinity);
    double[] p2 = [ 0.2, 0.2, 0.2, 0.4 ];
    assert(isClose(kullbackLeiblerDivergence(p1, p2), 0.0719281, 1e-5));
    assert(isClose(kullbackLeiblerDivergence(p2, p1), 0.0780719, 1e-5));
}

@safe unittest
{
    import std.numeric;

    import std.math.operations : isClose;

    double[] p = [ 0.0, 0, 0, 1 ];
    assert(jensenShannonDivergence(p, p) == 0);
    double[] p1 = [ 0.25, 0.25, 0.25, 0.25 ];
    assert(jensenShannonDivergence(p1, p1) == 0);
    assert(isClose(jensenShannonDivergence(p1, p), 0.548795, 1e-5));
    double[] p2 = [ 0.2, 0.2, 0.2, 0.4 ];
    assert(isClose(jensenShannonDivergence(p1, p2), 0.0186218, 1e-5));
    assert(isClose(jensenShannonDivergence(p2, p1), 0.0186218, 1e-5));
    assert(isClose(jensenShannonDivergence(p2, p1, 0.005), 0.00602366, 1e-5));
}

@system unittest
{
    import std.numeric;

    import std.math.operations : isClose;
    import std.math.algebraic : sqrt;

    string[] s = ["Hello", "brave", "new", "world"];
    string[] t = ["Hello", "new", "world"];
    assert(gapWeightedSimilarity(s, s, 1) == 15);
    assert(gapWeightedSimilarity(t, t, 1) == 7);
    assert(gapWeightedSimilarity(s, t, 1) == 7);
    assert(isClose(gapWeightedSimilarityNormalized(s, t, 1),
                    7.0 / sqrt(15.0 * 7), 0.01));
}

@system unittest
{
    import std.numeric;

    string[] s = ["Hello", "brave", "new", "world"];
    string[] t = ["Hello", "new", "world"];
    auto simIter = gapWeightedSimilarityIncremental(s, t, 1.0);
    assert(simIter.front == 3); // three 1-length matches
    simIter.popFront();
    assert(simIter.front == 3); // three 2-length matches
    simIter.popFront();
    assert(simIter.front == 1); // one 3-length match
    simIter.popFront();
    assert(simIter.empty);     // no more match
}

@safe unittest
{
    import std.numeric;

    assert(gcd(2 * 5 * 7 * 7, 5 * 7 * 11) == 5 * 7);
    const int a = 5 * 13 * 23 * 23, b = 13 * 59;
    assert(gcd(a, b) == 13);
}

@safe unittest
{
    import std.numeric;

    assert(lcm(1, 2) == 2);
    assert(lcm(3, 4) == 12);
    assert(lcm(5, 6) == 30);
}

@safe pure @nogc unittest
{
    import std.numeric;

    ubyte[21] fac;
    size_t idx = decimalToFactorial(2982, fac);

    assert(fac[0] == 4);
    assert(fac[1] == 0);
    assert(fac[2] == 4);
    assert(fac[3] == 1);
    assert(fac[4] == 0);
    assert(fac[5] == 0);
    assert(fac[6] == 0);
}

