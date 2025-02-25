@safe unittest
{
    import std.random;

    import std.algorithm.comparison : among, equal;
    import std.range : iota;

    // seed a random generator with a constant
    auto rnd = Random(42);

    // Generate a uniformly-distributed integer in the range [0, 14]
    // If no random generator is passed, the global `rndGen` would be used
    auto i = uniform(0, 15, rnd);
    assert(i >= 0 && i < 15);

    // Generate a uniformly-distributed real in the range [0, 100)
    auto r = uniform(0.0L, 100.0L, rnd);
    assert(r >= 0 && r < 100);

    // Sample from a custom type
    enum Fruit { apple, mango, pear }
    auto f = rnd.uniform!Fruit;
    with(Fruit)
    assert(f.among(apple, mango, pear));

    // Generate a 32-bit random number
    auto u = uniform!uint(rnd);
    static assert(is(typeof(u) == uint));

    // Generate a random number in the range in the range [0, 1)
    auto u2 = uniform01(rnd);
    assert(u2 >= 0 && u2 < 1);

    // Select an element randomly
    auto el = 10.iota.choice(rnd);
    assert(0 <= el && el < 10);

    // Throw a dice with custom proportions
    // 0: 20%, 1: 10%, 2: 60%
    auto val = rnd.dice(0.2, 0.1, 0.6);
    assert(0 <= val && val <= 2);

    auto rnd2 = MinstdRand0(42);

    // Select a random subsample from a range
    assert(10.iota.randomSample(3, rnd2).equal([7, 8, 9]));

    // Cover all elements in an array in random order
    version (D_LP64) // https://issues.dlang.org/show_bug.cgi?id=15147
        assert(10.iota.randomCover(rnd2).equal([7, 4, 2, 0, 1, 6, 8, 3, 9, 5]));
    else
        assert(10.iota.randomCover(rnd2).equal([4, 8, 7, 3, 5, 9, 2, 6, 0, 1]));

    // Shuffle an array
    version (D_LP64) // https://issues.dlang.org/show_bug.cgi?id=15147
        assert([0, 1, 2, 4, 5].randomShuffle(rnd2).equal([2, 0, 4, 5, 1]));
    else
        assert([0, 1, 2, 4, 5].randomShuffle(rnd2).equal([4, 2, 5, 0, 1]));
}

@safe unittest
{
    import std.random;

    struct NoRng
    {
        @property uint front() {return 0;}
        @property bool empty() {return false;}
        void popFront() {}
    }
    static assert(!isUniformRNG!(NoRng));

    struct validRng
    {
        @property uint front() {return 0;}
        @property bool empty() {return false;}
        void popFront() {}

        enum isUniformRandom = true;
    }
    static assert(isUniformRNG!(validRng, uint));
    static assert(isUniformRNG!(validRng));
}

@safe unittest
{
    import std.random;

    struct validRng
    {
        @property uint front() {return 0;}
        @property bool empty() {return false;}
        void popFront() {}

        enum isUniformRandom = true;
    }
    static assert(!isSeedable!(validRng, uint));
    static assert(!isSeedable!(validRng));

    struct seedRng
    {
        @property uint front() {return 0;}
        @property bool empty() {return false;}
        void popFront() {}
        void seed(uint val){}
        enum isUniformRandom = true;
    }
    static assert(isSeedable!(seedRng, uint));
    static assert(!isSeedable!(seedRng, ulong));
    static assert(isSeedable!(seedRng));
}

@safe unittest
{
    import std.random;

    alias CPP11LCG = LinearCongruentialEngine!(uint, 48271, 0, 2_147_483_647);

    // seed with a constant
    auto rnd = CPP11LCG(42);
    auto n = rnd.front; // same for each run
    assert(n == 2027382);
}

@safe unittest
{
    import std.random;

    // glibc's LCG
    alias GLibcLCG = LinearCongruentialEngine!(uint, 1103515245, 12345, 2_147_483_648);

    // Seed with an unpredictable value
    auto rnd = GLibcLCG(unpredictableSeed);
    auto n = rnd.front; // different across runs
}

@safe unittest
{
    import std.random;

    // Visual C++'s LCG
    alias MSVCLCG = LinearCongruentialEngine!(uint, 214013, 2531011, 0);

    // seed with a constant
    auto rnd = MSVCLCG(1);
    auto n = rnd.front; // same for each run
    assert(n == 2745024);
}

@safe @nogc unittest
{
    import std.random;

    // seed with a constant
    auto rnd0 = MinstdRand0(1);
    auto n = rnd0.front;
     // same for each run
    assert(n == 16807);

    // Seed with an unpredictable value
    rnd0.seed(unpredictableSeed);
    n = rnd0.front; // different across runs
}

@safe unittest
{
    import std.random;

    // seed with a constant
    Mt19937 gen;
    auto n = gen.front; // same for each run
    assert(n == 3499211612);

    // Seed with an unpredictable value
    gen.seed(unpredictableSeed);
    n = gen.front; // different across runs
}

@safe @nogc unittest
{
    import std.random;

    // seed with a constant
    Mt19937 gen;
    auto n = gen.front; // same for each run
    assert(n == 3499211612);

    // Seed with an unpredictable value
    gen.seed(unpredictableSeed);
    n = gen.front; // different across runs
}

@safe @nogc unittest
{
    import std.random;

    // Seed with a constant
    auto gen = Mt19937_64(12345);
    auto n = gen.front; // same for each run
    assert(n == 6597103971274460346);

    // Seed with an unpredictable value
    gen.seed(unpredictableSeed!ulong);
    n = gen.front; // different across runs
}

@safe unittest
{
    import std.random;

    alias Xorshift96  = XorshiftEngine!(uint, 96,  10, 5,  26);
    auto rnd = Xorshift96(42);
    auto num = rnd.front;  // same for each run
    assert(num == 2704588748);
}

@safe @nogc unittest
{
    import std.random;

    // Seed with a constant
    auto rnd = Xorshift(1);
    auto num = rnd.front;  // same for each run
    assert(num == 1405313047);

    // Seed with an unpredictable value
    rnd.seed(unpredictableSeed);
    num = rnd.front; // different across rnd
}

@safe @nogc unittest
{
    import std.random;

    auto rnd = Random(unpredictableSeed);
    auto n = rnd.front;
    static assert(is(typeof(n) == uint));
}

@safe nothrow @nogc unittest
{
    import std.random;

    import std.algorithm.iteration : sum;
    import std.range : take;
    auto rnd = rndGen;
    assert(rnd.take(3).sum > 0);
}

@safe unittest
{
    import std.random;

    auto rnd = Random(unpredictableSeed);

    // Generate an integer in [0, 1023]
    auto a = uniform(0, 1024, rnd);
    assert(0 <= a && a < 1024);

    // Generate a float in [0, 1)
    auto b = uniform(0.0f, 1.0f, rnd);
    assert(0 <= b && b < 1);

    // Generate a float in [0, 1]
    b = uniform!"[]"(0.0f, 1.0f, rnd);
    assert(0 <= b && b <= 1);

    // Generate a float in (0, 1)
    b = uniform!"()"(0.0f, 1.0f, rnd);
    assert(0 < b && b < 1);
}

@safe unittest
{
    import std.random;

    import std.array : array;
    import std.range : generate, takeExactly;

    int[] arr = generate!(() => uniform(0, 100)).takeExactly(10).array;
    assert(arr.length == 10);
    assert(arr[0] >= 0 && arr[0] < 100);
}

@safe unittest
{
    import std.random;

    import std.conv : to;
    import std.meta : AliasSeq;
    import std.range.primitives : isForwardRange;
    import std.traits : isIntegral, isSomeChar;

    auto gen = Mt19937(123_456_789);
    static assert(isForwardRange!(typeof(gen)));

    auto a = uniform(0, 1024, gen);
    assert(0 <= a && a <= 1024);
    auto b = uniform(0.0f, 1.0f, gen);
    assert(0 <= b && b < 1, to!string(b));
    auto c = uniform(0.0, 1.0);
    assert(0 <= c && c < 1);

    static foreach (T; AliasSeq!(char, wchar, dchar, byte, ubyte, short, ushort,
                          int, uint, long, ulong, float, double, real))
    {{
        T lo = 0, hi = 100;

        // Try tests with each of the possible bounds
        {
            T init = uniform(lo, hi);
            size_t i = 50;
            while (--i && uniform(lo, hi) == init) {}
            assert(i > 0);
        }
        {
            T init = uniform!"[)"(lo, hi);
            size_t i = 50;
            while (--i && uniform(lo, hi) == init) {}
            assert(i > 0);
        }
        {
            T init = uniform!"(]"(lo, hi);
            size_t i = 50;
            while (--i && uniform(lo, hi) == init) {}
            assert(i > 0);
        }
        {
            T init = uniform!"()"(lo, hi);
            size_t i = 50;
            while (--i && uniform(lo, hi) == init) {}
            assert(i > 0);
        }
        {
            T init = uniform!"[]"(lo, hi);
            size_t i = 50;
            while (--i && uniform(lo, hi) == init) {}
            assert(i > 0);
        }

        /* Test case with closed boundaries covering whole range
         * of integral type
         */
        static if (isIntegral!T || isSomeChar!T)
        {
            foreach (immutable _; 0 .. 100)
            {
                auto u = uniform!"[]"(T.min, T.max);
                static assert(is(typeof(u) == T));
                assert(T.min <= u, "Lower bound violation for uniform!\"[]\" with " ~ T.stringof);
                assert(u <= T.max, "Upper bound violation for uniform!\"[]\" with " ~ T.stringof);
            }
        }
    }}

    auto reproRng = Xorshift(239842);

    static foreach (T; AliasSeq!(char, wchar, dchar, byte, ubyte, short,
                          ushort, int, uint, long, ulong))
    {{
        T lo = T.min + 10, hi = T.max - 10;
        T init = uniform(lo, hi, reproRng);
        size_t i = 50;
        while (--i && uniform(lo, hi, reproRng) == init) {}
        assert(i > 0);
    }}

    {
        bool sawLB = false, sawUB = false;
        foreach (i; 0 .. 50)
        {
            auto x = uniform!"[]"('a', 'd', reproRng);
            if (x == 'a') sawLB = true;
            if (x == 'd') sawUB = true;
            assert('a' <= x && x <= 'd');
        }
        assert(sawLB && sawUB);
    }

    {
        bool sawLB = false, sawUB = false;
        foreach (i; 0 .. 50)
        {
            auto x = uniform('a', 'd', reproRng);
            if (x == 'a') sawLB = true;
            if (x == 'c') sawUB = true;
            assert('a' <= x && x < 'd');
        }
        assert(sawLB && sawUB);
    }

    {
        bool sawLB = false, sawUB = false;
        foreach (i; 0 .. 50)
        {
            immutable int lo = -2, hi = 2;
            auto x = uniform!"()"(lo, hi, reproRng);
            if (x == (lo+1)) sawLB = true;
            if (x == (hi-1)) sawUB = true;
            assert(lo < x && x < hi);
        }
        assert(sawLB && sawUB);
    }

    {
        bool sawLB = false, sawUB = false;
        foreach (i; 0 .. 50)
        {
            immutable ubyte lo = 0, hi = 5;
            auto x = uniform(lo, hi, reproRng);
            if (x == lo) sawLB = true;
            if (x == (hi-1)) sawUB = true;
            assert(lo <= x && x < hi);
        }
        assert(sawLB && sawUB);
    }

    {
        foreach (i; 0 .. 30)
        {
            assert(i == uniform(i, i+1, reproRng));
        }
    }
}

@safe unittest
{
    import std.random;

    auto rnd = MinstdRand0(42);

    assert(rnd.uniform!ubyte == 102);
    assert(rnd.uniform!ulong == 4838462006927449017);

    enum Fruit { apple, mango, pear }
    version (D_LP64) // https://issues.dlang.org/show_bug.cgi?id=15147
    assert(rnd.uniform!Fruit == Fruit.mango);
}

@safe @nogc unittest
{
    import std.random;

    import std.math.operations : feqrel;

    auto rnd = MinstdRand0(42);

    // Generate random numbers in the range in the range [0, 1)
    auto u1 = uniform01(rnd);
    assert(u1 >= 0 && u1 < 1);

    auto u2 = rnd.uniform01!float;
    assert(u2 >= 0 && u2 < 1);

    // Confirm that the random values with the initial seed 42 are 0.000328707 and 0.524587
    assert(u1.feqrel(0.000328707) > 20);
    assert(u2.feqrel(0.524587) > 20);
}

@safe unittest
{
    import std.random;

    import std.algorithm.iteration : reduce;
    import std.math.operations : isClose;

    auto a = uniformDistribution(5);
    assert(a.length == 5);
    assert(isClose(reduce!"a + b"(a), 1));

    a = uniformDistribution(10, a);
    assert(a.length == 10);
    assert(isClose(reduce!"a + b"(a), 1));
}

@safe unittest
{
    import std.random;

    auto rnd = MinstdRand0(42);

    auto elem  = [1, 2, 3, 4, 5].choice(rnd);
    version (D_LP64) // https://issues.dlang.org/show_bug.cgi?id=15147
    assert(elem == 3);
}

@safe unittest
{
    import std.random;

    auto rnd = MinstdRand0(42);

    auto arr = [1, 2, 3, 4, 5].randomShuffle(rnd);
    version (D_LP64) // https://issues.dlang.org/show_bug.cgi?id=15147
    assert(arr == [3, 5, 2, 4, 1]);
}

@safe unittest
{
    import std.random;

    auto rnd = MinstdRand0(42);

    auto arr = [1, 2, 3, 4, 5, 6];
    arr = arr.dup.partialShuffle(1, rnd);

    version (D_LP64) // https://issues.dlang.org/show_bug.cgi?id=15147
    assert(arr == [2, 1, 3, 4, 5, 6]); // 1<->2

    arr = arr.dup.partialShuffle(2, rnd);
    version (D_LP64) // https://issues.dlang.org/show_bug.cgi?id=15147
    assert(arr == [1, 4, 3, 2, 5, 6]); // 1<->2, 2<->4

    arr = arr.dup.partialShuffle(3, rnd);
    version (D_LP64) // https://issues.dlang.org/show_bug.cgi?id=15147
    assert(arr == [5, 4, 6, 2, 1, 3]); // 1<->5, 2<->4, 3<->6
}

@safe unittest
{
    import std.random;

    auto d6  = 1 + dice(1, 1, 1, 1, 1, 1); // fair dice roll
    auto d6b = 1 + dice(2, 1, 1, 1, 1, 1); // double the chance to roll '1'

    auto x = dice(0.5, 0.5);   // x is 0 or 1 in equal proportions
    auto y = dice(50, 50);     // y is 0 or 1 in equal proportions
    auto z = dice(70, 20, 10); // z is 0 70% of the time, 1 20% of the time,
                               // and 2 10% of the time
}

@safe unittest
{
    import std.random;

    auto rnd = MinstdRand0(42);
    auto z = rnd.dice(70, 20, 10);
    assert(z == 0);
    z = rnd.dice(30, 20, 40, 10);
    assert(z == 2);
}

@safe unittest
{
    import std.random;

    auto rnd = Xorshift(123_456_789);
    auto i = dice(rnd, 0.0, 100.0);
    assert(i == 1);
    i = dice(rnd, 100.0, 0.0);
    assert(i == 0);

    i = dice(100U, 0U);
    assert(i == 0);
}

@safe unittest
{
    import std.random;

    import std.algorithm.comparison : equal;
    import std.range : iota;
    auto rnd = MinstdRand0(42);

    version (D_LP64) // https://issues.dlang.org/show_bug.cgi?id=15147
    assert(10.iota.randomCover(rnd).equal([7, 4, 2, 0, 1, 6, 8, 3, 9, 5]));
}

@safe unittest
{
    import std.random;

    import std.algorithm.comparison : equal;
    import std.range : iota;
    auto rnd = MinstdRand0(42);
    assert(10.iota.randomSample(3, rnd).equal([7, 8, 9]));
}

