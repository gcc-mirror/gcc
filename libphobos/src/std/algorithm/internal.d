// Written in the D programming language.

/// Helper functions for std.algorithm package.
module std.algorithm.internal;


// Same as std.string.format, but "self-importing".
// Helps reduce code and imports, particularly in static asserts.
// Also helps with missing imports errors.
package template algoFormat()
{
    import std.format : format;
    alias algoFormat = format;
}

// Internal random array generators
version (StdUnittest)
{
    package enum size_t maxArraySize = 50;
    package enum size_t minArraySize = maxArraySize - 1;

    package string[] rndstuff(T : string)()
    {
        import std.random : Xorshift, uniform;

        static rnd = Xorshift(234_567_891);
        string[] result =
            new string[uniform(minArraySize, maxArraySize, rnd)];
        string alpha = "abcdefghijABCDEFGHIJ";
        foreach (ref s; result)
        {
            foreach (i; 0 .. uniform(0u, 20u, rnd))
            {
                auto j = uniform(0, alpha.length - 1, rnd);
                s ~= alpha[j];
            }
        }
        return result;
    }

    package int[] rndstuff(T : int)()
    {
        import std.random : Xorshift, uniform;

        static rnd = Xorshift(345_678_912);
        int[] result = new int[uniform(minArraySize, maxArraySize, rnd)];
        foreach (ref i; result)
        {
            i = uniform(-100, 100, rnd);
        }
        return result;
    }

    package double[] rndstuff(T : double)()
    {
        double[] result;
        foreach (i; rndstuff!(int)())
        {
            result ~= i / 50.0;
        }
        return result;
    }
}

// Used instead of `&object.member` when `member` may be
// either a field or a @property function.
package(std) T* addressOf(T)(ref T val) { return &val; }
