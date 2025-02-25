@system unittest
{
    import std.parallelism;

    import std.algorithm.iteration : map;
    import std.math.operations : isClose;
    import std.parallelism : taskPool;
    import std.range : iota;

    // Parallel reduce can be combined with
    // std.algorithm.iteration.map to interesting effect.
    // The following example (thanks to Russel Winder)
    // calculates pi by quadrature  using
    // std.algorithm.map and TaskPool.reduce.
    // getTerm is evaluated in parallel as needed by
    // TaskPool.reduce.
    //
    // Timings on an Intel i5-3450 quad core machine
    // for n = 1_000_000_000:
    //
    // TaskPool.reduce:       1.067 s
    // std.algorithm.reduce:  4.011 s

    enum n = 1_000_000;
    enum delta = 1.0 / n;

    alias getTerm = (int i)
    {
        immutable x = ( i - 0.5 ) * delta;
        return delta / ( 1.0 + x * x ) ;
    };

    immutable pi = 4.0 * taskPool.reduce!"a + b"(n.iota.map!getTerm);

    assert(pi.isClose(3.14159, 1e-5));
}

