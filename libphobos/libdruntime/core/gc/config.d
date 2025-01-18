/**
* Contains the garbage collector configuration.
*
* Copyright: Copyright Digital Mars 2016
* License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
*/

module core.gc.config;

import core.internal.parseoptions;
import core.stdc.stdio : printf;

__gshared Config config;

struct Config
{
    bool disable;            // start disabled
    bool fork = false;       // optional concurrent behaviour
    ubyte profile;           // enable profiling with summary when terminating program
    string gc = "conservative"; // select gc implementation conservative|precise|manual

    @MemVal size_t initReserve;      // initial reserve (bytes)
    @MemVal size_t minPoolSize = 1  << 20;  // initial and minimum pool size (bytes)
    @MemVal size_t maxPoolSize = 64 << 20;  // maximum pool size (bytes)
    @MemVal size_t incPoolSize = 3  << 20;  // pool size increment (bytes)
    uint parallel = 99;      // number of additional threads for marking (limited by cpuid.threadsPerCPU-1)
    float heapSizeFactor = 2.0; // heap size to used memory ratio
    string cleanup = "collect"; // select gc cleanup method none|collect|finalize

@nogc nothrow:

    bool initialize()
    {
        return initConfigOptions(this, "gcopt");
    }

    void help() @nogc nothrow
    {
        import core.gc.registry : registeredGCFactories;

        printf("GC options are specified as white space separated assignments:
    disable:0|1    - start disabled (%d)
    fork:0|1       - set fork behaviour (%d)
    profile:0|1|2  - enable profiling with summary when terminating program (%d)
    gc:".ptr, disable, fork, profile);
        foreach (i, entry; registeredGCFactories)
        {
            if (i) printf("|");
            printf("%.*s", cast(int) entry.name.length, entry.name.ptr);
        }
        auto _initReserve = initReserve.bytes2prettyStruct;
        auto _minPoolSize = minPoolSize.bytes2prettyStruct;
        auto _maxPoolSize = maxPoolSize.bytes2prettyStruct;
        auto _incPoolSize = incPoolSize.bytes2prettyStruct;
        printf(" - select gc implementation (default = conservative)

    initReserve:N  - initial memory to reserve in MB (%lld%c)
    minPoolSize:N  - initial and minimum pool size in MB (%lld%c)
    maxPoolSize:N  - maximum pool size in MB (%lld%c)
    incPoolSize:N  - pool size increment MB (%lld%c)
    parallel:N     - number of additional threads for marking (%lld)
    heapSizeFactor:N - targeted heap size to used memory ratio (%g)
    cleanup:none|collect|finalize - how to treat live objects when terminating (collect)

    Memory-related values can use B, K, M or G suffixes.
".ptr,
               _initReserve.v, _initReserve.u,
               _minPoolSize.v, _minPoolSize.u,
               _maxPoolSize.v, _maxPoolSize.u,
               _incPoolSize.v, _incPoolSize.u,
               cast(long)parallel, heapSizeFactor);
    }

    string errorName() @nogc nothrow { return "GC"; }
}

private struct PrettyBytes
{
    long v;
    char u; /// unit
}

pure @nogc nothrow:

private PrettyBytes bytes2prettyStruct(size_t val)
{
    char c = prettyBytes(val);

    return PrettyBytes(val, c);
}

private static char prettyBytes(ref size_t val)
{
    char sym = 'B';

    if (val == 0)
        return sym;

    char[3] units = ['K', 'M', 'G'];

    foreach (u; units)
        if (val % (1 << 10) == 0)
        {
            val /= (1 << 10);
            sym = u;
        }
        else if (sym != 'B')
            break;

    return sym;
}
unittest
{
    size_t v = 1024;
    assert(prettyBytes(v) == 'K');
    assert(v == 1);

    v = 1025;
    assert(prettyBytes(v) == 'B');
    assert(v == 1025);

    v = 1024UL * 1024 * 1024 * 3;
    assert(prettyBytes(v) == 'G');
    assert(v == 3);

    v = 1024 * 1024 + 1;
    assert(prettyBytes(v) == 'B');
    assert(v == 1024 * 1024 + 1);
}
