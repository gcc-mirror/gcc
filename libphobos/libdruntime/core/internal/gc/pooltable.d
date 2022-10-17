/**
 * A sorted array to quickly lookup pools.
 *
 * Copyright: D Language Foundation 2001 - 2021
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, David Friedman, Sean Kelly, Martin Nowak
 */
module core.internal.gc.pooltable;

static import cstdlib=core.stdc.stdlib;

struct PoolTable(Pool)
{
    import core.stdc.string : memmove;

    void Dtor() nothrow @nogc
    {
        cstdlib.free(pools);
        pools = null;
        npools = 0;
    }

    bool insert(Pool* pool) nothrow @nogc
    {
        auto newpools = cast(Pool **)cstdlib.realloc(pools, (npools + 1) * pools[0].sizeof);
        if (!newpools)
            return false;

        pools = newpools;

        // Sort pool into newpooltable[]
        size_t i;
        for (; i < npools; ++i)
        {
            if (pool.baseAddr < pools[i].baseAddr)
                break;
        }
        if (i != npools)
            memmove(pools + i + 1, pools + i, (npools - i) * pools[0].sizeof);
        pools[i] = pool;

        ++npools;

        foreach (idx; i .. npools)
            pools[idx].ptIndex = idx;

        _minAddr = pools[0].baseAddr;
        _maxAddr = pools[npools - 1].topAddr;

        return true;
    }

    @property size_t length() const scope @safe pure nothrow @nogc
    {
        return npools;
    }

    ref inout(Pool*) opIndex(size_t idx) inout return @trusted pure nothrow @nogc
    in { assert(idx < length); }
    do
    {
        return pools[idx];
    }

    inout(Pool*)[] opSlice(size_t a, size_t b) inout return @trusted pure nothrow @nogc
    in { assert(a <= length && b <= length); }
    do
    {
        return pools[a .. b];
    }

    /// Returns: A slice over all pools in this `PoolTable`
    inout(Pool*)[] opSlice() inout return @trusted pure nothrow @nogc
    {
        return this.pools[0 .. this.length];
    }

    alias opDollar = length;

    /**
     * Find Pool that pointer is in.
     * Return null if not in a Pool.
     * Assume pooltable[] is sorted.
     */
    Pool *findPool(void *p) nothrow @nogc
    {
        if (p >= minAddr && p < maxAddr)
        {
            assert(npools);

            // let dmd allocate a register for this.pools
            auto pools = this.pools;

            if (npools == 1)
                return pools[0];

            /* The pooltable[] is sorted by address, so do a binary search
             */
            size_t low = 0;
            size_t high = npools - 1;
            while (low <= high)
            {
                size_t mid = (low + high) >> 1;
                auto pool = pools[mid];
                if (p < pool.baseAddr)
                    high = mid - 1;
                else if (p >= pool.topAddr)
                    low = mid + 1;
                else
                    return pool;
            }
        }
        return null;
    }

    // semi-stable partition, returns right half for which pred is false
    Pool*[] minimize() pure nothrow @nogc
    {
        static void swap(ref Pool* a, ref Pool* b)
        {
            auto c = a; a = b; b = c;
        }

        size_t i;
        // find first bad entry
        for (; i < npools; ++i)
            if (pools[i].isFree) break;

        // move good in front of bad entries
        size_t j = i + 1;
        for (; j < npools; ++j)
        {
            if (!pools[j].isFree) // keep
            {
                swap(pools[i], pools[j]);
                pools[i].ptIndex = i;
                ++i;
            }
        }
        // npooltable[0 .. i]      => used pools
        // npooltable[i .. npools] => free pools

        if (i)
        {
            _minAddr = pools[0].baseAddr;
            _maxAddr = pools[i - 1].topAddr;
        }
        else
        {
            _minAddr = _maxAddr = null;
        }

        immutable len = npools;
        npools = i;
        // return freed pools to the caller
        return pools[npools .. len];
    }

    void Invariant() const nothrow @nogc
    {
        if (!npools) return;

        foreach (i; 0 .. npools)
            assert(pools[i].ptIndex == i);

        foreach (i, pool; pools[0 .. npools - 1])
            assert(pool.baseAddr < pools[i + 1].baseAddr);

        assert(_minAddr == pools[0].baseAddr);
        assert(_maxAddr == pools[npools - 1].topAddr);
    }

    @property const(void)* minAddr() const @safe pure nothrow @nogc { return _minAddr; }
    @property const(void)* maxAddr() const @safe pure nothrow @nogc { return _maxAddr; }

package:
    Pool** pools;
    size_t npools;
    void* _minAddr, _maxAddr;
}

unittest
{
    enum NPOOLS = 6;
    enum NPAGES = 10;
    enum PAGESIZE = 4096;

    static struct MockPool
    {
        byte* baseAddr, topAddr;
        size_t freepages, npages, ptIndex;
        @property bool isFree() const scope pure nothrow @nogc { return freepages == npages; }
    }
    PoolTable!MockPool pooltable;

    void reset()
    {
        foreach (ref pool; pooltable[0 .. $])
            pool.freepages = pool.npages;
        pooltable.minimize();
        assert(pooltable.length == 0);

        foreach (i; 0 .. NPOOLS)
        {
            auto pool = cast(MockPool*)cstdlib.malloc(MockPool.sizeof);
            *pool = MockPool.init;
            assert(pooltable.insert(pool));
        }
    }

    void usePools()
    {
        foreach (pool; pooltable[0 .. $])
        {
            pool.npages = NPAGES;
            pool.freepages = NPAGES / 2;
        }
    }

    // all pools are free
    reset();
    assert(pooltable.length == NPOOLS);
    auto freed = pooltable.minimize();
    assert(freed.length == NPOOLS);
    assert(pooltable.length == 0);

    // all pools used
    reset();
    usePools();
    assert(pooltable.length == NPOOLS);
    freed = pooltable.minimize();
    assert(freed.length == 0);
    assert(pooltable.length == NPOOLS);

    // preserves order of used pools
    reset();
    usePools();

    {
        MockPool*[NPOOLS] opools = pooltable[0 .. NPOOLS];
        // make the 2nd pool free
        pooltable[2].freepages = NPAGES;

        pooltable.minimize();
        assert(pooltable.length == NPOOLS - 1);
        assert(pooltable[0] == opools[0]);
        assert(pooltable[1] == opools[1]);
        assert(pooltable[2] == opools[3]);
    }

    // test that PoolTable reduces min/max address span
    reset();
    usePools();

    byte* base, top;

    {
        // fill with fake addresses
        size_t i;
        foreach (pool; pooltable[0 .. NPOOLS])
        {
            pool.baseAddr = cast(byte*)(i++ * NPAGES * PAGESIZE);
            pool.topAddr = pool.baseAddr + NPAGES * PAGESIZE;
        }
        base = pooltable[0].baseAddr;
        top = pooltable[NPOOLS - 1].topAddr;
    }

    freed = pooltable.minimize();
    assert(freed.length == 0);
    assert(pooltable.length == NPOOLS);
    assert(pooltable.minAddr == base);
    assert(pooltable.maxAddr == top);

    pooltable[NPOOLS - 1].freepages = NPAGES;
    pooltable[NPOOLS - 2].freepages = NPAGES;

    freed = pooltable.minimize();
    assert(freed.length == 2);
    assert(pooltable.length == NPOOLS - 2);
    assert(pooltable.minAddr == base);
    assert(pooltable.maxAddr == pooltable[NPOOLS - 3].topAddr);

    pooltable[0].freepages = NPAGES;

    freed = pooltable.minimize();
    assert(freed.length == 1);
    assert(pooltable.length == NPOOLS - 3);
    assert(pooltable.minAddr != base);
    assert(pooltable.minAddr == pooltable[0].baseAddr);
    assert(pooltable.maxAddr == pooltable[NPOOLS - 4].topAddr);

    // free all
    foreach (pool; pooltable[0 .. $])
        pool.freepages = NPAGES;
    freed = pooltable.minimize();
    assert(freed.length == NPOOLS - 3);
    assert(pooltable.length == 0);
    pooltable.Dtor();
}
