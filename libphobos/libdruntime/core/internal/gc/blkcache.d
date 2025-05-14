/**
BlkInfo thread-local cache. Used for array appending in the conservative GC to avoid the lock when possible.

Note: this used to be in rt.lifetime, but was moved here to allow GCs to take over array operations.
*/
module core.internal.gc.blkcache;

import core.memory;
import core.attribute;

debug (PRINTF) import core.stdc.stdio : printf;

alias BlkInfo = GC.BlkInfo;
alias BlkAttr = GC.BlkAttr;

/**
  cache for the lookup of the block info
  */
private enum N_CACHE_BLOCKS = 8;

// note this is TLS, so no need to sync.
BlkInfo *__blkcache_storage;

static if (N_CACHE_BLOCKS == 1)
{
    version=single_cache;
}
else
{
    //version=simple_cache; // uncomment to test simple cache strategy
    //version=random_cache; // uncomment to test random cache strategy

    // ensure N_CACHE_BLOCKS is power of 2.
    static assert(!((N_CACHE_BLOCKS - 1) & N_CACHE_BLOCKS));

    version (random_cache)
    {
        int __nextRndNum = 0;
    }
    int __nextBlkIdx;
}

@property BlkInfo *__blkcache() nothrow @nogc
{
    if (!__blkcache_storage)
    {
        import core.stdc.stdlib : calloc;
        import core.thread.threadbase;
        auto tBase = ThreadBase.getThis();
        if (tBase is null)
            // if we don't have a thread object, this is a detached thread, and
            // this won't be properly maintained by the GC.
            return null;

        // allocate the block cache for the first time
        immutable size = BlkInfo.sizeof * N_CACHE_BLOCKS;
        // use C alloc, because this may become a detached thread, and the GC
        // would then clean up the cache without zeroing this pointer.
        __blkcache_storage = cast(BlkInfo*) calloc(size, 1);
        tBase.tlsGCData = __blkcache_storage;
    }
    return __blkcache_storage;
}

// free the allocation on thread exit.
@standalone static ~this()
{
    if (__blkcache_storage)
    {
        import core.stdc.stdlib : free;
        import core.thread.threadbase;
        auto tBase = ThreadBase.getThis();
        if (tBase !is null)
            tBase.tlsGCData = null;
        free(__blkcache_storage);
        __blkcache_storage = null;
    }
}

/**
 * Indicates whether an address has been marked by the GC.
 */
enum IsMarked : int
{
         no, /// Address is not marked.
        yes, /// Address is marked.
    unknown, /// Address is not managed by the GC.
}

alias IsMarkedDg = IsMarked delegate(void* addr) nothrow; /// The isMarked callback function.

// we expect this to be called with the lock in place
void processGCMarks(void* data, scope IsMarkedDg isMarked) nothrow
{
    if (!data)
        return;

    auto cache = cast(BlkInfo*) data;
    // called after the mark routine to eliminate block cache data when it
    // might be ready to sweep

    debug(PRINTF) printf("processing GC Marks, %p\n", cache);
    debug(PRINTF) foreach (i; 0 .. N_CACHE_BLOCKS)
    {
        printf("cache entry %d has base ptr %p\tsize %zd\tflags %x\n", i, cache[i].base, cache[i].size, cache[i].attr);
    }
    auto cache_end = cache + N_CACHE_BLOCKS;
    for (;cache < cache_end; ++cache)
    {
        if (cache.base != null && isMarked(cache.base) == IsMarked.no)
        {
            debug(PRINTF) printf("clearing cache entry at %p\n", cache.base);
            cache.base = null; // clear that data.
        }
    }
}

unittest
{
    // Bugzilla 10701 - segfault in GC
    ubyte[] result; result.length = 4096;
    GC.free(result.ptr);
    GC.collect();
}

/**
  Get the cached block info of an interior pointer.  Returns null if the
  interior pointer's block is not cached.

  NOTE: The following note was not valid, but is retained for historical
        purposes. The data cannot be cleared because the stack contains a
        reference to the affected block (e.g. through `interior`). Therefore,
        the element will not be collected, and the data will remain valid.

  ORIGINAL: The base ptr in this struct can be cleared asynchronously by the GC,
        so any use of the returned BlkInfo should copy it and then check the
        base ptr of the copy before actually using it.
  */
BlkInfo *__getBlkInfo(void *interior) nothrow @nogc
{
    BlkInfo *ptr = __blkcache;
    if (ptr is null)
        // if for some reason we don't have a cache, return null.
        return null;
    version (single_cache)
    {
        if (ptr.base && ptr.base <= interior && (interior - ptr.base) < ptr.size)
            return ptr;
        return null; // not in cache.
    }
    else version (simple_cache)
    {
        foreach (i; 0..N_CACHE_BLOCKS)
        {
            if (ptr.base && ptr.base <= interior && (interior - ptr.base) < ptr.size)
                return ptr;
            ptr++;
        }
    }
    else
    {
        // try to do a smart lookup, using __nextBlkIdx as the "head"
        auto curi = ptr + __nextBlkIdx;
        for (auto i = curi; i >= ptr; --i)
        {
            if (i.base && i.base <= interior && cast(size_t)(interior - i.base) < i.size)
                return i;
        }

        for (auto i = ptr + N_CACHE_BLOCKS - 1; i > curi; --i)
        {
            if (i.base && i.base <= interior && cast(size_t)(interior - i.base) < i.size)
                return i;
        }
    }
    return null; // not in cache.
}

void __insertBlkInfoCache(BlkInfo bi, BlkInfo *curpos) nothrow @nogc
{
    auto cache = __blkcache;
    if (cache is null)
        // no cache to use.
        return;

    version (single_cache)
    {
        *cache = bi;
        return;
    }
    else
    {
        version (simple_cache)
        {
            if (curpos)
                *curpos = bi;
            else
            {
                // note, this is a super-simple algorithm that does not care about
                // most recently used.  It simply uses a round-robin technique to
                // cache block info.  This means that the ordering of the cache
                // doesn't mean anything.  Certain patterns of allocation may
                // render the cache near-useless.
                cache[__nextBlkIdx] = bi;
                __nextBlkIdx = (__nextBlkIdx+1) & (N_CACHE_BLOCKS - 1);
            }
        }
        else version (random_cache)
        {
            // strategy: if the block currently is in the cache, move the
            // current block index to the a random element and evict that
            // element.
            if (!curpos)
            {
                __nextBlkIdx = (__nextRndNum = 1664525 * __nextRndNum + 1013904223) & (N_CACHE_BLOCKS - 1);
                curpos = cache + __nextBlkIdx;
            }
            else
            {
                __nextBlkIdx = curpos - cache;
            }
            *curpos = bi;
        }
        else
        {
            //
            // strategy: If the block currently is in the cache, swap it with
            // the head element.  Otherwise, move the head element up by one,
            // and insert it there.
            //
            if (!curpos)
            {
                __nextBlkIdx = (__nextBlkIdx+1) & (N_CACHE_BLOCKS - 1);
                curpos = cache + __nextBlkIdx;
            }
            else if (curpos !is cache + __nextBlkIdx)
            {
                *curpos = cache[__nextBlkIdx];
                curpos = cache + __nextBlkIdx;
            }
            *curpos = bi;
        }
    }
}

debug(PRINTF)
{
    extern(C) void printArrayCache()
    {
        auto ptr = __blkcache;
        printf("CACHE: \n");
        foreach (i; 0 .. N_CACHE_BLOCKS)
        {
            printf("  %d\taddr:% .8p\tsize:% .10zd\tflags:% .8x\n", i, ptr[i].base, ptr[i].size, ptr[i].attr);
        }
    }
}
