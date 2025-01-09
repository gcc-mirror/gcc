/**
 * Contains the garbage collector implementation.
 *
 * Copyright: D Language Foundation 2001 - 2021.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, David Friedman, Sean Kelly
 */
module core.internal.gc.impl.conservative.gc;

// D Programming Language Garbage Collector implementation

/************** Debugging ***************************/

//debug = PRINTF;               // turn on printf's
//debug = PARALLEL_PRINTF;      // turn on printf's
//debug = COLLECT_PRINTF;       // turn on printf's
//debug = MARK_PRINTF;          // turn on printf's
//debug = PRINTF_TO_FILE;       // redirect printf's ouptut to file "gcx.log"
//debug = LOGGING;              // log allocations / frees
//debug = MEMSTOMP;             // stomp on memory
//debug = SENTINEL;             // add underrun/overrrun protection
                                // NOTE: this needs to be enabled globally in the makefiles
                                // (-debug=SENTINEL) to pass druntime's unittests.
//debug = PTRCHECK;             // more pointer checking
//debug = PTRCHECK2;            // thorough but slow pointer checking
//debug = INVARIANT;            // enable invariants
//debug = PROFILE_API;          // profile API calls for config.profile > 1
//debug = GC_RECURSIVE_LOCK;    // check for recursive locking on the same thread
//debug = VALGRIND;             // Valgrind memcheck integration

/***************************************************/
version = COLLECT_PARALLEL;  // parallel scanning
version (Posix)
    version = COLLECT_FORK;

import core.internal.gc.bits;
import core.internal.gc.os;
import core.gc.config;
import core.gc.gcinterface;

import core.internal.container.treap;
import core.internal.spinlock;
import core.internal.gc.pooltable;
import core.internal.gc.blkcache;

import cstdlib = core.stdc.stdlib : calloc, free, malloc, realloc;
import core.stdc.string : memcpy, memset, memmove;
import core.bitop;
import core.thread;
static import core.memory;

version (GNU) import gcc.builtins;

debug (PRINTF_TO_FILE) import core.stdc.stdio : sprintf, fprintf, fopen, fflush, FILE;
else                   import core.stdc.stdio : sprintf, printf; // needed to output profiling results

debug (VALGRIND) import etc.valgrind.valgrind;

import core.time;
alias currTime = MonoTime.currTime;

// Track total time spent preparing for GC,
// marking, sweeping and recovering pages.
__gshared Duration prepTime;
__gshared Duration markTime;
__gshared Duration sweepTime;
__gshared Duration pauseTime;
__gshared Duration maxPauseTime;
__gshared Duration maxCollectionTime;
__gshared size_t numCollections;
__gshared size_t maxPoolMemory;

__gshared long numMallocs;
__gshared long numFrees;
__gshared long numReallocs;
__gshared long numExtends;
__gshared long numOthers;
__gshared long mallocTime; // using ticks instead of MonoTime for better performance
__gshared long freeTime;
__gshared long reallocTime;
__gshared long extendTime;
__gshared long otherTime;
__gshared long lockTime;

ulong bytesAllocated;   // thread local counter

private
{
    extern (C)
    {
        // to allow compilation of this module without access to the rt package,
        //  make these functions available from rt.lifetime
        void rt_finalizeFromGC(void* p, size_t size, uint attr) nothrow;
        int rt_hasFinalizerInSegment(void* p, size_t size, uint attr, const scope void[] segment) nothrow;

        // Declared as an extern instead of importing core.exception
        // to avoid inlining - see https://issues.dlang.org/show_bug.cgi?id=13725.
        noreturn onInvalidMemoryOperationError(void* pretend_sideffect = null, string file = __FILE__, size_t line = __LINE__) @trusted pure nothrow @nogc;
        noreturn onOutOfMemoryError(void* pretend_sideffect = null, string file = __FILE__, size_t line = __LINE__) @trusted pure nothrow @nogc;

        version (COLLECT_FORK)
            version (OSX)
                pid_t __fork() nothrow;
    }

    enum
    {
        OPFAIL = ~cast(size_t)0
    }
}

alias GC gc_t;

/* ============================ GC =============================== */

// register GC in C constructor (_STI_)
private pragma(crt_constructor) void gc_conservative_ctor()
{
    _d_register_conservative_gc();
}

extern(C) void _d_register_conservative_gc()
{
    import core.gc.registry;
    registerGCFactory("conservative", &initialize);
}

private pragma(crt_constructor) void gc_precise_ctor()
{
    _d_register_precise_gc();
}

extern(C) void _d_register_precise_gc()
{
    import core.gc.registry;
    registerGCFactory("precise", &initialize_precise);
}

private GC initialize()
{
    import core.lifetime : emplace;

    auto gc = cast(ConservativeGC) cstdlib.malloc(__traits(classInstanceSize, ConservativeGC));
    if (!gc)
        onOutOfMemoryError();

    return emplace(gc);
}

private GC initialize_precise()
{
    ConservativeGC.isPrecise = true;
    return initialize();
}

class ConservativeGC : GC
{
    // For passing to debug code (not thread safe)
    __gshared size_t line;
    __gshared char*  file;

    Gcx *gcx;                   // implementation

    static gcLock = shared(AlignedSpinLock)(SpinLock.Contention.brief);
    static bool _inFinalizer;
    __gshared bool isPrecise = false;

    /*
     * Lock the GC.
     *
     * Throws: InvalidMemoryOperationError on recursive locking during finalization.
     */
    static void lockNR() @safe @nogc nothrow
    {
        if (_inFinalizer)
            onInvalidMemoryOperationError();
        gcLock.lock();
    }

    /*
     * Initialize the GC based on command line configuration.
     *
     * Throws:
     *  OutOfMemoryError if failed to initialize GC due to not enough memory.
     */
    this()
    {
        //config is assumed to have already been initialized

        gcx = cast(Gcx*)cstdlib.calloc(1, Gcx.sizeof);
        if (!gcx)
            onOutOfMemoryError();
        gcx.initialize();

        if (config.initReserve)
            gcx.reserve(config.initReserve);
        if (config.disable)
            gcx.disabled++;
    }


    ~this()
    {
        version (linux)
        {
            //debug(PRINTF) printf("Thread %x ", pthread_self());
            //debug(PRINTF) printf("GC.Dtor()\n");
        }

        if (gcx)
        {
            gcx.Dtor();
            cstdlib.free(gcx);
            gcx = null;
        }
        // TODO: cannot free as memory is overwritten and
        //  the monitor is still read in rt_finalize (called by destroy)
        // cstdlib.free(cast(void*) this);
    }


    /**
     * Enables the GC if disable() was previously called. Must be called
     * for each time disable was called in order to enable the GC again.
     */
    void enable()
    {
        static void go(Gcx* gcx) nothrow
        {
            assert(gcx.disabled > 0);
            gcx.disabled--;
        }
        runLocked!(go, otherTime, numOthers)(gcx);
    }


    /**
     * Disable the GC. The GC may still run if it deems necessary.
     */
    void disable()
    {
        static void go(Gcx* gcx) nothrow
        {
            gcx.disabled++;
        }
        runLocked!(go, otherTime, numOthers)(gcx);
    }

    debug (GC_RECURSIVE_LOCK) static bool lockedOnThisThread;

    /**
     * Run a function inside a lock/unlock set.
     *
     * Params:
     *  func = The function to run.
     *  args = The function arguments.
     */
    auto runLocked(alias func, Args...)(auto ref Args args)
    {
        debug(PROFILE_API) immutable tm = (config.profile > 1 ? currTime.ticks : 0);
        debug(GC_RECURSIVE_LOCK)
        {
            if (lockedOnThisThread)
                onInvalidMemoryOperationError();
            lockedOnThisThread = true;
        }
        lockNR();
        scope (failure) gcLock.unlock();
        debug(PROFILE_API) immutable tm2 = (config.profile > 1 ? currTime.ticks : 0);

        static if (is(typeof(func(args)) == void))
            func(args);
        else
            auto res = func(args);

        debug(PROFILE_API) if (config.profile > 1)
            lockTime += tm2 - tm;
        gcLock.unlock();
        debug(GC_RECURSIVE_LOCK)
        {
            if (!lockedOnThisThread)
                onInvalidMemoryOperationError();
            lockedOnThisThread = false;
        }

        static if (!is(typeof(func(args)) == void))
            return res;
    }

    /**
     * Run a function in an lock/unlock set that keeps track of
     * how much time was spend inside this function (in ticks)
     * and how many times this fuction was called.
     *
     * Params:
     *  func = The function to run.
     *  time = The variable keeping track of the time (in ticks).
     *  count = The variable keeping track of how many times this fuction was called.
     *  args = The function arguments.
     */
    auto runLocked(alias func, alias time, alias count, Args...)(auto ref Args args)
    {
        debug(PROFILE_API) immutable tm = (config.profile > 1 ? currTime.ticks : 0);
        debug(GC_RECURSIVE_LOCK)
        {
            if (lockedOnThisThread)
                onInvalidMemoryOperationError();
            lockedOnThisThread = true;
        }
        lockNR();
        scope (failure) gcLock.unlock();
        debug(PROFILE_API) immutable tm2 = (config.profile > 1 ? currTime.ticks : 0);

        static if (is(typeof(func(args)) == void))
            func(args);
        else
            auto res = func(args);

        debug(PROFILE_API) if (config.profile > 1)
        {
            count++;
            immutable now = currTime.ticks;
            lockTime += tm2 - tm;
            time += now - tm2;
        }
        gcLock.unlock();
        debug(GC_RECURSIVE_LOCK)
        {
            if (!lockedOnThisThread)
                onInvalidMemoryOperationError();
            lockedOnThisThread = false;
        }

        static if (!is(typeof(func(args)) == void))
            return res;
    }


    /**
     * Returns a bit field representing all block attributes set for the memory
     * referenced by p.
     *
     * Params:
     *  p = A pointer to the base of a valid memory block or to null.
     *
     * Returns:
     *  A bit field containing any bits set for the memory block referenced by
     *  p or zero on error.
     */
    uint getAttr(void* p) nothrow
    {
        if (!p)
        {
            return 0;
        }

        static uint go(Gcx* gcx, void* p) nothrow
        {
            Pool* pool = gcx.findPool(p);
            uint  oldb = 0;

            if (pool)
            {
                p = sentinel_sub(p);
                if (p != pool.findBase(p))
                    return 0;
                auto biti = cast(size_t)(p - pool.baseAddr) >> pool.shiftBy;

                oldb = pool.getBits(biti);
            }
            return oldb;
        }

        return runLocked!(go, otherTime, numOthers)(gcx, p);
    }

    /**
     * Sets the specified bits for the memory references by p.
     *
     * If p was not allocated by the GC, points inside a block, or is null, no
     * action will be taken.
     *
     * Params:
     *  p = A pointer to the base of a valid memory block or to null.
     *  mask = A bit field containing any bits to set for this memory block.
     *
     * Returns:
     *  The result of a call to getAttr after the specified bits have been
     *  set.
     */
    uint setAttr(void* p, uint mask) nothrow
    {
        if (!p)
        {
            return 0;
        }

        static uint go(Gcx* gcx, void* p, uint mask) nothrow
        {
            Pool* pool = gcx.findPool(p);
            uint  oldb = 0;

            if (pool)
            {
                p = sentinel_sub(p);
                if (p != pool.findBase(p))
                    return 0;
                const biti = cast(size_t)(p - pool.baseAddr) >> pool.shiftBy;

                oldb = pool.getBits(biti);
                pool.setBits(biti, mask);
            }
            return oldb;
        }

        return runLocked!(go, otherTime, numOthers)(gcx, p, mask);
    }


    /**
     * Clears the specified bits for the memory references by p.
     *
     * If p was not allocated by the GC, points inside a block, or is null, no
     * action will be taken.
     *
     * Params:
     *  p = A pointer to the base of a valid memory block or to null.
     *  mask = A bit field containing any bits to clear for this memory block.
     *
     * Returns:
     *  The result of a call to getAttr after the specified bits have been
     *  cleared
     */
    uint clrAttr(void* p, uint mask) nothrow
    {
        if (!p)
        {
            return 0;
        }

        static uint go(Gcx* gcx, void* p, uint mask) nothrow
        {
            Pool* pool = gcx.findPool(p);
            uint  oldb = 0;

            if (pool)
            {
                p = sentinel_sub(p);
                if (p != pool.findBase(p))
                    return 0;
                const biti = cast(size_t)(p - pool.baseAddr) >> pool.shiftBy;

                oldb = pool.getBits(biti);
                pool.clrBits(biti, mask);
            }
            return oldb;
        }

        return runLocked!(go, otherTime, numOthers)(gcx, p, mask);
    }

    /**
     * Requests an aligned block of managed memory from the garbage collector.
     *
     * Params:
     *  size = The desired allocation size in bytes.
     *  bits = A bitmask of the attributes to set on this block.
     *  ti = TypeInfo to describe the memory.
     *
     * Returns:
     *  A reference to the allocated memory or null if no memory was requested.
     *
     * Throws:
     *  OutOfMemoryError on allocation failure
     */
    void *malloc(size_t size, uint bits = 0, const TypeInfo ti = null) nothrow
    {
        if (!size)
        {
            return null;
        }

        size_t localAllocSize = void;

        auto p = runLocked!(mallocNoSync, mallocTime, numMallocs)(size, bits, localAllocSize, ti);

        invalidate(p[0 .. localAllocSize], 0xF0, true);

        if (!(bits & BlkAttr.NO_SCAN))
        {
            memset(p + size, 0, localAllocSize - size);
        }

        return p;
    }


    //
    // Implementation for malloc and calloc.
    //
    private void *mallocNoSync(size_t size, uint bits, ref size_t alloc_size, const TypeInfo ti = null) nothrow
    {
        assert(size != 0);

        debug(PRINTF)
            printf("GC::malloc(gcx = %p, size = %zd bits = %x, ti = %s)\n", gcx, size, bits, debugTypeName(ti).ptr);

        assert(gcx);
        //debug(PRINTF) printf("gcx.self = %x, pthread_self() = %x\n", gcx.self, pthread_self());

        auto p = gcx.alloc(size + SENTINEL_EXTRA, alloc_size, bits, ti);
        if (!p)
            onOutOfMemoryError();

        debug (SENTINEL)
        {
            p = sentinel_add(p);
            sentinel_init(p, size);
            alloc_size = size;
        }
        gcx.leakDetector.log_malloc(p, size);
        bytesAllocated += alloc_size;

        debug(PRINTF) printf("  => p = %p\n", p);
        return p;
    }

    BlkInfo qalloc( size_t size, uint bits, const scope TypeInfo ti) nothrow
    {

        if (!size)
        {
            return BlkInfo.init;
        }

        BlkInfo retval;

        retval.base = runLocked!(mallocNoSync, mallocTime, numMallocs)(size, bits, retval.size, ti);

        if (!(bits & BlkAttr.NO_SCAN))
        {
            memset(retval.base + size, 0, retval.size - size);
        }

        retval.attr = bits;
        return retval;
    }


    /**
     * Requests an aligned block of managed memory from the garbage collector,
     * which is initialized with all bits set to zero.
     *
     * Params:
     *  size = The desired allocation size in bytes.
     *  bits = A bitmask of the attributes to set on this block.
     *  ti = TypeInfo to describe the memory.
     *
     * Returns:
     *  A reference to the allocated memory or null if no memory was requested.
     *
     * Throws:
     *  OutOfMemoryError on allocation failure.
     */
    void *calloc(size_t size, uint bits = 0, const TypeInfo ti = null) nothrow
    {
        if (!size)
        {
            return null;
        }

        size_t localAllocSize = void;

        auto p = runLocked!(mallocNoSync, mallocTime, numMallocs)(size, bits, localAllocSize, ti);

        debug (VALGRIND) makeMemUndefined(p[0..size]);
        invalidate((p + size)[0 .. localAllocSize - size], 0xF0, true);

        memset(p, 0, size);
        if (!(bits & BlkAttr.NO_SCAN))
        {
            memset(p + size, 0, localAllocSize - size);
        }

        return p;
    }

    /**
     * Request that the GC reallocate a block of memory, attempting to adjust
     * the size in place if possible. If size is 0, the memory will be freed.
     *
     * If p was not allocated by the GC, points inside a block, or is null, no
     * action will be taken.
     *
     * Params:
     *  p = A pointer to the root of a valid memory block or to null.
     *  size = The desired allocation size in bytes.
     *  bits = A bitmask of the attributes to set on this block.
     *  ti = TypeInfo to describe the memory.
     *
     * Returns:
     *  A reference to the allocated memory on success or null if size is
     *  zero.
     *
     * Throws:
     *  OutOfMemoryError on allocation failure.
     */
    void *realloc(void *p, size_t size, uint bits = 0, const TypeInfo ti = null) nothrow
    {
        size_t localAllocSize = void;
        auto oldp = p;

        p = runLocked!(reallocNoSync, mallocTime, numMallocs)(p, size, bits, localAllocSize, ti);

        if (p && !(bits & BlkAttr.NO_SCAN))
        {
            memset(p + size, 0, localAllocSize - size);
        }

        return p;
    }


    //
    // The implementation of realloc.
    //
    // bits will be set to the resulting bits of the new block
    //
    private void *reallocNoSync(void *p, size_t size, ref uint bits, ref size_t alloc_size, const TypeInfo ti = null) nothrow
    {
        if (!size)
        {
            if (p)
                freeNoSync(p);
            alloc_size = 0;
            return null;
        }
        if (!p)
            return mallocNoSync(size, bits, alloc_size, ti);

        debug(PRINTF) printf("GC::realloc(p = %p, size = %llu)\n", p, cast(ulong)size);

        Pool *pool = gcx.findPool(p);
        if (!pool)
            return null;

        size_t psize;
        size_t biti;

        debug(SENTINEL)
        {
            void* q = p;
            p = sentinel_sub(p);
            bool alwaysMalloc = true;
        }
        else
        {
            alias q = p;
            enum alwaysMalloc = false;
        }

        void* doMalloc()
        {
            if (!bits)
                bits = pool.getBits(biti);

            void* p2 = mallocNoSync(size, bits, alloc_size, ti);
            debug (SENTINEL)
                psize = sentinel_size(q, psize);
            if (psize < size)
                size = psize;
            //debug(PRINTF) printf("\tcopying %d bytes\n",size);
            memcpy(p2, q, size);
            freeNoSync(q);
            return p2;
        }

        if (pool.isLargeObject)
        {
            auto lpool = cast(LargeObjectPool*) pool;
            auto psz = lpool.getPages(p);     // get allocated size
            if (psz == 0)
                return null;      // interior pointer
            psize = psz * PAGESIZE;

            alias pagenum = biti; // happens to be the same, but rename for clarity
            pagenum = lpool.pagenumOf(p);

            if (size <= PAGESIZE / 2 || alwaysMalloc)
                return doMalloc(); // switching from large object pool to small object pool

            auto newsz = lpool.numPages(size);
            if (newsz == psz)
            {
                // nothing to do
            }
            else if (newsz < psz)
            {
                // Shrink in place
                invalidate((p + size)[0 .. psize - size], 0xF2, false);
                lpool.freePages(pagenum + newsz, psz - newsz);
                lpool.mergeFreePageOffsets!(false, true)(pagenum + newsz, psz - newsz);
                lpool.bPageOffsets[pagenum] = cast(uint) newsz;
            }
            else if (pagenum + newsz <= pool.npages)
            {
                // Attempt to expand in place (TODO: merge with extend)
                if (lpool.pagetable[pagenum + psz] != Bins.B_FREE)
                    return doMalloc();

                auto newPages = newsz - psz;
                auto freesz = lpool.bPageOffsets[pagenum + psz];
                if (freesz < newPages)
                    return doMalloc(); // free range too small

                invalidate((p + psize)[0 .. size - psize], 0xF0, true);
                debug (PRINTF) printFreeInfo(pool);
                memset(&lpool.pagetable[pagenum + psz], Bins.B_PAGEPLUS, newPages);
                lpool.bPageOffsets[pagenum] = cast(uint) newsz;
                for (auto offset = psz; offset < newsz; offset++)
                    lpool.bPageOffsets[pagenum + offset] = cast(uint) offset;
                if (freesz > newPages)
                    lpool.setFreePageOffsets(pagenum + newsz, freesz - newPages);
                gcx.usedLargePages += newPages;
                lpool.freepages -= newPages;
                debug (PRINTF) printFreeInfo(pool);
            }
            else
                return doMalloc(); // does not fit into current pool

            alloc_size = newsz * PAGESIZE;
        }
        else
        {
            psize = (cast(SmallObjectPool*) pool).getSize(p);   // get allocated bin size
            if (psize == 0)
                return null;    // interior pointer
            biti = cast(size_t)(p - pool.baseAddr) >> Pool.ShiftBy.Small;
            if (pool.freebits.test (biti))
                return null;

            // allocate if new size is bigger or less than half
            if (psize < size || psize > size * 2 || alwaysMalloc)
                return doMalloc();

            alloc_size = psize;
            if (isPrecise)
                pool.setPointerBitmapSmall(p, size, psize, bits, ti);
        }

        if (bits)
        {
            pool.clrBits(biti, ~BlkAttr.NONE);
            pool.setBits(biti, bits);
        }
        return p;
    }


    size_t extend(void* p, size_t minsize, size_t maxsize, const TypeInfo ti) nothrow
    {
        return runLocked!(extendNoSync, extendTime, numExtends)(p, minsize, maxsize, ti);
    }


    //
    // Implementation of extend.
    //
    private size_t extendNoSync(void* p, size_t minsize, size_t maxsize, const TypeInfo ti = null) nothrow
    in
    {
        assert(minsize <= maxsize);
    }
    do
    {
        debug(PRINTF) printf("GC::extend(p = %p, minsize = %zu, maxsize = %zu)\n", p, minsize, maxsize);
        debug (SENTINEL)
        {
            return 0;
        }
        else
        {
            auto pool = gcx.findPool(p);
            if (!pool || !pool.isLargeObject)
                return 0;

            auto lpool = cast(LargeObjectPool*) pool;
            const pagenum = lpool.pagenumOf(p);
            if (lpool.pagetable[pagenum] != Bins.B_PAGE)
                return 0;

            uint psz = lpool.bPageOffsets[pagenum];
            assert(psz > 0);

            const minsz = lpool.numPages(minsize);
            const maxsz = lpool.numPages(maxsize);

            if (pagenum + psz >= lpool.npages)
                return 0;
            if (lpool.pagetable[pagenum + psz] != Bins.B_FREE)
                return 0;

            const freesz = lpool.bPageOffsets[pagenum + psz];
            if (freesz < minsz)
                return 0;
            const sz = freesz > maxsz ? maxsz : freesz;
            invalidate((pool.baseAddr + (pagenum + psz) * PAGESIZE)[0 .. sz * PAGESIZE], 0xF0, true);
            memset(lpool.pagetable + pagenum + psz, Bins.B_PAGEPLUS, sz);
            lpool.bPageOffsets[pagenum] = cast(uint) (psz + sz);
            for (auto offset = psz; offset < psz + sz; offset++)
                lpool.bPageOffsets[pagenum + offset] = cast(uint) offset;
            if (freesz > sz)
                lpool.setFreePageOffsets(pagenum + psz + sz, freesz - sz);
            lpool.freepages -= sz;
            gcx.usedLargePages += sz;
            return (psz + sz) * PAGESIZE;
        }
    }


    /**
     * Requests that at least size bytes of memory be obtained from the operating
     * system and marked as free.
     *
     * Params:
     *  size = The desired size in bytes.
     *
     * Returns:
     *  The actual number of bytes reserved or zero on error.
     */
    size_t reserve(size_t size) nothrow
    {
        if (!size)
        {
            return 0;
        }

        return runLocked!(reserveNoSync, otherTime, numOthers)(size);
    }


    //
    // Implementation of reserve
    //
    private size_t reserveNoSync(size_t size) nothrow
    {
        assert(size != 0);
        assert(gcx);

        return gcx.reserve(size);
    }


    /**
     * Deallocates the memory referenced by p.
     *
     * If p was not allocated by the GC, points inside a block, is null, or
     * if free is called from a finalizer, no action will be taken.
     *
     * Params:
     *  p = A pointer to the root of a valid memory block or to null.
     */
    void free(void *p) nothrow
    {
        if (!p || _inFinalizer)
        {
            return;
        }

        return runLocked!(freeNoSync, freeTime, numFrees)(p);
    }


    //
    // Implementation of free.
    //
    private void freeNoSync(void *p) nothrow @nogc
    {
        debug(PRINTF) printf("Freeing %#zx\n", cast(size_t) p);
        assert (p);

        Pool*  pool;
        size_t pagenum;
        Bins   bin;
        size_t biti;

        // Find which page it is in
        pool = gcx.findPool(p);
        if (!pool)                              // if not one of ours
            return;                             // ignore

        pagenum = pool.pagenumOf(p);

        debug(PRINTF) printf("pool base = %p, PAGENUM = %zd of %zd, bin = %d\n", pool.baseAddr, pagenum, pool.npages, pool.pagetable[pagenum]);
        debug(PRINTF) if (pool.isLargeObject) printf("Block size = %d\n", pool.bPageOffsets[pagenum]);

        bin = pool.pagetable[pagenum];

        // Verify that the pointer is at the beginning of a block,
        //  no action should be taken if p is an interior pointer
        if (bin > Bins.B_PAGE) // B_PAGEPLUS or B_FREE
            return;
        size_t off = (sentinel_sub(p) - pool.baseAddr);
        size_t base = baseOffset(off, bin);
        if (off != base)
            return;

        sentinel_Invariant(p);
        auto q = p;
        p = sentinel_sub(p);
        size_t ssize;

        if (pool.isLargeObject)              // if large alloc
        {
            biti = cast(size_t)(p - pool.baseAddr) >> pool.ShiftBy.Large;
            assert(bin == Bins.B_PAGE);
            auto lpool = cast(LargeObjectPool*) pool;

            // Free pages
            size_t npages = lpool.bPageOffsets[pagenum];
            auto size = npages * PAGESIZE;
            ssize = sentinel_size(q, size);
            invalidate(p[0 .. size], 0xF2, false);
            lpool.freePages(pagenum, npages);
            lpool.mergeFreePageOffsets!(true, true)(pagenum, npages);
        }
        else
        {
            biti = cast(size_t)(p - pool.baseAddr) >> pool.ShiftBy.Small;
            if (pool.freebits.test (biti))
                return;
            // Add to free list
            List *list = cast(List*)p;

            auto size = binsize[bin];
            ssize = sentinel_size(q, size);
            invalidate(p[0 .. size], 0xF2, false);

            // in case the page hasn't been recovered yet, don't add the object to the free list
            if (!gcx.recoverPool[bin] || pool.binPageChain[pagenum] == Pool.PageRecovered)
            {
                undefinedWrite(list.next, gcx.bucket[bin]);
                undefinedWrite(list.pool, pool);
                gcx.bucket[bin] = list;
            }
            pool.freebits.set(biti);
        }
        pool.clrBits(biti, ~BlkAttr.NONE);

        gcx.leakDetector.log_free(sentinel_add(p), ssize);
    }


    /**
     * Determine the base address of the block containing p.  If p is not a gc
     * allocated pointer, return null.
     *
     * Params:
     *  p = A pointer to the root or the interior of a valid memory block or to
     *      null.
     *
     * Returns:
     *  The base address of the memory block referenced by p or null on error.
     */
    void* addrOf(void *p) nothrow
    {
        if (!p)
        {
            return null;
        }

        return runLocked!(addrOfNoSync, otherTime, numOthers)(p);
    }


    //
    // Implementation of addrOf.
    //
    void* addrOfNoSync(void *p) nothrow @nogc
    {
        if (!p)
        {
            return null;
        }

        auto q = gcx.findBase(p);
        if (q)
            q = sentinel_add(q);
        return q;
    }


    /**
     * Determine the allocated size of pointer p.  If p is an interior pointer
     * or not a gc allocated pointer, return 0.
     *
     * Params:
     *  p = A pointer to the root of a valid memory block or to null.
     *
     * Returns:
     *  The size in bytes of the memory block referenced by p or zero on error.
     */
    size_t sizeOf(void *p) nothrow
    {
        if (!p)
        {
            return 0;
        }

        return runLocked!(sizeOfNoSync, otherTime, numOthers)(p);
    }


    //
    // Implementation of sizeOf.
    //
    private size_t sizeOfNoSync(void *p) nothrow @nogc
    {
        assert (p);

        debug (SENTINEL)
        {
            p = sentinel_sub(p);
            size_t size = gcx.findSize(p);
            return size ? size - SENTINEL_EXTRA : 0;
        }
        else
        {
            size_t size = gcx.findSize(p);
            return size;
        }
    }


    /**
     * Determine the base address of the block containing p.  If p is not a gc
     * allocated pointer, return null.
     *
     * Params:
     *  p = A pointer to the root or the interior of a valid memory block or to
     *      null.
     *
     * Returns:
     *  Information regarding the memory block referenced by p or BlkInfo.init
     *  on error.
     */
    BlkInfo query(void *p) nothrow
    {
        if (!p)
        {
            BlkInfo i;
            return  i;
        }

        return runLocked!(queryNoSync, otherTime, numOthers)(p);
    }

    //
    // Implementation of query
    //
    BlkInfo queryNoSync(void *p) nothrow
    {
        assert(p);

        BlkInfo info = gcx.getInfo(p);
        debug(SENTINEL)
        {
            if (info.base)
            {
                info.base = sentinel_add(info.base);
                info.size = *sentinel_psize(info.base);
            }
        }
        return info;
    }


    /**
     * Performs certain checks on a pointer. These checks will cause asserts to
     * fail unless the following conditions are met:
     *  1) The poiinter belongs to this memory pool.
     *  2) The pointer points to the start of an allocated piece of memory.
     *  3) The pointer is not on a free list.
     *
     * Params:
     *  p = The pointer to be checked.
     */
    void check(void *p) nothrow
    {
        if (!p)
        {
            return;
        }

        return runLocked!(checkNoSync, otherTime, numOthers)(p);
    }


    //
    // Implementation of check
    //
    private void checkNoSync(void *p) nothrow
    {
        assert(p);

        sentinel_Invariant(p);
        debug (PTRCHECK)
        {
            Pool*  pool;
            size_t pagenum;
            Bins   bin;

            p = sentinel_sub(p);
            pool = gcx.findPool(p);
            assert(pool);
            pagenum = pool.pagenumOf(p);
            bin = pool.pagetable[pagenum];
            assert(bin <= Bins.B_PAGE);
            assert(p == cast(void*)baseOffset(cast(size_t)p, bin));

            debug (PTRCHECK2)
            {
                if (bin < Bins.B_PAGE)
                {
                    // Check that p is not on a free list
                    List *list;

                    for (list = gcx.bucket[bin]; list; list = list.next)
                    {
                        assert(cast(void*)list != p);
                    }
                }
            }
        }
    }


    /**
     * Add p to list of roots. If p is null, no operation is performed.
     *
     * Params:
     *  p = A pointer into a GC-managed memory block or null.
     */
    void addRoot(void *p) nothrow @nogc
    {
        if (!p)
        {
            return;
        }

        gcx.addRoot(p);
    }


    /**
     * Remove p from list of roots. If p is null or is not a value
     * previously passed to addRoot() then no operation is performed.
     *
     * Params:
     *  p = A pointer into a GC-managed memory block or null.
     */
    void removeRoot(void *p) nothrow @nogc
    {
        if (!p)
        {
            return;
        }

        gcx.removeRoot(p);
    }

    /**
     * Returns an iterator allowing roots to be traversed via a foreach loop.
     */
    @property RootIterator rootIter() @nogc
    {
        return &gcx.rootsApply;
    }


    /**
     * Add range to scan for roots. If p is null or sz is 0, no operation is performed.
     *
     * Params:
     *  p  = A pointer to a valid memory address or to null.
     *  sz = The size in bytes of the block to add.
     *  ti = TypeInfo to describe the memory.
     */
    void addRange(void *p, size_t sz, const TypeInfo ti = null) nothrow @nogc
    {
        if (!p || !sz)
        {
            return;
        }

        gcx.addRange(p, p + sz, ti);
    }


    /**
     * Remove range from list of ranges. If p is null or does not represent
     * a value previously passed to addRange() then no operation is
     * performed.
     *
     * Params:
     *  p  = A pointer to a valid memory address or to null.
     */
    void removeRange(void *p) nothrow @nogc
    {
        if (!p)
        {
            return;
        }

        gcx.removeRange(p);
    }


    /**
     * Returns an iterator allowing ranges to be traversed via a foreach loop.
     */
    @property RangeIterator rangeIter() @nogc
    {
        return &gcx.rangesApply;
    }


    /**
     * Run all finalizers in the code segment.
     *
     * Params:
     *  segment = address range of a code segment
     */
    void runFinalizers(const scope void[] segment) nothrow
    {
        static void go(Gcx* gcx, const scope void[] segment) nothrow
        {
            gcx.runFinalizers(segment);
        }
        return runLocked!(go, otherTime, numOthers)(gcx, segment);
    }


    bool inFinalizer() nothrow @nogc
    {
        return _inFinalizer;
    }


    void collect() nothrow
    {
        fullCollect();
    }


    /**
     * Begins a full collection, scanning all stack segments for roots.
     *
     * Returns:
     *  The number of pages freed.
     */
    size_t fullCollect() nothrow
    {
        debug(PRINTF) printf("GC.fullCollect()\n");

        // Since a finalizer could launch a new thread, we always need to lock
        // when collecting.
        static size_t go(Gcx* gcx) nothrow
        {
            return gcx.fullcollect(false, true); // standard stop the world
        }
        immutable result = runLocked!go(gcx);

        version (none)
        {
            GCStats stats;

            getStats(stats);
            debug(PRINTF) printf("heapSize = %zx, freeSize = %zx\n",
                stats.heapSize, stats.freeSize);
        }

        gcx.leakDetector.log_collect();
        return result;
    }


    /**
     * Minimize free space usage.
     */
    void minimize() nothrow
    {
        static void go(Gcx* gcx) nothrow
        {
            gcx.minimize();
        }
        runLocked!(go, otherTime, numOthers)(gcx);
    }


    core.memory.GC.Stats stats() @safe nothrow @nogc
    {
        typeof(return) ret;

        runLocked!(getStatsNoSync, otherTime, numOthers)(ret);

        return ret;
    }


    core.memory.GC.ProfileStats profileStats() nothrow @trusted
    {
        typeof(return) ret;

        ret.numCollections = numCollections;
        ret.totalCollectionTime = prepTime + markTime + sweepTime;
        ret.totalPauseTime = pauseTime;
        ret.maxCollectionTime = maxCollectionTime;
        ret.maxPauseTime = maxPauseTime;

        return ret;
    }


    ulong allocatedInCurrentThread() nothrow
    {
        return bytesAllocated;
    }


    //
    // Implementation of getStats
    //
    private void getStatsNoSync(out core.memory.GC.Stats stats) @trusted nothrow @nogc
    {
        // This function is trusted for two reasons: `pool.pagetable` is a pointer,
        // which is being sliced in the below foreach, and so is `binPageChain`,
        // also sliced a bit later in this function.
        // However, both usages are safe as long as the assumption that `npools`
        // defines the limit for `pagetable`'s length holds true (see allocation).
        // The slicing happens at __LINE__ + 4 and __LINE__ + 24.
        // `@trusted` delegates are not used to prevent any performance issue.
        foreach (pool; gcx.pooltable[])
        {
            foreach (bin; pool.pagetable[0 .. pool.npages])
            {
                if (bin == Bins.B_FREE)
                    stats.freeSize += PAGESIZE;
                else
                    stats.usedSize += PAGESIZE;
            }
        }

        size_t freeListSize;
        foreach (n; 0 .. Bins.B_PAGE)
        {
            immutable sz = binsize[n];
            for (List *list = gcx.bucket[n]; list; list = list.next)
                freeListSize += sz;

            foreach (pool; gcx.pooltable[])
            {
                if (pool.isLargeObject)
                    continue;
                for (uint pn = pool.recoverPageFirst[n]; pn < pool.npages; pn = pool.binPageChain[pn])
                {
                    const bitbase = pn * PAGESIZE / 16;
                    const top = PAGESIZE - sz + 1; // ensure <size> bytes available even if unaligned
                    for (size_t u = 0; u < top; u += sz)
                        if (pool.freebits.test(bitbase + u / 16))
                            freeListSize += sz;
                }
            }
        }

        stats.usedSize -= freeListSize;
        stats.freeSize += freeListSize;
        stats.allocatedInCurrentThread = bytesAllocated;
    }

    // ARRAY FUNCTIONS
    void[] getArrayUsed(void *ptr, bool atomic = false) nothrow
    {
        import core.internal.gc.blockmeta;
        import core.internal.gc.blkcache;
        import core.internal.array.utils;

        // lookup the block info, using the cache if possible.
        auto bic = atomic ? null : __getBlkInfo(ptr);
        auto info = bic ? *bic : query(ptr);

        if (!(info.attr & BlkAttr.APPENDABLE))
            // not appendable
            return null;

        assert(info.base); // sanity check.
        if (!bic && !atomic)
            // cache the lookup for next time
            __insertBlkInfoCache(info, null);

        auto usedSize = atomic ? __arrayAllocLengthAtomic(info) : __arrayAllocLength(info);
        return __arrayStart(info)[0 .. usedSize];
    }

    /* NOTE about @trusted in these functions:
     * These functions do a lot of pointer manipulation, and has writeable
     * access to BlkInfo which is used to interface with other parts of the GC,
     * including the block metadata and block cache. Marking these functions as
     * @safe would mean that any modification of BlkInfo fields should be
     * considered @safe, which is not the case. For example, it would be
     * perfectly legal to change the BlkInfo size to some huge number, and then
     * store it in the block cache to blow up later. The utility functions
     * count on the BlkInfo representing the correct information inside the GC.
     *
     * In order to mark these @safe, we would need a BlkInfo that has
     * restrictive access (i.e. @system only) to the information inside the
     * BlkInfo. Until then any use of these structures needs to be @trusted,
     * and therefore the entire functions are @trusted. The API is still @safe
     * because the information is stored and looked up by the GC, not the
     * caller.
     */
    bool expandArrayUsed(void[] slice, size_t newUsed, bool atomic = false) nothrow @trusted
    {
        import core.internal.gc.blockmeta;
        import core.internal.gc.blkcache;
        import core.internal.array.utils;

        if (newUsed < slice.length)
            // cannot "expand" by shrinking.
            return false;

        // lookup the block info, using the cache if possible
        auto bic = atomic ? null : __getBlkInfo(slice.ptr);
        auto info = bic ? *bic : query(slice.ptr);

        if (!(info.attr & BlkAttr.APPENDABLE))
            // not appendable
            return false;

        assert(info.base); // sanity check.

        immutable offset = slice.ptr - __arrayStart(info);
        newUsed += offset;
        auto existingUsed = slice.length + offset;

        size_t typeInfoSize = (info.attr & BlkAttr.STRUCTFINAL) ? size_t.sizeof : 0;
        if (__setArrayAllocLengthImpl(info, offset + newUsed, atomic, existingUsed, typeInfoSize))
        {
            // could expand without extending
            if (!bic && !atomic)
                // cache the lookup for next time
                __insertBlkInfoCache(info, null);
            return true;
        }

        // if we got here, just setting the used size did not work.
        if (info.size < PAGESIZE)
            // nothing else we can do
            return false;

        // try extending the block into subsequent pages.
        immutable requiredExtension = newUsed - info.size - LARGEPAD;
        auto extendedSize = extend(info.base, requiredExtension, requiredExtension, null);
        if (extendedSize == 0)
            // could not extend, can't satisfy the request
            return false;

        info.size = extendedSize;
        if (bic)
            *bic = info;
        else if (!atomic)
            __insertBlkInfoCache(info, null);

        // this should always work.
        return __setArrayAllocLengthImpl(info, newUsed, atomic, existingUsed, typeInfoSize);
    }

    bool shrinkArrayUsed(void[] slice, size_t existingUsed, bool atomic = false) nothrow
    {
        import core.internal.gc.blockmeta;
        import core.internal.gc.blkcache;
        import core.internal.array.utils;

        if (existingUsed < slice.length)
            // cannot "shrink" by growing.
            return false;

        // lookup the block info, using the cache if possible.
        auto bic = atomic ? null : __getBlkInfo(slice.ptr);
        auto info = bic ? *bic : query(slice.ptr);

        if (!(info.attr & BlkAttr.APPENDABLE))
            // not appendable
            return false;

        assert(info.base); // sanity check

        immutable offset = slice.ptr - __arrayStart(info);
        existingUsed += offset;
        auto newUsed = slice.length + offset;

        size_t typeInfoSize = (info.attr & BlkAttr.STRUCTFINAL) ? size_t.sizeof : 0;

        if (__setArrayAllocLengthImpl(info, newUsed, atomic, existingUsed, typeInfoSize))
        {
            if (!bic && !atomic)
                __insertBlkInfoCache(info, null);
            return true;
        }

        return false;
    }

    size_t reserveArrayCapacity(void[] slice, size_t request, bool atomic = false) nothrow @trusted
    {
        import core.internal.gc.blockmeta;
        import core.internal.gc.blkcache;
        import core.internal.array.utils;

        // lookup the block info, using the cache if possible.
        auto bic = atomic ? null : __getBlkInfo(slice.ptr);
        auto info = bic ? *bic : query(slice.ptr);

        if (!(info.attr & BlkAttr.APPENDABLE))
            // not appendable
            return 0;

        assert(info.base); // sanity check

        immutable offset = slice.ptr - __arrayStart(info);
        request += offset;
        auto existingUsed = slice.length + offset;

        // make sure this slice ends at the used space
        auto blockUsed = atomic ? __arrayAllocLengthAtomic(info) : __arrayAllocLength(info);
        if (existingUsed != blockUsed)
            // not an expandable slice.
            return 0;

        // see if the capacity can contain the existing data
        auto existingCapacity = __arrayAllocCapacity(info);
        if (existingCapacity < request)
        {
            if (info.size < PAGESIZE)
                // no possibility to extend
                return 0;

            immutable requiredExtension = request - existingCapacity;
            auto extendedSize = extend(info.base, requiredExtension, requiredExtension, null);
            if (extendedSize == 0)
                // could not extend, can't satisfy the request
                return 0;

            info.size = extendedSize;

            // update the block info cache if it was used
            if (bic)
                *bic = info;
            else if (!atomic)
                __insertBlkInfoCache(info, null);

            existingCapacity = __arrayAllocCapacity(info);
        }

        return existingCapacity - offset;
    }
}


/* ============================ Gcx =============================== */

enum
{   PAGESIZE =    4096,
}


enum Bins : ubyte
{
    B_16,
    B_32,
    B_48,
    B_64,
    B_96,
    B_128,
    B_176,
    B_256,
    B_368,
    B_512,
    B_816,
    B_1024,
    B_1360,
    B_2048,
    B_NUMSMALL,

    B_PAGE = B_NUMSMALL,// start of large alloc
    B_PAGEPLUS,         // continuation of large alloc
    B_FREE,             // free page
    B_MAX,
}

struct List
{
    List *next;
    Pool *pool;
}

// non power of two sizes optimized for small remainder within page (<= 64 bytes)
immutable short[Bins.B_NUMSMALL + 1] binsize = [ 16, 32, 48, 64, 96, 128, 176, 256, 368, 512, 816, 1024, 1360, 2048, 4096 ];
immutable short[PAGESIZE / 16][Bins.B_NUMSMALL + 1] binbase = calcBinBase();

short[PAGESIZE / 16][Bins.B_NUMSMALL + 1] calcBinBase()
{
    short[PAGESIZE / 16][Bins.B_NUMSMALL + 1] bin;

    foreach (i, size; binsize)
    {
        short end = cast(short) ((PAGESIZE / size) * size);
        short bsz = size / 16;
        foreach (off; 0..PAGESIZE/16)
        {
            // add the remainder to the last bin, so no check during scanning
            //  is needed if a false pointer targets that area
            const base = (off - off % bsz) * 16;
            bin[i][off] = cast(short)(base < end ? base : end - size);
        }
    }
    return bin;
}

size_t baseOffset(size_t offset, Bins bin) @nogc nothrow
{
    assert(bin <= Bins.B_PAGE);
    return (offset & ~(PAGESIZE - 1)) + binbase[bin][(offset & (PAGESIZE - 1)) >> 4];
}

alias PageBits = GCBits.wordtype[PAGESIZE / 16 / GCBits.BITS_PER_WORD];
static assert(PAGESIZE % (GCBits.BITS_PER_WORD * 16) == 0);

// bitmask with bits set at base offsets of objects
immutable PageBits[Bins.B_NUMSMALL] baseOffsetBits = (){
    PageBits[Bins.B_NUMSMALL] bits;
    foreach (bin; 0 .. Bins.B_NUMSMALL)
    {
        size_t size = binsize[bin];
        const top = PAGESIZE - size + 1; // ensure <size> bytes available even if unaligned
        for (size_t u = 0; u < top; u += size)
        {
            size_t biti = u / 16;
            size_t off = biti / GCBits.BITS_PER_WORD;
            size_t mod = biti % GCBits.BITS_PER_WORD;
            bits[bin][off] |= GCBits.BITS_1 << mod;
        }
    }
    return bits;
}();

private void set(ref PageBits bits, size_t i) @nogc pure nothrow
{
    assert(i < PageBits.sizeof * 8);
    bts(bits.ptr, i);
}

/* ============================ Gcx =============================== */

struct Gcx
{
    auto rootsLock = shared(AlignedSpinLock)(SpinLock.Contention.brief);
    auto rangesLock = shared(AlignedSpinLock)(SpinLock.Contention.brief);
    Treap!Root roots;
    Treap!Range ranges;
    bool minimizeAfterNextCollection = false;
    version (COLLECT_FORK)
    {
        private pid_t markProcPid = 0;
        bool shouldFork;
    }

    debug(INVARIANT) bool initialized;
    debug(INVARIANT) bool inCollection;
    uint disabled; // turn off collections if >0

    PoolTable!Pool pooltable;

    List*[Bins.B_NUMSMALL] bucket; // free list for each small size

    // run a collection when reaching those thresholds (number of used pages)
    float smallCollectThreshold = 0.0f, largeCollectThreshold = 0.0f;
    uint usedSmallPages, usedLargePages;
    // total number of mapped pages
    uint mappedPages;

    debug (LOGGING)
        LeakDetector leakDetector;
    else
        alias leakDetector = LeakDetector;

    SmallObjectPool*[Bins.B_NUMSMALL] recoverPool;
    version (Posix) __gshared Gcx* instance;

    void initialize()
    {
        (cast(byte*)&this)[0 .. Gcx.sizeof] = 0;
        leakDetector.initialize(&this);
        roots.initialize(0x243F6A8885A308D3UL);
        ranges.initialize(0x13198A2E03707344UL);
        smallCollectThreshold = largeCollectThreshold = 0.0f;
        usedSmallPages = usedLargePages = 0;
        mappedPages = 0;
        //printf("gcx = %p, self = %x\n", &this, self);
        version (Posix)
        {
            import core.sys.posix.pthread : pthread_atfork;
            instance = &this;
            __gshared atforkHandlersInstalled = false;
            if (!atforkHandlersInstalled)
            {
                pthread_atfork(
                    &_d_gcx_atfork_prepare,
                    &_d_gcx_atfork_parent,
                    &_d_gcx_atfork_child);
                atforkHandlersInstalled = true;
            }
        }
        debug(INVARIANT) initialized = true;
        version (COLLECT_FORK)
            shouldFork = config.fork;

    }

    void Dtor()
    {
        if (config.profile)
        {
            printf("\tNumber of collections:  %llu\n", cast(ulong)numCollections);
            printf("\tTotal GC prep time:  %lld milliseconds\n",
                   prepTime.total!("msecs"));
            printf("\tTotal mark time:  %lld milliseconds\n",
                   markTime.total!("msecs"));
            printf("\tTotal sweep time:  %lld milliseconds\n",
                   sweepTime.total!("msecs"));
            long maxPause = maxPauseTime.total!("msecs");
            printf("\tMax Pause Time:  %lld milliseconds\n", maxPause);
            long gcTime = (sweepTime + markTime + prepTime).total!("msecs");
            printf("\tGrand total GC time:  %lld milliseconds\n", gcTime);
            long pauseTime = (markTime + prepTime).total!("msecs");

            char[30] apitxt = void;
            apitxt[0] = 0;
            debug(PROFILE_API) if (config.profile > 1)
            {
                static Duration toDuration(long dur)
                {
                    return MonoTime(dur) - MonoTime(0);
                }

                printf("\n");
                printf("\tmalloc:  %llu calls, %lld ms\n", cast(ulong)numMallocs, toDuration(mallocTime).total!"msecs");
                printf("\trealloc: %llu calls, %lld ms\n", cast(ulong)numReallocs, toDuration(reallocTime).total!"msecs");
                printf("\tfree:    %llu calls, %lld ms\n", cast(ulong)numFrees, toDuration(freeTime).total!"msecs");
                printf("\textend:  %llu calls, %lld ms\n", cast(ulong)numExtends, toDuration(extendTime).total!"msecs");
                printf("\tother:   %llu calls, %lld ms\n", cast(ulong)numOthers, toDuration(otherTime).total!"msecs");
                printf("\tlock time: %lld ms\n", toDuration(lockTime).total!"msecs");

                long apiTime = mallocTime + reallocTime + freeTime + extendTime + otherTime + lockTime;
                printf("\tGC API: %lld ms\n", toDuration(apiTime).total!"msecs");
                sprintf(apitxt.ptr, " API%5lld ms", toDuration(apiTime).total!"msecs");
            }

            printf("GC summary:%5lld MB,%5lld GC%5lld ms, Pauses%5lld ms <%5lld ms%s\n",
                   cast(long) maxPoolMemory >> 20, cast(ulong)numCollections, gcTime,
                   pauseTime, maxPause, apitxt.ptr);
        }

        version (Posix)
            instance = null;
        version (COLLECT_PARALLEL)
            stopScanThreads();

        debug(INVARIANT) initialized = false;

        foreach (Pool* pool; this.pooltable[])
        {
            mappedPages -= pool.npages;
            pool.Dtor();
            cstdlib.free(pool);
        }
        assert(!mappedPages);
        pooltable.Dtor();

        roots.removeAll();
        ranges.removeAll();
        toscanConservative.reset();
        toscanPrecise.reset();
    }


    void Invariant() const { }

    debug(INVARIANT)
    invariant
    {
        if (initialized)
        {
            //printf("Gcx.invariant(): this = %p\n", &this);
            pooltable.Invariant();
            for (size_t p = 0; p < pooltable.length; p++)
                if (pooltable.pools[p].isLargeObject)
                    (cast(LargeObjectPool*)(pooltable.pools[p])).Invariant();
                else
                    (cast(SmallObjectPool*)(pooltable.pools[p])).Invariant();

            if (!inCollection)
                (cast()rangesLock).lock();
            foreach (range; ranges)
            {
                assert(range.pbot);
                assert(range.ptop);
                assert(range.pbot <= range.ptop);
            }
            if (!inCollection)
                (cast()rangesLock).unlock();

            for (size_t i = 0; i < Bins.B_NUMSMALL; i++)
            {
                size_t j = 0;
                List* prev, pprev, ppprev; // keep a short history to inspect in the debugger
                for (auto list = cast(List*)bucket[i]; list; list = list.next)
                {
                    auto pool = list.pool;
                    auto biti = cast(size_t)(cast(void*)list - pool.baseAddr) >> Pool.ShiftBy.Small;
                    assert(pool.freebits.test(biti));
                    ppprev = pprev;
                    pprev = prev;
                    prev = list;
                }
            }
        }
    }

    @property bool collectInProgress() const nothrow
    {
        version (COLLECT_FORK)
            return markProcPid != 0;
        else
            return false;
    }


    /**
     *
     */
    void addRoot(void *p) nothrow @nogc
    {
        rootsLock.lock();
        scope (failure) rootsLock.unlock();
        roots.insert(Root(p));
        rootsLock.unlock();
    }


    /**
     *
     */
    void removeRoot(void *p) nothrow @nogc
    {
        rootsLock.lock();
        scope (failure) rootsLock.unlock();
        roots.remove(Root(p));
        rootsLock.unlock();
    }


    /**
     *
     */
    int rootsApply(scope int delegate(ref Root) nothrow dg) nothrow
    {
        rootsLock.lock();
        scope (failure) rootsLock.unlock();
        auto ret = roots.opApply(dg);
        rootsLock.unlock();
        return ret;
    }


    /**
     *
     */
    void addRange(void *pbot, void *ptop, const TypeInfo ti) nothrow @nogc
    {
        //debug(PRINTF) printf("Thread %x ", pthread_self());
        debug(PRINTF) printf("%p.Gcx::addRange(%p, %p)\n", &this, pbot, ptop);
        rangesLock.lock();
        scope (failure) rangesLock.unlock();
        ranges.insert(Range(pbot, ptop));
        rangesLock.unlock();
    }


    /**
     *
     */
    void removeRange(void *pbot) nothrow @nogc
    {
        //debug(PRINTF) printf("Thread %x ", pthread_self());
        debug(PRINTF) printf("Gcx.removeRange(%p)\n", pbot);
        rangesLock.lock();
        scope (failure) rangesLock.unlock();
        ranges.remove(Range(pbot, pbot)); // only pbot is used, see Range.opCmp
        rangesLock.unlock();

        // debug(PRINTF) printf("Wrong thread\n");
        // This is a fatal error, but ignore it.
        // The problem is that we can get a Close() call on a thread
        // other than the one the range was allocated on.
        //assert(zero);
    }

    /**
     *
     */
    int rangesApply(scope int delegate(ref Range) nothrow dg) nothrow
    {
        rangesLock.lock();
        scope (failure) rangesLock.unlock();
        auto ret = ranges.opApply(dg);
        rangesLock.unlock();
        return ret;
    }


    /**
     *
     */
    void runFinalizers(const scope void[] segment) nothrow
    {
        ConservativeGC._inFinalizer = true;
        scope (failure) ConservativeGC._inFinalizer = false;

        foreach (pool; this.pooltable[])
        {
            if (!pool.finals.nbits) continue;

            if (pool.isLargeObject)
            {
                auto lpool = cast(LargeObjectPool*) pool;
                lpool.runFinalizers(segment);
            }
            else
            {
                auto spool = cast(SmallObjectPool*) pool;
                spool.runFinalizers(segment);
            }
        }
        ConservativeGC._inFinalizer = false;
    }

    Pool* findPool(void* p) pure nothrow @nogc
    {
        return pooltable.findPool(p);
    }

    /**
     * Find base address of block containing pointer p.
     * Returns null if not a gc'd pointer
     */
    void* findBase(void *p) nothrow @nogc
    {
        Pool *pool;

        pool = findPool(p);
        if (pool)
            return pool.findBase(p);
        return null;
    }


    /**
     * Find size of pointer p.
     * Returns 0 if not a gc'd pointer
     */
    size_t findSize(void *p) nothrow @nogc
    {
        Pool* pool = findPool(p);
        if (pool)
            return pool.slGetSize(p);
        return 0;
    }

    /**
     *
     */
    BlkInfo getInfo(void* p) nothrow
    {
        Pool* pool = findPool(p);
        if (pool)
            return pool.slGetInfo(p);
        return BlkInfo();
    }

    /**
     * Computes the bin table using CTFE.
     */
    static Bins[2049] ctfeBins() nothrow
    {
        Bins[2049] ret;
        size_t p = 0;
        for (Bins b = Bins.B_16; b <= Bins.B_2048; b++)
            for ( ; p <= binsize[b]; p++)
                ret[p] = b;

        return ret;
    }

    static immutable Bins[2049] binTable = ctfeBins();

    /**
     * Allocate a new pool of at least size bytes.
     * Sort it into pooltable[].
     * Mark all memory in the pool as B_FREE.
     * Return the actual number of bytes reserved or 0 on error.
     */
    size_t reserve(size_t size) nothrow
    {
        size_t npages = (size + PAGESIZE - 1) / PAGESIZE;

        // Assume reserve() is for small objects.
        Pool*  pool = newPool(npages, false);

        if (!pool)
            return 0;
        return pool.npages * PAGESIZE;
    }

    /**
     * Update the thresholds for when to collect the next time
     */
    void updateCollectThresholds() nothrow
    {
        static float max(float a, float b) nothrow
        {
            return a >= b ? a : b;
        }

        // instantly increases, slowly decreases
        static float smoothDecay(float oldVal, float newVal) nothrow
        {
            // decay to 63.2% of newVal over 5 collections
            // http://en.wikipedia.org/wiki/Low-pass_filter#Simple_infinite_impulse_response_filter
            enum alpha = 1.0 / (5 + 1);
            immutable decay = (newVal - oldVal) * alpha + oldVal;
            return max(newVal, decay);
        }

        immutable smTarget = usedSmallPages * config.heapSizeFactor;
        smallCollectThreshold = smoothDecay(smallCollectThreshold, smTarget);
        immutable lgTarget = usedLargePages * config.heapSizeFactor;
        largeCollectThreshold = smoothDecay(largeCollectThreshold, lgTarget);
    }

    /**
     * Minimizes physical memory usage by returning free pools to the OS.
     */
    void minimize() nothrow
    {
        debug(PRINTF) printf("Minimizing.\n");

        foreach (pool; pooltable.minimize())
        {
            debug(PRINTF) printFreeInfo(pool);
            mappedPages -= pool.npages;
            pool.Dtor();
            cstdlib.free(pool);
        }

        debug(PRINTF) printf("Done minimizing.\n");
    }

    private @property bool lowMem() const nothrow
    {
        return isLowOnMem(cast(size_t)mappedPages * PAGESIZE);
    }

    void* alloc(size_t size, ref size_t alloc_size, uint bits, const TypeInfo ti) nothrow
    {
        return size <= PAGESIZE/2 ? smallAlloc(size, alloc_size, bits, ti)
                                  : bigAlloc(size, alloc_size, bits, ti);
    }

    void* smallAlloc(size_t size, ref size_t alloc_size, uint bits, const TypeInfo ti) nothrow
    {
        immutable bin = binTable[size];
        alloc_size = binsize[bin];

        void* p = bucket[bin];
        if (p)
            goto L_hasBin;

        if (recoverPool[bin])
            recoverNextPage(bin);

        bool tryAlloc() nothrow
        {
            if (!bucket[bin])
            {
                bucket[bin] = allocPage(bin);
                if (!bucket[bin])
                    return false;
            }
            p = bucket[bin];
            return true;
        }

        if (!tryAlloc())
        {
            if (!lowMem && (disabled || usedSmallPages < smallCollectThreshold))
            {
                // disabled or threshold not reached => allocate a new pool instead of collecting
                if (!newPool(1, false))
                {
                    // out of memory => try to free some memory
                    fullcollect(false, true); // stop the world
                    if (lowMem)
                        minimize();
                    recoverNextPage(bin);
                }
            }
            else if (usedSmallPages > 0)
            {
                fullcollect();
                if (lowMem)
                    minimize();
                recoverNextPage(bin);
            }
            // tryAlloc will succeed if a new pool was allocated above, if it fails allocate a new pool now
            if (!tryAlloc() && (!newPool(1, false) || !tryAlloc()))
                // out of luck or memory
                onOutOfMemoryError();
        }
        assert(p !is null);
    L_hasBin:
        // Return next item from free list
        bucket[bin] = undefinedRead((cast(List*)p).next);
        auto pool = undefinedRead((cast(List*)p).pool);

        auto biti = (p - pool.baseAddr) >> pool.shiftBy;
        assert(pool.freebits.test(biti));
        if (collectInProgress)
            pool.mark.setLocked(biti); // be sure that the child is aware of the page being used
        pool.freebits.clear(biti);
        if (bits)
            pool.setBits(biti, bits);
        //debug(PRINTF) printf("\tmalloc => %p\n", p);
        invalidate(p[0 .. alloc_size], 0xF0, true);

        if (ConservativeGC.isPrecise)
        {
            debug(SENTINEL)
                pool.setPointerBitmapSmall(sentinel_add(p), size - SENTINEL_EXTRA, size - SENTINEL_EXTRA, bits, ti);
            else
                pool.setPointerBitmapSmall(p, size, alloc_size, bits, ti);
        }
        return p;
    }

    /**
     * Allocate a chunk of memory that is larger than a page.
     * Return null if out of memory.
     */
    void* bigAlloc(size_t size, ref size_t alloc_size, uint bits, const TypeInfo ti = null) nothrow
    {
        debug(PRINTF) printf("In bigAlloc.  Size:  %zd\n", size);

        LargeObjectPool* pool;
        size_t pn;
        immutable npages = LargeObjectPool.numPages(size);
        if (npages == size_t.max)
            onOutOfMemoryError(); // size just below size_t.max requested

        bool tryAlloc() nothrow
        {
            foreach (p; this.pooltable[])
            {
                if (!p.isLargeObject || p.freepages < npages)
                    continue;
                auto lpool = cast(LargeObjectPool*) p;
                if ((pn = lpool.allocPages(npages)) == OPFAIL)
                    continue;
                pool = lpool;
                return true;
            }
            return false;
        }

        bool tryAllocNewPool() nothrow
        {
            pool = cast(LargeObjectPool*) newPool(npages, true);
            if (!pool) return false;
            pn = pool.allocPages(npages);
            assert(pn != OPFAIL);
            return true;
        }

        if (!tryAlloc())
        {
            if (!lowMem && (disabled || usedLargePages < largeCollectThreshold))
            {
                // disabled or threshold not reached => allocate a new pool instead of collecting
                if (!tryAllocNewPool())
                {
                    // disabled but out of memory => try to free some memory
                    minimizeAfterNextCollection = true;
                    fullcollect(false, true);
                }
            }
            else if (usedLargePages > 0)
            {
                minimizeAfterNextCollection = true;
                fullcollect();
            }
            // If alloc didn't yet succeed retry now that we collected/minimized
            if (!pool && !tryAlloc() && !tryAllocNewPool())
                // out of luck or memory
                return null;
        }
        assert(pool);

        debug(PRINTF) printFreeInfo(&pool.base);
        if (collectInProgress)
            pool.mark.setLocked(pn);
        usedLargePages += npages;

        debug(PRINTF) printFreeInfo(&pool.base);

        auto p = pool.baseAddr + pn * PAGESIZE;
        debug(PRINTF) printf("Got large alloc:  %p, pt = %d, np = %zd\n", p, pool.pagetable[pn], npages);
        invalidate(p[0 .. size], 0xF1, true);
        alloc_size = npages * PAGESIZE;
        //debug(PRINTF) printf("\tp = %p\n", p);

        if (bits)
            pool.setBits(pn, bits);

        if (ConservativeGC.isPrecise)
        {
            // an array of classes is in fact an array of pointers
            immutable(void)* rtinfo;
            if (!ti)
                rtinfo = rtinfoHasPointers;
            else if ((bits & BlkAttr.APPENDABLE) && (typeid(ti) is typeid(TypeInfo_Class)))
                rtinfo = rtinfoHasPointers;
            else
                rtinfo = ti.rtInfo();
            pool.rtinfo[pn] = cast(immutable(size_t)*)rtinfo;
        }

        return p;
    }


    /**
     * Allocate a new pool with at least npages in it.
     * Sort it into pooltable[].
     * Return null if failed.
     */
    Pool *newPool(size_t npages, bool isLargeObject) nothrow
    {
        //debug(PRINTF) printf("************Gcx::newPool(npages = %d)****************\n", npages);

        // Minimum of POOLSIZE
        size_t minPages = config.minPoolSize / PAGESIZE;
        if (npages < minPages)
            npages = minPages;
        else if (npages > minPages)
        {   // Give us 150% of requested size, so there's room to extend
            auto n = npages + (npages >> 1);
            if (n < size_t.max/PAGESIZE)
                npages = n;
        }

        // Allocate successively larger pools up to 8 megs
        if (this.pooltable.length)
        {
            size_t n;

            n = config.minPoolSize + config.incPoolSize * this.pooltable.length;
            if (n > config.maxPoolSize)
                n = config.maxPoolSize;                 // cap pool size
            n /= PAGESIZE; // convert bytes to pages
            if (npages < n)
                npages = n;
        }

        //printf("npages = %d\n", npages);

        auto pool = cast(Pool *)cstdlib.calloc(1, isLargeObject ? LargeObjectPool.sizeof : SmallObjectPool.sizeof);
        if (pool)
        {
            pool.initialize(npages, isLargeObject);
            if (collectInProgress)
                pool.mark.setAll();
            if (!pool.baseAddr || !pooltable.insert(pool))
            {
                pool.Dtor();
                cstdlib.free(pool);
                return null;
            }
        }

        mappedPages += npages;

        if (config.profile)
        {
            if (cast(size_t)mappedPages * PAGESIZE > maxPoolMemory)
                maxPoolMemory = cast(size_t)mappedPages * PAGESIZE;
        }
        return pool;
    }

    /**
    * Allocate a page of bin's.
    * Returns:
    *           head of a single linked list of new entries
    */
    List* allocPage(Bins bin) nothrow
    {
        //debug(PRINTF) printf("Gcx::allocPage(bin = %d)\n", bin);
        foreach (Pool* pool; this.pooltable[])
        {
            if (pool.isLargeObject)
                continue;
            if (List* p = (cast(SmallObjectPool*)pool).allocPage(bin))
            {
                ++usedSmallPages;
                return p;
            }
        }
        return null;
    }

    static struct ScanRange(bool precise)
    {
        void* pbot;
        void* ptop;
        static if (precise)
        {
            void** pbase;      // start of memory described by ptrbitmap
            size_t* ptrbmp;    // bits from is_pointer or rtinfo
            size_t bmplength;  // number of valid bits
        }
    }

    static struct ToScanStack(RANGE)
    {
    nothrow:
        @disable this(this);
        auto stackLock = shared(AlignedSpinLock)(SpinLock.Contention.brief);

        void reset()
        {
            _length = 0;
            if (_p)
            {
                os_mem_unmap(_p, _cap * RANGE.sizeof);
                _p = null;
            }
            _cap = 0;
        }
        void clear()
        {
            _length = 0;
        }

        void push(RANGE rng)
        {
            if (_length == _cap) grow();
            _p[_length++] = rng;
        }

        RANGE pop()
        in { assert(!empty); }
        do
        {
            return _p[--_length];
        }

        bool popLocked(ref RANGE rng)
        {
            if (_length == 0)
                return false;

            stackLock.lock();
            scope(exit) stackLock.unlock();
            if (_length == 0)
                return false;
            rng = _p[--_length];
            return true;
        }

        ref inout(RANGE) opIndex(size_t idx) inout
        in { assert(idx < _length); }
        do
        {
            return _p[idx];
        }

        @property size_t length() const { return _length; }
        @property bool empty() const { return !length; }

    private:
        void grow()
        {
            pragma(inline, false);

            enum initSize = 64 * 1024; // Windows VirtualAlloc granularity
            immutable ncap = _cap ? 2 * _cap : initSize / RANGE.sizeof;
            auto p = cast(RANGE*)os_mem_map(ncap * RANGE.sizeof);
            if (p is null) onOutOfMemoryError();
            debug (VALGRIND) makeMemUndefined(p[0..ncap]);
            if (_p !is null)
            {
                p[0 .. _length] = _p[0 .. _length];
                os_mem_unmap(_p, _cap * RANGE.sizeof);
            }
            _p = p;
            _cap = ncap;
        }

        size_t _length;
        RANGE* _p;
        size_t _cap;
    }

    ToScanStack!(ScanRange!false) toscanConservative;
    ToScanStack!(ScanRange!true) toscanPrecise;

    template scanStack(bool precise)
    {
        static if (precise)
            alias scanStack = toscanPrecise;
        else
            alias scanStack = toscanConservative;
    }

    /**
     * Search a range of memory values and mark any pointers into the GC pool.
     */
    private void mark(bool precise, bool parallel, bool shared_mem)(ScanRange!precise rng) scope nothrow
    {
        alias toscan = scanStack!precise;

        debug(MARK_PRINTF)
            printf("marking range: [%p..%p] (%#llx)\n", rng.pbot, rng.ptop, cast(long)(rng.ptop - rng.pbot));

        // limit the amount of ranges added to the toscan stack
        enum FANOUT_LIMIT = 32;
        size_t stackPos;
        ScanRange!precise[FANOUT_LIMIT] stack = void;

        size_t pcache = 0;

        // let dmd allocate a register for this.pools
        auto pools = pooltable.pools;
        const highpool = pooltable.length - 1;
        const minAddr = pooltable.minAddr;
        size_t memSize = pooltable.maxAddr - minAddr;
        Pool* pool = null;

        // properties of allocation pointed to
        ScanRange!precise tgt = void;

        for (;;)
        {
            auto p = undefinedRead(*cast(void**)(rng.pbot));
            debug (VALGRIND) makeMemDefined((&p)[0 .. 1]);

            debug(MARK_PRINTF) printf("\tmark %p: %p\n", rng.pbot, p);

            if (cast(size_t)(p - minAddr) < memSize &&
                (cast(size_t)p & ~cast(size_t)(PAGESIZE-1)) != pcache)
            {
                static if (precise) if (rng.pbase)
                {
                    size_t bitpos = cast(void**)rng.pbot - rng.pbase;
                    while (bitpos >= rng.bmplength)
                    {
                        bitpos -= rng.bmplength;
                        rng.pbase += rng.bmplength;
                    }
                    if (!core.bitop.bt(rng.ptrbmp, bitpos))
                    {
                        debug(MARK_PRINTF) printf("\t\tskipping non-pointer\n");
                        goto LnextPtr;
                    }
                }

                if (!pool || p < pool.baseAddr || p >= pool.topAddr)
                {
                    size_t low = 0;
                    size_t high = highpool;
                    while (true)
                    {
                        size_t mid = (low + high) >> 1;
                        pool = pools[mid];
                        if (p < pool.baseAddr)
                            high = mid - 1;
                        else if (p >= pool.topAddr)
                            low = mid + 1;
                        else break;

                        if (low > high)
                            goto LnextPtr;
                    }
                }
                size_t offset = cast(size_t)(p - pool.baseAddr);
                size_t biti = void;
                size_t pn = offset / PAGESIZE;
                size_t bin = pool.pagetable[pn]; // not Bins to avoid multiple size extension instructions

                debug(MARK_PRINTF)
                    printf("\t\tfound pool %p, base=%p, pn = %llu, bin = %llu\n", pool, pool.baseAddr, cast(ulong)pn, cast(ulong)bin);

                // Adjust bit to be at start of allocated memory block
                if (bin < Bins.B_PAGE)
                {
                    // We don't care abou setting pointsToBase correctly
                    // because it's ignored for small object pools anyhow.
                    auto offsetBase = baseOffset(offset, cast(Bins)bin);
                    biti = offsetBase >> Pool.ShiftBy.Small;
                    //debug(PRINTF) printf("\t\tbiti = x%x\n", biti);

                    if (!pool.mark.testAndSet!shared_mem(biti) && !pool.noscan.test(biti))
                    {
                        tgt.pbot = pool.baseAddr + offsetBase;
                        tgt.ptop = tgt.pbot + binsize[bin];
                        static if (precise)
                        {
                            tgt.pbase = cast(void**)pool.baseAddr;
                            tgt.ptrbmp = pool.is_pointer.data;
                            tgt.bmplength = size_t.max; // no repetition
                        }
                        goto LaddRange;
                    }
                }
                else if (bin == Bins.B_PAGE)
                {
                    biti = offset >> Pool.ShiftBy.Large;
                    //debug(PRINTF) printf("\t\tbiti = x%x\n", biti);

                    pcache = cast(size_t)p & ~cast(size_t)(PAGESIZE-1);
                    tgt.pbot = cast(void*)pcache;

                    // For the NO_INTERIOR attribute.  This tracks whether
                    // the pointer is an interior pointer or points to the
                    // base address of a block.
                    if (tgt.pbot != sentinel_sub(p) && pool.nointerior.nbits && pool.nointerior.test(biti))
                        goto LnextPtr;

                    if (!pool.mark.testAndSet!shared_mem(biti) && !pool.noscan.test(biti))
                    {
                        tgt.ptop = tgt.pbot + (cast(LargeObjectPool*)pool).getSize(pn);
                        goto LaddLargeRange;
                    }
                }
                else if (bin == Bins.B_PAGEPLUS)
                {
                    pn -= pool.bPageOffsets[pn];
                    biti = pn * (PAGESIZE >> Pool.ShiftBy.Large);

                    pcache = cast(size_t)p & ~cast(size_t)(PAGESIZE-1);
                    if (pool.nointerior.nbits && pool.nointerior.test(biti))
                        goto LnextPtr;

                    if (!pool.mark.testAndSet!shared_mem(biti) && !pool.noscan.test(biti))
                    {
                        tgt.pbot = pool.baseAddr + (pn * PAGESIZE);
                        tgt.ptop = tgt.pbot + (cast(LargeObjectPool*)pool).getSize(pn);
                    LaddLargeRange:
                        static if (precise)
                        {
                            auto rtinfo = pool.rtinfo[biti];
                            if (rtinfo is rtinfoNoPointers)
                                goto LnextPtr; // only if inconsistent with noscan
                            if (rtinfo is rtinfoHasPointers)
                            {
                                tgt.pbase = null; // conservative
                            }
                            else
                            {
                                tgt.ptrbmp = cast(size_t*)rtinfo;
                                size_t element_size = *tgt.ptrbmp++;
                                tgt.bmplength = (element_size + (void*).sizeof - 1) / (void*).sizeof;
                                assert(tgt.bmplength);

                                debug(SENTINEL)
                                    tgt.pbot = sentinel_add(tgt.pbot);
                                if (pool.appendable.test(biti))
                                {
                                    // take advantage of knowing array layout in rt.lifetime
                                    void* arrtop = tgt.pbot + 16 + *cast(size_t*)tgt.pbot;
                                    assert (arrtop > tgt.pbot && arrtop <= tgt.ptop);
                                    tgt.pbot += 16;
                                    tgt.ptop = arrtop;
                                }
                                else
                                {
                                    tgt.ptop = tgt.pbot + element_size;
                                }
                                tgt.pbase = cast(void**)tgt.pbot;
                            }
                        }
                        goto LaddRange;
                    }
                }
                else
                {
                    // Don't mark bits in B_FREE pages
                    assert(bin == Bins.B_FREE);
                }
            }
        LnextPtr:
            rng.pbot += (void*).sizeof;
            if (rng.pbot < rng.ptop)
                continue;

        LnextRange:
            if (stackPos)
            {
                // pop range from local stack and recurse
                rng = stack[--stackPos];
            }
            else
            {
                static if (parallel)
                {
                    if (!toscan.popLocked(rng))
                        break; // nothing more to do
                }
                else
                {
                    if (toscan.empty)
                        break; // nothing more to do

                    // pop range from global stack and recurse
                    rng = toscan.pop();
                }
            }
            // printf("  pop [%p..%p] (%#zx)\n", p1, p2, cast(size_t)p2 - cast(size_t)p1);
            goto LcontRange;

        LaddRange:
            rng.pbot += (void*).sizeof;
            if (rng.pbot < rng.ptop)
            {
                if (stackPos < stack.length)
                {
                    stack[stackPos] = tgt;
                    stackPos++;
                    continue;
                }
                static if (parallel)
                {
                    toscan.stackLock.lock();
                    scope(exit) toscan.stackLock.unlock();
                }
                toscan.push(rng);
                // reverse order for depth-first-order traversal
                foreach_reverse (ref range; stack)
                    toscan.push(range);
                stackPos = 0;
            }
        LendOfRange:
            // continue with last found range
            rng = tgt;

        LcontRange:
            pcache = 0;
        }
    }

    void markConservative(bool shared_mem)(void *pbot, void *ptop) scope nothrow
    {
        if (pbot < ptop)
            mark!(false, false, shared_mem)(ScanRange!false(pbot, ptop));
    }

    void markPrecise(bool shared_mem)(void *pbot, void *ptop) scope nothrow
    {
        if (pbot < ptop)
            mark!(true, false, shared_mem)(ScanRange!true(pbot, ptop, null));
    }

    version (COLLECT_PARALLEL)
    ToScanStack!(void*) toscanRoots;

    version (COLLECT_PARALLEL)
    void collectRoots(void *pbot, void *ptop) scope nothrow
    {
        const minAddr = pooltable.minAddr;
        size_t memSize = pooltable.maxAddr - minAddr;

        for (auto p = cast(void**)pbot; cast(void*)p < ptop; p++)
        {
            auto ptr = *p;
            debug (VALGRIND) makeMemDefined((&ptr)[0 .. 1]);
            if (cast(size_t)(ptr - minAddr) < memSize)
                toscanRoots.push(ptr);
        }
    }

    // collection step 1: prepare freebits and mark bits
    void prepare() nothrow
    {
        debug(COLLECT_PRINTF) printf("preparing mark.\n");

        foreach (Pool* pool; this.pooltable[])
        {
            if (pool.isLargeObject)
                pool.mark.zero();
            else
                pool.mark.copy(&pool.freebits);
        }
    }

    // collection step 2: mark roots and heap
    void markAll(alias markFn)() nothrow
    {
        debug(COLLECT_PRINTF) printf("\tscan stacks.\n");
        // Scan stacks registers, and TLS for each paused thread
        thread_scanAll(&markFn);

        // Scan roots[]
        debug(COLLECT_PRINTF) printf("\tscan roots[]\n");
        foreach (root; roots)
        {
            markFn(cast(void*)&root.proot, cast(void*)(&root.proot + 1));
        }

        // Scan ranges[]
        debug(COLLECT_PRINTF) printf("\tscan ranges[]\n");
        //log++;
        foreach (range; ranges)
        {
            debug(COLLECT_PRINTF) printf("\t\t%p .. %p\n", range.pbot, range.ptop);
            markFn(range.pbot, range.ptop);
        }
        //log--;
    }

    version (COLLECT_PARALLEL)
    void collectAllRoots() nothrow
    {
        debug(COLLECT_PRINTF) printf("\tcollect stacks.\n");
        // Scan stacks registers and TLS for each paused thread
        thread_scanAll(&collectRoots);

        // Scan roots[]
        debug(COLLECT_PRINTF) printf("\tcollect roots[]\n");
        foreach (root; roots)
        {
            toscanRoots.push(root);
        }

        // Scan ranges[]
        debug(COLLECT_PRINTF) printf("\tcollect ranges[]\n");
        foreach (range; ranges)
        {
            debug(COLLECT_PRINTF) printf("\t\t%p .. %p\n", range.pbot, range.ptop);
            collectRoots(range.pbot, range.ptop);
        }
    }

    // collection step 3: finalize unreferenced objects, recover full pages with no live objects
    size_t sweep() nothrow
    {
        // Free up everything not marked
        debug(COLLECT_PRINTF) printf("\tfree'ing\n");
        size_t freedLargePages;
        size_t freedSmallPages;
        size_t freed;
        foreach (Pool* pool; this.pooltable[])
        {
            size_t pn;

            if (pool.isLargeObject)
            {
                auto lpool = cast(LargeObjectPool*)pool;
                size_t numFree = 0;
                size_t npages;
                for (pn = 0; pn < pool.npages; pn += npages)
                {
                    npages = pool.bPageOffsets[pn];
                    Bins bin = cast(Bins)pool.pagetable[pn];
                    if (bin == Bins.B_FREE)
                    {
                        numFree += npages;
                        continue;
                    }
                    assert(bin == Bins.B_PAGE);
                    size_t biti = pn;

                    if (!pool.mark.test(biti))
                    {
                        void *p = pool.baseAddr + pn * PAGESIZE;
                        void* q = sentinel_add(p);
                        sentinel_Invariant(q);

                        if (pool.finals.nbits && pool.finals.clear(biti))
                        {
                            size_t size = npages * PAGESIZE - SENTINEL_EXTRA;
                            uint attr = pool.getBits(biti);
                            rt_finalizeFromGC(q, sentinel_size(q, size), attr);
                        }

                        pool.clrBits(biti, ~BlkAttr.NONE ^ BlkAttr.FINALIZE);

                        debug(COLLECT_PRINTF) printf("\tcollecting big %p\n", p);
                        leakDetector.log_free(q, sentinel_size(q, npages * PAGESIZE - SENTINEL_EXTRA));
                        pool.pagetable[pn..pn+npages] = Bins.B_FREE;
                        if (pn < pool.searchStart) pool.searchStart = pn;
                        freedLargePages += npages;
                        pool.freepages += npages;
                        numFree += npages;

                        invalidate(p[0 .. npages * PAGESIZE], 0xF3, false);
                        // Don't need to update searchStart here because
                        // pn is guaranteed to be greater than last time
                        // we updated it.

                        pool.largestFree = pool.freepages; // invalidate
                    }
                    else
                    {
                        if (numFree > 0)
                        {
                            lpool.setFreePageOffsets(pn - numFree, numFree);
                            numFree = 0;
                        }
                    }
                }
                if (numFree > 0)
                    lpool.setFreePageOffsets(pn - numFree, numFree);
            }
            else
            {
                // reinit chain of pages to rebuild free list
                pool.recoverPageFirst[] = cast(uint)pool.npages;

                for (pn = 0; pn < pool.npages; pn++)
                {
                    Bins bin = cast(Bins)pool.pagetable[pn];

                    if (bin < Bins.B_PAGE)
                    {
                        auto freebitsdata = pool.freebits.data + pn * PageBits.length;
                        auto markdata = pool.mark.data + pn * PageBits.length;

                        // the entries to free are allocated objects (freebits == false)
                        // that are not marked (mark == false)
                        PageBits toFree;
                        static foreach (w; 0 .. PageBits.length)
                            toFree[w] = (~freebitsdata[w] & ~markdata[w]);

                        // the page is unchanged if there is nothing to free
                        bool unchanged = true;
                        static foreach (w; 0 .. PageBits.length)
                            unchanged = unchanged && (toFree[w] == 0);
                        if (unchanged)
                        {
                            bool hasDead = false;
                            static foreach (w; 0 .. PageBits.length)
                                hasDead = hasDead || (~freebitsdata[w] != baseOffsetBits[bin][w]);
                            if (hasDead)
                            {
                                // add to recover chain
                                pool.binPageChain[pn] = pool.recoverPageFirst[bin];
                                pool.recoverPageFirst[bin] = cast(uint)pn;
                            }
                            else
                            {
                                pool.binPageChain[pn] = Pool.PageRecovered;
                            }
                            continue;
                        }

                        // the page can be recovered if all of the allocated objects (freebits == false)
                        // are freed
                        bool recoverPage = true;
                        static foreach (w; 0 .. PageBits.length)
                            recoverPage = recoverPage && (~freebitsdata[w] == toFree[w]);

                        // We need to loop through each object if any have a finalizer,
                        // or, if any of the debug hooks are enabled.
                        bool doLoop = false;
                        debug (SENTINEL)
                            doLoop = true;
                        else version (assert)
                            doLoop = true;
                        else debug (COLLECT_PRINTF) // need output for each object
                            doLoop = true;
                        else debug (LOGGING)
                            doLoop = true;
                        else debug (MEMSTOMP)
                            doLoop = true;
                        else if (pool.finals.data)
                        {
                            // finalizers must be called on objects that are about to be freed
                            auto finalsdata = pool.finals.data + pn * PageBits.length;
                            static foreach (w; 0 .. PageBits.length)
                                doLoop = doLoop || (toFree[w] & finalsdata[w]) != 0;
                        }

                        if (doLoop)
                        {
                            immutable size = binsize[bin];
                            void *p = pool.baseAddr + pn * PAGESIZE;
                            immutable base = pn * (PAGESIZE/16);
                            immutable bitstride = size / 16;

                            // ensure that there are at least <size> bytes for every address
                            //  below ptop even if unaligned
                            void *ptop = p + PAGESIZE - size + 1;
                            for (size_t i; p < ptop; p += size, i += bitstride)
                            {
                                immutable biti = base + i;

                                if (!pool.mark.test(biti))
                                {
                                    void* q = sentinel_add(p);
                                    sentinel_Invariant(q);

                                    if (pool.finals.nbits && pool.finals.test(biti))
                                        rt_finalizeFromGC(q, sentinel_size(q, size), pool.getBits(biti));

                                    assert(core.bitop.bt(toFree.ptr, i));

                                    debug(COLLECT_PRINTF) printf("\tcollecting %p\n", p);
                                    leakDetector.log_free(q, sentinel_size(q, size));

                                    invalidate(p[0 .. size], 0xF3, false);
                                }
                            }
                        }

                        if (recoverPage)
                        {
                            pool.freeAllPageBits(pn);

                            pool.pagetable[pn] = Bins.B_FREE;
                            // add to free chain
                            pool.binPageChain[pn] = cast(uint) pool.searchStart;
                            pool.searchStart = pn;
                            pool.freepages++;
                            freedSmallPages++;
                        }
                        else
                        {
                            pool.freePageBits(pn, toFree);

                            // add to recover chain
                            pool.binPageChain[pn] = pool.recoverPageFirst[bin];
                            pool.recoverPageFirst[bin] = cast(uint)pn;
                        }
                    }
                }
            }
        }

        assert(freedLargePages <= usedLargePages);
        usedLargePages -= freedLargePages;
        debug(COLLECT_PRINTF) printf("\tfree'd %u bytes, %u pages from %u pools\n",
                                     freed, freedLargePages, this.pooltable.length);

        assert(freedSmallPages <= usedSmallPages);
        usedSmallPages -= freedSmallPages;
        debug(COLLECT_PRINTF) printf("\trecovered small pages = %d\n", freedSmallPages);

        return freedLargePages + freedSmallPages;
    }

    bool recoverPage(SmallObjectPool* pool, size_t pn, Bins bin) nothrow
    {
        size_t size = binsize[bin];
        size_t bitbase = pn * (PAGESIZE / 16);

        auto freebitsdata = pool.freebits.data + pn * PageBits.length;

        // the page had dead objects when collecting, these cannot have been resurrected
        bool hasDead = false;
        static foreach (w; 0 .. PageBits.length)
            hasDead = hasDead || (freebitsdata[w] != 0);
        assert(hasDead);

        // prepend to buckets, but with forward addresses inside the page
        assert(bucket[bin] is null);
        List** bucketTail = &bucket[bin];

        void* p = pool.baseAddr + pn * PAGESIZE;
        const top = PAGESIZE - size + 1; // ensure <size> bytes available even if unaligned
        for (size_t u = 0; u < top; u += size)
        {
            if (!core.bitop.bt(freebitsdata, u / 16))
                continue;
            auto elem = cast(List *)(p + u);
            undefinedWrite(elem.pool, &pool.base);
            undefinedWrite(*bucketTail, elem);
            bucketTail = &elem.next;
        }
        undefinedWrite(*bucketTail, null);
        assert(bucket[bin] !is null);
        return true;
    }

    bool recoverNextPage(Bins bin) nothrow
    {
        SmallObjectPool* pool = recoverPool[bin];
        while (pool)
        {
            auto pn = pool.recoverPageFirst[bin];
            while (pn < pool.npages)
            {
                auto next = pool.binPageChain[pn];
                pool.binPageChain[pn] = Pool.PageRecovered;
                pool.recoverPageFirst[bin] = next;
                if (recoverPage(pool, pn, bin))
                    return true;
                pn = next;
            }
            pool = setNextRecoverPool(bin, pool.ptIndex + 1);
        }
        return false;
    }

    private SmallObjectPool* setNextRecoverPool(Bins bin, size_t poolIndex) nothrow
    {
        Pool* pool;
        while (poolIndex < this.pooltable.length &&
               ((pool = this.pooltable[poolIndex]).isLargeObject ||
                pool.recoverPageFirst[bin] >= pool.npages))
            poolIndex++;

        return recoverPool[bin] = poolIndex < this.pooltable.length ? cast(SmallObjectPool*)pool : null;
    }

    version (COLLECT_FORK)
    void disableFork() nothrow
    {
        markProcPid = 0;
        shouldFork = false;
    }

    version (COLLECT_FORK)
    ChildStatus collectFork(bool block) nothrow
    {
        typeof(return) rc = wait_pid(markProcPid, block);
        final switch (rc)
        {
            case ChildStatus.done:
                debug(COLLECT_PRINTF) printf("\t\tmark proc DONE (block=%d)\n",
                                                cast(int) block);
                markProcPid = 0;
                // process GC marks then sweep
                thread_suspendAll();
                thread_processTLSGCData(&clearBlkCacheData);
                thread_resumeAll();
                break;
            case ChildStatus.running:
                debug(COLLECT_PRINTF) printf("\t\tmark proc RUNNING\n");
                if (!block)
                    break;
                // Something went wrong, if block is true, wait() should never
                // return RUNNING.
                goto case ChildStatus.error;
            case ChildStatus.error:
                debug(COLLECT_PRINTF) printf("\t\tmark proc ERROR\n");
                // Try to keep going without forking
                // and do the marking in this thread
                break;
        }
        return rc;
    }

    version (COLLECT_FORK)
    ChildStatus markFork(bool block, bool doParallel) nothrow
    {
        // Forking is enabled, so we fork() and start a new concurrent mark phase
        // in the child. If the collection should not block, the parent process
        // tells the caller no memory could be recycled immediately (if this collection
        // was triggered by an allocation, the caller should allocate more memory
        // to fulfill the request).
        // If the collection should block, the parent will wait for the mark phase
        // to finish before returning control to the mutator,
        // but other threads are restarted and may run in parallel with the mark phase
        // (unless they allocate or use the GC themselves, in which case
        // the global GC lock will stop them).
        // fork now and sweep later
        int child_mark() scope
        {
            if (doParallel)
                markParallel();
            else if (ConservativeGC.isPrecise)
                markAll!(markPrecise!true)();
            else
                markAll!(markConservative!true)();
            return 0;
        }

        import core.stdc.stdlib : _Exit;
        debug (PRINTF_TO_FILE)
        {
            fflush(null); // avoid duplicated FILE* output
        }
        version (OSX)
        {
            auto pid = __fork(); // avoids calling handlers (from libc source code)
        }
        else version (linux)
        {
            // clone() fits better as we don't want to do anything but scanning in the child process.
            // no fork-handlers are called, so we can avoid deadlocks due to malloc locks. Probably related:
            //  https://sourceware.org/bugzilla/show_bug.cgi?id=4737
            import core.sys.linux.sched : clone;
            import core.sys.posix.signal : SIGCHLD;
            const flags = SIGCHLD; // exit signal
            scope int delegate() scope dg = &child_mark;
            extern(C) static int wrap_delegate(void* arg)
            {
                auto dg = cast(int delegate() scope*)arg;
                return (*dg)();
            }
            ubyte[256] stackbuf; // enough stack space for clone() to place some info for the child without stomping the parent stack
            auto stack = stackbuf.ptr + (isStackGrowingDown ? stackbuf.length : 0);
            auto pid = clone(&wrap_delegate, stack, flags, &dg);
        }
        else
        {
            fork_needs_lock = false;
            auto pid = fork();
            fork_needs_lock = true;
        }
        switch (pid)
        {
            case -1: // fork() failed, retry without forking
                return ChildStatus.error;
            case 0: // child process (not run with clone)
                child_mark();
                _Exit(0);
            default: // the parent
                thread_resumeAll();
                if (!block)
                {
                    markProcPid = pid;
                    return ChildStatus.running;
                }
                ChildStatus r = wait_pid(pid); // block until marking is done
                if (r == ChildStatus.error)
                {
                    thread_suspendAll();
                    // there was an error
                    // do the marking in this thread
                    disableFork();
                    if (doParallel)
                        markParallel();
                    else if (ConservativeGC.isPrecise)
                        markAll!(markPrecise!false)();
                    else
                        markAll!(markConservative!false)();
                } else {
                    assert(r == ChildStatus.done);
                    assert(r != ChildStatus.running);
                }
        }
        return ChildStatus.done; // waited for the child
    }

    /**
     * Return number of full pages free'd.
     * The collection is done concurrently only if block and isFinal are false.
     */
    size_t fullcollect(bool block = false, bool isFinal = false) nothrow
    {
        // It is possible that `fullcollect` will be called from a thread which
        // is not yet registered in runtime (because allocating `new Thread` is
        // part of `thread_attachThis` implementation). In that case it is
        // better not to try actually collecting anything

        if (Thread.getThis() is null)
            return 0;

        MonoTime start, stop, begin;
        begin = start = currTime;

        debug(COLLECT_PRINTF) printf("Gcx.fullcollect()\n");
        version (COLLECT_PARALLEL)
        {
            bool doParallel = config.parallel > 0 && !config.fork;
            if (doParallel && !scanThreadData)
            {
                if (isFinal) // avoid starting threads for parallel marking
                    doParallel = false;
                else
                    startScanThreads();
            }
        }
        else
            enum doParallel = false;

        //printf("\tpool address range = %p .. %p\n", minAddr, maxAddr);

        version (COLLECT_FORK)
            alias doFork = shouldFork;
        else
            enum doFork = false;

        if (doFork && collectInProgress)
        {
            version (COLLECT_FORK)
            {
                // If there is a mark process running, check if it already finished.
                // If that is the case, we move to the sweep phase.
                // If it's still running, either we block until the mark phase is
                // done (and then sweep to finish the collection), or in case of error
                // we redo the mark phase without forking.
                ChildStatus rc = collectFork(block);
                final switch (rc)
                {
                    case ChildStatus.done:
                        break;
                    case ChildStatus.running:
                        return 0;
                    case ChildStatus.error:
                        disableFork();
                        goto Lmark;
                }
            }
        }
        else
        {
Lmark:
            // lock roots and ranges around suspending threads b/c they're not reentrant safe
            rangesLock.lock();
            rootsLock.lock();
            debug(INVARIANT) inCollection = true;
            scope (exit)
            {
                debug(INVARIANT) inCollection = false;
                rangesLock.unlock();
                rootsLock.unlock();
            }
            thread_suspendAll();

            prepare();

            stop = currTime;
            prepTime += (stop - start);
            start = stop;

            if (doFork && !isFinal && !block) // don't start a new fork during termination
            {
                version (COLLECT_FORK)
                {
                    auto forkResult = markFork(block, doParallel);
                    final switch (forkResult)
                    {
                        case ChildStatus.error:
                            // fork() failed, retry without forking
                            disableFork();
                            goto Lmark;
                        case ChildStatus.running:
                            // update profiling informations
                            stop = currTime;
                            markTime += (stop - start);
                            Duration pause = stop - begin;
                            if (pause > maxPauseTime)
                                maxPauseTime = pause;
                            pauseTime += pause;
                            return 0;
                        case ChildStatus.done:
                            break;
                    }
                    // if we get here, forking failed and a standard STW collection got issued
                    // threads were suspended again, restart them
                    thread_suspendAll();
                }
            }
            else if (doParallel)
            {
                version (COLLECT_PARALLEL)
                    markParallel();
            }
            else
            {
                if (ConservativeGC.isPrecise)
                    markAll!(markPrecise!false)();
                else
                    markAll!(markConservative!false)();
            }

            thread_processTLSGCData(&clearBlkCacheData);
            thread_resumeAll();
            isFinal = false;
        }

        // If we get here with the forking GC, the child process has finished the marking phase
        // or block == true and we are using standard stop the world collection.
        // It is time to sweep

        stop = currTime;
        markTime += (stop - start);
        Duration pause = stop - begin;
        if (pause > maxPauseTime)
            maxPauseTime = pause;
        pauseTime += pause;
        start = stop;

        ConservativeGC._inFinalizer = true;
        size_t freedPages = void;
        {
            scope (failure) ConservativeGC._inFinalizer = false;
            freedPages = sweep();
            ConservativeGC._inFinalizer = false;
        }

        // minimize() should be called only after a call to fullcollect
        // terminates with a sweep
        if (minimizeAfterNextCollection || lowMem)
        {
            minimizeAfterNextCollection = false;
            minimize();
        }

        // init bucket lists
        bucket[] = null;
        foreach (Bins bin; Bins.B_16 .. Bins.B_NUMSMALL)
            setNextRecoverPool(bin, 0);

        stop = currTime;
        sweepTime += (stop - start);

        Duration collectionTime = stop - begin;
        if (collectionTime > maxCollectionTime)
            maxCollectionTime = collectionTime;

        ++numCollections;

        updateCollectThresholds();
        if (doFork && isFinal)
            return fullcollect(true, false);
        return freedPages;
    }

    /**
     * Clear the block cache data if it exists, given the data which is the
     * block info cache.
     *
     * Warning! This should only be called while the world is stopped inside
     * the fullcollect function after all live objects have been marked, but
     * before sweeping.
     */
    void *clearBlkCacheData(void* data) scope nothrow
    {
        processGCMarks(data, &isMarked);
        return data;
    }

    /**
     * Returns true if the addr lies within a marked block.
     *
     * Warning! This should only be called while the world is stopped inside
     * the fullcollect function after all live objects have been marked, but before sweeping.
     */
    IsMarked isMarked(void *addr) scope nothrow
    {
        // first, we find the Pool this block is in, then check to see if the
        // mark bit is clear.
        if (auto pool = findPool(addr))
        {
            auto offset = cast(size_t)(addr - pool.baseAddr);
            auto pn = offset / PAGESIZE;
            auto bins = cast(Bins)pool.pagetable[pn];
            size_t biti = void;
            if (bins < Bins.B_PAGE)
            {
                biti = baseOffset(offset, bins) >> pool.ShiftBy.Small;
                // doesn't need to check freebits because no pointer must exist
                //  to a block that was free before starting the collection
            }
            else if (bins == Bins.B_PAGE)
            {
                biti = pn * (PAGESIZE >> pool.ShiftBy.Large);
            }
            else if (bins == Bins.B_PAGEPLUS)
            {
                pn -= pool.bPageOffsets[pn];
                biti = pn * (PAGESIZE >> pool.ShiftBy.Large);
            }
            else // bins == Bins.B_FREE
            {
                assert(bins == Bins.B_FREE);
                return IsMarked.no;
            }
            return pool.mark.test(biti) ? IsMarked.yes : IsMarked.no;
        }
        return IsMarked.unknown;
    }

    version (Posix)
    {
        // A fork might happen while GC code is running in a different thread.
        // Because that would leave the GC in an inconsistent state,
        // make sure no GC code is running by acquiring the lock here,
        // before a fork.
        // This must not happen if fork is called from the GC with the lock already held

        __gshared bool fork_needs_lock = true; // racing condition with cocurrent calls of fork?


        extern(C) static void _d_gcx_atfork_prepare()
        {
            if (instance && fork_needs_lock)
                ConservativeGC.lockNR();
        }

        extern(C) static void _d_gcx_atfork_parent()
        {
            if (instance && fork_needs_lock)
                ConservativeGC.gcLock.unlock();
        }

        extern(C) static void _d_gcx_atfork_child()
        {
            if (instance && fork_needs_lock)
            {
                ConservativeGC.gcLock.unlock();

                // make sure the threads and event handles are reinitialized in a fork
                version (COLLECT_PARALLEL)
                {
                    if (Gcx.instance.scanThreadData)
                    {
                        cstdlib.free(Gcx.instance.scanThreadData);
                        Gcx.instance.numScanThreads = 0;
                        Gcx.instance.scanThreadData = null;
                        Gcx.instance.busyThreads = 0;

                        memset(&Gcx.instance.evStart, 0, Gcx.instance.evStart.sizeof);
                        memset(&Gcx.instance.evDone, 0, Gcx.instance.evDone.sizeof);
                    }
                }
            }
        }
    }

    /* ============================ Parallel scanning =============================== */
    version (COLLECT_PARALLEL):

    import core.atomic;
    import core.cpuid;
    import core.sync.event;

    private: // disable invariants for background threads

    static struct ScanThreadData
    {
        ThreadID tid;
    }
    uint numScanThreads;
    ScanThreadData* scanThreadData;

    Event evStart;
    Event evDone;

    shared uint busyThreads;
    shared uint stoppedThreads;
    bool stopGC;

    void markParallel() nothrow
    {
        toscanRoots.clear();
        collectAllRoots();
        if (toscanRoots.empty)
            return;

        void** pbot = toscanRoots._p;
        void** ptop = toscanRoots._p + toscanRoots._length;

        debug(PARALLEL_PRINTF) printf("markParallel\n");

        size_t pointersPerThread = toscanRoots._length / (numScanThreads + 1);
        if (pointersPerThread > 0)
        {
            void pushRanges(bool precise)()
            {
                alias toscan = scanStack!precise;
                toscan.stackLock.lock();

                for (int idx = 0; idx < numScanThreads; idx++)
                {
                    toscan.push(ScanRange!precise(pbot, pbot + pointersPerThread));
                    pbot += pointersPerThread;
                }
                toscan.stackLock.unlock();
            }
            if (ConservativeGC.isPrecise)
                pushRanges!true();
            else
                pushRanges!false();
        }
        assert(pbot < ptop);

        busyThreads.atomicOp!"+="(1); // main thread is busy

        evStart.setIfInitialized();

        debug(PARALLEL_PRINTF) printf("mark %lld roots\n", cast(ulong)(ptop - pbot));

        if (ConservativeGC.isPrecise)
            mark!(true, true, true)(ScanRange!true(pbot, ptop, null));
        else
            mark!(false, true, true)(ScanRange!false(pbot, ptop));

        busyThreads.atomicOp!"-="(1);

        debug(PARALLEL_PRINTF) printf("waitForScanDone\n");
        pullFromScanStack();
        debug(PARALLEL_PRINTF) printf("waitForScanDone done\n");
    }

    int maxParallelThreads() nothrow
    {
        auto threads = threadsPerCPU();

        if (threads == 0)
        {
            // If the GC is called by module ctors no explicit
            // import dependency on the GC is generated. So the
            // GC module is not correctly inserted into the module
            // initialization chain. As it relies on core.cpuid being
            // initialized, force this here.
            try
            {
                foreach (m; ModuleInfo)
                    if (m.name == "core.cpuid")
                        if (auto ctor = m.ctor())
                        {
                            ctor();
                            threads = threadsPerCPU();
                            break;
                        }
            }
            catch (Exception)
            {
                assert(false, "unexpected exception iterating ModuleInfo");
            }
        }
        return threads;
    }


    void startScanThreads() nothrow
    {
        auto threads = maxParallelThreads();
        debug(PARALLEL_PRINTF) printf("startScanThreads: %d threads per CPU\n", threads);
        if (threads <= 1)
            return; // either core.cpuid not initialized or single core

        numScanThreads = threads >= config.parallel ? config.parallel : threads - 1;

        scanThreadData = cast(ScanThreadData*) cstdlib.calloc(numScanThreads, ScanThreadData.sizeof);
        if (!scanThreadData)
            onOutOfMemoryError();

        evStart.initialize(false, false);
        evDone.initialize(false, false);

        version (Posix)
        {
            import core.sys.posix.signal : pthread_sigmask, SIG_BLOCK, SIG_SETMASK, sigfillset, sigset_t;
            // block all signals, scanBackground inherits this mask.
            // see https://issues.dlang.org/show_bug.cgi?id=20256
            sigset_t new_mask, old_mask;
            sigfillset(&new_mask);
            auto sigmask_rc = pthread_sigmask(SIG_BLOCK, &new_mask, &old_mask);
            assert(sigmask_rc == 0, "failed to set up GC scan thread sigmask");
        }

        for (int idx = 0; idx < numScanThreads; idx++)
            scanThreadData[idx].tid = createLowLevelThread(&scanBackground, 0x4000, &stopScanThreads);

        version (Posix)
        {
            sigmask_rc = pthread_sigmask(SIG_SETMASK, &old_mask, null);
            assert(sigmask_rc == 0, "failed to set up GC scan thread sigmask");
        }
    }

    void stopScanThreads() nothrow
    {
        if (!numScanThreads)
            return;

        debug(PARALLEL_PRINTF) printf("stopScanThreads\n");
        int startedThreads = 0;
        for (int idx = 0; idx < numScanThreads; idx++)
            if (scanThreadData[idx].tid != scanThreadData[idx].tid.init)
                startedThreads++;

        version (Windows)
            alias allThreadsDead = thread_DLLProcessDetaching;
        else
            enum allThreadsDead = false;
        stopGC = true;
        while (atomicLoad(stoppedThreads) < startedThreads && !allThreadsDead)
        {
            evStart.setIfInitialized();
            evDone.wait(dur!"msecs"(1));
        }

        for (int idx = 0; idx < numScanThreads; idx++)
        {
            if (scanThreadData[idx].tid != scanThreadData[idx].tid.init)
            {
                joinLowLevelThread(scanThreadData[idx].tid);
                scanThreadData[idx].tid = scanThreadData[idx].tid.init;
            }
        }

        evDone.terminate();
        evStart.terminate();

        cstdlib.free(scanThreadData);
        // scanThreadData = null; // keep non-null to not start again after shutdown
        numScanThreads = 0;

        debug(PARALLEL_PRINTF) printf("stopScanThreads done\n");
    }

    void scanBackground() nothrow
    {
        while (!stopGC)
        {
            evStart.wait();
            pullFromScanStack();
            evDone.setIfInitialized();
        }
        stoppedThreads.atomicOp!"+="(1);
    }

    void pullFromScanStack() nothrow
    {
        if (ConservativeGC.isPrecise)
            pullFromScanStackImpl!true();
        else
            pullFromScanStackImpl!false();
    }

    void pullFromScanStackImpl(bool precise)() nothrow
    {
        if (atomicLoad(busyThreads) == 0)
            return;

        version (Posix) debug (PARALLEL_PRINTF)
        {
            import core.sys.posix.pthread : pthread_self, pthread_t;
            pthread_t threadId = pthread_self();
            printf("scanBackground thread %d start\n", threadId);
        }

        ScanRange!precise rng;
        alias toscan = scanStack!precise;

        while (atomicLoad(busyThreads) > 0)
        {
            if (toscan.empty)
            {
                evDone.wait(dur!"msecs"(1));
                continue;
            }

            busyThreads.atomicOp!"+="(1);
            if (toscan.popLocked(rng))
            {
                version (Posix) debug (PARALLEL_PRINTF)
                {
                    printf("scanBackground thread %d scanning range [%p,%lld] from stack\n",
                        threadId, rng.pbot, cast(long) (rng.ptop - rng.pbot));
                }
                mark!(precise, true, true)(rng);
            }
            busyThreads.atomicOp!"-="(1);
        }
        version (Posix) debug (PARALLEL_PRINTF) printf("scanBackground thread %d done\n", threadId);
    }
}

/* ============================ Pool  =============================== */

struct Pool
{
    void* baseAddr;
    void* topAddr;
    size_t ptIndex;     // index in pool table
    GCBits mark;        // entries already scanned, or should not be scanned
    GCBits freebits;    // entries that are on the free list (all bits set but for allocated objects at their base offset)
    GCBits finals;      // entries that need finalizer run on them
    GCBits structFinals;// struct entries that need a finalzier run on them
    GCBits noscan;      // entries that should not be scanned
    GCBits appendable;  // entries that are appendable
    GCBits nointerior;  // interior pointers should be ignored.
                        // Only implemented for large object pools.
    GCBits is_pointer;  // precise GC only: per-word, not per-block like the rest of them (SmallObjectPool only)
    size_t npages;
    size_t freepages;     // The number of pages not in use.
    Bins* pagetable;

    bool isLargeObject;

    enum ShiftBy
    {
        Small = 4,
        Large = 12
    }
    ShiftBy shiftBy = void;    // shift count for the divisor used for determining bit indices.

    // This tracks how far back we have to go to find the nearest B_PAGE at
    // a smaller address than a B_PAGEPLUS.  To save space, we use a uint.
    // This limits individual allocations to 16 terabytes, assuming a 4k
    // pagesize. (LargeObjectPool only)
    // For B_PAGE and B_FREE, this specifies the number of pages in this block.
    // As an optimization, a contiguous range of free pages tracks this information
    //  only for the first and the last page.
    uint* bPageOffsets;

    // The small object pool uses the same array to keep a chain of
    // - pages with the same bin size that are still to be recovered
    // - free pages (searchStart is first free page)
    // other pages are marked by value PageRecovered
    alias binPageChain = bPageOffsets;

    enum PageRecovered = uint.max;

    // first of chain of pages to recover (SmallObjectPool only)
    uint[Bins.B_NUMSMALL] recoverPageFirst;

    // precise GC: TypeInfo.rtInfo for allocation (LargeObjectPool only)
    immutable(size_t)** rtinfo;

    // This variable tracks a conservative estimate of where the first free
    // page in this pool is, so that if a lot of pages towards the beginning
    // are occupied, we can bypass them in O(1).
    size_t searchStart;
    size_t largestFree; // upper limit for largest free chunk in large object pool

    void initialize(size_t npages, bool isLargeObject) nothrow
    {
        assert(npages >= 256);

        this.isLargeObject = isLargeObject;
        size_t poolsize;

        shiftBy = isLargeObject ? ShiftBy.Large : ShiftBy.Small;

        //debug(PRINTF) printf("Pool::Pool(%u)\n", npages);
        poolsize = npages * PAGESIZE;
        baseAddr = cast(byte *)os_mem_map(poolsize);
        version (VALGRIND) makeMemNoAccess(baseAddr[0..poolsize]);

        // Some of the code depends on page alignment of memory pools
        assert((cast(size_t)baseAddr & (PAGESIZE - 1)) == 0);

        if (!baseAddr)
        {
            //debug(PRINTF) printf("GC fail: poolsize = x%zx, errno = %d\n", poolsize, errno);
            //debug(PRINTF) printf("message = '%s'\n", sys_errlist[errno]);

            npages = 0;
            poolsize = 0;
        }
        //assert(baseAddr);
        topAddr = baseAddr + poolsize;
        auto nbits = cast(size_t)poolsize >> shiftBy;

        version (COLLECT_FORK)
            mark.alloc(nbits, config.fork);
        else
            mark.alloc(nbits);
        if (ConservativeGC.isPrecise)
        {
            if (isLargeObject)
            {
                rtinfo = cast(immutable(size_t)**)cstdlib.malloc(npages * (size_t*).sizeof);
                if (!rtinfo)
                    onOutOfMemoryError();
                memset(rtinfo, 0, npages * (size_t*).sizeof);
            }
            else
            {
                is_pointer.alloc(cast(size_t)poolsize/(void*).sizeof);
                is_pointer.clrRange(0, is_pointer.nbits);
            }
        }

        // pagetable already keeps track of what's free for the large object
        // pool.
        if (!isLargeObject)
        {
            freebits.alloc(nbits);
            freebits.setRange(0, nbits);
        }

        noscan.alloc(nbits);
        appendable.alloc(nbits);

        pagetable = cast(Bins*)cstdlib.malloc(npages * Bins.sizeof);
        if (!pagetable)
            onOutOfMemoryError();

        if (npages > 0)
        {
            bPageOffsets = cast(uint*)cstdlib.malloc(npages * uint.sizeof);
            if (!bPageOffsets)
                onOutOfMemoryError();

            if (isLargeObject)
            {
                bPageOffsets[0] = cast(uint)npages;
                bPageOffsets[npages-1] = cast(uint)npages;
            }
            else
            {
                // all pages free
                foreach (n; 0..npages)
                    binPageChain[n] = cast(uint)(n + 1);
                recoverPageFirst[] = cast(uint)npages;
            }
        }

        memset(pagetable, Bins.B_FREE, npages);

        this.npages = npages;
        this.freepages = npages;
        this.searchStart = 0;
        this.largestFree = npages;
    }


    void Dtor() nothrow
    {
        if (baseAddr)
        {
            int result;

            if (npages)
            {
                result = os_mem_unmap(baseAddr, npages * PAGESIZE);
                assert(result == 0);
                npages = 0;
            }

            baseAddr = null;
            topAddr = null;
        }
        if (pagetable)
        {
            cstdlib.free(pagetable);
            pagetable = null;
        }

        if (bPageOffsets)
        {
            cstdlib.free(bPageOffsets);
            bPageOffsets = null;
        }

        mark.Dtor(config.fork);
        if (ConservativeGC.isPrecise)
        {
            if (isLargeObject)
                cstdlib.free(rtinfo);
            else
                is_pointer.Dtor();
        }
        if (isLargeObject)
        {
            nointerior.Dtor();
        }
        else
        {
            freebits.Dtor();
        }
        finals.Dtor();
        structFinals.Dtor();
        noscan.Dtor();
        appendable.Dtor();
    }

    /**
    *
    */
    uint getBits(size_t biti) nothrow
    {
        uint bits;

        if (finals.nbits && finals.test(biti))
            bits |= BlkAttr.FINALIZE;
        if (structFinals.nbits && structFinals.test(biti))
            bits |= BlkAttr.STRUCTFINAL;
        if (noscan.test(biti))
            bits |= BlkAttr.NO_SCAN;
        if (nointerior.nbits && nointerior.test(biti))
            bits |= BlkAttr.NO_INTERIOR;
        if (appendable.test(biti))
            bits |= BlkAttr.APPENDABLE;
        return bits;
    }

    /**
     *
     */
    void clrBits(size_t biti, uint mask) nothrow @nogc
    {
        immutable dataIndex =  biti >> GCBits.BITS_SHIFT;
        immutable bitOffset = biti & GCBits.BITS_MASK;
        immutable keep = ~(GCBits.BITS_1 << bitOffset);

        if (mask & BlkAttr.FINALIZE && finals.nbits)
            finals.data[dataIndex] &= keep;

        if (structFinals.nbits && (mask & BlkAttr.STRUCTFINAL))
            structFinals.data[dataIndex] &= keep;

        if (mask & BlkAttr.NO_SCAN)
            noscan.data[dataIndex] &= keep;
        if (mask & BlkAttr.APPENDABLE)
            appendable.data[dataIndex] &= keep;
        if (nointerior.nbits && (mask & BlkAttr.NO_INTERIOR))
            nointerior.data[dataIndex] &= keep;
    }

    /**
     *
     */
    void setBits(size_t biti, uint mask) nothrow
    {
        // Calculate the mask and bit offset once and then use it to
        // set all of the bits we need to set.
        immutable dataIndex = biti >> GCBits.BITS_SHIFT;
        immutable bitOffset = biti & GCBits.BITS_MASK;
        immutable orWith = GCBits.BITS_1 << bitOffset;

        if (mask & BlkAttr.STRUCTFINAL)
        {
            if (!structFinals.nbits)
                structFinals.alloc(mark.nbits);
            structFinals.data[dataIndex] |= orWith;
        }

        if (mask & BlkAttr.FINALIZE)
        {
            if (!finals.nbits)
                finals.alloc(mark.nbits);
            finals.data[dataIndex] |= orWith;
        }

        if (mask & BlkAttr.NO_SCAN)
        {
            noscan.data[dataIndex] |= orWith;
        }
//        if (mask & BlkAttr.NO_MOVE)
//        {
//            if (!nomove.nbits)
//                nomove.alloc(mark.nbits);
//            nomove.data[dataIndex] |= orWith;
//        }
        if (mask & BlkAttr.APPENDABLE)
        {
            appendable.data[dataIndex] |= orWith;
        }

        if (isLargeObject && (mask & BlkAttr.NO_INTERIOR))
        {
            if (!nointerior.nbits)
                nointerior.alloc(mark.nbits);
            nointerior.data[dataIndex] |= orWith;
        }
    }

    void freePageBits(size_t pagenum, const scope ref PageBits toFree) nothrow
    {
        assert(!isLargeObject);
        assert(!nointerior.nbits); // only for large objects

        import core.internal.traits : staticIota;
        immutable beg = pagenum * (PAGESIZE / 16 / GCBits.BITS_PER_WORD);
        foreach (i; staticIota!(0, PageBits.length))
        {
            immutable w = toFree[i];
            if (!w) continue;

            immutable wi = beg + i;
            freebits.data[wi] |= w;
            noscan.data[wi] &= ~w;
            appendable.data[wi] &= ~w;
        }

        if (finals.nbits)
        {
            foreach (i; staticIota!(0, PageBits.length))
                if (toFree[i])
                    finals.data[beg + i] &= ~toFree[i];
        }

        if (structFinals.nbits)
        {
            foreach (i; staticIota!(0, PageBits.length))
                if (toFree[i])
                    structFinals.data[beg + i] &= ~toFree[i];
        }
    }

    void freeAllPageBits(size_t pagenum) nothrow
    {
        assert(!isLargeObject);
        assert(!nointerior.nbits); // only for large objects

        immutable beg = pagenum * PageBits.length;
        static foreach (i; 0 .. PageBits.length)
        {{
            immutable w = beg + i;
            freebits.data[w] = ~0;
            noscan.data[w] = 0;
            appendable.data[w] = 0;
            if (finals.data)
                finals.data[w] = 0;
            if (structFinals.data)
                structFinals.data[w] = 0;
        }}
    }

    /**
     * Given a pointer p in the p, return the pagenum.
     */
    size_t pagenumOf(void *p) const nothrow @nogc
    in
    {
        assert(p >= baseAddr);
        assert(p < topAddr);
    }
    do
    {
        return cast(size_t)(p - baseAddr) / PAGESIZE;
    }

    public
    @property bool isFree() const scope @safe pure nothrow @nogc
    {
        return npages == freepages;
    }

    /**
     * Return number of pages necessary for an allocation of the given size
     *
     * returns size_t.max if more than uint.max pages are requested
     * (return type is still size_t to avoid truncation when being used
     *  in calculations, e.g. npages * PAGESIZE)
     */
    static size_t numPages(size_t size) nothrow @nogc
    {
        version (D_LP64)
        {
            if (size > PAGESIZE * cast(size_t)uint.max)
                return size_t.max;
        }
        else
        {
            if (size > size_t.max - PAGESIZE)
                return size_t.max;
        }
        return (size + PAGESIZE - 1) / PAGESIZE;
    }

    void* findBase(void* p) nothrow @nogc
    {
        size_t offset = cast(size_t)(p - baseAddr);
        size_t pn = offset / PAGESIZE;
        Bins   bin = pagetable[pn];

        // Adjust bit to be at start of allocated memory block
        if (bin < Bins.B_NUMSMALL)
        {
            auto baseOff = baseOffset(offset, bin);
            const biti = baseOff >> Pool.ShiftBy.Small;
            if (freebits.test (biti))
                return null;
            return baseAddr + baseOff;
        }
        if (bin == Bins.B_PAGE)
        {
            return baseAddr + (offset & (offset.max ^ (PAGESIZE-1)));
        }
        if (bin == Bins.B_PAGEPLUS)
        {
            size_t pageOffset = bPageOffsets[pn];
            offset -= pageOffset * PAGESIZE;
            pn -= pageOffset;

            return baseAddr + (offset & (offset.max ^ (PAGESIZE-1)));
        }
        // we are in a B_FREE page
        assert(bin == Bins.B_FREE);
        return null;
    }

    size_t slGetSize(void* p) nothrow @nogc
    {
        if (isLargeObject)
            return (cast(LargeObjectPool*)&this).getPages(p) * PAGESIZE;
        else
            return (cast(SmallObjectPool*)&this).getSize(p);
    }

    BlkInfo slGetInfo(void* p) nothrow
    {
        if (isLargeObject)
            return (cast(LargeObjectPool*)&this).getInfo(p);
        else
            return (cast(SmallObjectPool*)&this).getInfo(p);
    }


    void Invariant() const {}

    debug(INVARIANT)
    invariant
    {
        if (baseAddr)
        {
            //if (baseAddr + npages * PAGESIZE != topAddr)
                //printf("baseAddr = %p, npages = %d, topAddr = %p\n", baseAddr, npages, topAddr);
            assert(baseAddr + npages * PAGESIZE == topAddr);
        }

        if (pagetable !is null)
        {
            for (size_t i = 0; i < npages; i++)
            {
                Bins bin = pagetable[i];
                assert(bin < Bins.B_MAX);
            }
        }
    }

    void setPointerBitmapSmall(void* p, size_t s, size_t allocSize, uint attr, const TypeInfo ti) nothrow
    {
        if (!(attr & BlkAttr.NO_SCAN))
            setPointerBitmap(p, s, allocSize, ti, attr);
    }

    pragma(inline,false)
    void setPointerBitmap(void* p, size_t s, size_t allocSize, const TypeInfo ti, uint attr) nothrow
    {
        size_t offset = p - baseAddr;
        //debug(PRINTF) printGCBits(&pool.is_pointer);

        debug(PRINTF)
            printf("Setting a pointer bitmap for %s at %p + %llu\n", debugTypeName(ti).ptr, p, cast(ulong)s);

        if (ti)
        {
            if (attr & BlkAttr.APPENDABLE)
            {
                // an array of classes is in fact an array of pointers
                if (typeid(ti) is typeid(TypeInfo_Class))
                    goto L_conservative;
                s = allocSize;
            }

            auto rtInfo = cast(const(size_t)*)ti.rtInfo();

            if (rtInfo is rtinfoNoPointers)
            {
                debug(PRINTF) printf("\tCompiler generated rtInfo: no pointers\n");
                is_pointer.clrRange(offset/(void*).sizeof, s/(void*).sizeof);
            }
            else if (rtInfo is rtinfoHasPointers)
            {
                debug(PRINTF) printf("\tCompiler generated rtInfo: has pointers\n");
                is_pointer.setRange(offset/(void*).sizeof, s/(void*).sizeof);
            }
            else
            {
                const(size_t)* bitmap = cast (size_t*) rtInfo;
                //first element of rtInfo is the size of the object the bitmap encodes
                size_t element_size = * bitmap;
                bitmap++;
                size_t tocopy;
                if (attr & BlkAttr.APPENDABLE)
                {
                    tocopy = s/(void*).sizeof;
                    is_pointer.copyRangeRepeating(offset/(void*).sizeof, tocopy, bitmap, element_size/(void*).sizeof);
                }
                else
                {
                    tocopy = (s < element_size ? s : element_size)/(void*).sizeof;
                    is_pointer.copyRange(offset/(void*).sizeof, tocopy, bitmap);
                }

                debug(PRINTF) printf("\tSetting bitmap for new object (%s)\n\t\tat %p\t\tcopying from %p + %llu: ",
                                     debugTypeName(ti).ptr, p, bitmap, cast(ulong)element_size);
                debug(PRINTF)
                    for (size_t i = 0; i < element_size/((void*).sizeof); i++)
                        printf("%zd", (bitmap[i/(8*size_t.sizeof)] >> (i%(8*size_t.sizeof))) & 1);
                debug(PRINTF) printf("\n");

                if (tocopy * (void*).sizeof < s) // better safe than sorry: if allocated more, assume pointers inside
                {
                    debug(PRINTF) printf("    Appending %zd pointer bits\n", s/(void*).sizeof - tocopy);
                    is_pointer.setRange(offset/(void*).sizeof + tocopy, s/(void*).sizeof - tocopy);
                }
            }

            if (s < allocSize)
            {
                offset = (offset + s + (void*).sizeof - 1) & ~((void*).sizeof - 1);
                is_pointer.clrRange(offset/(void*).sizeof, (allocSize - s)/(void*).sizeof);
            }
        }
        else
        {
        L_conservative:
            // limit pointers to actual size of allocation? might fail for arrays that append
            // without notifying the GC
            s = allocSize;

            debug(PRINTF) printf("Allocating a block without TypeInfo\n");
            is_pointer.setRange(offset/(void*).sizeof, s/(void*).sizeof);
        }
        //debug(PRINTF) printGCBits(&pool.is_pointer);
    }
}

struct LargeObjectPool
{
    Pool base;
    alias base this;

    debug(INVARIANT)
    void Invariant()
    {
        //base.Invariant();
        for (size_t n = 0; n < npages; )
        {
            uint np = bPageOffsets[n];
            assert(np > 0 && np <= npages - n);

            if (pagetable[n] == Bins.B_PAGE)
            {
                for (uint p = 1; p < np; p++)
                {
                    assert(pagetable[n + p] == Bins.B_PAGEPLUS);
                    assert(bPageOffsets[n + p] == p);
                }
            }
            else if (pagetable[n] == Bins.B_FREE)
            {
                for (uint p = 1; p < np; p++)
                {
                    assert(pagetable[n + p] == Bins.B_FREE);
                }
                assert(bPageOffsets[n + np - 1] == np);
            }
            else
                assert(false);
            n += np;
        }
    }

    /**
     * Allocate n pages from Pool.
     * Returns OPFAIL on failure.
     */
    size_t allocPages(size_t n) nothrow
    {
        if (largestFree < n || searchStart + n > npages)
            return OPFAIL;

        //debug(PRINTF) printf("Pool::allocPages(n = %d)\n", n);
        size_t largest = 0;
        if (pagetable[searchStart] == Bins.B_PAGEPLUS)
        {
            searchStart -= bPageOffsets[searchStart]; // jump to B_PAGE
            searchStart += bPageOffsets[searchStart];
        }
        while (searchStart < npages && pagetable[searchStart] == Bins.B_PAGE)
            searchStart += bPageOffsets[searchStart];

        for (size_t i = searchStart; i < npages; )
        {
            assert(pagetable[i] == Bins.B_FREE);

            auto p = bPageOffsets[i];
            if (p > n)
            {
                setFreePageOffsets(i + n, p - n);
                goto L_found;
            }
            if (p == n)
            {
            L_found:
                pagetable[i] = Bins.B_PAGE;
                bPageOffsets[i] = cast(uint) n;
                if (n > 1)
                {
                    memset(&pagetable[i + 1], Bins.B_PAGEPLUS, n - 1);
                    for (auto offset = 1; offset < n; offset++)
                        bPageOffsets[i + offset] = cast(uint) offset;
                }
                freepages -= n;
                return i;
            }
            if (p > largest)
                largest = p;

            i += p;
            while (i < npages && pagetable[i] == Bins.B_PAGE)
            {
                // we have the size information, so we skip a whole bunch of pages.
                i += bPageOffsets[i];
            }
        }

        // not enough free pages found, remember largest free chunk
        largestFree = largest;
        return OPFAIL;
    }

    /**
     * Free npages pages starting with pagenum.
     */
    void freePages(size_t pagenum, size_t npages) nothrow @nogc
    {
        //memset(&pagetable[pagenum], B_FREE, npages);
        if (pagenum < searchStart)
            searchStart = pagenum;

        for (size_t i = pagenum; i < npages + pagenum; i++)
        {
            assert(pagetable[i] < Bins.B_FREE);
            pagetable[i] = Bins.B_FREE;
        }
        freepages += npages;
        largestFree = freepages; // invalidate
    }

    /**
     * Set the first and the last entry of a B_FREE block to the size
     */
    void setFreePageOffsets(size_t page, size_t num) nothrow @nogc
    {
        assert(pagetable[page] == Bins.B_FREE);
        assert(pagetable[page + num - 1] == Bins.B_FREE);
        bPageOffsets[page] = cast(uint)num;
        if (num > 1)
            bPageOffsets[page + num - 1] = cast(uint)num;
    }

    void mergeFreePageOffsets(bool bwd, bool fwd)(size_t page, size_t num) nothrow @nogc
    {
        static if (bwd)
        {
            if (page > 0 && pagetable[page - 1] == Bins.B_FREE)
            {
                auto sz = bPageOffsets[page - 1];
                page -= sz;
                num += sz;
            }
        }
        static if (fwd)
        {
            if (page + num < npages && pagetable[page + num] == Bins.B_FREE)
                num += bPageOffsets[page + num];
        }
        setFreePageOffsets(page, num);
    }

    /**
     * Get pages of allocation at pointer p in pool.
     */
    size_t getPages(void *p) const nothrow @nogc
    in
    {
        assert(p >= baseAddr);
        assert(p < topAddr);
    }
    do
    {
        if (cast(size_t)p & (PAGESIZE - 1)) // check for interior pointer
            return 0;
        size_t pagenum = pagenumOf(p);
        Bins bin = pagetable[pagenum];
        if (bin != Bins.B_PAGE)
            return 0;
        return bPageOffsets[pagenum];
    }

    /**
    * Get size of allocation at page pn in pool.
    */
    size_t getSize(size_t pn) const nothrow @nogc
    {
        assert(pagetable[pn] == Bins.B_PAGE);
        return cast(size_t) bPageOffsets[pn] * PAGESIZE;
    }

    /**
    *
    */
    BlkInfo getInfo(void* p) nothrow
    {
        BlkInfo info;

        size_t offset = cast(size_t)(p - baseAddr);
        size_t pn = offset / PAGESIZE;
        Bins bin = pagetable[pn];

        if (bin == Bins.B_PAGEPLUS)
            pn -= bPageOffsets[pn];
        else if (bin != Bins.B_PAGE)
            return info;           // no info for free pages

        info.base = baseAddr + pn * PAGESIZE;
        info.size = getSize(pn);
        info.attr = getBits(pn);
        return info;
    }

    void runFinalizers(const scope void[] segment) nothrow
    {
        foreach (pn; 0 .. npages)
        {
            Bins bin = pagetable[pn];
            if (bin > Bins.B_PAGE)
                continue;
            size_t biti = pn;

            if (!finals.test(biti))
                continue;

            auto p = sentinel_add(baseAddr + pn * PAGESIZE);
            size_t size = sentinel_size(p, getSize(pn));
            uint attr = getBits(biti);

            if (!rt_hasFinalizerInSegment(p, size, attr, segment))
                continue;

            rt_finalizeFromGC(p, size, attr);

            clrBits(biti, ~BlkAttr.NONE);

            if (pn < searchStart)
                searchStart = pn;

            debug(COLLECT_PRINTF) printf("\tcollecting big %p\n", p);
            //log_free(sentinel_add(p));

            size_t n = 1;
            for (; pn + n < npages; ++n)
                if (pagetable[pn + n] != Bins.B_PAGEPLUS)
                    break;
            invalidate((baseAddr + pn * PAGESIZE)[0 .. n * PAGESIZE], 0xF3, false);
            freePages(pn, n);
            mergeFreePageOffsets!(true, true)(pn, n);
        }
    }
}


struct SmallObjectPool
{
    Pool base;
    alias base this;

    debug(INVARIANT)
    void Invariant()
    {
        //base.Invariant();
        uint cntRecover = 0;
        foreach (Bins bin; Bins.B_16 .. Bins.B_NUMSMALL)
        {
            for (auto pn = recoverPageFirst[bin]; pn < npages; pn = binPageChain[pn])
            {
                assert(pagetable[pn] == bin);
                cntRecover++;
            }
        }
        uint cntFree = 0;
        for (auto pn = searchStart; pn < npages; pn = binPageChain[pn])
        {
            assert(pagetable[pn] == Bins.B_FREE);
            cntFree++;
        }
        assert(cntFree == freepages);
        assert(cntFree + cntRecover <= npages);
    }

    /**
    * Get size of pointer p in pool.
    */
    size_t getSize(void *p) const nothrow @nogc
    in
    {
        assert(p >= baseAddr);
        assert(p < topAddr);
    }
    do
    {
        size_t pagenum = pagenumOf(p);
        Bins bin = pagetable[pagenum];
        assert(bin < Bins.B_PAGE);
        if (p != cast(void*)baseOffset(cast(size_t)p, bin)) // check for interior pointer
            return 0;
        const biti = cast(size_t)(p - baseAddr) >> ShiftBy.Small;
        if (freebits.test (biti))
            return 0;
        return binsize[bin];
    }

    BlkInfo getInfo(void* p) nothrow
    {
        BlkInfo info;
        size_t offset = cast(size_t)(p - baseAddr);
        size_t pn = offset / PAGESIZE;
        Bins   bin = pagetable[pn];

        if (bin >= Bins.B_PAGE)
            return info;

        auto base = cast(void*)baseOffset(cast(size_t)p, bin);
        const biti = cast(size_t)(base - baseAddr) >> ShiftBy.Small;
        if (freebits.test (biti))
            return info;

        info.base = base;
        info.size = binsize[bin];
        offset = info.base - baseAddr;
        info.attr = getBits(biti);

        return info;
    }

    void runFinalizers(const scope void[] segment) nothrow
    {
        foreach (pn; 0 .. npages)
        {
            Bins bin = pagetable[pn];
            if (bin >= Bins.B_PAGE)
                continue;

            immutable size = binsize[bin];
            auto p = baseAddr + pn * PAGESIZE;
            const ptop = p + PAGESIZE - size + 1;
            immutable base = pn * (PAGESIZE/16);
            immutable bitstride = size / 16;

            bool freeBits;
            PageBits toFree;

            for (size_t i; p < ptop; p += size, i += bitstride)
            {
                immutable biti = base + i;

                if (!finals.test(biti))
                    continue;

                auto q = sentinel_add(p);
                uint attr = getBits(biti);
                const ssize = sentinel_size(q, size);
                if (!rt_hasFinalizerInSegment(q, ssize, attr, segment))
                    continue;

                rt_finalizeFromGC(q, ssize, attr);

                freeBits = true;
                toFree.set(i);

                debug(COLLECT_PRINTF) printf("\tcollecting %p\n", p);
                //log_free(sentinel_add(p));

                invalidate(p[0 .. size], 0xF3, false);
            }

            if (freeBits)
                freePageBits(pn, toFree);
        }
    }

    /**
    * Allocate a page of bin's.
    * Returns:
    *           head of a single linked list of new entries
    */
    List* allocPage(Bins bin) nothrow
    {
        if (searchStart >= npages)
            return null;

        assert(pagetable[searchStart] == Bins.B_FREE);

    L1:
        size_t pn = searchStart;
        searchStart = binPageChain[searchStart];
        binPageChain[pn] = Pool.PageRecovered;
        pagetable[pn] = bin;
        freepages--;

        // Convert page to free list
        size_t size = binsize[bin];
        void* p = baseAddr + pn * PAGESIZE;
        auto first = cast(List*) p;

        // ensure 2 <size> bytes blocks are available below ptop, one
        //  being set in the loop, and one for the tail block
        void* ptop = p + PAGESIZE - 2 * size + 1;
        for (; p < ptop; p += size)
        {
            undefinedWrite((cast(List *)p).next, cast(List *)(p + size));
            undefinedWrite((cast(List *)p).pool, &base);
        }
        undefinedWrite((cast(List *)p).next, null);
        undefinedWrite((cast(List *)p).pool, &base);
        return first;
    }
}

debug(SENTINEL) {} else // no additional capacity with SENTINEL
unittest // https://issues.dlang.org/show_bug.cgi?id=14467
{
    int[] arr = new int[10];
    assert(arr.capacity);
    arr = arr[$..$];
    assert(arr.capacity);
}

unittest // https://issues.dlang.org/show_bug.cgi?id=15353
{
    import core.memory : GC;

    static struct Foo
    {
        ~this()
        {
            GC.free(buf); // ignored in finalizer
        }

        void* buf;
    }
    new Foo(GC.malloc(10));
    GC.collect();
}

unittest // https://issues.dlang.org/show_bug.cgi?id=15822
{
    import core.memory : GC;

    __gshared ubyte[16] buf;
    static struct Foo
    {
        ~this()
        {
            GC.removeRange(ptr);
            GC.removeRoot(ptr);
        }

        ubyte* ptr;
    }
    GC.addRoot(buf.ptr);
    GC.addRange(buf.ptr, buf.length);
    new Foo(buf.ptr);
    GC.collect();
}

unittest // https://issues.dlang.org/show_bug.cgi?id=1180
{
    import core.exception;
    try
    {
        size_t x = size_t.max - 100;
        byte[] big_buf = new byte[x];
    }
    catch (OutOfMemoryError)
    {
    }
}

/* ============================ PRINTF =============================== */

debug(PRINTF_TO_FILE)
{
    private __gshared MonoTime gcStartTick;
    private __gshared FILE* gcx_fh;
    private __gshared bool hadNewline = false;
    import core.internal.spinlock;
    static printLock = shared(AlignedSpinLock)(SpinLock.Contention.lengthy);

    private int printf(ARGS...)(const char* fmt, ARGS args) nothrow
    {
        printLock.lock();
        scope(exit) printLock.unlock();

        if (!gcx_fh)
            gcx_fh = fopen("gcx.log", "w");
        if (!gcx_fh)
            return 0;

        int len;
        if (MonoTime.ticksPerSecond == 0)
        {
            len = fprintf(gcx_fh, "before init: ");
        }
        else if (hadNewline)
        {
            if (gcStartTick == MonoTime.init)
                gcStartTick = MonoTime.currTime;
            immutable timeElapsed = MonoTime.currTime - gcStartTick;
            immutable secondsAsDouble = timeElapsed.total!"hnsecs" / cast(double)convert!("seconds", "hnsecs")(1);
            len = fprintf(gcx_fh, "%10.6f: ", secondsAsDouble);
        }
        len += fprintf(gcx_fh, fmt, args);
        fflush(gcx_fh);
        import core.stdc.string;
        hadNewline = fmt && fmt[0] && fmt[strlen(fmt) - 1] == '\n';
        return len;
    }
}

debug(PRINTF) void printFreeInfo(Pool* pool) nothrow
{
    uint nReallyFree;
    foreach (i; 0..pool.npages) {
        if (pool.pagetable[i] >= Bins.B_FREE) nReallyFree++;
    }

    printf("Pool %p:  %d really free, %zd supposedly free\n", pool, nReallyFree, pool.freepages);
}

debug(PRINTF)
void printGCBits(GCBits* bits)
{
    for (size_t i = 0; i < bits.nwords; i++)
    {
        if (i % 32 == 0) printf("\n\t");
        printf("%zx ", bits.data[i]);
    }
    printf("\n");
}

// we can assume the name is always from a literal, so it is zero terminated
debug(PRINTF)
string debugTypeName(const(TypeInfo) ti) nothrow
{
    string name;
    if (ti is null)
        name = "null";
    else if (auto ci = cast(TypeInfo_Class)ti)
        name = ci.name;
    else if (auto si = cast(TypeInfo_Struct)ti)
        name = si.mangledName; // .name() might GC-allocate, avoid deadlock
    else if (auto ci = cast(TypeInfo_Const)ti)
        static if (__traits(compiles,ci.base)) // different whether compiled with object.di or object.d
            return debugTypeName(ci.base);
        else
            return debugTypeName(ci.next);
    else
        name = typeid(ti).name;
    return name;
}

/* ======================= Leak Detector =========================== */

debug (LOGGING)
{
    struct Log
    {
        void*  p;
        size_t size;
        size_t line;
        char*  file;
        void*  parent;

        void print() nothrow
        {
            printf("    p = %p, size = %lld, parent = %p ", p, cast(ulong)size, parent);
            if (file)
            {
                printf("%s(%u)", file, cast(uint)line);
            }
            printf("\n");
        }
    }


    struct LogArray
    {
        size_t dim;
        size_t allocdim;
        Log *data;

        void Dtor() nothrow @nogc
        {
            if (data)
                cstdlib.free(data);
            data = null;
        }

        void reserve(size_t nentries) nothrow @nogc
        {
            assert(dim <= allocdim);
            if (allocdim - dim < nentries)
            {
                allocdim = (dim + nentries) * 2;
                assert(dim + nentries <= allocdim);
                if (!data)
                {
                    data = cast(Log*)cstdlib.malloc(allocdim * Log.sizeof);
                    if (!data && allocdim)
                        onOutOfMemoryError();
                }
                else
                {   Log *newdata;

                    newdata = cast(Log*)cstdlib.malloc(allocdim * Log.sizeof);
                    if (!newdata && allocdim)
                        onOutOfMemoryError();
                    memcpy(newdata, data, dim * Log.sizeof);
                    cstdlib.free(data);
                    data = newdata;
                }
            }
        }


        void push(Log log) nothrow @nogc
        {
            reserve(1);
            data[dim++] = log;
        }

        void remove(size_t i) nothrow @nogc
        {
            memmove(data + i, data + i + 1, (dim - i) * Log.sizeof);
            dim--;
        }


        size_t find(void *p) nothrow @nogc
        {
            for (size_t i = 0; i < dim; i++)
            {
                if (data[i].p == p)
                    return i;
            }
            return OPFAIL; // not found
        }


        void copy(LogArray *from) nothrow @nogc
        {
            if (allocdim < from.dim)
                reserve(from.dim - dim);
            assert(from.dim <= allocdim);
            memcpy(data, from.data, from.dim * Log.sizeof);
            dim = from.dim;
        }
    }

    struct LeakDetector
    {
        Gcx* gcx;
        LogArray current;
        LogArray prev;

        private void initialize(Gcx* gc)
        {
            gcx = gc;
            //debug(PRINTF) printf("+log_init()\n");
            current.reserve(1000);
            prev.reserve(1000);
            //debug(PRINTF) printf("-log_init()\n");
        }


        private void log_malloc(void *p, size_t size) nothrow
        {
            //debug(PRINTF) printf("+log_malloc(p = %p, size = %zd)\n", p, size);
            Log log;

            log.p = p;
            log.size = size;
            log.line = ConservativeGC.line;
            log.file = ConservativeGC.file;
            log.parent = null;

            ConservativeGC.line = 0;
            ConservativeGC.file = null;

            current.push(log);
            //debug(PRINTF) printf("-log_malloc()\n");
        }


        private void log_free(void *p, size_t size) nothrow @nogc
        {
            //debug(PRINTF) printf("+log_free(%p)\n", p);
            auto i = current.find(p);
            if (i == OPFAIL)
            {
                debug(PRINTF) printf("free'ing unallocated memory %p (size %zu)\n", p, size);
            }
            else
                current.remove(i);
            //debug(PRINTF) printf("-log_free()\n");
        }


        private void log_collect() nothrow
        {
            //debug(PRINTF) printf("+log_collect()\n");
            // Print everything in current that is not in prev

            debug(PRINTF) printf("New pointers this cycle: --------------------------------\n");
            size_t used = 0;
            for (size_t i = 0; i < current.dim; i++)
            {
                auto j = prev.find(current.data[i].p);
                if (j == OPFAIL)
                    current.data[i].print();
                else
                    used++;
            }

            debug(PRINTF) printf("All roots this cycle: --------------------------------\n");
            for (size_t i = 0; i < current.dim; i++)
            {
                void* p = current.data[i].p;
                if (!gcx.findPool(current.data[i].parent))
                {
                    auto j = prev.find(current.data[i].p);
                    debug(PRINTF) printf(j == OPFAIL ? "N" : " ");
                    current.data[i].print();
                }
            }

            debug(PRINTF) printf("Used = %d-------------------------------------------------\n", used);
            prev.copy(&current);

            debug(PRINTF) printf("-log_collect()\n");
        }


        private void log_parent(void *p, void *parent) nothrow
        {
            //debug(PRINTF) printf("+log_parent()\n");
            auto i = current.find(p);
            if (i == OPFAIL)
            {
                debug(PRINTF) printf("parent'ing unallocated memory %p, parent = %p\n", p, parent);
                Pool *pool;
                pool = gcx.findPool(p);
                assert(pool);
                size_t offset = cast(size_t)(p - pool.baseAddr);
                size_t biti;
                size_t pn = offset / PAGESIZE;
                Bins bin = pool.pagetable[pn];
                biti = (offset & (PAGESIZE - 1)) >> pool.shiftBy;
                debug(PRINTF) printf("\tbin = %d, offset = x%x, biti = x%x\n", bin, offset, biti);
            }
            else
            {
                current.data[i].parent = parent;
            }
            //debug(PRINTF) printf("-log_parent()\n");
        }
    }
}
else
{
    struct LeakDetector
    {
        static void initialize(Gcx* gcx) nothrow { }
        static void log_malloc(void *p, size_t size) nothrow { }
        static void log_free(void *p, size_t size) nothrow @nogc {}
        static void log_collect() nothrow { }
        static void log_parent(void *p, void *parent) nothrow { }
    }
}

/* ============================ SENTINEL =============================== */

debug (SENTINEL)
{
    // pre-sentinel must be smaller than 16 bytes so that the same GC bits
    //  are used for the allocated pointer and the user pointer
    // so use uint for both 32 and 64 bit platforms, limiting usage to < 4GB
    const uint  SENTINEL_PRE = 0xF4F4F4F4;
    const ubyte SENTINEL_POST = 0xF5;           // 8 bits
    const uint  SENTINEL_EXTRA = 2 * uint.sizeof + 1;


    inout(uint*)  sentinel_psize(inout void *p) nothrow @nogc { return &(cast(inout uint *)p)[-2]; }
    inout(uint*)  sentinel_pre(inout void *p)   nothrow @nogc { return &(cast(inout uint *)p)[-1]; }
    inout(ubyte*) sentinel_post(inout void *p)  nothrow @nogc { return &(cast(inout ubyte *)p)[*sentinel_psize(p)]; }


    void sentinel_init(void *p, size_t size) nothrow @nogc
    {
        assert(size <= uint.max);
        *sentinel_psize(p) = cast(uint)size;
        debug (VALGRIND)
        {
            makeMemNoAccess(sentinel_pre(p)[0..1]);
            makeMemNoAccess(sentinel_post(p)[0..1]);
        }
        else
        {
            *sentinel_pre(p) = SENTINEL_PRE;
            *sentinel_post(p) = SENTINEL_POST;
        }
    }


    void sentinel_Invariant(const void *p) nothrow @nogc
    {
        debug (VALGRIND) {} else
        debug
        {
            assert(*sentinel_pre(p) == SENTINEL_PRE);
            assert(*sentinel_post(p) == SENTINEL_POST);
        }
        else if (*sentinel_pre(p) != SENTINEL_PRE || *sentinel_post(p) != SENTINEL_POST)
            onInvalidMemoryOperationError(); // also trigger in release build
    }

    size_t sentinel_size(const void *p, size_t alloc_size) nothrow @nogc
    {
        return *sentinel_psize(p);
    }

    void *sentinel_add(void *p) nothrow @nogc
    {
        return p + 2 * uint.sizeof;
    }


    void *sentinel_sub(void *p) nothrow @nogc
    {
        return p - 2 * uint.sizeof;
    }
}
else
{
    const uint SENTINEL_EXTRA = 0;


    void sentinel_init(void *p, size_t size) nothrow @nogc
    {
    }


    void sentinel_Invariant(const void *p) nothrow @nogc
    {
    }

    size_t sentinel_size(const void *p, size_t alloc_size) nothrow @nogc
    {
        return alloc_size;
    }

    void *sentinel_add(void *p) nothrow @nogc
    {
        return p;
    }


    void *sentinel_sub(void *p) nothrow @nogc
    {
        return p;
    }
}

debug (MEMSTOMP)
unittest
{
    import core.memory;
    auto p = cast(size_t*)GC.malloc(size_t.sizeof*3);
    assert(*p == cast(size_t)0xF0F0F0F0F0F0F0F0);
    p[2] = 0; // First two will be used for free list
    GC.free(p);
    assert(p[2] == cast(size_t)0xF2F2F2F2F2F2F2F2);
}

debug (SENTINEL)
unittest
{
    import core.memory;
    auto p = cast(ubyte*)GC.malloc(1);
    assert(p[-1] == 0xF4);
    assert(p[ 1] == 0xF5);

    // See also stand-alone tests in test/gc
}

unittest
{
    import core.memory;

    // https://issues.dlang.org/show_bug.cgi?id=9275
    GC.removeRoot(null);
    GC.removeRoot(cast(void*)13);
}

// improve predictability of coverage of code that is eventually not hit by other tests
debug (SENTINEL) {} else // cannot extend with SENTINEL
debug (MARK_PRINTF) {} else // takes forever
version (OnlyLowMemUnittests) {} else
unittest
{
    import core.memory;
    auto p = GC.malloc(260 << 20); // new pool has 390 MB
    auto q = GC.malloc(65 << 20);  // next chunk (larger than 64MB to ensure the same pool is used)
    auto r = GC.malloc(65 << 20);  // another chunk in same pool
    assert(p + (260 << 20) == q);
    assert(q + (65 << 20) == r);
    GC.free(q);
    // should trigger "assert(bin == Bins.B_FREE);" in mark due to dangling pointer q:
    GC.collect();
    // should trigger "break;" in extendNoSync:
    size_t sz = GC.extend(p, 64 << 20, 66 << 20); // trigger size after p large enough (but limited)
    assert(sz == 325 << 20);
    GC.free(p);
    GC.free(r);
    r = q; // ensure q is not trashed before collection above

    p = GC.malloc(70 << 20); // from the same pool
    q = GC.malloc(70 << 20);
    r = GC.malloc(70 << 20);
    auto s = GC.malloc(70 << 20);
    auto t = GC.malloc(70 << 20); // 350 MB of 390 MB used
    assert(p + (70 << 20) == q);
    assert(q + (70 << 20) == r);
    assert(r + (70 << 20) == s);
    assert(s + (70 << 20) == t);
    GC.free(r); // ensure recalculation of largestFree in nxxt allocPages
    auto z = GC.malloc(75 << 20); // needs new pool

    GC.free(p);
    GC.free(q);
    GC.free(s);
    GC.free(t);
    GC.free(z);
    GC.minimize(); // release huge pool
}

// https://issues.dlang.org/show_bug.cgi?id=19281
debug (SENTINEL) {} else // cannot allow >= 4 GB with SENTINEL
debug (MEMSTOMP) {} else // might take too long to actually touch the memory
version (D_LP64) unittest
{
    static if (__traits(compiles, os_physical_mem))
    {
        // only run if the system has enough physical memory
        size_t sz = 2L^^32;
        size_t phys_mem = os_physical_mem(true);
        if (phys_mem > sz)
        {
            import core.memory;
            import core.exception;
            GC.collect();
            GC.minimize();
            try
            {
                auto stats = GC.stats();
                auto ptr = GC.malloc(sz, BlkAttr.NO_SCAN);
                auto info = GC.query(ptr);
                //printf("info.size = %lld", info.size);
                assert(info.size >= sz);
                GC.free(ptr);
                GC.minimize();
                auto nstats = GC.stats();
                assert(nstats.usedSize == stats.usedSize);
                assert(nstats.freeSize == stats.freeSize);
                assert(nstats.allocatedInCurrentThread - sz == stats.allocatedInCurrentThread);
            }
            catch (OutOfMemoryError)
            {
                // ignore if the system still doesn't have enough virtual memory
                import core.stdc.stdio;
                printf("%s(%d): out-of-memory execption ignored, phys_mem = %zd",
                       __FILE__.ptr, __LINE__, phys_mem);
            }
        }
    }
}

// https://issues.dlang.org/show_bug.cgi?id=19522
unittest
{
    import core.memory;

    void test(void* p)
    {
        assert(GC.getAttr(p) == BlkAttr.NO_SCAN);
        assert(GC.setAttr(p + 4, BlkAttr.NO_SCAN) == 0); // interior pointer should fail
        assert(GC.clrAttr(p + 4, BlkAttr.NO_SCAN) == 0); // interior pointer should fail
        GC.free(p);
        assert(GC.query(p).base == null);
        assert(GC.query(p).size == 0);
        assert(GC.addrOf(p) == null);
        assert(GC.sizeOf(p) == 0); // fails
        assert(GC.getAttr(p) == 0);
        assert(GC.setAttr(p, BlkAttr.NO_SCAN) == 0);
        assert(GC.clrAttr(p, BlkAttr.NO_SCAN) == 0);
    }
    void* large = GC.malloc(10000, BlkAttr.NO_SCAN);
    test(large);

    void* small = GC.malloc(100, BlkAttr.NO_SCAN);
    test(small);
}

unittest
{
    import core.memory;

    auto now = currTime;
    GC.ProfileStats stats1 = GC.profileStats();
    GC.collect();
    GC.ProfileStats stats2 = GC.profileStats();
    auto diff = currTime - now;

    assert(stats2.totalCollectionTime - stats1.totalCollectionTime <= diff);
    assert(stats2.totalPauseTime - stats1.totalPauseTime <= stats2.totalCollectionTime - stats1.totalCollectionTime);

    assert(stats2.maxPauseTime >= stats1.maxPauseTime);
    assert(stats2.maxCollectionTime >= stats1.maxCollectionTime);
}

// https://issues.dlang.org/show_bug.cgi?id=20214
unittest
{
    import core.memory;
    import core.stdc.stdio;

    // allocate from large pool
    auto o = GC.malloc(10);
    auto p = (cast(void**)GC.malloc(4096 * (void*).sizeof))[0 .. 4096];
    auto q = (cast(void**)GC.malloc(4096 * (void*).sizeof))[0 .. 4096];
    if (p.ptr + p.length is q.ptr)
    {
        q[] = o; // fill with pointers

        // shrink, unused area cleared?
        auto nq = (cast(void**)GC.realloc(q.ptr, 4000 * (void*).sizeof))[0 .. 4000];
        assert(q.ptr is nq.ptr);
        assert(q.ptr[4095] !is o);

        GC.free(q.ptr);
        // expected to extend in place
        auto np = (cast(void**)GC.realloc(p.ptr, 4200 * (void*).sizeof))[0 .. 4200];
        assert(p.ptr is np.ptr);
        assert(q.ptr[4200] !is o);
    }
    else
    {
        // adjacent allocations likely but not guaranteed
        printf("unexpected pointers %p and %p\n", p.ptr, q.ptr);
    }
}

/* ============================ MEMSTOMP =============================== */

/// Mark the specified memory region as uninitialized -
/// reading from this region is an error.
/// If writable is false, writing to it is also an error.
pragma(inline, true)
void invalidate(void[] mem, ubyte pattern, bool writable) nothrow @nogc
{
    debug (MEMSTOMP) memset(mem.ptr, pattern, mem.length);
    debug (VALGRIND)
    {
        if (writable)
            makeMemUndefined(mem);
        else
            makeMemNoAccess(mem);
    }
}

/// Read memory that should otherwise be marked as unreadable
/// (e.g. free lists overlapped with unallocated heap objects).
pragma(inline, true)
T undefinedRead(T)(ref T var) nothrow
{
    debug (VALGRIND)
    {
        auto varArr = (&var)[0..1];
        disableAddrReportingInRange(varArr);
        T result = var;
        enableAddrReportingInRange(varArr);
        return result;
    }
    else
        return var;
}

/// Write memory that should otherwise be marked as unwritable.
pragma(inline, true)
void undefinedWrite(T)(ref T var, T value) nothrow
{
    debug (VALGRIND)
    {
        auto varArr = (&var)[0..1];
        disableAddrReportingInRange(varArr);
        var = value;
        enableAddrReportingInRange(varArr);
    }
    else
        var = value;
}
