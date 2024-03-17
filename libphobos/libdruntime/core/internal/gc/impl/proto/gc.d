
module core.internal.gc.impl.proto.gc;

import core.gc.gcinterface;

import core.internal.container.array;

import cstdlib = core.stdc.stdlib : calloc, free, malloc, realloc;
static import core.memory;

extern (C) noreturn onOutOfMemoryError(void* pretend_sideffect = null, string file = __FILE__, size_t line = __LINE__) @trusted pure nothrow @nogc; /* dmd @@@BUG11461@@@ */

private
{
    extern (C) void gc_init_nothrow() nothrow @nogc;
    extern (C) void gc_term();

    extern (C) void gc_enable() nothrow;
    extern (C) void gc_disable() nothrow;

    extern (C) void*    gc_malloc( size_t sz, uint ba = 0, const scope TypeInfo = null ) pure nothrow;
    extern (C) void*    gc_calloc( size_t sz, uint ba = 0, const scope TypeInfo = null ) pure nothrow;
    extern (C) BlkInfo  gc_qalloc( size_t sz, uint ba = 0, const scope TypeInfo = null ) pure nothrow;
    extern (C) void*    gc_realloc(return scope void* p, size_t sz, uint ba = 0, const scope TypeInfo = null ) pure nothrow;
    extern (C) size_t   gc_reserve( size_t sz ) nothrow;

    extern (C) void gc_addRange(const void* p, size_t sz, const scope TypeInfo ti = null ) nothrow @nogc;
    extern (C) void gc_addRoot(const void* p ) nothrow @nogc;
}

class ProtoGC : GC
{
    Array!Root roots;
    Array!Range ranges;

    // Call this function when initializing the real GC
    // upon ProtoGC term. This function should be called
    // after the real GC is in place.
    void transferRangesAndRoots()
    {
        // Transfer all ranges
        foreach (ref r; ranges)
        {
            // Range(p, p + sz, cast() ti)
            gc_addRange(r.pbot, r.ptop - r.pbot, r.ti);
        }

        // Transfer all roots
        foreach (ref r; roots)
        {
            gc_addRoot(r.proot);
        }
    }

    this()
    {
    }

    void Dtor()
    {
    }

    void enable()
    {
        .gc_init_nothrow();
        .gc_enable();
    }

    void disable()
    {
        .gc_init_nothrow();
        .gc_disable();
    }

    void collect() nothrow
    {
    }

    void collectNoStack() nothrow
    {
    }

    void minimize() nothrow
    {
    }

    uint getAttr(void* p) nothrow
    {
        return 0;
    }

    uint setAttr(void* p, uint mask) nothrow
    {
        return 0;
    }

    uint clrAttr(void* p, uint mask) nothrow
    {
        return 0;
    }

    void* malloc(size_t size, uint bits, const scope TypeInfo ti) nothrow
    {
        .gc_init_nothrow();
        return .gc_malloc(size, bits, ti);
    }

    BlkInfo qalloc(size_t size, uint bits, const scope TypeInfo ti) nothrow
    {
        .gc_init_nothrow();
        return .gc_qalloc(size, bits, ti);
    }

    void* calloc(size_t size, uint bits, const scope TypeInfo ti) nothrow
    {
        .gc_init_nothrow();
        return .gc_calloc(size, bits, ti);
    }

    void* realloc(void* p, size_t size, uint bits, const scope TypeInfo ti) nothrow
    {
        .gc_init_nothrow();
        return .gc_realloc(p, size, bits, ti);
    }

    size_t extend(void* p, size_t minsize, size_t maxsize, const scope TypeInfo ti) nothrow
    {
        return 0;
    }

    size_t reserve(size_t size) nothrow
    {
        .gc_init_nothrow();
        return .gc_reserve(size);
    }

    void free(void* p) nothrow @nogc
    {
        if (p) assert(false, "Invalid memory deallocation");
    }

    void* addrOf(void* p) nothrow @nogc
    {
        return null;
    }

    size_t sizeOf(void* p) nothrow @nogc
    {
        return 0;
    }

    BlkInfo query(void* p) nothrow
    {
        return BlkInfo.init;
    }

    core.memory.GC.Stats stats() nothrow
    {
        return typeof(return).init;
    }


    core.memory.GC.ProfileStats profileStats() nothrow
    {
        return typeof(return).init;
    }


    void addRoot(void* p) nothrow @nogc
    {
        roots.insertBack(Root(p));
    }

    void removeRoot(void* p) nothrow @nogc
    {
        foreach (ref r; roots)
        {
            if (r is p)
            {
                r = roots.back;
                roots.popBack();
                return;
            }
        }
    }

    @property RootIterator rootIter() return @nogc
    {
        return &rootsApply;
    }

    private int rootsApply(scope int delegate(ref Root) nothrow dg)
    {
        foreach (ref r; roots)
        {
            if (auto result = dg(r))
                return result;
        }
        return 0;
    }

    void addRange(void* p, size_t sz, const TypeInfo ti = null) nothrow @nogc
    {
        ranges.insertBack(Range(p, p + sz, cast() ti));
    }

    void removeRange(void* p) nothrow @nogc
    {
        foreach (ref r; ranges)
        {
            if (r.pbot is p)
            {
                r = ranges.back;
                ranges.popBack();
                return;
            }
        }
    }

    @property RangeIterator rangeIter() return @nogc
    {
        return &rangesApply;
    }

    private int rangesApply(scope int delegate(ref Range) nothrow dg)
    {
        foreach (ref r; ranges)
        {
            if (auto result = dg(r))
                return result;
        }
        return 0;
    }

    void runFinalizers(const scope void[] segment) nothrow
    {
    }

    bool inFinalizer() nothrow
    {
        return false;
    }

    ulong allocatedInCurrentThread() nothrow
    {
        return stats().allocatedInCurrentThread;
    }
}
