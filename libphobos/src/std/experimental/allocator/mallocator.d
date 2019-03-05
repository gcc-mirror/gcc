///
module std.experimental.allocator.mallocator;
import std.experimental.allocator.common;

/**
   The C heap allocator.
 */
struct Mallocator
{
    @system unittest { testAllocator!(() => Mallocator.instance); }

    /**
    The alignment is a static constant equal to $(D platformAlignment), which
    ensures proper alignment for any D data type.
    */
    enum uint alignment = platformAlignment;

    /**
    Standard allocator methods per the semantics defined above. The
    $(D deallocate) and $(D reallocate) methods are $(D @system) because they
    may move memory around, leaving dangling pointers in user code. Somewhat
    paradoxically, $(D malloc) is $(D @safe) but that's only useful to safe
    programs that can afford to leak memory allocated.
    */
    @trusted @nogc nothrow
    void[] allocate(size_t bytes) shared
    {
        import core.stdc.stdlib : malloc;
        if (!bytes) return null;
        auto p = malloc(bytes);
        return p ? p[0 .. bytes] : null;
    }

    /// Ditto
    @system @nogc nothrow
    bool deallocate(void[] b) shared
    {
        import core.stdc.stdlib : free;
        free(b.ptr);
        return true;
    }

    /// Ditto
    @system @nogc nothrow
    bool reallocate(ref void[] b, size_t s) shared
    {
        import core.stdc.stdlib : realloc;
        if (!s)
        {
            // fuzzy area in the C standard, see http://goo.gl/ZpWeSE
            // so just deallocate and nullify the pointer
            deallocate(b);
            b = null;
            return true;
        }
        auto p = cast(ubyte*) realloc(b.ptr, s);
        if (!p) return false;
        b = p[0 .. s];
        return true;
    }

    /**
    Returns the global instance of this allocator type. The C heap allocator is
    thread-safe, therefore all of its methods and `it` itself are
    $(D shared).
    */
    static shared Mallocator instance;
}

///
@nogc nothrow
@system unittest
{
    auto buffer = Mallocator.instance.allocate(1024 * 1024 * 4);
    scope(exit) Mallocator.instance.deallocate(buffer);
    //...
}

@nogc nothrow
@system unittest
{
    @nogc nothrow
    static void test(A)()
    {
        int* p = null;
        p = cast(int*) A.instance.allocate(int.sizeof);
        scope(exit) A.instance.deallocate(p[0 .. int.sizeof]);
        *p = 42;
        assert(*p == 42);
    }
    test!Mallocator();
}

@nogc nothrow
@system unittest
{
    static void test(A)()
    {
        import std.experimental.allocator : make;
        Object p = null;
        p = A.instance.make!Object();
        assert(p !is null);
    }

    test!Mallocator();
}

version (Windows)
{
    // DMD Win 32 bit, DigitalMars C standard library misses the _aligned_xxx
    // functions family (snn.lib)
    version (CRuntime_DigitalMars)
    {
        // Helper to cast the infos written before the aligned pointer
        // this header keeps track of the size (required to realloc) and of
        // the base ptr (required to free).
        private struct AlignInfo
        {
            void* basePtr;
            size_t size;

            @nogc nothrow
            static AlignInfo* opCall(void* ptr)
            {
                return cast(AlignInfo*) (ptr - AlignInfo.sizeof);
            }
        }

        @nogc nothrow
        private void* _aligned_malloc(size_t size, size_t alignment)
        {
            import core.stdc.stdlib : malloc;
            size_t offset = alignment + size_t.sizeof * 2 - 1;

            // unaligned chunk
            void* basePtr = malloc(size + offset);
            if (!basePtr) return null;

            // get aligned location within the chunk
            void* alignedPtr = cast(void**)((cast(size_t)(basePtr) + offset)
                & ~(alignment - 1));

            // write the header before the aligned pointer
            AlignInfo* head = AlignInfo(alignedPtr);
            head.basePtr = basePtr;
            head.size = size;

            return alignedPtr;
        }

        @nogc nothrow
        private void* _aligned_realloc(void* ptr, size_t size, size_t alignment)
        {
            import core.stdc.stdlib : free;
            import core.stdc.string : memcpy;

            if (!ptr) return _aligned_malloc(size, alignment);

            // gets the header from the exising pointer
            AlignInfo* head = AlignInfo(ptr);

            // gets a new aligned pointer
            void* alignedPtr = _aligned_malloc(size, alignment);
            if (!alignedPtr)
            {
                //to https://msdn.microsoft.com/en-us/library/ms235462.aspx
                //see Return value: in this case the original block is unchanged
                return null;
            }

            // copy exising data
            memcpy(alignedPtr, ptr, head.size);
            free(head.basePtr);

            return alignedPtr;
        }

        @nogc nothrow
        private void _aligned_free(void *ptr)
        {
            import core.stdc.stdlib : free;
            if (!ptr) return;
            AlignInfo* head = AlignInfo(ptr);
            free(head.basePtr);
        }

    }
    // DMD Win 64 bit, uses microsoft standard C library which implements them
    else
    {
        @nogc nothrow private extern(C) void* _aligned_malloc(size_t, size_t);
        @nogc nothrow private extern(C) void _aligned_free(void *memblock);
        @nogc nothrow private extern(C) void* _aligned_realloc(void *, size_t, size_t);
    }
}

/**
   Aligned allocator using OS-specific primitives, under a uniform API.
 */
struct AlignedMallocator
{
    @system unittest { testAllocator!(() => typeof(this).instance); }

    /**
    The default alignment is $(D platformAlignment).
    */
    enum uint alignment = platformAlignment;

    /**
    Forwards to $(D alignedAllocate(bytes, platformAlignment)).
    */
    @trusted @nogc nothrow
    void[] allocate(size_t bytes) shared
    {
        if (!bytes) return null;
        return alignedAllocate(bytes, alignment);
    }

    /**
    Uses $(HTTP man7.org/linux/man-pages/man3/posix_memalign.3.html,
    $(D posix_memalign)) on Posix and
    $(HTTP msdn.microsoft.com/en-us/library/8z34s9c6(v=vs.80).aspx,
    $(D __aligned_malloc)) on Windows.
    */
    version (Posix)
    @trusted @nogc nothrow
    void[] alignedAllocate(size_t bytes, uint a) shared
    {
        import core.stdc.errno : ENOMEM, EINVAL;
        import core.sys.posix.stdlib : posix_memalign;
        assert(a.isGoodDynamicAlignment);
        void* result;
        auto code = posix_memalign(&result, a, bytes);
        if (code == ENOMEM)
            return null;

        else if (code == EINVAL)
        {
            assert(0, "AlignedMallocator.alignment is not a power of two "
                ~"multiple of (void*).sizeof, according to posix_memalign!");
        }
        else if (code != 0)
            assert(0, "posix_memalign returned an unknown code!");

        else
            return result[0 .. bytes];
    }
    else version (Windows)
    @trusted @nogc nothrow
    void[] alignedAllocate(size_t bytes, uint a) shared
    {
        auto result = _aligned_malloc(bytes, a);
        return result ? result[0 .. bytes] : null;
    }
    else static assert(0);

    /**
    Calls $(D free(b.ptr)) on Posix and
    $(HTTP msdn.microsoft.com/en-US/library/17b5h8td(v=vs.80).aspx,
    $(D __aligned_free(b.ptr))) on Windows.
    */
    version (Posix)
    @system @nogc nothrow
    bool deallocate(void[] b) shared
    {
        import core.stdc.stdlib : free;
        free(b.ptr);
        return true;
    }
    else version (Windows)
    @system @nogc nothrow
    bool deallocate(void[] b) shared
    {
        _aligned_free(b.ptr);
        return true;
    }
    else static assert(0);

    /**
    On Posix, forwards to $(D realloc). On Windows, forwards to
    $(D alignedReallocate(b, newSize, platformAlignment)).
    */
    version (Posix)
    @system @nogc nothrow
    bool reallocate(ref void[] b, size_t newSize) shared
    {
        return Mallocator.instance.reallocate(b, newSize);
    }
    version (Windows)
    @system @nogc nothrow
    bool reallocate(ref void[] b, size_t newSize) shared
    {
        return alignedReallocate(b, newSize, alignment);
    }

    /**
    On Posix, uses $(D alignedAllocate) and copies data around because there is
    no realloc for aligned memory. On Windows, calls
    $(HTTP msdn.microsoft.com/en-US/library/y69db7sx(v=vs.80).aspx,
    $(D __aligned_realloc(b.ptr, newSize, a))).
    */
    version (Windows)
    @system @nogc nothrow
    bool alignedReallocate(ref void[] b, size_t s, uint a) shared
    {
        if (!s)
        {
            deallocate(b);
            b = null;
            return true;
        }
        auto p = cast(ubyte*) _aligned_realloc(b.ptr, s, a);
        if (!p) return false;
        b = p[0 .. s];
        return true;
    }

    /**
    Returns the global instance of this allocator type. The C heap allocator is
    thread-safe, therefore all of its methods and `instance` itself are
    $(D shared).
    */
    static shared AlignedMallocator instance;
}

///
@nogc nothrow
@system unittest
{
    auto buffer = AlignedMallocator.instance.alignedAllocate(1024 * 1024 * 4,
        128);
    scope(exit) AlignedMallocator.instance.deallocate(buffer);
    //...
}

version (unittest) version (CRuntime_DigitalMars)
@nogc nothrow
size_t addr(ref void* ptr) { return cast(size_t) ptr; }

version (CRuntime_DigitalMars)
@nogc nothrow
@system unittest
{
    void* m;

    m = _aligned_malloc(16, 0x10);
    if (m)
    {
        assert((m.addr & 0xF) == 0);
        _aligned_free(m);
    }

    m = _aligned_malloc(16, 0x100);
    if (m)
    {
        assert((m.addr & 0xFF) == 0);
        _aligned_free(m);
    }

    m = _aligned_malloc(16, 0x1000);
    if (m)
    {
        assert((m.addr & 0xFFF) == 0);
        _aligned_free(m);
    }

    m = _aligned_malloc(16, 0x10);
    if (m)
    {
        assert((cast(size_t) m & 0xF) == 0);
        m = _aligned_realloc(m, 32, 0x10000);
        if (m) assert((m.addr & 0xFFFF) == 0);
        _aligned_free(m);
    }

    m = _aligned_malloc(8, 0x10);
    if (m)
    {
        *cast(ulong*) m = 0X01234567_89ABCDEF;
        m = _aligned_realloc(m, 0x800, 0x1000);
        if (m) assert(*cast(ulong*) m == 0X01234567_89ABCDEF);
        _aligned_free(m);
    }
}
