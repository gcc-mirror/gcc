// Written in the D programming language.
/**
The C heap allocator.

Source: $(PHOBOSSRC std/experimental/allocator/mallocator.d)
*/
module std.experimental.allocator.mallocator;
import std.experimental.allocator.common;

/**
   The C heap allocator.
 */
struct Mallocator
{
    version (StdUnittest) @system unittest { testAllocator!(() => Mallocator.instance); }

    /**
    The alignment is a static constant equal to `platformAlignment`, which
    ensures proper alignment for any D data type.
    */
    enum uint alignment = platformAlignment;

    /**
    Standard allocator methods per the semantics defined above. The
    `deallocate` and `reallocate` methods are `@system` because they
    may move memory around, leaving dangling pointers in user code. Somewhat
    paradoxically, `malloc` is `@safe` but that's only useful to safe
    programs that can afford to leak memory allocated.
    */
    @trusted @nogc nothrow pure
    void[] allocate(size_t bytes) shared const
    {
        import core.memory : pureMalloc;
        if (!bytes) return null;
        auto p = pureMalloc(bytes);
        return p ? p[0 .. bytes] : null;
    }

    /// Ditto
    @system @nogc nothrow pure
    bool deallocate(void[] b) shared const
    {
        import core.memory : pureFree;
        pureFree(b.ptr);
        return true;
    }

    /// Ditto
    @system @nogc nothrow pure
    bool reallocate(ref void[] b, size_t s) shared const
    {
        import core.memory : pureRealloc;
        if (!s)
        {
            // fuzzy area in the C standard, see https://stackoverflow.com/questions/6502077/malloc-and-realloc-functions
            // so just deallocate and nullify the pointer
            deallocate(b);
            b = null;
            return true;
        }
        auto p = cast(ubyte*) pureRealloc(b.ptr, s);
        if (!p) return false;
        b = p[0 .. s];
        return true;
    }

    @trusted @nogc nothrow pure
    package void[] allocateZeroed()(size_t bytes) shared const
    {
        import core.memory : pureCalloc;
        if (!bytes) return null;
        auto p = pureCalloc(1, bytes);
        return p ? p[0 .. bytes] : null;
    }

    /**
    Returns the global instance of this allocator type. The C heap allocator is
    thread-safe, therefore all of its methods and `it` itself are
    `shared`.
    */
    static shared Mallocator instance;
}

///
@nogc @system nothrow unittest
{
    auto buffer = Mallocator.instance.allocate(1024 * 1024 * 4);
    scope(exit) Mallocator.instance.deallocate(buffer);
    //...
}

@nogc @system nothrow pure unittest
{
    @nogc nothrow pure
    static void test(A)()
    {
        int* p = null;
        p = cast(int*) A.instance.allocate(int.sizeof);
        scope(exit) () nothrow @nogc { A.instance.deallocate(p[0 .. int.sizeof]); }();
        *p = 42;
        assert(*p == 42);
    }
    test!Mallocator();
}

@nogc @system nothrow pure unittest
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

version (CRuntime_Microsoft)
{
    @nogc nothrow pure private extern(C) void* _aligned_malloc(size_t, size_t);
    @nogc nothrow pure private extern(C) void _aligned_free(void *memblock);
    @nogc nothrow pure private extern(C) void* _aligned_realloc(void *, size_t, size_t);
}

/**
   Aligned allocator using OS-specific primitives, under a uniform API.
 */
struct AlignedMallocator
{
    version (StdUnittest) @system unittest { testAllocator!(() => typeof(this).instance); }

    /**
    The default alignment is `platformAlignment`.
    */
    enum uint alignment = platformAlignment;

    /**
    Forwards to $(D alignedAllocate(bytes, platformAlignment)).
    */
    @trusted @nogc nothrow pure
    void[] allocate(size_t bytes) shared
    {
        if (!bytes) return null;
        return alignedAllocate(bytes, alignment);
    }

    /**
    Uses $(HTTP man7.org/linux/man-pages/man3/posix_memalign.3.html,
    `posix_memalign`) on Posix and
    $(HTTP msdn.microsoft.com/en-us/library/8z34s9c6(v=vs.80).aspx,
    `__aligned_malloc`) on Windows.
    */
    version (Posix)
    @trusted @nogc nothrow pure
    void[] alignedAllocate(size_t bytes, uint a) shared
    {
        import core.stdc.errno : ENOMEM, EINVAL;
        import core.sys.posix.stdlib : posix_memalign;
        assert(a.isGoodDynamicAlignment);
        void* result;
        auto code = posix_memalign(&result, a, bytes);

version (OSX)
version (LDC_AddressSanitizer)
{
        // The return value with AddressSanitizer may be -1 instead of ENOMEM
        // or EINVAL. See https://bugs.llvm.org/show_bug.cgi?id=36510
        if (code == -1)
            return null;
}
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
    @trusted @nogc nothrow pure
    void[] alignedAllocate(size_t bytes, uint a) shared
    {
        auto result = _aligned_malloc(bytes, a);
        return result ? result[0 .. bytes] : null;
    }
    else static assert(0);

    /**
    Calls `free(b.ptr)` on Posix and
    $(HTTP msdn.microsoft.com/en-US/library/17b5h8td(v=vs.80).aspx,
    `__aligned_free(b.ptr)`) on Windows.
    */
    version (Posix)
    @system @nogc nothrow pure
    bool deallocate(void[] b) shared
    {
        import core.memory : pureFree;
        pureFree(b.ptr);
        return true;
    }
    else version (Windows)
    @system @nogc nothrow pure
    bool deallocate(void[] b) shared
    {
        _aligned_free(b.ptr);
        return true;
    }
    else static assert(0);

    /**
    Forwards to $(D alignedReallocate(b, newSize, platformAlignment)).
    Should be used with blocks obtained with `allocate` otherwise the custom
    alignment passed with `alignedAllocate` can be lost.
    */
    @system @nogc nothrow pure
    bool reallocate(ref void[] b, size_t newSize) shared
    {
        return alignedReallocate(b, newSize, alignment);
    }

    /**
    On Posix there is no `realloc` for aligned memory, so `alignedReallocate` emulates
    the needed behavior by using `alignedAllocate` to get a new block. The existing
    block is copied to the new block and then freed.
    On Windows, calls $(HTTPS msdn.microsoft.com/en-us/library/y69db7sx.aspx,
    $(D __aligned_realloc(b.ptr, newSize, a))).
    */
    version (Windows)
    @system @nogc nothrow pure
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

    /// ditto
    version (Posix)
    @system @nogc nothrow pure
    bool alignedReallocate(ref void[] b, size_t s, uint a) shared
    {
        if (!s)
        {
            deallocate(b);
            b = null;
            return true;
        }
        auto p = alignedAllocate(s, a);
        if (!p.ptr)
        {
            return false;
        }
        import std.algorithm.comparison : min;
        const upTo = min(s, b.length);
        p[0 .. upTo] = b[0 .. upTo];
        deallocate(b);
        b = p;
        return true;
    }

    /**
    Returns the global instance of this allocator type. The C heap allocator is
    thread-safe, therefore all of its methods and `instance` itself are
    `shared`.
    */
    static shared AlignedMallocator instance;
}

///
pure @nogc @system nothrow unittest
{
    auto buffer = AlignedMallocator.instance.alignedAllocate(1024 * 1024 * 4,
        128);
    scope(exit) AlignedMallocator.instance.deallocate(buffer);
    //...
}

version (Posix)
pure @nogc @system nothrow unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=16398
    // test the "pseudo" alignedReallocate for Posix
    void[] b = AlignedMallocator.instance.alignedAllocate(16, 32);
    (cast(ubyte[]) b)[] = ubyte(1);
    AlignedMallocator.instance.alignedReallocate(b, 32, 32);
    ubyte[16] o;
    o[] = 1;
    assert((cast(ubyte[]) b)[0 .. 16] == o);
    AlignedMallocator.instance.alignedReallocate(b, 4, 32);
    assert((cast(ubyte[]) b)[0 .. 3] == o[0 .. 3]);
    AlignedMallocator.instance.alignedReallocate(b, 128, 32);
    assert((cast(ubyte[]) b)[0 .. 3] == o[0 .. 3]);
    AlignedMallocator.instance.deallocate(b);

    void[] c;
    AlignedMallocator.instance.alignedReallocate(c, 32, 32);
    assert(c.ptr);

    version (LDC_AddressSanitizer) {} else // AddressSanitizer does not support such large memory allocations (0x10000000000 max)
    version (DragonFlyBSD) {} else    /* FIXME: Malloc on DragonFly does not return NULL when allocating more than UINTPTR_MAX
                                       * $(LINK: https://bugs.dragonflybsd.org/issues/3114, dragonfly bug report)
                                       * $(LINK: https://github.com/dlang/druntime/pull/1999#discussion_r157536030, PR Discussion) */
    assert(!AlignedMallocator.instance.alignedReallocate(c, size_t.max, 4096));
    AlignedMallocator.instance.deallocate(c);
}
