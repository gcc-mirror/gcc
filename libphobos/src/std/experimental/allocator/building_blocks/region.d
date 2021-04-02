///
module std.experimental.allocator.building_blocks.region;

import std.experimental.allocator.building_blocks.null_allocator;
import std.experimental.allocator.common;
import std.typecons : Flag, Yes, No;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

/**
A $(D Region) allocator allocates memory straight from one contiguous chunk.
There is no deallocation, and once the region is full, allocation requests
return $(D null). Therefore, $(D Region)s are often used (a) in conjunction with
more sophisticated allocators; or (b) for batch-style very fast allocations
that deallocate everything at once.

The region only stores three pointers, corresponding to the current position in
the store and the limits. One allocation entails rounding up the allocation
size for alignment purposes, bumping the current pointer, and comparing it
against the limit.

If $(D ParentAllocator) is different from $(D NullAllocator), $(D Region)
deallocates the chunk of memory during destruction.

The $(D minAlign) parameter establishes alignment. If $(D minAlign > 1), the
sizes of all allocation requests are rounded up to a multiple of $(D minAlign).
Applications aiming at maximum speed may want to choose $(D minAlign = 1) and
control alignment externally.

*/
struct Region(ParentAllocator = NullAllocator,
    uint minAlign = platformAlignment,
    Flag!"growDownwards" growDownwards = No.growDownwards)
{
    static assert(minAlign.isGoodStaticAlignment);
    static assert(ParentAllocator.alignment >= minAlign);

    import std.traits : hasMember;
    import std.typecons : Ternary;

    // state
    /**
    The _parent allocator. Depending on whether $(D ParentAllocator) holds state
    or not, this is a member variable or an alias for
    `ParentAllocator.instance`.
    */
    static if (stateSize!ParentAllocator)
    {
        ParentAllocator parent;
    }
    else
    {
        alias parent = ParentAllocator.instance;
    }
    private void* _current, _begin, _end;

    /**
    Constructs a region backed by a user-provided store. Assumes $(D store) is
    aligned at $(D minAlign). Also assumes the memory was allocated with $(D
    ParentAllocator) (if different from $(D NullAllocator)).

    Params:
    store = User-provided store backing up the region. $(D store) must be
    aligned at $(D minAlign) (enforced with $(D assert)). If $(D
    ParentAllocator) is different from $(D NullAllocator), memory is assumed to
    have been allocated with $(D ParentAllocator).
    n = Bytes to allocate using $(D ParentAllocator). This constructor is only
    defined If $(D ParentAllocator) is different from $(D NullAllocator). If
    $(D parent.allocate(n)) returns $(D null), the region will be initialized
    as empty (correctly initialized but unable to allocate).
    */
    this(ubyte[] store)
    {
        store = cast(ubyte[])(store.roundUpToAlignment(alignment));
        store = store[0 .. $.roundDownToAlignment(alignment)];
        assert(store.ptr.alignedAt(minAlign));
        assert(store.length % minAlign == 0);
        _begin = store.ptr;
        _end = store.ptr + store.length;
        static if (growDownwards)
            _current = _end;
        else
            _current = store.ptr;
    }

    /// Ditto
    static if (!is(ParentAllocator == NullAllocator))
    this(size_t n)
    {
        this(cast(ubyte[])(parent.allocate(n.roundUpToAlignment(alignment))));
    }

    /*
    TODO: The postblit of $(D BasicRegion) should be disabled because such objects
    should not be copied around naively.
    */

    /**
    If `ParentAllocator` is not `NullAllocator` and defines `deallocate`, the region defines a destructor that uses `ParentAllocator.delete` to free the
    memory chunk.
    */
    static if (!is(ParentAllocator == NullAllocator)
        && hasMember!(ParentAllocator, "deallocate"))
    ~this()
    {
        parent.deallocate(_begin[0 .. _end - _begin]);
    }


    /**
    Alignment offered.
    */
    alias alignment = minAlign;

    /**
    Allocates $(D n) bytes of memory. The shortest path involves an alignment
    adjustment (if $(D alignment > 1)), an increment, and a comparison.

    Params:
    n = number of bytes to allocate

    Returns:
    A properly-aligned buffer of size $(D n) or $(D null) if request could not
    be satisfied.
    */
    void[] allocate(size_t n)
    {
        static if (growDownwards)
        {
            if (available < n) return null;
            static if (minAlign > 1)
                const rounded = n.roundUpToAlignment(alignment);
            else
                alias rounded = n;
            assert(available >= rounded);
            auto result = (_current - rounded)[0 .. n];
            assert(result.ptr >= _begin);
            _current = result.ptr;
            assert(owns(result) == Ternary.yes);
            return result;
        }
        else
        {
            auto result = _current[0 .. n];
            static if (minAlign > 1)
                const rounded = n.roundUpToAlignment(alignment);
            else
                alias rounded = n;
            _current += rounded;
            if (_current <= _end) return result;
            // Slow path, backtrack
            _current -= rounded;
            return null;
        }
    }

    /**
    Allocates $(D n) bytes of memory aligned at alignment $(D a).

    Params:
    n = number of bytes to allocate
    a = alignment for the allocated block

    Returns:
    Either a suitable block of $(D n) bytes aligned at $(D a), or $(D null).
    */
    void[] alignedAllocate(size_t n, uint a)
    {
        import std.math : isPowerOf2;
        assert(a.isPowerOf2);
        static if (growDownwards)
        {
            const available = _current - _begin;
            if (available < n) return null;
            auto result = (_current - n).alignDownTo(a)[0 .. n];
            if (result.ptr >= _begin)
            {
                _current = result.ptr;
                return result;
            }
        }
        else
        {
            // Just bump the pointer to the next good allocation
            auto save = _current;
            _current = _current.alignUpTo(a);
            auto result = allocate(n);
            if (result.ptr)
            {
                assert(result.length == n);
                return result;
            }
            // Failed, rollback
            _current = save;
        }
        return null;
    }

    /// Allocates and returns all memory available to this region.
    void[] allocateAll()
    {
        static if (growDownwards)
        {
            auto result = _begin[0 .. available];
            _current = _begin;
        }
        else
        {
            auto result = _current[0 .. available];
            _current = _end;
        }
        return result;
    }

    /**
    Expands an allocated block in place. Expansion will succeed only if the
    block is the last allocated. Defined only if `growDownwards` is
    `No.growDownwards`.
    */
    static if (growDownwards == No.growDownwards)
    bool expand(ref void[] b, size_t delta)
    {
        assert(owns(b) == Ternary.yes || b.ptr is null);
        assert(b.ptr + b.length <= _current || b.ptr is null);
        if (!b.ptr) return delta == 0;
        auto newLength = b.length + delta;
        if (_current < b.ptr + b.length + alignment)
        {
            // This was the last allocation! Allocate some more and we're done.
            if (this.goodAllocSize(b.length) == this.goodAllocSize(newLength)
                || allocate(delta).length == delta)
            {
                b = b.ptr[0 .. newLength];
                assert(_current < b.ptr + b.length + alignment);
                return true;
            }
        }
        return false;
    }

    /**
    Deallocates $(D b). This works only if $(D b) was obtained as the last call
    to $(D allocate); otherwise (i.e. another allocation has occurred since) it
    does nothing. This semantics is tricky and therefore $(D deallocate) is
    defined only if $(D Region) is instantiated with $(D Yes.defineDeallocate)
    as the third template argument.

    Params:
    b = Block previously obtained by a call to $(D allocate) against this
    allocator ($(D null) is allowed).
    */
    bool deallocate(void[] b)
    {
        assert(owns(b) == Ternary.yes || b.ptr is null);
        static if (growDownwards)
        {
            if (b.ptr == _current)
            {
                _current += this.goodAllocSize(b.length);
                return true;
            }
        }
        else
        {
            if (b.ptr + this.goodAllocSize(b.length) == _current)
            {
                assert(b.ptr !is null || _current is null);
                _current = b.ptr;
                return true;
            }
        }
        return false;
    }

    /**
    Deallocates all memory allocated by this region, which can be subsequently
    reused for new allocations.
    */
    bool deallocateAll()
    {
        static if (growDownwards)
        {
            _current = _end;
        }
        else
        {
            _current = _begin;
        }
        return true;
    }

    /**
    Queries whether $(D b) has been allocated with this region.

    Params:
    b = Arbitrary block of memory ($(D null) is allowed; $(D owns(null))
    returns $(D false)).

    Returns:
    $(D true) if $(D b) has been allocated with this region, $(D false)
    otherwise.
    */
    Ternary owns(void[] b) const
    {
        return Ternary(b.ptr >= _begin && b.ptr + b.length <= _end);
    }

    /**
    Returns `Ternary.yes` if no memory has been allocated in this region,
    `Ternary.no` otherwise. (Never returns `Ternary.unknown`.)
    */
    Ternary empty() const
    {
        return Ternary(_current == _begin);
    }

    /// Nonstandard property that returns bytes available for allocation.
    size_t available() const
    {
        static if (growDownwards)
        {
            return _current - _begin;
        }
        else
        {
            return _end - _current;
        }
    }
}

///
@system unittest
{
    import std.algorithm.comparison : max;
    import std.experimental.allocator.building_blocks.allocator_list
        : AllocatorList;
    import std.experimental.allocator.mallocator : Mallocator;
    // Create a scalable list of regions. Each gets at least 1MB at a time by
    // using malloc.
    auto batchAllocator = AllocatorList!(
        (size_t n) => Region!Mallocator(max(n, 1024 * 1024))
    )();
    auto b = batchAllocator.allocate(101);
    assert(b.length == 101);
    // This will cause a second allocation
    b = batchAllocator.allocate(2 * 1024 * 1024);
    assert(b.length == 2 * 1024 * 1024);
    // Destructor will free the memory
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    // Create a 64 KB region allocated with malloc
    auto reg = Region!(Mallocator, Mallocator.alignment,
        Yes.growDownwards)(1024 * 64);
    const b = reg.allocate(101);
    assert(b.length == 101);
    // Destructor will free the memory
}

/**

$(D InSituRegion) is a convenient region that carries its storage within itself
(in the form of a statically-sized array).

The first template argument is the size of the region and the second is the
needed alignment. Depending on the alignment requested and platform details,
the actual available storage may be smaller than the compile-time parameter. To
make sure that at least $(D n) bytes are available in the region, use
$(D InSituRegion!(n + a - 1, a)).

Given that the most frequent use of `InSituRegion` is as a stack allocator, it
allocates starting at the end on systems where stack grows downwards, such that
hot memory is used first.

*/
struct InSituRegion(size_t size, size_t minAlign = platformAlignment)
{
    import std.algorithm.comparison : max;
    import std.conv : to;
    import std.traits : hasMember;
    import std.typecons : Ternary;

    static assert(minAlign.isGoodStaticAlignment);
    static assert(size >= minAlign);

    version (X86) enum growDownwards = Yes.growDownwards;
    else version (X86_64) enum growDownwards = Yes.growDownwards;
    else version (ARM) enum growDownwards = Yes.growDownwards;
    else version (AArch64) enum growDownwards = Yes.growDownwards;
    else version (HPPA) enum growDownwards = No.growDownwards;
    else version (PPC) enum growDownwards = Yes.growDownwards;
    else version (PPC64) enum growDownwards = Yes.growDownwards;
    else version (MIPS32) enum growDownwards = Yes.growDownwards;
    else version (MIPS64) enum growDownwards = Yes.growDownwards;
    else version (RISCV32) enum growDownwards = Yes.growDownwards;
    else version (RISCV64) enum growDownwards = Yes.growDownwards;
    else version (SPARC) enum growDownwards = Yes.growDownwards;
    else version (SPARC64) enum growDownwards = Yes.growDownwards;
    else version (SystemZ) enum growDownwards = Yes.growDownwards;
    else static assert(0, "Dunno how the stack grows on this architecture.");

    @disable this(this);

    // state {
    private Region!(NullAllocator, minAlign, growDownwards) _impl;
    union
    {
        private ubyte[size] _store = void;
        private double _forAlignmentOnly1 = void;
    }
    // }

    /**
    An alias for $(D minAlign), which must be a valid alignment (nonzero power
    of 2). The start of the region and all allocation requests will be rounded
    up to a multiple of the alignment.

    ----
    InSituRegion!(4096) a1;
    assert(a1.alignment == platformAlignment);
    InSituRegion!(4096, 64) a2;
    assert(a2.alignment == 64);
    ----
    */
    alias alignment = minAlign;

    private void lazyInit()
    {
        assert(!_impl._current);
        _impl = typeof(_impl)(_store);
        assert(_impl._current.alignedAt(alignment));
    }

    /**
    Allocates $(D bytes) and returns them, or $(D null) if the region cannot
    accommodate the request. For efficiency reasons, if $(D bytes == 0) the
    function returns an empty non-null slice.
    */
    void[] allocate(size_t n)
    {
        // Fast path
    entry:
        auto result = _impl.allocate(n);
        if (result.length == n) return result;
        // Slow path
        if (_impl._current) return null; // no more room
        lazyInit;
        assert(_impl._current);
        goto entry;
    }

    /**
    As above, but the memory allocated is aligned at $(D a) bytes.
    */
    void[] alignedAllocate(size_t n, uint a)
    {
        // Fast path
    entry:
        auto result = _impl.alignedAllocate(n, a);
        if (result.length == n) return result;
        // Slow path
        if (_impl._current) return null; // no more room
        lazyInit;
        assert(_impl._current);
        goto entry;
    }

    /**
    Deallocates $(D b). This works only if $(D b) was obtained as the last call
    to $(D allocate); otherwise (i.e. another allocation has occurred since) it
    does nothing. This semantics is tricky and therefore $(D deallocate) is
    defined only if $(D Region) is instantiated with $(D Yes.defineDeallocate)
    as the third template argument.

    Params:
    b = Block previously obtained by a call to $(D allocate) against this
    allocator ($(D null) is allowed).
    */
    bool deallocate(void[] b)
    {
        if (!_impl._current) return b is null;
        return _impl.deallocate(b);
    }

    /**
    Returns `Ternary.yes` if `b` is the result of a previous allocation,
    `Ternary.no` otherwise.
    */
    Ternary owns(void[] b)
    {
        if (!_impl._current) return Ternary.no;
        return _impl.owns(b);
    }

    /**
    Expands an allocated block in place. Expansion will succeed only if the
    block is the last allocated.
    */
    static if (hasMember!(typeof(_impl), "expand"))
    bool expand(ref void[] b, size_t delta)
    {
        if (!_impl._current) lazyInit;
        return _impl.expand(b, delta);
    }

    /**
    Deallocates all memory allocated with this allocator.
    */
    bool deallocateAll()
    {
        // We don't care to lazily init the region
        return _impl.deallocateAll;
    }

    /**
    Allocates all memory available with this allocator.
    */
    void[] allocateAll()
    {
        if (!_impl._current) lazyInit;
        return _impl.allocateAll;
    }

    /**
    Nonstandard function that returns the bytes available for allocation.
    */
    size_t available()
    {
        if (!_impl._current) lazyInit;
        return _impl.available;
    }
}

///
@system unittest
{
    // 128KB region, allocated to x86's cache line
    InSituRegion!(128 * 1024, 16) r1;
    auto a1 = r1.allocate(101);
    assert(a1.length == 101);

    // 128KB region, with fallback to the garbage collector.
    import std.experimental.allocator.building_blocks.fallback_allocator
        : FallbackAllocator;
    import std.experimental.allocator.building_blocks.free_list
        : FreeList;
    import std.experimental.allocator.building_blocks.bitmapped_block
        : BitmappedBlock;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    FallbackAllocator!(InSituRegion!(128 * 1024), GCAllocator) r2;
    const a2 = r2.allocate(102);
    assert(a2.length == 102);

    // Reap with GC fallback.
    InSituRegion!(128 * 1024, 8) tmp3;
    FallbackAllocator!(BitmappedBlock!(64, 8), GCAllocator) r3;
    r3.primary = BitmappedBlock!(64, 8)(cast(ubyte[])(tmp3.allocateAll()));
    const a3 = r3.allocate(103);
    assert(a3.length == 103);

    // Reap/GC with a freelist for small objects up to 16 bytes.
    InSituRegion!(128 * 1024, 64) tmp4;
    FreeList!(FallbackAllocator!(BitmappedBlock!(64, 64), GCAllocator), 0, 16) r4;
    r4.parent.primary = BitmappedBlock!(64, 64)(cast(ubyte[])(tmp4.allocateAll()));
    const a4 = r4.allocate(104);
    assert(a4.length == 104);
}

@system unittest
{
    InSituRegion!(4096, 1) r1;
    auto a = r1.allocate(2001);
    assert(a.length == 2001);
    import std.conv : text;
    assert(r1.available == 2095, text(r1.available));

    InSituRegion!(65_536, 1024*4) r2;
    assert(r2.available <= 65_536);
    a = r2.allocate(2001);
    assert(a.length == 2001);
}

version (CRuntime_Musl)
{
    // sbrk and brk are disabled in Musl:
    // https://git.musl-libc.org/cgit/musl/commit/?id=7a995fe706e519a4f55399776ef0df9596101f93
    // https://git.musl-libc.org/cgit/musl/commit/?id=863d628d93ea341b6a32661a1654320ce69f6a07
}
version (DragonFlyBSD)
{
    // sbrk is deprecated in favor of mmap   (we could implement a mmap + MAP_NORESERVE + PROT_NONE version)
    // brk has been removed
    // https://www.dragonflydigest.com/2019/02/22/22586.html
    // http://gitweb.dragonflybsd.org/dragonfly.git/commitdiff/dc676eaefa61b0f47bbea1c53eab86fd5ccd78c6
    // http://gitweb.dragonflybsd.org/dragonfly.git/commitdiff/4b5665564ef37dc939a3a9ffbafaab9894c18885
    // http://gitweb.dragonflybsd.org/dragonfly.git/commitdiff/8618d94a0e2ff8303ad93c123a3fa598c26a116e
}
else
{
    private extern(C) void* sbrk(long) nothrow @nogc;
    private extern(C) int brk(shared void*) nothrow @nogc;
}

/**

Allocator backed by $(D $(LINK2 https://en.wikipedia.org/wiki/Sbrk, sbrk))
for Posix systems. Due to the fact that $(D sbrk) is not thread-safe
$(HTTP lifecs.likai.org/2010/02/sbrk-is-not-thread-safe.html, by design),
$(D SbrkRegion) uses a mutex internally. This implies
that uncontrolled calls to $(D brk) and $(D sbrk) may affect the workings of $(D
SbrkRegion) adversely.

*/
version (CRuntime_Musl) {} else
version (DragonFlyBSD) {} else
version (Posix) struct SbrkRegion(uint minAlign = platformAlignment)
{
    import core.sys.posix.pthread : pthread_mutex_init, pthread_mutex_destroy,
        pthread_mutex_t, pthread_mutex_lock, pthread_mutex_unlock,

    PTHREAD_MUTEX_INITIALIZER;
    private static shared pthread_mutex_t sbrkMutex = PTHREAD_MUTEX_INITIALIZER;
    import std.typecons : Ternary;

    static assert(minAlign.isGoodStaticAlignment);
    static assert(size_t.sizeof == (void*).sizeof);
    private shared void* _brkInitial, _brkCurrent;

    /**
    Instance shared by all callers.
    */
    static shared SbrkRegion instance;

    /**
    Standard allocator primitives.
    */
    enum uint alignment = minAlign;

    /// Ditto
    void[] allocate(size_t bytes) shared
    {
        static if (minAlign > 1)
            const rounded = bytes.roundUpToMultipleOf(alignment);
        else
            alias rounded = bytes;
        pthread_mutex_lock(cast(pthread_mutex_t*) &sbrkMutex) == 0 || assert(0);
        scope(exit) pthread_mutex_unlock(cast(pthread_mutex_t*) &sbrkMutex) == 0
            || assert(0);
        // Assume sbrk returns the old break. Most online documentation confirms
        // that, except for http://www.inf.udec.cl/~leo/Malloc_tutorial.pdf,
        // which claims the returned value is not portable.
        auto p = sbrk(rounded);
        if (p == cast(void*) -1)
        {
            return null;
        }
        if (!_brkInitial)
        {
            _brkInitial = cast(shared) p;
            assert(cast(size_t) _brkInitial % minAlign == 0,
                "Too large alignment chosen for " ~ typeof(this).stringof);
        }
        _brkCurrent = cast(shared) (p + rounded);
        return p[0 .. bytes];
    }

    /// Ditto
    void[] alignedAllocate(size_t bytes, uint a) shared
    {
        pthread_mutex_lock(cast(pthread_mutex_t*) &sbrkMutex) == 0 || assert(0);
        scope(exit) pthread_mutex_unlock(cast(pthread_mutex_t*) &sbrkMutex) == 0
            || assert(0);
        if (!_brkInitial)
        {
            // This is one extra call, but it'll happen only once.
            _brkInitial = cast(shared) sbrk(0);
            assert(cast(size_t) _brkInitial % minAlign == 0,
                "Too large alignment chosen for " ~ typeof(this).stringof);
            (_brkInitial != cast(void*) -1) || assert(0);
            _brkCurrent = _brkInitial;
        }
        immutable size_t delta = cast(shared void*) roundUpToMultipleOf(
            cast(size_t) _brkCurrent, a) - _brkCurrent;
        // Still must make sure the total size is aligned to the allocator's
        // alignment.
        immutable rounded = (bytes + delta).roundUpToMultipleOf(alignment);

        auto p = sbrk(rounded);
        if (p == cast(void*) -1)
        {
            return null;
        }
        _brkCurrent = cast(shared) (p + rounded);
        return p[delta .. delta + bytes];
    }

    /**

    The $(D expand) method may only succeed if the argument is the last block
    allocated. In that case, $(D expand) attempts to push the break pointer to
    the right.

    */
    bool expand(ref void[] b, size_t delta) shared
    {
        if (b is null) return delta == 0;
        assert(_brkInitial && _brkCurrent); // otherwise where did b come from?
        pthread_mutex_lock(cast(pthread_mutex_t*) &sbrkMutex) == 0 || assert(0);
        scope(exit) pthread_mutex_unlock(cast(pthread_mutex_t*) &sbrkMutex) == 0
            || assert(0);
        if (_brkCurrent != b.ptr + b.length) return false;
        // Great, can expand the last block
        static if (minAlign > 1)
            const rounded = delta.roundUpToMultipleOf(alignment);
        else
            alias rounded = bytes;
        auto p = sbrk(rounded);
        if (p == cast(void*) -1)
        {
            return false;
        }
        _brkCurrent = cast(shared) (p + rounded);
        b = b.ptr[0 .. b.length + delta];
        return true;
    }

    /// Ditto
    Ternary owns(void[] b) shared
    {
        // No need to lock here.
        assert(!_brkCurrent || b.ptr + b.length <= _brkCurrent);
        return Ternary(_brkInitial && b.ptr >= _brkInitial);
    }

    /**

    The $(D deallocate) method only works (and returns $(D true))  on systems
    that support reducing the  break address (i.e. accept calls to $(D sbrk)
    with negative offsets). OSX does not accept such. In addition the argument
    must be the last block allocated.

    */
    bool deallocate(void[] b) shared
    {
        static if (minAlign > 1)
            const rounded = b.length.roundUpToMultipleOf(alignment);
        else
            const rounded = b.length;
        pthread_mutex_lock(cast(pthread_mutex_t*) &sbrkMutex) == 0 || assert(0);
        scope(exit) pthread_mutex_unlock(cast(pthread_mutex_t*) &sbrkMutex) == 0
            || assert(0);
        if (_brkCurrent != b.ptr + rounded) return false;
        assert(b.ptr >= _brkInitial);
        if (sbrk(-rounded) == cast(void*) -1)
            return false;
        _brkCurrent = cast(shared) b.ptr;
        return true;
    }

    /**
    The $(D deallocateAll) method only works (and returns $(D true)) on systems
    that support reducing the  break address (i.e. accept calls to $(D sbrk)
    with negative offsets). OSX does not accept such.
    */
    bool deallocateAll() shared
    {
        pthread_mutex_lock(cast(pthread_mutex_t*) &sbrkMutex) == 0 || assert(0);
        scope(exit) pthread_mutex_unlock(cast(pthread_mutex_t*) &sbrkMutex) == 0
            || assert(0);
        return !_brkInitial || brk(_brkInitial) == 0;
    }

    /// Standard allocator API.
    Ternary empty()
    {
        // Also works when they're both null.
        return Ternary(_brkCurrent == _brkInitial);
    }
}

version (CRuntime_Musl) {} else
version (DragonFlyBSD) {} else
version (Posix) @system nothrow @nogc unittest
{
    // Let's test the assumption that sbrk(n) returns the old address
    const p1 = sbrk(0);
    const p2 = sbrk(4096);
    assert(p1 == p2);
    const p3 = sbrk(0);
    assert(p3 == p2 + 4096);
    // Try to reset brk, but don't make a fuss if it doesn't work
    sbrk(-4096);
}

version (CRuntime_Musl) {} else
version (DragonFlyBSD) {} else
version (Posix) @system nothrow @nogc unittest
{
    import std.typecons : Ternary;
    alias alloc = SbrkRegion!(8).instance;
    auto a = alloc.alignedAllocate(2001, 4096);
    assert(a.length == 2001);
    auto b = alloc.allocate(2001);
    assert(b.length == 2001);
    assert(alloc.owns(a) == Ternary.yes);
    assert(alloc.owns(b) == Ternary.yes);
    // reducing the brk does not work on OSX
    version (Darwin) {} else
    {
        assert(alloc.deallocate(b));
        assert(alloc.deallocateAll);
    }
}
