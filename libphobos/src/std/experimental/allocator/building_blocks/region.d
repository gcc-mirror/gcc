// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/region.d)
*/
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
A `Region` allocator allocates memory straight from one contiguous chunk.
There is no deallocation, and once the region is full, allocation requests
return `null`. Therefore, `Region`s are often used (a) in conjunction with
more sophisticated allocators; or (b) for batch-style very fast allocations
that deallocate everything at once.

The region only stores three pointers, corresponding to the current position in
the store and the limits. One allocation entails rounding up the allocation
size for alignment purposes, bumping the current pointer, and comparing it
against the limit.

`Region` deallocates the chunk of memory during destruction.

The `minAlign` parameter establishes alignment. If $(D minAlign > 1), the
sizes of all allocation requests are rounded up to a multiple of `minAlign`.
Applications aiming at maximum speed may want to choose $(D minAlign = 1) and
control alignment externally.

*/
struct Region(ParentAllocator,
    uint minAlign = platformAlignment,
    Flag!"growDownwards" growDownwards = No.growDownwards)
{
    static assert(minAlign.isGoodStaticAlignment);
    static assert(ParentAllocator.alignment >= minAlign);

    import std.traits : hasMember;
    import std.typecons : Ternary;

    // state
    /**
    The _parent allocator. Depending on whether `ParentAllocator` holds state
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

    private BorrowedRegion!(minAlign, growDownwards) _impl;

    private void* roundedBegin() const pure nothrow @trusted @nogc
    {
        return _impl.roundedBegin;
    }

    private void* roundedEnd() const pure nothrow @trusted @nogc
    {
        return _impl.roundedEnd;
    }
    /**
    Constructs a region backed by a user-provided store.
    Assumes the memory was allocated with `ParentAllocator`.

    Params:
        store = User-provided store backing up the region. Assumed to have been
        allocated with `ParentAllocator`.
        n = Bytes to allocate using `ParentAllocator`. If `parent.allocate(n)`
        returns `null`, the region will be initialized as empty (correctly
        initialized but unable to allocate).
        */
    this(ubyte[] store) pure nothrow @nogc
    {
        _impl = store;
    }

    /// Ditto
    static if (!stateSize!ParentAllocator)
    this(size_t n)
    {
        this(cast(ubyte[]) (parent.allocate(n.roundUpToAlignment(alignment))));
    }

    /// Ditto
    static if (stateSize!ParentAllocator)
    this(ParentAllocator parent, size_t n)
    {
        this.parent = parent;
        this(cast(ubyte[]) (parent.allocate(n.roundUpToAlignment(alignment))));
    }

    /*
    TODO: The postblit of `BasicRegion` should be disabled because such objects
    should not be copied around naively.
    */

    /**
    If `ParentAllocator` defines `deallocate`, the region defines a destructor
    that uses `ParentAllocator.deallocate` to free the memory chunk.
    */
    static if (hasMember!(ParentAllocator, "deallocate"))
    ~this()
    {
        with (_impl) parent.deallocate(_begin[0 .. _end - _begin]);
    }

    /**
    Rounds the given size to a multiple of the `alignment`
    */
    size_t goodAllocSize(size_t n) const pure nothrow @safe @nogc
    {
        return _impl.goodAllocSize(n);
    }

    /**
    Alignment offered.
    */
    alias alignment = minAlign;

    /**
    Allocates `n` bytes of memory. The shortest path involves an alignment
    adjustment (if $(D alignment > 1)), an increment, and a comparison.

    Params:
        n = number of bytes to allocate

    Returns:
        A properly-aligned buffer of size `n` or `null` if request could not
        be satisfied.
    */
    void[] allocate(size_t n) pure nothrow @trusted @nogc
    {
        return _impl.allocate(n);
    }

    /**
    Allocates `n` bytes of memory aligned at alignment `a`.

    Params:
        n = number of bytes to allocate
        a = alignment for the allocated block

    Returns:
        Either a suitable block of `n` bytes aligned at `a`, or `null`.
    */
    void[] alignedAllocate(size_t n, uint a) pure nothrow @trusted @nogc
    {
        return _impl.alignedAllocate(n, a);
    }

    /// Allocates and returns all memory available to this region.
    void[] allocateAll() pure nothrow @trusted @nogc
    {
        return _impl.allocateAll;
    }

    /**
    Expands an allocated block in place. Expansion will succeed only if the
    block is the last allocated. Defined only if `growDownwards` is
    `No.growDownwards`.
    */
    static if (growDownwards == No.growDownwards)
    bool expand(ref void[] b, size_t delta) pure nothrow @safe @nogc
    {
        return _impl.expand(b, delta);
    }

    /**
    Deallocates `b`. This works only if `b` was obtained as the last call
    to `allocate`; otherwise (i.e. another allocation has occurred since) it
    does nothing.

    Params:
        b = Block previously obtained by a call to `allocate` against this
        allocator (`null` is allowed).
    */
    bool deallocate(void[] b) pure nothrow @nogc
    {
        return _impl.deallocate(b);
    }

    /**
    Deallocates all memory allocated by this region, which can be subsequently
    reused for new allocations.
    */
    bool deallocateAll() pure nothrow @nogc
    {
        return _impl.deallocateAll;
    }

    /**
    Queries whether `b` has been allocated with this region.

    Params:
        b = Arbitrary block of memory (`null` is allowed; `owns(null)` returns
        `false`).

    Returns:
        `true` if `b` has been allocated with this region, `false` otherwise.
    */
    Ternary owns(const void[] b) const pure nothrow @trusted @nogc
    {
        return _impl.owns(b);
    }

    /**
    Returns `Ternary.yes` if no memory has been allocated in this region,
    `Ternary.no` otherwise. (Never returns `Ternary.unknown`.)
    */
    Ternary empty() const pure nothrow @safe @nogc
    {
        return _impl.empty;
    }

    /// Nonstandard property that returns bytes available for allocation.
    size_t available() const @safe pure nothrow @nogc
    {
        return _impl.available;
    }
}

///
@system nothrow unittest
{
    import std.algorithm.comparison : max;
    import std.experimental.allocator.building_blocks.allocator_list
        : AllocatorList;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Ternary;
    // Create a scalable list of regions. Each gets at least 1MB at a time by
    // using malloc.
    auto batchAllocator = AllocatorList!(
        (size_t n) => Region!Mallocator(max(n, 1024 * 1024))
    )();
    assert(batchAllocator.empty ==  Ternary.yes);
    auto b = batchAllocator.allocate(101);
    assert(b.length == 101);
    assert(batchAllocator.empty ==  Ternary.no);
    // This will cause a second allocation
    b = batchAllocator.allocate(2 * 1024 * 1024);
    assert(b.length == 2 * 1024 * 1024);
    // Destructor will free the memory
}

@system nothrow @nogc unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Ternary;

    static void testAlloc(Allocator)(ref Allocator a)
    {
        assert((() pure nothrow @safe @nogc => a.empty)() ==  Ternary.yes);
        const b = a.allocate(101);
        assert(b.length == 101);
        assert((() nothrow @safe @nogc => a.owns(b))() == Ternary.yes);

        // Ensure deallocate inherits from parent allocators
        auto c = a.allocate(42);
        assert(c.length == 42);
        assert((() nothrow @nogc => a.deallocate(c))());
        assert((() pure nothrow @safe @nogc => a.empty)() ==  Ternary.no);
    }

    // Create a 64 KB region allocated with malloc
    auto reg = Region!(Mallocator, Mallocator.alignment,
        Yes.growDownwards)(1024 * 64);
    testAlloc(reg);

    // Create a 64 KB shared region allocated with malloc
    auto sharedReg = SharedRegion!(Mallocator, Mallocator.alignment,
        Yes.growDownwards)(1024 * 64);
    testAlloc(sharedReg);
}

@system nothrow @nogc unittest
{
    // test 'this(ubyte[] store)' constructed regions properly clean up
    // their inner storage after destruction
    import std.experimental.allocator.mallocator : Mallocator;

    static shared struct LocalAllocator
    {
    nothrow @nogc:
        enum alignment = Mallocator.alignment;
        void[] buf;
        bool deallocate(void[] b)
        {
            assert(buf.ptr == b.ptr && buf.length == b.length);
            return true;
        }

        void[] allocate(size_t n)
        {
            return null;
        }

    }

    enum bufLen = 10 * Mallocator.alignment;
    void[] tmp = Mallocator.instance.allocate(bufLen);

    LocalAllocator a;
    a.buf = cast(typeof(a.buf)) tmp[1 .. $];

    auto reg = Region!(LocalAllocator, Mallocator.alignment,
        Yes.growDownwards)(cast(ubyte[]) a.buf);
    auto sharedReg = SharedRegion!(LocalAllocator, Mallocator.alignment,
        Yes.growDownwards)(cast(ubyte[]) a.buf);
    reg.parent = a;
    sharedReg.parent = a;

    Mallocator.instance.deallocate(tmp);
}

version (StdUnittest)
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;

    testAllocator!(() => Region!(Mallocator)(1024 * 64));
    testAllocator!(() => Region!(Mallocator, Mallocator.alignment, Yes.growDownwards)(1024 * 64));

    testAllocator!(() => SharedRegion!(Mallocator)(1024 * 64));
    testAllocator!(() => SharedRegion!(Mallocator, Mallocator.alignment, Yes.growDownwards)(1024 * 64));
}

@system nothrow @nogc unittest
{
    import std.experimental.allocator.mallocator : Mallocator;

    auto reg = Region!(Mallocator)(1024 * 64);
    auto b = reg.allocate(101);
    assert(b.length == 101);
    assert((() pure nothrow @safe @nogc => reg.expand(b, 20))());
    assert((() pure nothrow @safe @nogc => reg.expand(b, 73))());
    assert((() pure nothrow @safe @nogc => !reg.expand(b, 1024 * 64))());
    assert((() nothrow @nogc => reg.deallocateAll())());
}

/**
A `BorrowedRegion` allocates directly from a user-provided block of memory.

Unlike a `Region`, a `BorrowedRegion` does not own the memory it allocates from
and will not deallocate that memory upon destruction. Instead, it is the user's
responsibility to ensure that the memory is properly disposed of.

In all other respects, a `BorrowedRegion` behaves exactly like a `Region`.
*/
struct BorrowedRegion(uint minAlign = platformAlignment,
    Flag!"growDownwards" growDownwards = No.growDownwards)
{
    static assert(minAlign.isGoodStaticAlignment);

    import std.typecons : Ternary;

    // state
    private void* _current, _begin, _end;

    private void* roundedBegin() const pure nothrow @trusted @nogc
    {
        return cast(void*) roundUpToAlignment(cast(size_t) _begin, alignment);
    }

    private void* roundedEnd() const pure nothrow @trusted @nogc
    {
        return cast(void*) roundDownToAlignment(cast(size_t) _end, alignment);
    }

    /**
    Constructs a region backed by a user-provided store.

    Params:
        store = User-provided store backing up the region.
    */
    this(ubyte[] store) pure nothrow @nogc
    {
        _begin = store.ptr;
        _end = store.ptr + store.length;
        static if (growDownwards)
            _current = roundedEnd();
        else
            _current = roundedBegin();
    }

    /*
    TODO: The postblit of `BorrowedRegion` should be disabled because such objects
    should not be copied around naively.
    */

    /**
    Rounds the given size to a multiple of the `alignment`
    */
    size_t goodAllocSize(size_t n) const pure nothrow @safe @nogc
    {
        return n.roundUpToAlignment(alignment);
    }

    /**
    Alignment offered.
    */
    alias alignment = minAlign;

    /**
    Allocates `n` bytes of memory. The shortest path involves an alignment
    adjustment (if $(D alignment > 1)), an increment, and a comparison.

    Params:
        n = number of bytes to allocate

    Returns:
        A properly-aligned buffer of size `n` or `null` if request could not
        be satisfied.
    */
    void[] allocate(size_t n) pure nothrow @trusted @nogc
    {
        const rounded = goodAllocSize(n);
        if (n == 0 || rounded < n || available < rounded) return null;

        static if (growDownwards)
        {
            assert(available >= rounded);
            auto result = (_current - rounded)[0 .. n];
            assert(result.ptr >= _begin);
            _current = result.ptr;
            assert(owns(result) == Ternary.yes);
        }
        else
        {
            auto result = _current[0 .. n];
            _current += rounded;
        }

        return result;
    }

    /**
    Allocates `n` bytes of memory aligned at alignment `a`.

    Params:
        n = number of bytes to allocate
        a = alignment for the allocated block

    Returns:
        Either a suitable block of `n` bytes aligned at `a`, or `null`.
    */
    void[] alignedAllocate(size_t n, uint a) pure nothrow @trusted @nogc
    {
        import std.math.traits : isPowerOf2;
        assert(a.isPowerOf2);

        const rounded = goodAllocSize(n);
        if (n == 0 || rounded < n || available < rounded) return null;

        static if (growDownwards)
        {
            auto tmpCurrent = _current - rounded;
            auto result = tmpCurrent.alignDownTo(a);
            if (result <= tmpCurrent && result >= _begin)
            {
                _current = result;
                return cast(void[]) result[0 .. n];
            }
        }
        else
        {
            // Just bump the pointer to the next good allocation
            auto newCurrent = _current.alignUpTo(a);
            if (newCurrent < _current || newCurrent > _end)
                return null;

            auto save = _current;
            _current = newCurrent;
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
    void[] allocateAll() pure nothrow @trusted @nogc
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
    bool expand(ref void[] b, size_t delta) pure nothrow @safe @nogc
    {
        assert(owns(b) == Ternary.yes || b is null);
        assert((() @trusted => b.ptr + b.length <= _current)() || b is null);
        if (b is null || delta == 0) return delta == 0;
        auto newLength = b.length + delta;
        if ((() @trusted => _current < b.ptr + b.length + alignment)())
        {
            immutable currentGoodSize = this.goodAllocSize(b.length);
            immutable newGoodSize = this.goodAllocSize(newLength);
            immutable goodDelta = newGoodSize - currentGoodSize;
            // This was the last allocation! Allocate some more and we're done.
            if (goodDelta == 0
                || (() @trusted => allocate(goodDelta).length == goodDelta)())
            {
                b = (() @trusted => b.ptr[0 .. newLength])();
                assert((() @trusted => _current < b.ptr + b.length + alignment)());
                return true;
            }
        }
        return false;
    }

    /**
    Deallocates `b`. This works only if `b` was obtained as the last call
    to `allocate`; otherwise (i.e. another allocation has occurred since) it
    does nothing.

    Params:
        b = Block previously obtained by a call to `allocate` against this
        allocator (`null` is allowed).
    */
    bool deallocate(void[] b) pure nothrow @nogc
    {
        assert(owns(b) == Ternary.yes || b.ptr is null);
        auto rounded = goodAllocSize(b.length);
        static if (growDownwards)
        {
            if (b.ptr == _current)
            {
                _current += rounded;
                return true;
            }
        }
        else
        {
            if (b.ptr + rounded == _current)
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
    bool deallocateAll() pure nothrow @nogc
    {
        static if (growDownwards)
        {
            _current = roundedEnd();
        }
        else
        {
            _current = roundedBegin();
        }
        return true;
    }

    /**
    Queries whether `b` has been allocated with this region.

    Params:
        b = Arbitrary block of memory (`null` is allowed; `owns(null)` returns
        `false`).

    Returns:
        `true` if `b` has been allocated with this region, `false` otherwise.
    */
    Ternary owns(const void[] b) const pure nothrow @trusted @nogc
    {
        return Ternary(b && (&b[0] >= _begin) && (&b[0] + b.length <= _end));
    }

    /**
    Returns `Ternary.yes` if no memory has been allocated in this region,
    `Ternary.no` otherwise. (Never returns `Ternary.unknown`.)
    */
    Ternary empty() const pure nothrow @safe @nogc
    {
        static if (growDownwards)
            return Ternary(_current == roundedEnd());
        else
            return Ternary(_current == roundedBegin());
    }

    /// Nonstandard property that returns bytes available for allocation.
    size_t available() const @safe pure nothrow @nogc
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
@system nothrow @nogc unittest
{
    import std.typecons : Ternary;

    ubyte[1024] store;
    auto myRegion = BorrowedRegion!(1)(store[]);

    assert(myRegion.empty == Ternary.yes);
    assert(myRegion.available == store.length);

    void[] b = myRegion.allocate(101);

    assert(b.length == 101);
    assert(myRegion.empty == Ternary.no);
    assert(myRegion.owns(b) == Ternary.yes);
    assert(myRegion.available == store.length - b.length);

    void[] b2 = myRegion.allocate(256);

    // Can only free the most recent allocation
    assert(myRegion.deallocate(b) == false);
    assert(myRegion.deallocate(b2) == true);

    myRegion.deallocateAll();

    assert(myRegion.empty == Ternary.yes);
}

@system nothrow @nogc unittest
{
    import std.experimental.allocator.mallocator : AlignedMallocator;
    import std.typecons : Ternary;

    ubyte[] buf = cast(ubyte[]) AlignedMallocator.instance.alignedAllocate(64, 64);
    auto reg = BorrowedRegion!(64, Yes.growDownwards)(buf);
    assert(reg.alignedAllocate(10, 32).length == 10);
    assert(!reg.available);
}

/**

`InSituRegion` is a convenient region that carries its storage within itself
(in the form of a statically-sized array).

The first template argument is the size of the region and the second is the
needed alignment. Depending on the alignment requested and platform details,
the actual available storage may be smaller than the compile-time parameter. To
make sure that at least `n` bytes are available in the region, use
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
    import core.thread.types : isStackGrowingDown;

    static assert(minAlign.isGoodStaticAlignment);
    static assert(size >= minAlign);

    static if (isStackGrowingDown)
        enum growDownwards = Yes.growDownwards;
    else
        enum growDownwards = No.growDownwards;

    @disable this(this);

    // state {
    private BorrowedRegion!(minAlign, growDownwards) _impl;
    union
    {
        private ubyte[size] _store = void;
        private double _forAlignmentOnly1;
    }
    // }

    /**
    An alias for `minAlign`, which must be a valid alignment (nonzero power
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
    Allocates `bytes` and returns them, or `null` if the region cannot
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
    As above, but the memory allocated is aligned at `a` bytes.
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
    Deallocates `b`. This works only if `b` was obtained as the last call
    to `allocate`; otherwise (i.e. another allocation has occurred since) it
    does nothing. This semantics is tricky and therefore `deallocate` is
    defined only if `Region` is instantiated with `Yes.defineDeallocate`
    as the third template argument.

    Params:
        b = Block previously obtained by a call to `allocate` against this
        allocator (`null` is allowed).
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
    Ternary owns(const void[] b) pure nothrow @safe @nogc
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
    r3.primary = BitmappedBlock!(64, 8)(cast(ubyte[]) (tmp3.allocateAll()));
    const a3 = r3.allocate(103);
    assert(a3.length == 103);

    // Reap/GC with a freelist for small objects up to 16 bytes.
    InSituRegion!(128 * 1024, 64) tmp4;
    FreeList!(FallbackAllocator!(BitmappedBlock!(64, 64), GCAllocator), 0, 16) r4;
    r4.parent.primary = BitmappedBlock!(64, 64)(cast(ubyte[]) (tmp4.allocateAll()));
    const a4 = r4.allocate(104);
    assert(a4.length == 104);
}

@system pure nothrow unittest
{
    import std.typecons : Ternary;

    InSituRegion!(4096, 1) r1;
    auto a = r1.allocate(2001);
    assert(a.length == 2001);
    import std.conv : text;
    assert(r1.available == 2095, text(r1.available));
    // Ensure deallocate inherits from parent
    assert((() nothrow @nogc => r1.deallocate(a))());
    assert((() nothrow @nogc => r1.deallocateAll())());

    InSituRegion!(65_536, 1024*4) r2;
    assert(r2.available <= 65_536);
    a = r2.allocate(2001);
    assert(a.length == 2001);
    const void[] buff = r2.allocate(42);
    assert((() nothrow @safe @nogc => r2.owns(buff))() == Ternary.yes);
    assert((() nothrow @nogc => r2.deallocateAll())());
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
for Posix systems. Due to the fact that `sbrk` is not thread-safe
$(HTTP lifecs.likai.org/2010/02/sbrk-is-not-thread-safe.html, by design),
`SbrkRegion` uses a mutex internally. This implies
that uncontrolled calls to `brk` and `sbrk` may affect the workings of $(D
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

    /**
    Rounds the given size to a multiple of thew `alignment`
    */
    size_t goodAllocSize(size_t n) shared const pure nothrow @safe @nogc
    {
        return n.roundUpToMultipleOf(alignment);
    }

    /// Ditto
    void[] allocate(size_t bytes) shared @trusted nothrow @nogc
    {
        // Take alignment rounding into account
        const rounded = goodAllocSize(bytes);

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
    void[] alignedAllocate(size_t bytes, uint a) shared @trusted nothrow @nogc
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

    The `expand` method may only succeed if the argument is the last block
    allocated. In that case, `expand` attempts to push the break pointer to
    the right.

    */
    bool expand(ref void[] b, size_t delta) shared nothrow @trusted @nogc
    {
        if (b is null || delta == 0) return delta == 0;
        assert(_brkInitial && _brkCurrent); // otherwise where did b come from?
        pthread_mutex_lock(cast(pthread_mutex_t*) &sbrkMutex) == 0 || assert(0);
        scope(exit) pthread_mutex_unlock(cast(pthread_mutex_t*) &sbrkMutex) == 0
            || assert(0);

        // Take alignment rounding into account
        const rounded = goodAllocSize(b.length);

        const slack = rounded - b.length;
        if (delta <= slack)
        {
            b = b.ptr[0 .. b.length + delta];
            return true;
        }

        if (_brkCurrent != b.ptr + rounded) return false;
        // Great, can expand the last block
        delta -= slack;

        const roundedDelta = goodAllocSize(delta);
        auto p = sbrk(roundedDelta);
        if (p == cast(void*) -1)
        {
            return false;
        }
        _brkCurrent = cast(shared) (p + roundedDelta);
        b = b.ptr[0 .. b.length + slack + delta];
        return true;
    }

    /// Ditto
    Ternary owns(const void[] b) shared pure nothrow @trusted @nogc
    {
        // No need to lock here.
        assert(!_brkCurrent || !b || &b[0] + b.length <= _brkCurrent);
        return Ternary(_brkInitial && b && (&b[0] >= _brkInitial));
    }

    /**

    The `deallocate` method only works (and returns `true`)  on systems
    that support reducing the  break address (i.e. accept calls to `sbrk`
    with negative offsets). OSX does not accept such. In addition the argument
    must be the last block allocated.

    */
    bool deallocate(void[] b) shared nothrow @nogc
    {
        // Take alignment rounding into account
        const rounded = goodAllocSize(b.length);
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
    The `deallocateAll` method only works (and returns `true`) on systems
    that support reducing the  break address (i.e. accept calls to `sbrk`
    with negative offsets). OSX does not accept such.
    */
    nothrow @nogc
    bool deallocateAll() shared
    {
        pthread_mutex_lock(cast(pthread_mutex_t*) &sbrkMutex) == 0 || assert(0);
        scope(exit) pthread_mutex_unlock(cast(pthread_mutex_t*) &sbrkMutex) == 0
            || assert(0);
        return !_brkInitial || brk(_brkInitial) == 0;
    }

    /// Standard allocator API.
    Ternary empty() shared pure nothrow @safe @nogc
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
    import std.algorithm.comparison : min;
    alias alloc = SbrkRegion!(min(8, platformAlignment)).instance;
    assert((() nothrow @safe @nogc => alloc.empty)() == Ternary.yes);
    auto a = alloc.alignedAllocate(2001, 4096);
    assert(a.length == 2001);
    assert((() nothrow @safe @nogc => alloc.empty)() == Ternary.no);
    auto oldBrkCurr = alloc._brkCurrent;
    auto b = alloc.allocate(2001);
    assert(b.length == 2001);
    assert((() nothrow @safe @nogc => alloc.expand(b, 0))());
    assert(b.length == 2001);
    // Expand with a small size to fit the rounded slack due to alignment
    assert((() nothrow @safe @nogc => alloc.expand(b, 1))());
    assert(b.length == 2002);
    // Exceed the rounded slack due to alignment
    assert((() nothrow @safe @nogc => alloc.expand(b, 10))());
    assert(b.length == 2012);
    assert((() nothrow @safe @nogc => alloc.owns(a))() == Ternary.yes);
    assert((() nothrow @safe @nogc => alloc.owns(b))() == Ternary.yes);
    // reducing the brk does not work on OSX
    version (Darwin) {} else
    {
        assert((() nothrow @nogc => alloc.deallocate(b))());
        // Check that expand and deallocate work well
        assert(oldBrkCurr == alloc._brkCurrent);
        assert((() nothrow @nogc => alloc.deallocate(a))());
        assert((() nothrow @nogc => alloc.deallocateAll())());
    }
    const void[] c = alloc.allocate(2001);
    assert(c.length == 2001);
    assert((() nothrow @safe @nogc => alloc.owns(c))() == Ternary.yes);
    assert((() nothrow @safe @nogc => alloc.owns(null))() == Ternary.no);
}

/**
The threadsafe version of the `Region` allocator.
Allocations and deallocations are lock-free based using $(REF cas, core,atomic).
*/
shared struct SharedRegion(ParentAllocator,
    uint minAlign = platformAlignment,
    Flag!"growDownwards" growDownwards = No.growDownwards)
{
    static assert(minAlign.isGoodStaticAlignment);
    static assert(ParentAllocator.alignment >= minAlign);

    import std.traits : hasMember;
    import std.typecons : Ternary;

    // state
    /**
    The _parent allocator. Depending on whether `ParentAllocator` holds state
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
    private shared SharedBorrowedRegion!(minAlign, growDownwards) _impl;

    private void* roundedBegin() const pure nothrow @trusted @nogc
    {
        return _impl.roundedBegin;
    }

    private void* roundedEnd() const pure nothrow @trusted @nogc
    {
        return _impl.roundedEnd;
    }


    /**
    Constructs a region backed by a user-provided store.
    Assumes the memory was allocated with `ParentAllocator`.

    Params:
        store = User-provided store backing up the region. Assumed to have been
        allocated with `ParentAllocator`.
        n = Bytes to allocate using `ParentAllocator`. If `parent.allocate(n)`
        returns `null`, the region will be initialized as empty (correctly
        initialized but unable to allocate).
    */
    this(ubyte[] store) pure nothrow @nogc
    {
        _impl = store;
    }

    /// Ditto
    this(size_t n)
    {
        this(cast(ubyte[]) (parent.allocate(n.roundUpToAlignment(alignment))));
    }

    /**
    Rounds the given size to a multiple of the `alignment`
    */
    size_t goodAllocSize(size_t n) const pure nothrow @safe @nogc
    {
        return _impl.goodAllocSize(n);
    }

    /**
    Alignment offered.
    */
    alias alignment = minAlign;

    /**
    Allocates `n` bytes of memory. The allocation is served by atomically incrementing
    a pointer which keeps track of the current used space.

    Params:
        n = number of bytes to allocate

    Returns:
        A properly-aligned buffer of size `n`, or `null` if request could not
        be satisfied.
    */
    void[] allocate(size_t n) pure nothrow @trusted @nogc
    {
        return _impl.allocate(n);
    }

    /**
    Deallocates `b`. This works only if `b` was obtained as the last call
    to `allocate`; otherwise (i.e. another allocation has occurred since) it
    does nothing.

    Params:
        b = Block previously obtained by a call to `allocate` against this
        allocator (`null` is allowed).
    */
    bool deallocate(void[] b) pure nothrow @nogc
    {
        return _impl.deallocate(b);
    }

    /**
    Deallocates all memory allocated by this region, which can be subsequently
    reused for new allocations.
    */
    bool deallocateAll() pure nothrow @nogc
    {
        return _impl.deallocateAll;
    }

    /**
    Allocates `n` bytes of memory aligned at alignment `a`.
    Params:
        n = number of bytes to allocate
        a = alignment for the allocated block

    Returns:
        Either a suitable block of `n` bytes aligned at `a`, or `null`.
    */
    void[] alignedAllocate(size_t n, uint a) pure nothrow @trusted @nogc
    {
        return _impl.alignedAllocate(n, a);
    }

    /**
    Queries whether `b` has been allocated with this region.

    Params:
        b = Arbitrary block of memory (`null` is allowed; `owns(null)` returns
        `false`).

    Returns:
        `true` if `b` has been allocated with this region, `false` otherwise.
    */
    Ternary owns(const void[] b) const pure nothrow @trusted @nogc
    {
        return _impl.owns(b);
    }

    /**
    Returns `Ternary.yes` if no memory has been allocated in this region,
    `Ternary.no` otherwise. (Never returns `Ternary.unknown`.)
    */
    Ternary empty() const pure nothrow @safe @nogc
    {
        return _impl.empty;
    }

    /**
    If `ParentAllocator` defines `deallocate`, the region defines a destructor
    that uses `ParentAllocator.deallocate` to free the memory chunk.
    */
    static if (hasMember!(ParentAllocator, "deallocate"))
    ~this()
    {
        with (_impl) parent.deallocate(cast(void[]) _begin[0 .. _end - _begin]);
    }
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;

    static void testAlloc(Allocator)(ref Allocator a, bool growDownwards)
    {
        import core.thread : ThreadGroup;
        import std.algorithm.sorting : sort;
        import core.internal.spinlock : SpinLock;

        SpinLock lock = SpinLock(SpinLock.Contention.brief);
        enum numThreads = 100;
        void[][numThreads] buf;
        size_t count = 0;

        void fun()
        {
            void[] b = a.allocate(63);
            assert(b.length == 63);

            lock.lock();
            buf[count] = b;
            count++;
            lock.unlock();
        }

        auto tg = new ThreadGroup;
        foreach (i; 0 .. numThreads)
        {
            tg.create(&fun);
        }
        tg.joinAll();

        sort!((a, b) => a.ptr < b.ptr)(buf[0 .. numThreads]);
        foreach (i; 0 .. numThreads - 1)
        {
            assert(buf[i].ptr + a.goodAllocSize(buf[i].length) == buf[i + 1].ptr);
        }

        assert(!a.deallocate(buf[1]));

        foreach (i; 0 .. numThreads)
        {
            if (!growDownwards)
                assert(a.deallocate(buf[numThreads - 1 - i]));
            else
                assert(a.deallocate(buf[i]));
        }

        assert(a.deallocateAll());
        void[] b = a.allocate(63);
        assert(b.length == 63);
        assert(a.deallocate(b));
    }

    auto a1 = SharedRegion!(Mallocator, Mallocator.alignment,
        Yes.growDownwards)(1024 * 64);

    auto a2 = SharedRegion!(Mallocator, Mallocator.alignment,
        No.growDownwards)(1024 * 64);

    testAlloc(a1, true);
    testAlloc(a2, false);
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;

    static void testAlloc(Allocator)(ref Allocator a, bool growDownwards)
    {
        import core.thread : ThreadGroup;
        import std.algorithm.sorting : sort;
        import core.internal.spinlock : SpinLock;

        SpinLock lock = SpinLock(SpinLock.Contention.brief);
        enum numThreads = 100;
        void[][2 * numThreads] buf;
        size_t count = 0;

        void fun()
        {
            void[] b = a.allocate(63);
            assert(b.length == 63);

            lock.lock();
            buf[count] = b;
            count++;
            lock.unlock();

            b = a.alignedAllocate(63, 32);
            assert(b.length == 63);
            assert(cast(size_t) b.ptr % 32 == 0);

            lock.lock();
            buf[count] = b;
            count++;
            lock.unlock();
        }

        auto tg = new ThreadGroup;
        foreach (i; 0 .. numThreads)
        {
            tg.create(&fun);
        }
        tg.joinAll();

        sort!((a, b) => a.ptr < b.ptr)(buf[0 .. 2 * numThreads]);
        foreach (i; 0 .. 2 * numThreads - 1)
        {
            assert(buf[i].ptr + buf[i].length <= buf[i + 1].ptr);
        }

        assert(!a.deallocate(buf[1]));
        assert(a.deallocateAll());

        void[] b = a.allocate(13);
        assert(b.length == 13);
        assert(a.deallocate(b));
    }

    auto a1 = SharedRegion!(Mallocator, Mallocator.alignment,
        Yes.growDownwards)(1024 * 64);

    auto a2 = SharedRegion!(Mallocator, Mallocator.alignment,
        No.growDownwards)(1024 * 64);

    testAlloc(a1, true);
    testAlloc(a2, false);
}

/**
A `SharedBorrowedRegion` allocates directly from a user-provided block of memory.

Unlike a `SharedRegion`, a `SharedBorrowedRegion` does not own the memory it
allocates from and will not deallocate that memory upon destruction. Instead,
it is the user's responsibility to ensure that the memory is properly disposed
of.

In all other respects, a `SharedBorrowedRegion` behaves exactly like a `SharedRegion`.
*/
shared struct SharedBorrowedRegion(uint minAlign = platformAlignment,
    Flag!"growDownwards" growDownwards = No.growDownwards)
{
    static assert(minAlign.isGoodStaticAlignment);

    import std.typecons : Ternary;

    // state
    private void* _current, _begin, _end;

    private void* roundedBegin() shared const pure nothrow @trusted @nogc
    {
        return cast(void*) roundUpToAlignment(cast(size_t) _begin, alignment);
    }

    private void* roundedEnd() shared const pure nothrow @trusted @nogc
    {
        return cast(void*) roundDownToAlignment(cast(size_t) _end, alignment);
    }

    /**
    Constructs a region backed by a user-provided store.

    Params:
        store = User-provided store backing up the region. Must not be aliased.
    */
    this(ubyte[] store) shared pure nothrow @nogc
    {
        _begin = cast(typeof(_begin)) store.ptr;
        _end = cast(typeof(_end)) (store.ptr + store.length);
        static if (growDownwards)
            _current = cast(typeof(_current)) roundedEnd();
        else
            _current = cast(typeof(_current)) roundedBegin();
    }

    /*
    TODO: The postblit of `SharedBorrowedRegion` should be disabled because
    such objects should not be copied around naively.
    */

    /**
    Rounds the given size to a multiple of the `alignment`
    */
    size_t goodAllocSize(size_t n) shared const pure nothrow @safe @nogc
    {
        return n.roundUpToAlignment(alignment);
    }

    /**
    Alignment offered.
    */
    alias alignment = minAlign;

    /**
    Allocates `n` bytes of memory. The allocation is served by atomically incrementing
    a pointer which keeps track of the current used space.

    Params:
        n = number of bytes to allocate

    Returns:
        A properly-aligned buffer of size `n`, or `null` if request could not
        be satisfied.
    */
    void[] allocate(size_t n) shared pure nothrow @trusted @nogc
    {
        import core.atomic : cas, atomicLoad;

        if (n == 0) return null;
        const rounded = goodAllocSize(n);

        shared void* localCurrent, localNewCurrent;
        static if (growDownwards)
        {
            do
            {
                localCurrent = atomicLoad(_current);
                localNewCurrent = localCurrent - rounded;
                if (localNewCurrent > localCurrent || localNewCurrent < _begin)
                    return null;
            } while (!cas(&_current, localCurrent, localNewCurrent));

            return cast(void[]) localNewCurrent[0 .. n];
        }
        else
        {
            do
            {
                localCurrent = atomicLoad(_current);
                localNewCurrent = localCurrent + rounded;
                if (localNewCurrent < localCurrent || localNewCurrent > _end)
                    return null;
            } while (!cas(&_current, localCurrent, localNewCurrent));

            return cast(void[]) localCurrent[0 .. n];
        }

        assert(0, "Unexpected error in SharedBorrowedRegion.allocate");
    }

    /**
    Allocates `n` bytes of memory aligned at alignment `a`.

    Params:
        n = number of bytes to allocate
        a = alignment for the allocated block

    Returns:
        Either a suitable block of `n` bytes aligned at `a`, or `null`.
    */
    void[] alignedAllocate(size_t n, uint a) shared pure nothrow @trusted @nogc
    {
        import core.atomic : cas, atomicLoad;
        import std.math.traits : isPowerOf2;

        assert(a.isPowerOf2);
        if (n == 0) return null;

        const rounded = goodAllocSize(n);
        shared void* localCurrent, localNewCurrent;

        static if (growDownwards)
        {
            do
            {
                localCurrent = atomicLoad(_current);
                auto alignedCurrent = cast(void*)(localCurrent - rounded);
                localNewCurrent = cast(shared(void*)) alignedCurrent.alignDownTo(a);
                if (alignedCurrent > localCurrent || localNewCurrent > alignedCurrent ||
                    localNewCurrent < _begin)
                    return null;
            } while (!cas(&_current, localCurrent, localNewCurrent));

            return cast(void[]) localNewCurrent[0 .. n];
        }
        else
        {
            do
            {
                localCurrent = atomicLoad(_current);
                auto alignedCurrent = alignUpTo(cast(void*) localCurrent, a);
                localNewCurrent = cast(shared(void*)) (alignedCurrent + rounded);
                if (alignedCurrent < localCurrent || localNewCurrent < alignedCurrent ||
                    localNewCurrent > _end)
                    return null;
            } while (!cas(&_current, localCurrent, localNewCurrent));

            return cast(void[]) (localNewCurrent - rounded)[0 .. n];
        }

        assert(0, "Unexpected error in SharedBorrowedRegion.alignedAllocate");
    }

    /**
    Deallocates `b`. This works only if `b` was obtained as the last call
    to `allocate`; otherwise (i.e. another allocation has occurred since) it
    does nothing.

    Params:
        b = Block previously obtained by a call to `allocate` against this
        allocator (`null` is allowed).
    */
    bool deallocate(void[] b) shared pure nothrow @nogc
    {
        import core.atomic : cas, atomicLoad;

        const rounded = goodAllocSize(b.length);
        shared void* localCurrent, localNewCurrent;

        // The cas is done only once, because only the last allocation can be reverted
        localCurrent = atomicLoad(_current);
        static if (growDownwards)
        {
            localNewCurrent = localCurrent + rounded;
            if (b.ptr == localCurrent)
                return cas(&_current, localCurrent, localNewCurrent);
        }
        else
        {
            localNewCurrent = localCurrent - rounded;
            if (b.ptr == localNewCurrent)
                return cas(&_current, localCurrent, localNewCurrent);
        }

        return false;
    }

    /**
    Deallocates all memory allocated by this region, which can be subsequently
    reused for new allocations.
    */
    bool deallocateAll() shared pure nothrow @nogc
    {
        import core.atomic : atomicStore;
        static if (growDownwards)
        {
            atomicStore(_current, cast(shared(void*)) roundedEnd());
        }
        else
        {
            atomicStore(_current, cast(shared(void*)) roundedBegin());
        }
        return true;
    }

    /**
    Queries whether `b` has been allocated with this region.

    Params:
        b = Arbitrary block of memory (`null` is allowed; `owns(null)` returns
        `false`).

    Returns:
        `true` if `b` has been allocated with this region, `false` otherwise.
    */
    Ternary owns(const void[] b) shared const pure nothrow @trusted @nogc
    {
        return Ternary(b && (&b[0] >= _begin) && (&b[0] + b.length <= _end));
    }

    /**
    Returns `Ternary.yes` if no memory has been allocated in this region,
    `Ternary.no` otherwise. (Never returns `Ternary.unknown`.)
    */
    Ternary empty() shared const pure nothrow @safe @nogc
    {
        import core.atomic : atomicLoad;

        auto localCurrent = atomicLoad(_current);
        static if (growDownwards)
            return Ternary(localCurrent == roundedEnd());
        else
            return Ternary(localCurrent == roundedBegin());
    }
}
