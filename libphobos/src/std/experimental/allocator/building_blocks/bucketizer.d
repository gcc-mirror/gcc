// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/bucketizer.d)
*/
module std.experimental.allocator.building_blocks.bucketizer;

/**

A `Bucketizer` uses distinct allocators for handling allocations of sizes in
the intervals $(D [min, min + step - 1]), $(D [min + step, min + 2 * step - 1]),
$(D [min + 2 * step, min + 3 * step - 1]), `...`, $(D [max - step + 1, max]).

`Bucketizer` holds a fixed-size array of allocators and dispatches calls to
them appropriately. The size of the array is $(D (max + 1 - min) / step), which
must be an exact division.

Allocations for sizes smaller than `min` or larger than `max` are illegal
for `Bucketizer`. To handle them separately, `Segregator` may be of use.

*/
struct Bucketizer(Allocator, size_t min, size_t max, size_t step)
{
    import common = std.experimental.allocator.common : roundUpToMultipleOf,
           alignedAt;
    import std.traits : hasMember;
    import std.typecons : Ternary;

    static assert((max - (min - 1)) % step == 0,
        "Invalid limits when instantiating " ~ Bucketizer.stringof);

    // state
    /**
    The array of allocators is publicly available for e.g. initialization and
    inspection.
    */
    Allocator[(max + 1 - min) / step] buckets;

    pure nothrow @safe @nogc
    private Allocator* allocatorFor(size_t n)
    {
        const i = (n - min) / step;
        return i < buckets.length ? &buckets[i] : null;
    }

    /**
    The alignment offered is the same as `Allocator.alignment`.
    */
    enum uint alignment = Allocator.alignment;

    /**
    Rounds up to the maximum size of the bucket in which `bytes` falls.
    */
    pure nothrow @safe @nogc
    size_t goodAllocSize(size_t bytes) const
    {
        // round up bytes such that bytes - min + 1 is a multiple of step
        assert(bytes >= min);
        const min_1 = min - 1;
        return min_1 + roundUpToMultipleOf(bytes - min_1, step);
    }

    /**
    Directs the call to either one of the `buckets` allocators.
    */
    void[] allocate(size_t bytes)
    {
        if (!bytes) return null;
        if (auto a = allocatorFor(bytes))
        {
            const actual = goodAllocSize(bytes);
            auto result = a.allocate(actual);
            return result.ptr ? result.ptr[0 .. bytes] : null;
        }
        return null;
    }

    static if (hasMember!(Allocator, "allocateZeroed"))
    package(std) void[] allocateZeroed()(size_t bytes)
    {
        if (!bytes) return null;
        if (auto a = allocatorFor(bytes))
        {
            const actual = goodAllocSize(bytes);
            auto result = a.allocateZeroed(actual);
            return result.ptr ? result.ptr[0 .. bytes] : null;
        }
        return null;
    }

    /**
    Allocates the requested `bytes` of memory with specified `alignment`.
    Directs the call to either one of the `buckets` allocators. Defined only
    if `Allocator` defines `alignedAllocate`.
    */
    static if (hasMember!(Allocator, "alignedAllocate"))
    void[] alignedAllocate(size_t bytes, uint alignment)
    {
        if (!bytes) return null;
        if (auto a = allocatorFor(bytes))
        {
            const actual = goodAllocSize(bytes);
            auto result = a.alignedAllocate(actual, alignment);
            return result !is null ? (() @trusted => (&result[0])[0 .. bytes])() : null;
        }
        return null;
    }

    /**
    This method allows expansion within the respective bucket range. It succeeds
    if both `b.length` and $(D b.length + delta) fall in a range of the form
    $(D [min + k * step, min + (k + 1) * step - 1]).
    */
    bool expand(ref void[] b, size_t delta)
    {
        if (!b || delta == 0) return delta == 0;
        assert(b.length >= min && b.length <= max);
        const available = goodAllocSize(b.length);
        const desired = b.length + delta;
        if (available < desired) return false;
        b = (() @trusted => b.ptr[0 .. desired])();
        return true;
    }

    /**
    This method allows reallocation within the respective bucket range. If both
    `b.length` and `size` fall in a range of the form $(D [min + k *
    step, min + (k + 1) * step - 1]), then reallocation is in place. Otherwise,
    reallocation with moving is attempted.
    */
    bool reallocate(ref void[] b, size_t size)
    {
        if (size == 0)
        {
            deallocate(b);
            b = null;
            return true;
        }
        if (size >= b.length && expand(b, size - b.length))
        {
            return true;
        }
        assert(b.length >= min && b.length <= max);
        if (goodAllocSize(size) == goodAllocSize(b.length))
        {
            b = b.ptr[0 .. size];
            return true;
        }
        // Move cross buckets
        return common.reallocate(this, b, size);
    }

    /**
    Similar to `reallocate`, with alignment. Defined only if `Allocator`
    defines `alignedReallocate`.
    */
    static if (hasMember!(Allocator, "alignedReallocate"))
    bool alignedReallocate(ref void[] b, size_t size, uint a)
    {
        if (size == 0)
        {
            deallocate(b);
            b = null;
            return true;
        }
        if (size >= b.length && b.ptr.alignedAt(a) && expand(b, size - b.length))
        {
            return true;
        }
        assert(b.length >= min && b.length <= max);
        if (goodAllocSize(size) == goodAllocSize(b.length) && b.ptr.alignedAt(a))
        {
            b = b.ptr[0 .. size];
            return true;
        }
        // Move cross buckets
        return common.alignedReallocate(this, b, size, a);
    }

    /**
    Defined only if `Allocator` defines `owns`. Finds the owner of `b` and forwards the call to it.
    */
    static if (hasMember!(Allocator, "owns"))
    Ternary owns(void[] b)
    {
        if (!b.ptr) return Ternary.no;
        if (auto a = allocatorFor(b.length))
        {
            const actual = goodAllocSize(b.length);
            return a.owns(b.ptr[0 .. actual]);
        }
        return Ternary.no;
    }

    /**
    This method is only defined if `Allocator` defines `deallocate`.
    */
    static if (hasMember!(Allocator, "deallocate"))
    bool deallocate(void[] b)
    {
        if (!b.ptr) return true;
        if (auto a = allocatorFor(b.length))
        {
            a.deallocate(b.ptr[0 .. goodAllocSize(b.length)]);
        }
        return true;
    }

    /**
    This method is only defined if all allocators involved define $(D
    deallocateAll), and calls it for each bucket in turn. Returns `true` if all
    allocators could deallocate all.
    */
    static if (hasMember!(Allocator, "deallocateAll"))
    bool deallocateAll()
    {
        bool result = true;
        foreach (ref a; buckets)
        {
            if (!a.deallocateAll()) result = false;
        }
        return result;
    }

    /**
    This method is only defined if all allocators involved define $(D
    resolveInternalPointer), and tries it for each bucket in turn.
    */
    static if (hasMember!(Allocator, "resolveInternalPointer"))
    Ternary resolveInternalPointer(const void* p, ref void[] result)
    {
        foreach (ref a; buckets)
        {
            Ternary r = a.resolveInternalPointer(p, result);
            if (r == Ternary.yes) return r;
        }
        return Ternary.no;
    }
}

///
@system unittest
{
    import std.algorithm.comparison : max;
    import std.experimental.allocator.building_blocks.allocator_list : AllocatorList;
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.building_blocks.region : Region;
    import std.experimental.allocator.common : unbounded;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Ternary;
    Bucketizer!(
        FreeList!(
            AllocatorList!(
                (size_t n) => Region!Mallocator(max(n, 1024 * 1024))),
            0, unbounded),
        65, 512, 64) a;
    auto b = a.allocate(400);
    assert(b.length == 400);
    assert(a.owns(b) == Ternary.yes);
    a.deallocate(b);
}

@system unittest
{
    import std.algorithm.comparison : max;
    import std.experimental.allocator.building_blocks.allocator_list : AllocatorList;
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.building_blocks.region : Region;
    import std.experimental.allocator.common : unbounded;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Ternary;

    Bucketizer!(
        FreeList!(
            AllocatorList!(
                (size_t n) => Region!Mallocator(max(n, 1024 * 1024)), Mallocator),
            0, unbounded),
        65, 512, 64) a;

    assert((() pure nothrow @safe @nogc => a.goodAllocSize(65))() == 128);

    auto b = a.allocate(100);
    assert(b.length == 100);
    // Make reallocate use extend
    assert((() nothrow @nogc => a.reallocate(b, 101))());
    assert(b.length == 101);
    // Move cross buckets
    assert((() nothrow @nogc => a.reallocate(b, 200))());
    assert(b.length == 200);
    // Free through realloc
    assert((() nothrow @nogc => a.reallocate(b, 0))());
    assert(b is null);
    // Ensure deallocate inherits from parent allocators
    assert((() nothrow @nogc => a.deallocate(b))());
    assert((() nothrow @nogc => a.deallocateAll())());
}

// Test alignedAllocate
@system unittest
{
    import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlock;
    import std.experimental.allocator.gc_allocator : GCAllocator;

    Bucketizer!(BitmappedBlock!(64, 8, GCAllocator), 65, 512, 64) a;
    foreach (ref bucket; a.buckets)
    {
        bucket = BitmappedBlock!(64, 8, GCAllocator)(new ubyte[1024]);
    }

    auto b = a.alignedAllocate(100, 16);
    assert(b.length == 100);
    assert(a.alignedAllocate(42, 16) is null);
    assert(a.alignedAllocate(0, 16) is null);
    assert((() pure nothrow @safe @nogc => a.expand(b, 0))());
    assert(b.length == 100);
    assert((() pure nothrow @safe @nogc => a.expand(b, 28))());
    assert(b.length == 128);
    assert((() pure nothrow @safe @nogc => !a.expand(b, 1))());
}

@system unittest
{
    import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlock;
    import std.experimental.allocator.gc_allocator : GCAllocator;

    Bucketizer!(BitmappedBlock!(64, 8, GCAllocator), 1, 512, 64) a;
    foreach (ref bucket; a.buckets)
    {
        bucket = BitmappedBlock!(64, 8, GCAllocator)(new ubyte[1024]);
    }

    auto b = a.alignedAllocate(1, 4);
    assert(b.length == 1);
    // Make reallocate use extend
    assert(a.alignedReallocate(b, 11, 4));
    assert(b.length == 11);
    // Make reallocate use use realloc because of alignment change
    assert(a.alignedReallocate(b, 21, 16));
    assert(b.length == 21);
    // Make reallocate use extend
    assert(a.alignedReallocate(b, 22, 16));
    assert(b.length == 22);
    // Move cross buckets
    assert(a.alignedReallocate(b, 101, 16));
    assert(b.length == 101);
    // Free through realloc
    assert(a.alignedReallocate(b, 0, 16));
    assert(b is null);
}
