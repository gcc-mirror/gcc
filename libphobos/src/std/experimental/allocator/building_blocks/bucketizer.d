///
module std.experimental.allocator.building_blocks.bucketizer;

/**

A $(D Bucketizer) uses distinct allocators for handling allocations of sizes in
the intervals $(D [min, min + step - 1]), $(D [min + step, min + 2 * step - 1]),
$(D [min + 2 * step, min + 3 * step - 1]), $(D ...), $(D [max - step + 1, max]).

$(D Bucketizer) holds a fixed-size array of allocators and dispatches calls to
them appropriately. The size of the array is $(D (max + 1 - min) / step), which
must be an exact division.

Allocations for sizes smaller than $(D min) or larger than $(D max) are illegal
for $(D Bucketizer). To handle them separately, $(D Segregator) may be of use.

*/
struct Bucketizer(Allocator, size_t min, size_t max, size_t step)
{
    import common = std.experimental.allocator.common : roundUpToMultipleOf;
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

    private Allocator* allocatorFor(size_t n)
    {
        const i = (n - min) / step;
        return i < buckets.length ? buckets.ptr + i : null;
    }

    /**
    The alignment offered is the same as $(D Allocator.alignment).
    */
    enum uint alignment = Allocator.alignment;

    /**
    Rounds up to the maximum size of the bucket in which $(D bytes) falls.
    */
    size_t goodAllocSize(size_t bytes) const
    {
        // round up bytes such that bytes - min + 1 is a multiple of step
        assert(bytes >= min);
        const min_1 = min - 1;
        return min_1 + roundUpToMultipleOf(bytes - min_1, step);
    }

    /**
    Directs the call to either one of the $(D buckets) allocators.
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

    /**
    Directs the call to either one of the $(D buckets) allocators. Defined only
    if `Allocator` defines `alignedAllocate`.
    */
    static if (hasMember!(Allocator, "alignedAllocate"))
    void[] alignedAllocate(size_t bytes, uint a)
    {
        if (!bytes) return null;
        if (auto a = allocatorFor(b.length))
        {
            const actual = goodAllocSize(bytes);
            auto result = a.alignedAllocate(actual);
            return result.ptr ? result.ptr[0 .. bytes] : null;
        }
        return null;
    }

    /**
    This method allows expansion within the respective bucket range. It succeeds
    if both $(D b.length) and $(D b.length + delta) fall in a range of the form
    $(D [min + k * step, min + (k + 1) * step - 1]).
    */
    bool expand(ref void[] b, size_t delta)
    {
        if (!b.ptr) return delta == 0;
        assert(b.length >= min && b.length <= max);
        const available = goodAllocSize(b.length);
        const desired = b.length + delta;
        if (available < desired) return false;
        b = b.ptr[0 .. desired];
        return true;
    }

    /**
    This method allows reallocation within the respective bucket range. If both
    $(D b.length) and $(D size) fall in a range of the form $(D [min + k *
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
        if (size >= b.length)
        {
            return expand(b, size - b.length);
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
        if (size >= b.length)
        {
            return expand(b, size - b.length);
        }
        assert(b.length >= min && b.length <= max);
        if (goodAllocSize(size) == goodAllocSize(b.length))
        {
            b = b.ptr[0 .. size];
            return true;
        }
        // Move cross buckets
        return .alignedReallocate(this, b, size, a);
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
    This method is only defined if $(D Allocator) defines $(D deallocate).
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
    void[] p;
    a.deallocate(b);
}
