/**

Collection of typical and useful prebuilt allocators using the given
components. User code would typically import this module and use its
facilities, or import individual heap building blocks and assemble them.

*/
module std.experimental.allocator.showcase;

import std.experimental.allocator.building_blocks.fallback_allocator,
    std.experimental.allocator.gc_allocator,
    std.experimental.allocator.building_blocks.region;
import std.traits : hasMember;

/**

Allocator that uses stack allocation for up to $(D stackSize) bytes and
then falls back to $(D Allocator). Defined as:

----
alias StackFront(size_t stackSize, Allocator) =
    FallbackAllocator!(
        InSituRegion!(stackSize, Allocator.alignment,
            hasMember!(Allocator, "deallocate")
                ? Yes.defineDeallocate
                : No.defineDeallocate),
        Allocator);
----

Choosing `stackSize` is as always a compromise. Too small a size exhausts the
stack storage after a few allocations, after which there are no gains over the
backup allocator. Too large a size increases the stack consumed by the thread
and may end up worse off because it explores cold portions of the stack.

*/
alias StackFront(size_t stackSize, Allocator = GCAllocator) =
    FallbackAllocator!(
        InSituRegion!(stackSize, Allocator.alignment),
        Allocator);

///
@system unittest
{
    StackFront!4096 a;
    auto b = a.allocate(4000);
    assert(b.length == 4000);
    auto c = a.allocate(4000);
    assert(c.length == 4000);
    a.deallocate(b);
    a.deallocate(c);
}

/**
Creates a scalable `AllocatorList` of `Regions`, each having at least
`bytesPerRegion` bytes. Allocation is very fast. This allocator does not offer
`deallocate` but does free all regions in its destructor. It is recommended for
short-lived batch applications that count on never running out of memory.
*/
auto mmapRegionList(size_t bytesPerRegion)
{
    static struct Factory
    {
        size_t bytesPerRegion;
        import std.algorithm.comparison : max;
        import std.experimental.allocator.building_blocks.region
            : Region;
        import std.experimental.allocator.mmap_allocator
            : MmapAllocator;
        this(size_t n)
        {
            bytesPerRegion = n;
        }
        auto opCall(size_t n)
        {
            return Region!MmapAllocator(max(n, bytesPerRegion));
        }
    }
    import std.experimental.allocator.building_blocks.allocator_list
        : AllocatorList;
    import std.experimental.allocator.building_blocks.null_allocator
        : NullAllocator;
    auto shop = Factory(bytesPerRegion);
    return AllocatorList!(Factory, NullAllocator)(shop);
}

///
@system unittest
{
    auto alloc = mmapRegionList(1024 * 1024);
    const b = alloc.allocate(100);
    assert(b.length == 100);
}
