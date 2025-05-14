@system nothrow unittest
{
    import std.experimental.allocator.building_blocks.region;

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
    import std.experimental.allocator.building_blocks.region;

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

@system unittest
{
    import std.experimental.allocator.building_blocks.region;

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

