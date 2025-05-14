@system unittest
{
    import std.experimental.allocator.building_blocks.allocator_list;

    import std.algorithm.comparison : max;
    import std.experimental.allocator.building_blocks.free_list : ContiguousFreeList;
    import std.experimental.allocator.building_blocks.null_allocator : NullAllocator;
    import std.experimental.allocator.building_blocks.region : Region;
    import std.experimental.allocator.building_blocks.segregator : Segregator;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mmap_allocator : MmapAllocator;

    // Ouroboros allocator list based upon 4MB regions, fetched directly from
    // mmap. All memory is released upon destruction.
    alias A1 = AllocatorList!((n) => Region!MmapAllocator(max(n, 1024 * 4096)),
        NullAllocator);

    // Allocator list based upon 4MB regions, fetched from the garbage
    // collector. All memory is released upon destruction.
    alias A2 = AllocatorList!((n) => Region!GCAllocator(max(n, 1024 * 4096)));

    // Ouroboros allocator list based upon 4MB regions, fetched from the garbage
    // collector. Memory is left to the collector.
    alias A3 = AllocatorList!(
        (n) => Region!NullAllocator(new ubyte[max(n, 1024 * 4096)]),
        NullAllocator);

    // Allocator list that creates one freelist for all objects
    alias A4 =
        Segregator!(
            64, AllocatorList!(
                (n) => ContiguousFreeList!(NullAllocator, 0, 64)(
                    cast(ubyte[])(GCAllocator.instance.allocate(4096)))),
            GCAllocator);

    A4 a;
    auto small = a.allocate(64);
    assert(small);
    a.deallocate(small);
    auto b1 = a.allocate(1024 * 8192);
    assert(b1 !is null); // still works due to overdimensioning
    b1 = a.allocate(1024 * 10);
    assert(b1.length == 1024 * 10);
}

