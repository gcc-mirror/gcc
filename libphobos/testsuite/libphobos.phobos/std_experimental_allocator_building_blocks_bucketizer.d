@system unittest
{
    import std.experimental.allocator.building_blocks.bucketizer;

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

