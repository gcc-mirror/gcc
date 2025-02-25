@system unittest
{
    import std.experimental.allocator.building_blocks.segregator;

    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;
    alias A =
        Segregator!(
            1024 * 4,
            Segregator!(
                128, FreeList!(Mallocator, 0, 128),
                GCAllocator),
            Segregator!(
                1024 * 1024, Mallocator,
                GCAllocator)
            );
    A a;
    auto b = a.allocate(200);
    assert(b.length == 200);
    a.deallocate(b);
}

@system unittest
{
    import std.experimental.allocator.building_blocks.segregator;

    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;
    alias A =
        Segregator!(
            128, FreeList!(Mallocator, 0, 128),
            1024 * 4, GCAllocator,
            1024 * 1024, Mallocator,
            GCAllocator
        );
    A a;
    auto b = a.allocate(201);
    assert(b.length == 201);
    a.deallocate(b);
}

