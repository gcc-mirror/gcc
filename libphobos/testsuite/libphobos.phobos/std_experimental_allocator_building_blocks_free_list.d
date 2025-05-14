@safe unittest
{
    import std.experimental.allocator.building_blocks.free_list;

    import std.experimental.allocator.building_blocks.allocator_list
        : AllocatorList;
    import std.experimental.allocator.gc_allocator : GCAllocator;

    import std.experimental.allocator.common : unbounded;

    alias ScalableFreeList = AllocatorList!((n) =>
        ContiguousFreeList!(GCAllocator, 0, unbounded)(4096)
    );
}

@safe unittest
{
    import std.experimental.allocator.building_blocks.free_list;

    import std.experimental.allocator.common : chooseAtRuntime;
    import std.experimental.allocator.mallocator : Mallocator;

    shared SharedFreeList!(Mallocator, chooseAtRuntime, chooseAtRuntime) a;
    a.setBounds(64, 128);
    assert(a.max == 128);
    assert(a.min == 64);
}

@safe unittest
{
    import std.experimental.allocator.building_blocks.free_list;

    import std.experimental.allocator.common : chooseAtRuntime;
    import std.experimental.allocator.mallocator : Mallocator;

    shared SharedFreeList!(Mallocator, 50, 50, chooseAtRuntime) a;
    // Set the maxSize first so setting the minSize doesn't throw
    a.approxMaxLength = 128;
    assert(a.approxMaxLength  == 128);
    a.approxMaxLength = 1024;
    assert(a.approxMaxLength  == 1024);
    a.approxMaxLength = 1;
    assert(a.approxMaxLength  == 1);
}

