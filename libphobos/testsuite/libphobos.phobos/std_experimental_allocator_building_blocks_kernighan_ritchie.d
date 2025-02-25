@system unittest
{
    import std.experimental.allocator.building_blocks.kernighan_ritchie;

        import std.experimental.allocator.gc_allocator : GCAllocator;
        auto alloc = KRRegion!GCAllocator(1024 * 64);
        const b1 = alloc.allocate(2048);
        assert(b1.length == 2048);
        const b2 = alloc.allocateAll;
        assert(b2.length == 1024 * 62);
    
}

@system unittest
{
    import std.experimental.allocator.building_blocks.kernighan_ritchie;

    import std.experimental.allocator.building_blocks.fallback_allocator
        : fallbackAllocator;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.typecons : Ternary;
    // KRRegion fronting a general-purpose allocator
    align(KRRegion!().alignment) ubyte[1024 * 128] buf;
    auto alloc = fallbackAllocator(KRRegion!()(buf), GCAllocator.instance);
    auto b = alloc.allocate(100);
    assert(b.length == 100);
    assert((() pure nothrow @safe @nogc => alloc.primary.owns(b))() == Ternary.yes);
}

@system unittest
{
    import std.experimental.allocator.building_blocks.kernighan_ritchie;

    import std.algorithm.comparison : max;
    import std.experimental.allocator.building_blocks.allocator_list
        : AllocatorList;
    import std.experimental.allocator.mmap_allocator : MmapAllocator;
    AllocatorList!(n => KRRegion!MmapAllocator(max(n * 16, 1024 * 1024))) alloc;
}

