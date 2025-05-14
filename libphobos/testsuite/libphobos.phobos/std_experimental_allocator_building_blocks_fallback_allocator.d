@system unittest
{
    import std.experimental.allocator.building_blocks.fallback_allocator;

    import std.experimental.allocator.building_blocks.region : Region;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.typecons : Ternary;
    auto a = fallbackAllocator(Region!GCAllocator(1024), GCAllocator.instance);
    auto b1 = a.allocate(1020);
    assert(b1.length == 1020);
    assert(a.primary.owns(b1) == Ternary.yes);
    auto b2 = a.allocate(10);
    assert(b2.length == 10);
    assert(a.primary.owns(b2) == Ternary.no);
}

