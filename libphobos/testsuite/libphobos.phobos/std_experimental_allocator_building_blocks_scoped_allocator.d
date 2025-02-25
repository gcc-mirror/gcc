@system unittest
{
    import std.experimental.allocator.building_blocks.scoped_allocator;

    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Ternary;
    ScopedAllocator!Mallocator alloc;
    assert(alloc.empty == Ternary.yes);
    const b = alloc.allocate(10);
    assert(b.length == 10);
    assert(alloc.empty == Ternary.no);
}

