@nogc @system nothrow unittest
{
    import std.experimental.allocator.mallocator;

    auto buffer = Mallocator.instance.allocate(1024 * 1024 * 4);
    scope(exit) Mallocator.instance.deallocate(buffer);
    //...
}

pure @nogc @system nothrow unittest
{
    import std.experimental.allocator.mallocator;

    auto buffer = AlignedMallocator.instance.alignedAllocate(1024 * 1024 * 4,
        128);
    scope(exit) AlignedMallocator.instance.deallocate(buffer);
    //...
}

