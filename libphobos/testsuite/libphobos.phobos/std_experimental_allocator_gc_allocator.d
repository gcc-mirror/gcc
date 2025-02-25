pure @system unittest
{
    import std.experimental.allocator.gc_allocator;

    auto buffer = GCAllocator.instance.allocate(1024 * 1024 * 4);
    // deallocate upon scope's end (alternatively: leave it to collection)
    scope(exit) GCAllocator.instance.deallocate(buffer);
    //...
}

