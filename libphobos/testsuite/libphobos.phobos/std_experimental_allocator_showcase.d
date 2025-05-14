@system unittest
{
    import std.experimental.allocator.showcase;

    StackFront!4096 a;
    auto b = a.allocate(4000);
    assert(b.length == 4000);
    auto c = a.allocate(4000);
    assert(c.length == 4000);
    a.deallocate(b);
    a.deallocate(c);
}

@system unittest
{
    import std.experimental.allocator.showcase;

    auto alloc = mmapRegionList(1024 * 1024);
    const b = alloc.allocate(100);
    assert(b.length == 100);
}

