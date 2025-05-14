@system @nogc nothrow unittest
{
    import std.experimental.allocator.building_blocks.ascending_page_allocator;

    import core.memory : pageSize;

    size_t numPages = 100;
    void[] buf;
    void[] prevBuf = null;
    AscendingPageAllocator a = AscendingPageAllocator(numPages * pageSize);

    foreach (i; 0 .. numPages)
    {
        // Allocation is rounded up to page size
        buf = a.allocate(pageSize - 100);
        assert(buf.length == pageSize - 100);

        // Allocations are served at increasing addresses
        if (prevBuf)
            assert(prevBuf.ptr + pageSize == buf.ptr);

        assert(a.deallocate(buf));
        prevBuf = buf;
    }
}

@system unittest
{
    import std.experimental.allocator.building_blocks.ascending_page_allocator;

    import core.memory : pageSize;
    import core.thread : ThreadGroup;

    enum numThreads = 100;
    shared SharedAscendingPageAllocator a = SharedAscendingPageAllocator(pageSize * numThreads);

    void fun()
    {
        void[] b = a.allocate(pageSize);
        assert(b.length == pageSize);

        assert(a.deallocate(b));
    }

    auto tg = new ThreadGroup;
    foreach (i; 0 .. numThreads)
    {
        tg.create(&fun);
    }
    tg.joinAll();
}

