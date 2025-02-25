@system unittest
{
    import std.experimental.allocator.building_blocks.bitmapped_block;

    // Create a block allocator on top of a 10KB stack region.
    import std.experimental.allocator.building_blocks.region : InSituRegion;
    import std.traits : hasMember;
    InSituRegion!(10_240, 64) r;
    auto a = BitmappedBlock!(64, 64)(cast(ubyte[])(r.allocateAll()));
    static assert(hasMember!(InSituRegion!(10_240, 64), "allocateAll"));
    const b = a.allocate(100);
    assert(b.length == 100);
}

@system unittest
{
    import std.experimental.allocator.building_blocks.bitmapped_block;

    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Flag, Yes;

    enum blockSize = 64;
    enum numBlocks = 10;

    // The 'BitmappedBlock' is implicitly instantiated with Yes.multiblock
    auto a = BitmappedBlock!(blockSize, 8, Mallocator, Yes.multiblock)(numBlocks * blockSize);

    // Instantiated with Yes.multiblock, can allocate more than one block at a time
    void[] buf = a.allocate(2 * blockSize);
    assert(buf.length == 2 * blockSize);
    assert(a.deallocate(buf));

    // Can also allocate less than one block
    buf = a.allocate(blockSize / 2);
    assert(buf.length == blockSize / 2);

    // Expands inside the same block
    assert(a.expand(buf, blockSize / 2));
    assert(buf.length == blockSize);

    // If Yes.multiblock, can expand past the size of a single block
    assert(a.expand(buf, 3 * blockSize));
    assert(buf.length == 4 * blockSize);
    assert(a.deallocate(buf));
}

@system unittest
{
    import std.experimental.allocator.building_blocks.bitmapped_block;

    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Flag, No;

    enum blockSize = 64;
    auto a = BitmappedBlock!(blockSize, 8, Mallocator, No.multiblock)(1024 * blockSize);

    // Since instantiated with No.multiblock, can only allocate at most the block size
    void[] buf = a.allocate(blockSize + 1);
    assert(buf is null);

    buf = a.allocate(blockSize);
    assert(buf.length == blockSize);
    assert(a.deallocate(buf));

    // This is also fine, because it's less than the block size
    buf = a.allocate(blockSize / 2);
    assert(buf.length == blockSize / 2);

    // Can expand the buffer until its length is at most 64
    assert(a.expand(buf, blockSize / 2));
    assert(buf.length == blockSize);

    // Cannot expand anymore
    assert(!a.expand(buf, 1));
    assert(a.deallocate(buf));
}

@system unittest
{
    import std.experimental.allocator.building_blocks.bitmapped_block;

    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.common : platformAlignment;
    import std.typecons : Flag, Yes, No;

    // Create 'numThreads' threads, each allocating in parallel a chunk of memory
    static void testAlloc(Allocator)(ref Allocator a, size_t allocSize)
    {
        import core.thread : ThreadGroup;
        import std.algorithm.sorting : sort;
        import core.internal.spinlock : SpinLock;

        SpinLock lock = SpinLock(SpinLock.Contention.brief);
        enum numThreads = 10;
        void[][numThreads] buf;
        size_t count = 0;

        // Each threads allocates 'allocSize'
        void fun()
        {
            void[] b = a.allocate(allocSize);
            assert(b.length == allocSize);

            lock.lock();
            scope(exit) lock.unlock();

            buf[count] = b;
            count++;
        }

        auto tg = new ThreadGroup;
        foreach (i; 0 .. numThreads)
        {
            tg.create(&fun);
        }
        tg.joinAll();

        // Sorting the allocations made by each thread, we expect the buffers to be
        // adjacent inside the SharedBitmappedBlock
        sort!((a, b) => a.ptr < b.ptr)(buf[0 .. numThreads]);
        foreach (i; 0 .. numThreads - 1)
        {
            assert(buf[i].ptr + a.goodAllocSize(buf[i].length) <= buf[i + 1].ptr);
        }

        // Deallocate everything
        foreach (i; 0 .. numThreads)
        {
            assert(a.deallocate(buf[i]));
        }
    }

    enum blockSize = 64;
    auto alloc1 = SharedBitmappedBlock!(blockSize, platformAlignment, Mallocator, Yes.multiblock)(1024 * 1024);
    auto alloc2 = SharedBitmappedBlock!(blockSize, platformAlignment, Mallocator, No.multiblock)(1024 * 1024);
    testAlloc(alloc1, 2 * blockSize);
    testAlloc(alloc2, blockSize);
}

