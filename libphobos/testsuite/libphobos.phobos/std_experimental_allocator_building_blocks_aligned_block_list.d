@system unittest
{
    import std.experimental.allocator.building_blocks.aligned_block_list;

    import std.experimental.allocator.building_blocks.ascending_page_allocator : AscendingPageAllocator;
    import std.experimental.allocator.building_blocks.segregator : Segregator;
    import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlock;
    import std.typecons : Ternary;

    /*
    In this example we use 'AlignedBlockList' in conjunction with other allocators
    in order to create a more complex allocator.

    The 'SuperAllocator' uses a 'Segregator' to distribute allocations to sub-allocators,
    based on the requested size.

    Each sub-allocator is represented by an 'AlignedBlockList' of 'BitmappedBlocks'.
    Each 'AlignedBlockList' draws memory from a root allocator which in this case is an 'AscendingPageAllocator'

    Such an allocator not only provides good performance, but also a low degree of memory fragmentation.
    */
    alias SuperAllocator = Segregator!(
        32,
        AlignedBlockList!(BitmappedBlock!32, AscendingPageAllocator*, 1 << 12),
        Segregator!(

        64,
        AlignedBlockList!(BitmappedBlock!64, AscendingPageAllocator*, 1 << 12),
        Segregator!(

        128,
        AlignedBlockList!(BitmappedBlock!128, AscendingPageAllocator*, 1 << 12),
        AscendingPageAllocator*
    )));

    SuperAllocator a;
    auto pageAlloc = AscendingPageAllocator(128 * 4096);

    // Set the parent allocator for all the sub allocators
    a.allocatorForSize!256 = &pageAlloc;
    a.allocatorForSize!128.parent = &pageAlloc;
    a.allocatorForSize!64.parent = &pageAlloc;
    a.allocatorForSize!32.parent = &pageAlloc;

    enum testNum = 10;
    void[][testNum] buf;

    // Allocations of size 32 will go to the first 'AlignedBlockList'
    foreach (j; 0 .. testNum)
    {
        buf[j] = a.allocate(32);
        assert(buf[j].length == 32);

        // This is owned by the first 'AlignedBlockList'
        assert(a.allocatorForSize!32.owns(buf[j]) == Ternary.yes);
    }

    // Free the memory
    foreach (j; 0 .. testNum)
        assert(a.deallocate(buf[j]));

    // Allocations of size 64 will go to the second 'AlignedBlockList'
    foreach (j; 0 .. testNum)
    {
        buf[j] = a.allocate(64);
        assert(buf[j].length == 64);

        // This is owned by the second 'AlignedBlockList'
        assert(a.allocatorForSize!64.owns(buf[j]) == Ternary.yes);
    }

    // Free the memory
    foreach (j; 0 .. testNum)
        assert(a.deallocate(buf[j]));

    // Allocations of size 128 will go to the third 'AlignedBlockList'
    foreach (j; 0 .. testNum)
    {
        buf[j] = a.allocate(128);
        assert(buf[j].length == 128);

        // This is owned by the third 'AlignedBlockList'
        assert(a.allocatorForSize!128.owns(buf[j]) == Ternary.yes);
    }

    // Free the memory
    foreach (j; 0 .. testNum)
        assert(a.deallocate(buf[j]));

    // Allocations which exceed 128, will go to the 'AscendingPageAllocator*'
    void[] b = a.allocate(256);
    assert(b.length == 256);
    a.deallocate(b);
}

@system unittest
{
    import std.experimental.allocator.building_blocks.aligned_block_list;

    import std.experimental.allocator.building_blocks.region : SharedBorrowedRegion;
    import std.experimental.allocator.building_blocks.ascending_page_allocator : SharedAscendingPageAllocator;
    import std.experimental.allocator.building_blocks.null_allocator : NullAllocator;
    import core.thread : ThreadGroup;

    enum numThreads = 8;
    enum size = 2048;
    enum maxIter = 10;

    /*
    In this example we use 'SharedAlignedBlockList' together with
    'SharedBorrowedRegion', in order to create a fast, thread-safe allocator.
    */
    alias SuperAllocator = SharedAlignedBlockList!(
            SharedBorrowedRegion!(1),
            SharedAscendingPageAllocator,
            4096);

    SuperAllocator a;
    // The 'SuperAllocator' will draw memory from a 'SharedAscendingPageAllocator'
    a.parent = SharedAscendingPageAllocator(4096 * 1024);

    // Launch 'numThreads', each performing allocations
    void fun()
    {
        foreach (i; 0 .. maxIter)
        {
            void[] b = a.allocate(size);
            assert(b.length == size);
        }
    }

    auto tg = new ThreadGroup;
    foreach (i; 0 .. numThreads)
    {
        tg.create(&fun);
    }
    tg.joinAll();
}

