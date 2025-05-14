@safe unittest
{
    import std.experimental.allocator.building_blocks.stats_collector;

    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    alias Allocator = StatsCollector!(GCAllocator, Options.bytesUsed);
}

@system unittest
{
    import std.experimental.allocator.building_blocks.stats_collector;

    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    alias Allocator = StatsCollector!(GCAllocator, Options.all, Options.all);

    Allocator alloc;
    auto b = alloc.allocate(10);
    alloc.reallocate(b, 20);
    alloc.deallocate(b);

    import std.file : deleteme, remove;
    import std.range : walkLength;
    import std.stdio : File;

    auto f = deleteme ~ "-dlang.std.experimental.allocator.stats_collector.txt";
    scope(exit) remove(f);
    Allocator.reportPerCallStatistics(File(f, "w"));
    alloc.reportStatistics(File(f, "a"));
    assert(File(f).byLine.walkLength == 24);
}

