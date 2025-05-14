@system unittest
{
    import std.experimental.allocator.building_blocks.quantizer;

    import std.experimental.allocator.building_blocks.free_tree : FreeTree;
    import std.experimental.allocator.gc_allocator : GCAllocator;

    size_t roundUpToMultipleOf(size_t s, uint base)
    {
        auto rem = s % base;
        return rem ? s + base - rem : s;
    }

    // Quantize small allocations to a multiple of cache line, large ones to a
    // multiple of page size
    alias MyAlloc = Quantizer!(
        FreeTree!GCAllocator,
        n => roundUpToMultipleOf(n, n <= 16_384 ? 64 : 4096));
    MyAlloc alloc;
    const buf = alloc.allocate(256);
    assert(buf.ptr);
}

