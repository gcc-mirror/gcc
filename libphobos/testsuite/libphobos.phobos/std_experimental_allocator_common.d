@safe @nogc nothrow pure unittest
{
    import std.experimental.allocator.common;

    import std.experimental.allocator.building_blocks.null_allocator : NullAllocator;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mmap_allocator : MmapAllocator;
    static assert(isAllocator!NullAllocator);
    static assert(isAllocator!Mallocator);
    static assert(isAllocator!GCAllocator);
    static assert(isAllocator!MmapAllocator);
    static assert(!isAllocator!int);
}

@safe @nogc nothrow pure unittest
{
    import std.experimental.allocator.common;

    import std.experimental.allocator.building_blocks.null_allocator : NullAllocator;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mmap_allocator : MmapAllocator;
    struct S
    {
        mixin AllocatorState!NullAllocator n;
        mixin AllocatorState!GCAllocator g;
        mixin AllocatorState!Mallocator m;
        mixin AllocatorState!MmapAllocator p;
    }
    static assert(S.sizeof == 1);
}

