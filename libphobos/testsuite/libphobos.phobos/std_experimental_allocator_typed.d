@system unittest
{
    import std.experimental.allocator.typed;

    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.mmap_allocator : MmapAllocator;
    alias MyAllocator = TypedAllocator!(GCAllocator,
        AllocFlag.fixedSize | AllocFlag.threadLocal, Mallocator,
        AllocFlag.fixedSize | AllocFlag.threadLocal
                | AllocFlag.hasNoIndirections,
            MmapAllocator,
    );

    MyAllocator a;
    auto b = &a.allocatorFor!0();
    static assert(is(typeof(*b) == shared const(GCAllocator)));
    enum f1 = AllocFlag.fixedSize | AllocFlag.threadLocal;
    auto c = &a.allocatorFor!f1();
    static assert(is(typeof(*c) == Mallocator));
    enum f2 = AllocFlag.fixedSize | AllocFlag.threadLocal;
    static assert(is(typeof(a.allocatorFor!f2()) == Mallocator));
    // Partial match
    enum f3 = AllocFlag.threadLocal;
    static assert(is(typeof(a.allocatorFor!f3()) == Mallocator));

    int* p = a.make!int;
    scope(exit) a.dispose(p);
    int[] arr = a.makeArray!int(42);
    scope(exit) a.dispose(arr);
    assert(a.expandArray(arr, 3));
    assert(a.shrinkArray(arr, 4));
}

