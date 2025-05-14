@system unittest
{
    import std.experimental.allocator;

    // Install a new allocator that is faster for 128-byte allocations.
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    auto oldAllocator = theAllocator;
    scope(exit) theAllocator = oldAllocator;
    theAllocator = allocatorObject(FreeList!(GCAllocator, 128)());
    // Use the now changed allocator to allocate an array
    const ubyte[] arr = theAllocator.makeArray!ubyte(128);
    assert(arr.ptr);
    //...
}

@system unittest
{
    import std.experimental.allocator;

    // Dynamically allocate one integer
    const int* p1 = theAllocator.make!int;
    // It's implicitly initialized with its .init value
    assert(*p1 == 0);
    // Dynamically allocate one double, initialize to 42.5
    const double* p2 = theAllocator.make!double(42.5);
    assert(*p2 == 42.5);

    // Dynamically allocate a struct
    static struct Point
    {
        int x, y, z;
    }
    // Use the generated constructor taking field values in order
    const Point* p = theAllocator.make!Point(1, 2);
    assert(p.x == 1 && p.y == 2 && p.z == 0);

    // Dynamically allocate a class object
    static class Customer
    {
        uint id = uint.max;
        this() {}
        this(uint id) { this.id = id; }
        // ...
    }
    Customer cust = theAllocator.make!Customer;
    assert(cust.id == uint.max); // default initialized
    cust = theAllocator.make!Customer(42);
    assert(cust.id == 42);

    // explicit passing of outer pointer
    static class Outer
    {
        int x = 3;
        class Inner
        {
            auto getX() { return x; }
        }
    }
    auto outer = theAllocator.make!Outer();
    auto inner = theAllocator.make!(Outer.Inner)(outer);
    assert(outer.x == inner.getX);
}

@system unittest
{
    import std.experimental.allocator;

    import std.algorithm.comparison : equal;
    static void test(T)()
    {
        T[] a = theAllocator.makeArray!T(2);
        assert(a.equal([0, 0]));
        a = theAllocator.makeArray!T(3, 42);
        assert(a.equal([42, 42, 42]));
        import std.range : only;
        a = theAllocator.makeArray!T(only(42, 43, 44));
        assert(a.equal([42, 43, 44]));
    }
    test!int();
    test!(shared int)();
    test!(const int)();
    test!(immutable int)();
}

@system unittest
{
    import std.experimental.allocator;

    auto arr = theAllocator.makeArray!int([1, 2, 3]);
    assert(theAllocator.expandArray(arr, 2));
    assert(arr == [1, 2, 3, 0, 0]);
    import std.range : only;
    assert(theAllocator.expandArray(arr, only(4, 5)));
    assert(arr == [1, 2, 3, 0, 0, 4, 5]);
}

@system unittest
{
    import std.experimental.allocator;

    int[] a = theAllocator.makeArray!int(100, 42);
    assert(a.length == 100);
    assert(theAllocator.shrinkArray(a, 98));
    assert(a.length == 2);
    assert(a == [42, 42]);
}

@system unittest
{
    import std.experimental.allocator;

    import std.experimental.allocator.mallocator : Mallocator;

    auto mArray = Mallocator.instance.makeMultidimensionalArray!int(2, 3, 6);

    // deallocate when exiting scope
    scope(exit)
    {
        Mallocator.instance.disposeMultidimensionalArray(mArray);
    }

    assert(mArray.length == 2);
    foreach (lvl2Array; mArray)
    {
        assert(lvl2Array.length == 3);
        foreach (lvl3Array; lvl2Array)
            assert(lvl3Array.length == 6);
    }
}

@system unittest
{
    import std.experimental.allocator;

    struct TestAllocator
    {
        import std.experimental.allocator.common : platformAlignment;
        import std.experimental.allocator.mallocator : Mallocator;

        alias allocator = Mallocator.instance;

        private static struct ByteRange
        {
            void* ptr;
            size_t length;
        }

        private ByteRange[] _allocations;

        enum uint alignment = platformAlignment;

        void[] allocate(size_t numBytes)
        {
             auto ret = allocator.allocate(numBytes);
             _allocations ~= ByteRange(ret.ptr, ret.length);
             return ret;
        }

        bool deallocate(void[] bytes)
        {
            import std.algorithm.mutation : remove;
            import std.algorithm.searching : canFind;

            bool pred(ByteRange other)
            { return other.ptr == bytes.ptr && other.length == bytes.length; }

            assert(_allocations.canFind!pred);

             _allocations = _allocations.remove!pred;
             return allocator.deallocate(bytes);
        }

        ~this()
        {
            assert(!_allocations.length);
        }
    }

    TestAllocator allocator;

    auto mArray = allocator.makeMultidimensionalArray!int(2, 3, 5, 6, 7, 2);

    allocator.disposeMultidimensionalArray(mArray);
}

@system unittest
{
    import std.experimental.allocator;

    import std.experimental.allocator.mallocator : Mallocator;

    RCIAllocator a = allocatorObject(Mallocator.instance);
    auto b = a.allocate(100);
    assert(b.length == 100);
    assert(a.deallocate(b));

    // The in-situ region must be used by pointer
    import std.experimental.allocator.building_blocks.region : InSituRegion;
    auto r = InSituRegion!1024();
    a = allocatorObject(&r);
    b = a.allocate(200);
    assert(b.length == 200);
    // In-situ regions can deallocate the last allocation
    assert(a.deallocate(b));
}

@system unittest
{
    import std.experimental.allocator;

    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;

    static assert(!is(ThreadLocal!Mallocator));
    static assert(!is(ThreadLocal!GCAllocator));
    alias Allocator = ThreadLocal!(FreeList!(GCAllocator, 0, 8));
    auto b = Allocator.instance.allocate(5);
    static assert(__traits(hasMember, Allocator, "allocate"));
}

@system unittest
{
    import std.experimental.allocator;

    import std.experimental.allocator.building_blocks.allocator_list : AllocatorList;
    import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlock;
    import std.experimental.allocator.building_blocks.segregator : Segregator;
    import std.experimental.allocator.building_blocks.bucketizer : Bucketizer;
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;

    /// Define an allocator bound to the built-in GC.
    auto alloc = allocatorObject(GCAllocator.instance);
    auto b = alloc.allocate(42);
    assert(b.length == 42);
    assert(alloc.deallocate(b));

    import std.algorithm.comparison : max;
    // Define an elaborate allocator and bind it to the class API.
    alias FList = FreeList!(GCAllocator, 0, unbounded);
    alias A = ThreadLocal!(
        Segregator!(
            8, FreeList!(GCAllocator, 0, 8),
            128, Bucketizer!(FList, 1, 128, 16),
            256, Bucketizer!(FList, 129, 256, 32),
            512, Bucketizer!(FList, 257, 512, 64),
            1024, Bucketizer!(FList, 513, 1024, 128),
            2048, Bucketizer!(FList, 1025, 2048, 256),
            3584, Bucketizer!(FList, 2049, 3584, 512),
            4072 * 1024, AllocatorList!(
                (n) => BitmappedBlock!(4096)(cast(ubyte[]) GCAllocator.instance.allocate(
                    max(n, 4072 * 1024)))),
            GCAllocator
        )
    );

    auto alloc2 = allocatorObject(A.instance);
    b = alloc2.allocate(101);
    assert(alloc2.deallocate(b));
}

