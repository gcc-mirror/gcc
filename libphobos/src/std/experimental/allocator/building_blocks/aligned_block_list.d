// Written in the D programming language.
/**
`AlignedBlockList` represents a wrapper around a chain of allocators, allowing for fast deallocations
and preserving a low degree of fragmentation by means of aligned allocations.

Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/aligned_block_list.d)
*/
module std.experimental.allocator.building_blocks.aligned_block_list;

import std.experimental.allocator.common;
import std.experimental.allocator.building_blocks.null_allocator;

// Common function implementation for thread local and shared AlignedBlockList
private mixin template AlignedBlockListImpl(bool isShared)
{
    import std.traits : hasMember;
    import std.typecons : Ternary;

    static if (isShared)
    import core.internal.spinlock : SpinLock;

private:
    // Doubly linked list of 'AlignedBlockNode'
    // Each node contains an `Allocator` followed by its payload
    static struct AlignedBlockNode
    {
        AlignedBlockNode* next, prev;
        Allocator bAlloc;

        static if (isShared)
        {
            shared(size_t) bytesUsed;
            // Since the lock is not taken when allocating, this acts like a refcount
            // keeping the node alive
            uint keepAlive;
        }
        else
        {
            size_t bytesUsed;
        }
    }

    // Root of the internal doubly linked list
    AlignedBlockNode* root;

    // Number of active nodes
    uint numNodes;

    // If the numNodes exceeds this limit, we will start deallocating nodes
    enum uint maxNodes = 64;

    // This lock is always taken when changing the list
    // To improve performance, the lock is not taken when the allocation logic is called
    static if (isShared)
    SpinLock lock = SpinLock(SpinLock.Contention.brief);

    // Moves a node to the front of the list, allowing for quick allocations
    void moveToFront(AlignedBlockNode* tmp)
    {
        auto localRoot = cast(AlignedBlockNode*) root;
        if (tmp == localRoot)
            return;

        if (tmp.prev) tmp.prev.next = tmp.next;
        if (tmp.next) tmp.next.prev = tmp.prev;
        if (localRoot) localRoot.prev = tmp;
        tmp.next = localRoot;
        tmp.prev = null;

        root = cast(typeof(root)) tmp;
    }

    // Removes a node from the list, including its payload
    // The payload is deallocated by calling 'parent.deallocate'
    void removeNode(AlignedBlockNode* tmp)
    {
        auto next = tmp.next;
        if (tmp.prev) tmp.prev.next = tmp.next;
        if (tmp.next) tmp.next.prev = tmp.prev;
        parent.deallocate((cast(void*) tmp)[0 .. theAlignment]);

        if (tmp == cast(AlignedBlockNode*) root)
            root = cast(typeof(root)) next;

        static if (isShared)
        {
            import core.atomic : atomicOp;
            atomicOp!"-="(numNodes, 1);
        }
        else
        {
            numNodes--;
        }
    }

    // If the nodes do not have available space, a new node is created
    // by drawing memory from the parent allocator with aligned allocations.
    // The new node is inserted at the front of the list
    bool insertNewNode()
    {
        void[] buf = parent.alignedAllocate(theAlignment, theAlignment);
        if (buf is null)
            return false;

        auto localRoot = cast(AlignedBlockNode*) root;
        auto newNode = cast(AlignedBlockNode*) buf;

        // The first part of the allocation represent the node contents
        // followed by the actual payload
        ubyte[] payload = cast(ubyte[]) buf[AlignedBlockNode.sizeof .. $];
        newNode.bAlloc = Allocator(payload);

        newNode.next = localRoot;
        newNode.prev = null;
        if (localRoot)
            localRoot.prev = newNode;
        root = cast(typeof(root)) newNode;

        static if (isShared)
        {
            import core.atomic : atomicOp;
            atomicOp!"+="(numNodes, 1);
        }
        else
        {
            numNodes++;
        }

        return true;
    }

public:
    static if (stateSize!ParentAllocator) ParentAllocator parent;
    else alias parent = ParentAllocator.instance;

    enum ulong alignment = Allocator.alignment;

    // Since all memory is drawn from ParentAllocator, we can
    // forward this to the parent
    static if (hasMember!(ParentAllocator, "owns"))
    Ternary owns(void[] b)
    {
        return parent.owns(b);
    }

    // Use `theAlignment` to find the node which allocated this block
    bool deallocate(void[] b)
    {
        if (b is null)
            return true;

        // Round buffer to nearest `theAlignment` multiple to quickly find
        // the `parent` `AlignedBlockNode`
        enum ulong mask = ~(theAlignment - 1);
        ulong ptr = ((cast(ulong) b.ptr) & mask);
        AlignedBlockNode *node = cast(AlignedBlockNode*) ptr;
        if (node.bAlloc.deallocate(b))
        {
            static if (isShared)
            {
                import core.atomic : atomicOp;
                atomicOp!"-="(node.bytesUsed, b.length);
            }
            else
            {
                node.bytesUsed -= b.length;
            }
            return true;
        }
        return false;
    }

    // Allocate works only if memory can be provided via `alignedAllocate` from the parent
    static if (hasMember!(ParentAllocator, "alignedAllocate"))
    void[] allocate(size_t n)
    {
        static if (isShared)
        import core.atomic : atomicOp, atomicLoad;

        if (n == 0 || n > theAlignment)
            return null;

        static if (isShared)
        {
            lock.lock();
            scope(exit) lock.unlock();
        }

        auto tmp = cast(AlignedBlockNode*) root;

        // Iterate through list and find first node which has memory available
        while (tmp)
        {
            auto next = tmp.next;
            static if (isShared)
            {
                // Allocations can happen outside the lock
                // Make sure nobody deletes this node while using it
                tmp.keepAlive++;
                if (next) next.keepAlive++;
                lock.unlock();
            }

            auto result = tmp.bAlloc.allocate(n);
            if (result.length == n)
            {
                // Success
                static if (isShared)
                {
                    atomicOp!"+="(tmp.bytesUsed, n);
                    lock.lock();
                }
                else
                {
                    tmp.bytesUsed += n;
                }

                // Most likely this node has memory for more allocations
                // Move it to the front
                moveToFront(tmp);

                static if (isShared)
                {
                    tmp.keepAlive--;
                    if (next) next.keepAlive--;
                }

                return result;
            }

            // This node can now be removed if necessary
            static if (isShared)
            {
                lock.lock();
                tmp.keepAlive--;
                if (next) next.keepAlive--;
            }

            if (!next)
                break;

            tmp = next;
            next = tmp.next;

            // If there are too many nodes, free memory by removing empty nodes
            static if (isShared)
            {
                if (atomicLoad(numNodes) > maxNodes &&
                    atomicLoad(tmp.bytesUsed) == 0 &&
                    tmp.keepAlive == 0)
                {
                    removeNode(tmp);
                }
            }
            else
            {
                if (numNodes > maxNodes && tmp.bytesUsed == 0)
                {
                    removeNode(tmp);
                }
            }

            tmp = next;
        }

        // Cannot create new AlignedBlockNode. Most likely the ParentAllocator ran out of resources
        if (!insertNewNode())
            return null;

        tmp = cast(typeof(tmp)) root;
        void[] result = tmp.bAlloc.allocate(n);

        static if (isShared)
        {
            atomicOp!"+="(root.bytesUsed, result.length);
        }
        else
        {
            root.bytesUsed += result.length;
        }

        return result;
    }

    // goodAllocSize should not use state
    size_t goodAllocSize(const size_t n)
    {
        Allocator a = null;
        return a.goodAllocSize(n);
    }
}

/**
`AlignedBlockList` represents a wrapper around a chain of allocators, allowing for fast deallocations
and preserving a low degree of fragmentation.
The allocator holds internally a doubly linked list of `Allocator` objects, which will serve allocations
in a most-recently-used fashion. Most recent allocators used for `allocate` calls, will be
moved to the front of the list.

Although allocations are in theory served in linear searching time, `deallocate` calls take
$(BIGOH 1) time, by using aligned allocations. `ParentAllocator` must implement `alignedAllocate`
and it must be able to allocate `theAlignment` bytes at the same alignment. Each aligned allocation
done by `ParentAllocator` will contain metadata for an `Allocator`, followed by its payload.

Params:
    Allocator = the allocator which is used to manage each node; it must have a constructor which receives
        `ubyte[]` and it must not have any parent allocators, except for the `NullAllocator`
    ParentAllocator = each node draws memory from the parent allocator; it must support `alignedAllocate`
    theAlignment = alignment of each block and at the same time length of each node
*/
struct AlignedBlockList(Allocator, ParentAllocator, ulong theAlignment = (1 << 21))
{
    version (StdDdoc)
    {
        import std.typecons : Ternary;
        import std.traits : hasMember;

        /**
        Returns a chunk of memory of size `n`
        It finds the first node in the `AlignedBlockNode` list which has available memory,
        and moves it to the front of the list.

        All empty nodes which cannot return new memory, are removed from the list.

        Params:
            n = bytes to allocate
        Returns:
            A chunk of memory of the required length or `null` on failure or
        */
        static if (hasMember!(ParentAllocator, "alignedAllocate"))
        void[] allocate(size_t n);

        /**
        Deallocates the buffer `b` given as parameter. Deallocations take place in constant
        time, regardless of the number of nodes in the list. `b.ptr` is rounded down
        to the nearest multiple of the `alignment` to quickly find the corresponding
        `AlignedBlockNode`.

        Params:
            b = buffer candidate for deallocation
        Returns:
            `true` on success and `false` on failure
        */
        bool deallocate(void[] b);

        /**
        Returns `Ternary.yes` if the buffer belongs to the parent allocator and
        `Ternary.no` otherwise.

        Params:
            b = buffer tested if owned by this allocator
        Returns:
            `Ternary.yes` if owned by this allocator and `Ternary.no` otherwise
        */
        static if (hasMember!(ParentAllocator, "owns"))
        Ternary owns(void[] b);
    }
    else
    {
        import std.math.traits : isPowerOf2;
        static assert(isPowerOf2(alignment));
        mixin AlignedBlockListImpl!false;
    }
}

///
@system unittest
{
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

/**
`SharedAlignedBlockList` is the threadsafe version of `AlignedBlockList`.
The `Allocator` template parameter must refer a shared allocator.
Also, `ParentAllocator` must be a shared allocator, supporting `alignedAllocate`.

Params:
    Allocator = the shared allocator which is used to manage each node; it must have a constructor which receives
        `ubyte[]` and it must not have any parent allocators, except for the `NullAllocator`
    ParentAllocator = each node draws memory from the parent allocator; it must be shared and support `alignedAllocate`
    theAlignment = alignment of each block and at the same time length of each node
*/
shared struct SharedAlignedBlockList(Allocator, ParentAllocator, ulong theAlignment = (1 << 21))
{
    version (StdDdoc)
    {
        import std.typecons : Ternary;
        import std.traits : hasMember;

        /**
        Returns a chunk of memory of size `n`
        It finds the first node in the `AlignedBlockNode` list which has available memory,
        and moves it to the front of the list.

        All empty nodes which cannot return new memory, are removed from the list.

        Params:
            n = bytes to allocate
        Returns:
            A chunk of memory of the required length or `null` on failure or
        */
        static if (hasMember!(ParentAllocator, "alignedAllocate"))
        void[] allocate(size_t n);

        /**
        Deallocates the buffer `b` given as parameter. Deallocations take place in constant
        time, regardless of the number of nodes in the list. `b.ptr` is rounded down
        to the nearest multiple of the `alignment` to quickly find the corresponding
        `AlignedBlockNode`.

        Params:
            b = buffer candidate for deallocation
        Returns:
            `true` on success and `false` on failure
        */
        bool deallocate(void[] b);

        /**
        Returns `Ternary.yes` if the buffer belongs to the parent allocator and
        `Ternary.no` otherwise.

        Params:
            b = buffer tested if owned by this allocator
        Returns:
            `Ternary.yes` if owned by this allocator and `Ternary.no` otherwise
        */
        static if (hasMember!(ParentAllocator, "owns"))
        Ternary owns(void[] b);
    }
    else
    {
        import std.math.traits : isPowerOf2;
        static assert(isPowerOf2(alignment));
        mixin AlignedBlockListImpl!true;
    }
}

///
@system unittest
{
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

version (StdUnittest)
{
    static void testrw(void[] b)
    {
        ubyte* buf = cast(ubyte*) b.ptr;
        size_t len = (b.length).roundUpToMultipleOf(4096);
        for (int i = 0; i < len; i += 4096)
        {
            buf[i] =  (cast(ubyte) i % 256);
            assert(buf[i] == (cast(ubyte) i % 256));
        }
    }
}

@system unittest
{
    import std.experimental.allocator.building_blocks.region;
    import std.experimental.allocator.building_blocks.ascending_page_allocator;
    import std.random;
    import std.algorithm.sorting : sort;
    import core.thread : ThreadGroup;
    import core.internal.spinlock : SpinLock;

    enum pageSize = 4096;
    enum numThreads = 10;
    enum maxIter = 20;
    enum totalAllocs = maxIter * numThreads;
    size_t count = 0;
    SpinLock lock = SpinLock(SpinLock.Contention.brief);

    alias SuperAllocator = SharedAlignedBlockList!(
            SharedBorrowedRegion!(1),
            SharedAscendingPageAllocator,
            1 << 16);
    void[][totalAllocs] buf;

    SuperAllocator a;
    a.parent = SharedAscendingPageAllocator(4096 * 1024);

    void fun()
    {
        auto rnd = Random(1000);

        foreach (i; 0 .. maxIter)
        {
            auto size = uniform(1, pageSize + 1, rnd);
            void[] b = a.allocate(size);
            assert(b.length == size);
            testrw(b);

            lock.lock();
            buf[count++] = b;
            lock.unlock();
        }
    }
    auto tg = new ThreadGroup;
    foreach (i; 0 .. numThreads)
    {
        tg.create(&fun);
    }
    tg.joinAll();

    sort!((a, b) => a.ptr < b.ptr)(buf[0 .. totalAllocs]);
    foreach (i; 0 .. totalAllocs - 1)
    {
        assert(buf[i].ptr + a.goodAllocSize(buf[i].length) <= buf[i + 1].ptr);
    }

    foreach (i; 0 .. totalAllocs)
    {
        assert(a.deallocate(buf[totalAllocs - 1 - i]));
    }
}

@system unittest
{
    import std.experimental.allocator.building_blocks.ascending_page_allocator : AscendingPageAllocator;
    import std.experimental.allocator.building_blocks.segregator : Segregator;
    import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlock;
    import std.random;

    alias SuperAllocator = Segregator!(
        256,
        AlignedBlockList!(BitmappedBlock!256, AscendingPageAllocator*, 1 << 16),
        Segregator!(

        512,
        AlignedBlockList!(BitmappedBlock!512, AscendingPageAllocator*, 1 << 16),
        Segregator!(

        1024,
        AlignedBlockList!(BitmappedBlock!1024, AscendingPageAllocator*, 1 << 16),
        Segregator!(

        2048,
        AlignedBlockList!(BitmappedBlock!2048, AscendingPageAllocator*, 1 << 16),
        AscendingPageAllocator*
    ))));

    SuperAllocator a;
    auto pageAlloc = AscendingPageAllocator(4096 * 4096);
    a.allocatorForSize!4096 = &pageAlloc;
    a.allocatorForSize!2048.parent = &pageAlloc;
    a.allocatorForSize!1024.parent = &pageAlloc;
    a.allocatorForSize!512.parent = &pageAlloc;
    a.allocatorForSize!256.parent = &pageAlloc;

    auto rnd = Random(1000);

    size_t maxIter = 10;
    enum testNum = 10;
    void[][testNum] buf;
    int maxSize = 8192;
    foreach (i; 0 .. maxIter)
    {
        foreach (j; 0 .. testNum)
        {
            auto size = uniform(1, maxSize + 1, rnd);
            buf[j] = a.allocate(size);
            assert(buf[j].length == size);
            testrw(buf[j]);
        }

        randomShuffle(buf[]);

        foreach (j; 0 .. testNum)
        {
            assert(a.deallocate(buf[j]));
        }
    }
}
