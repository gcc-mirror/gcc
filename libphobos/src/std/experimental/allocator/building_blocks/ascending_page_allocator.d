// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/ascending_page_allocator.d)
*/
module std.experimental.allocator.building_blocks.ascending_page_allocator;

import core.memory : pageSize;

import std.experimental.allocator.common;

// Common implementations for shared and thread local AscendingPageAllocator
private mixin template AscendingPageAllocatorImpl(bool isShared)
{
    bool deallocate(void[] buf) nothrow @nogc
    {
        size_t goodSize = goodAllocSize(buf.length);
        version (Posix)
        {
            import core.sys.posix.sys.mman : mmap, MAP_FAILED, MAP_PRIVATE,
                MAP_ANON, MAP_FIXED, PROT_NONE, munmap;

            auto ptr = mmap(buf.ptr, goodSize, PROT_NONE, MAP_ANON | MAP_PRIVATE | MAP_FIXED, -1, 0);
            if (ptr == MAP_FAILED)
                 return false;
        }
        else version (Windows)
        {
            import core.sys.windows.winbase : VirtualFree;
            import core.sys.windows.winnt : MEM_DECOMMIT;

            auto ret = VirtualFree(buf.ptr, goodSize, MEM_DECOMMIT);
            if (ret == 0)
                 return false;
        }
        else
        {
            static assert(0, "Unsupported OS");
        }

        static if (!isShared)
        {
            pagesUsed -= goodSize / pageSize;
        }

        return true;
    }

    Ternary owns(void[] buf) nothrow @nogc
    {
        if (!data)
            return Ternary.no;
        return Ternary(buf.ptr >= data && buf.ptr < buf.ptr + numPages * pageSize);
    }

    bool deallocateAll() nothrow @nogc
    {
        version (Posix)
        {
            import core.sys.posix.sys.mman : munmap;
            auto ret = munmap(cast(void*) data, numPages * pageSize);
            if (ret != 0)
                assert(0, "Failed to unmap memory, munmap failure");
        }
        else version (Windows)
        {
            import core.sys.windows.winbase : VirtualFree;
            import core.sys.windows.winnt : MEM_RELEASE;
            auto ret = VirtualFree(cast(void*) data, 0, MEM_RELEASE);
            if (ret == 0)
                assert(0, "Failed to unmap memory, VirtualFree failure");
        }
        else
        {
            static assert(0, "Unsupported OS version");
        }
        data = null;
        offset = null;
        return true;
    }

    size_t goodAllocSize(size_t n) nothrow @nogc
    {
        return n.roundUpToMultipleOf(cast(uint) pageSize);
    }

    this(size_t n) nothrow @nogc
    {
        static if (isShared)
        {
            lock = SpinLock(SpinLock.Contention.brief);
        }

        pageSize = .pageSize;
        numPages = n.roundUpToMultipleOf(cast(uint) pageSize) / pageSize;

        version (Posix)
        {
            import core.sys.posix.sys.mman : mmap, MAP_ANON, PROT_NONE,
                MAP_PRIVATE, MAP_FAILED;

            data = cast(typeof(data)) mmap(null, pageSize * numPages,
                PROT_NONE, MAP_ANON | MAP_PRIVATE, -1, 0);
            if (data == MAP_FAILED)
                assert(0, "Failed to mmap memory");
        }
        else version (Windows)
        {
            import core.sys.windows.winbase : VirtualAlloc;
            import core.sys.windows.winnt : MEM_RESERVE, PAGE_NOACCESS;

            data = cast(typeof(data)) VirtualAlloc(null, pageSize * numPages,
                MEM_RESERVE, PAGE_NOACCESS);
            if (!data)
                assert(0, "Failed to VirtualAlloc memory");
        }
        else
        {
            static assert(0, "Unsupported OS version");
        }

        offset = data;
        readWriteLimit = data;
    }

    size_t getAvailableSize() nothrow @nogc
    {
        static if (isShared)
        {
            lock.lock();
        }

        auto size = numPages * pageSize + data - offset;
        static if (isShared)
        {
            lock.unlock();
        }
        return size;
    }

    // Sets the protection of a memory range to read/write
    private bool extendMemoryProtection(void* start, size_t size) nothrow @nogc
    {
        version (Posix)
        {
            import core.sys.posix.sys.mman : mprotect, PROT_WRITE, PROT_READ;

            auto ret = mprotect(start, size, PROT_WRITE | PROT_READ);
            return ret == 0;
        }
        else version (Windows)
        {
            import core.sys.windows.winbase : VirtualAlloc;
            import core.sys.windows.winnt : MEM_COMMIT, PAGE_READWRITE;

            auto ret = VirtualAlloc(start, size, MEM_COMMIT, PAGE_READWRITE);
            return ret != null;
        }
        else
        {
            static assert(0, "Unsupported OS");
        }
    }
}

/**
`AscendingPageAllocator` is a fast and safe allocator that rounds all allocations
to multiples of the system's page size. It reserves a range of virtual addresses
(using `mmap` on Posix and `VirtualAlloc` on Windows) and allocates memory at consecutive virtual
addresses.

When a chunk of memory is requested, the allocator finds a range of
virtual pages that satisfy the requested size, changing their protection to
read/write using OS primitives (`mprotect` and `VirtualProtect`, respectively).
The physical memory is allocated on demand, when the pages are accessed.

Deallocation removes any read/write permissions from the target pages
and notifies the OS to reclaim the physical memory, while keeping the virtual
memory.

Because the allocator does not reuse memory, any dangling references to
deallocated memory will always result in deterministically crashing the process.

See_Also:
$(HTTPS microsoft.com/en-us/research/wp-content/uploads/2017/03/kedia2017mem.pdf, Simple Fast and Safe Manual Memory Management) for the general approach.
*/
struct AscendingPageAllocator
{
    import std.typecons : Ternary;

    // Docs for mixin functions
    version (StdDdoc)
    {
        /**
        Rounds the mapping size to the next multiple of the page size and calls
        the OS primitive responsible for creating memory mappings: `mmap` on POSIX and
        `VirtualAlloc` on Windows.

        Params:
        n = mapping size in bytes
        */
        this(size_t n) nothrow @nogc;

        /**
        Rounds the requested size to the next multiple of the page size.
        */
        size_t goodAllocSize(size_t n) nothrow @nogc;

        /**
        Decommit all physical memory associated with the buffer given as parameter,
        but keep the range of virtual addresses.

        On POSIX systems `deallocate` calls `mmap` with `MAP_FIXED' a second time to decommit the memory.
        On Windows, it uses `VirtualFree` with `MEM_DECOMMIT`.
        */
        void deallocate(void[] b) nothrow @nogc;

        /**
        Returns `Ternary.yes` if the passed buffer is inside the range of virtual adresses.
        Does not guarantee that the passed buffer is still valid.
        */
        Ternary owns(void[] buf) nothrow @nogc;

        /**
        Removes the memory mapping causing all physical memory to be decommited and
        the virtual address space to be reclaimed.
        */
        bool deallocateAll() nothrow @nogc;

        /**
        Returns the available size for further allocations in bytes.
        */
        size_t getAvailableSize() nothrow @nogc;
    }

private:
    size_t pageSize;
    size_t numPages;

    // The start of the virtual address range
    void* data;

    // Keeps track of there the next allocation should start
    void* offset;

    // Number of pages which contain alive objects
    size_t pagesUsed;

    // On allocation requests, we allocate an extra 'extraAllocPages' pages
    // The address up to which we have permissions is stored in 'readWriteLimit'
    void* readWriteLimit;
    enum extraAllocPages = 1000;

public:
    enum uint alignment = 4096;

    // Inject common function implementations
    mixin AscendingPageAllocatorImpl!false;

    /**
    Rounds the allocation size to the next multiple of the page size.
    The allocation only reserves a range of virtual pages but the actual
    physical memory is allocated on demand, when accessing the memory.

    Params:
    n = Bytes to allocate

    Returns:
    `null` on failure or if the requested size exceeds the remaining capacity.
    */
    void[] allocate(size_t n) nothrow @nogc
    {
        import std.algorithm.comparison : min;

        immutable pagedBytes = numPages * pageSize;
        size_t goodSize = goodAllocSize(n);

        // Requested exceeds the virtual memory range
        if (goodSize > pagedBytes || offset - data > pagedBytes - goodSize)
            return null;

        // Current allocation exceeds readable/writable memory area
        if (offset + goodSize > readWriteLimit)
        {
            // Extend r/w memory range to new limit
            void* newReadWriteLimit = min(data + pagedBytes,
                offset + goodSize + extraAllocPages * pageSize);
            if (newReadWriteLimit != readWriteLimit)
            {
                assert(newReadWriteLimit > readWriteLimit);
                if (!extendMemoryProtection(readWriteLimit, newReadWriteLimit - readWriteLimit))
                    return null;

                readWriteLimit = newReadWriteLimit;
            }
        }

        void* result = offset;
        offset += goodSize;
        pagesUsed += goodSize / pageSize;

        return cast(void[]) result[0 .. n];
    }

    /**
    Rounds the allocation size to the next multiple of the page size.
    The allocation only reserves a range of virtual pages but the actual
    physical memory is allocated on demand, when accessing the memory.

    The allocated memory is aligned to the specified alignment `a`.

    Params:
    n = Bytes to allocate
    a = Alignment

    Returns:
    `null` on failure or if the requested size exceeds the remaining capacity.
    */
    void[] alignedAllocate(size_t n, uint a) nothrow @nogc
    {
        void* alignedStart = cast(void*) roundUpToMultipleOf(cast(size_t) offset, a);
        assert(alignedStart.alignedAt(a));
        immutable pagedBytes = numPages * pageSize;
        size_t goodSize = goodAllocSize(n);
        if (goodSize > pagedBytes ||
            alignedStart - data > pagedBytes - goodSize)
            return null;

        // Same logic as allocate, only that the buffer must be properly aligned
        auto oldOffset = offset;
        offset = alignedStart;
        auto result = allocate(n);
        if (!result)
            offset = oldOffset;
        return result;
    }

    /**
    If the passed buffer is not the last allocation, then `delta` can be
    at most the number of bytes left on the last page.
    Otherwise, we can expand the last allocation until the end of the virtual
    address range.
    */
    bool expand(ref void[] b, size_t delta) nothrow @nogc
    {
        import std.algorithm.comparison : min;

        if (!delta) return true;
        if (b is null) return false;

        size_t goodSize = goodAllocSize(b.length);
        size_t bytesLeftOnPage = goodSize - b.length;

        // If this is not the last allocation, we can only expand until
        // completely filling the last page covered by this buffer
        if (b.ptr + goodSize != offset && delta > bytesLeftOnPage)
            return false;

        size_t extraPages = 0;

        // If the extra `delta` bytes requested do not fit the last page
        // compute how many extra pages are neeeded
        if (delta > bytesLeftOnPage)
        {
            extraPages = goodAllocSize(delta - bytesLeftOnPage) / pageSize;
        }
        else
        {
            b = cast(void[]) b.ptr[0 .. b.length + delta];
            return true;
        }

        if (extraPages > numPages || offset - data > pageSize * (numPages - extraPages))
            return false;

        void* newPtrEnd = b.ptr + goodSize + extraPages * pageSize;
        if (newPtrEnd > readWriteLimit)
        {
            void* newReadWriteLimit = min(data + numPages * pageSize,
                newPtrEnd + extraAllocPages * pageSize);
            if (newReadWriteLimit > readWriteLimit)
            {
                if (!extendMemoryProtection(readWriteLimit, newReadWriteLimit - readWriteLimit))
                    return false;

                readWriteLimit = newReadWriteLimit;
            }
        }

        pagesUsed += extraPages;
        offset += extraPages * pageSize;
        b = cast(void[]) b.ptr[0 .. b.length + delta];
        return true;
    }

    /**
    Returns `Ternary.yes` if the allocator does not contain any alive objects
    and `Ternary.no` otherwise.
    */
    Ternary empty() nothrow @nogc
    {
        return Ternary(pagesUsed == 0);
    }

    /**
    Unmaps the whole virtual address range on destruction.
    */
    ~this() nothrow @nogc
    {
        if (data)
            deallocateAll();
    }
}

///
@system @nogc nothrow unittest
{
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

/**
`SharedAscendingPageAllocator` is the threadsafe version of `AscendingPageAllocator`.
*/
shared struct SharedAscendingPageAllocator
{
    import std.typecons : Ternary;
    import core.internal.spinlock : SpinLock;

    // Docs for mixin functions
    version (StdDdoc)
    {
        /**
        Rounds the mapping size to the next multiple of the page size and calls
        the OS primitive responsible for creating memory mappings: `mmap` on POSIX and
        `VirtualAlloc` on Windows.

        Params:
        n = mapping size in bytes
        */
        this(size_t n) nothrow @nogc;

        /**
        Rounds the requested size to the next multiple of the page size.
        */
        size_t goodAllocSize(size_t n) nothrow @nogc;

        /**
        Decommit all physical memory associated with the buffer given as parameter,
        but keep the range of virtual addresses.

        On POSIX systems `deallocate` calls `mmap` with `MAP_FIXED' a second time to decommit the memory.
        On Windows, it uses `VirtualFree` with `MEM_DECOMMIT`.
        */
        void deallocate(void[] b) nothrow @nogc;

        /**
        Returns `Ternary.yes` if the passed buffer is inside the range of virtual adresses.
        Does not guarantee that the passed buffer is still valid.
        */
        Ternary owns(void[] buf) nothrow @nogc;

        /**
        Removes the memory mapping causing all physical memory to be decommited and
        the virtual address space to be reclaimed.
        */
        bool deallocateAll() nothrow @nogc;

        /**
        Returns the available size for further allocations in bytes.
        */
        size_t getAvailableSize() nothrow @nogc;
    }

private:
    size_t pageSize;
    size_t numPages;

    // The start of the virtual address range
    shared void* data;

    // Keeps track of there the next allocation should start
    shared void* offset;

    // On allocation requests, we allocate an extra 'extraAllocPages' pages
    // The address up to which we have permissions is stored in 'readWriteLimit'
    shared void* readWriteLimit;
    enum extraAllocPages = 1000;
    SpinLock lock;

public:
    enum uint alignment = 4096;

    // Inject common function implementations
    mixin AscendingPageAllocatorImpl!true;

    /**
    Rounds the allocation size to the next multiple of the page size.
    The allocation only reserves a range of virtual pages but the actual
    physical memory is allocated on demand, when accessing the memory.

    Params:
    n = Bytes to allocate

    Returns:
    `null` on failure or if the requested size exceeds the remaining capacity.
    */
    void[] allocate(size_t n) nothrow @nogc
    {
        return allocateImpl(n, 1);
    }

    /**
    Rounds the allocation size to the next multiple of the page size.
    The allocation only reserves a range of virtual pages but the actual
    physical memory is allocated on demand, when accessing the memory.

    The allocated memory is aligned to the specified alignment `a`.

    Params:
    n = Bytes to allocate
    a = Alignment

    Returns:
    `null` on failure or if the requested size exceeds the remaining capacity.
    */
    void[] alignedAllocate(size_t n, uint a) nothrow @nogc
    {
        // For regular `allocate` calls, `a` will be set to 1
        return allocateImpl(n, a);
    }

    private void[] allocateImpl(size_t n, uint a) nothrow @nogc
    {
        import std.algorithm.comparison : min;

        size_t localExtraAlloc;
        void* localOffset;
        immutable pagedBytes = numPages * pageSize;
        size_t goodSize = goodAllocSize(n);

        if (goodSize > pagedBytes)
            return null;

        lock.lock();
        scope(exit) lock.unlock();

        localOffset = cast(void*) offset;
        void* alignedStart = cast(void*) roundUpToMultipleOf(cast(size_t) localOffset, a);
        assert(alignedStart.alignedAt(a));
        if (alignedStart - data > pagedBytes - goodSize)
            return null;

        localOffset = alignedStart + goodSize;
        if (localOffset > readWriteLimit)
        {
            void* newReadWriteLimit = min(cast(void*) data + pagedBytes,
                cast(void*) localOffset + extraAllocPages * pageSize);
            assert(newReadWriteLimit > readWriteLimit);
            localExtraAlloc = newReadWriteLimit - readWriteLimit;
            if (!extendMemoryProtection(cast(void*) readWriteLimit, localExtraAlloc))
                return null;
            readWriteLimit = cast(shared(void*)) newReadWriteLimit;
        }

        offset = cast(typeof(offset)) localOffset;
        return cast(void[]) alignedStart[0 .. n];
    }

    /**
    If the passed buffer is not the last allocation, then `delta` can be
    at most the number of bytes left on the last page.
    Otherwise, we can expand the last allocation until the end of the virtual
    address range.
    */
    bool expand(ref void[] b, size_t delta) nothrow @nogc
    {
        import std.algorithm.comparison : min;

        if (!delta) return true;
        if (b is null) return false;

        void* localOffset;
        size_t localExtraAlloc;
        size_t goodSize = goodAllocSize(b.length);
        size_t bytesLeftOnPage = goodSize - b.length;

        if (bytesLeftOnPage >= delta)
        {
            b = cast(void[]) b.ptr[0 .. b.length + delta];
            return true;
        }

        lock.lock();
        scope(exit) lock.unlock();

        localOffset = cast(void*) offset;
        if (b.ptr + goodSize != localOffset)
            return false;

        size_t extraPages = goodAllocSize(delta - bytesLeftOnPage) / pageSize;
        if (extraPages > numPages || localOffset - data > pageSize * (numPages - extraPages))
            return false;


        localOffset = b.ptr + goodSize + extraPages * pageSize;
        if (localOffset > readWriteLimit)
        {
            void* newReadWriteLimit = min(cast(void*) data + numPages * pageSize,
                localOffset + extraAllocPages * pageSize);
            assert(newReadWriteLimit > readWriteLimit);
            localExtraAlloc = newReadWriteLimit - readWriteLimit;
            if (!extendMemoryProtection(cast(void*) readWriteLimit, localExtraAlloc))
                return false;
            readWriteLimit = cast(shared(void*)) newReadWriteLimit;
        }

        offset = cast(typeof(offset)) localOffset;
        b = cast(void[]) b.ptr[0 .. b.length + delta];
        return true;
    }
}

///
@system unittest
{
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

version (StdUnittest)
{
    private static void testrw(void[] b) @nogc nothrow
    {
        ubyte* buf = cast(ubyte*) b.ptr;
        buf[0] = 100;
        assert(buf[0] == 100);
        buf[b.length - 1] = 101;
        assert(buf[b.length - 1] == 101);
    }
}

@system @nogc nothrow unittest
{
    static void testAlloc(Allocator)(ref Allocator a) @nogc nothrow
    {
        void[] b1 = a.allocate(1);
        assert(a.getAvailableSize() == 3 * pageSize);
        testrw(b1);
        void[] b2 = a.allocate(2);
        assert(a.getAvailableSize() == 2 * pageSize);
        testrw(b2);
        void[] b3 = a.allocate(pageSize + 1);
        assert(a.getAvailableSize() == 0);

        testrw(b3);
        assert(b1.length == 1);
        assert(b2.length == 2);
        assert(b3.length == pageSize + 1);

        assert(a.offset - a.data == 4 * pageSize);
        void[] b4 = a.allocate(4);
        assert(!b4);

        a.deallocate(b1);
        assert(a.data);
        a.deallocate(b2);
        assert(a.data);
        a.deallocate(b3);
    }

    AscendingPageAllocator a = AscendingPageAllocator(4 * pageSize);
    shared SharedAscendingPageAllocator aa = SharedAscendingPageAllocator(4 * pageSize);

    testAlloc(a);
    testAlloc(aa);
}

@system @nogc nothrow unittest
{
    size_t numPages = 26214;
    AscendingPageAllocator a = AscendingPageAllocator(numPages * pageSize);
    foreach (i; 0 .. numPages)
    {
        void[] buf = a.allocate(pageSize);
        assert(buf.length == pageSize);
        testrw(buf);
        a.deallocate(buf);
    }

    assert(!a.allocate(1));
    assert(a.getAvailableSize() == 0);
}

@system @nogc nothrow unittest
{
    size_t numPages = 26214;
    uint alignment = cast(uint) pageSize;
    AscendingPageAllocator a = AscendingPageAllocator(numPages * pageSize);

    foreach (i; 0 .. numPages)
    {
        void[] buf = a.alignedAllocate(pageSize, alignment);
        assert(buf.length == pageSize);
        testrw(buf);
        a.deallocate(buf);
    }

    assert(!a.allocate(1));
    assert(a.getAvailableSize() == 0);
}

@system @nogc nothrow unittest
{
    static void testAlloc(Allocator)(ref Allocator a) @nogc nothrow
    {
        import std.traits : hasMember;

        size_t numPages = 5;
        uint alignment = cast(uint) pageSize;

        void[] b1 = a.allocate(pageSize / 2);
        assert(b1.length == pageSize / 2);

        void[] b2 = a.alignedAllocate(pageSize / 2, alignment);
        assert(a.expand(b1, pageSize / 2));
        assert(a.expand(b1, 0));
        assert(!a.expand(b1, 1));
        testrw(b1);

        assert(a.expand(b2, pageSize / 2));
        testrw(b2);
        assert(b2.length == pageSize);

        assert(a.getAvailableSize() == pageSize * 3);

        void[] b3 = a.allocate(pageSize / 2);
        assert(a.reallocate(b1, b1.length));
        assert(a.reallocate(b2, b2.length));
        assert(a.reallocate(b3, b3.length));

        assert(b3.length == pageSize / 2);
        testrw(b3);
        assert(a.expand(b3, pageSize / 4));
        testrw(b3);
        assert(a.expand(b3, 0));
        assert(b3.length == pageSize / 2 + pageSize / 4);
        assert(a.expand(b3, pageSize / 4 - 1));
        testrw(b3);
        assert(a.expand(b3, 0));
        assert(b3.length == pageSize - 1);
        assert(a.expand(b3, 2));
        assert(a.expand(b3, 0));
        assert(a.getAvailableSize() == pageSize);
        assert(b3.length == pageSize + 1);
        testrw(b3);

        assert(a.reallocate(b1, b1.length));
        assert(a.reallocate(b2, b2.length));
        assert(a.reallocate(b3, b3.length));

        assert(a.reallocate(b3, 2 * pageSize));
        testrw(b3);
        assert(a.reallocate(b1, pageSize - 1));
        testrw(b1);
        assert(a.expand(b1, 1));
        testrw(b1);
        assert(!a.expand(b1, 1));

        a.deallocate(b1);
        a.deallocate(b2);
        a.deallocate(b3);
    }

    size_t numPages = 5;
    uint alignment = cast(uint) pageSize;
    AscendingPageAllocator a = AscendingPageAllocator(numPages * pageSize);
    shared SharedAscendingPageAllocator aa = SharedAscendingPageAllocator(numPages * pageSize);

    testAlloc(a);
    testAlloc(aa);
}

@system @nogc nothrow unittest
{
    size_t numPages = 21000;
    enum testNum = 100;
    enum allocPages = 10;
    void[][testNum] buf;
    AscendingPageAllocator a = AscendingPageAllocator(numPages * pageSize);

    for (int i = 0; i < numPages; i += testNum * allocPages)
    {
        foreach (j; 0 .. testNum)
        {
            buf[j] = a.allocate(pageSize * allocPages);
            testrw(buf[j]);
        }

        foreach (j; 0 .. testNum)
        {
            a.deallocate(buf[j]);
        }
    }
}

@system @nogc nothrow unittest
{
    size_t numPages = 21000;
    enum testNum = 100;
    enum allocPages = 10;
    void[][testNum] buf;
    shared SharedAscendingPageAllocator a = SharedAscendingPageAllocator(numPages * pageSize);

    for (int i = 0; i < numPages; i += testNum * allocPages)
    {
        foreach (j; 0 .. testNum)
        {
            buf[j] = a.allocate(pageSize * allocPages);
            testrw(buf[j]);
        }

        foreach (j; 0 .. testNum)
        {
            a.deallocate(buf[j]);
        }
    }
}

@system @nogc nothrow unittest
{
    enum numPages = 2;
    AscendingPageAllocator a = AscendingPageAllocator(numPages * pageSize);
    void[] b = a.allocate((numPages + 1) * pageSize);
    assert(b is null);
    b = a.allocate(1);
    assert(b.length == 1);
    assert(a.getAvailableSize() == pageSize);
    a.deallocateAll();
    assert(!a.data && !a.offset);
}

@system @nogc nothrow unittest
{
    enum numPages = 26;
    AscendingPageAllocator a = AscendingPageAllocator(numPages * pageSize);
    uint alignment = cast(uint) ((numPages / 2) * pageSize);
    void[] b = a.alignedAllocate(pageSize, alignment);
    assert(b.length == pageSize);
    testrw(b);
    assert(b.ptr.alignedAt(alignment));
    a.deallocateAll();
    assert(!a.data && !a.offset);
}

@system @nogc nothrow unittest
{
    enum numPages = 10;
    AscendingPageAllocator a = AscendingPageAllocator(numPages * pageSize);
    uint alignment = cast(uint) (2 * pageSize);

    void[] b1 = a.alignedAllocate(pageSize, alignment);
    assert(b1.length == pageSize);
    testrw(b1);
    assert(b1.ptr.alignedAt(alignment));

    void[] b2 = a.alignedAllocate(pageSize, alignment);
    assert(b2.length == pageSize);
    testrw(b2);
    assert(b2.ptr.alignedAt(alignment));

    void[] b3 = a.alignedAllocate(pageSize, alignment);
    assert(b3.length == pageSize);
    testrw(b3);
    assert(b3.ptr.alignedAt(alignment));

    void[] b4 = a.allocate(pageSize);
    assert(b4.length == pageSize);
    testrw(b4);

    assert(a.deallocate(b1));
    assert(a.deallocate(b2));
    assert(a.deallocate(b3));
    assert(a.deallocate(b4));

    a.deallocateAll();
    assert(!a.data && !a.offset);
}

@system unittest
{
    import core.thread : ThreadGroup;
    import std.algorithm.sorting : sort;
    import core.internal.spinlock : SpinLock;

    enum numThreads = 100;
    SpinLock lock = SpinLock(SpinLock.Contention.brief);
    ulong[numThreads] ptrVals;
    size_t count = 0;
    shared SharedAscendingPageAllocator a = SharedAscendingPageAllocator(pageSize * numThreads);

    void fun()
    {
        void[] b = a.allocate(4000);
        assert(b.length == 4000);

        assert(a.expand(b, 96));
        assert(b.length == 4096);

        lock.lock();
        ptrVals[count] = cast(ulong) b.ptr;
        count++;
        lock.unlock();
    }

    auto tg = new ThreadGroup;
    foreach (i; 0 .. numThreads)
    {
        tg.create(&fun);
    }
    tg.joinAll();

    ptrVals[].sort();
    foreach (i; 0 .. numThreads - 1)
    {
        assert(ptrVals[i] + pageSize == ptrVals[i + 1]);
    }
}

@system unittest
{
    import core.thread : ThreadGroup;
    import std.algorithm.sorting : sort;
    import core.internal.spinlock : SpinLock;

    SpinLock lock = SpinLock(SpinLock.Contention.brief);
    enum numThreads = 100;
    void[][numThreads] buf;
    size_t count = 0;
    shared SharedAscendingPageAllocator a = SharedAscendingPageAllocator(2 * pageSize * numThreads);

    void fun()
    {
        enum expand = 96;
        void[] b = a.allocate(pageSize - expand);
        assert(b.length == pageSize - expand);

        assert(a.expand(b, expand));
        assert(b.length == pageSize);

        a.expand(b, pageSize);
        assert(b.length == pageSize || b.length == pageSize * 2);

        lock.lock();
        buf[count] = b;
        count++;
        lock.unlock();
    }

    auto tg = new ThreadGroup;
    foreach (i; 0 .. numThreads)
    {
        tg.create(&fun);
    }
    tg.joinAll();

    sort!((a, b) => a.ptr < b.ptr)(buf[0 .. 100]);
    foreach (i; 0 .. numThreads - 1)
    {
        assert(buf[i].ptr + buf[i].length == buf[i + 1].ptr);
    }
}
