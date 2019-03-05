///
module std.experimental.allocator.mmap_allocator;

// MmapAllocator
/**

Allocator (currently defined only for Posix and Windows) using
$(D $(LINK2 https://en.wikipedia.org/wiki/Mmap, mmap))
and $(D $(LUCKY munmap)) directly (or their Windows equivalents). There is no
additional structure: each call to $(D allocate(s)) issues a call to
$(D mmap(null, s, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)),
and each call to $(D deallocate(b)) issues $(D munmap(b.ptr, b.length)).
So $(D MmapAllocator) is usually intended for allocating large chunks to be
managed by fine-granular allocators.

*/
struct MmapAllocator
{
    /// The one shared instance.
    static shared MmapAllocator instance;

    /**
    Alignment is page-size and hardcoded to 4096 (even though on certain systems
    it could be larger).
    */
    enum size_t alignment = 4096;

    version (Posix)
    {
        /// Allocator API.
        void[] allocate(size_t bytes) shared
        {
            import core.sys.posix.sys.mman : mmap, MAP_ANON, PROT_READ,
                PROT_WRITE, MAP_PRIVATE, MAP_FAILED;
            if (!bytes) return null;
            auto p = mmap(null, bytes, PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANON, -1, 0);
            if (p is MAP_FAILED) return null;
            return p[0 .. bytes];
        }

        /// Ditto
        bool deallocate(void[] b) shared
        {
            import core.sys.posix.sys.mman : munmap;
            if (b.ptr) munmap(b.ptr, b.length) == 0 || assert(0);
            return true;
        }
    }
    else version (Windows)
    {
        import core.sys.windows.windows : VirtualAlloc, VirtualFree, MEM_COMMIT,
            PAGE_READWRITE, MEM_RELEASE;

        /// Allocator API.
        void[] allocate(size_t bytes) shared
        {
            if (!bytes) return null;
            auto p = VirtualAlloc(null, bytes, MEM_COMMIT, PAGE_READWRITE);
            if (p == null)
                return null;
            return p[0 .. bytes];
        }

        /// Ditto
        bool deallocate(void[] b) shared
        {
            return b.ptr is null || VirtualFree(b.ptr, 0, MEM_RELEASE) != 0;
        }
    }
}

@system unittest
{
    alias alloc = MmapAllocator.instance;
    auto p = alloc.allocate(100);
    assert(p.length == 100);
    alloc.deallocate(p);
}
