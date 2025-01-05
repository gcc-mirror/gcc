// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/experimental/allocator/_mmap_allocator.d)
*/
module std.experimental.allocator.mmap_allocator;

/**
Allocator (currently defined only for Posix and Windows) using
$(D $(LINK2 https://en.wikipedia.org/wiki/Mmap, mmap))
and $(D $(LUCKY munmap)) directly (or their Windows equivalents). There is no
additional structure: each call to `allocate(s)` issues a call to
$(D mmap(null, s, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)),
and each call to `deallocate(b)` issues $(D munmap(b.ptr, b.length)).
So `MmapAllocator` is usually intended for allocating large chunks to be
managed by fine-granular allocators.
*/
struct MmapAllocator
{
    /// The one shared instance.
    static shared const MmapAllocator instance;

    /**
    Alignment is page-size and hardcoded to 4096 (even though on certain systems
    it could be larger).
    */
    enum size_t alignment = 4096;

    version (Posix)
    {
        /// Allocator API.
        pure nothrow @nogc @safe
        void[] allocate(size_t bytes) shared const
        {
            import core.sys.posix.sys.mman : MAP_ANON, PROT_READ,
                PROT_WRITE, MAP_PRIVATE, MAP_FAILED;
            if (!bytes) return null;
            const errnosave = (() @trusted => fakePureErrno())(); // For purity revert changes to errno.
            auto p = (() @trusted => fakePureMmap(null, bytes, PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANON, -1, 0))();
            if (p is MAP_FAILED)
            {
                (() @trusted => fakePureErrno() = errnosave)(); // errno only changed on MAP_FAILED.
                return null;
            }
            return (() @trusted => p[0 .. bytes])();
        }

        /// Ditto
        pure nothrow @nogc
        bool deallocate(void[] b) shared const
        {
            // Because we assert(0) on error we don't need to reset errno for purity.
            if (b.ptr) fakePureMunmap(b.ptr, b.length) == 0 || assert(0);
            return true;
        }

        // Anonymous mmap might be zero-filled on all Posix systems but
        // not all commit to this in the documentation.
        version (linux)
            // http://man7.org/linux/man-pages/man2/mmap.2.html
            package alias allocateZeroed = allocate;
        else version (NetBSD)
            // https://man.netbsd.org/mmap.2
            package alias allocateZeroed = allocate;
        else version (Solaris)
            // https://docs.oracle.com/cd/E88353_01/html/E37841/mmap-2.html
            package alias allocateZeroed = allocate;
        else version (AIX)
            // https://www.ibm.com/support/knowledgecenter/en/ssw_aix_71/com.ibm.aix.basetrf1/mmap.htm
            package alias allocateZeroed = allocate;
    }
    else version (Windows)
    {
        import core.sys.windows.winnt : MEM_COMMIT, PAGE_READWRITE, MEM_RELEASE;

        /// Allocator API.
        pure nothrow @nogc @safe
        void[] allocate(size_t bytes) shared const
        {
            if (!bytes) return null;
            // For purity ensure last-error does not visibly change.
            const lastErrorSave = (() @trusted => GetLastError())();
            auto p = (() @trusted => VirtualAlloc(null, bytes, MEM_COMMIT, PAGE_READWRITE))();
            if (p == null)
            {
                // Last-error only changed if allocation failed.
                (() @trusted => SetLastError(lastErrorSave))();
                return null;
            }
            return (() @trusted => p[0 .. bytes])();
        }

        /// Ditto
        pure nothrow @nogc
        bool deallocate(void[] b) shared const
        {
            const lastErrorSave = GetLastError(); // For purity ensure last-error does not visibly change.
            scope(exit) SetLastError(lastErrorSave);
            return b.ptr is null || VirtualFree(b.ptr, 0, MEM_RELEASE) != 0;
        }

        package alias allocateZeroed = allocate;
    }
}

// pure wrappers around `mmap` and `munmap` because they are used here locally
// solely to perform allocation and deallocation which in this case is `pure`
version (Posix)
extern (C) private pure @system @nogc nothrow
{
    import core.sys.posix.sys.types : off_t;
    pragma(mangle, "fakePureErrnoImpl") ref int fakePureErrno();
    pragma(mangle, "mmap") void* fakePureMmap(void*, size_t, int, int, int, off_t);
    pragma(mangle, "munmap") int fakePureMunmap(void*, size_t);
}

// Pure wrappers around VirtualAlloc/VirtualFree for use here only. Their use is sound
// because when we call them we ensure that last-error is not visibly changed.
version (Windows)
extern (Windows) private pure @system @nogc nothrow
{
    import core.sys.windows.basetsd : SIZE_T;
    import core.sys.windows.windef : BOOL, DWORD;
    import core.sys.windows.winnt : LPVOID, PVOID;

    DWORD GetLastError();
    void SetLastError(DWORD);
    PVOID VirtualAlloc(PVOID, SIZE_T, DWORD, DWORD);
    BOOL VirtualFree(PVOID, SIZE_T, DWORD);
}

pure nothrow @safe @nogc unittest
{
    alias alloc = MmapAllocator.instance;
    auto p = alloc.allocate(100);
    assert(p.length == 100);
    () @trusted { alloc.deallocate(p); p = null; }();
}
