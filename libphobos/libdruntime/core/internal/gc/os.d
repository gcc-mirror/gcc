/**
 * Contains OS-level routines needed by the garbage collector.
 *
 * Copyright: D Language Foundation 2005 - 2021.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, David Friedman, Sean Kelly, Leandro Lucarella
 */
module core.internal.gc.os;


version (Windows)
{
    import core.sys.windows.winbase : GetCurrentThreadId, VirtualAlloc, VirtualFree;
    import core.sys.windows.winnt : MEM_COMMIT, MEM_RELEASE, MEM_RESERVE, PAGE_READWRITE;

    alias int pthread_t;

    pthread_t pthread_self() nothrow
    {
        return cast(pthread_t) GetCurrentThreadId();
    }

    //version = GC_Use_Alloc_Win32;
}
else version (Posix)
{
    version (OSX)
        version = Darwin;
    else version (iOS)
        version = Darwin;
    else version (TVOS)
        version = Darwin;
    else version (WatchOS)
        version = Darwin;

    public import core.sys.posix.unistd : pid_t;

    static import core.sys.posix.unistd;
    static if (__traits(compiles, core.sys.posix.unistd._Fork))
        public import core.sys.posix.unistd : _Fork;

    static import core.sys.posix.sys.mman;
    static if (__traits(compiles, core.sys.posix.sys.mman.mmap))
        import core.sys.posix.sys.mman : MAP_ANON, MAP_FAILED, MAP_PRIVATE, MAP_SHARED, mmap, munmap, PROT_READ, PROT_WRITE;

    static import core.sys.posix.stdlib;
    static if (__traits(compiles, core.sys.posix.stdlib.valloc))
        import core.sys.posix.stdlib : valloc;

    import core.stdc.stdlib : free, malloc;


    /// Possible results for the wait_pid() function.
    enum ChildStatus
    {
        done, /// The process has finished successfully
        running, /// The process is still running
        error /// There was an error waiting for the process
    }

    /**
     * Wait for a process with PID pid to finish.
     *
     * If block is false, this function will not block, and return ChildStatus.running if
     * the process is still running. Otherwise it will return always ChildStatus.done
     * (unless there is an error, in which case ChildStatus.error is returned).
     */
    ChildStatus wait_pid(pid_t pid, bool block = true) nothrow @nogc
    {
        import core.exception : onForkError;
        import core.stdc.errno : ECHILD, EINTR, errno;
        import core.sys.posix.sys.wait : waitpid, WNOHANG;

        int status = void;
        pid_t waited_pid = void;
        // In the case where we are blocking, we need to consider signals
        // arriving while we wait, and resume the waiting if EINTR is returned
        do {
            errno = 0;
            waited_pid = waitpid(pid, &status, block ? 0 : WNOHANG);
        }
        while (waited_pid == -1 && errno == EINTR);
        if (waited_pid == 0)
            return ChildStatus.running;
        if (errno ==  ECHILD)
            return ChildStatus.done; // someone called posix.syswait
        if (waited_pid != pid || status != 0)
            onForkError();
        return ChildStatus.done;
    }

    version (DragonFlyBSD)
        version = GCSignalsUnblock;
    version (FreeBSD)
        version = GCSignalsUnblock;
    version (Solaris)
        version = GCSignalsUnblock;

    //version = GC_Use_Alloc_MMap;
}
else
{
    import core.stdc.stdlib : free, malloc;

    //version = GC_Use_Alloc_Malloc;
}

/+
static if (is(typeof(VirtualAlloc)))
    version = GC_Use_Alloc_Win32;
else static if (is(typeof(mmap)))
    version = GC_Use_Alloc_MMap;
else static if (is(typeof(valloc)))
    version = GC_Use_Alloc_Valloc;
else static if (is(typeof(malloc)))
    version = GC_Use_Alloc_Malloc;
else static assert(false, "No supported allocation methods available.");
+/

version (CoreDdoc)
{
    /**
     * Map memory.
     */
    void *os_mem_map(size_t nbytes) nothrow @nogc
    {
        return null;
    }

    /**
     * Unmap memory allocated with os_mem_map()
     * Returns:
     *      0       success
     *      !=0     failure
     */
    int os_mem_unmap(void *base, size_t nbytes) nothrow @nogc
    {
        return 0;
    }

    /**
     * Map memory that will be shared by child processes.
     * Note: only available if the OS supports this feature. Use `AllocSupportsShared` to test.
     */
    void *os_mem_map_shared(size_t nbytes) nothrow @nogc
    {
        return null;
    }

    /**
     * Unmap memory allocated with os_mem_map_shared()
     * Returns:
     *      0       success
     *      !=0     failure
     */
    int os_mem_unmap_shared(void *base, size_t nbytes) nothrow @nogc
    {
        return 0;
    }
}
else static if (is(typeof(VirtualAlloc))) // version (GC_Use_Alloc_Win32)
{
    void *os_mem_map(size_t nbytes) nothrow @nogc
    {
        return VirtualAlloc(null, nbytes, MEM_RESERVE | MEM_COMMIT,
                PAGE_READWRITE);
    }

    int os_mem_unmap(void *base, size_t nbytes) nothrow @nogc
    {
        return cast(int)(VirtualFree(base, 0, MEM_RELEASE) == 0);
    }
}
else static if (is(typeof(mmap)))  // else version (GC_Use_Alloc_MMap)
{
    void *os_mem_map(size_t nbytes) nothrow @nogc
    {
        void* p = mmap(null, nbytes, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        return (p == MAP_FAILED) ? null : p;
    }

    int os_mem_unmap(void *base, size_t nbytes) nothrow @nogc
    {
        return munmap(base, nbytes);
    }

    void *os_mem_map_shared(size_t nbytes) nothrow @nogc
    {
        void* p = mmap(null, nbytes, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0);
        return (p == MAP_FAILED) ? null : p;
    }

    int os_mem_unmap_shared(void *base, size_t nbytes) nothrow @nogc
    {
        return munmap(base, nbytes);
    }
}
else static if (is(typeof(valloc))) // else version (GC_Use_Alloc_Valloc)
{
    void *os_mem_map(size_t nbytes) nothrow @nogc
    {
        return valloc(nbytes);
    }


    int os_mem_unmap(void *base, size_t nbytes) nothrow @nogc
    {
        free(base);
        return 0;
    }
}
else static if (is(typeof(malloc))) // else version (GC_Use_Alloc_Malloc)
{
    // NOTE: This assumes malloc granularity is at least (void*).sizeof.  If
    //       (req_size + PAGESIZE) is allocated, and the pointer is rounded up
    //       to PAGESIZE alignment, there will be space for a void* at the end
    //       after PAGESIZE bytes used by the GC.

    import core.internal.gc.impl.conservative.gc;


    const size_t PAGE_MASK = PAGESIZE - 1;


    void *os_mem_map(size_t nbytes) nothrow @nogc
    {   byte *p, q;
        p = cast(byte *) malloc(nbytes + PAGESIZE);
        if (!p)
            return null;
        q = p + ((PAGESIZE - ((cast(size_t) p & PAGE_MASK))) & PAGE_MASK);
        * cast(void**)(q + nbytes) = p;
        return q;
    }


    int os_mem_unmap(void *base, size_t nbytes) nothrow @nogc
    {
        free( *cast(void**)( cast(byte*) base + nbytes ) );
        return 0;
    }
}
else
{
    static assert(false, "No supported allocation methods available.");
}

/**
* Indicates if an allocation method supports sharing between processes.
*/
enum AllocSupportsShared = __traits(compiles, os_mem_map_shared);

/**
   Check for any kind of memory pressure.

   Params:
      mapped = the amount of memory mapped by the GC in bytes
   Returns:
       true if memory is scarce
*/
// TODO: get virtual mem sizes and current usage from OS
// TODO: compare current RSS and avail. physical memory
bool isLowOnMem(size_t mapped) nothrow @nogc
{
    version (Windows)
    {
        import core.sys.windows.winbase : GlobalMemoryStatusEx, MEMORYSTATUSEX;

        MEMORYSTATUSEX stat;
        stat.dwLength = stat.sizeof;
        const success = GlobalMemoryStatusEx(&stat) != 0;
        assert(success, "GlobalMemoryStatusEx() failed");
        if (!success)
            return false;

        // dwMemoryLoad is the 'approximate percentage of physical memory that is in use'
        // https://docs.microsoft.com/en-us/windows/win32/api/sysinfoapi/ns-sysinfoapi-memorystatusex
        const percentPhysicalRAM = stat.ullTotalPhys / 100;
        return (stat.dwMemoryLoad >= 95 && mapped > percentPhysicalRAM)
            || (stat.dwMemoryLoad >= 90 && mapped > 10 * percentPhysicalRAM);
    }
    else
    {
        enum GB = 2 ^^ 30;
        version (D_LP64)
            return false;
        else version (Darwin)
        {
            // 80 % of available 4GB is used for GC (excluding malloc and mmap)
            enum size_t limit = 4UL * GB * 8 / 10;
            return mapped > limit;
        }
        else
        {
            // be conservative and assume 3GB
            enum size_t limit = 3UL * GB * 8 / 10;
            return mapped > limit;
        }
    }
}

/**
   Get the size of available physical memory

   Params:
       avail = if supported on the current platform, return the currently unused memory
               rather than the installed physical memory
   Returns:
       size of installed physical RAM
*/
version (Windows)
{
    ulong os_physical_mem(bool avail) nothrow @nogc
    {
        import core.sys.windows.winbase : GlobalMemoryStatus, MEMORYSTATUS;
        MEMORYSTATUS stat;
        GlobalMemoryStatus(&stat);
        return avail ? stat.dwAvailPhys : stat.dwTotalPhys; // limited to 4GB for Win32
    }
}
else version (Darwin)
{
    extern (C) int sysctl(const int* name, uint namelen, void* oldp, size_t* oldlenp, const void* newp, size_t newlen) @nogc nothrow;
    ulong os_physical_mem(bool avail) nothrow @nogc
    {
        enum
        {
            CTL_HW = 6,
            HW_MEMSIZE = 24,
        }
        int[2] mib = [ CTL_HW, HW_MEMSIZE ];
        ulong system_memory_bytes;
        size_t len = system_memory_bytes.sizeof;
        if (sysctl(mib.ptr, 2, &system_memory_bytes, &len, null, 0) != 0)
            return 0;
        return system_memory_bytes;
    }
}
else version (Posix)
{
    ulong os_physical_mem(bool avail) nothrow @nogc
    {
        static import core.sys.posix.unistd;
        import core.sys.posix.unistd : _SC_PAGESIZE, _SC_PHYS_PAGES, sysconf;
        const pageSize = sysconf(_SC_PAGESIZE);
        static if (__traits(compiles, core.sys.posix.unistd._SC_AVPHYS_PAGES)) // not available on all platforms
        {
            import core.sys.posix.unistd : _SC_AVPHYS_PAGES;
            const sc = avail ? _SC_AVPHYS_PAGES : _SC_PHYS_PAGES;
        }
        else
        {
            const sc = _SC_PHYS_PAGES;
        }
        const pages = sysconf(sc);
        return pageSize * pages;
    }
}

/**
   The GC signals might be blocked by `fork` when the atfork prepare
   handler is invoked. This guards us from the scenario where we are
   waiting for a GC action in another thread to complete, and that thread
   decides to call thread_suspendAll, then we must be able to response to
   that request, otherwise we end up in a deadlock situation.
 */
version (GCSignalsUnblock)
{
    void os_unblock_gc_signals() nothrow @nogc
    {
        import core.sys.posix.signal : pthread_sigmask, sigaddset, sigemptyset, sigset_t, SIG_UNBLOCK;
        import core.thread : thread_getGCSignals;

        int suspendSignal = void, resumeSignal = void;
        thread_getGCSignals(suspendSignal, resumeSignal);

        sigset_t set;
        sigemptyset(&set);
        sigaddset(&set, suspendSignal);
        sigaddset(&set, resumeSignal);

        auto sigmask = pthread_sigmask(SIG_UNBLOCK, &set, null);
        assert(sigmask == 0, "failed to unblock GC signals");
    }
}
