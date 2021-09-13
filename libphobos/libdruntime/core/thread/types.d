/**
 * This module provides types and constants used in thread package.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly, Walter Bright, Alex Rønne Petersen, Martin Nowak
 * Source:    $(DRUNTIMESRC core/thread/osthread.d)
 */

module core.thread.types;

/**
 * Represents the ID of a thread, as returned by $(D Thread.)$(LREF id).
 * The exact type varies from platform to platform.
 */
version (Windows)
    alias ThreadID = uint;
else
version (Posix)
{
    import core.sys.posix.pthread;

    alias ThreadID = pthread_t;
}

struct ll_ThreadData
{
    ThreadID tid;
    version (Windows)
        void delegate() nothrow cbDllUnload;
}

version (GNU)
{
    version (GNU_StackGrowsDown)
        enum isStackGrowingDown = true;
    else
        enum isStackGrowingDown = false;
}
else
{
    // this should be true for most architectures
    enum isStackGrowingDown = true;
}

package
{
    static immutable size_t PAGESIZE;
    version (Posix) static immutable size_t PTHREAD_STACK_MIN;
}

shared static this()
{
    version (Windows)
    {
        import core.sys.windows.winbase;

        SYSTEM_INFO info;
        GetSystemInfo(&info);

        PAGESIZE = info.dwPageSize;
        assert(PAGESIZE < int.max);
    }
    else version (Posix)
    {
        import core.sys.posix.unistd;

        PAGESIZE = cast(size_t)sysconf(_SC_PAGESIZE);
        PTHREAD_STACK_MIN = cast(size_t)sysconf(_SC_THREAD_STACK_MIN);
    }
    else
    {
        static assert(0, "unimplemented");
    }
}
