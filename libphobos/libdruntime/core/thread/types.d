/**
 * This module provides types and constants used in thread package.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly, Walter Bright, Alex Rønne Petersen, Martin Nowak
 * Source:    $(DRUNTIMESRC core/thread/types.d)
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
    import core.sys.posix.sys.types : pthread_t;

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
    version (X86) enum isStackGrowingDown = true;
    else version (X86_64) enum isStackGrowingDown = true;
    else static assert(0, "It is undefined how the stack grows on this architecture.");
}

package
{
    version (Posix) static immutable size_t PTHREAD_STACK_MIN;
}

shared static this()
{
    version (Posix)
    {
        import core.sys.posix.unistd : _SC_THREAD_STACK_MIN, sysconf;

        PTHREAD_STACK_MIN = cast(size_t)sysconf(_SC_THREAD_STACK_MIN);
    }
}
