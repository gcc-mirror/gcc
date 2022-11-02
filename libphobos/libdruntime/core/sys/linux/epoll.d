/**
 * D header file to interface with the Linux epoll API (http://man7.org/linux/man-pages/man7/epoll.7.html).
 * Available since Linux 2.6
 *
 * Copyright: Copyright Adil Baig 2012.
 * License : $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors  : Adil Baig (github.com/adilbaig)
 */
module core.sys.linux.epoll;

version (linux):

import core.sys.posix.signal : sigset_t;

extern (C):
@nogc:
nothrow:

version (ARM)     version = ARM_Any;
version (AArch64) version = ARM_Any;
version (HPPA)    version = HPPA_Any;
version (MIPS32)  version = MIPS_Any;
version (MIPS64)  version = MIPS_Any;
version (PPC)     version = PPC_Any;
version (PPC64)   version = PPC_Any;
version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;
version (S390)    version = IBMZ_Any;
version (SPARC)   version = SPARC_Any;
version (SPARC64) version = SPARC_Any;
version (SystemZ) version = IBMZ_Any;
version (X86)     version = X86_Any;
version (X86_64)  version = X86_Any;

enum
{
    EPOLL_CLOEXEC  = 0x80000,
    EPOLL_NONBLOCK = 0x800
}

enum
{
    EPOLLIN     = 0x001,
    EPOLLPRI    = 0x002,
    EPOLLOUT    = 0x004,
    EPOLLRDNORM = 0x040,
    EPOLLRDBAND = 0x080,
    EPOLLWRNORM = 0x100,
    EPOLLWRBAND = 0x200,
    EPOLLMSG    = 0x400,
    EPOLLERR    = 0x008,
    EPOLLHUP    = 0x010,
    EPOLLRDHUP  = 0x2000, // since Linux 2.6.17
    EPOLLEXCLUSIVE = 1u << 28, // since Linux 4.5
    EPOLLWAKEUP = 1u << 29,
    EPOLLONESHOT = 1u << 30,
    EPOLLET     = 1u << 31
}

/**
 * Valid opcodes ( "op" parameter ) to issue to epoll_ctl().
 */
enum
{
    EPOLL_CTL_ADD = 1, /// Add a file descriptor to the interface.
    EPOLL_CTL_DEL = 2, /// Remove a file descriptor from the interface.
    EPOLL_CTL_MOD = 3, /// Change file descriptor epoll_event structure.
}

version (X86_Any)
{
    align(1) struct epoll_event
    {
    align(1):
        uint events;
        epoll_data_t data;
    }
}
else version (ARM_Any)
{
    struct epoll_event
    {
        uint events;
        epoll_data_t data;
    }
}
else version (PPC_Any)
{
    struct epoll_event
    {
        uint events;
        epoll_data_t data;
    }
}
else version (HPPA_Any)
{
    struct epoll_event
    {
        uint events;
        epoll_data_t data;
    }
}
else version (MIPS_Any)
{
    struct epoll_event
    {
        uint events;
        epoll_data_t data;
    }
}
else version (RISCV_Any)
{
    struct epoll_event
    {
        uint events;
        epoll_data_t data;
    }
}
else version (SPARC_Any)
{
    struct epoll_event
    {
        uint events;
        epoll_data_t data;
    }
}
else version (IBMZ_Any)
{
    struct epoll_event
    {
        uint events;
        epoll_data_t data;
    }
}
else
{
    static assert(false, "Platform not supported");
}

union epoll_data_t
{
    void *ptr;
    int fd;
    uint u32;
    ulong u64;
}

/**
 * Creates an epoll instance.
 *
 * Params:
 *   size = a hint specifying the number of file descriptors to be associated
 *          with the new instance.  T
 * Returns: an fd for the new instance. The fd returned by epoll_create() should
 *          be closed with close().
 * See_also: epoll_create1 (int flags)
 */
int epoll_create (int size);

/* Same as epoll_create but with an FLAGS parameter.  The unused SIZE
   parameter has been dropped.  */

/**
 * Creates an epoll instance.
 *
 * Params:
 *   flags = a specified flag. If flags is 0, then, other than the fact that the
 *           obsolete size argument is dropped, epoll_create1() is the same as
 *           epoll_create().
 * Returns: an fd for the new instance. The fd returned by epoll_create() should
 *          be closed with close().
 * See_also: epoll_create (int size)
 */
int epoll_create1 (int flags);

/**
 * Manipulate an epoll instance
 *
 * Params:
 *   epfd = an epoll file descriptor instance
 *   op = one of the EPOLL_CTL_* constants
 *   fd = target file descriptor of the operation
 *   event = describes which events the caller is interested in and any
 *           associated user dat
 * Returns: 0 in case of success, -1 in case of error ( the "errno" variable
 *          will contain the specific error code )
 */
int epoll_ctl (int epfd, int op, int fd, epoll_event *event);


/**
 * Wait for events on an epoll instance.
 *
 *
 * Params:
 *   epfd = an epoll file descriptor instance
 *   events = a buffer that will contain triggered events
 *   maxevents = the maximum number of events to be returned ( usually size of
 *               "events" )
 *   timeout = specifies the maximum wait time in milliseconds (-1 == infinite)
 *
 * Returns: the number of triggered events returned in "events" buffer. Or -1 in
 *          case of error with the "errno" variable set to the specific error
 *          code.
 */
int epoll_wait (int epfd, epoll_event *events, int maxevents, int timeout);

/**
 * Wait for events on an epoll instance
 *
 *
 * Params:
 *   epfd = an epoll file descriptor instance
 *   events = a buffer that will contain triggered events
 *   maxevents = the maximum number of events to be returned ( usually size of
 *               "events" )
 *   timeout = specifies the maximum wait time in milliseconds (-1 == infinite)
 *   ss = a signal set. May be specified as `null`, in which case epoll_pwait() is
 *        equivalent to epoll_wait().
 *
 * Returns: the number of triggered events returned in "events" buffer. Or -1 in
 *          case of error with the "errno" variable set to the specific error
 *          code.
 */
int epoll_pwait (int epfd, epoll_event *events, int maxevents, int timeout,
    const sigset_t *ss);
