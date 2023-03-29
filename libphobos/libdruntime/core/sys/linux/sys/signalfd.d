/**
 * D header file for Linux.
 *
 * Copyright: Copyright Alex Rønne Petersen 2012.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Alex Rønne Petersen
 */
module core.sys.linux.sys.signalfd;

import core.sys.posix.signal;

version (linux):

extern (C):
nothrow:
@nogc:

struct signalfd_siginfo
{
    uint ssi_signo;
    int ssi_errno;
    int ssi_code;
    uint ssi_pid;
    uint ssi_uid;
    int ssi_fd;
    uint ssi_tid;
    uint ssi_band;
    uint ssi_overrun;
    uint ssi_trapno;
    int ssi_status;
    int ssi_int;
    ulong ssi_ptr;
    ulong ssi_utime;
    ulong ssi_stime;
    ulong ssi_addr;
    ubyte[48] __pad;
}

enum SFD_CLOEXEC = 0x80000; // 02000000
enum SFD_NONBLOCK = 0x800; // 04000

int signalfd (int __fd, const(sigset_t)* __mask, int __flags);
