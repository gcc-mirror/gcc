/**
 * D header file for NetBSD.
 *
 * Copyright: Copyright Martin Nowak 2012.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 *
 * http://cvsweb.netbsd.org/bsdweb.cgi/~checkout~/src/sys/sys/event.h
 */

/*          Copyright Martin Nowak 2012.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.netbsd.sys.event;

version (NetBSD):
extern (C):

import core.stdc.stdint;    // intptr_t, uintptr_t
import core.sys.posix.time; // timespec


enum
{
    EVFILT_READ     =  0,
    EVFILT_WRITE    =  1,
    EVFILT_AIO      =  2,
    EVFILT_VNODE    =  3,
    EVFILT_PROC     =  4,
    EVFILT_SIGNAL   =  5,
    EVFILT_TIMER    =  6,
    EVFILT_SYSCOUNT =  7
}

extern(D) void EV_SET(kevent_t* kevp, typeof(kevent_t.tupleof) args)
{
    *kevp = kevent_t(args);
}

struct kevent_t
{
    uintptr_t    ident;
    uint        filter;
    uint        flags;
    uint        fflags;
    ulong       data;
    void        *udata;
}

enum
{
    /* actions */
    EV_ADD      = 0x0001,
    EV_DELETE   = 0x0002,
    EV_ENABLE   = 0x0004,
    EV_DISABLE  = 0x0008,

    /* flags */
    EV_ONESHOT  = 0x0010,
    EV_CLEAR    = 0x0020,

    EV_SYSFLAGS = 0xF000,
    EV_FLAG1    = 0x2000,

    /* returned values */
    EV_EOF      = 0x8000,
    EV_ERROR    = 0x4000
}

enum
{
    /*
     * data/hint flags for EVFILT_{READ|WRITE}, shared with userspace
     */
    NOTE_LOWAT      = 0x0001,

    /*
     * data/hint flags for EVFILT_VNODE, shared with userspace
     */
    NOTE_DELETE     = 0x0001,
    NOTE_WRITE      = 0x0002,
    NOTE_EXTEND     = 0x0004,
    NOTE_ATTRIB     = 0x0008,
    NOTE_LINK       = 0x0010,
    NOTE_RENAME     = 0x0020,
    NOTE_REVOKE     = 0x0040,

    /*
     * data/hint flags for EVFILT_PROC, shared with userspace
     */
    NOTE_EXIT       = 0x80000000,
    NOTE_FORK       = 0x40000000,
    NOTE_EXEC       = 0x20000000,
    NOTE_PCTRLMASK  = 0xf0000000,
    NOTE_PDATAMASK  = 0x000fffff,

    /* additional flags for EVFILT_PROC */
    NOTE_TRACK      = 0x00000001,
    NOTE_TRACKERR   = 0x00000002,
    NOTE_CHILD      = 0x00000004

}

int kqueue();
int __kevent50(int kq, const kevent_t *changelist, int nchanges,
           kevent_t *eventlist, int nevents,
           const timespec *timeout);
alias kevent = __kevent50;
