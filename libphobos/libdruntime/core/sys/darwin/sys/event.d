/**
 * D header file for Darwin.
 *
 * Copyright: Copyright Martin Nowak 2012. Etienne Cimon 2015.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 */

/*          Copyright Martin Nowak 2012. Etienne Cimon 2015.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.darwin.sys.event;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):
extern (C):
nothrow:
@nogc:

import core.stdc.stdint;    // intptr_t, uintptr_t
import core.sys.posix.time; // timespec

enum : short
{
    EVFILT_READ     =  -1,
    EVFILT_WRITE    =  -2,
    EVFILT_AIO      =  -3,
    EVFILT_VNODE    =  -4,
    EVFILT_PROC     =  -5,
    EVFILT_SIGNAL   =  -6,
    EVFILT_TIMER    =  -7,
    EVFILT_MACHPORT =  -8,
    EVFILT_FS       =  -9,
    EVFILT_USER     = -10,
    EVFILT_VM       = -12,
    EVFILT_EXCEPT   = -15,
}

extern(D) void EV_SET()(kevent_t* kevp, typeof(kevent_t.tupleof) args)
{
    *kevp = kevent_t(args);
}

extern(D) void EV_SET64()(kevent64_s* kevp, typeof(kevent64_s.tupleof) args)
{
    *kevp = kevent64_s(args);
}

struct kevent_t
{
    uintptr_t    ident;
    short       filter;
    ushort       flags;
    uint        fflags;
    intptr_t      data;
    void        *udata;
}

struct kevent64_s
{
    ulong        ident;
    short       filter;
    ushort       flags;
    uint        fflags;
    long          data;
    ulong        udata;
    ulong[2]       ext;
}

enum
{

    KEVENT_FLAG_NONE         = 0x000,
    KEVENT_FLAG_IMMEDIATE    = 0x001,
    KEVENT_FLAG_ERROR_EVENTS = 0x002,

    EV_ADD      = 0x0001,
    EV_DELETE   = 0x0002,
    EV_ENABLE   = 0x0004,
    EV_DISABLE  = 0x0008,

    EV_ONESHOT        = 0x0010,
    EV_CLEAR          = 0x0020,
    EV_RECEIPT        = 0x0040,

    EV_DISPATCH       = 0x0080,
    EV_UDATA_SPECIFIC = 0x0100,

    EV_DISPATCH2      = EV_DISPATCH | EV_UDATA_SPECIFIC,

    EV_VANISHED       = 0x0200,

    EV_SYSFLAGS       = 0xF000,
    EV_FLAG0          = 0x1000,
    EV_FLAG1          = 0x2000,

    EV_EOF      = 0x8000,
    EV_ERROR    = 0x4000,
}

enum
{
    EV_POLL   = EV_FLAG0,
    EV_OOBAND = EV_FLAG1,
}

enum
{
    NOTE_TRIGGER = 0x01000000,

    NOTE_FFNOP      = 0x00000000,
    NOTE_FFAND      = 0x40000000,
    NOTE_FFOR       = 0x80000000,
    NOTE_FFCOPY     = 0xc0000000,
    NOTE_FFCTRLMASK = 0xc0000000,
    NOTE_FFLAGSMASK = 0x00ffffff,

    NOTE_LOWAT      = 0x0001,

    NOTE_OOB        = 0x0002,

    NOTE_DELETE     = 0x0001,
    NOTE_WRITE      = 0x0002,
    NOTE_EXTEND     = 0x0004,
    NOTE_ATTRIB     = 0x0008,
    NOTE_LINK       = 0x0010,
    NOTE_RENAME     = 0x0020,
    NOTE_REVOKE     = 0x0040,
    NOTE_NONE       = 0x0080,
    NOTE_FUNLOCK    = 0x0100,

    NOTE_EXIT        = 0x80000000,
    NOTE_FORK        = 0x40000000,
    NOTE_EXEC        = 0x20000000,
    NOTE_REAP        = 0x10000000,
    NOTE_SIGNAL      = 0x08000000,
    NOTE_EXITSTATUS  = 0x04000000,
    NOTE_EXIT_DETAIL = 0x02000000,
    NOTE_PDATAMASK   = 0x000fffff,
    NOTE_PCTRLMASK   = ~NOTE_PDATAMASK,

    NOTE_EXIT_REPARENTED = 0x00080000,

    NOTE_EXIT_DETAIL_MASK = 0x00070000,
    NOTE_EXIT_DECRYPTFAIL = 0x00010000,
    NOTE_EXIT_MEMORY      = 0x00020000,
    NOTE_EXIT_CSERROR     = 0x00040000,

    NOTE_VM_PRESSURE                  = 0x80000000,
    NOTE_VM_PRESSURE_TERMINATE        = 0x40000000,
    NOTE_VM_PRESSURE_SUDDEN_TERMINATE = 0x20000000,
    NOTE_VM_ERROR                     = 0x10000000,

    NOTE_SECONDS              = 0x00000001,
    NOTE_USECONDS             = 0x00000002,
    NOTE_NSECONDS             = 0x00000004,
    NOTE_ABSOLUTE             = 0x00000008,

    NOTE_LEEWAY               = 0x00000010,
    NOTE_CRITICAL             = 0x00000020,
    NOTE_BACKGROUND           = 0x00000040,
    NOTE_MACH_CONTINUOUS_TIME = 0x00000080,

    NOTE_TRACK      = 0x00000001,
    NOTE_TRACKERR   = 0x00000002,
    NOTE_CHILD      = 0x00000004,
}

int kqueue();
int kevent(int kq, const kevent_t *changelist, int nchanges,
           kevent_t *eventlist, int nevents,
           const timespec *timeout);
int kevent64(int kq,
             const kevent64_s *changelist, int nchanges,
             kevent64_s *eventlist, int nevents,
             uint flags,
             const timespec *timeout);
