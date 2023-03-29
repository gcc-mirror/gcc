/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.poll;

import core.sys.posix.config;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):
extern (C):
nothrow:
@nogc:

//
// XOpen (XSI)
//
/*
struct pollfd
{
    int     fd;
    short   events;
    short   revents;
}

nfds_t

int poll(pollfd[], nfds_t, int);
*/

version (CRuntime_Glibc)
{
    struct pollfd
    {
        int     fd;
        short   events;
        short   revents;
    }

    alias c_ulong nfds_t;

    int poll(pollfd*, nfds_t, int);
}
else version (Darwin)
{
    struct pollfd
    {
        int     fd;
        short   events;
        short   revents;
    }

    alias uint nfds_t;

    int poll(pollfd*, nfds_t, int);
}
else version (FreeBSD)
{
    alias uint nfds_t;

    struct pollfd
    {
        int     fd;
        short   events;
        short   revents;
    }

    int poll(pollfd*, nfds_t, int);
}
else version (NetBSD)
{
    alias uint nfds_t;

    struct pollfd
    {
        int     fd;
        short   events;
        short   revents;
    }

    int poll(pollfd*, nfds_t, int);
}
else version (OpenBSD)
{
    alias uint nfds_t;

    struct pollfd
    {
        int     fd;
        short   events;
        short   revents;
    }

    int poll(pollfd*, nfds_t, int);
}
else version (DragonFlyBSD)
{
    alias uint nfds_t;

    struct pollfd
    {
        int     fd;
        short   events;
        short   revents;
    }

    int poll(pollfd*, nfds_t, int);
}
else version (Solaris)
{
    alias c_ulong nfds_t;

    struct pollfd
    {
        int     fd;
        short   events;
        short   revents;
    }

    int poll(pollfd*, nfds_t, int);
}
else version (CRuntime_Bionic)
{
    struct pollfd
    {
        int     fd;
        short   events;
        short   revents;
    }

    alias uint nfds_t;

    int poll(pollfd*, nfds_t, c_long);
}
else version (CRuntime_Musl)
{
    struct pollfd
    {
        int     fd;
        short   events;
        short   revents;
    }

    alias uint nfds_t;

    int poll(pollfd*, nfds_t, c_long);
}
else version (CRuntime_UClibc)
{
    struct pollfd
    {
        int     fd;
        short   events;
        short   revents;
    }

    alias c_ulong nfds_t;

    int poll(pollfd*, nfds_t, int);
}
else
{
    static assert(false, "Unsupported platform");
}

/*
POLLIN
POLLRDNORM
POLLRDBAND
POLLPRI
POLLOUT
POLLWRNORM
POLLWRBAND
POLLERR
POLLHUP
POLLNVAL
*/

version (linux)
{
    enum
    {
        POLLIN      = 0x001,
        POLLRDNORM  = 0x040,
        POLLRDBAND  = 0x080,
        POLLPRI     = 0x002,
        POLLOUT     = 0x004,
        POLLWRNORM  = 0x100,
        POLLWRBAND  = 0x200,
        POLLERR     = 0x008,
        POLLHUP     = 0x010,
        POLLNVAL    = 0x020,
    }
}
else version (Darwin)
{
    enum
    {
        POLLIN      = 0x0001,
        POLLPRI     = 0x0002,
        POLLOUT     = 0x0004,
        POLLRDNORM  = 0x0040,
        POLLWRNORM  = POLLOUT,
        POLLRDBAND  = 0x0080,
        POLLWRBAND  = 0x0100,
        POLLEXTEND  = 0x0200,
        POLLATTRIB  = 0x0400,
        POLLNLINK   = 0x0800,
        POLLWRITE   = 0x1000,
        POLLERR     = 0x0008,
        POLLHUP     = 0x0010,
        POLLNVAL    = 0x0020,

        POLLSTANDARD = (POLLIN|POLLPRI|POLLOUT|POLLRDNORM|POLLRDBAND|
                        POLLWRBAND|POLLERR|POLLHUP|POLLNVAL)
    }
}
else version (FreeBSD)
{
    enum
    {
        POLLIN      = 0x0001,
        POLLPRI     = 0x0002,
        POLLOUT     = 0x0004,
        POLLRDNORM  = 0x0040,
        POLLWRNORM  = POLLOUT,
        POLLRDBAND  = 0x0080,
        POLLWRBAND  = 0x0100,
        //POLLEXTEND  = 0x0200,
        //POLLATTRIB  = 0x0400,
        //POLLNLINK   = 0x0800,
        //POLLWRITE   = 0x1000,
        POLLERR     = 0x0008,
        POLLHUP     = 0x0010,
        POLLNVAL    = 0x0020,

        POLLSTANDARD = (POLLIN|POLLPRI|POLLOUT|POLLRDNORM|POLLRDBAND|
        POLLWRBAND|POLLERR|POLLHUP|POLLNVAL)
    }
}
else version (NetBSD)
{
    enum
    {
        POLLIN      = 0x0001,
        POLLPRI     = 0x0002,
        POLLOUT     = 0x0004,
        POLLRDNORM  = 0x0040,
        POLLWRNORM  = POLLOUT,
        POLLRDBAND  = 0x0080,
        POLLWRBAND  = 0x0100,
        //POLLEXTEND  = 0x0200,
        //POLLATTRIB  = 0x0400,
        //POLLNLINK   = 0x0800,
        //POLLWRITE   = 0x1000,
        POLLERR     = 0x0008,
        POLLHUP     = 0x0010,
        POLLNVAL    = 0x0020,

        POLLSTANDARD = (POLLIN|POLLPRI|POLLOUT|POLLRDNORM|POLLRDBAND|
        POLLWRBAND|POLLERR|POLLHUP|POLLNVAL)
    }
}
else version (OpenBSD)
{
    enum
    {
        POLLIN      = 0x0001,
        POLLPRI     = 0x0002,
        POLLOUT     = 0x0004,
        POLLRDNORM  = 0x0040,
        POLLNORM    = POLLRDNORM,
        POLLWRNORM  = POLLOUT,
        POLLRDBAND  = 0x0080,
        POLLWRBAND  = 0x0100,
        POLLERR     = 0x0008,
        POLLHUP     = 0x0010,
        POLLNVAL    = 0x0020,

        POLLSTANDARD = (POLLIN|POLLPRI|POLLOUT|POLLRDNORM|POLLRDBAND|
        POLLWRBAND|POLLERR|POLLHUP|POLLNVAL)
    }
}
else version (DragonFlyBSD)
{
    enum
    {
        POLLIN      = 0x0001,
        POLLPRI     = 0x0002,
        POLLOUT     = 0x0004,
        POLLRDNORM  = 0x0040,
        POLLWRNORM  = POLLOUT,
        POLLRDBAND  = 0x0080,
        POLLWRBAND  = 0x0100,
        //POLLEXTEND  = 0x0200,
        //POLLATTRIB  = 0x0400,
        //POLLNLINK   = 0x0800,
        //POLLWRITE   = 0x1000,
        POLLERR     = 0x0008,
        POLLHUP     = 0x0010,
        POLLNVAL    = 0x0020,

        POLLSTANDARD = (POLLIN|POLLPRI|POLLOUT|POLLRDNORM|POLLRDBAND|
        POLLWRBAND|POLLERR|POLLHUP|POLLNVAL)
    }
}
else version (Solaris)
{
    enum
    {
        POLLIN      = 0x0001,
        POLLPRI     = 0x0002,
        POLLOUT     = 0x0004,
        POLLRDNORM  = 0x0040,
        POLLWRNORM  = POLLOUT,
        POLLRDBAND  = 0x0080,
        POLLWRBAND  = 0x0100,
        POLLERR     = 0x0008,
        POLLHUP     = 0x0010,
        POLLNVAL    = 0x0020,
    }
}
else
{
    static assert(false, "Unsupported platform");
}
