/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly, Alex Rønne Petersen
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.sys.time;

private import core.sys.posix.config;
public import core.sys.posix.sys.types;  // for time_t, suseconds_t
public import core.sys.posix.sys.select; // for fd_set, FD_CLR() FD_ISSET() FD_SET() FD_ZERO() FD_SETSIZE, select()

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (linux) public import core.sys.linux.sys.time;

version (Posix):
extern (C) nothrow @nogc:

//
// XOpen (XSI)
//
/*
struct timeval
{
    time_t      tv_sec;
    suseconds_t tv_usec;
}

struct itimerval
{
    timeval it_interval;
    timeval it_value;
}

ITIMER_REAL
ITIMER_VIRTUAL
ITIMER_PROF

int getitimer(int, itimerval*);
int gettimeofday(timeval*, void*);
int select(int, fd_set*, fd_set*, fd_set*, timeval*); (defined in core.sys.posix.sys.signal)
int setitimer(int, in itimerval*, itimerval*);
int utimes(in char*, ref const(timeval)[2]); // LEGACY
*/

version (CRuntime_Glibc)
{
    struct timeval
    {
        time_t      tv_sec;
        suseconds_t tv_usec;
    }

    struct itimerval
    {
        timeval it_interval;
        timeval it_value;
    }

    enum ITIMER_REAL    = 0;
    enum ITIMER_VIRTUAL = 1;
    enum ITIMER_PROF    = 2;

    int getitimer(int, itimerval*);
    int gettimeofday(timeval*, void*);
    int setitimer(int, in itimerval*, itimerval*);
    int utimes(in char*, ref const(timeval)[2]); // LEGACY
}
else version (CRuntime_Musl)
{
    struct timeval
    {
        time_t      tv_sec;
        suseconds_t tv_usec;
    }
    int gettimeofday(timeval*, void*);
    int utimes(in char*, ref const(timeval)[2]);
}
else version (Darwin)
{
    struct timeval
    {
        time_t      tv_sec;
        suseconds_t tv_usec;
    }

    struct itimerval
    {
        timeval it_interval;
        timeval it_value;
    }

    // non-standard
    struct timezone_t
    {
        int tz_minuteswest;
        int tz_dsttime;
    }

    int getitimer(int, itimerval*);
    int gettimeofday(timeval*, timezone_t*); // timezone_t* is normally void*
    int setitimer(int, in itimerval*, itimerval*);
    int utimes(in char*, ref const(timeval)[2]);
}
else version (FreeBSD)
{
    struct timeval
    {
        time_t      tv_sec;
        suseconds_t tv_usec;
    }

    struct itimerval
    {
        timeval it_interval;
        timeval it_value;
    }

    // non-standard
    struct timezone_t
    {
        int tz_minuteswest;
        int tz_dsttime;
    }

    int getitimer(int, itimerval*);
    int gettimeofday(timeval*, timezone_t*); // timezone_t* is normally void*
    int setitimer(int, in itimerval*, itimerval*);
    int utimes(in char*, ref const(timeval)[2]);
}
else version (NetBSD)
{
    struct timeval
    {
        time_t      tv_sec;
        suseconds_t tv_usec;
    }

    struct itimerval
    {
        timeval it_interval;
        timeval it_value;
    }

    int getitimer(int, itimerval*);
    int gettimeofday(timeval*, void*); // timezone_t* is normally void*
    int setitimer(int, in itimerval*, itimerval*);
    int utimes(in char*, ref const(timeval)[2]);
}
else version (DragonFlyBSD)
{
    struct timeval
    {
        time_t      tv_sec;
        suseconds_t tv_usec;
    }

    struct itimerval
    {
        timeval it_interval;
        timeval it_value;
    }

    // non-standard
    struct timezone_t
    {
        int tz_minuteswest;
        int tz_dsttime;
    }

    int getitimer(int, itimerval*);
    int gettimeofday(timeval*, timezone_t*); // timezone_t* is normally void*
    int setitimer(int, in itimerval*, itimerval*);
    int utimes(in char*, ref const(timeval)[2]);
}
else version (Solaris)
{
    struct timeval
    {
        time_t tv_sec;
        suseconds_t tv_usec;
    }

    struct itimerval
    {
        timeval it_interval;
        timeval it_value;
    }

    int getitimer(int, itimerval*);
    int gettimeofday(timeval*, void*);
    int setitimer(int, in itimerval*, itimerval*);
    int utimes(in char*, ref const(timeval)[2]);
}
else version (CRuntime_Bionic)
{
    struct timeval
    {
        time_t      tv_sec;
        suseconds_t tv_usec;
    }

    struct itimerval
    {
        timeval it_interval;
        timeval it_value;
    }

    struct timezone_t
    {
        int tz_minuteswest;
        int tz_dsttime;
    }

    enum ITIMER_REAL    = 0;
    enum ITIMER_VIRTUAL = 1;
    enum ITIMER_PROF    = 2;

    int getitimer(int, itimerval*);
    int gettimeofday(timeval*, timezone_t*);
    int setitimer(int, in itimerval*, itimerval*);
    int utimes(in char*, ref const(timeval)[2]);
}
else version (CRuntime_UClibc)
{
    struct timeval
    {
        time_t      tv_sec;
        suseconds_t tv_usec;
    }

    struct itimerval
    {
        timeval it_interval;
        timeval it_value;
    }

    enum ITIMER_REAL    = 0;
    enum ITIMER_VIRTUAL = 1;
    enum ITIMER_PROF    = 2;

    int getitimer(int, itimerval*);
    int gettimeofday(timeval*, void*);
    int setitimer(int, in itimerval*, itimerval*);
    int utimes(in char*, ref const(timeval)[2]);
}
else
{
    static assert(false, "Unsupported platform");
}
