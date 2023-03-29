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
module core.sys.posix.utime;

import core.sys.posix.config;
public import core.sys.posix.sys.types; // for time_t

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
// Required
//
/*
struct utimbuf
{
    time_t  actime;
    time_t  modtime;
}

int utime(const scope char*, const scope utimbuf*);
*/

version (CRuntime_Glibc)
{
    struct utimbuf
    {
        time_t  actime;
        time_t  modtime;
    }

    int utime(const scope char*, const scope utimbuf*);
}
else version (CRuntime_Musl)
{
    struct utimbuf
    {
        time_t  actime;
        time_t  modtime;
    }

    pragma(mangle, muslRedirTime64Mangle!("utime", "__utime64"))
    int utime(const scope char*, const scope utimbuf*);
}
else version (Darwin)
{
    struct utimbuf
    {
        time_t  actime;
        time_t  modtime;
    }

    int utime(const scope char*, const scope utimbuf*);
}
else version (FreeBSD)
{
    struct utimbuf
    {
        time_t  actime;
        time_t  modtime;
    }

    int utime(const scope char*, const scope utimbuf*);
}
else version (NetBSD)
{
    struct utimbuf
    {
        time_t  actime;
        time_t  modtime;
    }

    int utime(const scope char*, const scope utimbuf*);
}
else version (OpenBSD)
{
    struct utimbuf
    {
        time_t  actime;
        time_t  modtime;
    }

    int utime(const scope char*, const scope utimbuf*);
}
else version (DragonFlyBSD)
{
    struct utimbuf
    {
        time_t  actime;
        time_t  modtime;
    }

    int utime(const scope char*, const scope utimbuf*);
}
else version (Solaris)
{
    struct utimbuf
    {
        time_t  actime;
        time_t  modtime;
    }

    int utime(const scope char*, const scope utimbuf*);
}
else version (CRuntime_Bionic)
{
    struct utimbuf
    {
        time_t  actime;
        time_t  modtime;
    }

    int utime(const scope char*, const scope utimbuf*);
}
else version (CRuntime_UClibc)
{
    struct utimbuf
    {
        time_t  actime;
        time_t  modtime;
    }

    int utime(const scope char*, const scope utimbuf*);
}
