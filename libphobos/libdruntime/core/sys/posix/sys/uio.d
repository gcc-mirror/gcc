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
module core.sys.posix.sys.uio;

import core.sys.posix.config;
public import core.sys.posix.sys.types; // for ssize_t

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):
extern (C) nothrow @nogc:
@system:

//
// Required
//
/*
struct iovec
{
    void*  iov_base;
    size_t iov_len;
}

ssize_t // from core.sys.posix.sys.types
size_t  // from core.sys.posix.sys.types

ssize_t readv(int, const scope iovec*, int);
ssize_t writev(int, const scope iovec*, int);
*/

version (CRuntime_Glibc)
{
    struct iovec
    {
        void*  iov_base;
        size_t iov_len;
    }

    ssize_t readv(int, const scope iovec*, int);
    ssize_t writev(int, const scope iovec*, int);
}
else version (Darwin)
{
    struct iovec
    {
        void*  iov_base;
        size_t iov_len;
    }

    ssize_t readv(int, const scope iovec*, int);
    ssize_t writev(int, const scope iovec*, int);
}
else version (FreeBSD)
{
    struct iovec
    {
        void*  iov_base;
        size_t iov_len;
    }

    ssize_t readv(int, const scope iovec*, int);
    ssize_t writev(int, const scope iovec*, int);
}
else version (NetBSD)
{
    struct iovec
    {
        void*  iov_base;
        size_t iov_len;
    }

    ssize_t readv(int, const scope iovec*, int);
    ssize_t writev(int, const scope iovec*, int);
}
else version (OpenBSD)
{
    struct iovec
    {
        void*  iov_base;
        size_t iov_len;
    }

    ssize_t readv(int, const scope iovec*, int);
    ssize_t writev(int, const scope iovec*, int);
}
else version (DragonFlyBSD)
{
    struct iovec
    {
        void*  iov_base;
        size_t iov_len;
    }

    ssize_t readv(int, const scope iovec*, int);
    ssize_t writev(int, const scope iovec*, int);
}
else version (Solaris)
{
    struct iovec
    {
        void* iov_base;
        size_t iov_len;
    }

    ssize_t readv(int, const scope iovec*, int);
    ssize_t writev(int, const scope iovec*, int);
}
else version (CRuntime_Bionic)
{
    struct iovec
    {
        void*  iov_base;
        size_t iov_len;
    }

    int readv(int, const scope iovec*, int);
    int writev(int, const scope iovec*, int);
}
else version (CRuntime_Musl)
{
    struct iovec
    {
        void*  iov_base;
        size_t iov_len;
    }

    ssize_t readv(int, const scope iovec*, int);
    ssize_t writev(int, const scope iovec*, int);
}
else version (CRuntime_UClibc)
{
    struct iovec
    {
        void*  iov_base;
        size_t iov_len;
    }

    ssize_t readv(int, const scope iovec*, int);
    ssize_t writev(int, const scope iovec*, int);
}
else
{
    static assert(false, "Unsupported platform");
}
