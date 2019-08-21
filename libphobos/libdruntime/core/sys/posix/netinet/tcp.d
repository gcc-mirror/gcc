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
module core.sys.posix.netinet.tcp;

private import core.sys.posix.config;

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

//
// Required
//
/*
TCP_NODELAY
*/

version (CRuntime_Glibc)
{
    enum TCP_NODELAY = 1;
}
else version (Darwin)
{
    enum TCP_NODELAY = 1;
}
else version (FreeBSD)
{
    enum TCP_NODELAY = 1;
}
else version (NetBSD)
{
    enum TCP_NODELAY = 1;
}
else version (OpenBSD)
{
    enum TCP_NODELAY = 1;
}
else version (DragonFlyBSD)
{
    enum TCP_NODELAY = 1;
}
else version (Solaris)
{
    enum TCP_NODELAY = 1;
}
else version (linux)
{
    enum TCP_NODELAY = 1;
}
