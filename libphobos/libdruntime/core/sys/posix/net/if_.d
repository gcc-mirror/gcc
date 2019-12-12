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
module core.sys.posix.net.if_;

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
extern (C) nothrow @nogc:

//
// Required
//
/*
struct if_nameindex // renamed to if_nameindex_t
{
    uint    if_index;
    char*   if_name;
}

IF_NAMESIZE

uint            if_nametoindex(in char*);
char*           if_indextoname(uint, char*);
if_nameindex_t* if_nameindex();
void            if_freenameindex(if_nameindex_t*);
*/

version (CRuntime_Glibc)
{
    struct if_nameindex_t
    {
        uint    if_index;
        char*   if_name;
    }

    enum IF_NAMESIZE = 16;

    uint            if_nametoindex(in char*);
    char*           if_indextoname(uint, char*);
    if_nameindex_t* if_nameindex();
    void            if_freenameindex(if_nameindex_t*);
}
else version (Darwin)
{
    struct if_nameindex_t
    {
        uint    if_index;
        char*   if_name;
    }

    enum IF_NAMESIZE = 16;

    uint            if_nametoindex(in char*);
    char*           if_indextoname(uint, char*);
    if_nameindex_t* if_nameindex();
    void            if_freenameindex(if_nameindex_t*);
}
else version (FreeBSD)
{
    struct if_nameindex_t
    {
        uint    if_index;
        char*   if_name;
    }

    enum IF_NAMESIZE = 16;

    uint            if_nametoindex(in char*);
    char*           if_indextoname(uint, char*);
    if_nameindex_t* if_nameindex();
    void            if_freenameindex(if_nameindex_t*);
}
else version (NetBSD)
{
    struct if_nameindex_t
    {
        uint    if_index;
        char*   if_name;
    }

    enum IF_NAMESIZE = 16;

    uint            if_nametoindex(in char*);
    char*           if_indextoname(uint, char*);
    if_nameindex_t* if_nameindex();
    void            if_freenameindex(if_nameindex_t*);
}
else version (OpenBSD)
{
    struct if_nameindex_t
    {
        uint    if_index;
        char*   if_name;
    }

    enum IF_NAMESIZE = 16;

    uint            if_nametoindex(in char*);
    char*           if_indextoname(uint, char*);
    if_nameindex_t* if_nameindex();
    void            if_freenameindex(if_nameindex_t*);
}
else version (DragonFlyBSD)
{
    struct if_nameindex_t
    {
        uint    if_index;
        char*   if_name;
    }

    enum IF_NAMESIZE = 16;

    uint            if_nametoindex(in char*);
    char*           if_indextoname(uint, char*);
    if_nameindex_t* if_nameindex();
    void            if_freenameindex(if_nameindex_t*);
}
else version (CRuntime_Bionic)
{
    enum IF_NAMESIZE = 16;

    uint            if_nametoindex(in char*);
    char*           if_indextoname(uint, char*);
}
else version (CRuntime_UClibc)
{
    struct if_nameindex_t
    {
        uint    if_index;
        char*   if_name;
    }

    enum IF_NAMESIZE = 16;

    uint            if_nametoindex(in char*);
    char*           if_indextoname(uint, char*);
    if_nameindex_t* if_nameindex();
    void            if_freenameindex(if_nameindex_t*);
}
