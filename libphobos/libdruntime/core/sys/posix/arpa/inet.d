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
module core.sys.posix.arpa.inet;

import core.sys.posix.config;
public import core.stdc.inttypes; // for uint32_t, uint16_t
public import core.sys.posix.sys.socket; // for socklen_t

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
NOTE: The following must must be defined in core.sys.posix.arpa.inet to break
      a circular import: in_port_t, in_addr_t, struct in_addr, INET_ADDRSTRLEN.

in_port_t // from core.sys.posix.netinet.in_
in_addr_t // from core.sys.posix.netinet.in_

struct in_addr  // from core.sys.posix.netinet.in_
INET_ADDRSTRLEN // from core.sys.posix.netinet.in_

uint32_t // from core.stdc.inttypes
uint16_t // from core.stdc.inttypes

uint32_t htonl(uint32_t);
uint16_t htons(uint16_t);
uint32_t ntohl(uint32_t);
uint16_t ntohs(uint16_t);

in_addr_t inet_addr(const scope char*);
char*     inet_ntoa(in_addr);
// per spec: const char* inet_ntop(int, const void*, char*, socklen_t);
char*     inet_ntop(int, const scope void*, char*, socklen_t);
int       inet_pton(int, const scope char*, void*);
*/

version (CRuntime_Glibc)
{
    alias uint16_t in_port_t;
    alias uint32_t in_addr_t;

    struct in_addr
    {
        in_addr_t s_addr;
    }

    @trusted pure
    {
    uint32_t htonl(uint32_t);
    uint16_t htons(uint16_t);
    uint32_t ntohl(uint32_t);
    uint16_t ntohs(uint16_t);
    }

    in_addr_t       inet_addr(const scope char*);
    char*           inet_ntoa(in_addr);
    const(char)*    inet_ntop(int, const scope void*, char*, socklen_t);
    int             inet_pton(int, const scope char*, void*);
}
else version (Darwin)
{
    alias uint16_t in_port_t;
    alias uint32_t in_addr_t;

    struct in_addr
    {
        in_addr_t s_addr;
    }

    @trusted pure
    {
    uint32_t htonl(uint32_t);
    uint16_t htons(uint16_t);
    uint32_t ntohl(uint32_t);
    uint16_t ntohs(uint16_t);
    }

    in_addr_t       inet_addr(const scope char*);
    char*           inet_ntoa(in_addr);
    const(char)*    inet_ntop(int, const scope void*, char*, socklen_t);
    int             inet_pton(int, const scope char*, void*);
}
else version (FreeBSD)
{
    alias uint16_t in_port_t;
    alias uint32_t in_addr_t;

    struct in_addr
    {
        in_addr_t s_addr;
    }

    @trusted pure
    {
    uint32_t htonl(uint32_t);
    uint16_t htons(uint16_t);
    uint32_t ntohl(uint32_t);
    uint16_t ntohs(uint16_t);
    }

    in_addr_t       inet_addr(const scope char*);
    char*           inet_ntoa(in_addr);
    const(char)*    inet_ntop(int, const scope void*, char*, socklen_t);
    int             inet_pton(int, const scope char*, void*);
}
else version (NetBSD)
{
    alias uint16_t in_port_t;
    alias uint32_t in_addr_t;

    struct in_addr
    {
        in_addr_t s_addr;
    }

    @trusted pure
    {
    uint32_t htonl(uint32_t);
    uint16_t htons(uint16_t);
    uint32_t ntohl(uint32_t);
    uint16_t ntohs(uint16_t);
    }

    in_addr_t       inet_addr(const scope char*);
    char*           inet_ntoa(in_addr);
    const(char)*    inet_ntop(int, const scope void*, char*, socklen_t);
    int             inet_pton(int, const scope char*, void*);
}
else version (OpenBSD)
{
    alias uint16_t in_port_t;
    alias uint32_t in_addr_t;

    struct in_addr
    {
        in_addr_t s_addr;
    }

    @safe pure extern (D)
    {
        version (BigEndian)
        {
            uint32_t htonl(uint32_t x) { return x; }
            uint16_t htons(uint16_t x) { return x; }
        }
        else
        {
            import core.bitop : bswap, byteswap;

            uint32_t htonl(uint32_t x) { return bswap(x); }
            uint16_t htons(uint16_t x) { return byteswap(x); }
        }
        alias ntohl = htonl;
        alias ntohs = htons;
    }

    in_addr_t       inet_addr(const scope char*);
    char*           inet_ntoa(in_addr);
    const(char)*    inet_ntop(int, const scope void*, char*, socklen_t);
    int             inet_pton(int, const scope char*, void*);
}
else version (DragonFlyBSD)
{
    alias uint16_t in_port_t;
    alias uint32_t in_addr_t;

    struct in_addr
    {
        in_addr_t s_addr;
    }

    @trusted pure
    {
    uint32_t htonl(uint32_t);
    uint16_t htons(uint16_t);
    uint32_t ntohl(uint32_t);
    uint16_t ntohs(uint16_t);
    }

    in_addr_t       inet_addr(const scope char*);
    char*           inet_ntoa(in_addr);
    const(char)*    inet_ntop(int, const scope void*, char*, socklen_t);
    int             inet_pton(int, const scope char*, void*);
}
else version (Solaris)
{
    alias uint16_t in_port_t;
    alias uint32_t in_addr_t;

    struct in_addr
    {
        in_addr_t s_addr;
    }

    @trusted pure
    {
    uint32_t htonl(uint32_t);
    uint16_t htons(uint16_t);
    uint32_t ntohl(uint32_t);
    uint16_t ntohs(uint16_t);
    }

    in_addr_t       inet_addr(const scope char*);
    char*           inet_ntoa(in_addr);
    const(char)*    inet_ntop(int, const scope void*, char*, socklen_t);
    int             inet_pton(int, const scope char*, void*);
}
else version (CRuntime_Bionic)
{
    alias uint32_t in_addr_t;

    struct in_addr
    {
        in_addr_t s_addr;
    }

    @safe pure extern (D)
    {
        version (BigEndian)
        {
            uint32_t htonl(uint32_t x) { return x; }
            uint16_t htons(uint16_t x) { return x; }
        }
        else
        {
            import core.bitop : bswap, byteswap;

            uint32_t htonl(uint32_t x) { return bswap(x); }
            uint16_t htons(uint16_t x) { return byteswap(x); }
        }
        alias ntohl = htonl;
        alias ntohs = htons;
    }

    in_addr_t       inet_addr(const scope char*);
    char*           inet_ntoa(in_addr);
    const(char)*    inet_ntop(int, const scope void*, char*, size_t);
    int             inet_pton(int, const scope char*, void*);
}
else version (CRuntime_Musl)
{
    alias uint16_t in_port_t;
    alias uint32_t in_addr_t;

    struct in_addr
    {
        in_addr_t s_addr;
    }

    @trusted pure
    {
    uint32_t htonl(uint32_t);
    uint16_t htons(uint16_t);
    uint32_t ntohl(uint32_t);
    uint16_t ntohs(uint16_t);
    }

    in_addr_t       inet_addr(const scope char*);
    char*           inet_ntoa(in_addr);
    const(char)*    inet_ntop(int, const scope void*, char*, socklen_t);
    int             inet_pton(int, const scope char*, void*);
}
else version (CRuntime_UClibc)
{
    alias uint16_t in_port_t;
    alias uint32_t in_addr_t;

    struct in_addr
    {
        in_addr_t s_addr;
    }

    @trusted pure
    {
    uint32_t htonl(uint32_t);
    uint16_t htons(uint16_t);
    uint32_t ntohl(uint32_t);
    uint16_t ntohs(uint16_t);
    }

    in_addr_t       inet_addr(const scope char*);
    char*           inet_ntoa(in_addr);
    const(char)*    inet_ntop(int, const scope void*, char*, socklen_t);
    int             inet_pton(int, const scope char*, void*);
}

/*
NOTE: The following must must be defined in core.sys.posix.arpa.inet to break
      a circular import: INET6_ADDRSTRLEN.

INET6_ADDRSTRLEN // from core.sys.posix.netinet.in_
*/

enum INET_ADDRSTRLEN  = 16;
enum INET6_ADDRSTRLEN = 46;
