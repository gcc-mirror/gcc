//Written in the D programming language

/++
    D header file for FreeBSD's ifaddrs.h.

    Copyright: Copyright 2023 - 2024
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.freebsd.ifaddrs;

version (FreeBSD):
extern(C):
@nogc:
nothrow:

import core.sys.posix.sys.socket : sockaddr;

struct ifaddrs
{
    ifaddrs*  ifa_next;
    char*     ifa_name;
    uint      ifa_flags;
    sockaddr* ifa_addr;
    sockaddr* ifa_netmask;
    alias ifa_broadaddr = ifa_dstaddr;
    sockaddr* ifa_dstaddr;
    void*     ifa_data;
}

struct ifmaddrs
{
    ifmaddrs* ifma_next;
    sockaddr* ifma_name;
    sockaddr* ifma_addr;
    sockaddr* ifma_lladdr;
}

int getifaddrs(ifaddrs**);
void freeifaddrs(ifaddrs*);
int getifmaddrs(ifmaddrs**);
void freeifmaddrs(ifmaddrs*);
