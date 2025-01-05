//Written in the D programming language

/++
    D header file for FreeBSD's ifaddrs.h.

    Copyright: Copyright 2024
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.freebsd.mqueue;

public import core.sys.posix.mqueue;

version (FreeBSD):
extern(C):
@nogc:
nothrow:

int mq_getfd_np(mqd_t mqd);
