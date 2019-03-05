//Written in the D programming language

/++
    D header file for NetBSD's extensions to POSIX's time.h.

    Copyright: Copyright 2014
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)

    http://cvsweb.netbsd.org/bsdweb.cgi/~checkout~/src/sys/sys/time.h
 +/
module core.sys.netbsd.time;

public import core.sys.posix.time;

version (NetBSD):

enum CLOCK_REALTIME          = 0;
enum CLOCK_VIRTUAL           = 1;
enum CLOCK_PROF              = 2;
enum CLOCK_MONOTONIC         = 3;
