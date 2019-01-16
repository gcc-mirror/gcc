//Written in the D programming language

/++
    D header file for FreeBSD's extensions to POSIX's time.h.

    Copyright: Copyright 2014
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.freebsd.time;

public import core.sys.posix.time;

version (FreeBSD):

enum CLOCK_VIRTUAL           = 1;
enum CLOCK_PROF              = 2;
enum CLOCK_UPTIME            = 5;
enum CLOCK_UPTIME_PRECISE    = 7;
enum CLOCK_UPTIME_FAST       = 8;
enum CLOCK_REALTIME_PRECISE  = 9;
enum CLOCK_REALTIME_FAST     = 10;
enum CLOCK_MONOTONIC_PRECISE = 11;
enum CLOCK_MONOTONIC_FAST    = 12;
enum CLOCK_SECOND            = 13;
