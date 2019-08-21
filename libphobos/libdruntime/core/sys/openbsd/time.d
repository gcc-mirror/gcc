//Written in the D programming language

/++
    D header file for OpenBSD's extensions to POSIX's time.h.

    Copyright: Copyright 2019
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Iain Buclaw
 +/
module core.sys.openbsd.time;

public import core.sys.posix.time;

version (OpenBSD):

enum CLOCK_REALTIME           = 0;
enum CLOCK_PROCESS_CPUTIME_ID = 2;
enum CLOCK_MONOTONIC          = 3;
enum CLOCK_THREAD_CPUTIME_ID  = 4;
enum CLOCK_UPTIME             = 5;
enum CLOCK_BOOTTIME           = 6;
