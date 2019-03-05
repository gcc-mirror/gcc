//Written in the D programming language

/++
    D header file for Linux extensions to POSIX's time.h.

    Copyright: Copyright 2014
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.linux.time;

public import core.sys.posix.time;

version (linux):

enum CLOCK_MONOTONIC_RAW      = 4;
enum CLOCK_REALTIME_COARSE    = 5;
enum CLOCK_MONOTONIC_COARSE   = 6;
enum CLOCK_BOOTTIME           = 7;
enum CLOCK_REALTIME_ALARM     = 8;
enum CLOCK_BOOTTIME_ALARM     = 9;
enum CLOCK_SGI_CYCLE          = 10;
enum CLOCK_TAI                = 11;
