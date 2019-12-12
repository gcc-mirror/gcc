//Written in the D programming language

/++
    D header file for Solaris's extensions to POSIX's time.h.

    Copyright: Copyright 2014
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Kai Nacke
 +/
module core.sys.solaris.time;

public import core.sys.posix.time;

version (Solaris):

enum CLOCK_VIRTUAL           = 1;
enum CLOCK_HIGHRES           = CLOCK_MONOTONIC;
enum CLOCK_PROF              = CLOCK_THREAD_CPUTIME_ID;
