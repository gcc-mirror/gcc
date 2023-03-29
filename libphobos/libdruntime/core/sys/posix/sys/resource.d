/**
 * D header file for POSIX.
 *
 * Copyright: Copyright (c) 2013 Lars Tandle Kyllingstad.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Lars Tandle Kyllingstad
 * Standards: The Open Group Base Specifications Issue 7, IEEE Std 1003.1-2008
 */
module core.sys.posix.sys.resource;
version (Posix):

public import core.sys.posix.sys.time;
public import core.sys.posix.sys.types: id_t;
import core.sys.posix.config;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

nothrow @nogc extern(C):

//
// XOpen (XSI)
//
// http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_resource.h.html
/*
enum
{
    PRIO_PROCESS,
    PRIO_PGRP,
    PRIO_USER,
}

alias ulong rlim_t;

enum
{
    RLIM_INFINITY,
    RLIM_SAVED_MAX,
    RLIM_SAVED_CUR,
}

enum
{
    RUSAGE_SELF,
    RUSAGE_CHILDREN,
}

struct rusage
{
    timeval ru_utime;
    timeval ru_stime;
}

enum
{
    RLIMIT_CORE,
    RLIMIT_CPU,
    RLIMIT_DATA,
    RLIMIT_FSIZE,
    RLIMIT_NOFILE,
    RLIMIT_STACK,
    RLIMIT_AS,
}
*/

version (linux)
{
    enum
    {
        PRIO_PROCESS = 0,
        PRIO_PGRP    = 1,
        PRIO_USER    = 2,
    }

    static if (__USE_FILE_OFFSET64)
         alias ulong rlim_t;
    else
         alias c_ulong rlim_t;

    static if (__USE_FILE_OFFSET64)
        enum RLIM_INFINITY = 0xffffffffffffffffUL;
    else
        enum RLIM_INFINITY = cast(c_ulong)(~0UL);

    enum RLIM_SAVED_MAX = RLIM_INFINITY;
    enum RLIM_SAVED_CUR = RLIM_INFINITY;

    enum
    {
        RUSAGE_SELF     =  0,
        RUSAGE_CHILDREN = -1,
        RUSAGE_THREAD = 1
    }

    struct rusage
    {
        timeval ru_utime;
        timeval ru_stime;
        c_long ru_maxrss;
        c_long ru_ixrss;
        c_long ru_idrss;
        c_long ru_isrss;
        c_long ru_minflt;
        c_long ru_majflt;
        c_long ru_nswap;
        c_long ru_inblock;
        c_long ru_oublock;
        c_long ru_msgsnd;
        c_long ru_msgrcv;
        c_long ru_nsignals;
        c_long ru_nvcsw;
        c_long ru_nivcsw;
        version (CRuntime_Musl)
            c_long[16] __reserved;
    }

    enum
    {
        RLIMIT_CORE   = 4,
        RLIMIT_CPU    = 0,
        RLIMIT_DATA   = 2,
        RLIMIT_FSIZE  = 1,
        RLIMIT_NOFILE = 7,
        RLIMIT_STACK  = 3,
        RLIMIT_AS     = 9,
    }
}
else version (Darwin)
{
    enum
    {
        PRIO_PROCESS = 0,
        PRIO_PGRP    = 1,
        PRIO_USER    = 2,
    }

    alias ulong rlim_t;

    enum
    {
        RLIM_INFINITY  = ((cast(ulong) 1 << 63) - 1),
        RLIM_SAVED_MAX = RLIM_INFINITY,
        RLIM_SAVED_CUR = RLIM_INFINITY,
    }

    enum
    {
        RUSAGE_SELF     =  0,
        RUSAGE_CHILDREN = -1,
    }

    struct rusage
    {
        timeval ru_utime;
        timeval ru_stime;
        c_long[14] ru_opaque;
    }

    enum
    {
        RLIMIT_CORE   = 4,
        RLIMIT_CPU    = 0,
        RLIMIT_DATA   = 2,
        RLIMIT_FSIZE  = 1,
        RLIMIT_NOFILE = 8,
        RLIMIT_STACK  = 3,
        RLIMIT_AS     = 5,
    }
}
else version (FreeBSD)
{
    enum
    {
        PRIO_PROCESS = 0,
        PRIO_PGRP    = 1,
        PRIO_USER    = 2,
    }

    alias long rlim_t;

    enum
    {
        RLIM_INFINITY   = (cast(rlim_t)((cast(ulong) 1 << 63) - 1)),
        RLIM_SAVED_MAX  = RLIM_INFINITY,
        RLIM_SAVED_CUR  = RLIM_INFINITY,
    }

    enum
    {
        RUSAGE_SELF     =  0,
        RUSAGE_CHILDREN = -1,
    }

    struct rusage
    {
        timeval ru_utime;
        timeval ru_stime;
        c_long ru_maxrss;
        alias ru_ixrss ru_first;
        c_long ru_ixrss;
        c_long ru_idrss;
        c_long ru_isrss;
        c_long ru_minflt;
        c_long ru_majflt;
        c_long ru_nswap;
        c_long ru_inblock;
        c_long ru_oublock;
        c_long ru_msgsnd;
        c_long ru_msgrcv;
        c_long ru_nsignals;
        c_long ru_nvcsw;
        c_long ru_nivcsw;
        alias ru_nivcsw ru_last;
    }

    enum
    {
        RLIMIT_CORE   =  4,
        RLIMIT_CPU    =  0,
        RLIMIT_DATA   =  2,
        RLIMIT_FSIZE  =  1,
        RLIMIT_NOFILE =  8,
        RLIMIT_STACK  =  3,
        RLIMIT_AS     = 10,
    }
}
else version (NetBSD)
{
    enum
    {
        PRIO_PROCESS = 0,
        PRIO_PGRP    = 1,
        PRIO_USER    = 2,
    }

    alias long rlim_t;

    enum
    {
        RLIM_INFINITY   = (cast(rlim_t)((cast(ulong) 1 << 63) - 1)),
        RLIM_SAVED_MAX = RLIM_INFINITY,
        RLIM_SAVED_CUR = RLIM_INFINITY,
    }

    enum
    {
        RUSAGE_SELF     =  0,
        RUSAGE_CHILDREN = -1,
    }

    struct rusage
    {
        timeval ru_utime;
        timeval ru_stime;
        c_long ru_maxrss;
        alias ru_ixrss ru_first;
        c_long ru_ixrss;
        c_long ru_idrss;
        c_long ru_isrss;
        c_long ru_minflt;
        c_long ru_majflt;
        c_long ru_nswap;
        c_long ru_inblock;
        c_long ru_oublock;
        c_long ru_msgsnd;
        c_long ru_msgrcv;
        c_long ru_nsignals;
        c_long ru_nvcsw;
        c_long ru_nivcsw;
        alias ru_nivcsw ru_last;
    }

    enum
    {
        RLIMIT_CORE   =  4,
        RLIMIT_CPU    =  0,
        RLIMIT_DATA   =  2,
        RLIMIT_FSIZE  =  1,
        RLIMIT_NOFILE =  8,
        RLIMIT_STACK  =  3,
        RLIMIT_AS     = 10,
    }
}
else version (OpenBSD)
{
    enum
    {
        PRIO_PROCESS = 0,
        PRIO_PGRP    = 1,
        PRIO_USER    = 2,
    }

    alias ulong rlim_t;

    enum
    {
        RLIM_INFINITY  = (cast(rlim_t)((cast(ulong) 1 << 63) - 1)),
        RLIM_SAVED_MAX = RLIM_INFINITY,
        RLIM_SAVED_CUR = RLIM_INFINITY,
    }

    enum
    {
        RUSAGE_SELF     =  0,
        RUSAGE_CHILDREN = -1,
        RUSAGE_THREAD   =  1,
    }

    struct rusage
    {
        timeval ru_utime;
        timeval ru_stime;
        c_long ru_maxrss;
        alias ru_ixrss ru_first;
        c_long ru_ixrss;
        c_long ru_idrss;
        c_long ru_isrss;
        c_long ru_minflt;
        c_long ru_majflt;
        c_long ru_nswap;
        c_long ru_inblock;
        c_long ru_oublock;
        c_long ru_msgsnd;
        c_long ru_msgrcv;
        c_long ru_nsignals;
        c_long ru_nvcsw;
        c_long ru_nivcsw;
        alias ru_nivcsw ru_last;
    }

    enum
    {
        RLIMIT_CORE   =  4,
        RLIMIT_CPU    =  0,
        RLIMIT_DATA   =  2,
        RLIMIT_FSIZE  =  1,
        RLIMIT_NOFILE =  8,
        RLIMIT_STACK  =  3,
        // OpenBSD does not define the following:
        //RLIMIT_AS,
    }
}
else version (DragonFlyBSD)
{
    enum
    {
        PRIO_PROCESS = 0,
        PRIO_PGRP    = 1,
        PRIO_USER    = 2,
    }

    alias long rlim_t;

    enum
    {
        RLIM_INFINITY   = (cast(rlim_t)((cast(ulong) 1 << 63) - 1)),
        RLIM_SAVED_MAX  = RLIM_INFINITY,
        RLIM_SAVED_CUR  = RLIM_INFINITY,
    }

    enum
    {
        RUSAGE_SELF     =  0,
        RUSAGE_CHILDREN = -1,
    }

    struct rusage
    {
        timeval ru_utime;
        timeval ru_stime;
        c_long ru_maxrss;
        alias ru_ixrss ru_first;
        c_long ru_ixrss;
        c_long ru_idrss;
        c_long ru_isrss;
        c_long ru_minflt;
        c_long ru_majflt;
        c_long ru_nswap;
        c_long ru_inblock;
        c_long ru_oublock;
        c_long ru_msgsnd;
        c_long ru_msgrcv;
        c_long ru_nsignals;
        c_long ru_nvcsw;
        c_long ru_nivcsw;
        alias ru_nivcsw ru_last;
    }

    enum
    {
        RLIMIT_CORE   =  4,
        RLIMIT_CPU    =  0,
        RLIMIT_DATA   =  2,
        RLIMIT_FSIZE  =  1,
        RLIMIT_NOFILE =  8,
        RLIMIT_STACK  =  3,
        RLIMIT_AS     = 10,
    }
}
else version (Solaris)
{
    enum
    {
        PRIO_PROCESS = 0,
        PRIO_PGRP    = 1,
        PRIO_USER    = 2,
    }

    alias c_ulong rlim_t;

    enum : c_long
    {
        RLIM_INFINITY   = -3,
        RLIM_SAVED_MAX  = -2,
        RLIM_SAVED_CUR  = -1,
    }

    enum
    {
        RUSAGE_SELF     =  0,
        RUSAGE_CHILDREN = -1,
    }

    struct rusage
    {
        timeval ru_utime;
        timeval ru_stime;
        c_long ru_maxrss;
        c_long ru_ixrss;
        c_long ru_idrss;
        c_long ru_isrss;
        c_long ru_minflt;
        c_long ru_majflt;
        c_long ru_nswap;
        c_long ru_inblock;
        c_long ru_oublock;
        c_long ru_msgsnd;
        c_long ru_msgrcv;
        c_long ru_nsignals;
        c_long ru_nvcsw;
        c_long ru_nivcsw;
    }

    enum
    {
        RLIMIT_CORE   = 4,
        RLIMIT_CPU    = 0,
        RLIMIT_DATA   = 2,
        RLIMIT_FSIZE  = 1,
        RLIMIT_NOFILE = 5,
        RLIMIT_STACK  = 3,
        RLIMIT_AS     = 6,
    }
}
else
    static assert (false, "Unsupported platform");

/*
struct rlimit
{
    rlim_t rlim_cur;
    rlim_t rlim_max;
}

int getpriority(int, id_t);
int getrlimit(int, rlimit*);
int getrusage(int, rusage*);
int setpriority(int, id_t, int);
int setrlimit(int, const rlimit*);
*/

struct rlimit
{
    rlim_t rlim_cur;
    rlim_t rlim_max;
}

version (CRuntime_Glibc)
{
    int getpriority(int, id_t);
    int setpriority(int, id_t, int);
    static if (__USE_FILE_OFFSET64)
    {
        int getrlimit64(int, rlimit*);
        int setrlimit64(int, const scope rlimit*);
        alias getrlimit = getrlimit64;
        alias setrlimit = setrlimit64;
    }
    else
    {
        int getrlimit(int, rlimit*);
        int setrlimit(int, const scope rlimit*);
    }
    int getrusage(int, rusage*);
}
else version (FreeBSD)
{
    int getpriority(int, int);
    int getrlimit(int, rlimit*);
    int getrusage(int, rusage*);
    int setpriority(int, int, int);
    int setrlimit(int, const scope rlimit*);
}
else version (NetBSD)
{
    int getpriority(int, int);
    int getrlimit(int, rlimit*);
    int getrusage(int, rusage*);
    int setpriority(int, int, int);
    int setrlimit(int, const scope rlimit*);
}
else version (OpenBSD)
{
    int getpriority(int, int);
    int getrlimit(int, rlimit*);
    int getrusage(int, rusage*);
    int setpriority(int, int, int);
    int setrlimit(int, const scope rlimit*);
}
else version (DragonFlyBSD)
{
    int getpriority(int, int);
    int getrlimit(int, rlimit*);
    int getrusage(int, rusage*);
    int setpriority(int, int, int);
    int setrlimit(int, const scope rlimit*);
}
else version (CRuntime_Bionic)
{
    int getpriority(int, int);
    int getrlimit(int, rlimit*);
    int getrusage(int, rusage*);
    int setpriority(int, int, int);
    int setrlimit(int, const scope rlimit*);
}
else version (CRuntime_Musl)
{
    int getpriority(int, id_t);
    int setpriority(int, id_t, int);
    int getrlimit(int, rlimit*);
    int setrlimit(int, const scope rlimit*);
    alias getrlimit getrlimit64;
    alias setrlimit setrlimit64;
    pragma(mangle, muslRedirTime64Mangle!("getrusage", "__getrusage_time64"))
    int getrusage(int, rusage*);
}
else version (Solaris)
{
    int getpriority(int, int);
    int getrlimit(int, rlimit*);
    int getrusage(int, rusage*);
    int setpriority(int, int, int);
    int setrlimit(int, const scope rlimit*);
}
else version (Darwin)
{
    int getpriority(int, id_t);
    int getrlimit(int, rlimit*);
    int getrusage(int, rusage*);
    int setpriority(int, id_t, int);
    int setrlimit(int, const scope rlimit*);
}
else version (CRuntime_UClibc)
{
    int getpriority(int, id_t);
    int setpriority(int, id_t, int);
    static if (__USE_FILE_OFFSET64)
    {
        int getrlimit64(int, rlimit*);
        int setrlimit64(int, const scope rlimit*);
        alias getrlimit = getrlimit64;
        alias setrlimit = setrlimit64;
    }
    else
    {
        int getrlimit(int, rlimit*);
        int setrlimit(int, const scope rlimit*);
    }
    int getrusage(int, rusage*);
}
else
    static assert (false, "Unsupported platform");
