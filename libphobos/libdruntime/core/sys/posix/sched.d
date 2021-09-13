/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly,
              Alex Rønne Petersen
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.sched;

import core.sys.posix.config;
public import core.sys.posix.time;
public import core.sys.posix.sys.types;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):
extern (C):
nothrow:
@nogc:
@system:

//
// Required
//
/*
struct sched_param
{
    int sched_priority (THR)
    int sched_ss_low_priority (SS|TSP)
    struct timespec sched_ss_repl_period (SS|TSP)
    struct timespec sched_ss_init_budget (SS|TSP)
    int sched_ss_max_repl (SS|TSP)
}

SCHED_FIFO
SCHED_RR
SCHED_SPORADIC (SS|TSP)
SCHED_OTHER

int sched_getparam(pid_t, sched_param*);
int sched_getscheduler(pid_t);
int sched_setparam(pid_t, const scope sched_param*);
int sched_setscheduler(pid_t, int, const scope sched_param*);
*/

version (CRuntime_Glibc)
{
    struct sched_param
    {
        int sched_priority;
    }

    enum SCHED_OTHER    = 0;
    enum SCHED_FIFO     = 1;
    enum SCHED_RR       = 2;
    //SCHED_SPORADIC (SS|TSP)
}
else version (CRuntime_Musl)
{
    struct sched_param {
        int sched_priority;
        int sched_ss_low_priority;
        timespec sched_ss_repl_period;
        timespec sched_ss_init_budget;
        int sched_ss_max_repl;
    }
}
else version (Darwin)
{
    enum SCHED_OTHER    = 1;
    enum SCHED_FIFO     = 4;
    enum SCHED_RR       = 2;
    //SCHED_SPORADIC (SS|TSP)

    private enum __SCHED_PARAM_SIZE__ = 4;

    struct sched_param
    {
        int                             sched_priority;
        byte[__PTHREAD_MUTEX_SIZE__]    __opaque;
    }
}
else version (FreeBSD)
{
    struct sched_param
    {
        int sched_priority;
    }

    enum SCHED_FIFO     = 1;
    enum SCHED_OTHER    = 2;
    enum SCHED_RR       = 3;
}
else version (NetBSD)
{
    struct sched_param
    {
        int sched_priority;
    }

    enum SCHED_FIFO     = 1;
    enum SCHED_OTHER    = 0;
    enum SCHED_RR       = 2;
}
else version (OpenBSD)
{
    struct sched_param
    {
        int sched_priority;
    }

    enum SCHED_FIFO     = 1;
    enum SCHED_OTHER    = 2;
    enum SCHED_RR       = 3;
}
else version (DragonFlyBSD)
{
    struct sched_param
    {
        int sched_priority;
    }

    enum SCHED_FIFO     = 1;
    enum SCHED_OTHER    = 2;
    enum SCHED_RR       = 3;
}
else version (Solaris)
{
    struct sched_param
    {
        int sched_priority;
        int[8] sched_pad;
    }

    enum SCHED_OTHER = 0;
    enum SCHED_FIFO = 1;
    enum SCHED_RR = 2;
    enum SCHED_SYS = 3;
    enum SCHED_IA = 4;
    enum SCHED_FSS = 5;
    enum SCHED_FX = 6;
    enum _SCHED_NEXT = 7;
}
else version (CRuntime_Bionic)
{
    struct sched_param
    {
        int sched_priority;
    }

    enum SCHED_NORMAL   = 0;
    enum SCHED_OTHER    = 0;
    enum SCHED_FIFO     = 1;
    enum SCHED_RR       = 2;
}
else version (CRuntime_UClibc)
{
    struct sched_param
    {
        int sched_priority;
    }

    enum SCHED_OTHER    = 0;
    enum SCHED_FIFO     = 1;
    enum SCHED_RR       = 2;
    enum SCHED_BATCH    = 3;
    enum SCHED_IDLE     = 5;

    enum SCHED_RESET_ON_FORK    = 0x40000000;
}
else
{
    static assert(false, "Unsupported platform");
}

int sched_getparam(pid_t, sched_param*);
int sched_getscheduler(pid_t);
int sched_setparam(pid_t, const scope sched_param*);
int sched_setscheduler(pid_t, int, const scope sched_param*);

//
// Thread (THR)
//
/*
int sched_yield();
*/

version (CRuntime_Glibc)
{
    int sched_yield();
}
else version (Darwin)
{
    int sched_yield();
}
else version (FreeBSD)
{
    int sched_yield();
}
else version (NetBSD)
{
    int sched_yield();
}
else version (OpenBSD)
{
    int sched_yield();
}
else version (DragonFlyBSD)
{
    int sched_yield();
}
else version (Solaris)
{
    int sched_yield();
}
else version (CRuntime_Bionic)
{
    int sched_yield();
}
else version (CRuntime_Musl)
{
    int sched_yield();
}
else version (CRuntime_UClibc)
{
    int sched_yield();
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Scheduling (TPS)
//
/*
int sched_get_priority_max(int);
int sched_get_priority_min(int);
int sched_rr_get_interval(pid_t, timespec*);
*/

version (CRuntime_Glibc)
{
    int sched_get_priority_max(int);
    int sched_get_priority_min(int);
    int sched_rr_get_interval(pid_t, timespec*);
}
else version (Darwin)
{
    int sched_get_priority_min(int);
    int sched_get_priority_max(int);
    //int sched_rr_get_interval(pid_t, timespec*); // FIXME: unavailable?
}
else version (FreeBSD)
{
    int sched_get_priority_min(int);
    int sched_get_priority_max(int);
    int sched_rr_get_interval(pid_t, timespec*);
}
else version (NetBSD)
{
    int sched_get_priority_min(int);
    int sched_get_priority_max(int);
    int sched_rr_get_interval(pid_t, timespec*);
}
else version (OpenBSD)
{
    int sched_get_priority_min(int);
    int sched_get_priority_max(int);
    int sched_rr_get_interval(pid_t, timespec*);
}
else version (DragonFlyBSD)
{
    int sched_get_priority_min(int);
    int sched_get_priority_max(int);
    int sched_rr_get_interval(pid_t, timespec*);
}
else version (Solaris)
{
    int sched_get_priority_max(int);
    int sched_get_priority_min(int);
    int sched_rr_get_interval(pid_t, timespec*);
}
else version (CRuntime_Bionic)
{
    int sched_get_priority_max(int);
    int sched_get_priority_min(int);
    int sched_rr_get_interval(pid_t, timespec*);
}
else version (CRuntime_Musl)
{
    int sched_get_priority_max(int);
    int sched_get_priority_min(int);
    int sched_rr_get_interval(pid_t, timespec*);
}
else version (CRuntime_UClibc)
{
    int sched_get_priority_max(int);
    int sched_get_priority_min(int);
    int sched_rr_get_interval(pid_t, timespec*);
}
else
{
    static assert(false, "Unsupported platform");
}
