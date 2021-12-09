/**
  * D header file for OpenBSD sys/sysctl.h
  *
  * Copyright: Copyright © 2021, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Iain Buclaw
  */
module core.sys.openbsd.sys.sysctl;

version (OpenBSD):
extern (C):
nothrow:
@nogc:

// Top-level identifiers
enum
{
    CTL_UNSPEC  = 0,
    CTL_KERN    = 1,
    CTL_VM      = 2,
    CTL_FS      = 3,
    CTL_NET     = 4,
    CTL_DEBUG   = 5,
    CTL_HW      = 6,
    CTL_MACHDEP = 7,
    CTL_DDB     = 9,
    CTL_VFS     = 10,
    CTL_MAXID   = 11,
}

// CTL_KERN identifiers
enum
{
    KERN_OSTYPE         = 1,
    KERN_OSRELEASE      = 2,
    KERN_OSREV          = 3,
    KERN_VERSION        = 4,
    KERN_MAXVNODES      = 5,
    KERN_MAXPROC        = 6,
    KERN_MAXFILES       = 7,
    KERN_ARGMAX         = 8,
    KERN_SECURELVL      = 9,
    KERN_HOSTNAME       = 10,
    KERN_HOSTID         = 11,
    KERN_CLOCKRATE      = 12,
    KERN_PROF           = 16,
    KERN_POSIX1         = 17,
    KERN_NGROUPS        = 18,
    KERN_JOB_CONTROL    = 19,
    KERN_SAVED_IDS      = 20,
    KERN_BOOTTIME       = 21,
    KERN_DOMAINNAME     = 22,
    KERN_MAXPARTITIONS  = 23,
    KERN_RAWPARTITION   = 24,
    KERN_MAXTHREAD      = 25,
    KERN_NTHREADS       = 26,
    KERN_OSVERSION      = 27,
    KERN_SOMAXCONN      = 28,
    KERN_SOMINCONN      = 29,
    KERN_NOSUIDCOREDUMP = 32,
    KERN_FSYNC          = 33,
    KERN_SYSVMSG        = 34,
    KERN_SYSVSEM        = 35,
    KERN_SYSVSHM        = 36,
    KERN_MSGBUFSIZE     = 38,
    KERN_MALLOCSTATS    = 39,
    KERN_CPTIME         = 40,
    KERN_NCHSTATS       = 41,
    KERN_FORKSTAT       = 42,
    KERN_NSELCOLL       = 43,
    KERN_TTY            = 44,
    KERN_CCPU           = 45,
    KERN_FSCALE         = 46,
    KERN_NPROCS         = 47,
    KERN_MSGBUF         = 48,
    KERN_POOL           = 49,
    KERN_STACKGAPRANDOM = 50,
    KERN_SYSVIPC_INFO   = 51,
    KERN_ALLOWKMEM      = 52,
    KERN_WITNESSWATCH   = 53,
    KERN_SPLASSERT      = 54,
    KERN_PROC_ARGS      = 55,
    KERN_NFILES         = 56,
    KERN_TTYCOUNT       = 57,
    KERN_NUMVNODES      = 58,
    KERN_MBSTAT         = 59,
    KERN_WITNESS        = 60,
    KERN_SEMINFO        = 61,
    KERN_SHMINFO        = 62,
    KERN_INTRCNT        = 63,
    KERN_WATCHDOG       = 64,
    KERN_ALLOWDT        = 65,
    KERN_PROC           = 66,
    KERN_MAXCLUSTERS    = 67,
    KERN_EVCOUNT        = 68,
    KERN_TIMECOUNTER    = 69,
    KERN_MAXLOCKSPERUID = 70,
    KERN_CPTIME2        = 71,
    KERN_CACHEPCT       = 72,
    KERN_FILE           = 73,
    KERN_WXABORT        = 74,
    KERN_CONSDEV        = 75,
    KERN_NETLIVELOCKS   = 76,
    KERN_POOL_DEBUG     = 77,
    KERN_PROC_CWD       = 78,
    KERN_PROC_NOBROADCASTKILL = 79,
    KERN_PROC_VMMAP     = 80,
    KERN_GLOBAL_PTRACE  = 81,
    KERN_CONSBUFSIZE    = 82,
    KERN_CONSBUF        = 83,
    KERN_AUDIO          = 84,
    KERN_CPUSTATS       = 85,
    KERN_PFSTATUS       = 86,
    KERN_TIMEOUT_STATS  = 87,
    KERN_UTC_OFFSET     = 88,
    KERN_MAXID          = 89,
}

// KERN_PROC subtypes
enum
{
    KERN_PROC_ALL          = 0,
    KERN_PROC_PID          = 1,
    KERN_PROC_PGRP         = 2,
    KERN_PROC_SESSION      = 3,
    KERN_PROC_TTY          = 4,
    KERN_PROC_UID          = 5,
    KERN_PROC_RUID         = 6,
    KERN_PROC_KTHREAD      = 7,
    KERN_PROC_SHOW_THREADS = 0x40000000,
}

// KERN_SYSVIPC_INFO subtypes
enum
{
    KERN_SYSVIPC_MSG_INFO = 1,
    KERN_SYSVIPC_SEM_INFO = 2,
    KERN_SYSVIPC_SHM_INFO = 3,
}

// KERN_PROC_ARGS subtypes
enum
{
    KERN_PROC_ARGV  = 1,
    KERN_PROC_NARGV = 2,
    KERN_PROC_ENV   = 3,
    KERN_PROC_NENV  = 4,
}

// KERN_AUDIO subtypes
enum
{
    KERN_AUDIO_RECORD = 1,
    KERN_AUDIO_MAXID  = 2,
}

// KERN_WITNESS
enum
{
    KERN_WITNESS_WATCH     = 1,
    KERN_WITNESS_LOCKTRACE = 2,
    KERN_WITNESS_MAXID     = 3,
}

// KERN_FILE
enum
{
    KERN_FILE_BYFILE = 1,
    KERN_FILE_BYPID  = 2,
    KERN_FILE_BYUID  = 3,
    KERN_FILESLOP    = 10,

    KERN_FILE_TEXT   = -1,
    KERN_FILE_CDIR   = -2,
    KERN_FILE_RDIR   = -3,
    KERN_FILE_TRACE  = -4,
}

// KERN_INTRCNT
enum
{
    KERN_INTRCNT_NUM    = 1,
    KERN_INTRCNT_CNT    = 2,
    KERN_INTRCNT_NAME   = 3,
    KERN_INTRCNT_VECTOR = 4,
    KERN_INTRCNT_MAXID  = 5,
}

// KERN_WATCHDOG
enum
{
    KERN_WATCHDOG_PERIOD = 1,
    KERN_WATCHDOG_AUTO   = 2,
    KERN_WATCHDOG_MAXID  = 3,
}

// KERN_TIMECOUNTER
enum
{
    KERN_TIMECOUNTER_TICK             = 1,
    KERN_TIMECOUNTER_TIMESTEPWARNINGS = 2,
    KERN_TIMECOUNTER_HARDWARE         = 3,
    KERN_TIMECOUNTER_CHOICE           = 4,
    KERN_TIMECOUNTER_MAXID            = 5,
}

// CTL_FS identifiers
enum
{
    FS_POSIX        = 1,
    FS_MAXID        = 2,
}

// CTL_FS_POSIX identifiers
enum
{
    FS_POSIX_SETUID = 1,
    FS_POSIX_MAXID  = 2,
}

// CTL_HW identifiers
enum
{
    HW_MACHINE        = 1,
    HW_MODEL          = 2,
    HW_NCPU           = 3,
    HW_BYTEORDER      = 4,
    HW_PHYSMEM        = 5,
    HW_USERMEM        = 6,
    HW_PAGESIZE       = 7,
    HW_DISKNAMES      = 8,
    HW_DISKSTATS      = 9,
    HW_DISKCOUNT      = 10,
    HW_SENSORS        = 11,
    HW_CPUSPEED       = 12,
    HW_SETPERF        = 13,
    HW_VENDOR         = 14,
    HW_PRODUCT        = 15,
    HW_VERSION        = 16,
    HW_SERIALNO       = 17,
    HW_UUID           = 18,
    HW_PHYSMEM64      = 19,
    HW_USERMEM64      = 20,
    HW_NCPUFOUND      = 21,
    HW_ALLOWPOWERDOWN = 22,
    HW_PERFPOLICY     = 23,
    HW_SMT            = 24,
    HW_NCPUONLINE     = 25,
    HW_MAXID          = 26,
}

///
int sysctl(const int*, uint, void*, size_t*, void*, size_t);
