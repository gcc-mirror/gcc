/**
  * D header file for NetBSD sys/sysctl.h
  *
  * Copyright: Copyright © 2021, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Iain Buclaw
  */
module core.sys.netbsd.sys.sysctl;

version (NetBSD):
extern (C):
nothrow:
@nogc:

// Top-level identifiers
enum
{
    CTL_UNSPEC   = 0,
    CTL_KERN     = 1,
    CTL_VM       = 2,
    CTL_VFS      = 3,
    CTL_NET      = 4,
    CTL_DEBUG    = 5,
    CTL_HW       = 6,
    CTL_MACHDEP  = 7,
    CTL_DDB      = 9,
    CTL_PROC     = 10,
    CTL_VENDOR   = 11,
    CTL_EMUL     = 12,
    CTL_SECURITY = 13,
}

// CTL_KERN identifiers
enum
{
    KERN_OSTYPE            = 1,
    KERN_OSRELEASE         = 2,
    KERN_OSREV             = 3,
    KERN_VERSION           = 4,
    KERN_MAXVNODES         = 5,
    KERN_MAXPROC           = 6,
    KERN_MAXFILES          = 7,
    KERN_ARGMAX            = 8,
    KERN_SECURELVL         = 9,
    KERN_HOSTNAME          = 10,
    KERN_HOSTID            = 11,
    KERN_CLOCKRATE         = 12,
    KERN_VNODE             = 13,
    KERN_PROC              = 14,
    KERN_FILE              = 15,
    KERN_PROF              = 16,
    KERN_POSIX1            = 17,
    KERN_NGROUPS           = 18,
    KERN_JOB_CONTROL       = 19,
    KERN_SAVED_IDS         = 20,
    KERN_OBOOTTIME         = 21,
    KERN_DOMAINNAME        = 22,
    KERN_MAXPARTITIONS     = 23,
    KERN_RAWPARTITION      = 24,
    KERN_NTPTIME           = 25,
    KERN_TIMEX             = 26,
    KERN_AUTONICETIME      = 27,
    KERN_AUTONICEVAL       = 28,
    KERN_RTC_OFFSET        = 29,
    KERN_ROOT_DEVICE       = 30,
    KERN_MSGBUFSIZE        = 31,
    KERN_FSYNC             = 32,
    KERN_OLDSYSVMSG        = 33,
    KERN_OLDSYSVSEM        = 34,
    KERN_OLDSYSVSHM        = 35,
    KERN_OLDSHORTCORENAME  = 36,
    KERN_SYNCHRONIZED_IO   = 37,
    KERN_IOV_MAX           = 38,
    KERN_MBUF              = 39,
    KERN_MAPPED_FILES      = 40,
    KERN_MEMLOCK           = 41,
    KERN_MEMLOCK_RANGE     = 42,
    KERN_MEMORY_PROTECTION = 43,
    KERN_LOGIN_NAME_MAX    = 44,
    KERN_DEFCORENAME       = 45,
    KERN_LOGSIGEXIT        = 46,
    KERN_PROC2             = 47,
    KERN_PROC_ARGS         = 48,
    KERN_FSCALE            = 49,
    KERN_CCPU              = 50,
    KERN_CP_TIME           = 51,
    KERN_OLDSYSVIPC_INFO   = 52,
    KERN_MSGBUF            = 53,
    KERN_CONSDEV           = 54,
    KERN_MAXPTYS           = 55,
    KERN_PIPE              = 56,
    KERN_MAXPHYS           = 57,
    KERN_SBMAX             = 58,
    KERN_TKSTAT            = 59,
    KERN_MONOTONIC_CLOCK   = 60,
    KERN_URND              = 61,
    KERN_LABELSECTOR       = 62,
    KERN_LABELOFFSET       = 63,
    KERN_LWP               = 64,
    KERN_FORKFSLEEP        = 65,
    KERN_POSIX_THREADS     = 66,
    KERN_POSIX_SEMAPHORES  = 67,
    KERN_POSIX_BARRIERS    = 68,
    KERN_POSIX_TIMERS      = 69,
    KERN_POSIX_SPIN_LOCKS  = 70,
    KERN_POSIX_READER_WRITER_LOCKS = 71,
    KERN_DUMP_ON_PANIC     = 72,
    KERN_SOMAXKVA          = 73,
    KERN_ROOT_PARTITION    = 74,
    KERN_DRIVERS           = 75,
    KERN_BUF               = 76,
    KERN_FILE2             = 77,
    KERN_VERIEXEC          = 78,
    KERN_CP_ID             = 79,
    KERN_HARDCLOCK_TICKS   = 80,
    KERN_ARND              = 81,
    KERN_SYSVIPC           = 82,
    KERN_BOOTTIME          = 83,
    KERN_EVCNT             = 84,
    KERN_SOFIXEDBUF        = 85,
}

// KERN_PROC subtypes
enum
{
    KERN_PROC_ALL     = 0,
    KERN_PROC_PID     = 1,
    KERN_PROC_PGRP    = 2,
    KERN_PROC_SESSION = 3,
    KERN_PROC_TTY     = 4,
    KERN_PROC_UID     = 5,
    KERN_PROC_RUID    = 6,
    KERN_PROC_KTHREAD = 7,
    KERN_PROC_RGID    = 8,
}

// KERN_PROC_ARGS subtypes
enum
{
    KERN_PROC_ARGV     = 1,
    KERN_PROC_NARGV    = 2,
    KERN_PROC_ENV      = 3,
    KERN_PROC_NENV     = 4,
    KERN_PROC_PATHNAME = 5,
    KERN_PROC_CWD      = 6,
}

// KERN_SYSVIPC subtypes
enum
{
    KERN_SYSVIPC_INFO       = 1,
    KERN_SYSVIPC_MSG        = 2,
    KERN_SYSVIPC_SEM        = 3,
    KERN_SYSVIPC_SHM        = 4,
    KERN_SYSVIPC_SHMMAX     = 5,
    KERN_SYSVIPC_SHMMNI     = 6,
    KERN_SYSVIPC_SHMSEG     = 7,
    KERN_SYSVIPC_SHMMAXPGS  = 8,
    KERN_SYSVIPC_SHMUSEPHYS = 9,
}

// KERN_SYSVIPC_INFO subtypes
enum
{
    KERN_SYSVIPC_MSG_INFO = 4,
    KERN_SYSVIPC_SEM_INFO = 5,
    KERN_SYSVIPC_SHM_INFO = 6,
}

// KERN_TKSTAT subtypes
enum
{
    KERN_TKSTAT_NIN   = 1,
    KERN_TKSTAT_NOUT  = 2,
    KERN_TKSTAT_CANCC = 3,
    KERN_TKSTAT_RAWCC = 4,
}

// KERN_BUF subtypes
enum
{
    KERN_BUF_ALL = 0,
}

// KERN_FILE
enum
{
    KERN_FILE_BYFILE = 1,
    KERN_FILE_BYPID  = 2,
    KERN_FILESLOP    = 10,
}

// KERN_EVCNT
enum
{
    KERN_EVCNT_COUNT_ANY = 0,
    KERN_EVCNT_COUNT_NONZERO = 1,
}

// CTL_HW identifiers
enum
{
    HW_MACHINE      = 1,
    HW_MODEL        = 2,
    HW_NCPU         = 3,
    HW_BYTEORDER    = 4,
    HW_PHYSMEM      = 5,
    HW_USERMEM      = 6,
    HW_PAGESIZE     = 7,
    HW_DISKNAMES    = 8,
    HW_IOSTATS      = 9,
    HW_MACHINE_ARCH = 10,
    HW_ALIGNBYTES   = 11,
    HW_CNMAGIC      = 12,
    HW_PHYSMEM64    = 13,
    HW_USERMEM64    = 14,
    HW_IOSTATNAMES  = 15,
    HW_NCPUONLINE   = 16,
}

//  CTL_USER definitions
enum
{
    USER_CS_PATH          = 1,
    USER_BC_BASE_MAX      = 2,
    USER_BC_DIM_MAX       = 3,
    USER_BC_SCALE_MAX     = 4,
    USER_BC_STRING_MAX    = 5,
    USER_COLL_WEIGHTS_MAX = 6,
    USER_EXPR_NEST_MAX    = 7,
    USER_LINE_MAX         = 8,
    USER_RE_DUP_MAX       = 9,
    USER_POSIX2_VERSION   = 10,
    USER_POSIX2_C_BIND    = 11,
    USER_POSIX2_C_DEV     = 12,
    USER_POSIX2_CHAR_TERM = 13,
    USER_POSIX2_FORT_DEV  = 14,
    USER_POSIX2_FORT_RUN  = 15,
    USER_POSIX2_LOCALEDEF = 16,
    USER_POSIX2_SW_DEV    = 17,
    USER_POSIX2_UPE       = 18,
    USER_STREAM_MAX       = 19,
    USER_TZNAME_MAX       = 20,
    USER_ATEXIT_MAX       = 21,
}

///
int sysctl(const int* name, uint namelen, void* oldp, size_t* oldlenp,
           const void* newp, size_t newlen);
///
int sysctlbyname(const char* name, void* oldp, size_t* oldlenp,
                 const void* newp, size_t newlen);
///
int sysctlnametomib(const char* sname, int* name, size_t* namelenp);
