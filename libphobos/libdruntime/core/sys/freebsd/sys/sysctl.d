/**
  * D header file for FreeBSD sys/sysctl.h
  *
  * Copyright: Copyright © 2021, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Iain Buclaw
  */
module core.sys.freebsd.sys.sysctl;

version (FreeBSD):
extern (C):
nothrow:
@nogc:

// Top-level identifiers
enum
{
    CTL_SYSCTL   = 0,
    CTL_KERN     = 1,
    CTL_VM       = 2,
    CTL_VFS      = 3,
    CTL_NET      = 4,
    CTL_DEBUG    = 5,
    CTL_HW       = 6,
    CTL_MACHDEP  = 7,
    CTL_USER     = 8,
    CTL_P1003_1B = 9,
}

// CTL_SYSCTL identifiers
enum
{
    CTL_SYSCTL_DEBUG    = 0,
    CTL_SYSCTL_NAME     = 1,
    CTL_SYSCTL_NEXT     = 2,
    CTL_SYSCTL_NAME2OID = 3,
    CTL_SYSCTL_OIDFMT   = 4,
    CTL_SYSCTL_OIDDESCR = 5,
    CTL_SYSCTL_OIDLABEL = 6,
}

// CTL_KERN identifiers
enum
{
    KERN_OSTYPE          = 1,
    KERN_OSRELEASE       = 2,
    KERN_OSREV           = 3,
    KERN_VERSION         = 4,
    KERN_MAXVNODES       = 5,
    KERN_MAXPROC         = 6,
    KERN_MAXFILES        = 7,
    KERN_ARGMAX          = 8,
    KERN_SECURELVL       = 9,
    KERN_HOSTNAME        = 10,
    KERN_HOSTID          = 11,
    KERN_CLOCKRATE       = 12,
    KERN_VNODE           = 13,
    KERN_PROC            = 14,
    KERN_FILE            = 15,
    KERN_PROF            = 16,
    KERN_POSIX1          = 17,
    KERN_NGROUPS         = 18,
    KERN_JOB_CONTROL     = 19,
    KERN_SAVED_IDS       = 20,
    KERN_BOOTTIME        = 21,
    KERN_NISDOMAINNAME   = 22,
    KERN_UPDATEINTERVAL  = 23,
    KERN_OSRELDATE       = 24,
    KERN_NTP_PLL         = 25,
    KERN_BOOTFILE        = 26,
    KERN_MAXFILESPERPROC = 27,
    KERN_MAXPROCPERUID   = 28,
    KERN_DUMPDEV         = 29,
    KERN_IPC             = 30,
    KERN_DUMMY           = 31,
    KERN_PS_STRINGS      = 32,
    KERN_USRSTACK        = 33,
    KERN_LOGSIGEXIT      = 34,
    KERN_IOV_MAX         = 35,
    KERN_HOSTUUID        = 36,
    KERN_ARND            = 37,
    KERN_MAXPHYS         = 38,
}

// KERN_PROC subtypes
enum
{
    KERN_PROC_ALL        = 0,
    KERN_PROC_PID        = 1,
    KERN_PROC_PGRP       = 2,
    KERN_PROC_SESSION    = 3,
    KERN_PROC_TTY        = 4,
    KERN_PROC_UID        = 5,
    KERN_PROC_RUID       = 6,
    KERN_PROC_ARGS       = 7,
    KERN_PROC_PROC       = 8,
    KERN_PROC_SV_NAME    = 9,
    KERN_PROC_RGID       = 10,
    KERN_PROC_GID        = 11,
    KERN_PROC_PATHNAME   = 12,
    KERN_PROC_OVMMAP     = 13,
    KERN_PROC_OFILEDESC  = 14,
    KERN_PROC_KSTACK     = 15,
    KERN_PROC_INC_THREAD = 0x10,
    KERN_PROC_VMMAP      = 32,
    KERN_PROC_FILEDESC   = 33,
    KERN_PROC_GROUPS     = 34,
    KERN_PROC_ENV        = 35,
    KERN_PROC_AUXV       = 36,
    KERN_PROC_RLIMIT     = 37,
    KERN_PROC_PS_STRINGS = 38,
    KERN_PROC_UMASK      = 39,
    KERN_PROC_OSREL      = 40,
    KERN_PROC_SIGTRAMP   = 41,
    KERN_PROC_CWD        = 42,
    KERN_PROC_NFDS       = 43,
}

// KERN_IPC identifiers
enum
{
    KIPC_MAXSOCKBUF        = 1,
    KIPC_SOCKBUF_WASTE     = 2,
    KIPC_SOMAXCONN         = 3,
    KIPC_MAX_LINKHDR       = 4,
    KIPC_MAX_PROTOHDR      = 5,
    KIPC_MAX_HDR           = 6,
    KIPC_MAX_DATALEN       = 7,
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
    HW_DISKSTATS    = 9,
    HW_FLOATINGPT   = 10,
    HW_MACHINE_ARCH = 11,
    HW_REALMEM      = 12,
}

// CTL_USER identifiers
enum
{
    USER_CS_PATH           = 1,
    USER_BC_BASE_MAX       = 2,
    USER_BC_DIM_MAX        = 3,
    USER_BC_SCALE_MAX      = 4,
    USER_BC_STRING_MAX     = 5,
    USER_COLL_WEIGHTS_MAX  = 6,
    USER_EXPR_NEST_MAX     = 7,
    USER_LINE_MAX          = 8,
    USER_RE_DUP_MAX        = 9,
    USER_POSIX2_VERSION    = 10,
    USER_POSIX2_C_BIND     = 11,
    USER_POSIX2_C_DEV      = 12,
    USER_POSIX2_CHAR_TERM  = 13,
    USER_POSIX2_FORT_DEV   = 14,
    USER_POSIX2_FORT_RUN   = 15,
    USER_POSIX2_LOCALEDEF  = 16,
    USER_POSIX2_SW_DEV     = 17,
    USER_POSIX2_UPE        = 18,
    USER_STREAM_MAX        = 19,
    USER_TZNAME_MAX        = 20,
}

// POSIX 1003.1B definitions
enum
{
    CTL_P1003_1B_ASYNCHRONOUS_IO       = 1,
    CTL_P1003_1B_MAPPED_FILES          = 2,
    CTL_P1003_1B_MEMLOCK               = 3,
    CTL_P1003_1B_MEMLOCK_RANGE         = 4,
    CTL_P1003_1B_MEMORY_PROTECTION     = 5,
    CTL_P1003_1B_MESSAGE_PASSING       = 6,
    CTL_P1003_1B_PRIORITIZED_IO        = 7,
    CTL_P1003_1B_PRIORITY_SCHEDULING   = 8,
    CTL_P1003_1B_REALTIME_SIGNALS      = 9,
    CTL_P1003_1B_SEMAPHORES            = 10,
    CTL_P1003_1B_FSYNC                 = 11,
    CTL_P1003_1B_SHARED_MEMORY_OBJECTS = 12,
    CTL_P1003_1B_SYNCHRONIZED_IO       = 13,
    CTL_P1003_1B_TIMERS                = 14,
    CTL_P1003_1B_AIO_LISTIO_MAX        = 15,
    CTL_P1003_1B_AIO_MAX               = 16,
    CTL_P1003_1B_AIO_PRIO_DELTA_MAX    = 17,
    CTL_P1003_1B_DELAYTIMER_MAX        = 18,
    CTL_P1003_1B_UNUSED19              = 19,
    CTL_P1003_1B_PAGESIZE              = 20,
    CTL_P1003_1B_RTSIG_MAX             = 21,
    CTL_P1003_1B_SEM_NSEMS_MAX         = 22,
    CTL_P1003_1B_UNUSED23              = 23,
    CTL_P1003_1B_SIGQUEUE_MAX          = 24,
    CTL_P1003_1B_TIMER_MAX             = 25,
    CTL_P1003_1B_MAXID                 = 26,
}

///
int sysctl(const int* name, uint namelen, void* oldp, size_t* oldlenp,
           const void* newp, size_t newlen);
///
int sysctlbyname(const char* name, void* oldp, size_t* oldlenp,
                 const void* newp, size_t newlen);
///
int sysctlnametomib(const char* name, int* mibp, size_t* sizep);
