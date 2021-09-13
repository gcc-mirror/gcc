/**
  * D header file for Darwin sys/sysctl.h
  *
  * Copyright: Copyright © 2021, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Iain Buclaw
  */
module core.sys.darwin.sys.sysctl;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):
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
    CTL_USER     = 8,
    CTL_MAXID    = 9,
}

// CTL_KERN identifiers
enum
{
    KERN_OSTYPE             = 1,
    KERN_OSRELEASE          = 2,
    KERN_OSREV              = 3,
    KERN_VERSION            = 4,
    KERN_MAXVNODES          = 5,
    KERN_MAXPROC            = 6,
    KERN_MAXFILES           = 7,
    KERN_ARGMAX             = 8,
    KERN_SECURELVL          = 9,
    KERN_HOSTNAME           = 10,
    KERN_HOSTID             = 11,
    KERN_CLOCKRATE          = 12,
    KERN_VNODE              = 13,
    KERN_PROC               = 14,
    KERN_FILE               = 15,
    KERN_PROF               = 16,
    KERN_POSIX1             = 17,
    KERN_NGROUPS            = 18,
    KERN_JOB_CONTROL        = 19,
    KERN_SAVED_IDS          = 20,
    KERN_BOOTTIME           = 21,
    KERN_NISDOMAINNAME      = 22,
    KERN_DOMAINNAME         = KERN_NISDOMAINNAME,
    KERN_MAXPARTITIONS      = 23,
    KERN_KDEBUG             = 24,
    KERN_UPDATEINTERVAL     = 25,
    KERN_OSRELDATE          = 26,
    KERN_NTP_PLL            = 27,
    KERN_BOOTFILE           = 28,
    KERN_MAXFILESPERPROC    = 29,
    KERN_MAXPROCPERUID      = 30,
    KERN_DUMPDEV            = 31,
    KERN_IPC                = 32,
    KERN_DUMMY              = 33,
    KERN_PS_STRINGS         = 34,
    KERN_USRSTACK32         = 35,
    KERN_LOGSIGEXIT         = 36,
    KERN_SYMFILE            = 37,
    KERN_PROCARGS           = 38,
    KERN_NETBOOT            = 40,
    KERN_SYSV               = 42,
    KERN_AFFINITY           = 43,
    KERN_TRANSLATE          = 44,
    KERN_CLASSIC            = KERN_TRANSLATE,
    KERN_EXEC               = 45,
    KERN_CLASSICHANDLER     = KERN_EXEC,
    KERN_AIOMAX             = 46,
    KERN_AIOPROCMAX         = 47,
    KERN_AIOTHREADS         = 48,
    KERN_PROCARGS2          = 49,
    KERN_COREFILE           = 50,
    KERN_COREDUMP           = 51,
    KERN_SUGID_COREDUMP     = 52,
    KERN_PROCDELAYTERM      = 53,
    KERN_SHREG_PRIVATIZABLE = 54,
    KERN_LOW_PRI_WINDOW     = 56,
    KERN_LOW_PRI_DELAY      = 57,
    KERN_POSIX              = 58,
    KERN_USRSTACK64         = 59,
    KERN_NX_PROTECTION      = 60,
    KERN_TFP                = 61,
    KERN_PROCNAME           = 62,
    KERN_THALTSTACK         = 63,
    KERN_SPECULATIVE_READS  = 64,
    KERN_OSVERSION          = 65,
    KERN_SAFEBOOT           = 66,
    KERN_RAGEVNODE          = 68,
    KERN_TTY                = 69,
    KERN_CHECKOPENEVT       = 70,
    KERN_THREADNAME         = 71,
    KERN_MAXID              = 72,
}

// KERN_RAGEVNODE types
enum
{
    KERN_RAGE_PROC     = 1,
    KERN_RAGE_THREAD   = 2,
    KERN_UNRAGE_PROC   = 3,
    KERN_UNRAGE_THREAD = 4,
}

// KERN_OPENEVT types
enum
{
    KERN_OPENEVT_PROC   = 1,
    KERN_UNOPENEVT_PROC = 2,
}

// KERN_TFP types
enum
{
    KERN_TFP_POLICY = 1,
}

// KERN_TFP_POLICY values
enum
{
    KERN_TFP_POLICY_DENY    = 0,
    KERN_TFP_POLICY_DEFAULT = 2,
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
    KERN_PROC_LCID    = 7,
}

// KERN_VFSNSPACE subtypes
enum
{
    KERN_VFSNSPACE_HANDLE_PROC   = 1,
    KERN_VFSNSPACE_UNHANDLE_PROC = 2,
}

// KERN_IPC identifiers
enum
{
    KIPC_MAXSOCKBUF     = 1,
    KIPC_SOCKBUF_WASTE  = 2,
    KIPC_SOMAXCONN      = 3,
    KIPC_MAX_LINKHDR    = 4,
    KIPC_MAX_PROTOHDR   = 5,
    KIPC_MAX_HDR        = 6,
    KIPC_MAX_DATALEN    = 7,
    KIPC_MBSTAT         = 8,
    KIPC_NMBCLUSTERS    = 9,
    KIPC_SOQLIMITCOMPAT = 10,
}

// CTL_VM identifiers
enum
{
    VM_METER      = 1,
    VM_LOADAVG    = 2,
    VM_MACHFACTOR = 4,
    VM_SWAPUSAGE  = 5,
    VM_MAXID      = 6,
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
    HW_EPOCH        = 10,
    HW_FLOATINGPT   = 11,
    HW_MACHINE_ARCH = 12,
    HW_VECTORUNIT   = 13,
    HW_BUS_FREQ     = 14,
    HW_CPU_FREQ     = 15,
    HW_CACHELINE    = 16,
    HW_L1ICACHESIZE = 17,
    HW_L1DCACHESIZE = 18,
    HW_L2SETTINGS   = 19,
    HW_L2CACHESIZE  = 20,
    HW_L3SETTINGS   = 21,
    HW_L3CACHESIZE  = 22,
    HW_TB_FREQ      = 23,
    HW_MEMSIZE      = 24,
    HW_AVAILCPU     = 25,
    HW_MAXID        = 26,
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
    USER_MAXID             = 21,
}

///
int sysctl(const int* name, uint namelen, void* oldp, size_t* oldlenp,
           const void* newp, size_t newlen);
///
int sysctlbyname(const char* name, void* oldp, size_t* oldlenp,
                 const void* newp, size_t newlen);
///
int sysctlnametomib(const char* sname, int* name, size_t* namelenp);
