/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.unistd;

private import core.sys.posix.config;
private import core.stdc.stddef;
public import core.sys.posix.inttypes;  // for intptr_t
public import core.sys.posix.sys.types; // for ssize_t, uid_t, gid_t, off_t, pid_t, useconds_t

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

enum STDIN_FILENO  = 0;
enum STDOUT_FILENO = 1;
enum STDERR_FILENO = 2;

extern __gshared char*   optarg;
extern __gshared int     optind;
extern __gshared int     opterr;
extern __gshared int     optopt;

int     access(in char*, int);
uint    alarm(uint) @trusted;
int     chdir(in char*);
int     chown(in char*, uid_t, gid_t);
int     close(int) @trusted;
size_t  confstr(int, char*, size_t);
int     dup(int) @trusted;
int     dup2(int, int) @trusted;
int     execl(in char*, in char*, ...);
int     execle(in char*, in char*, ...);
int     execlp(in char*, in char*, ...);
int     execv(in char*, in char**);
int     execve(in char*, in char**, in char**);
int     execvp(in char*, in char**);
void    _exit(int) @trusted;
int     fchown(int, uid_t, gid_t) @trusted;
pid_t   fork() @trusted;
c_long  fpathconf(int, int) @trusted;
//int     ftruncate(int, off_t);
char*   getcwd(char*, size_t);
gid_t   getegid() @trusted;
uid_t   geteuid() @trusted;
gid_t   getgid() @trusted;
int     getgroups(int, gid_t *);
int     gethostname(char*, size_t);
char*   getlogin() @trusted;
int     getlogin_r(char*, size_t);
int     getopt(int, in char**, in char*);
pid_t   getpgrp() @trusted;
pid_t   getpid() @trusted;
pid_t   getppid() @trusted;
uid_t   getuid() @trusted;
int     isatty(int) @trusted;
int     link(in char*, in char*);
//off_t   lseek(int, off_t, int);
c_long  pathconf(in char*, int);
int     pause() @trusted;
int     pipe(ref int[2]) @trusted;
ssize_t read(int, void*, size_t);
ssize_t readlink(in char*, char*, size_t);
int     rmdir(in char*);
int     setegid(gid_t) @trusted;
int     seteuid(uid_t) @trusted;
int     setgid(gid_t) @trusted;
int     setgroups(size_t, in gid_t*) @trusted;
int     setpgid(pid_t, pid_t) @trusted;
pid_t   setsid() @trusted;
int     setuid(uid_t) @trusted;
uint    sleep(uint) @trusted;
int     symlink(in char*, in char*);
c_long  sysconf(int) @trusted;
pid_t   tcgetpgrp(int) @trusted;
int     tcsetpgrp(int, pid_t) @trusted;
char*   ttyname(int) @trusted;
int     ttyname_r(int, char*, size_t);
int     unlink(in char*);
ssize_t write(int, in void*, size_t);

version (CRuntime_Glibc)
{
  static if ( __USE_FILE_OFFSET64 )
  {
    off_t lseek64(int, off_t, int) @trusted;
    alias lseek64 lseek;
  }
  else
  {
    off_t lseek(int, off_t, int) @trusted;
  }
  static if ( __USE_LARGEFILE64 )
  {
    int   ftruncate64(int, off_t) @trusted;
    alias ftruncate64 ftruncate;
  }
  else
  {
    int   ftruncate(int, off_t) @trusted;
  }
}
else version (FreeBSD)
{
    off_t lseek(int, off_t, int) @trusted;
    int   ftruncate(int, off_t) @trusted;
}
else version (NetBSD)
{
    off_t lseek(int, off_t, int) @trusted;
    int   ftruncate(int, off_t) @trusted;
}
else version (OpenBSD)
{
    off_t lseek(int, off_t, int) @trusted;
    int   ftruncate(int, off_t) @trusted;
}
else version (DragonFlyBSD)
{
    off_t lseek(int, off_t, int) @trusted;
    int   ftruncate(int, off_t) @trusted;
}
else version (Solaris)
{
    version (D_LP64)
    {
        off_t   lseek(int, off_t, int) @trusted;
        alias   lseek lseek64;

        int     ftruncate(int, off_t) @trusted;
        alias   ftruncate ftruncate64;
    }
    else
    {
        static if ( __USE_LARGEFILE64 )
        {
            off64_t lseek64(int, off64_t, int) @trusted;
            alias   lseek64 lseek;

            int     ftruncate64(int, off64_t) @trusted;
            alias   ftruncate64 ftruncate;
        }
        else
        {
            off_t   lseek(int, off_t, int) @trusted;
            int     ftruncate(int, off_t) @trusted;
        }
    }
}
else version (Darwin)
{
    off_t lseek(int, off_t, int) @trusted;
    int   ftruncate(int, off_t) @trusted;
}
else version (CRuntime_Bionic)
{
    off_t lseek(int, off_t, int) @trusted;
    int   ftruncate(int, off_t) @trusted;
}
else version (CRuntime_Musl)
{
    int ftruncate(int, off_t) @trusted;
    off_t lseek(int, off_t, int) @trusted;
    alias ftruncate ftruncate64;
    alias lseek lseek64;
}
else version (CRuntime_UClibc)
{
  static if ( __USE_FILE_OFFSET64 )
  {
    off_t lseek64(int, off_t, int) @trusted;
    alias lseek64 lseek;
  }
  else
  {
    off_t lseek(int, off_t, int) @trusted;
  }
  static if ( __USE_LARGEFILE64 )
  {
    int   ftruncate64(int, off_t) @trusted;
    alias ftruncate64 ftruncate;
  }
  else
  {
    int   ftruncate(int, off_t) @trusted;
  }
}

version (CRuntime_Glibc)
{
    enum F_OK       = 0;
    enum R_OK       = 4;
    enum W_OK       = 2;
    enum X_OK       = 1;

    enum F_ULOCK    = 0;
    enum F_LOCK     = 1;
    enum F_TLOCK    = 2;
    enum F_TEST     = 3;

    enum
    {
        _CS_PATH,

        _CS_V6_WIDTH_RESTRICTED_ENVS,

        _CS_GNU_LIBC_VERSION,
        _CS_GNU_LIBPTHREAD_VERSION,

        _CS_LFS_CFLAGS = 1000,
        _CS_LFS_LDFLAGS,
        _CS_LFS_LIBS,
        _CS_LFS_LINTFLAGS,
        _CS_LFS64_CFLAGS,
        _CS_LFS64_LDFLAGS,
        _CS_LFS64_LIBS,
        _CS_LFS64_LINTFLAGS,

        _CS_XBS5_ILP32_OFF32_CFLAGS = 1100,
        _CS_XBS5_ILP32_OFF32_LDFLAGS,
        _CS_XBS5_ILP32_OFF32_LIBS,
        _CS_XBS5_ILP32_OFF32_LINTFLAGS,
        _CS_XBS5_ILP32_OFFBIG_CFLAGS,
        _CS_XBS5_ILP32_OFFBIG_LDFLAGS,
        _CS_XBS5_ILP32_OFFBIG_LIBS,
        _CS_XBS5_ILP32_OFFBIG_LINTFLAGS,
        _CS_XBS5_LP64_OFF64_CFLAGS,
        _CS_XBS5_LP64_OFF64_LDFLAGS,
        _CS_XBS5_LP64_OFF64_LIBS,
        _CS_XBS5_LP64_OFF64_LINTFLAGS,
        _CS_XBS5_LPBIG_OFFBIG_CFLAGS,
        _CS_XBS5_LPBIG_OFFBIG_LDFLAGS,
        _CS_XBS5_LPBIG_OFFBIG_LIBS,
        _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS,

        _CS_POSIX_V6_ILP32_OFF32_CFLAGS,
        _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,
        _CS_POSIX_V6_ILP32_OFF32_LIBS,
        _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS,
        _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,
        _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,
        _CS_POSIX_V6_ILP32_OFFBIG_LIBS,
        _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS,
        _CS_POSIX_V6_LP64_OFF64_CFLAGS,
        _CS_POSIX_V6_LP64_OFF64_LDFLAGS,
        _CS_POSIX_V6_LP64_OFF64_LIBS,
        _CS_POSIX_V6_LP64_OFF64_LINTFLAGS,
        _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,
        _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,
        _CS_POSIX_V6_LPBIG_OFFBIG_LIBS,
        _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS
    }

    enum
    {
        _PC_LINK_MAX,
        _PC_MAX_CANON,
        _PC_MAX_INPUT,
        _PC_NAME_MAX,
        _PC_PATH_MAX,
        _PC_PIPE_BUF,
        _PC_CHOWN_RESTRICTED,
        _PC_NO_TRUNC,
        _PC_VDISABLE,
        _PC_SYNC_IO,
        _PC_ASYNC_IO,
        _PC_PRIO_IO,
        _PC_SOCK_MAXBUF,
        _PC_FILESIZEBITS,
        _PC_REC_INCR_XFER_SIZE,
        _PC_REC_MAX_XFER_SIZE,
        _PC_REC_MIN_XFER_SIZE,
        _PC_REC_XFER_ALIGN,
        _PC_ALLOC_SIZE_MIN,
        _PC_SYMLINK_MAX,
        _PC_2_SYMLINKS
    }

    enum
    {
        _SC_ARG_MAX,
        _SC_CHILD_MAX,
        _SC_CLK_TCK,
        _SC_NGROUPS_MAX,
        _SC_OPEN_MAX,
        _SC_STREAM_MAX,
        _SC_TZNAME_MAX,
        _SC_JOB_CONTROL,
        _SC_SAVED_IDS,
        _SC_REALTIME_SIGNALS,
        _SC_PRIORITY_SCHEDULING,
        _SC_TIMERS,
        _SC_ASYNCHRONOUS_IO,
        _SC_PRIORITIZED_IO,
        _SC_SYNCHRONIZED_IO,
        _SC_FSYNC,
        _SC_MAPPED_FILES,
        _SC_MEMLOCK,
        _SC_MEMLOCK_RANGE,
        _SC_MEMORY_PROTECTION,
        _SC_MESSAGE_PASSING,
        _SC_SEMAPHORES,
        _SC_SHARED_MEMORY_OBJECTS,
        _SC_AIO_LISTIO_MAX,
        _SC_AIO_MAX,
        _SC_AIO_PRIO_DELTA_MAX,
        _SC_DELAYTIMER_MAX,
        _SC_MQ_OPEN_MAX,
        _SC_MQ_PRIO_MAX,
        _SC_VERSION,
        _SC_PAGESIZE,
        _SC_PAGE_SIZE = _SC_PAGESIZE,
        _SC_RTSIG_MAX,
        _SC_SEM_NSEMS_MAX,
        _SC_SEM_VALUE_MAX,
        _SC_SIGQUEUE_MAX,
        _SC_TIMER_MAX,

        _SC_BC_BASE_MAX,
        _SC_BC_DIM_MAX,
        _SC_BC_SCALE_MAX,
        _SC_BC_STRING_MAX,
        _SC_COLL_WEIGHTS_MAX,
        _SC_EQUIV_CLASS_MAX,
        _SC_EXPR_NEST_MAX,
        _SC_LINE_MAX,
        _SC_RE_DUP_MAX,
        _SC_CHARCLASS_NAME_MAX,

        _SC_2_VERSION,
        _SC_2_C_BIND,
        _SC_2_C_DEV,
        _SC_2_FORT_DEV,
        _SC_2_FORT_RUN,
        _SC_2_SW_DEV,
        _SC_2_LOCALEDEF,

        _SC_PII,
        _SC_PII_XTI,
        _SC_PII_SOCKET,
        _SC_PII_INTERNET,
        _SC_PII_OSI,
        _SC_POLL,
        _SC_SELECT,
        _SC_UIO_MAXIOV,
        _SC_IOV_MAX = _SC_UIO_MAXIOV,
        _SC_PII_INTERNET_STREAM,
        _SC_PII_INTERNET_DGRAM,
        _SC_PII_OSI_COTS,
        _SC_PII_OSI_CLTS,
        _SC_PII_OSI_M,
        _SC_T_IOV_MAX,

        _SC_THREADS,
        _SC_THREAD_SAFE_FUNCTIONS,
        _SC_GETGR_R_SIZE_MAX,
        _SC_GETPW_R_SIZE_MAX,
        _SC_LOGIN_NAME_MAX,
        _SC_TTY_NAME_MAX,
        _SC_THREAD_DESTRUCTOR_ITERATIONS,
        _SC_THREAD_KEYS_MAX,
        _SC_THREAD_STACK_MIN,
        _SC_THREAD_THREADS_MAX,
        _SC_THREAD_ATTR_STACKADDR,
        _SC_THREAD_ATTR_STACKSIZE,
        _SC_THREAD_PRIORITY_SCHEDULING,
        _SC_THREAD_PRIO_INHERIT,
        _SC_THREAD_PRIO_PROTECT,
        _SC_THREAD_PROCESS_SHARED,

        _SC_NPROCESSORS_CONF,
        _SC_NPROCESSORS_ONLN,
        _SC_PHYS_PAGES,
        _SC_AVPHYS_PAGES,
        _SC_ATEXIT_MAX,
        _SC_PASS_MAX,

        _SC_XOPEN_VERSION,
        _SC_XOPEN_XCU_VERSION,
        _SC_XOPEN_UNIX,
        _SC_XOPEN_CRYPT,
        _SC_XOPEN_ENH_I18N,
        _SC_XOPEN_SHM,

        _SC_2_CHAR_TERM,
        _SC_2_C_VERSION,
        _SC_2_UPE,

        _SC_XOPEN_XPG2,
        _SC_XOPEN_XPG3,
        _SC_XOPEN_XPG4,

        _SC_CHAR_BIT,
        _SC_CHAR_MAX,
        _SC_CHAR_MIN,
        _SC_INT_MAX,
        _SC_INT_MIN,
        _SC_LONG_BIT,
        _SC_WORD_BIT,
        _SC_MB_LEN_MAX,
        _SC_NZERO,
        _SC_SSIZE_MAX,
        _SC_SCHAR_MAX,
        _SC_SCHAR_MIN,
        _SC_SHRT_MAX,
        _SC_SHRT_MIN,
        _SC_UCHAR_MAX,
        _SC_UINT_MAX,
        _SC_ULONG_MAX,
        _SC_USHRT_MAX,

        _SC_NL_ARGMAX,
        _SC_NL_LANGMAX,
        _SC_NL_MSGMAX,
        _SC_NL_NMAX,
        _SC_NL_SETMAX,
        _SC_NL_TEXTMAX,

        _SC_XBS5_ILP32_OFF32,
        _SC_XBS5_ILP32_OFFBIG,
        _SC_XBS5_LP64_OFF64,
        _SC_XBS5_LPBIG_OFFBIG,

        _SC_XOPEN_LEGACY,
        _SC_XOPEN_REALTIME,
        _SC_XOPEN_REALTIME_THREADS,

        _SC_ADVISORY_INFO,
        _SC_BARRIERS,
        _SC_BASE,
        _SC_C_LANG_SUPPORT,
        _SC_C_LANG_SUPPORT_R,
        _SC_CLOCK_SELECTION,
        _SC_CPUTIME,
        _SC_THREAD_CPUTIME,
        _SC_DEVICE_IO,
        _SC_DEVICE_SPECIFIC,
        _SC_DEVICE_SPECIFIC_R,
        _SC_FD_MGMT,
        _SC_FIFO,
        _SC_PIPE,
        _SC_FILE_ATTRIBUTES,
        _SC_FILE_LOCKING,
        _SC_FILE_SYSTEM,
        _SC_MONOTONIC_CLOCK,
        _SC_MULTI_PROCESS,
        _SC_SINGLE_PROCESS,
        _SC_NETWORKING,
        _SC_READER_WRITER_LOCKS,
        _SC_SPIN_LOCKS,
        _SC_REGEXP,
        _SC_REGEX_VERSION,
        _SC_SHELL,
        _SC_SIGNALS,
        _SC_SPAWN,
        _SC_SPORADIC_SERVER,
        _SC_THREAD_SPORADIC_SERVER,
        _SC_SYSTEM_DATABASE,
        _SC_SYSTEM_DATABASE_R,
        _SC_TIMEOUTS,
        _SC_TYPED_MEMORY_OBJECTS,
        _SC_USER_GROUPS,
        _SC_USER_GROUPS_R,
        _SC_2_PBS,
        _SC_2_PBS_ACCOUNTING,
        _SC_2_PBS_LOCATE,
        _SC_2_PBS_MESSAGE,
        _SC_2_PBS_TRACK,
        _SC_SYMLOOP_MAX,
        _SC_STREAMS,
        _SC_2_PBS_CHECKPOINT,

        _SC_V6_ILP32_OFF32,
        _SC_V6_ILP32_OFFBIG,
        _SC_V6_LP64_OFF64,
        _SC_V6_LPBIG_OFFBIG,

        _SC_HOST_NAME_MAX,
        _SC_TRACE,
        _SC_TRACE_EVENT_FILTER,
        _SC_TRACE_INHERIT,
        _SC_TRACE_LOG,

        _SC_LEVEL1_ICACHE_SIZE,
        _SC_LEVEL1_ICACHE_ASSOC,
        _SC_LEVEL1_ICACHE_LINESIZE,
        _SC_LEVEL1_DCACHE_SIZE,
        _SC_LEVEL1_DCACHE_ASSOC,
        _SC_LEVEL1_DCACHE_LINESIZE,
        _SC_LEVEL2_CACHE_SIZE,
        _SC_LEVEL2_CACHE_ASSOC,
        _SC_LEVEL2_CACHE_LINESIZE,
        _SC_LEVEL3_CACHE_SIZE,
        _SC_LEVEL3_CACHE_ASSOC,
        _SC_LEVEL3_CACHE_LINESIZE,
        _SC_LEVEL4_CACHE_SIZE,
        _SC_LEVEL4_CACHE_ASSOC,
        _SC_LEVEL4_CACHE_LINESIZE,

        _SC_IPV6 = _SC_LEVEL1_ICACHE_SIZE + 50,
        _SC_RAW_SOCKETS
    }
}
else version (Darwin)
{
    enum F_OK       = 0;
    enum R_OK       = 4;
    enum W_OK       = 2;
    enum X_OK       = 1;

    enum F_ULOCK    = 0;
    enum F_LOCK     = 1;
    enum F_TLOCK    = 2;
    enum F_TEST     = 3;

    enum
    {
        _SC_ARG_MAX                      =   1,
        _SC_CHILD_MAX                    =   2,
        _SC_CLK_TCK                      =   3,
        _SC_NGROUPS_MAX                  =   4,
        _SC_OPEN_MAX                     =   5,
        _SC_JOB_CONTROL                  =   6,
        _SC_SAVED_IDS                    =   7,
        _SC_VERSION                      =   8,
        _SC_BC_BASE_MAX                  =   9,
        _SC_BC_DIM_MAX                   =  10,
        _SC_BC_SCALE_MAX                 =  11,
        _SC_BC_STRING_MAX                =  12,
        _SC_COLL_WEIGHTS_MAX             =  13,
        _SC_EXPR_NEST_MAX                =  14,
        _SC_LINE_MAX                     =  15,
        _SC_RE_DUP_MAX                   =  16,
        _SC_2_VERSION                    =  17,
        _SC_2_C_BIND                     =  18,
        _SC_2_C_DEV                      =  19,
        _SC_2_CHAR_TERM                  =  20,
        _SC_2_FORT_DEV                   =  21,
        _SC_2_FORT_RUN                   =  22,
        _SC_2_LOCALEDEF                  =  23,
        _SC_2_SW_DEV                     =  24,
        _SC_2_UPE                        =  25,
        _SC_STREAM_MAX                   =  26,
        _SC_TZNAME_MAX                   =  27,
        _SC_ASYNCHRONOUS_IO              =  28,
        _SC_PAGESIZE                     =  29,
        _SC_MEMLOCK                      =  30,
        _SC_MEMLOCK_RANGE                =  31,
        _SC_MEMORY_PROTECTION            =  32,
        _SC_MESSAGE_PASSING              =  33,
        _SC_PRIORITIZED_IO               =  34,
        _SC_PRIORITY_SCHEDULING          =  35,
        _SC_REALTIME_SIGNALS             =  36,
        _SC_SEMAPHORES                   =  37,
        _SC_FSYNC                        =  38,
        _SC_SHARED_MEMORY_OBJECTS        =  39,
        _SC_SYNCHRONIZED_IO              =  40,
        _SC_TIMERS                       =  41,
        _SC_AIO_LISTIO_MAX               =  42,
        _SC_AIO_MAX                      =  43,
        _SC_AIO_PRIO_DELTA_MAX           =  44,
        _SC_DELAYTIMER_MAX               =  45,
        _SC_MQ_OPEN_MAX                  =  46,
        _SC_MAPPED_FILES                 =  47,
        _SC_RTSIG_MAX                    =  48,
        _SC_SEM_NSEMS_MAX                =  49,
        _SC_SEM_VALUE_MAX                =  50,
        _SC_SIGQUEUE_MAX                 =  51,
        _SC_TIMER_MAX                    =  52,
        _SC_IOV_MAX                      =  56,
        _SC_NPROCESSORS_CONF             =  57,
        _SC_NPROCESSORS_ONLN             =  58,
        _SC_2_PBS                        =  59,
        _SC_2_PBS_ACCOUNTING             =  60,
        _SC_2_PBS_CHECKPOINT             =  61,
        _SC_2_PBS_LOCATE                 =  62,
        _SC_2_PBS_MESSAGE                =  63,
        _SC_2_PBS_TRACK                  =  64,
        _SC_ADVISORY_INFO                =  65,
        _SC_BARRIERS                     =  66,
        _SC_CLOCK_SELECTION              =  67,
        _SC_CPUTIME                      =  68,
        _SC_FILE_LOCKING                 =  69,
        _SC_GETGR_R_SIZE_MAX             =  70,
        _SC_GETPW_R_SIZE_MAX             =  71,
        _SC_HOST_NAME_MAX                =  72,
        _SC_LOGIN_NAME_MAX               =  73,
        _SC_MONOTONIC_CLOCK              =  74,
        _SC_MQ_PRIO_MAX                  =  75,
        _SC_READER_WRITER_LOCKS          =  76,
        _SC_REGEXP                       =  77,
        _SC_SHELL                        =  78,
        _SC_SPAWN                        =  79,
        _SC_SPIN_LOCKS                   =  80,
        _SC_SPORADIC_SERVER              =  81,
        _SC_THREAD_ATTR_STACKADDR        =  82,
        _SC_THREAD_ATTR_STACKSIZE        =  83,
        _SC_THREAD_CPUTIME               =  84,
        _SC_THREAD_DESTRUCTOR_ITERATIONS =  85,
        _SC_THREAD_KEYS_MAX              =  86,
        _SC_THREAD_PRIO_INHERIT          =  87,
        _SC_THREAD_PRIO_PROTECT          =  88,
        _SC_THREAD_PRIORITY_SCHEDULING   =  89,
        _SC_THREAD_PROCESS_SHARED        =  90,
        _SC_THREAD_SAFE_FUNCTIONS        =  91,
        _SC_THREAD_SPORADIC_SERVER       =  92,
        _SC_THREAD_STACK_MIN             =  93,
        _SC_THREAD_THREADS_MAX           =  94,
        _SC_TIMEOUTS                     =  95,
        _SC_THREADS                      =  96,
        _SC_TRACE                        =  97,
        _SC_TRACE_EVENT_FILTER           =  98,
        _SC_TRACE_INHERIT                =  99,
        _SC_TRACE_LOG                    = 100,
        _SC_TTY_NAME_MAX                 = 101,
        _SC_TYPED_MEMORY_OBJECTS         = 102,
        _SC_V6_ILP32_OFF32               = 103,
        _SC_V6_ILP32_OFFBIG              = 104,
        _SC_V6_LP64_OFF64                = 105,
        _SC_V6_LPBIG_OFFBIG              = 106,
        _SC_ATEXIT_MAX                   = 107,
        _SC_XOPEN_CRYPT                  = 108,
        _SC_XOPEN_ENH_I18N               = 109,
        _SC_XOPEN_LEGACY                 = 110,
        _SC_XOPEN_REALTIME               = 111,
        _SC_XOPEN_REALTIME_THREADS       = 112,
        _SC_XOPEN_SHM                    = 113,
        _SC_XOPEN_STREAMS                = 114,
        _SC_XOPEN_UNIX                   = 115,
        _SC_XOPEN_VERSION                = 116,
        _SC_IPV6                         = 118,
        _SC_RAW_SOCKETS                  = 119,
        _SC_SYMLOOP_MAX                  = 120,
        _SC_XOPEN_XCU_VERSION            = 121,
        _SC_XBS5_ILP32_OFF32             = 122,
        _SC_XBS5_ILP32_OFFBIG            = 123,
        _SC_XBS5_LP64_OFF64              = 124,
        _SC_XBS5_LPBIG_OFFBIG            = 125,
        _SC_SS_REPL_MAX                  = 126,
        _SC_TRACE_EVENT_NAME_MAX         = 127,
        _SC_TRACE_NAME_MAX               = 128,
        _SC_TRACE_SYS_MAX                = 129,
        _SC_TRACE_USER_EVENT_MAX         = 130,
        _SC_PASS_MAX                     = 131,
    }

    enum _SC_PAGE_SIZE = _SC_PAGESIZE;

    enum
    {
        _CS_PATH                                =     1,
        _CS_POSIX_V6_ILP32_OFF32_CFLAGS         =     2,
        _CS_POSIX_V6_ILP32_OFF32_LDFLAGS        =     3,
        _CS_POSIX_V6_ILP32_OFF32_LIBS           =     4,
        _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS        =     5,
        _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS       =     6,
        _CS_POSIX_V6_ILP32_OFFBIG_LIBS          =     7,
        _CS_POSIX_V6_LP64_OFF64_CFLAGS          =     8,
        _CS_POSIX_V6_LP64_OFF64_LDFLAGS         =     9,
        _CS_POSIX_V6_LP64_OFF64_LIBS            =    10,
        _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS        =    11,
        _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS       =    12,
        _CS_POSIX_V6_LPBIG_OFFBIG_LIBS          =    13,
        _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS      =    14,

        _CS_XBS5_ILP32_OFF32_CFLAGS             =    20,
        _CS_XBS5_ILP32_OFF32_LDFLAGS            =    21,
        _CS_XBS5_ILP32_OFF32_LIBS               =    22,
        _CS_XBS5_ILP32_OFF32_LINTFLAGS          =    23,
        _CS_XBS5_ILP32_OFFBIG_CFLAGS            =    24,
        _CS_XBS5_ILP32_OFFBIG_LDFLAGS           =    25,
        _CS_XBS5_ILP32_OFFBIG_LIBS              =    26,
        _CS_XBS5_ILP32_OFFBIG_LINTFLAGS         =    27,
        _CS_XBS5_LP64_OFF64_CFLAGS              =    28,
        _CS_XBS5_LP64_OFF64_LDFLAGS             =    29,
        _CS_XBS5_LP64_OFF64_LIBS                =    30,
        _CS_XBS5_LP64_OFF64_LINTFLAGS           =    31,
        _CS_XBS5_LPBIG_OFFBIG_CFLAGS            =    32,
        _CS_XBS5_LPBIG_OFFBIG_LDFLAGS           =    33,
        _CS_XBS5_LPBIG_OFFBIG_LIBS              =    34,
        _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS         =    35,

        _CS_DARWIN_USER_DIR                     = 65536,
        _CS_DARWIN_USER_TEMP_DIR                = 65537,
        _CS_DARWIN_USER_CACHE_DIR               = 65538,
    }
}
else version (FreeBSD)
{
    enum F_OK       = 0;
    enum R_OK       = 0x04;
    enum W_OK       = 0x02;
    enum X_OK       = 0x01;

    enum F_ULOCK    = 0;
    enum F_LOCK     = 1;
    enum F_TLOCK    = 2;
    enum F_TEST     = 3;

    enum
    {
        _SC_ARG_MAX                        =   1,
        _SC_CHILD_MAX                      =   2,
        _SC_CLK_TCK                        =   3,
        _SC_NGROUPS_MAX                    =   4,
        _SC_OPEN_MAX                       =   5,
        _SC_JOB_CONTROL                    =   6,
        _SC_SAVED_IDS                      =   7,
        _SC_VERSION                        =   8,
        _SC_BC_BASE_MAX                    =   9,
        _SC_BC_DIM_MAX                     =  10,
        _SC_BC_SCALE_MAX                   =  11,
        _SC_BC_STRING_MAX                  =  12,
        _SC_COLL_WEIGHTS_MAX               =  13,
        _SC_EXPR_NEST_MAX                  =  14,
        _SC_LINE_MAX                       =  15,
        _SC_RE_DUP_MAX                     =  16,
        _SC_2_VERSION                      =  17,
        _SC_2_C_BIND                       =  18,
        _SC_2_C_DEV                        =  19,
        _SC_2_CHAR_TERM                    =  20,
        _SC_2_FORT_DEV                     =  21,
        _SC_2_FORT_RUN                     =  22,
        _SC_2_LOCALEDEF                    =  23,
        _SC_2_SW_DEV                       =  24,
        _SC_2_UPE                          =  25,
        _SC_STREAM_MAX                     =  26,
        _SC_TZNAME_MAX                     =  27,
        _SC_ASYNCHRONOUS_IO                =  28,
        _SC_MAPPED_FILES                   =  29,
        _SC_MEMLOCK                        =  30,
        _SC_MEMLOCK_RANGE                  =  31,
        _SC_MEMORY_PROTECTION              =  32,
        _SC_MESSAGE_PASSING                =  33,
        _SC_PRIORITIZED_IO                 =  34,
        _SC_PRIORITY_SCHEDULING            =  35,
        _SC_REALTIME_SIGNALS               =  36,
        _SC_SEMAPHORES                     =  37,
        _SC_FSYNC                          =  38,
        _SC_SHARED_MEMORY_OBJECTS          =  39,
        _SC_SYNCHRONIZED_IO                =  40,
        _SC_TIMERS                         =  41,
        _SC_AIO_LISTIO_MAX                 =  42,
        _SC_AIO_MAX                        =  43,
        _SC_AIO_PRIO_DELTA_MAX             =  44,
        _SC_DELAYTIMER_MAX                 =  45,
        _SC_MQ_OPEN_MAX                    =  46,
        _SC_PAGESIZE                       =  47,
        _SC_RTSIG_MAX                      =  48,
        _SC_SEM_NSEMS_MAX                  =  49,
        _SC_SEM_VALUE_MAX                  =  50,
        _SC_SIGQUEUE_MAX                   =  51,
        _SC_TIMER_MAX                      =  52,
        _SC_IOV_MAX                        =  56,
        _SC_NPROCESSORS_CONF               =  57,
        _SC_NPROCESSORS_ONLN               =  58,
        _SC_2_PBS                          =  59,
        _SC_2_PBS_ACCOUNTING               =  60,
        _SC_2_PBS_CHECKPOINT               =  61,
        _SC_2_PBS_LOCATE                   =  62,
        _SC_2_PBS_MESSAGE                  =  63,
        _SC_2_PBS_TRACK                    =  64,
        _SC_ADVISORY_INFO                  =  65,
        _SC_BARRIERS                       =  66,
        _SC_CLOCK_SELECTION                =  67,
        _SC_CPUTIME                        =  68,
        _SC_FILE_LOCKING                   =  69,
        _SC_GETGR_R_SIZE_MAX               =  70,
        _SC_GETPW_R_SIZE_MAX               =  71,
        _SC_HOST_NAME_MAX                  =  72,
        _SC_LOGIN_NAME_MAX                 =  73,
        _SC_MONOTONIC_CLOCK                =  74,
        _SC_MQ_PRIO_MAX                    =  75,
        _SC_READER_WRITER_LOCKS            =  76,
        _SC_REGEXP                         =  77,
        _SC_SHELL                          =  78,
        _SC_SPAWN                          =  79,
        _SC_SPIN_LOCKS                     =  80,
        _SC_SPORADIC_SERVER                =  81,
        _SC_THREAD_ATTR_STACKADDR          =  82,
        _SC_THREAD_ATTR_STACKSIZE          =  83,
        _SC_THREAD_CPUTIME                 =  84,
        _SC_THREAD_DESTRUCTOR_ITERATIONS   =  85,
        _SC_THREAD_KEYS_MAX                =  86,
        _SC_THREAD_PRIO_INHERIT            =  87,
        _SC_THREAD_PRIO_PROTECT            =  88,
        _SC_THREAD_PRIORITY_SCHEDULING     =  89,
        _SC_THREAD_PROCESS_SHARED          =  90,
        _SC_THREAD_SAFE_FUNCTIONS          =  91,
        _SC_THREAD_SPORADIC_SERVER         =  92,
        _SC_THREAD_STACK_MIN               =  93,
        _SC_THREAD_THREADS_MAX             =  94,
        _SC_TIMEOUTS                       =  95,
        _SC_THREADS                        =  96,
        _SC_TRACE                          =  97,
        _SC_TRACE_EVENT_FILTER             =  98,
        _SC_TRACE_INHERIT                  =  99,
        _SC_TRACE_LOG                      = 100,
        _SC_TTY_NAME_MAX                   = 101,
        _SC_TYPED_MEMORY_OBJECTS           = 102,
        _SC_V6_ILP32_OFF32                 = 103,
        _SC_V6_ILP32_OFFBIG                = 104,
        _SC_V6_LP64_OFF64                  = 105,
        _SC_V6_LPBIG_OFFBIG                = 106,
        _SC_IPV6                           = 118,
        _SC_RAW_SOCKETS                    = 119,
        _SC_SYMLOOP_MAX                    = 120,
        _SC_ATEXIT_MAX                     = 107,
        _SC_XOPEN_CRYPT                    = 108,
        _SC_XOPEN_ENH_I18N                 = 109,
        _SC_XOPEN_LEGACY                   = 110,
        _SC_XOPEN_REALTIME                 = 111,
        _SC_XOPEN_REALTIME_THREADS         = 112,
        _SC_XOPEN_SHM                      = 113,
        _SC_XOPEN_STREAMS                  = 114,
        _SC_XOPEN_UNIX                     = 115,
        _SC_XOPEN_VERSION                  = 116,
        _SC_XOPEN_XCU_VERSION              = 117,
        _SC_CPUSET_SIZE                    = 122,
        _SC_PHYS_PAGES                     = 121,
    }

    enum _SC_PAGE_SIZE = _SC_PAGESIZE;

    enum
    {
        _CS_PATH                           =   1,
        _CS_POSIX_V6_ILP32_OFF32_CFLAGS    =   2,
        _CS_POSIX_V6_ILP32_OFF32_LDFLAGS   =   3,
        _CS_POSIX_V6_ILP32_OFF32_LIBS      =   4,
        _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS   =   5,
        _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS  =   6,
        _CS_POSIX_V6_ILP32_OFFBIG_LIBS     =   7,
        _CS_POSIX_V6_LP64_OFF64_CFLAGS     =   8,
        _CS_POSIX_V6_LP64_OFF64_LDFLAGS    =   9,
        _CS_POSIX_V6_LP64_OFF64_LIBS       =  10,
        _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS   =  11,
        _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS  =  12,
        _CS_POSIX_V6_LPBIG_OFFBIG_LIBS     =  13,
        _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS =  14,
    }
}
else version (NetBSD)
{
    enum F_OK       = 0;
    enum R_OK       = 0x04;
    enum W_OK       = 0x02;
    enum X_OK       = 0x01;

    enum F_ULOCK    = 0;
    enum F_LOCK     = 1;
    enum F_TLOCK    = 2;
    enum F_TEST     = 3;

    enum
    {
        _SC_ARG_MAX                        =   1,
        _SC_CHILD_MAX                      =   2,
        _O_SC_CLK_TCK                      =   3,
        _SC_NGROUPS_MAX                    =   4,
        _SC_OPEN_MAX                       =   5,
        _SC_JOB_CONTROL                    =   6,
        _SC_SAVED_IDS                      =   7,
        _SC_VERSION                        =   8,
        _SC_BC_BASE_MAX                    =   9,
        _SC_BC_DIM_MAX                     =  10,
        _SC_BC_SCALE_MAX                   =  11,
        _SC_BC_STRING_MAX                  =  12,
        _SC_COLL_WEIGHTS_MAX               =  13,
        _SC_EXPR_NEST_MAX                  =  14,
        _SC_LINE_MAX                       =  15,
        _SC_RE_DUP_MAX                     =  16,
        _SC_2_VERSION                      =  17,
        _SC_2_C_BIND                       =  18,
        _SC_2_C_DEV                        =  19,
        _SC_2_CHAR_TERM                    =  20,
        _SC_2_FORT_DEV                     =  21,
        _SC_2_FORT_RUN                     =  22,
        _SC_2_LOCALEDEF                    =  23,
        _SC_2_SW_DEV                       =  24,
        _SC_2_UPE                          =  25,
        _SC_STREAM_MAX                     =  26,
        _SC_TZNAME_MAX                     =  27,
        _SC_PAGESIZE                       =  28,
        _SC_FSYNC                          =  29,
        _SC_XOPEN_SHM                      =  30,
        _SC_SYNCHRONIZED_IO                =  31,
        _SC_IOV_MAX                        =  32,
        _SC_MAPPED_FILES                   =  33,
        _SC_MEMLOCK                        =  34,
        _SC_MEMLOCK_RANGE                  =  35,
        _SC_MEMORY_PROTECTION              =  36,
        _SC_LOGIN_NAME_MAX                 =  37,
        _SC_MONOTONIC_CLOCK                =  38,
        _SC_CLK_TCK                        =  39,
        _SC_ATEXIT_MAX                     =  40,
        _SC_THREADS                        =  41,
        _SC_SEMAPHORES                     =  42,
        _SC_BARRIERS                       =  43,
        _SC_TIMERS                         =  44,
        _SC_SPIN_LOCKS                     =  45,
        _SC_READER_WRITER_LOCKS            =  46,
        _SC_GETGR_R_SIZE_MAX               =  47,
        _SC_GETPW_R_SIZE_MAX               =  48,
        _SC_CLOCK_SELECTION                =  49,
        _SC_ASYNCHRONOUS_IO                =  50,
        _SC_AIO_LISTIO_MAX                 =  51,
        _SC_AIO_MAX                        =  52,
        _SC_MESSAGE_PASSING     = 53,
        _SC_MQ_OPEN_MAX         = 54,
        _SC_MQ_PRIO_MAX         = 55,
        _SC_PRIORITY_SCHEDULING = 56,
        _SC_THREAD_DESTRUCTOR_ITERATIONS = 57,
        _SC_THREAD_KEYS_MAX             = 58,
        _SC_THREAD_STACK_MIN            = 59,
        _SC_THREAD_THREADS_MAX          = 60,
        _SC_THREAD_ATTR_STACKADDR       = 61,
        _SC_THREAD_ATTR_STACKSIZE       = 62,
        _SC_THREAD_PRIORITY_SCHEDULING  = 63,
        _SC_THREAD_PRIO_INHERIT         = 64,
        _SC_THREAD_PRIO_PROTECT         = 65,
        _SC_THREAD_PROCESS_SHARED       = 66,
        _SC_THREAD_SAFE_FUNCTIONS       = 67,
        _SC_TTY_NAME_MAX                = 68,
        _SC_HOST_NAME_MAX               = 69,
        _SC_PASS_MAX                    = 70,
        _SC_REGEXP                      = 71,
        _SC_SHELL                       = 72,
        _SC_SYMLOOP_MAX                 = 73,

        /* Actually, they are not supported or implemented yet */
        _SC_V6_ILP32_OFF32              = 74,
        _SC_V6_ILP32_OFFBIG             = 75,
        _SC_V6_LP64_OFF64               = 76,
        _SC_V6_LPBIG_OFFBIG             = 77,
        _SC_2_PBS                       = 80,
        _SC_2_PBS_ACCOUNTING            = 81,
        _SC_2_PBS_CHECKPOINT            = 82,
        _SC_2_PBS_LOCATE                = 83,
        _SC_2_PBS_MESSAGE               = 84,
        _SC_2_PBS_TRACK                 = 85,

        /* These are implemented */
        _SC_SPAWN                       = 86,
        _SC_SHARED_MEMORY_OBJECTS       = 87,

        /* Extensions found in Solaris and Linux. */
        _SC_PHYS_PAGES          = 121,

        /* Commonly provided sysconf() extensions */
        _SC_NPROCESSORS_CONF    = 1001,
        _SC_NPROCESSORS_ONLN    = 1002,
        /* Native variables */
        _SC_SCHED_RT_TS         = 2001,
        _SC_SCHED_PRI_MIN       = 2002,
        _SC_SCHED_PRI_MAX       = 2003

    }

    enum _SC_PAGE_SIZE = _SC_PAGESIZE;

    enum
    {
        _CS_PATH                           =   1,
        _CS_POSIX_V6_ILP32_OFF32_CFLAGS    =   2,
        _CS_POSIX_V6_ILP32_OFF32_LDFLAGS   =   3,
        _CS_POSIX_V6_ILP32_OFF32_LIBS      =   4,
        _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS   =   5,
        _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS  =   6,
        _CS_POSIX_V6_ILP32_OFFBIG_LIBS     =   7,
        _CS_POSIX_V6_LP64_OFF64_CFLAGS     =   8,
        _CS_POSIX_V6_LP64_OFF64_LDFLAGS    =   9,
        _CS_POSIX_V6_LP64_OFF64_LIBS       =  10,
        _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS   =  11,
        _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS  =  12,
        _CS_POSIX_V6_LPBIG_OFFBIG_LIBS     =  13,
        _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS =  14,
    }
}
else version (OpenBSD)
{
    enum F_OK       = 0;
    enum R_OK       = 0x04;
    enum W_OK       = 0x02;
    enum X_OK       = 0x01;

    enum F_ULOCK    = 0;
    enum F_LOCK     = 1;
    enum F_TLOCK    = 2;
    enum F_TEST     = 3;

    enum
    {
        _SC_ARG_MAX                      =  1,
        _SC_CHILD_MAX                    =  2,
        _O_SC_CLK_TCK                    =  3,
        _SC_NGROUPS_MAX                  =  4,
        _SC_OPEN_MAX                     =  5,
        _SC_JOB_CONTROL                  =  6,
        _SC_SAVED_IDS                    =  7,
        _SC_VERSION                      =  8,
        _SC_BC_BASE_MAX                  =  9,
        _SC_BC_DIM_MAX                   = 10,
        _SC_BC_SCALE_MAX                 = 11,
        _SC_BC_STRING_MAX                = 12,
        _SC_COLL_WEIGHTS_MAX             = 13,
        _SC_EXPR_NEST_MAX                = 14,
        _SC_LINE_MAX                     = 15,
        _SC_RE_DUP_MAX                   = 16,
        _SC_2_VERSION                    = 17,
        _SC_2_C_BIND                     = 18,
        _SC_2_C_DEV                      = 19,
        _SC_2_CHAR_TERM                  = 20,
        _SC_2_FORT_DEV                   = 21,
        _SC_2_FORT_RUN                   = 22,
        _SC_2_LOCALEDEF                  = 23,
        _SC_2_SW_DEV                     = 24,
        _SC_2_UPE                        = 25,
        _SC_STREAM_MAX                   = 26,
        _SC_TZNAME_MAX                   = 27,
        _SC_PAGESIZE                     = 28,
        _SC_FSYNC                        = 29,
        _SC_XOPEN_SHM                    = 30,
        _SC_SEM_NSEMS_MAX                = 31,
        _SC_SEM_VALUE_MAX                = 32,
        _SC_HOST_NAME_MAX                = 33,
        _SC_MONOTONIC_CLOCK              = 34,
        _SC_2_PBS                        = 35,
        _SC_2_PBS_ACCOUNTING             = 36,
        _SC_2_PBS_CHECKPOINT             = 37,
        _SC_2_PBS_LOCATE                 = 38,
        _SC_2_PBS_MESSAGE                = 39,
        _SC_2_PBS_TRACK                  = 40,
        _SC_ADVISORY_INFO                = 41,
        _SC_AIO_LISTIO_MAX               = 42,
        _SC_AIO_MAX                      = 43,
        _SC_AIO_PRIO_DELTA_MAX           = 44,
        _SC_ASYNCHRONOUS_IO              = 45,
        _SC_ATEXIT_MAX                   = 46,
        _SC_BARRIERS                     = 47,
        _SC_CLOCK_SELECTION              = 48,
        _SC_CPUTIME                      = 49,
        _SC_DELAYTIMER_MAX               = 50,
        _SC_IOV_MAX                      = 51,
        _SC_IPV6                         = 52,
        _SC_MAPPED_FILES                 = 53,
        _SC_MEMLOCK                      = 54,
        _SC_MEMLOCK_RANGE                = 55,
        _SC_MEMORY_PROTECTION            = 56,
        _SC_MESSAGE_PASSING              = 57,
        _SC_MQ_OPEN_MAX                  = 58,
        _SC_MQ_PRIO_MAX                  = 59,
        _SC_PRIORITIZED_IO               = 60,
        _SC_PRIORITY_SCHEDULING          = 61,
        _SC_RAW_SOCKETS                  = 62,
        _SC_READER_WRITER_LOCKS          = 63,
        _SC_REALTIME_SIGNALS             = 64,
        _SC_REGEXP                       = 65,
        _SC_RTSIG_MAX                    = 66,
        _SC_SEMAPHORES                   = 67,
        _SC_SHARED_MEMORY_OBJECTS        = 68,
        _SC_SHELL                        = 69,
        _SC_SIGQUEUE_MAX                 = 70,
        _SC_SPAWN                        = 71,
        _SC_SPIN_LOCKS                   = 72,
        _SC_SPORADIC_SERVER              = 73,
        _SC_SS_REPL_MAX                  = 74,
        _SC_SYNCHRONIZED_IO              = 75,
        _SC_SYMLOOP_MAX                  = 76,
        _SC_THREAD_ATTR_STACKADDR        = 77,
        _SC_THREAD_ATTR_STACKSIZE        = 78,
        _SC_THREAD_CPUTIME               = 79,
        _SC_THREAD_DESTRUCTOR_ITERATIONS = 80,
        _SC_THREAD_KEYS_MAX              = 81,
        _SC_THREAD_PRIO_INHERIT          = 82,
        _SC_THREAD_PRIO_PROTECT          = 83,
        _SC_THREAD_PRIORITY_SCHEDULING   = 84,
        _SC_THREAD_PROCESS_SHARED        = 85,
        _SC_THREAD_ROBUST_PRIO_INHERIT   = 86,
        _SC_THREAD_ROBUST_PRIO_PROTECT   = 87,
        _SC_THREAD_SPORADIC_SERVER       = 88,
        _SC_THREAD_STACK_MIN             = 89,
        _SC_THREAD_THREADS_MAX           = 90,
        _SC_THREADS                      = 91,
        _SC_TIMEOUTS                     = 92,
        _SC_TIMER_MAX                    = 93,
        _SC_TIMERS                       = 94,
        _SC_TRACE                        = 95,
        _SC_TRACE_EVENT_FILTER           = 96,
        _SC_TRACE_EVENT_NAME_MAX         = 97,
        _SC_TRACE_INHERIT                = 98,
        _SC_TRACE_LOG                    = 99,
        _SC_GETGR_R_SIZE_MAX             = 100,
        _SC_GETPW_R_SIZE_MAX             = 101,
        _SC_LOGIN_NAME_MAX               = 102,
        _SC_THREAD_SAFE_FUNCTIONS        = 103,
        _SC_TRACE_NAME_MAX               = 104,
        _SC_TRACE_SYS_MAX                = 105,
        _SC_TRACE_USER_EVENT_MAX         = 106,
        _SC_TTY_NAME_MAX                 = 107,
        _SC_TYPED_MEMORY_OBJECTS         = 108,
        _SC_V6_ILP32_OFF32               = 109,
        _SC_V6_ILP32_OFFBIG              = 110,
        _SC_V6_LP64_OFF64                = 111,
        _SC_V6_LPBIG_OFFBIG              = 112,
        _SC_V7_ILP32_OFF32               = 113,
        _SC_V7_ILP32_OFFBIG              = 114,
        _SC_V7_LP64_OFF64                = 115,
        _SC_V7_LPBIG_OFFBIG              = 116,
        _SC_XOPEN_CRYPT                  = 117,
        _SC_XOPEN_ENH_I18N               = 118,
        _SC_XOPEN_LEGACY                 = 119,
        _SC_XOPEN_REALTIME               = 120,
        _SC_XOPEN_REALTIME_THREADS       = 121,
        _SC_XOPEN_STREAMS                = 122,
        _SC_XOPEN_UNIX                   = 123,
        _SC_XOPEN_UUCP                   = 124,
        _SC_XOPEN_VERSION                = 125,
        _SC_PHYS_PAGES                   = 500,
        _SC_AVPHYS_PAGES                 = 501,
        _SC_NPROCESSORS_CONF             = 502,
        _SC_NPROCESSORS_ONLN             = 503,
    }

    enum _SC_PAGE_SIZE = _SC_PAGESIZE;

    enum
    {
        _CS_PATH                           = 1,
        _CS_POSIX_V6_ILP32_OFF32_CFLAGS    = 2,
        _CS_POSIX_V6_ILP32_OFF32_LDFLAGS   = 3,
        _CS_POSIX_V6_ILP32_OFF32_LIBS      = 4,
        _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS   = 5,
        _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS  = 6,
        _CS_POSIX_V6_ILP32_OFFBIG_LIBS     = 7,
        _CS_POSIX_V6_LP64_OFF64_CFLAGS     = 8,
        _CS_POSIX_V6_LP64_OFF64_LDFLAGS    = 9,
        _CS_POSIX_V6_LP64_OFF64_LIBS       = 10,
        _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS   = 11,
        _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS  = 12,
        _CS_POSIX_V6_LPBIG_OFFBIG_LIBS     = 13,
        _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS = 14,
        _CS_V6_ENV                         = 15,
        _CS_POSIX_V7_ILP32_OFF32_CFLAGS    = 16,
        _CS_POSIX_V7_ILP32_OFF32_LDFLAGS   = 17,
        _CS_POSIX_V7_ILP32_OFF32_LIBS      = 18,
        _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS   = 19,
        _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS  = 20,
        _CS_POSIX_V7_ILP32_OFFBIG_LIBS     = 21,
        _CS_POSIX_V7_LP64_OFF64_CFLAGS     = 22,
        _CS_POSIX_V7_LP64_OFF64_LDFLAGS    = 23,
        _CS_POSIX_V7_LP64_OFF64_LIBS       = 24,
        _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS   = 25,
        _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS  = 26,
        _CS_POSIX_V7_LPBIG_OFFBIG_LIBS     = 27,
        _CS_POSIX_V7_THREADS_CFLAGS        = 28,
        _CS_POSIX_V7_THREADS_LDFLAGS       = 29,
        _CS_POSIX_V7_WIDTH_RESTRICTED_ENVS = 30,
        _CS_V7_ENV                         = 31,
    }
}
else version (DragonFlyBSD)
{
    enum F_OK       = 0;
    enum R_OK       = 0x04;
    enum W_OK       = 0x02;
    enum X_OK       = 0x01;

    enum F_ULOCK    = 0;
    enum F_LOCK     = 1;
    enum F_TLOCK    = 2;
    enum F_TEST     = 3;

    enum
    {
        _SC_ARG_MAX                        =   1,
        _SC_CHILD_MAX                      =   2,
        _SC_CLK_TCK                        =   3,
        _SC_NGROUPS_MAX                    =   4,
        _SC_OPEN_MAX                       =   5,
        _SC_JOB_CONTROL                    =   6,
        _SC_SAVED_IDS                      =   7,
        _SC_VERSION                        =   8,
        _SC_BC_BASE_MAX                    =   9,
        _SC_BC_DIM_MAX                     =  10,
        _SC_BC_SCALE_MAX                   =  11,
        _SC_BC_STRING_MAX                  =  12,
        _SC_COLL_WEIGHTS_MAX               =  13,
        _SC_EXPR_NEST_MAX                  =  14,
        _SC_LINE_MAX                       =  15,
        _SC_RE_DUP_MAX                     =  16,
        _SC_2_VERSION                      =  17,
        _SC_2_C_BIND                       =  18,
        _SC_2_C_DEV                        =  19,
        _SC_2_CHAR_TERM                    =  20,
        _SC_2_FORT_DEV                     =  21,
        _SC_2_FORT_RUN                     =  22,
        _SC_2_LOCALEDEF                    =  23,
        _SC_2_SW_DEV                       =  24,
        _SC_2_UPE                          =  25,
        _SC_STREAM_MAX                     =  26,
        _SC_TZNAME_MAX                     =  27,
        _SC_ASYNCHRONOUS_IO                =  28,
        _SC_MAPPED_FILES                   =  29,
        _SC_MEMLOCK                        =  30,
        _SC_MEMLOCK_RANGE                  =  31,
        _SC_MEMORY_PROTECTION              =  32,
        _SC_MESSAGE_PASSING                =  33,
        _SC_PRIORITIZED_IO                 =  34,
        _SC_PRIORITY_SCHEDULING            =  35,
        _SC_REALTIME_SIGNALS               =  36,
        _SC_SEMAPHORES                     =  37,
        _SC_FSYNC                          =  38,
        _SC_SHARED_MEMORY_OBJECTS          =  39,
        _SC_SYNCHRONIZED_IO                =  40,
        _SC_TIMERS                         =  41,
        _SC_AIO_LISTIO_MAX                 =  42,
        _SC_AIO_MAX                        =  43,
        _SC_AIO_PRIO_DELTA_MAX             =  44,
        _SC_DELAYTIMER_MAX                 =  45,
        _SC_MQ_OPEN_MAX                    =  46,
        _SC_PAGESIZE                       =  47,
        _SC_RTSIG_MAX                      =  48,
        _SC_SEM_NSEMS_MAX                  =  49,
        _SC_SEM_VALUE_MAX                  =  50,
        _SC_SIGQUEUE_MAX                   =  51,
        _SC_TIMER_MAX                      =  52,
        _SC_IOV_MAX                        =  56,
        _SC_NPROCESSORS_CONF               =  57,
        _SC_NPROCESSORS_ONLN               =  58,
        _SC_2_PBS                          =  59,
        _SC_2_PBS_ACCOUNTING               =  60,
        _SC_2_PBS_CHECKPOINT               =  61,
        _SC_2_PBS_LOCATE                   =  62,
        _SC_2_PBS_MESSAGE                  =  63,
        _SC_2_PBS_TRACK                    =  64,
        _SC_ADVISORY_INFO                  =  65,
        _SC_BARRIERS                       =  66,
        _SC_CLOCK_SELECTION                =  67,
        _SC_CPUTIME                        =  68,
        _SC_FILE_LOCKING                   =  69,
        _SC_GETGR_R_SIZE_MAX               =  70,
        _SC_GETPW_R_SIZE_MAX               =  71,
        _SC_HOST_NAME_MAX                  =  72,
        _SC_LOGIN_NAME_MAX                 =  73,
        _SC_MONOTONIC_CLOCK                =  74,
        _SC_MQ_PRIO_MAX                    =  75,
        _SC_READER_WRITER_LOCKS            =  76,
        _SC_REGEXP                         =  77,
        _SC_SHELL                          =  78,
        _SC_SPAWN                          =  79,
        _SC_SPIN_LOCKS                     =  80,
        _SC_SPORADIC_SERVER                =  81,
        _SC_THREAD_ATTR_STACKADDR          =  82,
        _SC_THREAD_ATTR_STACKSIZE          =  83,
        _SC_THREAD_CPUTIME                 =  84,
        _SC_THREAD_DESTRUCTOR_ITERATIONS   =  85,
        _SC_THREAD_KEYS_MAX                =  86,
        _SC_THREAD_PRIO_INHERIT            =  87,
        _SC_THREAD_PRIO_PROTECT            =  88,
        _SC_THREAD_PRIORITY_SCHEDULING     =  89,
        _SC_THREAD_PROCESS_SHARED          =  90,
        _SC_THREAD_SAFE_FUNCTIONS          =  91,
        _SC_THREAD_SPORADIC_SERVER         =  92,
        _SC_THREAD_STACK_MIN               =  93,
        _SC_THREAD_THREADS_MAX             =  94,
        _SC_TIMEOUTS                       =  95,
        _SC_THREADS                        =  96,
        _SC_TRACE                          =  97,
        _SC_TRACE_EVENT_FILTER             =  98,
        _SC_TRACE_INHERIT                  =  99,
        _SC_TRACE_LOG                      = 100,
        _SC_TTY_NAME_MAX                   = 101,
        _SC_TYPED_MEMORY_OBJECTS           = 102,
        _SC_V6_ILP32_OFF32                 = 103,
        _SC_V6_ILP32_OFFBIG                = 104,
        _SC_V6_LP64_OFF64                  = 105,
        _SC_V6_LPBIG_OFFBIG                = 106,
        _SC_IPV6                           = 118,
        _SC_RAW_SOCKETS                    = 119,
        _SC_SYMLOOP_MAX                    = 120,
        _SC_ATEXIT_MAX                     = 107,
        _SC_XOPEN_CRYPT                    = 108,
        _SC_XOPEN_ENH_I18N                 = 109,
        _SC_XOPEN_LEGACY                   = 110,
        _SC_XOPEN_REALTIME                 = 111,
        _SC_XOPEN_REALTIME_THREADS         = 112,
        _SC_XOPEN_SHM                      = 113,
        _SC_XOPEN_STREAMS                  = 114,
        _SC_XOPEN_UNIX                     = 115,
        _SC_XOPEN_VERSION                  = 116,
        _SC_XOPEN_XCU_VERSION              = 117,
        _SC_CPUSET_SIZE                    = 122,
        _SC_PHYS_PAGES                     = 121,
    }

    enum _SC_PAGE_SIZE = _SC_PAGESIZE;

    enum
    {
        _CS_PATH                           =   1,
        _CS_POSIX_V6_ILP32_OFF32_CFLAGS    =   2,
        _CS_POSIX_V6_ILP32_OFF32_LDFLAGS   =   3,
        _CS_POSIX_V6_ILP32_OFF32_LIBS      =   4,
        _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS   =   5,
        _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS  =   6,
        _CS_POSIX_V6_ILP32_OFFBIG_LIBS     =   7,
        _CS_POSIX_V6_LP64_OFF64_CFLAGS     =   8,
        _CS_POSIX_V6_LP64_OFF64_LDFLAGS    =   9,
        _CS_POSIX_V6_LP64_OFF64_LIBS       =  10,
        _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS   =  11,
        _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS  =  12,
        _CS_POSIX_V6_LPBIG_OFFBIG_LIBS     =  13,
        _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS =  14,
    }
}
else version (CRuntime_Bionic)
{
    enum F_OK       = 0;
    enum R_OK       = 4;
    enum W_OK       = 2;
    enum X_OK       = 1;

    enum
    {
        _SC_ARG_MAX             = 0x0000,
        _SC_BC_BASE_MAX         = 0x0001,
        _SC_BC_DIM_MAX          = 0x0002,
        _SC_BC_SCALE_MAX        = 0x0003,
        _SC_BC_STRING_MAX       = 0x0004,
        _SC_CHILD_MAX           = 0x0005,
        _SC_CLK_TCK             = 0x0006,
        _SC_COLL_WEIGHTS_MAX    = 0x0007,
        _SC_EXPR_NEST_MAX       = 0x0008,
        _SC_LINE_MAX            = 0x0009,
        _SC_NGROUPS_MAX         = 0x000a,
        _SC_OPEN_MAX            = 0x000b,
        _SC_PASS_MAX            = 0x000c,
        _SC_2_C_BIND            = 0x000d,
        _SC_2_C_DEV             = 0x000e,
        _SC_2_C_VERSION         = 0x000f,
        _SC_2_CHAR_TERM         = 0x0010,
        _SC_2_FORT_DEV          = 0x0011,
        _SC_2_FORT_RUN          = 0x0012,
        _SC_2_LOCALEDEF         = 0x0013,
        _SC_2_SW_DEV            = 0x0014,
        _SC_2_UPE               = 0x0015,
        _SC_2_VERSION           = 0x0016,
        _SC_JOB_CONTROL         = 0x0017,
        _SC_SAVED_IDS           = 0x0018,
        _SC_VERSION             = 0x0019,
        _SC_RE_DUP_MAX          = 0x001a,
        _SC_STREAM_MAX          = 0x001b,
        _SC_TZNAME_MAX          = 0x001c,
        _SC_XOPEN_CRYPT         = 0x001d,
        _SC_XOPEN_ENH_I18N      = 0x001e,
        _SC_XOPEN_SHM           = 0x001f,
        _SC_XOPEN_VERSION       = 0x0020,
        _SC_XOPEN_XCU_VERSION   = 0x0021,
        _SC_XOPEN_REALTIME      = 0x0022,
        _SC_XOPEN_REALTIME_THREADS = 0x0023,
        _SC_XOPEN_LEGACY        = 0x0024,
        _SC_ATEXIT_MAX          = 0x0025,
        _SC_IOV_MAX             = 0x0026,
        _SC_UIO_MAXIOV          = _SC_IOV_MAX,
        _SC_PAGESIZE            = 0x0027,
        _SC_PAGE_SIZE           = 0x0028,
        _SC_XOPEN_UNIX          = 0x0029,
        _SC_XBS5_ILP32_OFF32    = 0x002a,
        _SC_XBS5_ILP32_OFFBIG   = 0x002b,
        _SC_XBS5_LP64_OFF64     = 0x002c,
        _SC_XBS5_LPBIG_OFFBIG   = 0x002d,
        _SC_AIO_LISTIO_MAX      = 0x002e,
        _SC_AIO_MAX             = 0x002f,
        _SC_AIO_PRIO_DELTA_MAX  = 0x0030,
        _SC_DELAYTIMER_MAX      = 0x0031,
        _SC_MQ_OPEN_MAX         = 0x0032,
        _SC_MQ_PRIO_MAX         = 0x0033,
        _SC_RTSIG_MAX           = 0x0034,
        _SC_SEM_NSEMS_MAX       = 0x0035,
        _SC_SEM_VALUE_MAX       = 0x0036,
        _SC_SIGQUEUE_MAX        = 0x0037,
        _SC_TIMER_MAX           = 0x0038,
        _SC_ASYNCHRONOUS_IO     = 0x0039,
        _SC_FSYNC               = 0x003a,
        _SC_MAPPED_FILES        = 0x003b,
        _SC_MEMLOCK             = 0x003c,
        _SC_MEMLOCK_RANGE       = 0x003d,
        _SC_MEMORY_PROTECTION   = 0x003e,
        _SC_MESSAGE_PASSING     = 0x003f,
        _SC_PRIORITIZED_IO      = 0x0040,
        _SC_PRIORITY_SCHEDULING = 0x0041,
        _SC_REALTIME_SIGNALS    = 0x0042,
        _SC_SEMAPHORES          = 0x0043,
        _SC_SHARED_MEMORY_OBJECTS = 0x0044,
        _SC_SYNCHRONIZED_IO     = 0x0045,
        _SC_TIMERS              = 0x0046,
        _SC_GETGR_R_SIZE_MAX    = 0x0047,
        _SC_GETPW_R_SIZE_MAX    = 0x0048,
        _SC_LOGIN_NAME_MAX      = 0x0049,
        _SC_THREAD_DESTRUCTOR_ITERATIONS = 0x004a,
        _SC_THREAD_KEYS_MAX     = 0x004b,
        _SC_THREAD_STACK_MIN    = 0x004c,
        _SC_THREAD_THREADS_MAX  = 0x004d,
        _SC_TTY_NAME_MAX        = 0x004e,

        _SC_THREADS                    = 0x004f,
        _SC_THREAD_ATTR_STACKADDR      = 0x0050,
        _SC_THREAD_ATTR_STACKSIZE      = 0x0051,
        _SC_THREAD_PRIORITY_SCHEDULING = 0x0052,
        _SC_THREAD_PRIO_INHERIT        = 0x0053,
        _SC_THREAD_PRIO_PROTECT        = 0x0054,
        _SC_THREAD_SAFE_FUNCTIONS      = 0x0055,

        _SC_NPROCESSORS_CONF           = 0x0060,
        _SC_NPROCESSORS_ONLN           = 0x0061,
        _SC_PHYS_PAGES                 = 0x0062,
        _SC_AVPHYS_PAGES               = 0x0063,
        _SC_MONOTONIC_CLOCK            = 0x0064,

        _SC_2_PBS               = 0x0065,
        _SC_2_PBS_ACCOUNTING    = 0x0066,
        _SC_2_PBS_CHECKPOINT    = 0x0067,
        _SC_2_PBS_LOCATE        = 0x0068,
        _SC_2_PBS_MESSAGE       = 0x0069,
        _SC_2_PBS_TRACK         = 0x006a,
        _SC_ADVISORY_INFO       = 0x006b,
        _SC_BARRIERS            = 0x006c,
        _SC_CLOCK_SELECTION     = 0x006d,
        _SC_CPUTIME             = 0x006e,
        _SC_HOST_NAME_MAX       = 0x006f,
        _SC_IPV6                = 0x0070,
        _SC_RAW_SOCKETS         = 0x0071,
        _SC_READER_WRITER_LOCKS = 0x0072,
        _SC_REGEXP              = 0x0073,
        _SC_SHELL               = 0x0074,
        _SC_SPAWN               = 0x0075,
        _SC_SPIN_LOCKS          = 0x0076,
        _SC_SPORADIC_SERVER     = 0x0077,
        _SC_SS_REPL_MAX         = 0x0078,
        _SC_SYMLOOP_MAX         = 0x0079,
        _SC_THREAD_CPUTIME      = 0x007a,
        _SC_THREAD_PROCESS_SHARED      = 0x007b,
        _SC_THREAD_ROBUST_PRIO_INHERIT = 0x007c,
        _SC_THREAD_ROBUST_PRIO_PROTECT = 0x007d,
        _SC_THREAD_SPORADIC_SERVER     = 0x007e,
        _SC_TIMEOUTS            = 0x007f,
        _SC_TRACE               = 0x0080,
        _SC_TRACE_EVENT_FILTER  = 0x0081,
        _SC_TRACE_EVENT_NAME_MAX = 0x0082,
        _SC_TRACE_INHERIT       = 0x0083,
        _SC_TRACE_LOG           = 0x0084,
        _SC_TRACE_NAME_MAX      = 0x0085,
        _SC_TRACE_SYS_MAX       = 0x0086,
        _SC_TRACE_USER_EVENT_MAX = 0x0087,
        _SC_TYPED_MEMORY_OBJECTS = 0x0088,
        _SC_V7_ILP32_OFF32      = 0x0089,
        _SC_V7_ILP32_OFFBIG     = 0x008a,
        _SC_V7_LP64_OFF64       = 0x008b,
        _SC_V7_LPBIG_OFFBIG     = 0x008c,
        _SC_XOPEN_STREAMS       = 0x008d,
        _SC_XOPEN_UUCP          = 0x008e,

        _SC_LEVEL1_ICACHE_SIZE     = 0x008f,
        _SC_LEVEL1_ICACHE_ASSOC    = 0x0090,
        _SC_LEVEL1_ICACHE_LINESIZE = 0x0091,
        _SC_LEVEL1_DCACHE_SIZE     = 0x0092,
        _SC_LEVEL1_DCACHE_ASSOC    = 0x0093,
        _SC_LEVEL1_DCACHE_LINESIZE = 0x0094,
        _SC_LEVEL2_CACHE_SIZE      = 0x0095,
        _SC_LEVEL2_CACHE_ASSOC     = 0x0096,
        _SC_LEVEL2_CACHE_LINESIZE  = 0x0097,
        _SC_LEVEL3_CACHE_SIZE      = 0x0098,
        _SC_LEVEL3_CACHE_ASSOC     = 0x0099,
        _SC_LEVEL3_CACHE_LINESIZE  = 0x009a,
        _SC_LEVEL4_CACHE_SIZE      = 0x009b,
        _SC_LEVEL4_CACHE_ASSOC     = 0x009c,
        _SC_LEVEL4_CACHE_LINESIZE  = 0x009d,
    }
}
else version (Solaris)
{
    enum F_OK       = 0;
    enum R_OK       = 4;
    enum W_OK       = 2;
    enum X_OK       = 1;

    enum F_ULOCK    = 0;
    enum F_LOCK     = 1;
    enum F_TLOCK    = 2;
    enum F_TEST     = 3;

    enum
    {
        // large file compilation environment configuration
        _CS_LFS_CFLAGS                  = 68,
        _CS_LFS_LDFLAGS                 = 69,
        _CS_LFS_LIBS                    = 70,
        _CS_LFS_LINTFLAGS               = 71,
        // transitional large file interface configuration
        _CS_LFS64_CFLAGS                = 72,
        _CS_LFS64_LDFLAGS               = 73,
        _CS_LFS64_LIBS                  = 74,
        _CS_LFS64_LINTFLAGS             = 75,

        // UNIX 98
        _CS_XBS5_ILP32_OFF32_CFLAGS     = 700,
        _CS_XBS5_ILP32_OFF32_LDFLAGS    = 701,
        _CS_XBS5_ILP32_OFF32_LIBS       = 702,
        _CS_XBS5_ILP32_OFF32_LINTFLAGS  = 703,
        _CS_XBS5_ILP32_OFFBIG_CFLAGS    = 705,
        _CS_XBS5_ILP32_OFFBIG_LDFLAGS   = 706,
        _CS_XBS5_ILP32_OFFBIG_LIBS      = 707,
        _CS_XBS5_ILP32_OFFBIG_LINTFLAGS = 708,
        _CS_XBS5_LP64_OFF64_CFLAGS      = 709,
        _CS_XBS5_LP64_OFF64_LDFLAGS     = 710,
        _CS_XBS5_LP64_OFF64_LIBS        = 711,
        _CS_XBS5_LP64_OFF64_LINTFLAGS   = 712,
        _CS_XBS5_LPBIG_OFFBIG_CFLAGS    = 713,
        _CS_XBS5_LPBIG_OFFBIG_LDFLAGS   = 714,
        _CS_XBS5_LPBIG_OFFBIG_LIBS      = 715,
        _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS = 716,

        // UNIX 03
        _CS_POSIX_V6_ILP32_OFF32_CFLAGS         = 800,
        _CS_POSIX_V6_ILP32_OFF32_LDFLAGS        = 801,
        _CS_POSIX_V6_ILP32_OFF32_LIBS           = 802,
        _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS      = 803,
        _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS        = 804,
        _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS       = 805,
        _CS_POSIX_V6_ILP32_OFFBIG_LIBS          = 806,
        _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS     = 807,
        _CS_POSIX_V6_LP64_OFF64_CFLAGS          = 808,
        _CS_POSIX_V6_LP64_OFF64_LDFLAGS         = 809,
        _CS_POSIX_V6_LP64_OFF64_LIBS            = 810,
        _CS_POSIX_V6_LP64_OFF64_LINTFLAGS       = 811,
        _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS        = 812,
        _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS       = 813,
        _CS_POSIX_V6_LPBIG_OFFBIG_LIBS          = 814,
        _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS     = 815,
        _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS      = 816
    }

    enum {
        _SC_ARG_MAX                     = 1,
        _SC_CHILD_MAX                   = 2,
        _SC_CLK_TCK                     = 3,
        _SC_NGROUPS_MAX                 = 4,
        _SC_OPEN_MAX                    = 5,
        _SC_JOB_CONTROL                 = 6,
        _SC_SAVED_IDS                   = 7,
        _SC_VERSION                     = 8,

        _SC_PASS_MAX                    = 9,
        _SC_LOGNAME_MAX                 = 10,
        _SC_PAGESIZE                    = 11,
        _SC_XOPEN_VERSION               = 12,
        // 13 reserved for SVr4-ES/MP _SC_NACLS_MAX
        _SC_NPROCESSORS_CONF            = 14,
        _SC_NPROCESSORS_ONLN            = 15,
        _SC_STREAM_MAX                  = 16,
        _SC_TZNAME_MAX                  = 17,

        _SC_AIO_LISTIO_MAX              = 18,
        _SC_AIO_MAX                     = 19,
        _SC_AIO_PRIO_DELTA_MAX          = 20,
        _SC_ASYNCHRONOUS_IO             = 21,
        _SC_DELAYTIMER_MAX              = 22,
        _SC_FSYNC                       = 23,
        _SC_MAPPED_FILES                = 24,
        _SC_MEMLOCK                     = 25,
        _SC_MEMLOCK_RANGE               = 26,
        _SC_MEMORY_PROTECTION           = 27,
        _SC_MESSAGE_PASSING             = 28,
        _SC_MQ_OPEN_MAX                 = 29,
        _SC_MQ_PRIO_MAX                 = 30,
        _SC_PRIORITIZED_IO              = 31,
        _SC_PRIORITY_SCHEDULING         = 32,
        _SC_REALTIME_SIGNALS            = 33,
        _SC_RTSIG_MAX                   = 34,
        _SC_SEMAPHORES                  = 35,
        _SC_SEM_NSEMS_MAX               = 36,
        _SC_SEM_VALUE_MAX               = 37,
        _SC_SHARED_MEMORY_OBJECTS       = 38,
        _SC_SIGQUEUE_MAX                = 39,
        _SC_SIGRT_MIN                   = 40,
        _SC_SIGRT_MAX                   = 41,
        _SC_SYNCHRONIZED_IO             = 42,
        _SC_TIMERS                      = 43,
        _SC_TIMER_MAX                   = 44,

        _SC_2_C_BIND                    = 45,
        _SC_2_C_DEV                     = 46,
        _SC_2_C_VERSION                 = 47,
        _SC_2_FORT_DEV                  = 48,
        _SC_2_FORT_RUN                  = 49,
        _SC_2_LOCALEDEF                 = 50,
        _SC_2_SW_DEV                    = 51,
        _SC_2_UPE                       = 52,
        _SC_2_VERSION                   = 53,
        _SC_BC_BASE_MAX                 = 54,
        _SC_BC_DIM_MAX                  = 55,
        _SC_BC_SCALE_MAX                = 56,
        _SC_BC_STRING_MAX               = 57,
        _SC_COLL_WEIGHTS_MAX            = 58,
        _SC_EXPR_NEST_MAX               = 59,
        _SC_LINE_MAX                    = 60,
        _SC_RE_DUP_MAX                  = 61,
        _SC_XOPEN_CRYPT                 = 62,
        _SC_XOPEN_ENH_I18N              = 63,
        _SC_XOPEN_SHM                   = 64,
        _SC_2_CHAR_TERM                 = 66,
        _SC_XOPEN_XCU_VERSION           = 67,

        _SC_ATEXIT_MAX                  = 76,
        _SC_IOV_MAX                     = 77,
        _SC_XOPEN_UNIX                  = 78,

        _SC_T_IOV_MAX                   = 79,

        _SC_PHYS_PAGES                  = 500,
        _SC_AVPHYS_PAGES                = 501,

        _SC_COHER_BLKSZ         = 503,
        _SC_SPLIT_CACHE         = 504,
        _SC_ICACHE_SZ           = 505,
        _SC_DCACHE_SZ           = 506,
        _SC_ICACHE_LINESZ       = 507,
        _SC_DCACHE_LINESZ       = 508,
        _SC_ICACHE_BLKSZ        = 509,
        _SC_DCACHE_BLKSZ        = 510,
        _SC_DCACHE_TBLKSZ       = 511,
        _SC_ICACHE_ASSOC        = 512,
        _SC_DCACHE_ASSOC        = 513,

        _SC_MAXPID              = 514,
        _SC_STACK_PROT          = 515,
        _SC_NPROCESSORS_MAX     = 516,
        _SC_CPUID_MAX           = 517,
        _SC_EPHID_MAX           = 518,

        _SC_THREAD_DESTRUCTOR_ITERATIONS = 568,
        _SC_GETGR_R_SIZE_MAX            = 569,
        _SC_GETPW_R_SIZE_MAX            = 570,
        _SC_LOGIN_NAME_MAX              = 571,
        _SC_THREAD_KEYS_MAX             = 572,
        _SC_THREAD_STACK_MIN            = 573,
        _SC_THREAD_THREADS_MAX          = 574,
        _SC_TTY_NAME_MAX                = 575,
        _SC_THREADS                     = 576,
        _SC_THREAD_ATTR_STACKADDR       = 577,
        _SC_THREAD_ATTR_STACKSIZE       = 578,
        _SC_THREAD_PRIORITY_SCHEDULING  = 579,
        _SC_THREAD_PRIO_INHERIT         = 580,
        _SC_THREAD_PRIO_PROTECT         = 581,
        _SC_THREAD_PROCESS_SHARED       = 582,
        _SC_THREAD_SAFE_FUNCTIONS       = 583,

        _SC_XOPEN_LEGACY                = 717,
        _SC_XOPEN_REALTIME              = 718,
        _SC_XOPEN_REALTIME_THREADS      = 719,
        _SC_XBS5_ILP32_OFF32            = 720,
        _SC_XBS5_ILP32_OFFBIG           = 721,
        _SC_XBS5_LP64_OFF64             = 722,
        _SC_XBS5_LPBIG_OFFBIG           = 723,

        _SC_2_PBS                       = 724,
        _SC_2_PBS_ACCOUNTING            = 725,
        _SC_2_PBS_CHECKPOINT            = 726,
        _SC_2_PBS_LOCATE                = 728,
        _SC_2_PBS_MESSAGE               = 729,
        _SC_2_PBS_TRACK                 = 730,
        _SC_ADVISORY_INFO               = 731,
        _SC_BARRIERS                    = 732,
        _SC_CLOCK_SELECTION             = 733,
        _SC_CPUTIME                     = 734,
        _SC_HOST_NAME_MAX               = 735,
        _SC_MONOTONIC_CLOCK             = 736,
        _SC_READER_WRITER_LOCKS         = 737,
        _SC_REGEXP                      = 738,
        _SC_SHELL                       = 739,
        _SC_SPAWN                       = 740,
        _SC_SPIN_LOCKS                  = 741,
        _SC_SPORADIC_SERVER             = 742,
        _SC_SS_REPL_MAX                 = 743,
        _SC_SYMLOOP_MAX                 = 744,
        _SC_THREAD_CPUTIME              = 745,
        _SC_THREAD_SPORADIC_SERVER      = 746,
        _SC_TIMEOUTS                    = 747,
        _SC_TRACE                       = 748,
        _SC_TRACE_EVENT_FILTER          = 749,
        _SC_TRACE_EVENT_NAME_MAX        = 750,
        _SC_TRACE_INHERIT               = 751,
        _SC_TRACE_LOG                   = 752,
        _SC_TRACE_NAME_MAX              = 753,
        _SC_TRACE_SYS_MAX               = 754,
        _SC_TRACE_USER_EVENT_MAX        = 755,
        _SC_TYPED_MEMORY_OBJECTS        = 756,
        _SC_V6_ILP32_OFF32              = 757,
        _SC_V6_ILP32_OFFBIG             = 758,
        _SC_V6_LP64_OFF64               = 759,
        _SC_V6_LPBIG_OFFBIG             = 760,
        _SC_XOPEN_STREAMS               = 761,
        _SC_IPV6                        = 762,
        _SC_RAW_SOCKETS                 = 763,
    }
    enum _SC_PAGE_SIZE = _SC_PAGESIZE;

    enum {
        _PC_LINK_MAX            = 1,
        _PC_MAX_CANON           = 2,
        _PC_MAX_INPUT           = 3,
        _PC_NAME_MAX            = 4,
        _PC_PATH_MAX            = 5,
        _PC_PIPE_BUF            = 6,
        _PC_NO_TRUNC            = 7,
        _PC_VDISABLE            = 8,
        _PC_CHOWN_RESTRICTED    = 9,

        _PC_ASYNC_IO            = 10,
        _PC_PRIO_IO             = 11,
        _PC_SYNC_IO             = 12,

        _PC_ALLOC_SIZE_MIN      = 13,
        _PC_REC_INCR_XFER_SIZE  = 14,
        _PC_REC_MAX_XFER_SIZE   = 15,
        _PC_REC_MIN_XFER_SIZE   = 16,
        _PC_REC_XFER_ALIGN      = 17,
        _PC_SYMLINK_MAX         = 18,
        _PC_2_SYMLINKS          = 19,
        _PC_ACL_ENABLED         = 20,
        _PC_MIN_HOLE_SIZE       = 21,
        _PC_CASE_BEHAVIOR       = 22,
        _PC_SATTR_ENABLED       = 23,
        _PC_SATTR_EXISTS        = 24,
        _PC_ACCESS_FILTERING    = 25,

        _PC_TIMESTAMP_RESOLUTION = 26,

        _PC_FILESIZEBITS        = 67,

        _PC_XATTR_ENABLED       = 100,
        _PC_XATTR_EXISTS        = 101
    }

    enum _PC_LAST = 101;
}
else version (CRuntime_Musl)
{
    enum F_OK       = 0;
    enum R_OK       = 4;
    enum W_OK       = 2;
    enum X_OK       = 1;

    enum F_ULOCK    = 0;
    enum F_LOCK     = 1;
    enum F_TLOCK    = 2;
    enum F_TEST     = 3;

    enum
    {
        _CS_PATH,
        _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS,
        _CS_GNU_LIBC_VERSION,
        _CS_GNU_LIBPTHREAD_VERSION,
        _CS_POSIX_V5_WIDTH_RESTRICTED_ENVS,
        _CS_POSIX_V7_WIDTH_RESTRICTED_ENVS,

        _CS_POSIX_V6_ILP32_OFF32_CFLAGS = 1116,
        _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,
        _CS_POSIX_V6_ILP32_OFF32_LIBS,
        _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS,
        _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,
        _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,
        _CS_POSIX_V6_ILP32_OFFBIG_LIBS,
        _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS,
        _CS_POSIX_V6_LP64_OFF64_CFLAGS,
        _CS_POSIX_V6_LP64_OFF64_LDFLAGS,
        _CS_POSIX_V6_LP64_OFF64_LIBS,
        _CS_POSIX_V6_LP64_OFF64_LINTFLAGS,
        _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,
        _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,
        _CS_POSIX_V6_LPBIG_OFFBIG_LIBS,
        _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS,
        _CS_POSIX_V7_ILP32_OFF32_CFLAGS,
        _CS_POSIX_V7_ILP32_OFF32_LDFLAGS,
        _CS_POSIX_V7_ILP32_OFF32_LIBS,
        _CS_POSIX_V7_ILP32_OFF32_LINTFLAGS,
        _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS,
        _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS,
        _CS_POSIX_V7_ILP32_OFFBIG_LIBS,
        _CS_POSIX_V7_ILP32_OFFBIG_LINTFLAGS,
        _CS_POSIX_V7_LP64_OFF64_CFLAGS,
        _CS_POSIX_V7_LP64_OFF64_LDFLAGS,
        _CS_POSIX_V7_LP64_OFF64_LIBS,
        _CS_POSIX_V7_LP64_OFF64_LINTFLAGS,
        _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS,
        _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS,
        _CS_POSIX_V7_LPBIG_OFFBIG_LIBS,
        _CS_POSIX_V7_LPBIG_OFFBIG_LINTFLAGS,
        _CS_V6_ENV,
        _CS_V7_ENV
    }

    enum
    {
        _PC_LINK_MAX,
        _PC_MAX_CANON,
        _PC_MAX_INPUT,
        _PC_NAME_MAX,
        _PC_PATH_MAX,
        _PC_PIPE_BUF,
        _PC_CHOWN_RESTRICTED,
        _PC_NO_TRUNC,
        _PC_VDISABLE,
        _PC_SYNC_IO,
        _PC_ASYNC_IO,
        _PC_PRIO_IO,
        _PC_SOCK_MAXBUF,
        _PC_FILESIZEBITS,
        _PC_REC_INCR_XFER_SIZE,
        _PC_REC_MAX_XFER_SIZE,
        _PC_REC_MIN_XFER_SIZE,
        _PC_REC_XFER_ALIGN,
        _PC_ALLOC_SIZE_MIN,
        _PC_SYMLINK_MAX,
        _PC_2_SYMLINKS
    }

    enum
    {
        _SC_ARG_MAX,
        _SC_CHILD_MAX,
        _SC_CLK_TCK,
        _SC_NGROUPS_MAX,
        _SC_OPEN_MAX,
        _SC_STREAM_MAX,
        _SC_TZNAME_MAX,
        _SC_JOB_CONTROL,
        _SC_SAVED_IDS,
        _SC_REALTIME_SIGNALS,
        _SC_PRIORITY_SCHEDULING,
        _SC_TIMERS,
        _SC_ASYNCHRONOUS_IO,
        _SC_PRIORITIZED_IO,
        _SC_SYNCHRONIZED_IO,
        _SC_FSYNC,
        _SC_MAPPED_FILES,
        _SC_MEMLOCK,
        _SC_MEMLOCK_RANGE,
        _SC_MEMORY_PROTECTION,
        _SC_MESSAGE_PASSING,
        _SC_SEMAPHORES,
        _SC_SHARED_MEMORY_OBJECTS,
        _SC_AIO_LISTIO_MAX,
        _SC_AIO_MAX,
        _SC_AIO_PRIO_DELTA_MAX,
        _SC_DELAYTIMER_MAX,
        _SC_MQ_OPEN_MAX,
        _SC_MQ_PRIO_MAX,
        _SC_VERSION,
        _SC_PAGE_SIZE,
        _SC_PAGESIZE = _SC_PAGE_SIZE,
        _SC_RTSIG_MAX,
        _SC_SEM_NSEMS_MAX,
        _SC_SEM_VALUE_MAX,
        _SC_SIGQUEUE_MAX,
        _SC_TIMER_MAX,
        _SC_BC_BASE_MAX,
        _SC_BC_DIM_MAX,
        _SC_BC_SCALE_MAX,
        _SC_BC_STRING_MAX,
        _SC_COLL_WEIGHTS_MAX,

        _SC_EXPR_NEST_MAX = 42,
        _SC_LINE_MAX,
        _SC_RE_DUP_MAX,

        _SC_2_VERSION = 46,
        _SC_2_C_BIND,
        _SC_2_C_DEV,
        _SC_2_FORT_DEV,
        _SC_2_FORT_RUN,
        _SC_2_SW_DEV,
        _SC_2_LOCALEDEF,

        _SC_UIO_MAXIOV = 60,
        _SC_IOV_MAX = _SC_UIO_MAXIOV,

        _SC_THREADS = 67,
        _SC_THREAD_SAFE_FUNCTIONS,
        _SC_GETGR_R_SIZE_MAX,
        _SC_GETPW_R_SIZE_MAX,
        _SC_LOGIN_NAME_MAX,
        _SC_TTY_NAME_MAX,
        _SC_THREAD_DESTRUCTOR_ITERATIONS,
        _SC_THREAD_KEYS_MAX,
        _SC_THREAD_STACK_MIN,
        _SC_THREAD_THREADS_MAX,
        _SC_THREAD_ATTR_STACKADDR,
        _SC_THREAD_ATTR_STACKSIZE,
        _SC_THREAD_PRIORITY_SCHEDULING,
        _SC_THREAD_PRIO_INHERIT,
        _SC_THREAD_PRIO_PROTECT,
        _SC_THREAD_PROCESS_SHARED,

        _SC_NPROCESSORS_CONF,
        _SC_NPROCESSORS_ONLN,
        _SC_PHYS_PAGES,
        _SC_AVPHYS_PAGES,
        _SC_ATEXIT_MAX,
        _SC_PASS_MAX,

        _SC_XOPEN_VERSION,
        _SC_XOPEN_XCU_VERSION,
        _SC_XOPEN_UNIX,
        _SC_XOPEN_CRYPT,
        _SC_XOPEN_ENH_I18N,
        _SC_XOPEN_SHM,

        _SC_2_CHAR_TERM,
        _SC_2_UPE = 97,

        _SC_XOPEN_XPG2,
        _SC_XOPEN_XPG3,
        _SC_XOPEN_XPG4,

        _SC_NZERO = 109,

        _SC_XBS5_ILP32_OFF32 = 125,
        _SC_XBS5_ILP32_OFFBIG,
        _SC_XBS5_LP64_OFF64,
        _SC_XBS5_LPBIG_OFFBIG,

        _SC_XOPEN_LEGACY,
        _SC_XOPEN_REALTIME,
        _SC_XOPEN_REALTIME_THREADS,

        _SC_ADVISORY_INFO,
        _SC_BARRIERS,
        _SC_CLOCK_SELECTION = 137,
        _SC_CPUTIME,
        _SC_THREAD_CPUTIME,
        _SC_MONOTONIC_CLOCK = 149,
        _SC_READER_WRITER_LOCKS = 153,
        _SC_SPIN_LOCKS,
        _SC_REGEXP,
        _SC_SHELL = 157,
        _SC_SPAWN = 159,
        _SC_SPORADIC_SERVER,
        _SC_THREAD_SPORADIC_SERVER,
        _SC_TIMEOUTS = 164,
        _SC_TYPED_MEMORY_OBJECTS,
        _SC_2_PBS = 168,
        _SC_2_PBS_ACCOUNTING,
        _SC_2_PBS_LOCATE,
        _SC_2_PBS_MESSAGE,
        _SC_2_PBS_TRACK,
        _SC_SYMLOOP_MAX,
        _SC_STREAMS,
        _SC_2_PBS_CHECKPOINT,

        _SC_V6_ILP32_OFF32,
        _SC_V6_ILP32_OFFBIG,
        _SC_V6_LP64_OFF64,
        _SC_V6_LPBIG_OFFBIG,

        _SC_HOST_NAME_MAX,
        _SC_TRACE,
        _SC_TRACE_EVENT_FILTER,
        _SC_TRACE_INHERIT,
        _SC_TRACE_LOG,

        _SC_IPV6 = 235,
        _SC_RAW_SOCKETS,
        _SC_V7_ILP32_OFF32,
        _SC_V7_ILP32_OFFBIG,
        _SC_V7_LP64_OFF64,
        _SC_V7_LPBIG_OFFBIG,
        _SC_SS_REPL_MAX,
        _SC_TRACE_EVENT_NAME_MAX,
        _SC_TRACE_NAME_MAX,
        _SC_TRACE_SYS_MAX,
        _SC_TRACE_USER_EVENT_MAX,
        _SC_XOPEN_STREAMS,
        _SC_THREAD_ROBUST_PRIO_INHERIT,
        _SC_THREAD_ROBUST_PRIO_PROTECT
    }
}
else version (CRuntime_UClibc)
{
    enum F_OK       = 0;
    enum R_OK       = 4;
    enum W_OK       = 2;
    enum X_OK       = 1;

    enum F_ULOCK    = 0;
    enum F_LOCK     = 1;
    enum F_TLOCK    = 2;
    enum F_TEST     = 3;

    enum
    {
        _CS_PATH,

        _CS_V6_WIDTH_RESTRICTED_ENVS,

        _CS_GNU_LIBC_VERSION,
        _CS_GNU_LIBPTHREAD_VERSION,

        _CS_LFS_CFLAGS = 1000,
        _CS_LFS_LDFLAGS,
        _CS_LFS_LIBS,
        _CS_LFS_LINTFLAGS,
        _CS_LFS64_CFLAGS,
        _CS_LFS64_LDFLAGS,
        _CS_LFS64_LIBS,
        _CS_LFS64_LINTFLAGS,

        _CS_XBS5_ILP32_OFF32_CFLAGS = 1100,
        _CS_XBS5_ILP32_OFF32_LDFLAGS,
        _CS_XBS5_ILP32_OFF32_LIBS,
        _CS_XBS5_ILP32_OFF32_LINTFLAGS,
        _CS_XBS5_ILP32_OFFBIG_CFLAGS,
        _CS_XBS5_ILP32_OFFBIG_LDFLAGS,
        _CS_XBS5_ILP32_OFFBIG_LIBS,
        _CS_XBS5_ILP32_OFFBIG_LINTFLAGS,
        _CS_XBS5_LP64_OFF64_CFLAGS,
        _CS_XBS5_LP64_OFF64_LDFLAGS,
        _CS_XBS5_LP64_OFF64_LIBS,
        _CS_XBS5_LP64_OFF64_LINTFLAGS,
        _CS_XBS5_LPBIG_OFFBIG_CFLAGS,
        _CS_XBS5_LPBIG_OFFBIG_LDFLAGS,
        _CS_XBS5_LPBIG_OFFBIG_LIBS,
        _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS,

        _CS_POSIX_V6_ILP32_OFF32_CFLAGS,
        _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,
        _CS_POSIX_V6_ILP32_OFF32_LIBS,
        _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS,
        _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,
        _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,
        _CS_POSIX_V6_ILP32_OFFBIG_LIBS,
        _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS,
        _CS_POSIX_V6_LP64_OFF64_CFLAGS,
        _CS_POSIX_V6_LP64_OFF64_LDFLAGS,
        _CS_POSIX_V6_LP64_OFF64_LIBS,
        _CS_POSIX_V6_LP64_OFF64_LINTFLAGS,
        _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,
        _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,
        _CS_POSIX_V6_LPBIG_OFFBIG_LIBS,
        _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS
    }

    enum
    {
        _PC_LINK_MAX,
        _PC_MAX_CANON,
        _PC_MAX_INPUT,
        _PC_NAME_MAX,
        _PC_PATH_MAX,
        _PC_PIPE_BUF,
        _PC_CHOWN_RESTRICTED,
        _PC_NO_TRUNC,
        _PC_VDISABLE,
        _PC_SYNC_IO,
        _PC_ASYNC_IO,
        _PC_PRIO_IO,
        _PC_SOCK_MAXBUF,
        _PC_FILESIZEBITS,
        _PC_REC_INCR_XFER_SIZE,
        _PC_REC_MAX_XFER_SIZE,
        _PC_REC_MIN_XFER_SIZE,
        _PC_REC_XFER_ALIGN,
        _PC_ALLOC_SIZE_MIN,
        _PC_SYMLINK_MAX,
        _PC_2_SYMLINKS
    }

    enum
    {
        _SC_ARG_MAX,
        _SC_CHILD_MAX,
        _SC_CLK_TCK,
        _SC_NGROUPS_MAX,
        _SC_OPEN_MAX,
        _SC_STREAM_MAX,
        _SC_TZNAME_MAX,
        _SC_JOB_CONTROL,
        _SC_SAVED_IDS,
        _SC_REALTIME_SIGNALS,
        _SC_PRIORITY_SCHEDULING,
        _SC_TIMERS,
        _SC_ASYNCHRONOUS_IO,
        _SC_PRIORITIZED_IO,
        _SC_SYNCHRONIZED_IO,
        _SC_FSYNC,
        _SC_MAPPED_FILES,
        _SC_MEMLOCK,
        _SC_MEMLOCK_RANGE,
        _SC_MEMORY_PROTECTION,
        _SC_MESSAGE_PASSING,
        _SC_SEMAPHORES,
        _SC_SHARED_MEMORY_OBJECTS,
        _SC_AIO_LISTIO_MAX,
        _SC_AIO_MAX,
        _SC_AIO_PRIO_DELTA_MAX,
        _SC_DELAYTIMER_MAX,
        _SC_MQ_OPEN_MAX,
        _SC_MQ_PRIO_MAX,
        _SC_VERSION,
        _SC_PAGESIZE,
        _SC_PAGE_SIZE = _SC_PAGESIZE,
        _SC_RTSIG_MAX,
        _SC_SEM_NSEMS_MAX,
        _SC_SEM_VALUE_MAX,
        _SC_SIGQUEUE_MAX,
        _SC_TIMER_MAX,

        _SC_BC_BASE_MAX,
        _SC_BC_DIM_MAX,
        _SC_BC_SCALE_MAX,
        _SC_BC_STRING_MAX,
        _SC_COLL_WEIGHTS_MAX,
        _SC_EQUIV_CLASS_MAX,
        _SC_EXPR_NEST_MAX,
        _SC_LINE_MAX,
        _SC_RE_DUP_MAX,
        _SC_CHARCLASS_NAME_MAX,

        _SC_2_VERSION,
        _SC_2_C_BIND,
        _SC_2_C_DEV,
        _SC_2_FORT_DEV,
        _SC_2_FORT_RUN,
        _SC_2_SW_DEV,
        _SC_2_LOCALEDEF,

        _SC_PII,
        _SC_PII_XTI,
        _SC_PII_SOCKET,
        _SC_PII_INTERNET,
        _SC_PII_OSI,
        _SC_POLL,
        _SC_SELECT,
        _SC_UIO_MAXIOV,
        _SC_IOV_MAX = _SC_UIO_MAXIOV,
        _SC_PII_INTERNET_STREAM,
        _SC_PII_INTERNET_DGRAM,
        _SC_PII_OSI_COTS,
        _SC_PII_OSI_CLTS,
        _SC_PII_OSI_M,
        _SC_T_IOV_MAX,

        _SC_THREADS,
        _SC_THREAD_SAFE_FUNCTIONS,
        _SC_GETGR_R_SIZE_MAX,
        _SC_GETPW_R_SIZE_MAX,
        _SC_LOGIN_NAME_MAX,
        _SC_TTY_NAME_MAX,
        _SC_THREAD_DESTRUCTOR_ITERATIONS,
        _SC_THREAD_KEYS_MAX,
        _SC_THREAD_STACK_MIN,
        _SC_THREAD_THREADS_MAX,
        _SC_THREAD_ATTR_STACKADDR,
        _SC_THREAD_ATTR_STACKSIZE,
        _SC_THREAD_PRIORITY_SCHEDULING,
        _SC_THREAD_PRIO_INHERIT,
        _SC_THREAD_PRIO_PROTECT,
        _SC_THREAD_PROCESS_SHARED,

        _SC_NPROCESSORS_CONF,
        _SC_NPROCESSORS_ONLN,
        _SC_PHYS_PAGES,
        _SC_AVPHYS_PAGES,
        _SC_ATEXIT_MAX,
        _SC_PASS_MAX,

        _SC_XOPEN_VERSION,
        _SC_XOPEN_XCU_VERSION,
        _SC_XOPEN_UNIX,
        _SC_XOPEN_CRYPT,
        _SC_XOPEN_ENH_I18N,
        _SC_XOPEN_SHM,

        _SC_2_CHAR_TERM,
        _SC_2_C_VERSION,
        _SC_2_UPE,

        _SC_XOPEN_XPG2,
        _SC_XOPEN_XPG3,
        _SC_XOPEN_XPG4,

        _SC_CHAR_BIT,
        _SC_CHAR_MAX,
        _SC_CHAR_MIN,
        _SC_INT_MAX,
        _SC_INT_MIN,
        _SC_LONG_BIT,
        _SC_WORD_BIT,
        _SC_MB_LEN_MAX,
        _SC_NZERO,
        _SC_SSIZE_MAX,
        _SC_SCHAR_MAX,
        _SC_SCHAR_MIN,
        _SC_SHRT_MAX,
        _SC_SHRT_MIN,
        _SC_UCHAR_MAX,
        _SC_UINT_MAX,
        _SC_ULONG_MAX,
        _SC_USHRT_MAX,

        _SC_NL_ARGMAX,
        _SC_NL_LANGMAX,
        _SC_NL_MSGMAX,
        _SC_NL_NMAX,
        _SC_NL_SETMAX,
        _SC_NL_TEXTMAX,

        _SC_XBS5_ILP32_OFF32,
        _SC_XBS5_ILP32_OFFBIG,
        _SC_XBS5_LP64_OFF64,
        _SC_XBS5_LPBIG_OFFBIG,

        _SC_XOPEN_LEGACY,
        _SC_XOPEN_REALTIME,
        _SC_XOPEN_REALTIME_THREADS,

        _SC_ADVISORY_INFO,
        _SC_BARRIERS,
        _SC_BASE,
        _SC_C_LANG_SUPPORT,
        _SC_C_LANG_SUPPORT_R,
        _SC_CLOCK_SELECTION,
        _SC_CPUTIME,
        _SC_THREAD_CPUTIME,
        _SC_DEVICE_IO,
        _SC_DEVICE_SPECIFIC,
        _SC_DEVICE_SPECIFIC_R,
        _SC_FD_MGMT,
        _SC_FIFO,
        _SC_PIPE,
        _SC_FILE_ATTRIBUTES,
        _SC_FILE_LOCKING,
        _SC_FILE_SYSTEM,
        _SC_MONOTONIC_CLOCK,
        _SC_MULTI_PROCESS,
        _SC_SINGLE_PROCESS,
        _SC_NETWORKING,
        _SC_READER_WRITER_LOCKS,
        _SC_SPIN_LOCKS,
        _SC_REGEXP,
        _SC_REGEX_VERSION,
        _SC_SHELL,
        _SC_SIGNALS,
        _SC_SPAWN,
        _SC_SPORADIC_SERVER,
        _SC_THREAD_SPORADIC_SERVER,
        _SC_SYSTEM_DATABASE,
        _SC_SYSTEM_DATABASE_R,
        _SC_TIMEOUTS,
        _SC_TYPED_MEMORY_OBJECTS,
        _SC_USER_GROUPS,
        _SC_USER_GROUPS_R,
        _SC_2_PBS,
        _SC_2_PBS_ACCOUNTING,
        _SC_2_PBS_LOCATE,
        _SC_2_PBS_MESSAGE,
        _SC_2_PBS_TRACK,
        _SC_SYMLOOP_MAX,
        _SC_STREAMS,
        _SC_2_PBS_CHECKPOINT,

        _SC_V6_ILP32_OFF32,
        _SC_V6_ILP32_OFFBIG,
        _SC_V6_LP64_OFF64,
        _SC_V6_LPBIG_OFFBIG,

        _SC_HOST_NAME_MAX,
        _SC_TRACE,
        _SC_TRACE_EVENT_FILTER,
        _SC_TRACE_INHERIT,
        _SC_TRACE_LOG,

        _SC_LEVEL1_ICACHE_SIZE,
        _SC_LEVEL1_ICACHE_ASSOC,
        _SC_LEVEL1_ICACHE_LINESIZE,
        _SC_LEVEL1_DCACHE_SIZE,
        _SC_LEVEL1_DCACHE_ASSOC,
        _SC_LEVEL1_DCACHE_LINESIZE,
        _SC_LEVEL2_CACHE_SIZE,
        _SC_LEVEL2_CACHE_ASSOC,
        _SC_LEVEL2_CACHE_LINESIZE,
        _SC_LEVEL3_CACHE_SIZE,
        _SC_LEVEL3_CACHE_ASSOC,
        _SC_LEVEL3_CACHE_LINESIZE,
        _SC_LEVEL4_CACHE_SIZE,
        _SC_LEVEL4_CACHE_ASSOC,
        _SC_LEVEL4_CACHE_LINESIZE,

        _SC_IPV6 = _SC_LEVEL1_ICACHE_SIZE + 50,
        _SC_RAW_SOCKETS
    }
}

//
// File Synchronization (FSC)
//
/*
int fsync(int);
*/

version (CRuntime_Glibc)
{
    int fsync(int) @trusted;
}
else version (Darwin)
{
    int fsync(int) @trusted;
}
else version (FreeBSD)
{
    int fsync(int) @trusted;
}
else version (NetBSD)
{
    int fsync(int) @trusted;
}
else version (OpenBSD)
{
    int fsync(int) @trusted;
}
else version (DragonFlyBSD)
{
    int fsync(int) @trusted;
}
else version (CRuntime_Bionic)
{
    int fsync(int) @trusted;
}
else version (CRuntime_Musl)
{
    int fsync(int) @trusted;
}
else version (Solaris)
{
    int fsync(int) @trusted;
}
else version (CRuntime_UClibc)
{
    int fsync(int) @trusted;
}

//
// Synchronized I/O (SIO)
//
/*
int fdatasync(int);
*/

version (CRuntime_Glibc)
{
    int fdatasync(int) @trusted;
}
else version (Solaris)
{
    int fdatasync(int) @trusted;
}
else version (CRuntime_Bionic)
{
    int fdatasync(int) @trusted;
}
else version (CRuntime_UClibc)
{
    int fdatasync(int) @trusted;
}

//
// XOpen (XSI)
//
/*
char*      crypt(in char*, in char*);
char*      ctermid(char*);
void       encrypt(ref char[64], int);
int        fchdir(int);
c_long     gethostid();
pid_t      getpgid(pid_t);
pid_t      getsid(pid_t);
char*      getwd(char*); // LEGACY
int        lchown(in char*, uid_t, gid_t);
int        lockf(int, int, off_t);
int        nice(int);
ssize_t    pread(int, void*, size_t, off_t);
ssize_t    pwrite(int, in void*, size_t, off_t);
pid_t      setpgrp();
int        setregid(gid_t, gid_t);
int        setreuid(uid_t, uid_t);
void       swab(in void*, void*, ssize_t);
void       sync();
int        truncate(in char*, off_t);
useconds_t ualarm(useconds_t, useconds_t);
int        usleep(useconds_t);
pid_t      vfork();
*/

version (CRuntime_Glibc)
{
    char*      crypt(in char*, in char*);
    char*      ctermid(char*);
    void       encrypt(ref char[64], int) @trusted;
    int        fchdir(int) @trusted;
    c_long     gethostid() @trusted;
    pid_t      getpgid(pid_t) @trusted;
    pid_t      getsid(pid_t) @trusted;
    char*      getwd(char*); // LEGACY
    int        lchown(in char*, uid_t, gid_t);
    //int        lockf(int, int, off_t);
    int        nice(int) @trusted;
    //ssize_t    pread(int, void*, size_t, off_t);
    //ssize_t    pwrite(int, in void*, size_t, off_t);
    pid_t      setpgrp() @trusted;
    int        setregid(gid_t, gid_t) @trusted;
    int        setreuid(uid_t, uid_t) @trusted;
    void       swab(in void*, void*, ssize_t);
    void       sync() @trusted;
    //int        truncate(in char*, off_t);
    useconds_t ualarm(useconds_t, useconds_t) @trusted;
    int        usleep(useconds_t) @trusted;
    pid_t      vfork();

  static if ( __USE_FILE_OFFSET64 )
  {
    int        lockf64(int, int, off_t) @trusted;
    alias      lockf64 lockf;

    ssize_t    pread64(int, void*, size_t, off_t);
    alias      pread64 pread;

    ssize_t    pwrite64(int, in void*, size_t, off_t);
    alias      pwrite64 pwrite;

    int        truncate64(in char*, off_t);
    alias      truncate64 truncate;
  }
  else
  {
    int        lockf(int, int, off_t) @trusted;
    ssize_t    pread(int, void*, size_t, off_t);
    ssize_t    pwrite(int, in void*, size_t, off_t);
    int        truncate(in char*, off_t);
  }
}
else version (CRuntime_Musl)
{
    int fchdir(int) @trusted;
    int lockf(int, int, off_t);
    alias lockf lockf64;
}
else version (Darwin)
{
    char*      crypt(in char*, in char*);
    char*      ctermid(char*);
    void       encrypt(ref char[64], int) @trusted;
    int        fchdir(int) @trusted;
    c_long     gethostid() @trusted;
    pid_t      getpgid(pid_t) @trusted;
    pid_t      getsid(pid_t) @trusted;
    char*      getwd(char*); // LEGACY
    int        lchown(in char*, uid_t, gid_t);
    int        lockf(int, int, off_t) @trusted;
    int        nice(int) @trusted;
    ssize_t    pread(int, void*, size_t, off_t);
    ssize_t    pwrite(int, in void*, size_t, off_t);
    pid_t      setpgrp() @trusted;
    int        setregid(gid_t, gid_t) @trusted;
    int        setreuid(uid_t, uid_t) @trusted;
    void       swab(in void*, void*, ssize_t);
    void       sync() @trusted;
    int        truncate(in char*, off_t);
    useconds_t ualarm(useconds_t, useconds_t) @trusted;
    int        usleep(useconds_t) @trusted;
    pid_t      vfork();
}
else version (FreeBSD)
{
    char*      crypt(in char*, in char*);
    //char*      ctermid(char*);
    void       encrypt(ref char[64], int) @trusted;
    int        fchdir(int) @trusted;
    c_long     gethostid() @trusted;
    int        getpgid(pid_t) @trusted;
    int        getsid(pid_t) @trusted;
    char*      getwd(char*); // LEGACY
    int        lchown(in char*, uid_t, gid_t);
    int        lockf(int, int, off_t) @trusted;
    int        nice(int) @trusted;
    ssize_t    pread(int, void*, size_t, off_t);
    ssize_t    pwrite(int, in void*, size_t, off_t);
    int        setpgrp(pid_t, pid_t) @trusted;
    int        setregid(gid_t, gid_t) @trusted;
    int        setreuid(uid_t, uid_t) @trusted;
    void       swab(in void*, void*, ssize_t);
    void       sync() @trusted;
    int        truncate(in char*, off_t);
    useconds_t ualarm(useconds_t, useconds_t) @trusted;
    int        usleep(useconds_t) @trusted;
    pid_t      vfork();
}
else version (NetBSD)
{
    char*      crypt(in char*, in char*);
    //char*      ctermid(char*);
    void       encrypt(ref char[64], int) @trusted;
    int        fchdir(int) @trusted;
    c_long     gethostid() @trusted;
    int        getpgid(pid_t) @trusted;
    int        getsid(pid_t) @trusted;
    char*      getwd(char*); // LEGACY
    int        lchown(in char*, uid_t, gid_t);
    int        lockf(int, int, off_t) @trusted;
    int        nice(int) @trusted;
    ssize_t    pread(int, void*, size_t, off_t);
    ssize_t    pwrite(int, in void*, size_t, off_t);
    int        setpgrp(pid_t, pid_t) @trusted;
    int        setregid(gid_t, gid_t) @trusted;
    int        setreuid(uid_t, uid_t) @trusted;
    void       swab(in void*, void*, ssize_t);
    void       sync() @trusted;
    int        truncate(in char*, off_t);
    useconds_t ualarm(useconds_t, useconds_t) @trusted;
    int        usleep(useconds_t) @trusted;
    pid_t      vfork();
}
else version (OpenBSD)
{
    char*      crypt(in char*, in char*);
    //char*      ctermid(char*);
    //void       encrypt(ref char[64], int) @trusted;
    int        fchdir(int) @trusted;
    c_long     gethostid() @trusted;
    pid_t      getpgid(pid_t) @trusted;
    pid_t      getsid(pid_t) @trusted;
    char*      getwd(char*);
    int        lchown(in char*, uid_t, gid_t);
    int        lockf(int, int, off_t) @trusted;
    int        nice(int) @trusted;
    ssize_t    pread(int, void*, size_t, off_t);
    ssize_t    pwrite(int, in void*, size_t, off_t);
    int        setpgrp(pid_t, pid_t) @trusted;
    int        setregid(gid_t, gid_t) @trusted;
    int        setreuid(uid_t, uid_t) @trusted;
    void       swab(in void*, void*, ssize_t);
    void       sync() @trusted;
    int        truncate(in char*, off_t);
    useconds_t ualarm(useconds_t, useconds_t) @trusted;
    int        usleep(useconds_t) @trusted;
    pid_t      vfork();
}
else version (DragonFlyBSD)
{
    char*      crypt(in char*, in char*);
    //char*      ctermid(char*);
    void       encrypt(ref char[64], int) @trusted;
    int        fchdir(int) @trusted;
    c_long     gethostid() @trusted;
    int        getpgid(pid_t) @trusted;
    int        getsid(pid_t) @trusted;
    char*      getwd(char*); // LEGACY
    int        lchown(in char*, uid_t, gid_t);
    int        lockf(int, int, off_t) @trusted;
    int        nice(int) @trusted;
    ssize_t    pread(int, void*, size_t, off_t);
    ssize_t    pwrite(int, in void*, size_t, off_t);
    int        setpgrp(pid_t, pid_t) @trusted;
    int        setregid(gid_t, gid_t) @trusted;
    int        setreuid(uid_t, uid_t) @trusted;
    void       swab(in void*, void*, ssize_t);
    void       sync() @trusted;
    int        truncate(in char*, off_t);
    useconds_t ualarm(useconds_t, useconds_t) @trusted;
    int        usleep(useconds_t) @trusted;
    pid_t      vfork();
}
else version (CRuntime_Bionic)
{
    int        fchdir(int) @trusted;
    pid_t      getpgid(pid_t) @trusted;
    int        lchown(in char*, uid_t, gid_t);
    int        nice(int) @trusted;
    ssize_t    pread(int, void*, size_t, off_t);
    ssize_t    pwrite(int, in void*, size_t, off_t);
    int        setpgrp() @trusted;
    int        setregid(gid_t, gid_t) @trusted;
    int        setreuid(uid_t, uid_t) @trusted;
    int        sync() @trusted;
    int        truncate(in char*, off_t);
    int        usleep(c_ulong) @trusted;
    pid_t      vfork();
}
else version (Solaris)
{
    char*      crypt(in char*, in char*);
    char*      ctermid(char*);
    void       encrypt(ref char[64], int);
    int        fchdir(int);
    c_long     gethostid();
    pid_t      getpgid(pid_t);
    pid_t      getsid(pid_t);
    char*      getwd(char*); // LEGACY
    int        lchown(in char*, uid_t, gid_t);
    int        nice(int);
    pid_t      setpgrp();
    int        setregid(gid_t, gid_t);
    int        setreuid(uid_t, uid_t);
    void       swab(in void*, void*, ssize_t);
    void       sync();
    useconds_t ualarm(useconds_t, useconds_t);
    int        usleep(useconds_t);
    pid_t      vfork();

    version (D_LP64)
    {
        int         lockf(int, int, off_t);
        alias       lockf lockf64;

        ssize_t     pread(int, void*, size_t, off_t);
        alias       pread pread64;

        ssize_t     pwrite(int, in void*, size_t, off_t);
        alias       pwrite pwrite64;

        int         truncate(in char*, off_t);
        alias       truncate truncate64;
    }
    else
    {
        static if ( __USE_FILE_OFFSET64 )
        {
            int        lockf64(int, int, off64_t);
            alias      lockf64 lockf;

            ssize_t    pread64(int, void*, size_t, off64_t);
            alias      pread64 pread;

            ssize_t    pwrite64(int, in void*, size_t, off_t);
            alias      pwrite64 pwrite;

            int        truncate64(in char*, off_t);
            alias      truncate64 truncate;
        }
        else
        {
            int        lockf(int, int, off_t);
            ssize_t    pread(int, void*, size_t, off_t);
            ssize_t    pwrite(int, in void*, size_t, off_t);
            int        truncate(in char*, off_t);
        }
    }
}
else version (CRuntime_UClibc)
{
    char*      crypt(in char*, in char*);
    char*      ctermid(char*);
    void       encrypt(ref char[64], int) @trusted;
    int        fchdir(int) @trusted;
    c_long     gethostid() @trusted;
    pid_t      getpgid(pid_t) @trusted;
    pid_t      getsid(pid_t) @trusted;
    char*      getwd(char*); // LEGACY
    int        lchown(in char*, uid_t, gid_t);
    int        nice(int) @trusted;
    pid_t      setpgrp() @trusted;
    int        setregid(gid_t, gid_t) @trusted;
    int        setreuid(uid_t, uid_t) @trusted;
    void       swab(in void*, void*, ssize_t);
    void       sync() @trusted;
    useconds_t ualarm(useconds_t, useconds_t) @trusted;
    int        usleep(useconds_t) @trusted;
    pid_t      vfork();

  static if ( __USE_FILE_OFFSET64 )
  {
    int        lockf64(int, int, off_t) @trusted;
    alias      lockf64 lockf;

    ssize_t    pread64(int, void*, size_t, off_t);
    alias      pread64 pread;

    ssize_t    pwrite64(int, in void*, size_t, off_t);
    alias      pwrite64 pwrite;

    int        truncate64(in char*, off_t);
    alias      truncate64 truncate;
  }
  else
  {
    int        lockf(int, int, off_t) @trusted;
    ssize_t    pread(int, void*, size_t, off_t);
    ssize_t    pwrite(int, in void*, size_t, off_t);
    int        truncate(in char*, off_t);
  }
}
