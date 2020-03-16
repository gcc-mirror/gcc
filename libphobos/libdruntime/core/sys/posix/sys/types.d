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
module core.sys.posix.sys.types;

private import core.sys.posix.config;
private import core.stdc.stdint;
public import core.stdc.stddef;

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

//
// bits/typesizes.h -- underlying types for *_t.
//
/*
__syscall_slong_t
__syscall_ulong_t
*/
version (CRuntime_Glibc)
{
    version (X86_64)
    {
        version (D_X32)
        {
            // X32 kernel interface is 64-bit.
            alias long slong_t;
            alias ulong ulong_t;
        }
        else
        {
            alias c_long slong_t;
            alias c_ulong ulong_t;
        }
    }
    else
    {
        alias c_long slong_t;
        alias c_ulong ulong_t;
    }
}
else
{
    alias c_long slong_t;
    alias c_ulong ulong_t;
}

//
// Required
//
/*
blkcnt_t
blksize_t
dev_t
gid_t
ino_t
mode_t
nlink_t
off_t
pid_t
size_t
ssize_t
time_t
uid_t
*/

version (CRuntime_Glibc)
{
  static if ( __USE_FILE_OFFSET64 )
  {
    alias long      blkcnt_t;
    alias ulong     ino_t;
    alias long      off_t;
  }
  else
  {
    alias slong_t   blkcnt_t;
    alias ulong_t   ino_t;
    alias slong_t   off_t;
  }
    alias slong_t   blksize_t;
    alias ulong     dev_t;
    alias uint      gid_t;
    alias uint      mode_t;
    alias ulong_t   nlink_t;
    alias int       pid_t;
    //size_t (defined in core.stdc.stddef)
    alias c_long    ssize_t;
    alias slong_t   time_t;
    alias uint      uid_t;
}
else version (CRuntime_Musl)
{
    version (AArch64)
    {
        alias int    blksize_t;
        alias uint   nlink_t;
    }
    else version (MIPS64)
    {
        alias c_long blksize_t;
        alias uint   nlink_t;
    }
    else version (RISCV64)
    {
        alias int    blksize_t;
        alias uint   nlink_t;
    }
    else
    {
        alias c_long blksize_t;
        alias c_ulong nlink_t;
    }
    alias long       dev_t;
    alias long       blkcnt_t;
    alias ulong      ino_t;
    alias long       off_t;
    alias int        pid_t;
    alias uint       uid_t;
    alias uint       gid_t;
    version (D_X32)
        alias long   time_t;
    else
        alias c_long time_t;
    alias c_long     clock_t;
    alias c_ulong    pthread_t;
    version (D_LP64)
        alias c_long ssize_t;
    else
        alias int    ssize_t;
}
else version (Darwin)
{
    alias long      blkcnt_t;
    alias int       blksize_t;
    alias int       dev_t;
    alias uint      gid_t;
    alias ulong     ino_t;
    alias ushort    mode_t;
    alias ushort    nlink_t;
    alias long      off_t;
    alias int       pid_t;
    //size_t (defined in core.stdc.stddef)
    alias c_long    ssize_t;
    alias c_long    time_t;
    alias uint      uid_t;
}
else version (FreeBSD)
{
    // https://github.com/freebsd/freebsd/blob/master/sys/sys/_types.h
    alias long      blkcnt_t;
    alias uint      blksize_t;
    alias uint      dev_t;
    alias uint      gid_t;
    alias uint      ino_t;
    alias ushort    mode_t;
    alias ushort    nlink_t;
    alias long      off_t;
    alias int       pid_t;
    //size_t (defined in core.stdc.stddef)
    alias c_long    ssize_t;
    alias c_long    time_t;
    alias uint      uid_t;
    alias uint      fflags_t;
}
else version (NetBSD)
{
    alias long      blkcnt_t;
    alias int       blksize_t;
    alias ulong     dev_t;
    alias uint      gid_t;
    alias ulong     ino_t;
    alias uint      mode_t;
    alias uint      nlink_t;
    alias ulong     off_t;
    alias int       pid_t;
    //size_t (defined in core.stdc.stddef)
    alias c_long      ssize_t;
    alias c_long      time_t;
    alias uint        uid_t;
}
else version (OpenBSD)
{
    alias char*     caddr_t;
    alias long      blkcnt_t;
    alias int       blksize_t;
    alias int       dev_t;
    alias uint      gid_t;
    alias ulong     ino_t;
    alias uint      mode_t;
    alias uint      nlink_t;
    alias long      off_t;
    alias int       pid_t;
    //size_t (defined in core.stdc.stddef)
    alias c_long    ssize_t;
    alias long      time_t;
    alias uint      uid_t;
}
else version (DragonFlyBSD)
{
    alias long      blkcnt_t;
    alias long      blksize_t;
    alias uint      dev_t;
    alias uint      gid_t;
    alias long      ino_t;
    alias ushort    mode_t;
    alias uint      nlink_t;
    alias long      off_t;      //__off_t (defined in /usr/include/sys/stdint.h -> core.stdc.stddef)
    alias int       pid_t;      // size_t (defined in /usr/include/sys/stdint.h -> core.stdc.stddef)
    alias c_long    ssize_t;
    alias long      time_t;
    alias uint      uid_t;
}
else version (Solaris)
{
    alias char* caddr_t;
    alias c_long daddr_t;
    alias short cnt_t;

    static if (__USE_FILE_OFFSET64)
    {
        alias long blkcnt_t;
        alias ulong ino_t;
        alias long off_t;
    }
    else
    {
        alias c_long blkcnt_t;
        alias c_ulong ino_t;
        alias c_long off_t;
    }

    version (D_LP64)
    {
        alias blkcnt_t blkcnt64_t;
        alias ino_t ino64_t;
        alias off_t off64_t;
    }
    else
    {
        alias long blkcnt64_t;
        alias ulong ino64_t;
        alias long off64_t;
    }

    alias uint blksize_t;
    alias c_ulong dev_t;
    alias uid_t gid_t;
    alias uint mode_t;
    alias uint nlink_t;
    alias int pid_t;
    alias c_long ssize_t;
    alias c_long time_t;
    alias uint uid_t;
}
else version (CRuntime_Bionic)
{
    alias c_ulong   blkcnt_t;
    alias c_ulong   blksize_t;
    alias size_t    dev_t;
    alias uint      gid_t;
    alias c_ulong   ino_t;
    alias c_long    off_t;
    alias int       pid_t;
    alias c_long    ssize_t;
    alias c_long    time_t;
    alias uint      uid_t;

    version (D_LP64)
    {
        alias uint      mode_t;
        alias uint      nlink_t;
    }
    else
    {
        alias ushort    mode_t;
        alias ushort    nlink_t;
    }
}
else version (CRuntime_UClibc)
{
    static if ( __USE_FILE_OFFSET64 )
    {
        alias long      blkcnt_t;
        alias ulong     ino_t;
        alias long      off_t;
    }
    else
    {
        alias slong_t   blkcnt_t;
        alias ulong_t   ino_t;
        alias slong_t   off_t;
    }

    version (D_LP64)
    {
        alias ino_t ino64_t;
        alias off_t off64_t;
    }
    else
    {
        alias ulong ino64_t;
        alias long off64_t;
    }

    alias slong_t   blksize_t;
    alias c_ulong   dev_t;
    alias uint      gid_t;
    alias uint      mode_t;
    alias uint      nlink_t;
    alias int       pid_t;
    //size_t (defined in core.stdc.stddef)
    alias c_long    ssize_t;
    alias slong_t   time_t;
    alias uint      uid_t;
}
else
{
    static assert(false, "Unsupported platform");
}

//
// XOpen (XSI)
//
/*
clock_t
fsblkcnt_t
fsfilcnt_t
id_t
key_t
suseconds_t
useconds_t
*/

version (CRuntime_Glibc)
{
  static if ( __USE_FILE_OFFSET64 )
  {
    alias ulong     fsblkcnt_t;
    alias ulong     fsfilcnt_t;
  }
  else
  {
    alias ulong_t   fsblkcnt_t;
    alias ulong_t   fsfilcnt_t;
  }
    alias slong_t   clock_t;
    alias uint      id_t;
    alias int       key_t;
    alias slong_t   suseconds_t;
    alias uint      useconds_t;
}
else version (Darwin)
{
    alias uint   fsblkcnt_t;
    alias uint   fsfilcnt_t;
    alias c_long clock_t;
    alias uint   id_t;
    // key_t
    alias int    suseconds_t;
    alias uint   useconds_t;
}
else version (FreeBSD)
{
    alias ulong     fsblkcnt_t;
    alias ulong     fsfilcnt_t;
    alias c_long    clock_t;
    alias long      id_t;
    alias c_long    key_t;
    alias c_long    suseconds_t;
    alias uint      useconds_t;
}
else version (NetBSD)
{
    alias ulong     fsblkcnt_t;
    alias ulong     fsfilcnt_t;
    alias c_long    clock_t;
    alias long      id_t;
    alias c_long    key_t;
    alias c_long    suseconds_t;
    alias uint      useconds_t;
}
else version (OpenBSD)
{
    alias ulong     fsblkcnt_t;
    alias ulong     fsfilcnt_t;
    alias long      clock_t;
    alias uint      id_t;
    alias c_long    key_t;
    alias c_long    suseconds_t;
    alias uint      useconds_t;
}
else version (DragonFlyBSD)
{
    alias ulong     fsblkcnt_t;
    alias ulong     fsfilcnt_t;
    alias c_long    clock_t;
    alias long      id_t;
    alias c_long    key_t;
    alias c_long    suseconds_t;
    alias uint      useconds_t;
}
else version (Solaris)
{
    static if (__USE_FILE_OFFSET64)
    {
        alias ulong fsblkcnt_t;
        alias ulong fsfilcnt_t;
    }
    else
    {
        alias c_ulong fsblkcnt_t;
        alias c_ulong fsfilcnt_t;
    }

    alias c_long clock_t;
    alias int id_t;
    alias int key_t;
    alias c_long suseconds_t;
    alias uint useconds_t;

    alias id_t taskid_t;
    alias id_t projid_t;
    alias id_t poolid_t;
    alias id_t zoneid_t;
    alias id_t ctid_t;
}
else version (CRuntime_Bionic)
{
    alias c_ulong  fsblkcnt_t;
    alias c_ulong  fsfilcnt_t;
    alias c_long   clock_t;
    alias uint     id_t;
    alias int      key_t;
    alias c_long   suseconds_t;
    alias uint     useconds_t; // Updated in Lollipop
}
else version (CRuntime_Musl)
{
  static if ( __USE_FILE_OFFSET64 )
  {
    alias ulong     fsblkcnt_t;
    alias ulong     fsfilcnt_t;
  }
  else
  {
    alias ulong_t   fsblkcnt_t;
    alias ulong_t   fsfilcnt_t;
  }
    alias uint mode_t;
    alias uint id_t;
    version (D_X32)
        alias long susseconds_t;
    else
        alias c_long suseconds_t;
}
else version (CRuntime_UClibc)
{
  static if ( __USE_FILE_OFFSET64 )
  {
    alias ulong     fsblkcnt_t;
    alias ulong     fsfilcnt_t;
  }
  else
  {
    alias ulong_t   fsblkcnt_t;
    alias ulong_t   fsfilcnt_t;
  }
    alias slong_t   clock_t;
    alias uint      id_t;
    alias int       key_t;
    alias slong_t   suseconds_t;
    alias uint      useconds_t;
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Thread (THR)
//
/*
pthread_attr_t
pthread_cond_t
pthread_condattr_t
pthread_key_t
pthread_mutex_t
pthread_mutexattr_t
pthread_once_t
pthread_rwlock_t
pthread_rwlockattr_t
pthread_t
*/

version (CRuntime_Glibc)
{
    version (X86)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 36;
        enum __SIZEOF_PTHREAD_MUTEX_T = 24;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 32;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 20;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (X86_64)
    {
        static if (__WORDSIZE == 64)
        {
            enum __SIZEOF_PTHREAD_ATTR_T = 56;
            enum __SIZEOF_PTHREAD_MUTEX_T = 40;
            enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
            enum __SIZEOF_PTHREAD_COND_T = 48;
            enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
            enum __SIZEOF_PTHREAD_RWLOCK_T = 56;
            enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
            enum __SIZEOF_PTHREAD_BARRIER_T = 32;
            enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
        }
        else
        {
            enum __SIZEOF_PTHREAD_ATTR_T = 32;
            enum __SIZEOF_PTHREAD_MUTEX_T = 32;
            enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
            enum __SIZEOF_PTHREAD_COND_T = 48;
            enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
            enum __SIZEOF_PTHREAD_RWLOCK_T = 44;
            enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
            enum __SIZEOF_PTHREAD_BARRIER_T = 20;
            enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
        }
    }
    else version (AArch64)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 64;
        enum __SIZEOF_PTHREAD_MUTEX_T = 48;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 8;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 8;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 56;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 32;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 8;
    }
    else version (ARM)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 36;
        enum __SIZEOF_PTHREAD_MUTEX_T = 24;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 32;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 20;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (HPPA)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 36;
        enum __SIZEOF_PTHREAD_MUTEX_T = 48;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 64;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 48;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (IA64)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 56;
        enum __SIZEOF_PTHREAD_MUTEX_T = 40;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 56;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 32;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (MIPS32)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 36;
        enum __SIZEOF_PTHREAD_MUTEX_T = 24;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 32;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 20;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (MIPS64)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 56;
        enum __SIZEOF_PTHREAD_MUTEX_T = 40;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 56;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 32;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (PPC)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 36;
        enum __SIZEOF_PTHREAD_MUTEX_T = 24;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 32;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 20;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (PPC64)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 56;
        enum __SIZEOF_PTHREAD_MUTEX_T = 40;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 56;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 32;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (RISCV32)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 36;
        enum __SIZEOF_PTHREAD_MUTEX_T = 24;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 32;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 20;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (RISCV64)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 56;
        enum __SIZEOF_PTHREAD_MUTEX_T = 40;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 56;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 32;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (SPARC64)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 56;
        enum __SIZEOF_PTHREAD_MUTEX_T = 40;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 56;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 32;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (S390)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 36;
        enum __SIZEOF_PTHREAD_MUTEX_T = 24;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 32;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 20;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else version (SystemZ)
    {
        enum __SIZEOF_PTHREAD_ATTR_T = 56;
        enum __SIZEOF_PTHREAD_MUTEX_T = 40;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 56;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 32;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
    }
    else
    {
        static assert (false, "Unsupported platform");
    }

    union pthread_attr_t
    {
        byte[__SIZEOF_PTHREAD_ATTR_T] __size;
        c_long __align;
    }

    private alias int __atomic_lock_t;

    private struct _pthread_fastlock
    {
        c_long          __status;
        __atomic_lock_t __spinlock;
    }

    private alias void* _pthread_descr;

    union pthread_cond_t
    {
        byte[__SIZEOF_PTHREAD_COND_T] __size;
        long  __align;
    }

    union pthread_condattr_t
    {
        byte[__SIZEOF_PTHREAD_CONDATTR_T] __size;
        int __align;
    }

    alias uint pthread_key_t;

    union pthread_mutex_t
    {
        byte[__SIZEOF_PTHREAD_MUTEX_T] __size;
        c_long __align;
    }

    union pthread_mutexattr_t
    {
        byte[__SIZEOF_PTHREAD_MUTEXATTR_T] __size;
        int __align;
    }

    alias int pthread_once_t;

    struct pthread_rwlock_t
    {
        byte[__SIZEOF_PTHREAD_RWLOCK_T] __size;
        c_long __align;
    }

    struct pthread_rwlockattr_t
    {
        byte[__SIZEOF_PTHREAD_RWLOCKATTR_T] __size;
        c_long __align;
    }

    alias c_ulong pthread_t;
}
else version (CRuntime_Musl)
{
    version (D_LP64)
    {
        union pthread_attr_t
        {
            int[14] __i;
            ulong[7] __s;
        }

        union pthread_cond_t
        {
            int[12] __i;
            void*[6] __p;
        }

        union pthread_mutex_t
        {
            int[10] __i;
            void*[5] __p;
        }

        union pthread_rwlock_t
        {
            int[14] __i;
            void*[7] __p;
        }
    }
    else
    {
        union pthread_attr_t
        {
            int[9] __i;
            uint[9] __s;
        }

        union pthread_cond_t
        {
            int[12] __i;
            void*[12] __p;
        }

        union pthread_mutex_t
        {
            int[6] __i;
            void*[6] __p;
        }

        union pthread_rwlock_t
        {
            int[8] __i;
            void*[8] __p;
        }
    }

    struct pthread_rwlockattr_t
    {
        uint[2] __attr;
    }

    alias uint pthread_key_t;

    struct pthread_condattr_t
    {
        uint __attr;
    }

    struct pthread_mutexattr_t
    {
        uint __attr;
    }

    alias int pthread_once_t;
}
else version (Darwin)
{
    version (D_LP64)
    {
        enum __PTHREAD_SIZE__               = 1168;
        enum __PTHREAD_ATTR_SIZE__          = 56;
        enum __PTHREAD_MUTEXATTR_SIZE__     = 8;
        enum __PTHREAD_MUTEX_SIZE__         = 56;
        enum __PTHREAD_CONDATTR_SIZE__      = 8;
        enum __PTHREAD_COND_SIZE__          = 40;
        enum __PTHREAD_ONCE_SIZE__          = 8;
        enum __PTHREAD_RWLOCK_SIZE__        = 192;
        enum __PTHREAD_RWLOCKATTR_SIZE__    = 16;
    }
    else
    {
        enum __PTHREAD_SIZE__               = 596;
        enum __PTHREAD_ATTR_SIZE__          = 36;
        enum __PTHREAD_MUTEXATTR_SIZE__     = 8;
        enum __PTHREAD_MUTEX_SIZE__         = 40;
        enum __PTHREAD_CONDATTR_SIZE__      = 4;
        enum __PTHREAD_COND_SIZE__          = 24;
        enum __PTHREAD_ONCE_SIZE__          = 4;
        enum __PTHREAD_RWLOCK_SIZE__        = 124;
        enum __PTHREAD_RWLOCKATTR_SIZE__    = 12;
    }

    struct pthread_handler_rec
    {
      void function(void*)  __routine;
      void*                 __arg;
      pthread_handler_rec*  __next;
    }

    struct pthread_attr_t
    {
        c_long                              __sig;
        byte[__PTHREAD_ATTR_SIZE__]         __opaque;
    }

    struct pthread_cond_t
    {
        c_long                              __sig;
        byte[__PTHREAD_COND_SIZE__]         __opaque;
    }

    struct pthread_condattr_t
    {
        c_long                              __sig;
        byte[__PTHREAD_CONDATTR_SIZE__]     __opaque;
    }

    alias c_ulong pthread_key_t;

    struct pthread_mutex_t
    {
        c_long                              __sig;
        byte[__PTHREAD_MUTEX_SIZE__]        __opaque;
    }

    struct pthread_mutexattr_t
    {
        c_long                              __sig;
        byte[__PTHREAD_MUTEXATTR_SIZE__]    __opaque;
    }

    struct pthread_once_t
    {
        c_long                              __sig;
        byte[__PTHREAD_ONCE_SIZE__]         __opaque;
    }

    struct pthread_rwlock_t
    {
        c_long                              __sig;
        byte[__PTHREAD_RWLOCK_SIZE__]       __opaque;
    }

    struct pthread_rwlockattr_t
    {
        c_long                              __sig;
        byte[__PTHREAD_RWLOCKATTR_SIZE__]   __opaque;
    }

    private struct _opaque_pthread_t
    {
        c_long                  __sig;
        pthread_handler_rec*    __cleanup_stack;
        byte[__PTHREAD_SIZE__]  __opaque;
    }

    alias _opaque_pthread_t* pthread_t;
}
else version (FreeBSD)
{
    alias int lwpid_t;

    alias void* pthread_attr_t;
    alias void* pthread_cond_t;
    alias void* pthread_condattr_t;
    alias void* pthread_key_t;
    alias void* pthread_mutex_t;
    alias void* pthread_mutexattr_t;
    alias void* pthread_once_t;
    alias void* pthread_rwlock_t;
    alias void* pthread_rwlockattr_t;
    alias void* pthread_t;
}
else version (NetBSD)
{
   struct pthread_queue_t {
         void*  ptqh_first;
         void** ptqh_last;
   }

    alias lwpid_t = int;
    alias pthread_spin_t = ubyte;
    struct pthread_attr_t {
        uint    pta_magic;
        int     pta_flags;
        void*   pta_private;
    }
    struct  pthread_spinlock_t {
        uint    pts_magic;
        pthread_spin_t  pts_spin;
        int             pts_flags;
    }
    struct pthread_cond_t {
        uint    ptc_magic;
        pthread_spin_t  ptc_lock;
        pthread_queue_t ptc_waiters;
        pthread_mutex_t *ptc_mutex;
        void*   ptc_private;
    }
    struct pthread_condattr_t {
        uint    ptca_magic;
        void    *ptca_private;
    }
    struct pthread_mutex_t {
        uint ptm_magic;
        pthread_spin_t  ptm_errorcheck;
        ubyte[3]         ptm_pad1;
        pthread_spin_t  ptm_interlock;
        ubyte[3] ptm_pad2;
        pthread_t ptm_owner;
        void* ptm_waiters;
        uint  ptm_recursed;
        void* ptm_spare2;
    }
    struct pthread_mutexattr_t{
        uint    ptma_magic;
        void*   ptma_private;
    }
    struct pthread_once_t{
        pthread_mutex_t pto_mutex;
        int     pto_done;
    }
    struct pthread_rwlock_t{
        uint    ptr_magic;

        pthread_spin_t  ptr_interlock;

        pthread_queue_t ptr_rblocked;
        pthread_queue_t ptr_wblocked;
        uint    ptr_nreaders;
        pthread_t ptr_owner;
        void    *ptr_private;
    }
    struct pthread_rwlockattr_t{
        uint    ptra_magic;
        void*   ptra_private;
    }

    alias uint pthread_key_t;
    alias void* pthread_t;
}
else version (OpenBSD)
{
    alias void* pthread_attr_t;
    alias void* pthread_cond_t;
    alias void* pthread_condattr_t;
    alias int   pthread_key_t;
    alias void* pthread_mutex_t;
    alias void* pthread_mutexattr_t;

    private struct pthread_once
    {
        int state;
        pthread_mutex_t mutex;
    }
    alias pthread_once pthread_once_t;

    alias void* pthread_rwlock_t;
    alias void* pthread_rwlockattr_t;
    alias void* pthread_t;
}
else version (DragonFlyBSD)
{
    alias int lwpid_t;

    alias void* pthread_attr_t;
    alias void* pthread_cond_t;
    alias void* pthread_condattr_t;
    alias void* pthread_key_t;
    alias void* pthread_mutex_t;
    alias void* pthread_mutexattr_t;
    alias void* pthread_once_t;
    alias void* pthread_rwlock_t;
    alias void* pthread_rwlockattr_t;
    alias void* pthread_t;
}
else version (Solaris)
{
    alias uint pthread_t;

    struct pthread_attr_t
    {
        void* __pthread_attrp;
    }

    struct pthread_cond_t
    {
        struct ___pthread_cond_flags
        {
            ubyte[4] __pthread_cond_flags;
            ushort __pthread_cond_type;
            ushort __pthread_cond_magic;
        }

        ___pthread_cond_flags __pthread_cond_flags;
        ulong __pthread_cond_data;
    }

    struct pthread_condattr_t
    {
        void* __pthread_condattrp;
    }

    struct pthread_rwlock_t
    {
        int __pthread_rwlock_readers;
        ushort __pthread_rwlock_type;
        ushort __pthread_rwlock_magic;
        pthread_mutex_t __pthread_rwlock_mutex;
        pthread_cond_t __pthread_rwlock_readercv;
        pthread_cond_t __pthread_rwlock_writercv;
    }

    struct pthread_rwlockattr_t
    {
        void* __pthread_rwlockattrp;
    }

    struct pthread_mutex_t
    {
        struct ___pthread_mutex_flags
        {
            ushort __pthread_mutex_flag1;
            ubyte __pthread_mutex_flag2;
            ubyte __pthread_mutex_ceiling;
            ushort __pthread_mutex_type;
            ushort __pthread_mutex_magic;
        }

        ___pthread_mutex_flags __pthread_mutex_flags;

        union ___pthread_mutex_lock
        {
            struct ___pthread_mutex_lock64
            {
                ubyte[8] __pthread_mutex_pad;
            }

            ___pthread_mutex_lock64 __pthread_mutex_lock64;

            struct ___pthread_mutex_lock32
            {
                uint __pthread_ownerpid;
                uint __pthread_lockword;
            }

            ___pthread_mutex_lock32 __pthread_mutex_lock32;
            ulong __pthread_mutex_owner64;
        }

        ___pthread_mutex_lock __pthread_mutex_lock;
        ulong __pthread_mutex_data;
    }

    struct pthread_mutexattr_t
    {
        void* __pthread_mutexattrp;
    }

    struct pthread_once_t
    {
        ulong[4] __pthread_once_pad;
    }

    alias uint pthread_key_t;
}
else version (CRuntime_Bionic)
{
    struct pthread_attr_t
    {
        uint    flags;
        void*   stack_base;
        size_t  stack_size;
        size_t  guard_size;
        int     sched_policy;
        int     sched_priority;
        version (D_LP64) char[16] __reserved = 0;
    }

    struct pthread_cond_t
    {
        version (D_LP64)
            int[12] __private;
        else
            int[1] __private;
    }

    alias c_long pthread_condattr_t;
    alias int    pthread_key_t;

    struct pthread_mutex_t
    {
        version (D_LP64)
            int[10] __private;
        else
            int[1] __private;
    }

    alias c_long pthread_mutexattr_t;
    alias int    pthread_once_t;

    struct pthread_rwlock_t
    {
        version (D_LP64)
            int[14] __private;
        else
            int[10] __private;
    }

    alias c_long pthread_rwlockattr_t;
    alias c_long pthread_t;
}
else version (CRuntime_UClibc)
{
     version (X86_64)
     {
        enum __SIZEOF_PTHREAD_ATTR_T        = 56;
        enum __SIZEOF_PTHREAD_MUTEX_T       = 40;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T   = 4;
        enum __SIZEOF_PTHREAD_COND_T        = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T    = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T      = 56;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T  = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T     = 32;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
     }
     else version (MIPS32)
     {
        enum __SIZEOF_PTHREAD_ATTR_T        = 36;
        enum __SIZEOF_PTHREAD_MUTEX_T       = 24;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T   = 4;
        enum __SIZEOF_PTHREAD_COND_T        = 48;
        enum __SIZEOF_PTHREAD_CONDATTR_T    = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T      = 32;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T  = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T     = 20;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
     }
     else version (ARM)
     {
        enum __SIZEOF_PTHREAD_ATTR_T = 36;
        enum __SIZEOF_PTHREAD_MUTEX_T = 24;
        enum __SIZEOF_PTHREAD_MUTEXATTR_T = 4;
        enum __SIZEOF_PTHREAD_COND_T = 48;
        enum __SIZEOF_PTHREAD_COND_COMPAT_T = 12;
        enum __SIZEOF_PTHREAD_CONDATTR_T = 4;
        enum __SIZEOF_PTHREAD_RWLOCK_T = 32;
        enum __SIZEOF_PTHREAD_RWLOCKATTR_T = 8;
        enum __SIZEOF_PTHREAD_BARRIER_T = 20;
        enum __SIZEOF_PTHREAD_BARRIERATTR_T = 4;
     }
     else
     {
        static assert (false, "Architecture unsupported");
     }

    union pthread_attr_t
    {
        byte[__SIZEOF_PTHREAD_ATTR_T] __size;
        c_long __align;
    }

    union pthread_cond_t
    {
        struct data
        {
            int __lock;
            uint __futex;
            ulong __total_seq;
            ulong __wakeup_seq;
            ulong __woken_seq;
            void *__mutex;
            uint __nwaiters;
            uint __broadcast_seq;
        } data __data;
        byte[__SIZEOF_PTHREAD_COND_T] __size;
        long  __align;
    }

    union pthread_condattr_t
    {
        byte[__SIZEOF_PTHREAD_CONDATTR_T] __size;
        c_long __align;
    }

    alias uint pthread_key_t;

    struct __pthread_slist_t
    {
      __pthread_slist_t* __next;
    }

    union pthread_mutex_t
    {
      struct __pthread_mutex_s
      {
        int __lock;
        uint __count;
        int __owner;
        /* KIND must stay at this position in the structure to maintain
           binary compatibility.  */
        int __kind;
        uint __nusers;
        union
        {
          int __spins;
          __pthread_slist_t __list;
        }
      }
      __pthread_mutex_s __data;
        byte[__SIZEOF_PTHREAD_MUTEX_T] __size;
        c_long __align;
    }

    union pthread_mutexattr_t
    {
        byte[__SIZEOF_PTHREAD_MUTEXATTR_T] __size;
        c_long __align;
    }

    alias int pthread_once_t;

    struct pthread_rwlock_t
    {
        struct data
        {
            int __lock;
            uint __nr_readers;
            uint __readers_wakeup;
            uint __writer_wakeup;
            uint __nr_readers_queued;
            uint __nr_writers_queued;
            version (BigEndian)
            {
                ubyte __pad1;
                ubyte __pad2;
                ubyte __shared;
                ubyte __flags;
            }
            else
            {
                ubyte __flags;
                ubyte __shared;
                ubyte __pad1;
                ubyte __pad2;
            }
            int __writer;
        } data __data;
        byte[__SIZEOF_PTHREAD_RWLOCK_T] __size;
        c_long __align;
    }

    struct pthread_rwlockattr_t
    {
        byte[__SIZEOF_PTHREAD_RWLOCKATTR_T] __size;
        c_long __align;
    }

    alias c_ulong pthread_t;
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Barrier (BAR)
//
/*
pthread_barrier_t
pthread_barrierattr_t
*/

version (CRuntime_Glibc)
{
    struct pthread_barrier_t
    {
        byte[__SIZEOF_PTHREAD_BARRIER_T] __size;
        c_long __align;
    }

    struct pthread_barrierattr_t
    {
        byte[__SIZEOF_PTHREAD_BARRIERATTR_T] __size;
        int __align;
    }
}
else version (FreeBSD)
{
    alias void* pthread_barrier_t;
    alias void* pthread_barrierattr_t;
}
else version (NetBSD)
{
    alias void* pthread_barrier_t;
    alias void* pthread_barrierattr_t;
}
else version (OpenBSD)
{
    alias void* pthread_barrier_t;
    alias void* pthread_barrierattr_t;
}
else version (DragonFlyBSD)
{
    alias void* pthread_barrier_t;
    alias void* pthread_barrierattr_t;
}
else version (Darwin)
{
}
else version (Solaris)
{
    struct pthread_barrier_t
    {
        uint __pthread_barrier_count;
        uint __pthread_barrier_current;
        ulong __pthread_barrier_cycle;
        ulong __pthread_barrier_reserved;
        pthread_mutex_t __pthread_barrier_lock;
        pthread_cond_t __pthread_barrier_cond;
    }

    struct pthread_barrierattr_t
    {
        void* __pthread_barrierattrp;
    }
}
else version (CRuntime_Bionic)
{
}
else version (CRuntime_Musl)
{
    version (D_LP64)
    {
        union pthread_barrier_t
        {
            int[8] __i;
            void*[4] __p;
        }
    }
    else
    {
        union pthread_barrier_t
        {
            int[5] __i;
            void*[5] __p;
        }
    }

    struct pthread_barrierattr_t
    {
        uint __attr;
    }
}
else version (CRuntime_UClibc)
{
    struct pthread_barrier_t
    {
        byte[__SIZEOF_PTHREAD_BARRIER_T] __size;
        c_long __align;
    }

    struct pthread_barrierattr_t
    {
        byte[__SIZEOF_PTHREAD_BARRIERATTR_T] __size;
        int __align;
    }
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Spin (SPN)
//
/*
pthread_spinlock_t
*/

version (CRuntime_Glibc)
{
    alias int pthread_spinlock_t; // volatile
}
else version (FreeBSD)
{
    alias void* pthread_spinlock_t;
}
else version (NetBSD)
{
    //already defined
}
else version (OpenBSD)
{
    alias void* pthread_spinlock_t;
}
else version (DragonFlyBSD)
{
    alias void* pthread_spinlock_t;
}
else version (Solaris)
{
    alias pthread_mutex_t pthread_spinlock_t;
}
else version (CRuntime_UClibc)
{
    alias int pthread_spinlock_t; // volatile
}
else version (CRuntime_Musl)
{
    alias int pthread_spinlock_t;
}

//
// Timer (TMR)
//
/*
clockid_t
timer_t
*/

//
// Trace (TRC)
//
/*
trace_attr_t
trace_event_id_t
trace_event_set_t
trace_id_t
*/
