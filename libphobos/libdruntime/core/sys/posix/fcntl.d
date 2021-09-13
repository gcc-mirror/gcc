/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly, Alex Rønne Petersen
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.fcntl;

import core.sys.posix.config;
import core.stdc.stdint;
public import core.sys.posix.sys.types; // for off_t, mode_t
public import core.sys.posix.sys.stat;  // for S_IFMT, etc.

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (ARM)     version = ARM_Any;
version (AArch64) version = ARM_Any;
version (HPPA)    version = HPPA_Any;
version (MIPS32)  version = MIPS_Any;
version (MIPS64)  version = MIPS_Any;
version (PPC)     version = PPC_Any;
version (PPC64)   version = PPC_Any;
version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;
version (S390)    version = IBMZ_Any;
version (SPARC)   version = SPARC_Any;
version (SPARC64) version = SPARC_Any;
version (SystemZ) version = IBMZ_Any;
version (X86)     version = X86_Any;
version (X86_64)  version = X86_Any;

version (Posix):
extern (C):

nothrow:
@nogc:
@system:

//
// Required
//
/*
F_DUPFD
F_GETFD
F_SETFD
F_GETFL
F_SETFL
F_GETLK
F_SETLK
F_SETLKW
F_GETOWN
F_SETOWN

FD_CLOEXEC

F_RDLCK
F_UNLCK
F_WRLCK

O_CREAT
O_EXCL
O_NOCTTY
O_TRUNC

O_APPEND
O_DSYNC
O_NONBLOCK
O_RSYNC
O_SYNC

O_ACCMODE
O_RDONLY
O_RDWR
O_WRONLY

struct flock
{
    short   l_type;
    short   l_whence;
    off_t   l_start;
    off_t   l_len;
    pid_t   l_pid;
}

int creat(const scope char*, mode_t);
int fcntl(int, int, ...);
int open(const scope char*, int, ...);
*/
version (CRuntime_Glibc)
{
    enum F_DUPFD        = 0;
    enum F_GETFD        = 1;
    enum F_SETFD        = 2;
    enum F_GETFL        = 3;
    enum F_SETFL        = 4;
  version (X86_64)
  {
    static assert(off_t.sizeof == 8);
    enum F_GETLK        = 5;
    enum F_SETLK        = 6;
    enum F_SETLKW       = 7;
  }
  else version (AArch64)
  {
    enum F_GETLK        = 5;
    enum F_SETLK        = 6;
    enum F_SETLKW       = 7;
  }
  else version (SystemZ)
  {
    static assert(off_t.sizeof == 8);
    enum F_GETLK        = 5;
    enum F_SETLK        = 6;
    enum F_SETLKW       = 7;
  }
  else
  static if ( __USE_FILE_OFFSET64 )
  {
    enum F_GETLK        = 12;
    enum F_SETLK        = 13;
    enum F_SETLKW       = 14;
  }
  else
  {
    enum F_GETLK        = 5;
    enum F_SETLK        = 6;
    enum F_SETLKW       = 7;
  }
    enum F_GETOWN       = 9;
    enum F_SETOWN       = 8;

    enum FD_CLOEXEC     = 1;

    enum F_RDLCK        = 0;
    enum F_UNLCK        = 2;
    enum F_WRLCK        = 1;

    version (X86_Any)
    {
        enum O_CREAT        = 0x40;     // octal     0100
        enum O_EXCL         = 0x80;     // octal     0200
        enum O_NOCTTY       = 0x100;    // octal     0400
        enum O_TRUNC        = 0x200;    // octal    01000

        enum O_APPEND       = 0x400;    // octal    02000
        enum O_NONBLOCK     = 0x800;    // octal    04000
        enum O_SYNC         = 0x101000; // octal 04010000
        enum O_DSYNC        = 0x1000;   // octal   010000
        enum O_RSYNC        = O_SYNC;
    }
    else version (HPPA_Any)
    {
        enum O_CREAT        = 0x00100;  // octal    04000
        enum O_EXCL         = 0x00400;  // octal     0200
        enum O_NOCTTY       = 0x20000;  // octal     0400
        enum O_TRUNC        = 0x00200;  // octal    01000

        enum O_APPEND       = 0x00008;  // octal      010
        enum O_NONBLOCK     = 0x10004;  // octal  0200004
        enum O_SYNC         = 0x48000;  // octal 01100000
        enum O_DSYNC        = 0x40000;  // octal 01000000
        enum O_RSYNC        = 0x80000;  // octal 02000000
    }
    else version (MIPS_Any)
    {
        enum O_CREAT        = 0x0100;
        enum O_EXCL         = 0x0400;
        enum O_NOCTTY       = 0x0800;
        enum O_TRUNC        = 0x0200;

        enum O_APPEND       = 0x0008;
        enum O_DSYNC        = 0x0010;
        enum O_NONBLOCK     = 0x0080;
        enum O_RSYNC        = O_SYNC;
        enum O_SYNC         = 0x4010;
    }
    else version (PPC_Any)
    {
        enum O_CREAT        = 0x40;     // octal     0100
        enum O_EXCL         = 0x80;     // octal     0200
        enum O_NOCTTY       = 0x100;    // octal     0400
        enum O_TRUNC        = 0x200;    // octal    01000

        enum O_APPEND       = 0x400;    // octal    02000
        enum O_NONBLOCK     = 0x800;    // octal    04000
        enum O_SYNC         = 0x101000; // octal 04010000
        enum O_DSYNC        = 0x1000;   // octal   010000
        enum O_RSYNC        = O_SYNC;
    }
    else version (ARM_Any)
    {
        enum O_CREAT        = 0x40;     // octal     0100
        enum O_EXCL         = 0x80;     // octal     0200
        enum O_NOCTTY       = 0x100;    // octal     0400
        enum O_TRUNC        = 0x200;    // octal    01000

        enum O_APPEND       = 0x400;    // octal    02000
        enum O_NONBLOCK     = 0x800;    // octal    04000
        enum O_SYNC         = 0x101000; // octal 04010000
        enum O_DSYNC        = 0x1000;   // octal   010000
        enum O_RSYNC        = O_SYNC;
    }
    else version (RISCV_Any)
    {
        enum O_CREAT        = 0x40;     // octal     0100
        enum O_EXCL         = 0x80;     // octal     0200
        enum O_NOCTTY       = 0x100;    // octal     0400
        enum O_TRUNC        = 0x200;    // octal    01000

        enum O_APPEND       = 0x400;    // octal    02000
        enum O_NONBLOCK     = 0x800;    // octal    04000
        enum O_SYNC         = 0x101000; // octal 04010000
        enum O_DSYNC        = 0x1000;   // octal   010000
        enum O_RSYNC        = O_SYNC;
    }
    else version (SPARC_Any)
    {
        enum O_CREAT        = 0x200;
        enum O_EXCL         = 0x800;
        enum O_NOCTTY       = 0x8000;
        enum O_TRUNC        = 0x400;

        enum O_APPEND       = 0x8;
        enum O_NONBLOCK     = 0x4000;
        enum O_SYNC         = 0x802000;
        enum O_DSYNC        = 0x2000;
        enum O_RSYNC        = O_SYNC;
    }
    else version (IBMZ_Any)
    {
        enum O_CREAT        = 0x40;     // octal     0100
        enum O_EXCL         = 0x80;     // octal     0200
        enum O_NOCTTY       = 0x100;    // octal     0400
        enum O_TRUNC        = 0x200;    // octal    01000

        enum O_APPEND       = 0x400;    // octal    02000
        enum O_NONBLOCK     = 0x800;    // octal    04000
        enum O_SYNC         = 0x101000; // octal 04010000
        enum O_DSYNC        = 0x1000;   // octal   010000
        enum O_RSYNC        = O_SYNC;
    }
    else
        static assert(0, "unimplemented");

    enum O_ACCMODE      = 0x3;
    enum O_RDONLY       = 0x0;
    enum O_WRONLY       = 0x1;
    enum O_RDWR         = 0x2;

    struct flock
    {
        short   l_type;
        short   l_whence;
        off_t   l_start;
        off_t   l_len;
        pid_t   l_pid;
    }

    static if ( __USE_FILE_OFFSET64 )
    {
        int   creat64(const scope char*, mode_t);
        alias creat64 creat;

        int   open64(const scope char*, int, ...);
        alias open64 open;
    }
    else
    {
        int   creat(const scope char*, mode_t);
        int   open(const scope char*, int, ...);
    }

    enum AT_SYMLINK_NOFOLLOW = 0x100;
    enum AT_FDCWD = -100;
}
else version (Darwin)
{
    enum F_DUPFD        = 0;
    enum F_GETFD        = 1;
    enum F_SETFD        = 2;
    enum F_GETFL        = 3;
    enum F_SETFL        = 4;
    enum F_GETOWN       = 5;
    enum F_SETOWN       = 6;
    enum F_GETLK        = 7;
    enum F_SETLK        = 8;
    enum F_SETLKW       = 9;

    enum FD_CLOEXEC     = 1;

    enum F_RDLCK        = 1;
    enum F_UNLCK        = 2;
    enum F_WRLCK        = 3;

    enum O_CREAT        = 0x0200;
    enum O_EXCL         = 0x0800;
    enum O_NOCTTY       = 0;
    enum O_TRUNC        = 0x0400;

    enum O_RDONLY       = 0x0000;
    enum O_WRONLY       = 0x0001;
    enum O_RDWR         = 0x0002;
    enum O_ACCMODE      = 0x0003;

    enum O_NONBLOCK     = 0x0004;
    enum O_APPEND       = 0x0008;
    enum O_SYNC         = 0x0080;
    //enum O_DSYNC
    //enum O_RSYNC

    struct flock
    {
        off_t   l_start;
        off_t   l_len;
        pid_t   l_pid;
        short   l_type;
        short   l_whence;
    }

    int creat(const scope char*, mode_t);
    int open(const scope char*, int, ...);
}
else version (FreeBSD)
{
    enum F_DUPFD        = 0;
    enum F_GETFD        = 1;
    enum F_SETFD        = 2;
    enum F_GETFL        = 3;
    enum F_SETFL        = 4;
    enum F_GETOWN       = 5;
    enum F_SETOWN       = 6;
    enum F_GETLK        = 11;
    enum F_SETLK        = 12;
    enum F_SETLKW       = 13;
    enum F_OGETLK       = 7;
    enum F_OSETLK       = 8;
    enum F_OSETLKW      = 9;
    enum F_DUP2FD       = 10;

    enum FD_CLOEXEC     = 1;

    enum F_RDLCK        = 1;
    enum F_UNLCK        = 2;
    enum F_WRLCK        = 3;

    enum O_CREAT        = 0x0200;
    enum O_EXCL         = 0x0800;
    enum O_NOCTTY       = 0x8000;
    enum O_TRUNC        = 0x0400;

    enum O_RDONLY       = 0x0000;
    enum O_WRONLY       = 0x0001;
    enum O_RDWR         = 0x0002;
    enum O_ACCMODE      = 0x0003;

    enum O_NONBLOCK     = 0x0004;
    enum O_APPEND       = 0x0008;
    enum O_SYNC         = 0x0080;
    //enum O_DSYNC
    //enum O_RSYNC

    struct flock
    {
        off_t   l_start;
        off_t   l_len;
        pid_t   l_pid;
        short   l_type;
        short   l_whence;
        int     l_sysid;
    }

    struct oflock
    {
        off_t   l_start;
        off_t   l_len;
        pid_t   l_pid;
        short   l_type;
        short   l_whence;
    }

    int creat(const scope char*, mode_t);
    int open(const scope char*, int, ...);

    enum AT_SYMLINK_NOFOLLOW = 0x200;
    enum AT_FDCWD = -100;
}
else version (OpenBSD)
{
    enum F_DUPFD        = 0;
    enum F_GETFD        = 1;
    enum F_SETFD        = 2;
    enum F_GETFL        = 3;
    enum F_SETFL        = 4;
    enum F_GETOWN       = 5;
    enum F_SETOWN       = 6;
    enum F_GETLK        = 7;
    enum F_SETLK        = 8;
    enum F_SETLKW       = 9;
    enum F_DUPFD_CLOEXEC= 10;
    enum F_ISATTY       = 11;

    enum FD_CLOEXEC     = 1;

    enum F_RDLCK        = 1;
    enum F_UNLCK        = 2;
    enum F_WRLCK        = 3;

    enum O_CREAT        = 0x0200;
    enum O_EXCL         = 0x0800;
    enum O_NOCTTY       = 0x8000;
    enum O_TRUNC        = 0x0400;

    enum O_RDONLY       = 0x0000;
    enum O_WRONLY       = 0x0001;
    enum O_RDWR         = 0x0002;
    enum O_ACCMODE      = 0x0003;
    enum O_SHLOCK       = 0x0010;
    enum O_EXLOCK       = 0x0020;
    enum O_ASYNC        = 0x0040;
    enum O_FSYNC        = 0x0080;
    enum O_NOFOLLOW     = 0x0100;

    enum O_NONBLOCK     = 0x0004;
    enum O_APPEND       = 0x0008;
    enum O_SYNC         = 0x0080;
    enum O_DSYNC        = O_SYNC;
    enum O_RSYNC        = O_SYNC;

    enum O_CLOEXEC      = 0x10000;
    enum O_DIRECTORY    = 0x20000;

    enum LOCK_SH        = 0x01;
    enum LOCK_EX        = 0x02;
    enum LOCK_NB        = 0x04;
    enum LOCK_UN        = 0x08;

    struct flock
    {
        off_t   l_start;
        off_t   l_len;
        pid_t   l_pid;
        short   l_type;
        short   l_whence;
    }

    int creat(const scope char*, mode_t);
    int open(const scope char*, int, ...);

    enum AT_FDCWD            = -100;

    enum AT_EACCESS          = 0x01;
    enum AT_SYMLINK_NOFOLLOW = 0x02;
    enum AT_SYMLINK_FOLLOW   = 0x04;
    enum AT_REMOVEDIR        = 0x08;
}
else version (NetBSD)
{
    enum F_DUPFD        = 0;
    enum F_GETFD        = 1;
    enum F_SETFD        = 2;
    enum F_GETFL        = 3;
    enum F_SETFL        = 4;
    enum F_GETOWN       = 5;
    enum F_SETOWN       = 6;
    enum F_GETLK        = 7;
    enum F_SETLK        = 8;
    enum F_SETLKW       = 9;
    enum F_CLOSEM       = 10;
    enum F_MAXFD        = 11;
    enum F_DUPFD_CLOEXEC= 12;
    enum F_GETNOSIGPIPE = 13;
    enum F_SETNOSIGPIPE = 14;

    enum FD_CLOEXEC     = 1;

    enum F_RDLCK        = 1;
    enum F_UNLCK        = 2;
    enum F_WRLCK        = 3;

    enum O_CREAT        = 0x0200;
    enum O_EXCL         = 0x0800;
    enum O_NOCTTY       = 0x8000;
    enum O_TRUNC        = 0x0400;

    enum O_RDONLY       = 0x0000;
    enum O_WRONLY       = 0x0001;
    enum O_RDWR         = 0x0002;
    enum O_ACCMODE      = 0x0003;

    enum O_NONBLOCK     = 0x0004;
    enum O_APPEND       = 0x0008;
    enum O_SYNC         = 0x0080;
    //enum O_DSYNC
    //enum O_RSYNC

    struct flock
    {
        off_t   l_start;
        off_t   l_len;
        pid_t   l_pid;
        short   l_type;
        short   l_whence;
    }


    int creat(const scope char*, mode_t);
    int open(const scope char*, int, ...);
}
else version (DragonFlyBSD)
{
    enum O_RDONLY       = 0x0000;
    enum O_WRONLY       = 0x0001;
    enum O_RDWR         = 0x0002;
    enum O_ACCMODE      = 0x0003;

    enum FREAD          = 0x0001;
    enum FWRITE         = 0x0002;
    enum O_NONBLOCK     = 0x0000004;
    enum O_APPEND       = 0x0000008;
    enum O_SHLOCK       = 0x0000010;
    enum O_EXLOCK       = 0x0000020;
    enum O_ASYNC        = 0x0000040;
    enum O_FSYNC        = 0x0000080;
    enum O_SYNC         = 0x0000080;
    enum O_NOFOLLOW     = 0x0000100;
    enum O_CREAT        = 0x0000200;
    enum O_TRUNC        = 0x0000400;
    enum O_EXCL         = 0x0000800;
    enum O_NOCTTY       = 0x0008000;
    enum O_DIRECT       = 0x0010000;
    enum O_CLOEXEC      = 0x0020000;
    enum O_FBLOCKING    = 0x0040000;
    enum O_FNONBLOCKING = 0x0080000;
    enum O_FAPPEND      = 0x0100000;
    enum O_FOFFSET      = 0x0200000;
    enum O_FSYNCWRITE   = 0x0400000;
    enum O_FASYNCWRITE  = 0x0800000;
    enum O_DIRECTORY    = 0x8000000;

    enum FAPPEND        = O_APPEND;
    enum FASYNC         = O_ASYNC;
    enum FFSYNC         = O_FSYNC;
    enum FNONBLOCK      = O_NONBLOCK;
    enum FNDELAY        = O_NONBLOCK;
    enum O_NDELAY       = O_NONBLOCK;
    enum FPOSIXSHM      = O_NOFOLLOW;

    enum FCNTLFLAGS = (FAPPEND|FASYNC|FFSYNC|FNONBLOCK|FPOSIXSHM|O_DIRECT);

    enum F_DUPFD        = 0;
    enum F_GETFD        = 1;
    enum F_SETFD        = 2;
    enum F_GETFL        = 3;
    enum F_SETFL        = 4;
    enum F_GETOWN       = 5;
    enum F_SETOWN       = 6;
    enum F_GETLK        = 7;
//    enum F_SETLK        = 8;
    enum F_SETLK        = 8;
    enum F_SETLKW       = 9;
    enum F_OGETLK       = F_GETLK;
    enum F_OSETLK       = F_SETLK;
    enum F_OSETLKW      = F_SETLKW;
    enum F_DUP2FD       = 10;
    //enum F_GETLK        = 11;
    //enum F_SETLK        = 12;
    //enum F_SETLKW       = 13;
    enum F_DUPFD_CLOEXEC = 17;
    enum F_DUP2FD_CLOEXEC = 18;

    enum FD_CLOEXEC     = 1;

    enum F_RDLCK        = 1;
    enum F_UNLCK        = 2;
    enum F_WRLCK        = 3;

    enum LOCK_SH        = 0x01;
    enum LOCK_EX        = 0x02;
    enum LOCK_NB        = 0x04;
    enum LOCK_UN        = 0x08;

    struct flock
    {
        off_t   l_start;
        off_t   l_len;
        pid_t   l_pid;
        short   l_type;
        short   l_whence;
    }

    alias oflock = flock;

    int creat(const scope char*, mode_t);
    int open(const scope char*, int, ...);
    //int fcntl(int, int, ...);  /*defined below*/
    //int flock(int, int);
}
else version (Solaris)
{
    enum F_DUPFD = 0;
    enum F_GETFD = 1;
    enum F_SETFD = 2;
    enum F_GETFL = 3;
    enum F_SETFL = 4;

    version (D_LP64)
    {
        enum F_GETLK = 14;
        enum F_SETLK = 6;
        enum F_SETLKW = 7;
    }
    else
    {
        static if (__USE_FILE_OFFSET64)
        {
            enum F_GETLK = 14;
            enum F_SETLK = 6;
            enum F_SETLKW = 7;
        }
        else
        {
            enum F_GETLK = 33;
            enum F_SETLK = 34;
            enum F_SETLKW = 35;
        }
    }

    enum F_GETOWN = 23;
    enum F_SETOWN = 24;

    enum FD_CLOEXEC = 1;

    enum F_RDLCK = 1;
    enum F_UNLCK = 3;
    enum F_WRLCK = 2;
    enum F_UNCKSYS = 4;

    enum O_CREAT = 0x0100;
    enum O_EXCL = 0x0400;
    enum O_NOCTTY = 0x0800;
    enum O_TRUNC = 0x0200;

    enum O_APPEND = 0x0008;
    enum O_NONBLOCK = 0x0080;
    enum O_SYNC = 0x0010;
    enum O_DSYNC = 0x0040;
    enum O_RSYNC = 0x8000;

    enum O_ACCMODE = (O_SEARCH | O_EXEC | 0x3);
    enum O_RDONLY = 0;
    enum O_WRONLY = 1;
    enum O_RDWR = 2;
    enum O_SEARCH = 0x200000;
    enum O_EXEC = 0x400000;

    struct flock
    {
        short l_type;
        short l_whence;
        off_t l_start;
        off_t l_len;
        int l_sysid;
        pid_t l_pid;
        c_long[4] l_pad;
    }

    static if (__USE_LARGEFILE64)
    {
        struct flock64
        {
            short       l_type;
            short       l_whence;
            off64_t     l_start;
            off64_t     l_len;
            int         l_sysid;
            pid_t       l_pid;
            c_long[4]   l_pad;
        }
    }

    version (D_LP64)
    {
        int creat(const scope char*, mode_t);
        int open(const scope char*, int, ...);

        static if (__USE_LARGEFILE64)
        {
            alias creat creat64;
            alias open open64;
        }
    }
    else
    {
        static if (__USE_LARGEFILE64)
        {
            int creat64(const scope char*, mode_t);
            alias creat64 creat;

            int open64(const scope char*, int, ...);
            alias open64 open;
        }
        else
        {
            int creat(const scope char*, mode_t);
            int open(const scope char*, int, ...);
        }
    }
}
else version (CRuntime_Bionic)
{
    // All these except for the two functions open and creat really come from
    // the linux kernel and can probably be merged.
    enum F_DUPFD        = 0;
    enum F_GETFD        = 1;
    enum F_SETFD        = 2;
    enum F_GETFL        = 3;
    enum F_SETFL        = 4;
    enum F_GETLK        = 5;
    enum F_SETLK        = 6;
    enum F_SETLKW       = 7;
    enum F_SETOWN       = 8;
    enum F_GETOWN       = 9;

    enum FD_CLOEXEC     = 1;

    enum F_RDLCK        = 0;
    enum F_WRLCK        = 1;
    enum F_UNLCK        = 2;

    enum O_CREAT        = 0x40;     // octal     0100
    enum O_EXCL         = 0x80;     // octal     0200
    enum O_NOCTTY       = 0x100;    // octal     0400
    enum O_TRUNC        = 0x200;    // octal    01000

    enum O_APPEND       = 0x400;    // octal    02000
    enum O_NONBLOCK     = 0x800;    // octal    04000

    version (D_LP64)
    {
        enum O_SYNC     = 0x101000; // octal 04010000
    }
    else
    {
        enum O_SYNC     = 0x1000;   // octal   010000
    }

    enum O_ACCMODE      = 0x3;
    enum O_RDONLY       = 0x0;
    enum O_WRONLY       = 0x1;
    enum O_RDWR         = 0x2;

    struct flock
    {
        short   l_type;
        short   l_whence;
        off_t   l_start;
        off_t   l_len;
        pid_t   l_pid;
    }

    int   creat(const scope char*, mode_t);
    int   open(const scope char*, int, ...);

    enum AT_FDCWD = -100;
}
else version (CRuntime_Musl)
{
    version (X86_64)
    {
        enum
        {
            O_DIRECTORY     = 0x010000, // octal   0200000
            O_NOFOLLOW      = 0x020000, // octal   0400000
            O_DIRECT        = 0x004000, // octal    040000
            O_LARGEFILE     =        0,
            O_TMPFILE       = 0x410000, // octal 020200000

            F_GETLK        = 5,
            F_SETLK        = 6,
            F_SETLKW       = 7,
        }
    }
    // Note: Definitions for i386 are in arch/generic/bits/fcntl.h
    else version (X86)
    {
        enum
        {
            O_DIRECTORY     = 0x010000, // octal   0200000
            O_NOFOLLOW      = 0x020000, // octal   0400000
            O_DIRECT        = 0x004000, // octal    040000
            O_LARGEFILE     = 0x008000, // octal   0100000
            O_TMPFILE       = 0x410000, // octal 020200000

            F_GETLK        = 12,
            F_SETLK        = 13,
            F_SETLKW       = 14,
        }
    }
    else version (ARM)
    {
        enum
        {
            O_DIRECTORY     = 0x004000, // octal    040000
            O_NOFOLLOW      = 0x008000, // octal   0100000
            O_DIRECT        = 0x010000, // octal   0200000
            O_LARGEFILE     = 0x020000, // octal   0400000
            O_TMPFILE       = 0x404000, // octal 020040000

            F_GETLK        = 12,
            F_SETLK        = 13,
            F_SETLKW       = 14,
        }
    }
    else version (AArch64)
    {
        enum
        {
            O_DIRECTORY     = 0x004000, // octal    040000
            O_NOFOLLOW      = 0x008000, // octal   0100000
            O_DIRECT        = 0x010000, // octal   0200000
            O_LARGEFILE     = 0x020000, // octal   0400000
            O_TMPFILE       = 0x404000, // octal 020040000

            F_GETLK        = 5,
            F_SETLK        = 6,
            F_SETLKW       = 7,
        }
    }
    else version (SystemZ)
    {
        enum
        {
            O_DIRECTORY     = 0x010000, // octal   0200000
            O_NOFOLLOW      = 0x020000, // octal   0400000
            O_DIRECT        = 0x004000, // octal    040000
            O_LARGEFILE     = 0x008000, // octal   0100000
            O_TMPFILE       = 0x410000, // octal 020200000

            F_GETLK        = 5,
            F_SETLK        = 6,
            F_SETLKW       = 7,
        }
    }
    else version (PPC64)
    {
        enum
        {
            O_DIRECTORY     = 0x004000, // octal    040000
            O_NOFOLLOW      = 0x008000, // octal   0100000
            O_DIRECT        = 0x020000, // octal   0400000
            O_LARGEFILE     = 0x010000, // octal   0200000
            O_TMPFILE       = 0x410000, // octal 020200000

            F_GETLK        = 5,
            F_SETLK        = 6,
            F_SETLKW       = 7,
        }
    }
    else
        static assert(0, "Platform not supported");

    enum
    {
        O_CREAT         = 0x40,     // octal     0100
        O_EXCL          = 0x80,     // octal     0200
        O_NOCTTY        = 0x100,    // octal     0400
        O_TRUNC         = 0x200,    // octal    01000

        O_APPEND        = 0x400,    // octal    02000
        O_NONBLOCK      = 0x800,    // octal    04000
        O_DSYNC         = 0x1000,   // octal   010000
        O_SYNC          = 0x101000, // octal 04010000
        O_RSYNC         = O_SYNC,
        O_CLOEXEC       = 0x80000,

        O_ASYNC         = 0x2000,
        O_NOATIME       = 0x40000,
        O_PATH          = 0x200000,
        O_NDELAY        = O_NONBLOCK,
        O_SEARCH        = O_PATH,
        O_EXEC          = O_PATH,

        O_ACCMODE       = (03|O_SEARCH),
        O_RDONLY        = 00,
        O_WRONLY        = 01,
        O_RDWR          = 02,
    }
    enum
    {
        F_DUPFD        = 0,
        F_GETFD        = 1,
        F_SETFD        = 2,
        F_GETFL        = 3,
        F_SETFL        = 4,
        // F_GETLK, F_SETLK, F_SETLKW are arch-specific
        F_SETOWN       = 8,
        F_GETOWN       = 9,
    }
    enum
    {
        F_RDLCK        = 0,
        F_WRLCK        = 1,
        F_UNLCK        = 2,
    }
    struct flock
    {
        short   l_type;
        short   l_whence;
        off_t   l_start;
        off_t   l_len;
        pid_t   l_pid;
    }
    enum FD_CLOEXEC     = 1;
    int open(const scope char*, int, ...);

    enum AT_FDCWD = -100;
    enum AT_SYMLINK_NOFOLLOW = 0x100;
    enum AT_REMOVEDIR = 0x200;
    enum AT_SYMLINK_FOLLOW = 0x400;
    enum AT_EACCESS = 0x200;
}
else version (CRuntime_UClibc)
{
    enum F_DUPFD        = 0;
    enum F_GETFD        = 1;
    enum F_SETFD        = 2;
    enum F_GETFL        = 3;
    enum F_SETFL        = 4;

    version (X86_64)
    {
        enum F_GETLK        = 5;
        enum F_SETLK        = 6;
        enum F_SETLKW       = 7;
    }
    else static if (__USE_FILE_OFFSET64)
    {
        enum F_GETLK        = 5;
        enum F_SETLK        = 6;
        enum F_SETLKW       = 7;
    }
    else
    {
        enum F_GETLK        = 12;
        enum F_SETLK        = 13;
        enum F_SETLKW       = 14;
    }

    enum F_GETOWN       = 9;
    enum F_SETOWN       = 8;

    enum FD_CLOEXEC     = 1;

    enum F_RDLCK        = 0;
    enum F_UNLCK        = 2;
    enum F_WRLCK        = 1;

    version (X86_Any)
    {
        enum O_CREAT        = 0x40;     // octal     0100
        enum O_EXCL         = 0x80;     // octal     0200
        enum O_NOCTTY       = 0x100;    // octal     0400
        enum O_TRUNC        = 0x200;    // octal    01000

        enum O_APPEND       = 0x400;    // octal    02000
        enum O_NONBLOCK     = 0x800;    // octal    04000
        enum O_CLOEXEC      = 0x80000;  // octal    02000000
        enum O_SYNC         = 0x1000;   // octal    010000
        enum O_NDELAY       = O_NONBLOCK;
        enum O_FSYNC        = O_SYNC;
        enum O_ASYNC        = 0x2000;   // octal    020000
    }
    else version (MIPS_Any)
    {
        enum O_CREAT        = 0x0100;
        enum O_EXCL         = 0x0400;
        enum O_NOCTTY       = 0x0800;
        enum O_TRUNC        = 0x0200;

        enum O_APPEND       = 0x0008;
        enum O_SYNC         = 0x0010;
        enum O_NONBLOCK     = 0x0080;
        enum O_CLOEXEC      = 0x80000;  // octal    02000000
        enum O_NDELAY       = O_NONBLOCK;
        enum O_FSYNC        = O_SYNC;
        enum O_ASYNC        = 0x1000;
    }
    else version (ARM_Any)
    {
        enum O_CREAT        = 0x40;     // octal     0100
        enum O_EXCL         = 0x80;     // octal     0200
        enum O_NOCTTY       = 0x100;    // octal     0400
        enum O_TRUNC        = 0x200;    // octal    01000

        enum O_APPEND       = 0x400;    // octal    02000
        enum O_NONBLOCK     = 0x800;    // octal    04000
        enum O_CLOEXEC      = 0x80000;  // octal    02000000
        enum O_SYNC         = 0x1000;   // octal    010000
        enum O_NDELAY       = O_NONBLOCK;
        enum O_FSYNC        = O_SYNC;
        enum O_ASYNC        = 0x2000;     // octal 020000
    }
    else
        static assert(0, "unimplemented");

    enum O_ACCMODE      = 0x3;
    enum O_RDONLY       = 0x0;
    enum O_WRONLY       = 0x1;
    enum O_RDWR         = 0x2;

    struct flock
    {
        short   l_type;
        short   l_whence;
        static if (__USE_FILE_OFFSET64)
        {
            off64_t   l_start;
            off64_t   l_len;
        }
        else
        {
            off_t   l_start;
            off_t   l_len;
        }
        pid_t   l_pid;
    }

    static if ( __USE_FILE_OFFSET64 )
    {
        int   creat64(const scope char*, mode_t);
        alias creat64 creat;

        int   open64(const scope char*, int, ...);
        alias open64 open;
    }
    else
    {
        int   creat(const scope char*, mode_t);
        int   open(const scope char*, int, ...);
    }

    enum AT_SYMLINK_NOFOLLOW    = 0x100;
    enum AT_FDCWD               = -100;
}
else
{
    static assert(false, "Unsupported platform");
}

//int creat(const scope char*, mode_t);
int fcntl(int, int, ...);
//int open(const scope char*, int, ...);

// Generic Posix fallocate
int posix_fallocate(int, off_t, off_t);

//
// Advisory Information (ADV)
//
/*
POSIX_FADV_NORMAL
POSIX_FADV_SEQUENTIAL
POSIX_FADV_RANDOM
POSIX_FADV_WILLNEED
POSIX_FADV_DONTNEED
POSIX_FADV_NOREUSE

int posix_fadvise(int, off_t, off_t, int);
*/
