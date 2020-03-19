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
module core.sys.posix.sys.stat;

private import core.sys.posix.config;
private import core.stdc.stdint;
private import core.sys.posix.time;     // for timespec
public import core.sys.posix.sys.types; // for off_t, mode_t

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;

version (Posix):
extern (C) nothrow @nogc:

//
// Required
//
/*
struct stat
{
    dev_t   st_dev;
    ino_t   st_ino;
    mode_t  st_mode;
    nlink_t st_nlink;
    uid_t   st_uid;
    gid_t   st_gid;
    off_t   st_size;
    time_t  st_atime;
    time_t  st_mtime;
    time_t  st_ctime;
}

S_IRWXU
    S_IRUSR
    S_IWUSR
    S_IXUSR
S_IRWXG
    S_IRGRP
    S_IWGRP
    S_IXGRP
S_IRWXO
    S_IROTH
    S_IWOTH
    S_IXOTH
S_ISUID
S_ISGID
S_ISVTX

S_ISBLK(m)
S_ISCHR(m)
S_ISDIR(m)
S_ISFIFO(m)
S_ISREG(m)
S_ISLNK(m)
S_ISSOCK(m)

S_TYPEISMQ(buf)
S_TYPEISSEM(buf)
S_TYPEISSHM(buf)

int    chmod(in char*, mode_t);
int    fchmod(int, mode_t);
int    fstat(int, stat*);
int    lstat(in char*, stat*);
int    mkdir(in char*, mode_t);
int    mkfifo(in char*, mode_t);
int    stat(in char*, stat*);
mode_t umask(mode_t);
 */

version (CRuntime_Glibc)
{
    version (X86)
    {
        struct stat_t
        {
            dev_t       st_dev;
            ushort      __pad1;
            static if (!__USE_FILE_OFFSET64)
            {
                ino_t       st_ino;
            }
            else
            {
                uint        __st_ino;
            }
            mode_t      st_mode;
            nlink_t     st_nlink;
            uid_t       st_uid;
            gid_t       st_gid;
            dev_t       st_rdev;
            ushort      __pad2;
            off_t       st_size;
            blksize_t   st_blksize;
            blkcnt_t    st_blocks;
            static if (__USE_MISC || __USE_XOPEN2K8)
            {
                timespec    st_atim;
                timespec    st_mtim;
                timespec    st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                time_t      st_atime;
                ulong_t     st_atimensec;
                time_t      st_mtime;
                ulong_t     st_mtimensec;
                time_t      st_ctime;
                ulong_t     st_ctimensec;
            }
            static if (__USE_FILE_OFFSET64)
            {
                ino_t       st_ino;
            }
            else
            {
                c_ulong     __unused4;
                c_ulong     __unused5;
            }
        }
    }
    else version (X86_64)
    {
        struct stat_t
        {
            dev_t       st_dev;
            ino_t       st_ino;
            nlink_t     st_nlink;
            mode_t      st_mode;
            uid_t       st_uid;
            gid_t       st_gid;
            uint        __pad0;
            dev_t       st_rdev;
            off_t       st_size;
            blksize_t   st_blksize;
            blkcnt_t    st_blocks;
            static if (__USE_MISC || __USE_XOPEN2K8)
            {
                timespec    st_atim;
                timespec    st_mtim;
                timespec    st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                time_t      st_atime;
                ulong_t     st_atimensec;
                time_t      st_mtime;
                ulong_t     st_mtimensec;
                time_t      st_ctime;
                ulong_t     st_ctimensec;
            }
            slong_t[3]     __unused;
        }
    }
    else version (HPPA)
    {
        private
        {
            alias __dev_t = ulong;
            alias __ino_t = c_ulong;
            alias __ino64_t = ulong;
            alias __mode_t = uint;
            alias __nlink_t = size_t;
            alias __uid_t = uint;
            alias __gid_t = uint;
            alias __off_t = c_long;
            alias __off64_t = long;
            alias __blksize_t = c_long;
            alias __blkcnt_t = c_long;
            alias __blkcnt64_t = long;
            alias __timespec = timespec;
            alias __time_t = time_t;
        }
        struct stat_t
        {
            __dev_t st_dev;
            ushort __pad1;

            static if (!__USE_FILE_OFFSET64)
            {
                __ino_t st_ino;
            }
            else
            {
                __ino_t __st_ino;
            }
            __mode_t st_mode;
            __nlink_t st_nlink;
            __uid_t st_uid;
            __gid_t st_gid;
            __dev_t st_rdev;
            ushort __pad2;

            static if (!__USE_FILE_OFFSET64)
            {
                __off_t st_size;
            }
            else
            {
                __off64_t st_size;
            }
            __blksize_t st_blksize;

            static if (!__USE_FILE_OFFSET64)
            {
                __blkcnt_t st_blocks;
            }
            else
            {
                __blkcnt64_t st_blocks;
            }

            static if ( __USE_MISC || __USE_XOPEN2K8)
            {
                __timespec st_atim;
                __timespec st_mtim;
                __timespec st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                __time_t st_atime;
                c_ulong st_atimensec;
                __time_t st_mtime;
                c_ulong st_mtimensec;
                __time_t st_ctime;
                c_ulong st_ctimensec;
            }

            static if (!__USE_FILE_OFFSET64)
            {
                c_ulong __unused4;
                c_ulong __unused5;
            }
            else
            {
                __ino64_t st_ino;
            }
        }
        static if (__USE_FILE_OFFSET64)
            static assert(stat_t.sizeof == 104);
        else
            static assert(stat_t.sizeof == 88);
    }
    else version (MIPS_O32)
    {
        struct stat_t
        {
            c_ulong     st_dev;
            c_long[3]   st_pad1;
            ino_t       st_ino;
            mode_t      st_mode;
            nlink_t     st_nlink;
            uid_t       st_uid;
            gid_t       st_gid;
            c_ulong     st_rdev;
            static if (!__USE_FILE_OFFSET64)
            {
                c_long[2]   st_pad2;
                off_t       st_size;
                c_long      st_pad3;
            }
            else
            {
                c_long[3]   st_pad2;
                off_t       st_size;
            }
            static if (__USE_MISC || __USE_XOPEN2K8)
            {
                timespec    st_atim;
                timespec    st_mtim;
                timespec    st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                time_t      st_atime;
                c_ulong     st_atimensec;
                time_t      st_mtime;
                c_ulong     st_mtimensec;
                time_t      st_ctime;
                c_ulong     st_ctimensec;
            }
            blksize_t   st_blksize;
            static if (!__USE_FILE_OFFSET64)
            {
                blkcnt_t    st_blocks;
            }
            else
            {
                c_long      st_pad4;
                blkcnt_t    st_blocks;
            }
            c_long[14]  st_pad5;
        }
    }
    else version (MIPS64)
    {
        struct stat_t
        {
            c_ulong     st_dev;
            int[3]      st_pad1;
            static if (!__USE_FILE_OFFSET64)
            {
                ino_t       st_ino;
            }
            else
            {
                c_ulong     st_ino;
            }
            mode_t      st_mode;
            nlink_t     st_nlink;
            uid_t       st_uid;
            gid_t       st_gid;
            c_ulong     st_rdev;
            static if (!__USE_FILE_OFFSET64)
            {
                uint[2]     st_pad2;
                off_t       st_size;
                int         st_pad3;
            }
            else
            {
                c_long[3]   st_pad2;
                c_long      st_size;
            }
            static if (__USE_MISC || __USE_XOPEN2K8)
            {
                timespec    st_atim;
                timespec    st_mtim;
                timespec    st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                time_t      st_atime;
                c_ulong     st_atimensec;
                time_t      st_mtime;
                c_ulong     st_mtimensec;
                time_t      st_ctime;
                c_ulong     st_ctimensec;
            }
            blksize_t   st_blksize;
            uint        st_pad4;
            static if (!__USE_FILE_OFFSET64)
            {
                blkcnt_t    st_blocks;
            }
            else
            {
                c_long  st_blocks;
            }
            c_long[14]  st_pad5;
        }
    }
    else version (PPC)
    {
        struct stat_t
        {
            c_ulong     st_dev;
            ino_t       st_ino;
            mode_t      st_mode;
            nlink_t     st_nlink;
            uid_t       st_uid;
            gid_t       st_gid;
            c_ulong     st_rdev;
            off_t       st_size;
            c_ulong     st_blksize;
            c_ulong     st_blocks;
            c_ulong     st_atime;
            c_ulong     st_atime_nsec;
            c_ulong     st_mtime;
            c_ulong     st_mtime_nsec;
            c_ulong     st_ctime;
            c_ulong     st_ctime_nsec;
            c_ulong     __unused4;
            c_ulong     __unused5;
        }
    }
    else version (PPC64)
    {
        struct stat_t
        {
            c_ulong     st_dev;
            ino_t       st_ino;
            nlink_t     st_nlink;
            mode_t      st_mode;
            uid_t       st_uid;
            gid_t       st_gid;
            c_ulong     st_rdev;
            off_t       st_size;
            c_ulong     st_blksize;
            c_ulong     st_blocks;
            c_ulong     st_atime;
            c_ulong     st_atime_nsec;
            c_ulong     st_mtime;
            c_ulong     st_mtime_nsec;
            c_ulong     st_ctime;
            c_ulong     st_ctime_nsec;
            c_ulong     __unused4;
            c_ulong     __unused5;
            c_ulong     __unused6;
        }
    }
    else version (RISCV_Any)
    {
        private
        {
            alias __dev_t = ulong;
            alias __ino_t = c_ulong;
            alias __ino64_t = ulong;
            alias __mode_t = uint;
            alias __nlink_t = uint;
            alias __uid_t = uint;
            alias __gid_t = uint;
            alias __off_t = c_long;
            alias __off64_t = long;
            alias __blksize_t = int;
            alias __blkcnt_t = c_long;
            alias __blkcnt64_t = long;
            alias __timespec = timespec;
            alias __time_t = time_t;
        }
        struct stat_t
        {
            __dev_t st_dev;

            static if (__USE_FILE_OFFSET64)
            {
                __ino64_t st_ino;
            }
            else
            {
                __ino_t st_ino;
            }
            __mode_t st_mode;
            __nlink_t st_nlink;
            __uid_t st_uid;
            __gid_t st_gid;
            __dev_t st_rdev;
            __dev_t __pad1;

            static if (__USE_FILE_OFFSET64)
            {
                __off64_t st_size;
            }
            else
            {
                __off_t st_size;
            }
            __blksize_t st_blksize;
            int __pad2;

            static if (__USE_FILE_OFFSET64)
            {
                __blkcnt64_t st_blocks;
            }
            else
            {
                __blkcnt_t st_blocks;
            }

            static if (__USE_MISC)
            {
                __timespec st_atim;
                __timespec st_mtim;
                __timespec st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                __time_t st_atime;
                c_ulong st_atimensec;
                __time_t st_mtime;
                c_ulong st_mtimensec;
                __time_t st_ctime;
                c_ulong st_ctimensec;
            }
            int[2] __unused;
        }
    }
    else version (ARM)
    {
        private
        {
            alias __dev_t = ulong;
            alias __ino_t = c_ulong;
            alias __ino64_t = ulong;
            alias __mode_t = uint;
            alias __nlink_t = size_t;
            alias __uid_t = uint;
            alias __gid_t = uint;
            alias __off_t = c_long;
            alias __off64_t = long;
            alias __blksize_t = c_long;
            alias __blkcnt_t = c_long;
            alias __blkcnt64_t = long;
            alias __timespec = timespec;
            alias __time_t = time_t;
        }
        struct stat_t
        {
            __dev_t st_dev;
            ushort __pad1;

            static if (!__USE_FILE_OFFSET64)
            {
                __ino_t st_ino;
            }
            else
            {
                __ino_t __st_ino;
            }
            __mode_t st_mode;
            __nlink_t st_nlink;
            __uid_t st_uid;
            __gid_t st_gid;
            __dev_t st_rdev;
            ushort __pad2;

            static if (!__USE_FILE_OFFSET64)
            {
                __off_t st_size;
            }
            else
            {
                __off64_t st_size;
            }
            __blksize_t st_blksize;

            static if (!__USE_FILE_OFFSET64)
            {
                __blkcnt_t st_blocks;
            }
            else
            {
                __blkcnt64_t st_blocks;
            }

            static if ( __USE_MISC || __USE_XOPEN2K8)
            {
                __timespec st_atim;
                __timespec st_mtim;
                __timespec st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                __time_t st_atime;
                c_ulong st_atimensec;
                __time_t st_mtime;
                c_ulong st_mtimensec;
                __time_t st_ctime;
                c_ulong st_ctimensec;
            }

            static if (!__USE_FILE_OFFSET64)
            {
                c_ulong __unused4;
                c_ulong __unused5;
            }
            else
            {
                __ino64_t st_ino;
            }
        }
        static if (__USE_FILE_OFFSET64)
            static assert(stat_t.sizeof == 104);
        else
            static assert(stat_t.sizeof == 88);
    }
    else version (AArch64)
    {
        private
        {
            alias __dev_t = ulong;
            alias __ino_t = c_ulong;
            alias __ino64_t = ulong;
            alias __mode_t = uint;
            alias __nlink_t = uint;
            alias __uid_t = uint;
            alias __gid_t = uint;
            alias __off_t = c_long;
            alias __off64_t = long;
            alias __blksize_t = int;
            alias __blkcnt_t = c_long;
            alias __blkcnt64_t = long;
            alias __timespec = timespec;
            alias __time_t = time_t;
        }
        struct stat_t
        {
            __dev_t st_dev;

            static if (!__USE_FILE_OFFSET64)
            {
                __ino_t st_ino;
            }
            else
            {
                __ino64_t st_ino;
            }
            __mode_t st_mode;
            __nlink_t st_nlink;
            __uid_t st_uid;
            __gid_t st_gid;
            __dev_t st_rdev;
            __dev_t __pad1;

            static if (!__USE_FILE_OFFSET64)
            {
                __off_t st_size;
            }
            else
            {
                __off64_t st_size;
            }
            __blksize_t st_blksize;
            int __pad2;

            static if (!__USE_FILE_OFFSET64)
            {
                __blkcnt_t st_blocks;
            }
            else
            {
                __blkcnt64_t st_blocks;
            }

            static if (__USE_MISC)
            {
                __timespec st_atim;
                __timespec st_mtim;
                __timespec st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                __time_t st_atime;
                c_ulong st_atimensec;
                __time_t st_mtime;
                c_ulong st_mtimensec;
                __time_t st_ctime;
                c_ulong st_ctimensec;
            }
            int[2] __unused;
        }
        version (D_LP64)
            static assert(stat_t.sizeof == 128);
        else
            static assert(stat_t.sizeof == 104);
    }
    else version (SPARC64)
    {
        private
        {
            alias __dev_t = ulong;
            alias __ino_t = c_ulong;
            alias __ino64_t = ulong;
            alias __mode_t = uint;
            alias __nlink_t = uint;
            alias __uid_t = uint;
            alias __gid_t = uint;
            alias __off_t = c_long;
            alias __off64_t = long;
            alias __blksize_t = c_long;
            alias __blkcnt_t = c_long;
            alias __blkcnt64_t = long;
            alias __timespec = timespec;
            alias __time_t = time_t;
        }
        struct stat_t
        {
            __dev_t st_dev;
            ushort __pad1;
            __ino_t st_ino;
            __mode_t st_mode;
            __nlink_t st_nlink;
            __uid_t st_uid;
            __gid_t st_gid;
            __dev_t st_rdev;
            ushort __pad2;

            static if (!__USE_FILE_OFFSET64)
            {
                __off_t st_size;
            }
            else
            {
                __off64_t st_size;
            }
            __blksize_t st_blksize;

            static if (!__USE_FILE_OFFSET64)
            {
                __blkcnt_t st_blocks;
            }
            else
            {
                __blkcnt64_t st_blocks;
            }

            static if (__USE_XOPEN2K8)
            {
                __timespec st_atim;
                __timespec st_mtim;
                __timespec st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                __time_t st_atime;
                c_ulong st_atimensec;
                __time_t st_mtime;
                c_ulong st_mtimensec;
                __time_t st_ctime;
                c_ulong st_ctimensec;
            }

            c_ulong __unused4;
            c_ulong __unused5;
        }
        static assert(stat_t.sizeof == 144);
    }
    else version (S390)
    {
        private
        {
            alias __dev_t = ulong;
            alias __ino_t = c_ulong;
            alias __ino64_t = ulong;
            alias __mode_t = uint;
            alias __nlink_t = uint;
            alias __uid_t = uint;
            alias __gid_t = uint;
            alias __off_t = c_long;
            alias __off64_t = long;
            alias __blksize_t = c_long;
            alias __blkcnt_t = c_long;
            alias __blkcnt64_t = long;
            alias __timespec = timespec;
            alias __time_t = time_t;
        }
        struct stat_t
        {
            __dev_t st_dev;
            uint __pad1;
            static if (!__USE_FILE_OFFSET64)
                __ino_t st_ino;
            else
                __ino_t __st_ino;
            __mode_t st_mode;
            __nlink_t st_nlink;
            __uid_t st_uid;
            __gid_t st_gid;
            __dev_t st_rdev;
            uint __pad2;
            static if (!__USE_FILE_OFFSET64)
                __off_t st_size;
            else
                __off64_t st_size;
            __blksize_t st_blksize;
            static if (!__USE_FILE_OFFSET64)
                __blkcnt_t st_blocks;
            else
                __blkcnt64_t st_blocks;
            static if (__USE_XOPEN2K8)
            {
                __timespec st_atim;
                __timespec st_mtim;
                __timespec st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                __time_t st_atime;
                c_ulong st_atimensec;
                __time_t st_mtime;
                c_ulong st_mtimensec;
                __time_t st_ctime;
                c_ulong st_ctimensec;
            }
            static if (!__USE_FILE_OFFSET64)
            {
                c_ulong __glibc_reserved4;
                c_ulong __glibc_reserved5;
            }
            else
                __ino64_t st_ino;
        }
        static if (__USE_FILE_OFFSET64)
            static assert(stat_t.sizeof == 104);
        else
            static assert(stat_t.sizeof == 88);
    }
    else version (SystemZ)
    {
        private
        {
            alias __dev_t = ulong;
            alias __ino_t = c_ulong;
            alias __ino64_t = ulong;
            alias __mode_t = uint;
            alias __nlink_t = ulong;
            alias __uid_t = uint;
            alias __gid_t = uint;
            alias __off_t = c_long;
            alias __off64_t = long;
            alias __blksize_t = c_long;
            alias __blkcnt_t = c_long;
            alias __blkcnt64_t = long;
            alias __timespec = timespec;
            alias __time_t = time_t;
        }
        struct stat_t
        {
            __dev_t st_dev;
            __ino_t st_ino;
            __nlink_t st_nlink;
            __mode_t st_mode;
            __uid_t st_uid;
            __gid_t st_gid;
            int __glibc_reserved0;
            __dev_t st_rdev;
            __off_t st_size;
            static if (__USE_XOPEN2K8)
            {
                __timespec st_atim;
                __timespec st_mtim;
                __timespec st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                __time_t st_atime;
                c_ulong st_atimensec;
                __time_t st_mtime;
                c_ulong st_mtimensec;
                __time_t st_ctime;
                c_ulong st_ctimensec;
            }
            __blksize_t st_blksize;
            __blkcnt_t st_blocks;
            c_long[3] __glibc_reserved;
        }
        static if (__USE_XOPEN2K8)
            static assert(stat_t.sizeof == 144);
        else
            static assert(stat_t.sizeof == 144);
    }
    else
        static assert(0, "unimplemented");

    enum S_IRUSR    = 0x100; // octal 0400
    enum S_IWUSR    = 0x080; // octal 0200
    enum S_IXUSR    = 0x040; // octal 0100
    enum S_IRWXU    = S_IRUSR | S_IWUSR | S_IXUSR;

    enum S_IRGRP    = S_IRUSR >> 3;
    enum S_IWGRP    = S_IWUSR >> 3;
    enum S_IXGRP    = S_IXUSR >> 3;
    enum S_IRWXG    = S_IRWXU >> 3;

    enum S_IROTH    = S_IRGRP >> 3;
    enum S_IWOTH    = S_IWGRP >> 3;
    enum S_IXOTH    = S_IXGRP >> 3;
    enum S_IRWXO    = S_IRWXG >> 3;

    enum S_ISUID    = 0x800; // octal 04000
    enum S_ISGID    = 0x400; // octal 02000
    enum S_ISVTX    = 0x200; // octal 01000

    private
    {
        extern (D) bool S_ISTYPE( mode_t mode, uint mask )
        {
            return ( mode & S_IFMT ) == mask;
        }
    }

    extern (D) bool S_ISBLK( mode_t mode )  { return S_ISTYPE( mode, S_IFBLK );  }
    extern (D) bool S_ISCHR( mode_t mode )  { return S_ISTYPE( mode, S_IFCHR );  }
    extern (D) bool S_ISDIR( mode_t mode )  { return S_ISTYPE( mode, S_IFDIR );  }
    extern (D) bool S_ISFIFO( mode_t mode ) { return S_ISTYPE( mode, S_IFIFO );  }
    extern (D) bool S_ISREG( mode_t mode )  { return S_ISTYPE( mode, S_IFREG );  }
    extern (D) bool S_ISLNK( mode_t mode )  { return S_ISTYPE( mode, S_IFLNK );  }
    extern (D) bool S_ISSOCK( mode_t mode ) { return S_ISTYPE( mode, S_IFSOCK ); }

    static if ( true /*__USE_POSIX199309*/ )
    {
        extern bool S_TYPEISMQ( stat_t* buf )  { return false; }
        extern bool S_TYPEISSEM( stat_t* buf ) { return false; }
        extern bool S_TYPEISSHM( stat_t* buf ) { return false; }
    }

    enum UTIME_NOW = 0x3fffffff;
    enum UTIME_OMIT = 0x3ffffffe;

    int utimensat(int dirfd, const char *pathname,
        ref const(timespec)[2] times, int flags);
    int futimens(int fd, ref const(timespec)[2] times);
}
else version (Darwin)
{
    // _DARWIN_FEATURE_64_BIT_INODE stat is default for Mac OSX >10.5 and is
    // only meaningful type for other OS X/Darwin variants (e.g. iOS).
    // man stat(2) gives details.
    struct stat_t
    {
        dev_t       st_dev;
        mode_t      st_mode;
        nlink_t     st_nlink;
        ino_t       st_ino;
        uid_t       st_uid;
        gid_t       st_gid;
        dev_t       st_rdev;
        union
        {
            struct
            {
                timespec  st_atimespec;
                timespec  st_mtimespec;
                timespec  st_ctimespec;
                timespec  st_birthtimespec;
            }
            struct
            {
                time_t      st_atime;
                c_long      st_atimensec;
                time_t      st_mtime;
                c_long      st_mtimensec;
                time_t      st_ctime;
                c_long      st_ctimensec;
                time_t      st_birthtime;
                c_long      st_birthtimensec;
            }
        }
        off_t       st_size;
        blkcnt_t    st_blocks;
        blksize_t   st_blksize;
        uint        st_flags;
        uint        st_gen;
        int         st_lspare;
        long[2]     st_qspare;
    }

    enum S_IRUSR    = 0x100;  // octal 0400
    enum S_IWUSR    = 0x080;  // octal 0200
    enum S_IXUSR    = 0x040;  // octal 0100
    enum S_IRWXU    = S_IRUSR | S_IWUSR | S_IXUSR;

    enum S_IRGRP    = S_IRUSR >> 3;
    enum S_IWGRP    = S_IWUSR >> 3;
    enum S_IXGRP    = S_IXUSR >> 3;
    enum S_IRWXG    = S_IRWXU >> 3;

    enum S_IROTH    = S_IRGRP >> 3;
    enum S_IWOTH    = S_IWGRP >> 3;
    enum S_IXOTH    = S_IXGRP >> 3;
    enum S_IRWXO    = S_IRWXG >> 3;

    enum S_ISUID    = 0x800; // octal 04000
    enum S_ISGID    = 0x400; // octal 02000
    enum S_ISVTX    = 0x200; // octal 01000

    private
    {
        extern (D) bool S_ISTYPE( mode_t mode, uint mask )
        {
            return ( mode & S_IFMT ) == mask;
        }
    }

    extern (D) bool S_ISBLK( mode_t mode )  { return S_ISTYPE( mode, S_IFBLK );  }
    extern (D) bool S_ISCHR( mode_t mode )  { return S_ISTYPE( mode, S_IFCHR );  }
    extern (D) bool S_ISDIR( mode_t mode )  { return S_ISTYPE( mode, S_IFDIR );  }
    extern (D) bool S_ISFIFO( mode_t mode ) { return S_ISTYPE( mode, S_IFIFO );  }
    extern (D) bool S_ISREG( mode_t mode )  { return S_ISTYPE( mode, S_IFREG );  }
    extern (D) bool S_ISLNK( mode_t mode )  { return S_ISTYPE( mode, S_IFLNK );  }
    extern (D) bool S_ISSOCK( mode_t mode ) { return S_ISTYPE( mode, S_IFSOCK ); }
}
else version (FreeBSD)
{
    // https://github.com/freebsd/freebsd/blob/master/sys/sys/stat.h

    struct stat_t
    {
        dev_t       st_dev;
        ino_t       st_ino;
        mode_t      st_mode;
        nlink_t     st_nlink;
        uid_t       st_uid;
        gid_t       st_gid;
        dev_t       st_rdev;

        time_t      st_atime;
        c_long      __st_atimensec;
        time_t      st_mtime;
        c_long      __st_mtimensec;
        time_t      st_ctime;
        c_long      __st_ctimensec;

        off_t       st_size;
        blkcnt_t    st_blocks;
        blksize_t   st_blksize;
        fflags_t    st_flags;
        uint        st_gen;
        int         st_lspare;

        time_t      st_birthtime;
        c_long      st_birthtimensec;

        ubyte[16 - timespec.sizeof] padding;
    }

    enum S_IRUSR    = 0x100; // octal 0000400
    enum S_IWUSR    = 0x080; // octal 0000200
    enum S_IXUSR    = 0x040; // octal 0000100
    enum S_IRWXU    = 0x1C0; // octal 0000700

    enum S_IRGRP    = 0x020;  // octal 0000040
    enum S_IWGRP    = 0x010;  // octal 0000020
    enum S_IXGRP    = 0x008;  // octal 0000010
    enum S_IRWXG    = 0x038;  // octal 0000070

    enum S_IROTH    = 0x4; // 0000004
    enum S_IWOTH    = 0x2; // 0000002
    enum S_IXOTH    = 0x1; // 0000001
    enum S_IRWXO    = 0x7; // 0000007

    enum S_ISUID    = 0x800; // octal 0004000
    enum S_ISGID    = 0x400; // octal 0002000
    enum S_ISVTX    = 0x200; // octal 0001000

    private
    {
        extern (D) bool S_ISTYPE( mode_t mode, uint mask )
        {
            return ( mode & S_IFMT ) == mask;
        }
    }

    extern (D) bool S_ISBLK( mode_t mode )  { return S_ISTYPE( mode, S_IFBLK );  }
    extern (D) bool S_ISCHR( mode_t mode )  { return S_ISTYPE( mode, S_IFCHR );  }
    extern (D) bool S_ISDIR( mode_t mode )  { return S_ISTYPE( mode, S_IFDIR );  }
    extern (D) bool S_ISFIFO( mode_t mode ) { return S_ISTYPE( mode, S_IFIFO );  }
    extern (D) bool S_ISREG( mode_t mode )  { return S_ISTYPE( mode, S_IFREG );  }
    extern (D) bool S_ISLNK( mode_t mode )  { return S_ISTYPE( mode, S_IFLNK );  }
    extern (D) bool S_ISSOCK( mode_t mode ) { return S_ISTYPE( mode, S_IFSOCK ); }

    enum UTIME_NOW = -1;
    enum UTIME_OMIT = -2;

    // Since FreeBSD 11:
    version (none)
    {
        int utimensat(int dirfd, const char *pathname,
            ref const(timespec)[2] times, int flags);
        int futimens(int fd, ref const(timespec)[2] times);
    }
}
else version (NetBSD)
{
    struct stat_t
    {
        dev_t     st_dev;               /* inode's device */
        mode_t    st_mode;              /* inode protection mode */
        ino_t     st_ino;               /* inode's number */
        nlink_t   st_nlink;             /* number of hard links */
        uid_t     st_uid;               /* user ID of the file's owner */
        gid_t     st_gid;               /* group ID of the file's group */
        dev_t     st_rdev;              /* device type */
        time_t    st_atime;             /* time of last access */
        long      st_atimensec;         /* nsec of last access */
        time_t    st_mtime;             /* time of last data modification */
        long      st_mtimensec;         /* nsec of last data modification */
        time_t    st_ctime;             /* time of last file status change */
        long      st_ctimensec;         /* nsec of last file status change */
        time_t    st_birthtime;         /* time of creation */
        long      st_birthtimensec;     /* nsec of time of creation */
        off_t     st_size;              /* file size, in bytes */
        blkcnt_t  st_blocks;            /* blocks allocated for file */
        blksize_t st_blksize;           /* optimal blocksize for I/O */
        uint32_t  st_flags;             /* user defined flags for file */
        uint32_t  st_gen;               /* file generation number */
        uint32_t[2]  st_spare;
    }

    enum S_IRUSR    = 0x100; // octal 0000400
    enum S_IWUSR    = 0x080; // octal 0000200
    enum S_IXUSR    = 0x040; // octal 0000100
    enum S_IRWXU    = 0x1C0; // octal 0000700

    enum S_IRGRP    = 0x020;  // octal 0000040
    enum S_IWGRP    = 0x010;  // octal 0000020
    enum S_IXGRP    = 0x008;  // octal 0000010
    enum S_IRWXG    = 0x038;  // octal 0000070

    enum S_IROTH    = 0x4; // 0000004
    enum S_IWOTH    = 0x2; // 0000002
    enum S_IXOTH    = 0x1; // 0000001
    enum S_IRWXO    = 0x7; // 0000007

    enum S_ISUID    = 0x800; // octal 0004000
    enum S_ISGID    = 0x400; // octal 0002000
    enum S_ISVTX    = 0x200; // octal 0001000

    private
    {
        extern (D) bool S_ISTYPE( mode_t mode, uint mask )
        {
            return ( mode & S_IFMT ) == mask;
        }
    }

    extern (D) bool S_ISBLK( mode_t mode )  { return S_ISTYPE( mode, S_IFBLK );  }
    extern (D) bool S_ISCHR( mode_t mode )  { return S_ISTYPE( mode, S_IFCHR );  }
    extern (D) bool S_ISDIR( mode_t mode )  { return S_ISTYPE( mode, S_IFDIR );  }
    extern (D) bool S_ISFIFO( mode_t mode ) { return S_ISTYPE( mode, S_IFIFO );  }
    extern (D) bool S_ISREG( mode_t mode )  { return S_ISTYPE( mode, S_IFREG );  }
    extern (D) bool S_ISLNK( mode_t mode )  { return S_ISTYPE( mode, S_IFLNK );  }
    extern (D) bool S_ISSOCK( mode_t mode ) { return S_ISTYPE( mode, S_IFSOCK ); }
}
else version (OpenBSD)
{
    import core.sys.openbsd.sys.cdefs;

    struct stat_t
    {
        mode_t    st_mode;
        dev_t     st_dev;
        ino_t     st_ino;
        nlink_t   st_nlink;
        uid_t     st_uid;
        gid_t     st_gid;
        dev_t     st_rdev;
      static if (__POSIX_VISIBLE >= 200809 || __BSD_VISIBLE)
      {
        timespec  st_atim;
        timespec  st_mtim;
        timespec  st_ctim;
        extern(D)
        {
            @property ref time_t st_atime() { return st_atim.tv_sec; }
            @property ref time_t st_mtime() { return st_mtim.tv_sec; }
            @property ref time_t st_ctime() { return st_ctim.tv_sec; }
        }
      }
      else
      {
        time_t    st_atime;
        long      st_atimensec;
        time_t    st_mtime;
        long      st_mtimensec;
        time_t    st_ctime;
        long      st_ctimensec;
      }
        off_t     st_size;
        blkcnt_t  st_blocks;
        blksize_t st_blksize;
        uint32_t  st_flags;
        uint32_t  st_gen;
      static if (__POSIX_VISIBLE >= 200809 || __BSD_VISIBLE)
      {
        timespec __st_birthtim;
      }
      else
      {
        time_t  __st_birthtime;
        long    __st_birthtimensec;
      }
    }

    enum S_IRUSR    = 0x100; // octal 0000400
    enum S_IWUSR    = 0x080; // octal 0000200
    enum S_IXUSR    = 0x040; // octal 0000100
    enum S_IRWXU    = 0x1C0; // octal 0000700

    enum S_IRGRP    = 0x020;  // octal 0000040
    enum S_IWGRP    = 0x010;  // octal 0000020
    enum S_IXGRP    = 0x008;  // octal 0000010
    enum S_IRWXG    = 0x038;  // octal 0000070

    enum S_IROTH    = 0x4; // 0000004
    enum S_IWOTH    = 0x2; // 0000002
    enum S_IXOTH    = 0x1; // 0000001
    enum S_IRWXO    = 0x7; // 0000007

    enum S_ISUID    = 0x800; // octal 0004000
    enum S_ISGID    = 0x400; // octal 0002000
    enum S_ISVTX    = 0x200; // octal 0001000

    extern (D) bool S_ISBLK(mode_t mode)  { return (mode & S_IFMT) == S_IFBLK;  }
    extern (D) bool S_ISCHR(mode_t mode)  { return (mode & S_IFMT) == S_IFCHR;  }
    extern (D) bool S_ISDIR(mode_t mode)  { return (mode & S_IFMT) == S_IFDIR;  }
    extern (D) bool S_ISFIFO(mode_t mode) { return (mode & S_IFMT) == S_IFIFO;  }
    extern (D) bool S_ISREG(mode_t mode)  { return (mode & S_IFMT) == S_IFREG;  }
    extern (D) bool S_ISLNK(mode_t mode)  { return (mode & S_IFMT) == S_IFLNK;  }
    extern (D) bool S_ISSOCK(mode_t mode) { return (mode & S_IFMT) == S_IFSOCK; }
}
else version (DragonFlyBSD)
{
    struct stat_t {
            ino_t     st_ino;               /* inode's number */
            nlink_t   st_nlink;             /* number of hard links */
            dev_t     st_dev;               /* inode's device */
            mode_t    st_mode;              /* inode protection mode */
            uint16_t  st_padding1;
            uid_t     st_uid;               /* user ID of the file's owner */
            gid_t     st_gid;               /* group ID of the file's group */
            dev_t     st_rdev;              /* device type */
            time_t      st_atime;
            c_long      __st_atimensec;
            time_t      st_mtime;
            c_long      __st_mtimensec;
            time_t      st_ctime;
            c_long      __st_ctimensec;
            off_t     st_size;              /* file size, in bytes */
            int64_t   st_blocks;            /* blocks allocated for file */
            uint32_t  st_blksize;           /* optimal blocksize for I/O */
            uint32_t  st_flags;             /* user defined flags for file */
            uint32_t  st_gen;               /* file generation number */
            int32_t   st_lspare;
            int64_t   st_qspare1;           /* was recursive change detect */
            int64_t   st_qspare2;
    };

    enum S_IRUSR    = 0x100; // octal 0000400
    enum S_IWUSR    = 0x080; // octal 0000200
    enum S_IXUSR    = 0x040; // octal 0000100
    enum S_IRWXU    = 0x1C0; // octal 0000700

    enum S_IRGRP    = 0x020;  // octal 0000040
    enum S_IWGRP    = 0x010;  // octal 0000020
    enum S_IXGRP    = 0x008;  // octal 0000010
    enum S_IRWXG    = 0x038;  // octal 0000070

    enum S_IROTH    = 0x4; // 0000004
    enum S_IWOTH    = 0x2; // 0000002
    enum S_IXOTH    = 0x1; // 0000001
    enum S_IRWXO    = 0x7; // 0000007

    enum S_ISUID    = 0x800; // octal 0004000
    enum S_ISGID    = 0x400; // octal 0002000
    enum S_ISVTX    = 0x200; // octal 0001000

    private
    {
        extern (D) bool S_ISTYPE( mode_t mode, uint mask )
        {
            return ( mode & S_IFMT ) == mask;
        }
    }

    extern (D) bool S_ISBLK( mode_t mode )  { return S_ISTYPE( mode, S_IFBLK );  }
    extern (D) bool S_ISCHR( mode_t mode )  { return S_ISTYPE( mode, S_IFCHR );  }
    extern (D) bool S_ISDIR( mode_t mode )  { return S_ISTYPE( mode, S_IFDIR );  }
    extern (D) bool S_ISFIFO( mode_t mode ) { return S_ISTYPE( mode, S_IFIFO );  }
    extern (D) bool S_ISREG( mode_t mode )  { return S_ISTYPE( mode, S_IFREG );  }
    extern (D) bool S_ISLNK( mode_t mode )  { return S_ISTYPE( mode, S_IFLNK );  }
    extern (D) bool S_ISSOCK( mode_t mode ) { return S_ISTYPE( mode, S_IFSOCK ); }
}
else version (Solaris)
{
    private enum _ST_FSTYPSZ = 16;

    version (D_LP64)
    {
        struct stat_t
        {
            dev_t st_dev;
            ino_t st_ino;
            mode_t st_mode;
            nlink_t st_nlink;
            uid_t st_uid;
            gid_t st_gid;
            dev_t st_rdev;
            off_t st_size;
            union
            {
                timestruc_t st_atim;
                time_t      st_atime;
            }
            union
            {
                timestruc_t st_mtim;
                time_t      st_mtime;
            }
            union
            {
                timestruc_t st_ctim;
                time_t      st_ctime;
            }
            blksize_t st_blksize;
            blkcnt_t st_blocks;
            char[_ST_FSTYPSZ] st_fstype = 0;
        }

        static if (__USE_LARGEFILE64) alias stat_t stat64_t;
    }
    else
    {
        struct stat32_t
        {
            dev_t st_dev;
            c_long[3] st_pad1;
            ino_t st_ino;
            mode_t st_mode;
            nlink_t st_nlink;
            uid_t st_uid;
            gid_t st_gid;
            dev_t st_rdev;
            c_long[2] st_pad2;
            off_t st_size;
            c_long st_pad3;
            union
            {
                timestruc_t st_atim;
                time_t      st_atime;
            }
            union
            {
                timestruc_t st_mtim;
                time_t      st_mtime;
            }
            union
            {
                timestruc_t st_ctim;
                time_t      st_ctime;
            }
            blksize_t st_blksize;
            blkcnt_t st_blocks;
            char[_ST_FSTYPSZ] st_fstype = 0;
            c_long[8] st_pad4;
        }

        struct stat64_t
        {
            dev_t st_dev;
            c_long[3] st_pad1;
            ino64_t st_ino;
            mode_t st_mode;
            nlink_t st_nlink;
            uid_t st_uid;
            gid_t st_gid;
            dev_t st_rdev;
            c_long[2] st_pad2;
            off64_t st_size;
            c_long st_pad3;
            union
            {
                timestruc_t st_atim;
                time_t      st_atime;
            }
            union
            {
                timestruc_t st_mtim;
                time_t      st_mtime;
            }
            union
            {
                timestruc_t st_ctim;
                time_t      st_ctime;
            }
            blksize_t st_blksize;
            blkcnt64_t st_blocks;
            char[_ST_FSTYPSZ] st_fstype = 0;
            c_long[8] st_pad4;
        }

        static if (__USE_FILE_OFFSET64)
            alias stat64_t stat_t;
        else
            alias stat32_t stat_t;

    }

    enum S_IRUSR = 0x100;
    enum S_IWUSR = 0x080;
    enum S_IXUSR = 0x040;
    enum S_IRWXU = 0x1C0;

    enum S_IRGRP = 0x020;
    enum S_IWGRP = 0x010;
    enum S_IXGRP = 0x008;
    enum S_IRWXG = 0x038;

    enum S_IROTH = 0x4; // 0000004
    enum S_IWOTH = 0x2; // 0000002
    enum S_IXOTH = 0x1; // 0000001
    enum S_IRWXO = 0x7; // 0000007

    enum S_ISUID = 0x800;
    enum S_ISGID = 0x400;
    enum S_ISVTX = 0x200;

    private
    {
        extern (D) bool S_ISTYPE(mode_t mode, uint mask)
        {
            return (mode & S_IFMT) == mask;
        }
    }

    extern (D) bool S_ISBLK(mode_t mode) { return S_ISTYPE(mode, S_IFBLK); }
    extern (D) bool S_ISCHR(mode_t mode) { return S_ISTYPE(mode, S_IFCHR); }
    extern (D) bool S_ISDIR(mode_t mode) { return S_ISTYPE(mode, S_IFDIR); }
    extern (D) bool S_ISFIFO(mode_t mode) { return S_ISTYPE(mode, S_IFIFO); }
    extern (D) bool S_ISREG(mode_t mode) { return S_ISTYPE(mode, S_IFREG); }
    extern (D) bool S_ISLNK(mode_t mode) { return S_ISTYPE(mode, S_IFLNK); }
    extern (D) bool S_ISSOCK(mode_t mode) { return S_ISTYPE(mode, S_IFSOCK); }
    extern (D) bool S_ISDOOR(mode_t mode) { return S_ISTYPE(mode, S_IFDOOR); }
    extern (D) bool S_ISPORT(mode_t mode) { return S_ISTYPE(mode, S_IFPORT); }
}
else version (CRuntime_Bionic)
{
    version (X86)
    {
        struct stat_t
        {
            ulong       st_dev;
            ubyte[4]    __pad0;
            c_ulong     __st_ino;
            uint        st_mode;
            uint        st_nlink;
            c_ulong     st_uid;
            c_ulong     st_gid;
            ulong       st_rdev;
            ubyte[4]    __pad3;

            long        st_size;
            c_ulong     st_blksize;
            ulong       st_blocks;
            c_ulong     st_atime;
            c_ulong     st_atime_nsec;
            c_ulong     st_mtime;
            c_ulong     st_mtime_nsec;
            c_ulong     st_ctime;
            c_ulong     st_ctime_nsec;
            ulong       st_ino;
        }
    }
    else version (ARM)
    {
        struct stat_t
        {
            ulong       st_dev;
            ubyte[4]    __pad0;
            c_ulong     __st_ino;
            uint        st_mode;
            uint        st_nlink;
            c_ulong     st_uid;
            c_ulong     st_gid;
            ulong       st_rdev;
            ubyte[4]    __pad3;

            long        st_size;
            c_ulong     st_blksize;
            ulong       st_blocks;
            c_ulong     st_atime;
            c_ulong     st_atime_nsec;
            c_ulong     st_mtime;
            c_ulong     st_mtime_nsec;
            c_ulong     st_ctime;
            c_ulong     st_ctime_nsec;
            ulong       st_ino;
        }
    }
    else version (AArch64)
    {
        struct stat_t
        {
            ulong       st_dev;
            ulong       st_ino;
            uint        st_mode;
            uint        st_nlink;
            uid_t       st_uid;
            gid_t       st_gid;
            ulong       st_rdev;
            ulong       __pad1;

            long        st_size;
            int         st_blksize;
            int         __pad2;
            long        st_blocks;
            long        st_atime;
            ulong       st_atime_nsec;
            long        st_mtime;
            ulong       st_mtime_nsec;
            long        st_ctime;
            ulong       st_ctime_nsec;
            uint        __unused4;
            uint        __unused5;
        }
    }
    else version (X86_64)
    {
        struct stat_t
        {
            ulong       st_dev;
            ulong       st_ino;
            ulong       st_nlink;
            uint        st_mode;
            uid_t       st_uid;
            gid_t       st_gid;
            uint        __pad0;

            ulong       st_rdev;
            long        st_size;
            long        st_blksize;
            long        st_blocks;
            long        st_atime;
            ulong       st_atime_nsec;
            long        st_mtime;
            ulong       st_mtime_nsec;
            long        st_ctime;
            ulong       st_ctime_nsec;
            long[3]     __pad3;
        }
    }
    else
    {
        static assert(false, "Architecture not supported.");
    }

    enum S_IRUSR    = 0x100; // octal 0000400
    enum S_IWUSR    = 0x080; // octal 0000200
    enum S_IXUSR    = 0x040; // octal 0000100
    enum S_IRWXU    = 0x1C0; // octal 0000700

    enum S_IRGRP    = 0x020;  // octal 0000040
    enum S_IWGRP    = 0x010;  // octal 0000020
    enum S_IXGRP    = 0x008;  // octal 0000010
    enum S_IRWXG    = 0x038;  // octal 0000070

    enum S_IROTH    = 0x4; // 0000004
    enum S_IWOTH    = 0x2; // 0000002
    enum S_IXOTH    = 0x1; // 0000001
    enum S_IRWXO    = 0x7; // 0000007

    enum S_ISUID    = 0x800; // octal 0004000
    enum S_ISGID    = 0x400; // octal 0002000
    enum S_ISVTX    = 0x200; // octal 0001000

    private
    {
        extern (D) bool S_ISTYPE( uint mode, uint mask )
        {
            return ( mode & S_IFMT ) == mask;
        }
    }

    extern (D) bool S_ISBLK( uint mode )  { return S_ISTYPE( mode, S_IFBLK );  }
    extern (D) bool S_ISCHR( uint mode )  { return S_ISTYPE( mode, S_IFCHR );  }
    extern (D) bool S_ISDIR( uint mode )  { return S_ISTYPE( mode, S_IFDIR );  }
    extern (D) bool S_ISFIFO( uint mode ) { return S_ISTYPE( mode, S_IFIFO );  }
    extern (D) bool S_ISREG( uint mode )  { return S_ISTYPE( mode, S_IFREG );  }
    extern (D) bool S_ISLNK( uint mode )  { return S_ISTYPE( mode, S_IFLNK );  }
    extern (D) bool S_ISSOCK( uint mode ) { return S_ISTYPE( mode, S_IFSOCK ); }

    // Added since Lollipop
    int utimensat(int dirfd, const char *pathname,
        ref const(timespec)[2] times, int flags);
}
else version (CRuntime_Musl)
{
    alias __mode_t = uint;
    enum {
        S_IRUSR    = 0x100, // octal 0400
        S_IWUSR    = 0x080, // octal 0200
        S_IXUSR    = 0x040, // octal 0100
        S_IRWXU    = S_IRUSR | S_IWUSR | S_IXUSR,

        S_IRGRP    = S_IRUSR >> 3,
        S_IWGRP    = S_IWUSR >> 3,
        S_IXGRP    = S_IXUSR >> 3,
        S_IRWXG    = S_IRWXU >> 3,

        S_IROTH    = S_IRGRP >> 3,
        S_IWOTH    = S_IWGRP >> 3,
        S_IXOTH    = S_IXGRP >> 3,
        S_IRWXO    = S_IRWXG >> 3,

        S_ISUID    = 0x800, // octal 04000
        S_ISGID    = 0x400, // octal 02000
        S_ISVTX    = 0x200, // octal 01000
    }
    version (ARM)
    {
        struct stat_t
        {
            dev_t st_dev;
            int __st_dev_padding;
            c_long __st_ino_truncated;
            mode_t st_mode;
            nlink_t st_nlink;

            uid_t st_uid;
            gid_t st_gid;
            dev_t st_rdev;
            int __st_rdev_padding;
            off_t st_size;
            blksize_t st_blksize;
            blkcnt_t st_blocks;

            timespec st_atim;
            timespec st_mtim;
            timespec st_ctim;
            ino_t st_ino;

            extern(D) @safe @property inout pure nothrow
            {
                ref inout(time_t) st_atime() return { return st_atim.tv_sec; }
                ref inout(time_t) st_mtime() return { return st_mtim.tv_sec; }
                ref inout(time_t) st_ctime() return { return st_ctim.tv_sec; }
            }
        }
    }
    else version (AArch64)
    {
        struct stat_t
        {
            dev_t st_dev;
            ino_t st_ino;
            mode_t st_mode;
            nlink_t st_nlink;

            uid_t st_uid;
            gid_t st_gid;
            dev_t st_rdev;
            c_ulong __pad;
            off_t st_size;
            blksize_t st_blksize;
            int __pad2;
            blkcnt_t st_blocks;

            timespec st_atim;
            timespec st_mtim;
            timespec st_ctim;
            uint[2] __unused;

            extern(D) @safe @property inout pure nothrow
            {
                ref inout(time_t) st_atime() return { return st_atim.tv_sec; }
                ref inout(time_t) st_mtime() return { return st_mtim.tv_sec; }
                ref inout(time_t) st_ctime() return { return st_ctim.tv_sec; }
            }
        }
    }
    else version (X86_64)
    {
        struct stat_t
        {
            dev_t st_dev;
            ino_t st_ino;
            nlink_t st_nlink;

            mode_t st_mode;
            uid_t st_uid;
            gid_t st_gid;
            uint   __pad0;
            dev_t st_rdev;
            off_t st_size;
            blksize_t st_blksize;
            blkcnt_t st_blocks;

            timespec st_atim;
            timespec st_mtim;
            timespec st_ctim;

            c_long[3] __unused;

            extern(D) @safe @property inout pure nothrow
            {
                ref inout(time_t) st_atime() return { return st_atim.tv_sec; }
                ref inout(time_t) st_mtime() return { return st_mtim.tv_sec; }
                ref inout(time_t) st_ctime() return { return st_ctim.tv_sec; }
            }
        }
    }
    else version (X86)
    {
        struct stat_t
        {
            dev_t st_dev;
            int __st_dev_padding;
            c_long __st_ino_truncated;
            mode_t st_mode;
            nlink_t st_nlink;

            uid_t st_uid;
            gid_t st_gid;
            dev_t st_rdev;
            int __st_rdev_padding;
            off_t st_size;
            blksize_t st_blksize;
            blkcnt_t st_blocks;

            timespec st_atim;
            timespec st_mtim;
            timespec st_ctim;
            ino_t st_ino;

            extern(D) @safe @property inout pure nothrow
            {
                ref inout(time_t) st_atime() return { return st_atim.tv_sec; }
                ref inout(time_t) st_mtime() return { return st_mtim.tv_sec; }
                ref inout(time_t) st_ctime() return { return st_ctim.tv_sec; }
            }
        }
    }
    else version (MIPS64)
    {
        struct stat_t
        {
            dev_t st_dev;
            int[3] __pad1;
            ino_t st_ino;
            mode_t st_mode;
            nlink_t st_nlink;

            uid_t st_uid;
            gid_t st_gid;
            dev_t st_rdev;
            uint[2] __pad2;
            off_t st_size;
            int __pad3;

            timespec st_atim;
            timespec st_mtim;
            timespec st_ctim;
            blksize_t st_blksize;
            uint __pad4;
            blkcnt_t st_blocks;
            int[14] __pad5;

            extern(D) @safe @property inout pure nothrow
            {
                ref inout(time_t) st_atime() return { return st_atim.tv_sec; }
                ref inout(time_t) st_mtime() return { return st_mtim.tv_sec; }
                ref inout(time_t) st_ctime() return { return st_ctim.tv_sec; }
            }
        }
    }
    else version (PPC64)
    {
        struct stat_t
        {
            dev_t st_dev;
            ino_t st_ino;
            nlink_t st_nlink;
            mode_t st_mode;

            uid_t st_uid;
            gid_t st_gid;
            dev_t st_rdev;
            off_t st_size;
            blksize_t st_blksize;
            blkcnt_t st_blocks;

            timespec st_atim;
            timespec st_mtim;
            timespec st_ctim;
            c_ulong[3] __unused;

            extern(D) @safe @property inout pure nothrow
            {
                ref inout(time_t) st_atime() return { return st_atim.tv_sec; }
                ref inout(time_t) st_mtime() return { return st_mtim.tv_sec; }
                ref inout(time_t) st_ctime() return { return st_ctim.tv_sec; }
            }
        }
    }
    else version (SystemZ)
    {
        struct stat_t
        {
            dev_t st_dev;
            ino_t st_ino;
            nlink_t st_nlink;
            mode_t st_mode;

            uid_t st_uid;
            gid_t st_gid;
            dev_t st_rdev;
            off_t st_size;

            timespec st_atim;
            timespec st_mtim;
            timespec st_ctim;

            blksize_t st_blksize;
            blkcnt_t st_blocks;
            c_ulong[3] __unused;

            extern(D) @safe @property inout pure nothrow
            {
                ref inout(time_t) st_atime() return { return st_atim.tv_sec; }
                ref inout(time_t) st_mtime() return { return st_mtim.tv_sec; }
                ref inout(time_t) st_ctime() return { return st_ctim.tv_sec; }
            }
        }
    }
    else
        static assert("Unsupported platform");

    private
    {
        extern (D) bool S_ISTYPE( mode_t mode, uint mask )
        {
            return ( mode & S_IFMT ) == mask;
        }
    }

    extern (D) bool S_ISBLK( mode_t mode )  { return S_ISTYPE( mode, S_IFBLK );  }
    extern (D) bool S_ISCHR( mode_t mode )  { return S_ISTYPE( mode, S_IFCHR );  }
    extern (D) bool S_ISDIR( mode_t mode )  { return S_ISTYPE( mode, S_IFDIR );  }
    extern (D) bool S_ISFIFO( mode_t mode ) { return S_ISTYPE( mode, S_IFIFO );  }
    extern (D) bool S_ISREG( mode_t mode )  { return S_ISTYPE( mode, S_IFREG );  }
    extern (D) bool S_ISLNK( mode_t mode )  { return S_ISTYPE( mode, S_IFLNK );  }
    extern (D) bool S_ISSOCK( mode_t mode ) { return S_ISTYPE( mode, S_IFSOCK ); }

    int utimensat(int dirfd, const char *pathname,
        ref const(timespec)[2] times, int flags);
}
else version (CRuntime_UClibc)
{
    version (X86_64)
    {
        struct stat_t
        {
            dev_t       st_dev;
            ino_t       st_ino;
            nlink_t     st_nlink;
            mode_t      st_mode;
            uid_t       st_uid;
            gid_t       st_gid;
            uint        __pad0;
            dev_t       st_rdev;
            off_t       st_size;
            blksize_t   st_blksize;
            blkcnt_t    st_blocks;
            time_t      st_atime;
            ulong_t     st_atimensec;
            time_t      st_mtime;
            ulong_t     st_mtimensec;
            time_t      st_ctime;
            ulong_t     st_ctimensec;
            slong_t[3]     __unused;
        }
    }
    else version (MIPS_O32)
    {
        struct stat_t
        {
            c_ulong     st_dev;
            c_long[3]   st_pad1;
            ino_t       st_ino;
            mode_t      st_mode;
            nlink_t     st_nlink;
            uid_t       st_uid;
            gid_t       st_gid;
            c_ulong     st_rdev;
            static if (!__USE_FILE_OFFSET64)
            {
                c_long[2]   st_pad2;
                off_t       st_size;
                c_long      st_pad3;
            }
            else
            {
                c_long[3]   st_pad2;
                off_t       st_size;
            }
            static if (__USE_MISC || __USE_XOPEN2K8)
            {
                timespec    st_atim;
                timespec    st_mtim;
                timespec    st_ctim;
                extern(D)
                {
                    @property ref time_t st_atime() { return st_atim.tv_sec; }
                    @property ref time_t st_mtime() { return st_mtim.tv_sec; }
                    @property ref time_t st_ctime() { return st_ctim.tv_sec; }
                }
            }
            else
            {
                time_t      st_atime;
                c_ulong     st_atimensec;
                time_t      st_mtime;
                c_ulong     st_mtimensec;
                time_t      st_ctime;
                c_ulong     st_ctimensec;
            }
            blksize_t   st_blksize;
            static if (!__USE_FILE_OFFSET64)
            {
                blkcnt_t    st_blocks;
            }
            else
            {
                c_long      st_pad4;
                blkcnt_t    st_blocks;
            }
            c_long[14]  st_pad5;
        }
    }
    else version (ARM)
    {
        private
        {
            alias __dev_t = ulong;
            alias __ino_t = c_ulong;
            alias __ino64_t = ulong;
            alias __mode_t = uint;
            alias __nlink_t = size_t;
            alias __uid_t = uint;
            alias __gid_t = uint;
            alias __off_t = c_long;
            alias __off64_t = long;
            alias __blksize_t = c_long;
            alias __blkcnt_t = c_long;
            alias __blkcnt64_t = long;
            alias __timespec = timespec;
            alias __time_t = time_t;
        }
        struct stat_t
        {
            __dev_t st_dev;
            ushort __pad1;

            static if (!__USE_FILE_OFFSET64)
            {
                __ino_t st_ino;
            }
            else
            {
                __ino_t __st_ino;
            }
            __mode_t st_mode;
            __nlink_t st_nlink;
            __uid_t st_uid;
            __gid_t st_gid;
            __dev_t st_rdev;
            ushort __pad2;

            static if (!__USE_FILE_OFFSET64)
            {
                __off_t st_size;
            }
            else
            {
                __off64_t st_size;
            }
            __blksize_t st_blksize;

            static if (!__USE_FILE_OFFSET64)
            {
                __blkcnt_t st_blocks;
            }
            else
            {
                __blkcnt64_t st_blocks;
            }

            __time_t st_atime;
            c_ulong st_atimensec;
            __time_t st_mtime;
            c_ulong st_mtimensec;
            __time_t st_ctime;
            c_ulong st_ctimensec;

            static if (!__USE_FILE_OFFSET64)
            {
                c_ulong __unused4;
                c_ulong __unused5;
            }
            else
            {
                __ino64_t st_ino;
            }
        }
        static if (__USE_FILE_OFFSET64)
            static assert(stat_t.sizeof == 104);
        else
            static assert(stat_t.sizeof == 88);
    }
    else
        static assert(0, "unimplemented");

    enum S_IRUSR    = 0x100; // octal 0400
    enum S_IWUSR    = 0x080; // octal 0200
    enum S_IXUSR    = 0x040; // octal 0100
    enum S_IRWXU    = S_IRUSR | S_IWUSR | S_IXUSR;

    enum S_IRGRP    = S_IRUSR >> 3;
    enum S_IWGRP    = S_IWUSR >> 3;
    enum S_IXGRP    = S_IXUSR >> 3;
    enum S_IRWXG    = S_IRWXU >> 3;

    enum S_IROTH    = S_IRGRP >> 3;
    enum S_IWOTH    = S_IWGRP >> 3;
    enum S_IXOTH    = S_IXGRP >> 3;
    enum S_IRWXO    = S_IRWXG >> 3;

    enum S_ISUID    = 0x800; // octal 04000
    enum S_ISGID    = 0x400; // octal 02000
    enum S_ISVTX    = 0x200; // octal 01000

    private
    {
        extern (D) bool S_ISTYPE( mode_t mode, uint mask )
        {
            return ( mode & S_IFMT ) == mask;
        }
    }

    extern (D) bool S_ISBLK( mode_t mode )  { return S_ISTYPE( mode, S_IFBLK );  }
    extern (D) bool S_ISCHR( mode_t mode )  { return S_ISTYPE( mode, S_IFCHR );  }
    extern (D) bool S_ISDIR( mode_t mode )  { return S_ISTYPE( mode, S_IFDIR );  }
    extern (D) bool S_ISFIFO( mode_t mode ) { return S_ISTYPE( mode, S_IFIFO );  }
    extern (D) bool S_ISREG( mode_t mode )  { return S_ISTYPE( mode, S_IFREG );  }
    extern (D) bool S_ISLNK( mode_t mode )  { return S_ISTYPE( mode, S_IFLNK );  }
    extern (D) bool S_ISSOCK( mode_t mode ) { return S_ISTYPE( mode, S_IFSOCK ); }

    static if ( true /*__USE_POSIX199309*/ )
    {
        extern bool S_TYPEISMQ( stat_t* buf )  { return false; }
        extern bool S_TYPEISSEM( stat_t* buf ) { return false; }
        extern bool S_TYPEISSHM( stat_t* buf ) { return false; }
    }

    enum UTIME_NOW = 0x3fffffff;
    enum UTIME_OMIT = 0x3ffffffe;

    int utimensat(int dirfd, const char *pathname,
    ref const(timespec)[2] times, int flags);
    int futimens(int fd, ref const(timespec)[2] times);
}
else
{
    static assert(false, "Unsupported platform");
}

int    chmod(in char*, mode_t);
int    fchmod(int, mode_t);
//int    fstat(int, stat_t*);
//int    lstat(in char*, stat_t*);
int    mkdir(in char*, mode_t);
int    mkfifo(in char*, mode_t);
//int    stat(in char*, stat_t*);
mode_t umask(mode_t);

version (CRuntime_Glibc)
{
  static if ( __USE_LARGEFILE64 )
  {
    int   fstat64(int, stat_t*) @trusted;
    alias fstat64 fstat;

    int   lstat64(in char*, stat_t*);
    alias lstat64 lstat;

    int   stat64(in char*, stat_t*);
    alias stat64 stat;
  }
  else
  {
    int   fstat(int, stat_t*) @trusted;
    int   lstat(in char*, stat_t*);
    int   stat(in char*, stat_t*);
  }
}
else version (Solaris)
{
    version (D_LP64)
    {
        int fstat(int, stat_t*) @trusted;
        int lstat(in char*, stat_t*);
        int stat(in char*, stat_t*);

        static if (__USE_LARGEFILE64)
        {
            alias fstat fstat64;
            alias lstat lstat64;
            alias stat stat64;
        }
    }
    else
    {
        static if (__USE_LARGEFILE64)
        {
            int   fstat64(int, stat_t*) @trusted;
            alias fstat64 fstat;

            int   lstat64(in char*, stat_t*);
            alias lstat64 lstat;

            int   stat64(in char*, stat_t*);
            alias stat64 stat;
        }
        else
        {
            int fstat(int, stat_t*) @trusted;
            int lstat(in char*, stat_t*);
            int stat(in char*, stat_t*);
        }
    }
}
else version (Darwin)
{
    // OS X maintains backwards compatibility with older binaries using 32-bit
    // inode functions by appending $INODE64 to newer 64-bit inode functions.
    version (OSX)
    {
        pragma(mangle, "fstat$INODE64") int fstat(int, stat_t*);
        pragma(mangle, "lstat$INODE64") int lstat(in char*, stat_t*);
        pragma(mangle, "stat$INODE64")  int stat(in char*, stat_t*);
    }
    else
    {
        int fstat(int, stat_t*);
        int lstat(in char*, stat_t*);
        int stat(in char*, stat_t*);
    }
}
else version (FreeBSD)
{
    int   fstat(int, stat_t*);
    int   lstat(in char*, stat_t*);
    int   stat(in char*, stat_t*);
}
else version (NetBSD)
{
    int   __fstat50(int, stat_t*);
    int   __lstat50(in char*, stat_t*);
    int   __stat50(in char*, stat_t*);
    alias __fstat50 fstat;
    alias __lstat50 lstat;
    alias __stat50 stat;
}
else version (OpenBSD)
{
    int   fstat(int, stat_t*);
    int   lstat(in char*, stat_t*);
    int   stat(in char*, stat_t*);
}
else version (DragonFlyBSD)
{
    int   fstat(int, stat_t*);
    int   lstat(in char*, stat_t*);
    int   stat(in char*, stat_t*);
}
else version (CRuntime_Bionic)
{
    int   fstat(int, stat_t*) @trusted;
    int   lstat(in char*, stat_t*);
    int   stat(in char*, stat_t*);
}
else version (CRuntime_Musl)
{
    int stat(in char*, stat_t*);
    int fstat(int, stat_t*);
    int lstat(in char*, stat_t*);

    alias fstat fstat64;
    alias lstat lstat64;
    alias stat stat64;
}
else version (CRuntime_UClibc)
{
  static if ( __USE_LARGEFILE64 )
  {
    int   fstat64(int, stat_t*) @trusted;
    alias fstat64 fstat;

    int   lstat64(in char*, stat_t*);
    alias lstat64 lstat;

    int   stat64(in char*, stat_t*);
    alias stat64 stat;
  }
  else
  {
    int   fstat(int, stat_t*) @trusted;
    int   lstat(in char*, stat_t*);
    int   stat(in char*, stat_t*);
  }
}

//
// Typed Memory Objects (TYM)
//
/*
S_TYPEISTMO(buf)
*/

//
// XOpen (XSI)
//
/*
S_IFMT
S_IFBLK
S_IFCHR
S_IFIFO
S_IFREG
S_IFDIR
S_IFLNK
S_IFSOCK

int mknod(in 3char*, mode_t, dev_t);
*/

version (CRuntime_Glibc)
{
    enum S_IFMT     = 0xF000; // octal 0170000
    enum S_IFBLK    = 0x6000; // octal 0060000
    enum S_IFCHR    = 0x2000; // octal 0020000
    enum S_IFIFO    = 0x1000; // octal 0010000
    enum S_IFREG    = 0x8000; // octal 0100000
    enum S_IFDIR    = 0x4000; // octal 0040000
    enum S_IFLNK    = 0xA000; // octal 0120000
    enum S_IFSOCK   = 0xC000; // octal 0140000

    int mknod(in char*, mode_t, dev_t);
}
else version (Darwin)
{
    enum S_IFMT     = 0xF000; // octal 0170000
    enum S_IFBLK    = 0x6000; // octal 0060000
    enum S_IFCHR    = 0x2000; // octal 0020000
    enum S_IFIFO    = 0x1000; // octal 0010000
    enum S_IFREG    = 0x8000; // octal 0100000
    enum S_IFDIR    = 0x4000; // octal 0040000
    enum S_IFLNK    = 0xA000; // octal 0120000
    enum S_IFSOCK   = 0xC000; // octal 0140000

    int mknod(in char*, mode_t, dev_t);
}
else version (FreeBSD)
{
    enum S_IFMT     = 0xF000; // octal 0170000
    enum S_IFBLK    = 0x6000; // octal 0060000
    enum S_IFCHR    = 0x2000; // octal 0020000
    enum S_IFIFO    = 0x1000; // octal 0010000
    enum S_IFREG    = 0x8000; // octal 0100000
    enum S_IFDIR    = 0x4000; // octal 0040000
    enum S_IFLNK    = 0xA000; // octal 0120000
    enum S_IFSOCK   = 0xC000; // octal 0140000

    int mknod(in char*, mode_t, dev_t);
}
else version (NetBSD)
{
    enum S_IFMT     = 0xF000; // octal 0170000
    enum S_IFBLK    = 0x6000; // octal 0060000
    enum S_IFCHR    = 0x2000; // octal 0020000
    enum S_IFIFO    = 0x1000; // octal 0010000
    enum S_IFREG    = 0x8000; // octal 0100000
    enum S_IFDIR    = 0x4000; // octal 0040000
    enum S_IFLNK    = 0xA000; // octal 0120000
    enum S_IFSOCK   = 0xC000; // octal 0140000

    int mknod(in char*, mode_t, dev_t);
}
else version (OpenBSD)
{
    enum S_IFMT     = 0xF000; // octal 0170000
    enum S_IFBLK    = 0x6000; // octal 0060000
    enum S_IFCHR    = 0x2000; // octal 0020000
    enum S_IFIFO    = 0x1000; // octal 0010000
    enum S_IFREG    = 0x8000; // octal 0100000
    enum S_IFDIR    = 0x4000; // octal 0040000
    enum S_IFLNK    = 0xA000; // octal 0120000
    enum S_IFSOCK   = 0xC000; // octal 0140000

    int mknod(in char*, mode_t, dev_t);
}
else version (DragonFlyBSD)
{
    enum S_IFMT     = 0xF000; // octal 0170000
    enum S_IFBLK    = 0x6000; // octal 0060000
    enum S_IFCHR    = 0x2000; // octal 0020000
    enum S_IFIFO    = 0x1000; // octal 0010000
    enum S_IFREG    = 0x8000; // octal 0100000
    enum S_IFDIR    = 0x4000; // octal 0040000
    enum S_IFLNK    = 0xA000; // octal 0120000
    enum S_IFSOCK   = 0xC000; // octal 0140000

    int mknod(in char*, mode_t, dev_t);
}
else version (Solaris)
{
    enum S_IFMT = 0xF000;
    enum S_IFBLK = 0x6000;
    enum S_IFCHR = 0x2000;
    enum S_IFIFO = 0x1000;
    enum S_IFREG = 0x8000;
    enum S_IFDIR = 0x4000;
    enum S_IFLNK = 0xA000;
    enum S_IFSOCK = 0xC000;
    enum S_IFDOOR = 0xD000;
    enum S_IFPORT = 0xE000;

    int mknod(in char*, mode_t, dev_t);
}
else version (CRuntime_Bionic)
{
    enum S_IFMT     = 0xF000; // octal 0170000
    enum S_IFBLK    = 0x6000; // octal 0060000
    enum S_IFCHR    = 0x2000; // octal 0020000
    enum S_IFIFO    = 0x1000; // octal 0010000
    enum S_IFREG    = 0x8000; // octal 0100000
    enum S_IFDIR    = 0x4000; // octal 0040000
    enum S_IFLNK    = 0xA000; // octal 0120000
    enum S_IFSOCK   = 0xC000; // octal 0140000

    int mknod(in char*, mode_t, dev_t);
}
else version (CRuntime_Musl)
{
    enum {
        S_IFMT     = 0xF000, // octal 0170000
        S_IFBLK    = 0x6000, // octal 0060000
        S_IFCHR    = 0x2000, // octal 0020000
        S_IFIFO    = 0x1000, // octal 0010000
        S_IFREG    = 0x8000, // octal 0100000
        S_IFDIR    = 0x4000, // octal 0040000
        S_IFLNK    = 0xA000, // octal 0120000
        S_IFSOCK   = 0xC000, // octal 0140000
    }

    int mknod(in char*, mode_t, dev_t);
}
else version (CRuntime_UClibc)
{
    enum S_IFMT     = 0xF000; // octal 0170000
    enum S_IFBLK    = 0x6000; // octal 0060000
    enum S_IFCHR    = 0x2000; // octal 0020000
    enum S_IFIFO    = 0x1000; // octal 0010000
    enum S_IFREG    = 0x8000; // octal 0100000
    enum S_IFDIR    = 0x4000; // octal 0040000
    enum S_IFLNK    = 0xA000; // octal 0120000
    enum S_IFSOCK   = 0xC000; // octal 0140000

    int mknod(in char*, mode_t, dev_t);
}
else
{
    static assert(false, "Unsupported platform");
}
