/++
    D header file correspoding to sys/statvfs.h.

    Copyright: Copyright 2012 -
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Robert Klotzner and $(HTTP jmdavisprog.com, Jonathan M Davis)
    Standards: $(HTTP http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_statvfs.h.html,
                      The Open Group Base Specifications Issue 7 IEEE Std 1003.1, 2018 Edition)
 +/
module core.sys.posix.sys.statvfs;
private import core.stdc.config;
private import core.sys.posix.config;
public import core.sys.posix.sys.types;

version (Posix):
extern (C) :
nothrow:
@nogc:

version (CRuntime_Glibc) {
    static if (__WORDSIZE == 32)
    {
        version=_STATVFSBUF_F_UNUSED;
    }
    struct statvfs_t
    {
        c_ulong f_bsize;
        c_ulong f_frsize;
        fsblkcnt_t f_blocks;
        fsblkcnt_t f_bfree;
        fsblkcnt_t f_bavail;
        fsfilcnt_t f_files;
        fsfilcnt_t f_ffree;
        fsfilcnt_t f_favail;
        c_ulong f_fsid;
        version (_STATVFSBUF_F_UNUSED)
        {
            int __f_unused;
        }
        c_ulong f_flag;
        c_ulong f_namemax;
        int[6] __f_spare;
    }
    /* Definitions for the flag in `f_flag'.  These definitions should be
      kept in sync with the definitions in <sys/mount.h>.  */
    static if (__USE_GNU)
    {
        enum FFlag
        {
            ST_RDONLY = 1,        /* Mount read-only.  */
            ST_NOSUID = 2,
            ST_NODEV = 4,         /* Disallow access to device special files.  */
            ST_NOEXEC = 8,        /* Disallow program execution.  */
            ST_SYNCHRONOUS = 16,      /* Writes are synced at once.  */
            ST_MANDLOCK = 64,     /* Allow mandatory locks on an FS.  */
            ST_WRITE = 128,       /* Write on file/directory/symlink.  */
            ST_APPEND = 256,      /* Append-only file.  */
            ST_IMMUTABLE = 512,       /* Immutable file.  */
            ST_NOATIME = 1024,        /* Do not update access times.  */
            ST_NODIRATIME = 2048,     /* Do not update directory access times.  */
            ST_RELATIME = 4096        /* Update atime relative to mtime/ctime.  */

        }
    }  /* Use GNU.  */
    else
    { // Posix defined:
        enum FFlag
        {
            ST_RDONLY = 1,        /* Mount read-only.  */
            ST_NOSUID = 2
        }
    }

    static if ( __USE_FILE_OFFSET64 )
    {
        int statvfs64 (const char * file, statvfs_t* buf);
        alias statvfs64 statvfs;

        int fstatvfs64 (int fildes, statvfs_t *buf) @trusted;
        alias fstatvfs64 fstatvfs;
    }
    else
    {
        int statvfs (const char * file, statvfs_t* buf);
        int fstatvfs (int fildes, statvfs_t *buf);
    }

}
else version (NetBSD)
{
    enum  _VFS_MNAMELEN = 1024;
    enum  _VFS_NAMELEN = 32;

    struct fsid_t
    {
       int[2] __fsid_val;
    }

    struct statvfs_t
    {
        c_ulong f_flag;
        c_ulong f_bsize;
        c_ulong f_frsize;
        c_ulong f_iosize;
        fsblkcnt_t f_blocks;
        fsblkcnt_t f_bfree;
        fsblkcnt_t f_bavail;
        fsblkcnt_t f_bresvd;
        fsfilcnt_t f_files;
        fsfilcnt_t f_ffree;
        fsfilcnt_t f_favail;
        fsfilcnt_t f_fresvd;
        ulong f_syncreads;
        ulong f_syncwrites;
        ulong f_asyncreads;
        ulong f_asyncwrites;
        fsid_t f_fsidx;
        c_ulong f_fsid;
        c_ulong f_namemax;
        int f_owner;
        int[4] f_spare;
        char[_VFS_NAMELEN] f_fstypename = 0;
        char[_VFS_MNAMELEN] f_mntonname = 0;
        char[_VFS_MNAMELEN] f_mntfromname = 0;
    }

    enum FFlag
    {
        ST_RDONLY = 1,        /* Mount read-only.  */
        ST_NOSUID = 2
    }

    int statvfs (const char * file, statvfs_t* buf);
    int fstatvfs (int fildes, statvfs_t *buf) @trusted;
}
else version (FreeBSD)
{
    import core.sys.freebsd.sys.mount;

    // @@@DEPRECATED_2.091@@@
    deprecated("Moved to core.sys.freebsd.sys.mount to correspond to C header file sys/mount.h")
    alias MFSNAMELEN = core.sys.freebsd.sys.mount.MFSNAMELEN;

    // @@@DEPRECATED_2.091@@@
    deprecated("Moved to core.sys.freebsd.sys.mount to correspond to C header file sys/mount.h")
    alias MNAMELEN = core.sys.freebsd.sys.mount.MNAMELEN;

    // @@@DEPRECATED_2.091@@@
    deprecated("Moved to core.sys.freebsd.sys.mount to correspond to C header file sys/mount.h")
    alias fsid_t = core.sys.freebsd.sys.mount.fsid_t;

    // @@@DEPRECATED_2.091@@@
    deprecated("Moved to core.sys.freebsd.sys.mount to correspond to C header file sys/mount.h")
    alias statfs_t = core.sys.freebsd.sys.mount.statfs_t;

    // @@@DEPRECATED_2.091@@@
    deprecated("Values moved to core.sys.freebsd.sys.mount to correspond to C header file sys/mount.h")
    enum FFlag
    {
        // @@@DEPRECATED_2.091@@@
        MNT_RDONLY = 1,          /* read only filesystem */

        // @@@DEPRECATED_2.091@@@
        MNT_SYNCHRONOUS = 2,     /* fs written synchronously */

        // @@@DEPRECATED_2.091@@@
        MNT_NOEXEC = 4,          /* can't exec from filesystem */

        // @@@DEPRECATED_2.091@@@
        MNT_NOSUID  = 8,         /* don't honor setuid fs bits */

        // @@@DEPRECATED_2.091@@@
        MNT_NFS4ACLS = 16,       /* enable NFS version 4 ACLs */

        // @@@DEPRECATED_2.091@@@
        MNT_UNION = 32,          /* union with underlying fs */

        // @@@DEPRECATED_2.091@@@
        MNT_ASYNC = 64,          /* fs written asynchronously */

        // @@@DEPRECATED_2.091@@@
        MNT_SUIDDIR = 128,       /* special SUID dir handling */

        // @@@DEPRECATED_2.091@@@
        MNT_SOFTDEP = 256,       /* using soft updates */

        // @@@DEPRECATED_2.091@@@
        MNT_NOSYMFOLLOW = 512,   /* do not follow symlinks */

        // @@@DEPRECATED_2.091@@@
        MNT_GJOURNAL = 1024,     /* GEOM journal support enabled */

        // @@@DEPRECATED_2.091@@@
        MNT_MULTILABEL = 2048,   /* MAC support for objects */

        // @@@DEPRECATED_2.091@@@
        MNT_ACLS = 4096,         /* ACL support enabled */

        // @@@DEPRECATED_2.091@@@
        MNT_NOATIME = 8192,      /* dont update file access time */

        // @@@DEPRECATED_2.091@@@
        MNT_NOCLUSTERR = 16384,  /* disable cluster read */

        // @@@DEPRECATED_2.091@@@
        MNT_NOCLUSTERW = 32768,  /* disable cluster write */

        // @@@DEPRECATED_2.091@@@
        MNT_SUJ = 65536,         /* using journaled soft updates */

        // @@@DEPRECATED_2.091@@@
        MNT_AUTOMOUNTED = 131072 /* mounted by automountd(8) */
    }

    deprecated("Moved to core.sys.freebsd.sys.mount to correspond to C header file sys/mount.h")
    alias statfs = core.sys.freebsd.sys.mount.statfs;

    deprecated("Moved to core.sys.freebsd.sys.mount to correspond to C header file sys/mount.h")
    alias fstatfs = core.sys.freebsd.sys.mount.fstatfs;

    struct statvfs_t
    {
        fsblkcnt_t f_bavail;
        fsblkcnt_t f_bfree;
        fsblkcnt_t f_blocks;
        fsfilcnt_t f_favail;
        fsfilcnt_t f_ffree;
        fsfilcnt_t f_files;
        ulong f_bsize;
        ulong f_flag;
        ulong f_frsize;
        ulong f_fsid;
        ulong f_namemax;
    }

    enum uint ST_RDONLY = 0x1;
    enum uint ST_NOSUID = 0x2;

    int fstatvfs(int, statvfs_t*);
    int statvfs(const char*, statvfs_t*);
}
else
{
    struct statvfs_t
    {
        c_ulong f_bsize;
        c_ulong f_frsize;
        fsblkcnt_t f_blocks;
        fsblkcnt_t f_bfree;
        fsblkcnt_t f_bavail;
        fsfilcnt_t f_files;
        fsfilcnt_t f_ffree;
        fsfilcnt_t f_favail;
        c_ulong f_fsid;
        c_ulong f_flag;
        c_ulong f_namemax;
    }

    enum FFlag
    {
        ST_RDONLY = 1,        /* Mount read-only.  */
        ST_NOSUID = 2
    }

    int statvfs (const char * file, statvfs_t* buf);
    int fstatvfs (int fildes, statvfs_t *buf) @trusted;
}
