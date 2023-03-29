/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly,
              Alex Rønne Petersn
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.dirent;

import core.sys.posix.config;
public import core.sys.posix.sys.types; // for ino_t

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

//
// Required
//
/*
struct dirent
{
    char[] d_name;
}
*/

version (linux)
{
    struct dirent
    {
        ino_t       d_ino;
        off_t       d_off;
        ushort      d_reclen;
        ubyte       d_type;
        char[256]   d_name = 0;
    }
}
else version (Darwin)
{
    // _DARWIN_FEATURE_64_BIT_INODE dirent is default for Mac OSX >10.5 and is
    // only meaningful type for other OS X/Darwin variants (e.g. iOS).
    // man dir(5) has some info, man stat(2) gives details.
    struct dirent
    {
        ino_t       d_ino;
        alias       d_fileno = d_ino;
        ulong       d_seekoff;
        ushort      d_reclen;
        ushort      d_namlen;
        ubyte       d_type;
        char[1024]  d_name = 0;
    }
}
else version (FreeBSD)
{
    import core.sys.freebsd.config;

    static if (__FreeBSD_version >= 1200000)
    {
        struct dirent
        {
            ino_t     d_fileno;
            off_t     d_off;
            ushort    d_reclen;
            ubyte     d_type;
            ubyte     d_pad0;
            ushort    d_namlen;
            ushort    d_pad1;
            char[256] d_name = 0;
        }
    }
    else
    {
        align(4)
        struct dirent
        {
            uint      d_fileno;
            ushort    d_reclen;
            ubyte     d_type;
            ubyte     d_namlen;
            char[256] d_name = 0;
        }
    }
}
else version (NetBSD)
{
    struct dirent
    {
        ulong      d_fileno;
        ushort    d_reclen;
        ushort    d_namlen;
        ubyte     d_type;
        char[512] d_name = 0;
    }
}
else version (OpenBSD)
{
    align(4)
    struct dirent
    {
        ino_t     d_fileno;
        off_t     d_off;
        ushort    d_reclen;
        ubyte     d_type;
        ubyte     d_namlen;
        ubyte[4]  __d_padding;
        char[256] d_name = 0;
    }
}
else version (DragonFlyBSD)
{
    struct dirent
    {
        ino_t     d_fileno;       /* file number of entry */
        ushort    d_reclen;       /* strlen(d_name) */
        ubyte     d_type;         /* file type, see blow */
        ubyte     d_unused1;      /* padding, reserved */
        uint      d_unused2;      /* reserved */
        char[256] d_name = 0;     /* name, NUL-terminated */
    }
}
else version (Solaris)
{
    struct dirent
    {
        ino_t d_ino;
        off_t d_off;
        ushort d_reclen;
        char[1] d_name = 0;
    }
}
else
{
    static assert(false, "Unsupported platform");
}

/*
DIR

int     closedir(DIR*);
DIR*    opendir(const scope char*);
dirent* readdir(DIR*);
void    rewinddir(DIR*);
*/

version (CRuntime_Glibc)
{
    // NOTE: The following constants are non-standard Linux definitions
    //       for dirent.d_type.
    enum
    {
        DT_UNKNOWN  = 0,
        DT_FIFO     = 1,
        DT_CHR      = 2,
        DT_DIR      = 4,
        DT_BLK      = 6,
        DT_REG      = 8,
        DT_LNK      = 10,
        DT_SOCK     = 12,
        DT_WHT      = 14
    }

    struct DIR
    {
        // Managed by OS
    }

    static if ( __USE_FILE_OFFSET64 )
    {
        dirent* readdir64(DIR*);
        alias   readdir64 readdir;
    }
    else
    {
        dirent* readdir(DIR*);
    }
}
else version (Darwin)
{
    enum
    {
        DT_UNKNOWN  = 0,
        DT_FIFO     = 1,
        DT_CHR      = 2,
        DT_DIR      = 4,
        DT_BLK      = 6,
        DT_REG      = 8,
        DT_LNK      = 10,
        DT_SOCK     = 12,
        DT_WHT      = 14
    }

    struct DIR
    {
        // Managed by OS
    }

    // OS X maintains backwards compatibility with older binaries using 32-bit
    // inode functions by appending $INODE64 to newer 64-bit inode functions.
    // Other Darwin variants (iOS, TVOS, WatchOS) only support 64-bit inodes,
    // no suffix needed
    version (OSX)
    {
        version (AArch64)
            dirent* readdir(DIR*);
        else
            pragma(mangle, "readdir$INODE64") dirent* readdir(DIR*);
    }
    else
        dirent* readdir(DIR*);
}
else version (FreeBSD)
{
    import core.sys.freebsd.config;

    // https://github.com/freebsd/freebsd/blob/master/sys/sys/dirent.h
    enum
    {
        DT_UNKNOWN  = 0,
        DT_FIFO     = 1,
        DT_CHR      = 2,
        DT_DIR      = 4,
        DT_BLK      = 6,
        DT_REG      = 8,
        DT_LNK      = 10,
        DT_SOCK     = 12,
        DT_WHT      = 14
    }

    alias void* DIR;

    version (GNU)
    {
        dirent* readdir(DIR*);
    }
    else
    {
        static if (__FreeBSD_version >= 1200000)
            pragma(mangle, "readdir@FBSD_1.5") dirent* readdir(DIR*);
        else
            pragma(mangle, "readdir@FBSD_1.0") dirent* readdir(DIR*);
    }
}
else version (NetBSD)
{
    enum
    {
        DT_UNKNOWN  = 0,
        DT_FIFO     = 1,
        DT_CHR      = 2,
        DT_DIR      = 4,
        DT_BLK      = 6,
        DT_REG      = 8,
        DT_LNK      = 10,
        DT_SOCK     = 12,
        DT_WHT      = 14
    }

    alias void* DIR;

    dirent* __readdir30(DIR*);
    alias __readdir30 readdir;
}
else version (OpenBSD)
{
    enum
    {
        DT_UNKNOWN  = 0,
        DT_FIFO     = 1,
        DT_CHR      = 2,
        DT_DIR      = 4,
        DT_BLK      = 6,
        DT_REG      = 8,
        DT_LNK      = 10,
        DT_SOCK     = 12,
    }

    alias void* DIR;

    dirent* readdir(DIR*);
}
else version (DragonFlyBSD)
{
    enum
    {
        DT_UNKNOWN  = 0,
        DT_FIFO     = 1,
        DT_CHR      = 2,
        DT_DIR      = 4,
        DT_BLK      = 6,
        DT_REG      = 8,
        DT_LNK      = 10,
        DT_SOCK     = 12,
        DT_WHT      = 14,
        DT_DBF      = 15,         /* database record file */
    }

    alias void* DIR;

    dirent* readdir(DIR*);
}
else version (Solaris)
{
    struct DIR
    {
        int dd_fd;
        int dd_loc;
        int dd_size;
        char* dd_buf;
    }

    version (D_LP64)
    {
        dirent* readdir(DIR*);
        alias readdir64 = readdir;
    }
    else
    {
        static if (__USE_LARGEFILE64)
        {
            dirent* readdir64(DIR*);
            alias readdir64 readdir;
        }
        else
        {
            dirent* readdir(DIR*);
        }
    }
}
else version (CRuntime_Bionic)
{
    enum
    {
        DT_UNKNOWN  = 0,
        DT_FIFO     = 1,
        DT_CHR      = 2,
        DT_DIR      = 4,
        DT_BLK      = 6,
        DT_REG      = 8,
        DT_LNK      = 10,
        DT_SOCK     = 12,
        DT_WHT      = 14
    }

    struct DIR
    {
    }

    dirent* readdir(DIR*);
}
else version (CRuntime_Musl)
{
    enum
    {
        DT_UNKNOWN  = 0,
        DT_FIFO     = 1,
        DT_CHR      = 2,
        DT_DIR      = 4,
        DT_BLK      = 6,
        DT_REG      = 8,
        DT_LNK      = 10,
        DT_SOCK     = 12,
        DT_WHT      = 14
    }

    struct DIR
    {
    }

    static if ( __USE_FILE_OFFSET64 )
    {
        dirent* readdir64(DIR*);
        alias   readdir64 readdir;
    }
    else
    {
        dirent* readdir(DIR*);
    }
}
else version (CRuntime_UClibc)
{
    // NOTE: The following constants are non-standard Linux definitions
    //       for dirent.d_type.
    enum
    {
        DT_UNKNOWN  = 0,
        DT_FIFO     = 1,
        DT_CHR      = 2,
        DT_DIR      = 4,
        DT_BLK      = 6,
        DT_REG      = 8,
        DT_LNK      = 10,
        DT_SOCK     = 12,
        DT_WHT      = 14
    }

    struct DIR
    {
        // Managed by OS
    }

    static if ( __USE_FILE_OFFSET64 )
    {
        dirent* readdir64(DIR*);
        alias   readdir64 readdir;
    }
    else
    {
        dirent* readdir(DIR*);
    }
}
else
{
    static assert(false, "Unsupported platform");
}

// Only OS X out of the Darwin family needs special treatment.  Other Darwins
// (iOS, TVOS, WatchOS) are fine with normal symbol names for these functions
// in else below.
version (OSX)
{
    version (AArch64)
    {
        int     closedir(DIR*);
        DIR*    opendir(const scope char*);
        void    rewinddir(DIR*);
    }
    else version (D_LP64)
    {
        int closedir(DIR*);
        pragma(mangle, "opendir$INODE64")   DIR* opendir(const scope char*);
        pragma(mangle, "rewinddir$INODE64") void rewinddir(DIR*);
    }
    else
    {
        // 32-bit mangles __DARWIN_UNIX03 specific functions with $UNIX2003 to
        // maintain backward compatibility with binaries build pre 10.5
        pragma(mangle, "closedir$UNIX2003")          int closedir(DIR*);
        pragma(mangle, "opendir$INODE64$UNIX2003")   DIR* opendir(const scope char*);
        pragma(mangle, "rewinddir$INODE64$UNIX2003") void rewinddir(DIR*);
    }
}
else version (NetBSD)
{
    int     closedir(DIR*);
    DIR*    __opendir30(const scope char*);
    alias __opendir30 opendir;
    void    rewinddir(DIR*);
}
else
{
    int     closedir(DIR*);
    DIR*    opendir(const scope char*);
    //dirent* readdir(DIR*);
    void    rewinddir(DIR*);
}

//
// Thread-Safe Functions (TSF)
//
/*
int readdir_r(DIR*, dirent*, dirent**);
*/

version (CRuntime_Glibc)
{
  static if ( __USE_LARGEFILE64 )
  {
    int   readdir64_r(DIR*, dirent*, dirent**);
    alias readdir64_r readdir_r;
  }
  else
  {
    int readdir_r(DIR*, dirent*, dirent**);
  }
}
else version (Darwin)
{
    version (OSX)
        pragma(mangle, "readdir_r$INODE64") int readdir_r(DIR*, dirent*, dirent**);
    else
        int readdir_r(DIR*, dirent*, dirent**);
}
else version (FreeBSD)
{
    version (GNU)
    {
        int readdir_r(DIR*, dirent*, dirent**);
    }
    else
    {
        static if (__FreeBSD_version >= 1200000)
            pragma(mangle, "readdir_r@FBSD_1.5") int readdir_r(DIR*, dirent*, dirent**);
        else
            pragma(mangle, "readdir_r@FBSD_1.0") int readdir_r(DIR*, dirent*, dirent**);
    }
}
else version (DragonFlyBSD)
{
    int readdir_r(DIR*, dirent*, dirent**);
}
else version (NetBSD)
{
    int __readdir_r30(DIR*, dirent*, dirent**);
    alias __readdir_r30 readdir_r;
}
else version (OpenBSD)
{
    int readdir_r(DIR*, dirent*, dirent**);
}
else version (Solaris)
{
    static if (__USE_LARGEFILE64)
    {
        int readdir64_r(DIR*, dirent*, dirent**);
        alias readdir64_r readdir_r;
    }
    else
    {
        int readdir_r(DIR*, dirent*, dirent**);
    }
}
else version (CRuntime_Bionic)
{
    int readdir_r(DIR*, dirent*, dirent**);
}
else version (CRuntime_Musl)
{
    int readdir_r(DIR*, dirent*, dirent**);
}
else version (CRuntime_UClibc)
{
  static if ( __USE_LARGEFILE64 )
  {
    int   readdir64_r(DIR*, dirent*, dirent**);
    alias readdir64_r readdir_r;
  }
  else
  {
    int readdir_r(DIR*, dirent*, dirent**);
  }
}
else
{
    static assert(false, "Unsupported platform");
}

//
// XOpen (XSI)
//
/*
void   seekdir(DIR*, c_long);
c_long telldir(DIR*);
*/

version (CRuntime_Glibc)
{
    void   seekdir(DIR*, c_long);
    c_long telldir(DIR*);
}
else version (FreeBSD)
{
    version (GNU)
    {
        void seekdir(DIR*, c_long);
        c_long telldir(DIR*);
    }
    else
    {
        pragma(mangle, "seekdir@@FBSD_1.0") void seekdir(DIR*, c_long);
        pragma(mangle, "telldir@@FBSD_1.0") c_long telldir(DIR*);
    }
}
else version (NetBSD)
{
    void   seekdir(DIR*, c_long);
    c_long telldir(DIR*);
}
else version (OpenBSD)
{
    void   seekdir(DIR*, c_long);
    c_long telldir(DIR*);
}
else version (DragonFlyBSD)
{
    void   seekdir(DIR*, c_long);
    c_long telldir(DIR*);
}
else version (Darwin)
{
    version (OSX)
    {
        version (D_LP64)
        {
            pragma(mangle, "seekdir$INODE64") void seekdir(DIR*, c_long);
            pragma(mangle, "telldir$INODE64") c_long telldir(DIR*);
        }
        else
        {
            // 32-bit mangles __DARWIN_UNIX03 specific functions with $UNIX2003 to
            // maintain backward compatibility with binaries build pre 10.5
            pragma(mangle, "seekdir$INODE64$UNIX2003") void seekdir(DIR*, c_long);
            pragma(mangle, "telldir$INODE64$UNIX2003") c_long telldir(DIR*);
        }
    }
    else // other Darwins (e.g. iOS, TVOS, WatchOS)
    {
        void seekdir(DIR*, c_long);
        c_long telldir(DIR*);
    }
}
else version (Solaris)
{
    c_long telldir(DIR*);
    void seekdir(DIR*, c_long);
}
else version (CRuntime_Bionic)
{
}
else version (CRuntime_Musl)
{
    void   seekdir(DIR*, c_long);
    c_long telldir(DIR*);
}
else version (CRuntime_UClibc)
{
    void   seekdir(DIR*, c_long);
    c_long telldir(DIR*);
}
else
{
    static assert(false, "Unsupported platform");
}
