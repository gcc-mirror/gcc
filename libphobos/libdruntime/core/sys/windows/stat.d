
/// $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
/// Author: Walter Bright

module core.sys.windows.stat;
version (Windows):

extern (C) nothrow @nogc:

import core.sys.windows.stdc.time;

// Posix version is in core.sys.posix.sys.stat

enum S_IFMT   = 0xF000;
enum S_IFDIR  = 0x4000;
enum S_IFCHR  = 0x2000;
enum S_IFIFO  = 0x1000;
enum S_IFREG  = 0x8000;
enum S_IREAD  = 0x0100;
enum S_IWRITE = 0x0080;
enum S_IEXEC  = 0x0040;
enum S_IFBLK  = 0x6000;
enum S_IFNAM  = 0x5000;

@safe pure
{
int S_ISREG(int m)  { return (m & S_IFMT) == S_IFREG; }
int S_ISBLK(int m)  { return (m & S_IFMT) == S_IFBLK; }
int S_ISNAM(int m)  { return (m & S_IFMT) == S_IFNAM; }
int S_ISDIR(int m)  { return (m & S_IFMT) == S_IFDIR; }
int S_ISCHR(int m)  { return (m & S_IFMT) == S_IFCHR; }
}

version (CRuntime_DigitalMars)
{
    struct struct_stat
    {
        short st_dev;
        ushort st_ino;
        ushort st_mode;
        short st_nlink;
        ushort st_uid;
        ushort st_gid;
        short st_rdev;
        short dummy;
        int st_size;
        time_t st_atime;
        time_t st_mtime;
        time_t st_ctime;
    }

    int stat(const(char)*, struct_stat *);
    int fstat(int, struct_stat *) @trusted;
    int _wstat(const(wchar)*, struct_stat *);
}
else version (CRuntime_Microsoft)
{
    struct struct_stat
    {
        uint st_dev;
        ushort st_ino;
        ushort st_mode;
        short st_nlink;
        short st_uid;
        short st_gid;
        uint st_rdev;
        int st_size;
        time_t st_atime;
        time_t st_mtime;
        time_t st_ctime;
    }

    // These assume time_t is 32 bits (which druntime's definition currently is)
    // Add pragma(mangle) to use _stat64 etc. when time_t is made 64-bit
    // See also: https://issues.dlang.org/show_bug.cgi?id=21134
    int stat(const(char)*, struct_stat *);
    int fstat(int, struct_stat *) @trusted;
    int _wstat(const(wchar)*, struct_stat *);
}
