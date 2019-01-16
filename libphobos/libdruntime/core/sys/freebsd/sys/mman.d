/**
 * D header file for FreeBSD
 *
 * Authors: Martin Nowak
 */
module core.sys.freebsd.sys.mman;

version (FreeBSD):
extern (C):
nothrow:

public import core.sys.posix.sys.mman;
import core.sys.freebsd.sys.cdefs;
import core.sys.posix.sys.types;
// https://svnweb.freebsd.org/base/head/sys/sys/mman.h?revision=270825&view=markup

static if (__BSD_VISIBLE)
{
    enum INHERIT_SHARE = 0;
    enum INHERIT_COPY = 1;
    enum INHERIT_NONE = 2;
}

// already in core.sys.posix.sys.mman
// enum PROT_NONE = 0x00;
// enum PROT_READ = 0x01;
// enum PROT_WRITE = 0x02;
// enum PROT_EXEC = 0x04;
// enum MAP_SHARED = 0x0001;
// enum MAP_PRIVATE = 0x0002;
static if (__BSD_VISIBLE)
    alias MAP_COPY = MAP_PRIVATE;
// enum MAP_FIXED = 0x0010;

static if (__BSD_VISIBLE)
{
    enum MAP_RENAME = 0x0020;
    enum MAP_NORESERVE = 0x0040;
    enum MAP_RESERVED0080 = 0x0080;
    enum MAP_RESERVED0100 = 0x0100;
    enum MAP_HASSEMAPHORE = 0x0200;
    enum MAP_STACK = 0x0400;
    enum MAP_NOSYNC = 0x0800;

    enum MAP_FILE = 0x0000;

    // already in core.sys.posix.sys.mman
    // enum MAP_ANON = 0x1000;
    //#ifndef _KERNEL
    alias MAP_ANONYMOUS = MAP_ANON;
    //#endif /* !_KERNEL */

    enum MAP_EXCL = 0x00004000;
    enum MAP_NOCORE = 0x00020000;
    enum MAP_PREFAULT_READ = 0x00040000;
    version (LP64)
        enum MAP_32BIT = 0x00080000;


    extern(D) int MAP_ALIGNED(int n) { return n << MAP_ALIGNMENT_SHIFT; }
    enum MAP_ALIGNMENT_SHIFT = 24;
    enum MAP_ALIGNMENT_MASK = MAP_ALIGNED(0xff);
    enum MAP_ALIGNED_SUPER = MAP_ALIGNED(1);
}

static if (__POSIX_VISIBLE >= 199309)
{
    // already in core.sys.posix.sys.mman
    // enum MCL_CURRENT = 0x0001;
    // enum MCL_FUTURE = 0x0002;
}

// already in core.sys.posix.sys.mman
enum MAP_FAILED = cast(void*)-1;

// already in core.sys.posix.sys.mman
// enum MS_SYNC = 0x0000;
// enum MS_ASYNC = 0x0001;
// enum MS_INVALIDATE = 0x0002;

enum _MADV_NORMAL = 0;
enum _MADV_RANDOM = 1;
enum _MADV_SEQUENTIAL = 2;
enum _MADV_WILLNEED = 3;
enum _MADV_DONTNEED = 4;

static if (__BSD_VISIBLE)
{
    alias MADV_NORMAL = _MADV_NORMAL;
    alias MADV_RANDOM = _MADV_RANDOM;
    alias MADV_SEQUENTIAL = _MADV_SEQUENTIAL;
    alias MADV_WILLNEED = _MADV_WILLNEED;
    alias MADV_DONTNEED = _MADV_DONTNEED;
    enum MADV_FREE = 5;
    enum MADV_NOSYNC = 6;
    enum MADV_AUTOSYNC = 7;
    enum MADV_NOCORE = 8;
    enum MADV_CORE = 9;
    enum MADV_PROTECT = 10;

    enum MINCORE_INCORE = 0x1;
    enum MINCORE_REFERENCED = 0x2;
    enum MINCORE_MODIFIED = 0x4;
    enum MINCORE_REFERENCED_OTHER = 0x8;
    enum MINCORE_MODIFIED_OTHER = 0x10;
    enum MINCORE_SUPER = 0x20;

    enum SHM_ANON = cast(const(char) *)1;
}

static if (__POSIX_VISIBLE >= 200112)
{
    // already in core.sys.posix.sys.mman
    // alias POSIX_MADV_NORMAL = _MADV_NORMAL;
    // alias POSIX_MADV_RANDOM = _MADV_RANDOM;
    // alias POSIX_MADV_SEQUENTIAL = _MADV_SEQUENTIAL;
    // alias POSIX_MADV_WILLNEED = _MADV_WILLNEED;
    // alias POSIX_MADV_DONTNEED = _MADV_DONTNEED;
}

static if (__BSD_VISIBLE)
{
    int getpagesizes(size_t *, int);
    int madvise(void *, size_t, int);
    int mincore(const(void) *, size_t, char *);
    int minherit(void *, size_t, int);
}
// already in core.sys.posix.sys.mman
// int mlock(const void *, size_t);
// void *  mmap(void *, size_t, int, int, int, off_t);
// int mprotect(const void *, size_t, int);
// int msync(void *, size_t, int);
// int munlock(const void *, size_t);
// int munmap(void *, size_t);
static if (__POSIX_VISIBLE >= 200112)
    // int posix_madvise(void *, size_t, int);
static if (__POSIX_VISIBLE >= 199309)
{
    // int mlockall(int);
    // int munlockall();
    // int shm_open(const(char) *, int, mode_t);
    // int shm_unlink(const(char) *);
}
