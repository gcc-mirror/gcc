/**
 * D header file for NetBSD
 *
 * Authors: Martin Nowak
 *
 * http://cvsweb.netbsd.org/bsdweb.cgi/~checkout~/src/sys/sys/mman.h
 */
module core.sys.netbsd.sys.mman;

version (NetBSD):
extern (C):
nothrow:

public import core.sys.posix.sys.mman;
import core.sys.posix.sys.types;

enum __BSD_VISIBLE = true;

static if (__BSD_VISIBLE)
{
    enum INHERIT_SHARE = 0;
    enum INHERIT_COPY = 1;
    enum INHERIT_NONE = 2;
    enum INHERIT_DONATE_COPY = 3;
    enum INHERIT_ZERO = 4;
}

// already in core.sys.posix.sys.mman
// enum PROT_NONE = 0x00;
// enum PROT_READ = 0x01;
// enum PROT_WRITE = 0x02;
// enum PROT_EXEC = 0x04;
// enum MAP_SHARED = 0x0001;
// enum MAP_PRIVATE = 0x0002;
static if (__BSD_VISIBLE)
    enum MAP_COPY = 0x0002;
// enum MAP_FIXED = 0x0010;

static if (__BSD_VISIBLE)
{
    enum MAP_RENAME = 0x0020;
    enum MAP_NORESERVE = 0x0040;
    enum MAP_HASSEMAPHORE = 0x0200;
    enum MAP_STACK = 0x2000;
    enum MAP_WIRED = 0x0800;

    enum MAP_FILE = 0x0000;

    // already in core.sys.posix.sys.mman
    // enum MAP_ANON = 0x1000;
    //#ifndef _KERNEL
    alias MAP_ANONYMOUS = MAP_ANON;
    //#endif /* !_KERNEL */


    extern(D) int MAP_ALIGNED(int n) { return n << MAP_ALIGNMENT_SHIFT; }
    enum MAP_ALIGNMENT_SHIFT = 24;
    enum MAP_ALIGNMENT_MASK = MAP_ALIGNED(0xff);
}

//static if (__POSIX_VISIBLE >= 199309)
//{
    // already in core.sys.posix.sys.mman
    // enum MCL_CURRENT = 0x0001;
    // enum MCL_FUTURE = 0x0002;
//}

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
    enum MADV_SPACEAVAIL = 5;
    enum MADV_FREE = 6;
}

//static if (__POSIX_VISIBLE >= 200112)
//{
    // already in core.sys.posix.sys.mman
    // alias POSIX_MADV_NORMAL = _MADV_NORMAL;
    // alias POSIX_MADV_RANDOM = _MADV_RANDOM;
    // alias POSIX_MADV_SEQUENTIAL = _MADV_SEQUENTIAL;
    // alias POSIX_MADV_WILLNEED = _MADV_WILLNEED;
    // alias POSIX_MADV_DONTNEED = _MADV_DONTNEED;
//}

static if (__BSD_VISIBLE)
{
    //int getpagesizes(size_t *, int);
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
//static if (__POSIX_VISIBLE >= 200112)
    // int posix_madvise(void *, size_t, int);
//static if (__POSIX_VISIBLE >= 199309)
//{
    // int mlockall(int);
    // int munlockall();
    // int shm_open(const(char) *, int, mode_t);
    // int shm_unlink(const(char) *);
//}
