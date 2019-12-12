/**
 * D header file for Darwin
 *
 * Authors: Martin Nowak
 */
module core.sys.darwin.sys.mman;

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

public import core.sys.posix.sys.mman;
import core.sys.darwin.sys.cdefs;
import core.sys.posix.sys.types;

// already in core.sys.posix.sys.mman
// enum PROT_NONE = 0x00;
// enum PROT_READ = 0x01;
// enum PROT_WRITE = 0x02;
// enum PROT_EXEC = 0x04;

// already in core.sys.posix.sys.mman
// enum MAP_SHARED = 0x0001;
// enum MAP_PRIVATE = 0x0002;
static if (_DARWIN_C_SOURCE)
    alias MAP_COPY = MAP_PRIVATE;
// enum MAP_FIXED = 0x0010;

static if (_DARWIN_C_SOURCE)
{
     enum MAP_RENAME = 0x0020;
     enum MAP_NORESERVE = 0x0040;
     enum MAP_RESERVED0080 = 0x0080;
     enum MAP_NOEXTEND = 0x0100;
     enum MAP_HASSEMAPHORE = 0x0200;
     enum MAP_NOCACHE = 0x0400;
     enum MAP_JIT = 0x0800;
}

// already in core.sys.posix.sys.mman
// enum MCL_CURRENT = 0x0001;
// enum MCL_FUTURE = 0x0002;

// enum MAP_FAILED = cast(void*)-1;

// enum MS_ASYNC = 0x0001;
// enum MS_INVALIDATE = 0x0002;
// enum MS_SYNC = 0x0010;

static if (_DARWIN_C_SOURCE)
{
    enum MS_KILLPAGES = 0x0004;
    enum MS_DEACTIVATE = 0x0008;

    enum MAP_FILE = 0x0000;

// already in core.sys.posix.sys.mman
// enum MAP_ANON = 0x1000;

// enum POSIX_MADV_NORMAL = 0;
// enum POSIX_MADV_RANDOM = 1;
// enum POSIX_MADV_SEQUENTIAL = 2;
// enum POSIX_MADV_WILLNEED = 3;
// enum POSIX_MADV_DONTNEED = 4;

    alias MADV_NORMAL = POSIX_MADV_NORMAL;
    alias MADV_RANDOM = POSIX_MADV_RANDOM;
    alias MADV_SEQUENTIAL = POSIX_MADV_SEQUENTIAL;
    alias MADV_WILLNEED = POSIX_MADV_WILLNEED;
    alias MADV_DONTNEED = POSIX_MADV_DONTNEED;
    enum MADV_FREE = 5;
    enum MADV_ZERO_WIRED_PAGES = 6;
    enum MADV_FREE_REUSABLE = 7;
    enum MADV_FREE_REUSE = 8;
    enum MADV_CAN_REUSE = 9;

    enum MINCORE_INCORE = 0x1;
    enum MINCORE_REFERENCED = 0x2;
    enum MINCORE_MODIFIED = 0x4;
    enum MINCORE_REFERENCED_OTHER = 0x8;
    enum MINCORE_MODIFIED_OTHER = 0x10;
}

// already in core.sys.posix.sys.mman
// int mlockall(int);
// int munlockall(void);
// int mlock(const void *, size_t);
// void *  mmap(void *, size_t, int, int, int, off_t);
// int mprotect(void *, size_t, int);
// int msync(void *, size_t, int);
// int munlock(const void *, size_t);
// int munmap(void *, size_t);
// int shm_open(const char *, int, ...);
// int shm_unlink(const char *);
// int posix_madvise(void *, size_t, int);

static if (_DARWIN_C_SOURCE)
{
    int madvise(void *, size_t, int);
    int mincore(const(void)*, size_t, char *);
    int minherit(void *, size_t, int);
}
