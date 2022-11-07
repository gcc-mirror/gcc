/**
 * D header file for OpenBSD
 *
 * Authors:  Iain Buclaw
 * Based-on: core/sys/freebsd/sys
 */
module core.sys.openbsd.sys.mman;

version (OpenBSD):
extern (C):
nothrow:

public import core.sys.posix.sys.mman;
import core.sys.openbsd.sys.cdefs;
import core.sys.posix.sys.types;

// already in core.sys.posix.sys.mman
// enum MAP_SHARED = 0x0001;
// enum MAP_PRIVATE = 0x0002;
// enum MAP_FIXED = 0x0010;
// enum MAP_ANON = 0x1000;
// enum MAP_STACK = 0x4000;

alias MAP_ANONYMOUS = MAP_ANON;
enum MAP_CONCEAL = 0x8000;

enum MAP_FLAGMASK = 0xfff7;

alias MAP_COPY = MAP_PRIVATE;
enum MAP_FILE = 0;
enum MAP_HASSEMAPHORE = 0;
enum MAP_INHERIT = 0;
enum MAP_NOEXTEND = 0;
enum MAP_NORESERVE = 0;
enum MAP_RENAME = 0;
enum MAP_TRYFIXED = 0;

static if (__BSD_VISIBLE)
{
    alias MADV_NORMAL = POSIX_MADV_NORMAL;
    alias MADV_RANDOM = POSIX_MADV_RANDOM;
    alias MADV_SEQUENTIAL = POSIX_MADV_SEQUENTIAL;
    alias MADV_WILLNEED = POSIX_MADV_WILLNEED;
    alias MADV_DONTNEED = POSIX_MADV_DONTNEED;
    enum MADV_SPACEAVAIL = 5;
    enum MADV_FREE = 6;

    int madvise(void *, size_t, int);
    int minherit(void *, size_t, int);
    int mimmutable(void *, size_t);
    void* mquery(void *, size_t, int, int, int, off_t);
}
