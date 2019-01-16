/**
 * D header file for DragonFlyBSD
 *
 * Authors:   Martin Nowak,Diederik de Groot(port:DragonFlyBSD)
 * Copied:  From core/sys/freebsd/sys
 */
module core.sys.dragonflybsd.sys.mman;

version (DragonFlyBSD):
extern (C) nothrow @nogc @system:

public import core.sys.posix.sys.mman;
import core.sys.dragonflybsd.sys.cdefs;
import core.sys.posix.sys.types;

enum INHERIT_SHARE        = 0;
enum INHERIT_COPY         = 1;
enum INHERIT_NONE         = 2;

// already in core.sys.posix.sys.mman
// enum PROT_NONE             = 0x00;
// enum PROT_READ             = 0x01;
// enum PROT_WRITE            = 0x02;
// enum PROT_EXEC             = 0x04;
// enum MAP_SHARED            = 0x0001;
// enum MAP_PRIVATE           = 0x0002;
alias MAP_COPY            = MAP_PRIVATE;

enum MAP_FIXED                = 0x00010;
enum MAP_RENAME           = 0x00020;
enum MAP_NORESERVE        = 0x00040;
enum MAP_INHERIT          = 0x00080;
enum MAP_NOEXTEND         = 0x00100;
enum MAP_HASSEMAPHORE     = 0x00200;
enum MAP_STACK            = 0x00400;
enum MAP_NOSYNC           = 0x00800;

enum MAP_FILE             = 0x00000;
//enum MAP_ANON           = 0x01000;        // already in core.sys.posix.sys.mman
alias MAP_ANONYMOUS       = MAP_ANON;
enum MAP_VPAGETABLE       = 0x02000;

enum MAP_TRYFIXED         = 0x10000;
enum MAP_NOCORE           = 0x20000;
enum MAP_SIZEALIGN        = 0x40000;

//enum MAP_EXCL           = 0x00004000;
//enum MAP_NOCORE         = 0x00020000;
//enum MAP_PREFAULT_READ  = 0x00040000;
//version (LP64)
//    enum MAP_32BIT      = 0x00080000;

//extern(D) int MAP_ALIGNED(int n) { return n << MAP_ALIGNMENT_SHIFT; }
//enum MAP_ALIGNMENT_SHIFT = 24;
//enum MAP_ALIGNMENT_MASK = MAP_ALIGNED(0xff);
//enum MAP_ALIGNED_SUPER = MAP_ALIGNED(1);

// enum MCL_CURRENT       = 0x0001;           // already in core.sys.posix.sys.mman
// enum MCL_FUTURE        = 0x0002;            // already in core.sys.posix.sys.mman

// already in core.sys.posix.sys.mman
enum MAP_FAILED = cast(void*)-1;
// enum MS_SYNC               = 0x0000;         // already in core.sys.posix.sys.mman
// enum MS_ASYNC              = 0x0001;         // already in core.sys.posix.sys.mman
// enum MS_INVALIDATE         = 0x0002;         // already in core.sys.posix.sys.mman

enum _MADV_NORMAL             = 0;
enum _MADV_RANDOM             = 1;
enum _MADV_SEQUENTIAL         = 2;
enum _MADV_WILLNEED           = 3;
enum _MADV_DONTNEED           = 4;

alias MADV_NORMAL         = _MADV_NORMAL;
alias MADV_RANDOM         = _MADV_RANDOM;
alias MADV_SEQUENTIAL     = _MADV_SEQUENTIAL;
alias MADV_WILLNEED       = _MADV_WILLNEED;
alias MADV_DONTNEED       = _MADV_DONTNEED;
enum MADV_FREE            = 5;
enum MADV_NOSYNC          = 6;
enum MADV_AUTOSYNC        = 7;
enum MADV_NOCORE          = 8;
enum MADV_CORE            = 9;
enum MADV_INVAL           = 10;
enum MADV_SETMAP          = 11;

alias MADV_CONTROL_START  = MADV_INVAL;
alias MADV_CONTROL_END    = MADV_SETMAP;

enum MINCORE_INCORE       = 0x1;
enum MINCORE_REFERENCED   = 0x2;
enum MINCORE_MODIFIED     = 0x4;
enum MINCORE_REFERENCED_OTHER = 0x8;
enum MINCORE_MODIFIED_OTHER   = 0x10;
enum MINCORE_SUPER        = 0x20;

enum SHM_ANON             = cast(const(char) *)1;

// already in core.sys.posix.sys.mman
// alias POSIX_MADV_NORMAL    = _MADV_NORMAL;
// alias POSIX_MADV_RANDOM    = _MADV_RANDOM;
// alias POSIX_MADV_SEQUENTIAL= _MADV_SEQUENTIAL;
// alias POSIX_MADV_WILLNEED  = _MADV_WILLNEED;
// alias POSIX_MADV_DONTNEED  = _MADV_DONTNEED;

int getpagesizes(size_t *, int);
int madvise(void *, size_t, int);
int mincore(const(void) *, size_t, char *);
int minherit(void *, size_t, int);

// already in core.sys.posix.sys.mman
// int mlock(const void *, size_t);
// void * mmap(void *, size_t, int, int, int, off_t);
// int mprotect(const void *, size_t, int);
// int msync(void *, size_t, int);
// int munlock(const void *, size_t);
// int munmap(void *, size_t);
// int posix_madvise(void *, size_t, int);

// int mlockall(int);
// int munlockall();
// int shm_open(const(char) *, int, mode_t);
// int shm_unlink(const(char) *);

// int madvise(void *, size_t, int);
// int mcontrol(void *, size_t, int, off_t);
// int mincore(const void *, size_t, char *);
// int minherit(void *, size_t, int);
