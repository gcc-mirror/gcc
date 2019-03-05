/**
 * D header file for GNU/Linux
 *
 * Authors: Martin Nowak
 */
module core.sys.linux.sys.mman;

version (linux):
extern (C):
nothrow:

version (ARM)     version = ARM_Any;
version (AArch64) version = ARM_Any;
version (HPPA)    version = HPPA_Any;
version (HPPA64)  version = HPPA_Any;
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

public import core.sys.posix.sys.mman;
import core.sys.linux.config;

// <bits/mman.h>
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/powerpc/bits/mman.h
version (PPC_Any)
{
    enum PROT_SAO = 0x10;

    static if (__USE_MISC) enum
    {
        MAP_GROWSDOWN = 0x00100,
        MAP_DENYWRITE = 0x00800,
        MAP_EXECUTABLE = 0x01000,
        MAP_LOCKED = 0x00080,
        MAP_NORESERVE = 0x00040,
        MAP_POPULATE = 0x08000,
        MAP_NONBLOCK = 0x10000,
        MAP_STACK = 0x20000,
        MAP_HUGETLB = 0x40000,
    }

    // in core.sys.posix.sys.mman
    // enum
    // {
    //     MCL_CURRENT = 0x2000,
    //     MCL_FUTURE = 0x4000,
    // }
}
// https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/riscv/bits/mman.h
else version (RISCV_Any)
{
    static if (__USE_MISC) enum
    {
        MAP_GROWSDOWN = 0x00100,
        MAP_DENYWRITE = 0x00800,
        MAP_EXECUTABLE = 0x01000,
        MAP_LOCKED = 0x02000,
        MAP_NORESERVE = 0x04000,
        MAP_POPULATE = 0x08000,
        MAP_NONBLOCK = 0x10000,
        MAP_STACK = 0x20000,
        MAP_HUGETLB = 0x40000,
        MAP_SYNC = 0x80000,
        MAP_FIXED_NOREPLACE = 0x100000,
    }

    // in core.sys.posix.sys.mman
    // enum
    // {
    //     MCL_CURRENT = 0x2000,
    //     MCL_FUTURE = 0x4000,
    // }
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/s390/bits/mman.h
else version (IBMZ_Any)
{
    static if (__USE_MISC) enum
    {
        MAP_GROWSDOWN = 0x00100,
        MAP_DENYWRITE = 0x00800,
        MAP_EXECUTABLE = 0x01000,
        MAP_LOCKED = 0x02000,
        MAP_NORESERVE = 0x04000,
        MAP_POPULATE = 0x08000,
        MAP_NONBLOCK = 0x10000,
        MAP_STACK = 0x20000,
        MAP_HUGETLB = 0x40000,
    }
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/sh/bits/mman.h
else version (SH)
{
    static if (__USE_MISC) enum
    {
        MAP_GROWSDOWN = 0x0100,
        MAP_DENYWRITE = 0x0800,
        MAP_EXECUTABLE = 0x1000,
        MAP_LOCKED = 0x2000,
        MAP_NORESERVE = 0x4000,
        MAP_POPULATE = 0x8000,
        MAP_NONBLOCK = 0x10000,
        MAP_STACK = 0x20000,
        MAP_HUGETLB = 0x40000,
    }
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/sparc/bits/mman.h
else version (SPARC_Any)
{
    static if (__USE_MISC) enum
    {
        MAP_GROWSDOWN = 0x0200,
        MAP_DENYWRITE = 0x0800,
        MAP_EXECUTABLE = 0x1000,
        MAP_LOCKED = 0x0100,
        MAP_NORESERVE = 0x0040,
        _MAP_NEW = 0x80000000,
        MAP_POPULATE = 0x8000,
        MAP_NONBLOCK = 0x10000,
        MAP_STACK = 0x20000,
        MAP_HUGETLB = 0x40000,
    }

    // in core.sys.posix.sys.mman
    // enum
    // {
    //     MCL_CURRENT = 0x2000,
    //     MCL_FUTURE = 0x4000,
    // }
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/x86/bits/mman.h
else version (X86_Any)
{
    static if (__USE_MISC) enum MAP_32BIT = 0x40;

    static if (__USE_MISC) enum
    {
        MAP_GROWSDOWN = 0x00100,
        MAP_DENYWRITE = 0x00800,
        MAP_EXECUTABLE = 0x01000,
        MAP_LOCKED = 0x02000,
        MAP_NORESERVE = 0x04000,
        MAP_POPULATE = 0x08000,
        MAP_NONBLOCK = 0x10000,
        MAP_STACK = 0x20000,
        MAP_HUGETLB = 0x40000,
    }
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/aarch64/bits/mman.h
else version (AArch64)
{
    static if (__USE_MISC) enum
    {
        MAP_GROWSDOWN = 0x00100,
        MAP_DENYWRITE = 0x00800,
        MAP_EXECUTABLE = 0x01000,
        MAP_LOCKED = 0x02000,
        MAP_NORESERVE = 0x04000,
        MAP_POPULATE = 0x08000,
        MAP_NONBLOCK = 0x10000,
        MAP_STACK = 0x20000,
        MAP_HUGETLB = 0x40000,
    }
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/alpha/bits/mman.h
else version (Alpha)
{
    enum
    {
        PROT_READ = 0x1,
        PROT_WRITE = 0x2,
        PROT_EXEC = 0x4,
        PROT_NONE = 0x0,
        PROT_GROWSDOWN = 0x01000000,
        PROT_GROWSUP = 0x02000000,
    }

    enum MAP_SHARED = 0x01;
    enum MAP_PRIVATE = 0x02;
    static if (__USE_MISC)
        enum MAP_TYPE = 0x0f;

    enum MAP_FIXED = 0x10;
    static if (__USE_MISC) enum
    {
        MAP_FILE = 0,
        MAP_ANONYMOUS = MAP_ANON,
        // in core.sys.posix.sys.mman
        // MAP_ANON = MAP_ANONYMOUS,
        MAP_HUGE_SHIFT = 26,
        MAP_HUGE_MASK = 0x3f,
    }

    static if (__USE_MISC) enum
    {
        MAP_GROWSDOWN = 0x01000,
        MAP_DENYWRITE = 0x02000,
        MAP_EXECUTABLE = 0x04000,
        MAP_LOCKED = 0x08000,
        MAP_NORESERVE = 0x10000,
        MAP_POPULATE = 0x20000,
        MAP_NONBLOCK = 0x40000,
        MAP_STACK = 0x80000,
        MAP_HUGETLB = 0x100000,
    }

    // in core.sys.posix.sys.mman
    // enum
    // {
    //     MS_ASYNC = 1,
    //     MS_SYNC = 2,
    //     MS_INVALIDATE = 4,
    // }

    // in core.sys.posix.sys.mman
    // enum
    // {
    //     MCL_CURRENT = 8192,
    //     MCL_FUTURE = 16384,
    // }

    static if (__USE_GNU) enum
    {
        MREMAP_MAYMOVE = 1,
        MREMAP_FIXED = 2,
    }

    static if (__USE_MISC) enum
    {
        MADV_NORMAL = 0,
        MADV_RANDOM = 1,
        MADV_SEQUENTIAL = 2,
        MADV_WILLNEED = 3,
        MADV_DONTNEED = 6,
        MADV_REMOVE = 9,
        MADV_DONTFORK = 10,
        MADV_DOFORK = 11,
        MADV_MERGEABLE = 12,
        MADV_UNMERGEABLE = 13,
        MADV_HUGEPAGE = 14,
        MADV_NOHUGEPAGE = 15,
        MADV_DONTDUMP = 16,
        MADV_DODUMP = 17,
        MADV_HWPOISON = 100,
    }

    // in core.sys.posix.sys.mman
    // static if (__USE_XOPEN2K) enum
    // {
    //         POSIX_MADV_NORMAL = 0,
    //         POSIX_MADV_RANDOM = 1,
    //         POSIX_MADV_SEQUENTIAL = 2,
    //         POSIX_MADV_WILLNEED = 3,
    //         POSIX_MADV_DONTNEED = 6,
    // }
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/arm/bits/mman.h
else version (ARM)
{
    static if (__USE_MISC) enum
    {
        MAP_GROWSDOWN = 0x00100,
        MAP_DENYWRITE = 0x00800,
        MAP_EXECUTABLE = 0x01000,
        MAP_LOCKED = 0x02000,
        MAP_NORESERVE = 0x04000,
        MAP_POPULATE = 0x08000,
        MAP_NONBLOCK = 0x10000,
        MAP_STACK = 0x20000,
        MAP_HUGETLB = 0x40000,
    }
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/hppa/bits/mman.h
else version (HPPA_Any)
{
    enum
    {
        PROT_READ = 0x1,
        PROT_WRITE = 0x2,
        PROT_EXEC = 0x4,
        PROT_NONE = 0x0,
        PROT_GROWSDOWN = 0x01000000,
        PROT_GROWSUP = 0x02000000,
    }

    enum MAP_SHARED = 0x01;
    enum MAP_PRIVATE = 0x02;
    static if (__USE_MISC)
        enum MAP_TYPE = 0x0f;

    enum MAP_FIXED = 0x04;
    static if (__USE_MISC) enum
    {
        MAP_FILE = 0,
        MAP_ANONYMOUS = MAP_ANON,
        // in core.sys.posix.sys.mman
        // MAP_ANON = MAP_ANONYMOUS,
        MAP_VARIABLE = 0,
        MAP_HUGE_SHIFT = 26,
        MAP_HUGE_MASK = 0x3f,
    }

    static if (__USE_MISC) enum
    {
        MAP_DENYWRITE = 0x0800,
        MAP_EXECUTABLE = 0x1000,
        MAP_LOCKED = 0x2000,
        MAP_NORESERVE = 0x4000,
        MAP_GROWSDOWN = 0x8000,
        MAP_POPULATE = 0x10000,
        MAP_NONBLOCK = 0x20000,
    }

    // in core.sys.posix.sys.mman
    // enum
    // {
    //     MS_ASYNC = 1,
    //     MS_SYNC = 2,
    //     MS_INVALIDATE = 4,
    // }

    // in core.sys.posix.sys.mman
    // enum
    // {
    //     MCL_CURRENT = 1,
    //     MCL_FUTURE = 2,
    // }

    static if (__USE_GNU) enum
    {
        MREMAP_MAYMOVE = 1,
        MREMAP_FIXED = 2,
    }

    static if (__USE_MISC) enum
    {
        MADV_NORMAL = 0,
        MADV_RANDOM = 1,
        MADV_SEQUENTIAL = 2,
        MADV_WILLNEED = 3,
        MADV_DONTNEED = 4,
        MADV_SPACEAVAIL = 5,
        MADV_VPS_PURGE = 6,
        MADV_VPS_INHERIT = 7,
        MADV_REMOVE = 9,
        MADV_DONTFORK = 10,
        MADV_DOFORK = 11,
        MADV_MERGEABLE = 65,
        MADV_UNMERGEABLE = 66,
        MADV_HUGEPAGE = 67,
        MADV_NOHUGEPAGE = 68,
        MADV_DONTDUMP = 69,
    }

    deprecated("MADV_*_PAGES are gone and never had any effect") enum
    {
        MADV_4K_PAGES = 12,
        MADV_16K_PAGES = 14,
        MADV_64K_PAGES = 16,
        MADV_256K_PAGES = 18,
        MADV_1M_PAGES = 20,
        MADV_4M_PAGES = 22,
        MADV_16M_PAGES = 24,
        MADV_64M_PAGES = 26,
    }

    // in core.sys.posix.sys.mman
    // static if (__USE_XOPEN2K) enum
    // {
    //     POSIX_MADV_NORMAL = 0,
    //     POSIX_MADV_RANDOM = 1,
    //     POSIX_MADV_SEQUENTIAL = 2,
    //     POSIX_MADV_WILLNEED = 3,
    //     POSIX_MADV_DONTNEED = 4,
    // }
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/ia64/bits/mman.h
else version (IA64)
{
    static if (__USE_MISC) enum
    {
        MAP_GROWSDOWN = 0x00100,
        MAP_GROWSUP = 0x00200,
        MAP_DENYWRITE = 0x00800,
        MAP_EXECUTABLE = 0x01000,
        MAP_LOCKED = 0x02000,
        MAP_NORESERVE = 0x04000,
        MAP_POPULATE = 0x08000,
        MAP_NONBLOCK = 0x10000,
        MAP_STACK = 0x20000,
        MAP_HUGETLB = 0x40000,
    }
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/m68k/bits/mman.h
else version (M68K)
{
    static if (__USE_MISC) enum
    {
        MAP_GROWSDOWN = 0x00100,
        MAP_DENYWRITE = 0x00800,
        MAP_EXECUTABLE = 0x01000,
        MAP_LOCKED = 0x02000,
        MAP_NORESERVE = 0x04000,
        MAP_POPULATE = 0x08000,
        MAP_NONBLOCK = 0x10000,
        MAP_STACK = 0x20000,
        MAP_HUGETLB = 0x40000,
    }
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/mips/bits/mman.h
else version (MIPS_Any)
{
    static if (__USE_MISC) enum
    {
        MAP_NORESERVE = 0x0400,
        MAP_GROWSDOWN = 0x1000,
        MAP_DENYWRITE = 0x2000,
        MAP_EXECUTABLE = 0x4000,
        MAP_LOCKED = 0x8000,
        MAP_POPULATE = 0x10000,
        MAP_NONBLOCK = 0x20000,
        MAP_STACK = 0x40000,
        MAP_HUGETLB = 0x80000,
    }
}
else
{
    static assert(0, "unimplemented");
}


// <bits/mman-linux.h>
// https://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=bits/mman-linux.h
version (Alpha)
{
}
else version (HPPA_Any)
{
}
else
{
    // in core.sys.posix.sys.mman
    // enum PROT_READ = 0x1;
    // enum PROT_WRITE = 0x2;
    // enum PROT_EXEC = 0x4;
    // enum PROT_NONE = 0x0;

    enum PROT_GROWSDOWN = 0x01000000;
    enum PROT_GROWSUP = 0x02000000;

    enum MAP_SHARED = 0x01;
    enum MAP_PRIVATE = 0x02;
    static if (__USE_MISC)
        enum MAP_TYPE = 0x0f;

    enum MAP_FIXED = 0x10;
    static if (__USE_MISC) enum
    {
        MAP_FILE = 0,
        MAP_ANONYMOUS = MAP_ANON,
        // in core.sys.posix.sys.mman
        // MAP_ANON = 0xXX,
        MAP_HUGE_SHIFT = 26,
        MAP_HUGE_MASK = 0x3f,
    }

    // in core.sys.posix.sys.mman
    // enum
    // {
    //     MS_ASYNC = 1,
    //     MS_SYNC = 4,
    //     MS_INVALIDATE = 2,
    // }

    static if (__USE_GNU) enum
    {
        MREMAP_MAYMOVE = 1,
        MREMAP_FIXED = 2,
    }

    static if (__USE_MISC) enum
    {
        MADV_NORMAL = 0,
        MADV_RANDOM = 1,
        MADV_SEQUENTIAL = 2,
        MADV_WILLNEED = 3,
        MADV_DONTNEED = 4,
        MADV_REMOVE = 9,
        MADV_DONTFORK = 10,
        MADV_DOFORK = 11,
        MADV_MERGEABLE = 12,
        MADV_UNMERGEABLE = 13,
        MADV_HUGEPAGE = 14,
        MADV_NOHUGEPAGE = 15,
        MADV_DONTDUMP = 16,
        MADV_DODUMP = 17,
        MADV_HWPOISON = 100,
    }

    // in core.sys.posix.sys.mman
    // static if (__USE_XOPEN2K) enum
    // {
    //     POSIX_MADV_NORMAL = 0,
    //     POSIX_MADV_RANDOM = 1,
    //     POSIX_MADV_SEQUENTIAL = 2,
    //     POSIX_MADV_WILLNEED = 3,
    //     POSIX_MADV_DONTNEED = 4,
    // }

    // in core.sys.posix.sys.mman
    // enum
    // {
    //
    //     MCL_CURRENT = 1,
    //     MCL_FUTURE = 2,
    // }
}

// Workaround https://issues.dlang.org/show_bug.cgi?id=17883
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/sparc/bits/mman.h
version (SPARC_Any)
{
    static if (__USE_MISC) enum MAP_RENAME = MAP_ANONYMOUS;
}
// http://sourceware.org/git/?p=glibc.git;a=blob;hb=51e945a8f950a6695754b11c1e6fba8bb750e100;f=sysdeps/unix/sysv/linux/mips/bits/mman.h
else version (MIPS_Any)
{
    static if (__USE_MISC) enum MAP_RENAME = MAP_ANONYMOUS;
}

// http://sourceware.org/git/?p=glibc.git;a=blob;f=misc/sys/mman.h
// in core.sys.posix.sys.mman
// static if (__USE_LARGEFILE64) void* mmap64(void*, size_t, int, int, int, off_t);
// static if (__USE_FILE_OFFSET64)
//     alias mmap64 mmap;
// else
//     void* mmap(void*, size_t, int, int, int, off_t);
// int munmap(void*, size_t);
// int mprotect(void *__addr, size_t __len, int __prot);
// int msync(void *__addr, size_t __len, int __flags);
static if (__USE_MISC) int madvise(void *__addr, size_t __len, int __advice);
// static if (__USE_XOPEN2K) int posix_madvise(void *__addr, size_t __len, int __advice);
// int mlock(const(void) *__addr, size_t __len);
// int munlock(const(void) *__addr, size_t __len);
// int mlockall(int __flags);
// int munlockall();
static if (__USE_MISC) int mincore(void *__start, size_t __len, ubyte *__vec);
static if (__USE_GNU) void *mremap(void *__addr, size_t __old_len, size_t __new_len, int __flags, ...);
static if (__USE_GNU) int remap_file_pages(void *__start, size_t __size, int __prot, size_t __pgoff, int __flags);
// int shm_open(in char *__name, int __oflag, mode_t __mode);
// int shm_unlink(in char *__name);
