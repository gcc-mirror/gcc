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
module core.sys.posix.sys.mman;

private import core.sys.posix.config;
public import core.sys.posix.sys.types; // for off_t, mode_t

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

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

version (Posix):
extern (C) nothrow @nogc:

//
// Advisory Information (ADV)
//
/*
int posix_madvise(void*, size_t, int);
*/

//
// Advisory Information and either Memory Mapped Files or Shared Memory Objects (MC1)
//
/*
POSIX_MADV_NORMAL
POSIX_MADV_SEQUENTIAL
POSIX_MADV_RANDOM
POSIX_MADV_WILLNEED
POSIX_MADV_DONTNEED
*/

version (CRuntime_Glibc)
{
    version (Alpha)
        private enum __POSIX_MADV_DONTNEED = 6;
    else
        private enum __POSIX_MADV_DONTNEED = 4;

    static if (__USE_XOPEN2K)
    {
        enum
        {
            POSIX_MADV_NORMAL = 0,
            POSIX_MADV_RANDOM = 1,
            POSIX_MADV_SEQUENTIAL = 2,
            POSIX_MADV_WILLNEED = 3,
            POSIX_MADV_DONTNEED = __POSIX_MADV_DONTNEED,
        }
        int posix_madvise(void *__addr, size_t __len, int __advice);
    }
}
else version (Darwin)
{
    enum POSIX_MADV_NORMAL      = 0;
    enum POSIX_MADV_RANDOM      = 1;
    enum POSIX_MADV_SEQUENTIAL  = 2;
    enum POSIX_MADV_WILLNEED    = 3;
    enum POSIX_MADV_DONTNEED    = 4;
    int posix_madvise(void *addr, size_t len, int advice);
}
else version (FreeBSD)
{
    enum POSIX_MADV_NORMAL      = 0;
    enum POSIX_MADV_RANDOM      = 1;
    enum POSIX_MADV_SEQUENTIAL  = 2;
    enum POSIX_MADV_WILLNEED    = 3;
    enum POSIX_MADV_DONTNEED    = 4;
    int posix_madvise(void *addr, size_t len, int advice);
}
else version (NetBSD)
{
    enum POSIX_MADV_NORMAL      = 0;
    enum POSIX_MADV_RANDOM      = 1;
    enum POSIX_MADV_SEQUENTIAL  = 2;
    enum POSIX_MADV_WILLNEED    = 3;
    enum POSIX_MADV_DONTNEED    = 4;
    int posix_madvise(void *addr, size_t len, int advice);
}
else version (DragonFlyBSD)
{
    enum POSIX_MADV_NORMAL      = 0;
    enum POSIX_MADV_RANDOM      = 1;
    enum POSIX_MADV_SEQUENTIAL  = 2;
    enum POSIX_MADV_WILLNEED    = 3;
    enum POSIX_MADV_DONTNEED    = 4;
    int posix_madvise(void *addr, size_t len, int advice);
}
else version (Solaris)
{
}
else version (CRuntime_Bionic)
{
}
else version (CRuntime_Musl)
{
}
else version (CRuntime_UClibc)
{
    enum
    {
        POSIX_MADV_NORMAL = 0,
        POSIX_MADV_RANDOM = 1,
        POSIX_MADV_SEQUENTIAL = 2,
        POSIX_MADV_WILLNEED = 3,
        POSIX_MADV_DONTNEED = 4,
    }
    int posix_madvise(void *__addr, size_t __len, int __advice);
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Memory Mapped Files, Shared Memory Objects, or Memory Protection (MC2)
//
/*
PROT_READ
PROT_WRITE
PROT_EXEC
PROT_NONE
*/

version (CRuntime_Glibc)
{
    enum PROT_NONE      = 0x0;
    enum PROT_READ      = 0x1;
    enum PROT_WRITE     = 0x2;
    enum PROT_EXEC      = 0x4;
}
else version (Darwin)
{
    enum PROT_NONE      = 0x00;
    enum PROT_READ      = 0x01;
    enum PROT_WRITE     = 0x02;
    enum PROT_EXEC      = 0x04;
}
else version (FreeBSD)
{
    enum PROT_NONE      = 0x00;
    enum PROT_READ      = 0x01;
    enum PROT_WRITE     = 0x02;
    enum PROT_EXEC      = 0x04;
}
else version (NetBSD)
{
    enum PROT_NONE      = 0x00;
    enum PROT_READ      = 0x01;
    enum PROT_WRITE     = 0x02;
    enum PROT_EXEC      = 0x04;
}
else version (DragonFlyBSD)
{
    enum PROT_NONE      = 0x00;
    enum PROT_READ      = 0x01;
    enum PROT_WRITE     = 0x02;
    enum PROT_EXEC      = 0x04;
}
else version (Solaris)
{
    enum PROT_NONE = 0x00;
    enum PROT_READ = 0x01;
    enum PROT_WRITE = 0x02;
    enum PROT_EXEC = 0x04;
}
else version (CRuntime_Bionic)
{
    enum PROT_NONE = 0x00;
    enum PROT_READ = 0x01;
    enum PROT_WRITE = 0x02;
    enum PROT_EXEC = 0x04;
}
else version (CRuntime_Musl)
{
    enum PROT_NONE      = 0x0;
    enum PROT_READ      = 0x1;
    enum PROT_WRITE     = 0x2;
    enum PROT_EXEC      = 0x4;
}
else version (CRuntime_UClibc)
{
    enum PROT_NONE      = 0x0;
    enum PROT_READ      = 0x1;
    enum PROT_WRITE     = 0x2;
    enum PROT_EXEC      = 0x4;
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Memory Mapped Files, Shared Memory Objects, or Typed Memory Objects (MC3)
//
/*
void* mmap(void*, size_t, int, int, int, off_t);
int munmap(void*, size_t);
*/

version (CRuntime_Glibc)
{
    static if (__USE_LARGEFILE64) void* mmap64(void*, size_t, int, int, int, off_t);
    static if (__USE_FILE_OFFSET64)
        alias mmap = mmap64;
    else
        void* mmap(void*, size_t, int, int, int, off_t);
    int munmap(void*, size_t);
}
else version (Darwin)
{
    void* mmap(void*, size_t, int, int, int, off_t);
    int   munmap(void*, size_t);
}
else version (FreeBSD)
{
    void* mmap(void*, size_t, int, int, int, off_t);
    int   munmap(void*, size_t);
}
else version (NetBSD)
{
    void* mmap(void*, size_t, int, int, int, off_t);
    int   munmap(void*, size_t);
}
else version (DragonFlyBSD)
{
    void* mmap(void*, size_t, int, int, int, off_t);
    int   munmap(void*, size_t);
}
else version (Solaris)
{
    void* mmap(void*, size_t, int, int, int, off_t);
    int   munmap(void*, size_t);
}
else version (CRuntime_Bionic)
{
    void* mmap(void*, size_t, int, int, int, off_t);
    int   munmap(void*, size_t);
}
else version (CRuntime_Musl)
{
    static if (__USE_LARGEFILE64) void* mmap64(void*, size_t, int, int, int, off_t);
    static if (__USE_FILE_OFFSET64)
        alias mmap = mmap64;
    else
        void* mmap(void*, size_t, int, int, int, off_t);
    int munmap(void*, size_t);
}
else version (CRuntime_UClibc)
{
    static if (__USE_LARGEFILE64) void* mmap64(void*, size_t, int, int, int, off64_t);
    static if (__USE_FILE_OFFSET64)
        alias mmap = mmap64;
    else
        void* mmap(void*, size_t, int, int, int, off_t);
    int munmap(void*, size_t);
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Memory Mapped Files (MF)
//
/*
MAP_SHARED (MF|SHM)
MAP_PRIVATE (MF|SHM)
MAP_FIXED  (MF|SHM)
MAP_FAILED (MF|SHM)

MS_ASYNC (MF|SIO)
MS_SYNC (MF|SIO)
MS_INVALIDATE (MF|SIO)

int msync(void*, size_t, int); (MF|SIO)
*/

version (CRuntime_Glibc)
{
    enum MAP_SHARED     = 0x01;
    enum MAP_PRIVATE    = 0x02;
    enum MAP_FIXED      = 0x10;

    enum MAP_FAILED     = cast(void*) -1;

    version (MICROBLAZE)
        private enum DEFAULTS = true;
    else version (Alpha)
    {
        private enum DEFAULTS = false;
        enum MAP_ANON = 0x10;
        enum MS_ASYNC = 1;
        enum MS_SYNC = 2;
        enum MS_INVALIDATE = 4;
    }
    else version (SH)
        private enum DEFAULTS = true;
    else version (ARM_Any)
        private enum DEFAULTS = true;
    else version (IBMZ_Any)
        private enum DEFAULTS = true;
    else version (IA64)
        private enum DEFAULTS = true;
    else version (HPPA_Any)
    {
        private enum DEFAULTS = false;
        enum MAP_ANON = 0x10;
        enum MS_SYNC = 1;
        enum MS_ASYNC = 2;
        enum MS_INVALIDATE = 4;
    }
    else version (M68K)
        private enum DEFAULTS = true;
    else version (TILE)
        private enum DEFAULTS = true;
    else version (X86_Any)
        private enum DEFAULTS = true;
    else version (MIPS_Any)
    {
        private enum DEFAULTS = false;
        enum MAP_ANON = 0x0800;
        enum MS_ASYNC = 1;
        enum MS_INVALIDATE = 2;
        enum MS_SYNC = 4;
    }
    else version (RISCV_Any)
        private enum DEFAULTS = true;
    else version (SPARC_Any)
        private enum DEFAULTS = true;
    else version (PPC_Any)
        private enum DEFAULTS = true;
    else
        static assert(0, "unimplemented");

    static if (DEFAULTS)
    {
        enum MAP_ANON = 0x20;
        enum MS_ASYNC = 1;
        enum MS_INVALIDATE = 2;
        enum MS_SYNC = 4;
    }

    int msync(void*, size_t, int);
}
else version (Darwin)
{
    enum MAP_SHARED     = 0x0001;
    enum MAP_PRIVATE    = 0x0002;
    enum MAP_FIXED      = 0x0010;
    enum MAP_ANON       = 0x1000;

    enum MAP_FAILED     = cast(void*)-1;

    enum MS_ASYNC       = 0x0001;
    enum MS_INVALIDATE  = 0x0002;
    enum MS_SYNC        = 0x0010;

    int msync(void*, size_t, int);
}
else version (FreeBSD)
{
    enum MAP_SHARED     = 0x0001;
    enum MAP_PRIVATE    = 0x0002;
    enum MAP_FIXED      = 0x0010;
    enum MAP_ANON       = 0x1000;

    enum MAP_FAILED     = cast(void*)-1;

    enum MS_SYNC        = 0x0000;
    enum MS_ASYNC       = 0x0001;
    enum MS_INVALIDATE  = 0x0002;

    int msync(void*, size_t, int);
}
else version (NetBSD)
{
    enum MAP_SHARED     = 0x0001;
    enum MAP_PRIVATE    = 0x0002;
    enum MAP_FIXED      = 0x0010;
    enum MAP_ANON       = 0x1000;

    enum MAP_FAILED     = cast(void*)-1;

    enum MS_SYNC        = 0x0004;
    enum MS_ASYNC       = 0x0001;
    enum MS_INVALIDATE  = 0x0002;

    int __msync13(void*, size_t, int);
    alias msync = __msync13;
}
else version (DragonFlyBSD)
{
    enum MAP_SHARED     = 0x0001;
    enum MAP_PRIVATE    = 0x0002;
    enum MAP_FIXED      = 0x0010;
    enum MAP_ANON       = 0x1000;

    enum MAP_FAILED     = cast(void*)-1;

    enum MS_SYNC        = 0x0000;
    enum MS_ASYNC       = 0x0001;
    enum MS_INVALIDATE  = 0x0002;

    int msync(void*, size_t, int);
}
else version (Solaris)
{
    enum MAP_SHARED = 0x0001;
    enum MAP_PRIVATE = 0x0002;
    enum MAP_FIXED = 0x0010;
    enum MAP_ANON = 0x0100;

    enum MAP_FAILED = cast(void*)-1;

    enum MS_SYNC = 0x0004;
    enum MS_ASYNC = 0x0001;
    enum MS_INVALIDATE  = 0x0002;

    int msync(void*, size_t, int);
}
else version (CRuntime_Bionic)
{
    enum MAP_SHARED     = 0x0001;
    enum MAP_PRIVATE    = 0x0002;
    enum MAP_FIXED      = 0x0010;
    enum MAP_ANON       = 0x0020;

    enum MAP_FAILED     = cast(void*)-1;

    enum MS_SYNC        = 4;
    enum MS_ASYNC       = 1;
    enum MS_INVALIDATE  = 2;

    int msync(in void*, size_t, int);
}
else version (CRuntime_Musl)
{
    enum MAP_SHARED     = 0x01;
    enum MAP_PRIVATE    = 0x02;
    enum MAP_FIXED      = 0x10;

    enum MAP_FAILED     = cast(void*) -1;
    enum MAP_ANON = 0x20;
    enum MS_ASYNC = 1;
    enum MS_INVALIDATE = 2;
    enum MS_SYNC = 4;
    int msync(void*, size_t, int);
}
else version (CRuntime_UClibc)
{
    enum MAP_SHARED     = 0x01;
    enum MAP_PRIVATE    = 0x02;
    enum MAP_FIXED      = 0x10;

    enum MAP_FAILED     = cast(void*) -1;

    version (X86_64)
    {
        enum MAP_ANON       = 0x20;
        enum MS_ASYNC       = 1;
        enum MS_INVALIDATE  = 2;
        enum MS_SYNC        = 4;
    }
    else version (MIPS32)
    {
        enum MAP_ANON       = 0x0800;
        enum MS_ASYNC       = 1;
        enum MS_INVALIDATE  = 2;
        enum MS_SYNC        = 4;
    }
    else version (ARM)
    {
        enum MAP_ANON       = 0x020;
        enum MS_ASYNC       = 1;
        enum MS_INVALIDATE  = 2;
        enum MS_SYNC        = 4;
    }
    else
    {
        static assert(false, "Architecture not supported.");
    }


    int msync(void*, size_t, int);
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Process Memory Locking (ML)
//
/*
MCL_CURRENT
MCL_FUTURE

int mlockall(int);
int munlockall();
*/

version (CRuntime_Glibc)
{
    version (SPARC_Any) enum
    {
        MCL_CURRENT = 0x2000,
        MCL_FUTURE = 0x4000,
    }
    else version (PPC_Any) enum
    {
        MCL_CURRENT = 0x2000,
        MCL_FUTURE = 0x4000,
    }
    else version (Alpha) enum
    {
        MCL_CURRENT = 8192,
        MCL_FUTURE = 16384,
    }
    else enum
    {
        MCL_CURRENT = 1,
        MCL_FUTURE = 2,
    }

    int mlockall(int);
    int munlockall();

}
else version (Darwin)
{
    enum MCL_CURRENT    = 0x0001;
    enum MCL_FUTURE     = 0x0002;

    int mlockall(int);
    int munlockall();
}
else version (FreeBSD)
{
    enum MCL_CURRENT    = 0x0001;
    enum MCL_FUTURE     = 0x0002;

    int mlockall(int);
    int munlockall();
}
else version (NetBSD)
{
    enum MCL_CURRENT    = 0x0001;
    enum MCL_FUTURE     = 0x0002;

    int mlockall(int);
    int munlockall();
}
else version (DragonFlyBSD)
{
    enum MCL_CURRENT    = 0x0001;
    enum MCL_FUTURE     = 0x0002;

    int mlockall(int);
    int munlockall();
}
else version (Solaris)
{
    enum MCL_CURRENT = 0x0001;
    enum MCL_FUTURE = 0x0002;

    int mlockall(int);
    int munlockall();
}
else version (CRuntime_Bionic)
{
    enum MCL_CURRENT = 1;
    enum MCL_FUTURE  = 2;

    int mlockall(int);
    int munlockall();
}
else version (CRuntime_Musl)
{
}
else version (CRuntime_UClibc)
{
    enum
    {
        MCL_CURRENT = 1,
        MCL_FUTURE = 2,
    }

    int mlockall(int);
    int munlockall();

}
else
{
    static assert(false, "Unsupported platform");
}

//
// Range Memory Locking (MLR)
//
/*
int mlock(in void*, size_t);
int munlock(in void*, size_t);
*/

version (CRuntime_Glibc)
{
    int mlock(in void*, size_t);
    int munlock(in void*, size_t);
}
else version (Darwin)
{
    int mlock(in void*, size_t);
    int munlock(in void*, size_t);
}
else version (FreeBSD)
{
    int mlock(in void*, size_t);
    int munlock(in void*, size_t);
}
else version (NetBSD)
{
    int mlock(in void*, size_t);
    int munlock(in void*, size_t);
}
else version (DragonFlyBSD)
{
    int mlock(in void*, size_t);
    int munlock(in void*, size_t);
}
else version (Solaris)
{
    int mlock(in void*, size_t);
    int munlock(in void*, size_t);
}
else version (CRuntime_Bionic)
{
    int mlock(in void*, size_t);
    int munlock(in void*, size_t);
}
else version (CRuntime_Musl)
{
}
else version (CRuntime_UClibc)
{
    int mlock(in void*, size_t);
    int munlock(in void*, size_t);
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Memory Protection (MPR)
//
/*
int mprotect(void*, size_t, int);
*/

version (CRuntime_Glibc)
{
    int mprotect(void*, size_t, int);
}
else version (Darwin)
{
    int mprotect(void*, size_t, int);
}
else version (FreeBSD)
{
    int mprotect(void*, size_t, int);
}
else version (NetBSD)
{
    int mprotect(void*, size_t, int);
}
else version (DragonFlyBSD)
{
    int mprotect(void*, size_t, int);
}
else version (Solaris)
{
    int mprotect(void*, size_t, int);
}
else version (CRuntime_Bionic)
{
    int mprotect(in void*, size_t, int);
}
else version (CRuntime_Musl)
{
    int mprotect(void*, size_t, int);
}
else version (CRuntime_UClibc)
{
    int mprotect(void*, size_t, int);
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Shared Memory Objects (SHM)
//
/*
int shm_open(in char*, int, mode_t);
int shm_unlink(in char*);
*/

version (CRuntime_Glibc)
{
    int shm_open(in char*, int, mode_t);
    int shm_unlink(in char*);
}
else version (Darwin)
{
    int shm_open(in char*, int, mode_t);
    int shm_unlink(in char*);
}
else version (FreeBSD)
{
    int shm_open(in char*, int, mode_t);
    int shm_unlink(in char*);
}
else version (NetBSD)
{
    int shm_open(in char*, int, mode_t);
    int shm_unlink(in char*);
}
else version (DragonFlyBSD)
{
    int shm_open(in char*, int, mode_t);
    int shm_unlink(in char*);
}
else version (Solaris)
{
    int shm_open(in char*, int, mode_t);
    int shm_unlink(in char*);
}
else version (CRuntime_Bionic)
{
}
else version (CRuntime_Musl)
{
}
else version (CRuntime_UClibc)
{
    int shm_open(in char*, int, mode_t);
    int shm_unlink(in char*);
}
else
{
    static assert(false, "Unsupported platform");
}

//
// Typed Memory Objects (TYM)
//
/*
POSIX_TYPED_MEM_ALLOCATE
POSIX_TYPED_MEM_ALLOCATE_CONTIG
POSIX_TYPED_MEM_MAP_ALLOCATABLE

struct posix_typed_mem_info
{
    size_t posix_tmi_length;
}

int posix_mem_offset(in void*, size_t, off_t *, size_t *, int *);
int posix_typed_mem_get_info(int, struct posix_typed_mem_info *);
int posix_typed_mem_open(in char*, int, int);
*/
