/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.sys.shm;

private import core.sys.posix.config;
public import core.sys.posix.sys.types; // for pid_t, time_t, key_t
public import core.sys.posix.sys.ipc;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):
extern (C) nothrow @nogc:

//
// XOpen (XSI)
//
/*
SHM_RDONLY
SHM_RND

SHMLBA

shmatt_t

struct shmid_ds
{
    ipc_perm    shm_perm;
    size_t      shm_segsz;
    pid_t       shm_lpid;
    pid_t       shm_cpid;
    shmatt_t    shm_nattch;
    time_t      shm_atime;
    time_t      shm_dtime;
    time_t      shm_ctime;
}

void* shmat(int, in void*, int);
int   shmctl(int, int, shmid_ds*);
int   shmdt(in void*);
int   shmget(key_t, size_t, int);
*/

version (CRuntime_Glibc)
{
    enum SHM_RDONLY     = 0x01000; // 010000
    enum SHM_RND        = 0x02000; // 020000

    int   __getpagesize();
    alias __getpagesize SHMLBA;

    alias c_ulong   shmatt_t;

    /* For any changes, please check /usr/include/bits/shm.h */
    struct shmid_ds
    {
        ipc_perm    shm_perm;
        size_t      shm_segsz;
        time_t      shm_atime;
        version (X86_64) {} else c_ulong     __unused1;
        time_t      shm_dtime;
        version (X86_64) {} else c_ulong     __unused2;
        time_t      shm_ctime;
        version (X86_64) {} else c_ulong     __unused3;
        pid_t       shm_cpid;
        pid_t       shm_lpid;
        shmatt_t    shm_nattch;
        c_ulong     __unused4;
        c_ulong     __unused5;
    }

    void* shmat(int, in void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(in void*);
    int   shmget(key_t, size_t, int);
}
else version (FreeBSD)
{
    enum SHM_RDONLY     = 0x01000; // 010000
    enum SHM_RND        = 0x02000; // 020000
    enum SHMLBA         = 1 << 12; // PAGE_SIZE = (1<<PAGE_SHIFT)

    alias c_ulong   shmatt_t;

    struct shmid_ds_old // <= FreeBSD7
    {
        ipc_perm_old    shm_perm;
        int             shm_segsz;
        pid_t           shm_lpid;
        pid_t           shm_cpid;
        short           shm_nattch;
        time_t          shm_atime;
        time_t          shm_dtime;
        time_t          shm_ctime;
        void*           shm_internal;
    }

    struct shmid_ds
    {
         ipc_perm    shm_perm;
         int         shm_segsz;
         pid_t       shm_lpid;
         pid_t       shm_cpid;
         short       shm_nattch;
         time_t      shm_atime;
         time_t      shm_dtime;
         time_t      shm_ctime;
    }

    void* shmat(int, in void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(in void*);
    int   shmget(key_t, size_t, int);
}
else version (NetBSD)
{
    enum SHM_RDONLY     = 0x01000; // 010000
    enum SHM_RND        = 0x02000; // 020000
    enum SHMLBA         = 1 << 12; // PAGE_SIZE = (1<<PAGE_SHIFT)

    alias c_ulong   shmatt_t;

    struct shmid_ds
    {
        ipc_perm        shm_perm;
        size_t             shm_segsz;
        pid_t           shm_lpid;
        pid_t           shm_cpid;
        short           shm_nattch;
        time_t          shm_atime;
        time_t          shm_dtime;
        time_t          shm_ctime;
        void*           shm_internal;
    }

    void* shmat(int, in void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(in void*);
    int   shmget(key_t, size_t, int);
}
else version (OpenBSD)
{
    enum SHM_RDONLY     = 0x01000; // 010000
    enum SHM_RND        = 0x02000; // 020000
    enum SHMLBA         = 1 << _MAX_PAGE_SHIFT;

    alias short shmatt_t;

    struct shmid_ds
    {
        ipc_perm   shm_perm;
        int        shm_segsz;
        pid_t      shm_lpid;
        pid_t      shm_cpid;
        shmatt_t   shm_nattch;
        time_t     shm_atime;
        c_long   __shm_atimensec;
        time_t     shm_dtime;
        c_long   __shm_dtimensec;
        time_t     shm_ctime;
        c_long   __shm_ctimensec;
        void*      shm_internal;
    }

    void* shmat(int, in void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(in void*);
    int   shmget(key_t, size_t, int);
}
else version (DragonFlyBSD)
{
    enum SHM_RDONLY     = 0x01000; // 010000
    enum SHM_RND        = 0x02000; // 020000
    enum SHMLBA         = 1 << 12; // PAGE_SIZE = (1<<PAGE_SHIFT)

    alias c_ulong   shmatt_t;

    struct shmid_ds
    {
         ipc_perm       shm_perm;
         int            shm_segsz;
         pid_t          shm_lpid;
         pid_t          shm_cpid;
         short          shm_nattch;
         time_t         shm_atime;
         time_t         shm_dtime;
         time_t         shm_ctime;
         private void*  shm_internal;
    }

    void* shmat(int, in void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(in void*);
    int   shmget(key_t, size_t, int);
}
else version (Darwin)
{

}
else version (CRuntime_UClibc)
{
    enum SHM_RDONLY     = 0x1000; // 010000
    enum SHM_RND        = 0x2000; // 020000
    enum SHM_REMAP      = 0x4000; // 040000

    int   __getpagesize();
    alias __getpagesize SHMLBA;

    alias c_ulong   shmatt_t;

    version (X86_64)
        enum includeUnused  = false;
    else version (MIPS32)
        enum includeUnused  = false;
    else
        enum includeUnused  = true;

    struct shmid_ds
    {
        ipc_perm    shm_perm;
        size_t      shm_segsz;
        time_t      shm_atime;
        static if (includeUnused) c_ulong     __unused1;
        time_t      shm_dtime;
        static if (includeUnused) c_ulong     __unused2;
        time_t      shm_ctime;
        static if (includeUnused) c_ulong     __unused3;
        pid_t       shm_cpid;
        pid_t       shm_lpid;
        shmatt_t    shm_nattch;
        c_ulong     __unused4;
        c_ulong     __unused5;
    }

    struct shminfo
    {
        c_ulong shmmax;
        c_ulong shmmin;
        c_ulong shmmni;
        c_ulong shmseg;
        c_ulong shmall;
        c_ulong __unused1;
        c_ulong __unused2;
        c_ulong __unused3;
        c_ulong __unused4;
    }

    struct shm_info
    {
        int used_ids;
        c_ulong shm_tot;
        c_ulong shm_rss;
        c_ulong shm_swp;
        c_ulong swap_attempts;
        c_ulong swap_successes;
    }

    void* shmat(int, in void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(in void*);
    int   shmget(key_t, size_t, int);
}
