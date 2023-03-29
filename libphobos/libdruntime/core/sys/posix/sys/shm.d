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

import core.sys.posix.config;
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
*/

version (linux)
{
    enum SHM_RDONLY     = 0x01000; // 010000
    enum SHM_RND        = 0x02000; // 020000
    enum SHM_REMAP      = 0x4000; // 040000

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
}
else version (FreeBSD)
{
    enum SHM_RDONLY     = 0x01000; // 010000
    enum SHM_RND        = 0x02000; // 020000

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
}
else version (NetBSD)
{
    enum SHM_RDONLY     = 0x01000; // 010000
    enum SHM_RND        = 0x02000; // 020000

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
}
else version (OpenBSD)
{
    enum SHM_RDONLY     = 0x01000; // 010000
    enum SHM_RND        = 0x02000; // 020000

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
}
else version (DragonFlyBSD)
{
    enum SHM_RDONLY     = 0x01000; // 010000
    enum SHM_RND        = 0x02000; // 020000

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
}
else version (Darwin)
{

}
else version (Solaris)
{

}
else
{
    static assert(false, "Unsupported platform");
}

/*
SHMLBA

void* shmat(int, const scope void*, int);
int   shmctl(int, int, shmid_ds*);
int   shmdt(const scope void*);
int   shmget(key_t, size_t, int);
*/

version (CRuntime_Glibc)
{
    int   __getpagesize();
    alias __getpagesize SHMLBA;

    void* shmat(int, const scope void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(const scope void*);
    int   shmget(key_t, size_t, int);
}
else version (FreeBSD)
{
    enum SHMLBA = 1 << 12; // PAGE_SIZE = (1<<PAGE_SHIFT)

    void* shmat(int, const scope void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(const scope void*);
    int   shmget(key_t, size_t, int);
}
else version (NetBSD)
{
    enum SHMLBA = 1 << 12; // PAGE_SIZE = (1<<PAGE_SHIFT)

    void* shmat(int, const scope void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(const scope void*);
    int   shmget(key_t, size_t, int);
}
else version (OpenBSD)
{
    enum SHMLBA = 1 << _MAX_PAGE_SHIFT;

    void* shmat(int, const scope void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(const scope void*);
    int   shmget(key_t, size_t, int);
}
else version (DragonFlyBSD)
{
    enum SHMLBA = 1 << 12; // PAGE_SIZE = (1<<PAGE_SHIFT)

    void* shmat(int, const scope void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(const scope void*);
    int   shmget(key_t, size_t, int);
}
else version (Darwin)
{

}
else version (Solaris)
{

}
else version (CRuntime_Musl)
{
    enum SHMLBA = 4096;

    void* shmat(int, const scope void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(const scope void*);
    int   shmget(key_t, size_t, int);
}
else version (CRuntime_Bionic)
{
    enum SHMLBA = 4096;

    deprecated("Not useful on Android because it's disallowed by SELinux")
    {
        void* shmat(int, const scope void*, int);
        int   shmctl(int, int, shmid_ds*);
        int   shmdt(const scope void*);
        int   shmget(key_t, size_t, int);
    }
}
else version (CRuntime_UClibc)
{
    int   __getpagesize();
    alias __getpagesize SHMLBA;

    void* shmat(int, const scope void*, int);
    int   shmctl(int, int, shmid_ds*);
    int   shmdt(const scope void*);
    int   shmget(key_t, size_t, int);
}
else
{
    static assert(false, "Unsupported platform");
}
