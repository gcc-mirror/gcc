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
module core.sys.posix.sys.ipc;

import core.sys.posix.config;
public import core.sys.posix.sys.types; // for uid_t, gid_t, mode_t, key_t

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
struct ipc_perm
{
    uid_t    uid;
    gid_t    gid;
    uid_t    cuid;
    gid_t    cgid;
    mode_t   mode;
}

IPC_CREAT
IPC_EXCL
IPC_NOWAIT

IPC_PRIVATE

IPC_RMID
IPC_SET
IPC_STAT
*/

version (linux)
{
    struct ipc_perm
    {
        key_t   __key;
        uid_t   uid;
        gid_t   gid;
        uid_t   cuid;
        gid_t   cgid;
        ushort  mode;
        ushort  __pad1;
        ushort  __seq;
        ushort  __pad2;
        c_ulong __unused1;
        c_ulong __unused2;
    }

    enum IPC_CREAT      = 0x0200; // 01000
    enum IPC_EXCL       = 0x0400; // 02000
    enum IPC_NOWAIT     = 0x0800; // 04000

    enum key_t IPC_PRIVATE = 0;

    enum IPC_RMID       = 0;
    enum IPC_SET        = 1;
    enum IPC_STAT       = 2;
}
else version (Darwin)
{
    align(4) struct ipc_perm
    {
        uid_t   uid;
        gid_t   gid;
        uid_t   cuid;
        gid_t   cgid;
        mode_t  mode;
        ushort  _seq;
        key_t   _key;
    }

    enum IPC_CREAT      = 0x0200; // 01000
    enum IPC_EXCL       = 0x0400; // 02000
    enum IPC_NOWAIT     = 0x0800; // 04000

    enum key_t IPC_PRIVATE = 0;

    enum IPC_RMID       = 0;
    enum IPC_SET        = 1;
    enum IPC_STAT       = 2;
}
else version (FreeBSD)
{
    struct ipc_perm_old // <= FreeBSD7
    {
        ushort cuid;
        ushort cguid;
        ushort uid;
        ushort gid;
        ushort mode;
        ushort seq;
        key_t key;
    }

    struct ipc_perm
    {
        uid_t   cuid;
        gid_t   cgid;
        uid_t   uid;
        gid_t   gid;
        mode_t  mode;
        ushort  seq;
        key_t   key;
    }

    enum IPC_CREAT      = 0x0200; // 01000
    enum IPC_EXCL       = 0x0400; // 02000
    enum IPC_NOWAIT     = 0x0800; // 04000

    enum key_t IPC_PRIVATE = 0;

    enum IPC_RMID       = 0;
    enum IPC_SET        = 1;
    enum IPC_STAT       = 2;
}
else version (NetBSD)
{
    struct ipc_perm
    {
        uid_t   cuid;
        gid_t   cgid;
        uid_t   uid;
        gid_t   gid;
        mode_t  mode;
        ushort  seq;
        key_t   key;
    }

    enum IPC_CREAT      = 0x0100; // 01000
    enum IPC_EXCL       = 0x0200; // 02000
    enum IPC_NOWAIT     = 0x0400; // 04000

    enum key_t IPC_PRIVATE = 0;

    enum IPC_RMID       = 0;
    enum IPC_SET        = 1;
    enum IPC_STAT       = 2;
}
else version (OpenBSD)
{
    struct ipc_perm
    {
        uid_t   cuid;
        gid_t   cgid;
        uid_t   uid;
        gid_t   gid;
        mode_t  mode;
        ushort  seq;
        key_t   key;
    }

    enum IPC_CREAT      = 0x0200; // 01000
    enum IPC_EXCL       = 0x0400; // 02000
    enum IPC_NOWAIT     = 0x0800; // 04000

    enum key_t IPC_PRIVATE = 0;

    enum IPC_RMID       = 0;
    enum IPC_SET        = 1;
    enum IPC_STAT       = 2;
}
else version (DragonFlyBSD)
{
    struct ipc_perm
    {
        uid_t   cuid;
        gid_t   cgid;
        uid_t   uid;
        gid_t   gid;
        mode_t  mode;
        ushort  seq;
        key_t   key;
    }

    enum IPC_CREAT      = 0x0200; // 01000
    enum IPC_EXCL       = 0x0400; // 02000
    enum IPC_NOWAIT     = 0x0800; // 04000

    enum key_t IPC_PRIVATE = 0;

    enum IPC_RMID       = 0;
    enum IPC_SET        = 1;
    enum IPC_STAT       = 2;
}
else version (Solaris)
{
    version (D_LP64)
    {
        struct ipc_perm
        {
            uid_t   uid;
            gid_t   gid;
            uid_t   cuid;
            gid_t   cgid;
            mode_t  mode;
            uint    seq;
            key_t   key;
        }
    }
    else
    {
        struct ipc_perm
        {
            uid_t   uid;
            gid_t   gid;
            uid_t   cuid;
            gid_t   cgid;
            mode_t  mode;
            uint    seq;
            key_t   key;
            int[4] pad;
        }
    }

    enum IPC_CREAT      = 0x200;
    enum IPC_EXCL       = 0x400;
    enum IPC_NOWAIT     = 0x800;

    enum key_t IPC_PRIVATE = 0;

    enum IPC_RMID       = 10;
    enum IPC_SET        = 11;
    enum IPC_STAT       = 12;
}
else
{
    static assert(false, "Unsupported platform");
}

/*
key_t ftok(const scope char*, int);
*/

version (CRuntime_Glibc)
{
    key_t ftok(const scope char*, int);
}
else version (Darwin)
{
    key_t ftok(const scope char*, int);
}
else version (FreeBSD)
{
    key_t ftok(const scope char*, int);
}
else version (NetBSD)
{
    key_t ftok(const scope char*, int);
}
else version (OpenBSD)
{
    key_t ftok(const scope char*, int);
}
else version (DragonFlyBSD)
{
    key_t ftok(const scope char*, int);
}
else version (Solaris)
{
    key_t ftok(const scope char*, int);
}
else version (CRuntime_Bionic)
{
    key_t ftok(const scope char*, int);
}
else version (CRuntime_Musl)
{
    key_t ftok(const scope char*, int);
}
else version (CRuntime_UClibc)
{
    key_t ftok(const scope char*, int);
}
else
{
    static assert(false, "Unsupported platform");
}
