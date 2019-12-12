/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009, Sönke Ludwig 2013.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly, Alex Rønne Petersen, Sönke Ludwig
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.grp;

private import core.sys.posix.config;
public import core.sys.posix.sys.types; // for gid_t, uid_t

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):
extern (C):
nothrow:
@nogc:

//
// Required
//
/*
struct group
{
    char*   gr_name;
    char*   gr_passwd;
    gid_t   gr_gid;
    char**  gr_mem;
}

group* getgrnam(in char*);
group* getgrgid(gid_t);
*/

version (CRuntime_Glibc)
{
    struct group
    {
        char*   gr_name;
        char*   gr_passwd;
        gid_t   gr_gid;
        char**  gr_mem;
    }
}
else version (Darwin)
{
    struct group
    {
        char*   gr_name;
        char*   gr_passwd;
        gid_t   gr_gid;
        char**  gr_mem;
    }
}
else version (FreeBSD)
{
    struct group
    {
        char*   gr_name;
        char*   gr_passwd;
        gid_t   gr_gid;
        char**  gr_mem;
    }
}
else version (NetBSD)
{
    struct group
    {
        char*   gr_name;
        char*   gr_passwd;
        gid_t   gr_gid;
        char**  gr_mem;
    }
}
else version (OpenBSD)
{
    struct group
    {
        char*   gr_name;
        char*   gr_passwd;
        gid_t   gr_gid;
        char**  gr_mem;
    }
}
else version (DragonFlyBSD)
{
    struct group
    {
        char*   gr_name;
        char*   gr_passwd;
        gid_t   gr_gid;
        char**  gr_mem;
    }
}
else version (Solaris)
{
    struct group
    {
        char*   gr_name;
        char*   gr_passwd;
        gid_t   gr_gid;
        char**  gr_mem;
    }
}
else version (CRuntime_Bionic)
{
    struct group
    {
        char*   gr_name;
        char*   gr_passwd;
        gid_t   gr_gid;
        char**  gr_mem;
    }
}
else version (CRuntime_UClibc)
{
    struct group
    {
        char*   gr_name;
        char*   gr_passwd;
        gid_t   gr_gid;
        char**  gr_mem;
    }
}
else version (CRuntime_Musl)
{
    struct group
    {
        char*   gr_name;
        char*   gr_passwd;
        gid_t   gr_gid;
        char**  gr_mem;
    }
}
else
{
    static assert(false, "Unsupported platform");
}

group* getgrnam(in char*);
group* getgrgid(gid_t);

//
// Thread-Safe Functions (TSF)
//
/*
int getgrnam_r(in char*, group*, char*, size_t, group**);
int getgrgid_r(gid_t, group*, char*, size_t, group**);
*/

version (CRuntime_Glibc)
{
    int getgrnam_r(in char*, group*, char*, size_t, group**);
    int getgrgid_r(gid_t, group*, char*, size_t, group**);
}
else version (Darwin)
{
    int getgrnam_r(in char*, group*, char*, size_t, group**);
    int getgrgid_r(gid_t, group*, char*, size_t, group**);
}
else version (FreeBSD)
{
    int getgrnam_r(in char*, group*, char*, size_t, group**);
    int getgrgid_r(gid_t, group*, char*, size_t, group**);
}
else version (NetBSD)
{
    int getgrnam_r(in char*, group*, char*, size_t, group**);
    int getgrgid_r(gid_t, group*, char*, size_t, group**);
}
else version (OpenBSD)
{
    int getgrnam_r(in char*, group*, char*, size_t, group**);
    int getgrgid_r(gid_t, group*, char*, size_t, group**);
}
else version (DragonFlyBSD)
{
    int getgrnam_r(in char*, group*, char*, size_t, group**);
    int getgrgid_r(gid_t, group*, char*, size_t, group**);
}
else version (Solaris)
{
    int getgrnam_r(in char*, group*, char*, int, group**);
    int getgrgid_r(gid_t, group*, char*, int, group**);
}
else version (CRuntime_Bionic)
{
}
else version (CRuntime_UClibc)
{
    int getgrnam_r(in char*, group*, char*, size_t, group**);
    int getgrgid_r(gid_t, group*, char*, size_t, group**);
}
else version (CRuntime_Musl)
{
    int getgrnam_r(in char*, group*, char*, size_t, group**);
    int getgrgid_r(gid_t, group*, char*, size_t, group**);
}
else
{
    static assert(false, "Unsupported platform");
}

//
// XOpen (XSI)
//
/*
struct group  *getgrent(void);
void           endgrent(void);
void           setgrent(void);
*/

version (CRuntime_Glibc)
{
    group* getgrent();
    @trusted void endgrent();
    @trusted void setgrent();
}
else version (Darwin)
{
    group* getgrent();
    @trusted void endgrent();
    @trusted void setgrent();
}
else version (FreeBSD)
{
    group* getgrent();
    @trusted void endgrent();
    @trusted void setgrent();
}
else version (NetBSD)
{
    group* getgrent();
    @trusted void endgrent();
    @trusted void setgrent();
}
else version (OpenBSD)
{
    group* getgrent();
    @trusted void endgrent();
    @trusted void setgrent();
}
else version (DragonFlyBSD)
{
    group* getgrent();
    @trusted void endgrent();
    @trusted void setgrent();
}
else version (Solaris)
{
    group* getgrent();
    @trusted void endgrent();
    @trusted void setgrent();
}
else version (CRuntime_Bionic)
{
}
else version (CRuntime_UClibc)
{
    group* getgrent();
    @trusted void endgrent();
    @trusted void setgrent();
}
else version (CRuntime_Musl)
{
    group* getgrent();
    @trusted void endgrent();
    @trusted void setgrent();
}
else
{
    static assert(false, "Unsupported platform");
}
