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
module core.sys.posix.pwd;

import core.sys.posix.config;
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
struct passwd
{
    char*   pw_name;
    uid_t   pw_uid;
    gid_t   pw_gid;
    char*   pw_dir;
    char*   pw_shell;
}

passwd* getpwnam(const scope char*);
passwd* getpwuid(uid_t);
*/

version (CRuntime_Glibc)
{
    struct passwd
    {
        char*   pw_name;
        char*   pw_passwd;
        uid_t   pw_uid;
        gid_t   pw_gid;
        char*   pw_gecos;
        char*   pw_dir;
        char*   pw_shell;
    }
}
else version (Darwin)
{
    struct passwd
    {
        char*   pw_name;
        char*   pw_passwd;
        uid_t   pw_uid;
        gid_t   pw_gid;
        time_t  pw_change;
        char*   pw_class;
        char*   pw_gecos;
        char*   pw_dir;
        char*   pw_shell;
        time_t  pw_expire;
    }
}
else version (FreeBSD)
{
    struct passwd
    {
        char*   pw_name;        /* user name */
        char*   pw_passwd;      /* encrypted password */
        uid_t   pw_uid;         /* user uid */
        gid_t   pw_gid;         /* user gid */
        time_t  pw_change;      /* password change time */
        char*   pw_class;       /* user access class */
        char*   pw_gecos;       /* Honeywell login info */
        char*   pw_dir;     /* home directory */
        char*   pw_shell;       /* default shell */
        time_t  pw_expire;      /* account expiration */
        int pw_fields;      /* internal: fields filled in */
    }
}
else version (NetBSD)
{
    struct passwd
    {
        char*   pw_name;        /* user name */
        char*   pw_passwd;      /* encrypted password */
        uid_t   pw_uid;         /* user uid */
        gid_t   pw_gid;         /* user gid */
        time_t  pw_change;      /* password change time */
        char*   pw_class;       /* user access class */
        char*   pw_gecos;       /* Honeywell login info */
        char*   pw_dir;     /* home directory */
        char*   pw_shell;       /* default shell */
        time_t  pw_expire;      /* account expiration */
    }
}
else version (OpenBSD)
{
    struct passwd
    {
        char*   pw_name;        /* user name */
        char*   pw_passwd;      /* encrypted password */
        uid_t   pw_uid;         /* user uid */
        gid_t   pw_gid;         /* user gid */
        time_t  pw_change;      /* password change time */
        char*   pw_class;       /* user access class */
        char*   pw_gecos;       /* Honeywell login info */
        char*   pw_dir;     /* home directory */
        char*   pw_shell;       /* default shell */
        time_t  pw_expire;      /* account expiration */
    }
}
else version (DragonFlyBSD)
{
    struct passwd
    {
        char*   pw_name;        /* user name */
        char*   pw_passwd;      /* encrypted password */
        uid_t   pw_uid;         /* user uid */
        gid_t   pw_gid;         /* user gid */
        time_t  pw_change;      /* password change time */
        char*   pw_class;       /* user access class */
        char*   pw_gecos;       /* Honeywell login info */
        char*   pw_dir;         /* home directory */
        char*   pw_shell;       /* default shell */
        time_t  pw_expire;      /* account expiration */
        int pw_fields;          /* internal: fields filled in */
    }
}
else version (Solaris)
{
    struct passwd
    {
        char* pw_name;
        char* pw_passwd;
        uid_t pw_uid;
        gid_t pw_gid;
        char* pw_age;
        char* pw_comment;
        char* pw_gecos;
        char* pw_dir;
        char* pw_shell;
    }
}
else version (CRuntime_Bionic)
{
    struct passwd
    {
        char*   pw_name;
        char*   pw_passwd;
        uid_t   pw_uid;
        gid_t   pw_gid;
        char*   pw_dir;
        char*   pw_shell;
    }
}
else version (CRuntime_Musl)
{
    struct passwd
    {
        char *pw_name;
        char *pw_passwd;
        uid_t pw_uid;
        gid_t pw_gid;
        char *pw_gecos;
        char *pw_dir;
        char *pw_shell;
    }
}
else version (CRuntime_UClibc)
{
    struct passwd
    {
        char*   pw_name;
        char*   pw_passwd;
        uid_t   pw_uid;
        gid_t   pw_gid;
        char*   pw_gecos;
        char*   pw_dir;
        char*   pw_shell;
    }
}
else
{
    static assert(false, "Unsupported platform");
}

passwd* getpwnam(const scope char*);
passwd* getpwuid(uid_t);

//
// Thread-Safe Functions (TSF)
//
/*
int getpwnam_r(const scope char*, passwd*, char*, size_t, passwd**);
int getpwuid_r(uid_t, passwd*, char*, size_t, passwd**);
*/

version (CRuntime_Glibc)
{
    int getpwnam_r(const scope char*, passwd*, char*, size_t, passwd**);
    int getpwuid_r(uid_t, passwd*, char*, size_t, passwd**);
}
else version (Darwin)
{
    int getpwnam_r(const scope char*, passwd*, char*, size_t, passwd**);
    int getpwuid_r(uid_t, passwd*, char*, size_t, passwd**);
}
else version (FreeBSD)
{
    int getpwnam_r(const scope char*, passwd*, char*, size_t, passwd**);
    int getpwuid_r(uid_t, passwd*, char*, size_t, passwd**);
}
else version (NetBSD)
{
    int __getpwnam_r50(const scope char*, passwd*, char*, size_t, passwd**);
    alias __getpwnam_r50 getpwnam_r;
    int __getpwuid_r50(uid_t, passwd*, char*, size_t, passwd**);
    alias __getpwuid_r50 getpwuid_r;
}
else version (OpenBSD)
{
    int getpwnam_r(const scope char*, passwd*, char*, size_t, passwd**);
    int getpwuid_r(uid_t, passwd*, char*, size_t, passwd**);
}
else version (DragonFlyBSD)
{
    int getpwnam_r(const scope char*, passwd*, char*, size_t, passwd**);
    int getpwuid_r(uid_t, passwd*, char*, size_t, passwd**);
}
else version (Solaris)
{
    alias getpwnam_r = __posix_getpwnam_r;
    alias getpwuid_r = __posix_getpwuid_r;

    // POSIX.1c standard version of the functions
    int __posix_getpwnam_r(const scope char*, passwd*, char*, size_t, passwd**);
    int __posix_getpwuid_r(uid_t, passwd*, char*, size_t, passwd**);
}
else version (CRuntime_Bionic)
{
}
else version (CRuntime_Musl)
{
    int getpwnam_r(const scope char*, passwd*, char*, size_t, passwd**);
    int getpwuid_r(uid_t, passwd*, char*, size_t, passwd**);
}
else version (CRuntime_UClibc)
{
    int getpwnam_r(const scope char*, passwd*, char*, size_t, passwd**);
    int getpwuid_r(uid_t, passwd*, char*, size_t, passwd**);
}
else
{
    static assert(false, "Unsupported platform");
}

//
// XOpen (XSI)
//
/*
void    endpwent();
passwd* getpwent();
void    setpwent();
*/

version (CRuntime_Glibc)
{
    void    endpwent();
    passwd* getpwent();
    void    setpwent();
}
else version (Darwin)
{
    void    endpwent();
    passwd* getpwent();
    void    setpwent();
}
else version (FreeBSD)
{
    void    endpwent();
    passwd* getpwent();
    void    setpwent();
}
else version (NetBSD)
{
    void    endpwent();
    passwd* getpwent();
    void    setpwent();
}
else version (OpenBSD)
{
    void    endpwent();
    passwd* getpwent();
    void    setpwent();
}
else version (DragonFlyBSD)
{
    void    endpwent();
    passwd* getpwent();
    void    setpwent();
}
else version (Solaris)
{
    void endpwent();
    passwd* getpwent();
    void setpwent();
}
else version (CRuntime_Bionic)
{
    void    endpwent();
}
else version (CRuntime_Musl)
{
    void    endpwent();
    passwd* getpwent();
    void    setpwent();
}
else version (CRuntime_UClibc)
{
    void    endpwent();
    passwd* getpwent();
    void    setpwent();
}
else
{
    static assert(false, "Unsupported platform");
}
