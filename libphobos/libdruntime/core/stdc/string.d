/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_string.h.html, _string.h)
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/stdc/_string.d)
 * Standards: ISO/IEC 9899:1999 (E)
 */

module core.stdc.string;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

extern (C):
@system:
nothrow:
@nogc:

///
pure void* memchr(return const void* s, int c, size_t n);
///
pure int   memcmp(scope const void* s1, scope const void* s2, size_t n);
///
pure void* memcpy(return void* s1, scope const void* s2, size_t n);
version (Windows)
{
    ///
    int memicmp(scope const char* s1, scope const char* s2, size_t n);
}
///
pure void* memmove(return void* s1, scope const void* s2, size_t n);
///
pure void* memset(return void* s, int c, size_t n);

///
pure char*  strcpy(return char* s1, scope const char* s2);
///
pure char*  strncpy(return char* s1, scope const char* s2, size_t n);
///
pure char*  strcat(return char* s1, scope const char* s2);
///
pure char*  strncat(return char* s1, scope const char* s2, size_t n);
///
pure int    strcmp(scope const char* s1, scope const char* s2);
///
int    strcoll(scope const char* s1, scope const char* s2);
///
pure int    strncmp(scope const char* s1, scope const char* s2, size_t n);
///
size_t strxfrm(scope char* s1, scope const char* s2, size_t n);
///
pure inout(char)*  strchr(return inout(char)* s, int c);
///
pure size_t strcspn(scope const char* s1, scope const char* s2);
///
pure inout(char)*  strpbrk(return inout(char)* s1, scope const char* s2);
///
pure inout(char)*  strrchr(return inout(char)* s, int c);
///
pure size_t strspn(scope const char* s1, scope const char* s2);
///
pure inout(char)*  strstr(return inout(char)* s1, scope const char* s2);
///
char*  strtok(return char* s1, scope const char* s2);
///
char*  strerror(int errnum);
version (CRuntime_Glibc)
{
    ///
    const(char)* strerror_r(int errnum, return char* buf, size_t buflen);
}
else version (Darwin)
{
    int strerror_r(int errnum, scope char* buf, size_t buflen);
}
else version (FreeBSD)
{
    int strerror_r(int errnum, scope char* buf, size_t buflen);
}
else version (NetBSD)
{
    int strerror_r(int errnum, char* buf, size_t buflen);
}
else version (OpenBSD)
{
    int strerror_r(int errnum, scope char* buf, size_t buflen);
}
else version (DragonFlyBSD)
{
    int strerror_r(int errnum, scope char* buf, size_t buflen);
}
else version (Solaris)
{
    int strerror_r(int errnum, scope char* buf, size_t buflen);
}
else version (CRuntime_Bionic)
{
    ///
    int strerror_r(int errnum, scope char* buf, size_t buflen);
}
else version (CRuntime_Musl)
{
    ///
    int strerror_r(int errnum, scope char *buf, size_t buflen);
}
else version (CRuntime_UClibc)
{
    ///
    const(char)* strerror_r(int errnum, return char* buf, size_t buflen);
}
///
pure size_t strlen(scope const char* s);
///
char*  strdup(scope const char *s);
