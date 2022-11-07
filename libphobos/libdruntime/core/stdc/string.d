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

// Those libs don't expose the mandated C interface
version (CRuntime_Glibc)
    version = ReturnStrerrorR;
else version (CRuntime_UClibc)
    version = ReturnStrerrorR;

extern (C):
nothrow:
@nogc:

///
inout(void)* memchr(return scope inout void* s, int c, size_t n) pure;
///
int   memcmp(scope const void* s1, scope const void* s2, size_t n) pure;
///
void* memcpy(return scope void* s1, scope const void* s2, size_t n) pure;
version (Windows)
{
    ///
    int memicmp(scope const char* s1, scope const char* s2, size_t n);
}
///
void* memmove(return scope void* s1, scope const void* s2, size_t n) pure;
///
void* memset(return scope void* s, int c, size_t n) pure;

///
char*  strcat(return scope char* s1, scope const char* s2) pure;
///
inout(char)*  strchr(return scope inout(char)* s, int c) pure;
///
int    strcmp(scope const char* s1, scope const char* s2) pure;
///
int    strcoll(scope const char* s1, scope const char* s2);
///
char*  strcpy(return scope char* s1, scope const char* s2) pure;
///
size_t strcspn(scope const char* s1, scope const char* s2) pure;
///
char*  strdup(scope const char *s);
///
char*  strerror(int errnum);
// This `strerror_r` definition is not following the POSIX standard
version (ReturnStrerrorR)
{
    ///
    const(char)* strerror_r(int errnum, return scope char* buf, size_t buflen);
}
// This one is
else
{
    int strerror_r(int errnum, scope char* buf, size_t buflen);
}
///
size_t strlen(scope const char* s) pure;
///
char*  strncat(return scope char* s1, scope const char* s2, size_t n) pure;
///
int    strncmp(scope const char* s1, scope const char* s2, size_t n) pure;
///
char*  strncpy(return scope char* s1, scope const char* s2, size_t n) pure;
///
inout(char)*  strpbrk(return scope inout(char)* s1, scope const char* s2) pure;
///
inout(char)*  strrchr(return scope inout(char)* s, int c) pure;
///
size_t strspn(scope const char* s1, scope const char* s2) pure;
///
inout(char)*  strstr(return scope inout(char)* s1, scope const char* s2) pure;
///
char*  strtok(return scope char* s1, scope const char* s2);
///
size_t strxfrm(scope char* s1, scope const char* s2, size_t n);
