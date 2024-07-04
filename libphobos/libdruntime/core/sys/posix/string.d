/**
 * D header file for POSIX's <string.h>.
 *
 * Note:
 * - The <string.h> header shall define NULL and size_t as described in <stddef.h>.
 *   However, D has builtin `null` and `size_t` is defined in `object`.
 *
 * See_Also:  https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/string.h.html
 * Copyright: D Language Foundation, 2019
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Mathias 'Geod24' Lang
 * Standards: The Open Group Base Specifications Issue 7, 2018 edition
 * Source:    $(DRUNTIMESRC core/sys/posix/_string.d)
 */
module core.sys.posix.string;

version (Posix):
extern(C):
nothrow:
@nogc:

/// Exposes `locale_t` as defined in `core.sys.posix.locale` (`<locale.h>`)
public import core.sys.posix.locale : locale_t;

/**
 * Exposes the C99 functions
 *
 * C extensions and XSI extensions are missing
 */
public import core.stdc.string;

/// Copy string until character found
void*  memccpy(return scope void* dst, scope const void* src, int c, size_t n) pure;
/// Copy string (including terminating '\0')
char*  stpcpy(return scope char* dst, scope const char* src) pure;
/// Ditto
char*  stpncpy(return scope char* dst, const char* src, size_t len) pure;
/// Compare strings according to current collation
int    strcoll_l(scope const char* s1, scope const char* s2, locale_t locale);
///
char*  strerror_l(int, locale_t);
/// Find length of string up to `maxlen`
size_t strnlen(scope const char* str, size_t maxlen) pure;
/// System signal messages
const(char)*  strsignal(int);
/// Isolate sequential tokens in a null-terminated string
char*  strtok_r(return scope char* str, scope const char* sep, char** context) pure;
/// Transform a string under locale
size_t strxfrm_l(char* s1, scope const char* s2, size_t n, locale_t locale);
