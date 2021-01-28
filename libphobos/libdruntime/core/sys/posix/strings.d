/**
 * D header file for POSIX's <strings.h>.
 *
 * Note: Do not mistake this module for <string.h> (singular),
 * available at `core.sys.posix.string`.
 *
 * See_Also:  https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/strings.h.html
 * Copyright: D Language Foundation, 2019
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Mathias 'Geod24' Lang
 * Standards: The Open Group Base Specifications Issue 7, 2018 edition
 * Source:    $(DRUNTIMESRC core/sys/posix/_strings.d)
 */
module core.sys.posix.strings;

version (Posix):
extern(C):
@system:
nothrow:
@nogc:

///
public import core.sys.posix.locale : locale_t;

/// Find first bit set in a word
int ffs(int i) @safe pure;
/// Compare two strings ignoring case
int strcasecmp(scope const char* s1, scope const char* s2);
/// Compare two strings ignoring case, with the specified locale
int strcasecmp_l(scope const char* s1, scope const char* s2, scope locale_t locale);
/// Compare two strings ignoring case, up to n characters
int strncasecmp(scope const char* s1, scope const char* s2, size_t n);
/// Compare two strings ignoring case, with the specified locale, up to n characters
int strncasecmp_l(scope const char* s1, const char* s2, size_t n, locale_t locale);
