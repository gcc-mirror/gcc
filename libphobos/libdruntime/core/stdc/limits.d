/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_limits.h.html, _limits.h)
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/stdc/_limits.d)
 * Standards: ISO/IEC 9899:1999 (E)
 */

module core.stdc.limits;

private import core.stdc.config;

extern (C):
@trusted: // Constants only.
nothrow:
@nogc:

///
enum CHAR_BIT       = 8;
///
enum SCHAR_MIN      = byte.min;
///
enum SCHAR_MAX      = byte.max;
///
enum UCHAR_MAX      = ubyte.max;
///
enum CHAR_MIN       = char.min;
///
enum CHAR_MAX       = char.max;
///
enum MB_LEN_MAX     = 2;
///
enum SHRT_MIN       = short.min;
///
enum SHRT_MAX       = short.max;
///
enum USHRT_MAX      = ushort.max;
///
enum INT_MIN        = int.min;
///
enum INT_MAX        = int.max;
///
enum UINT_MAX       = uint.max;
///
enum LONG_MIN       = c_long.min;
///
enum LONG_MAX       = c_long.max;
///
enum ULONG_MAX      = c_ulong.max;
///
enum LLONG_MIN      = long.min;
///
enum LLONG_MAX      = long.max;
///
enum ULLONG_MAX     = ulong.max;
