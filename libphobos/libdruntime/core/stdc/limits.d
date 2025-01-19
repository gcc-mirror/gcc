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

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

import core.stdc.config;

extern (C):
@trusted: // Constants only.
nothrow:
@nogc:

//
// Numerical limits
//

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

//
// File system limits
//

version (Darwin)
{
    ///
    enum MAX_CANON      = 1024;
    ///
    enum MAX_INPUT      = 1024;
    ///
    enum NAME_MAX       = 255;
    ///
    enum PATH_MAX       = 1024;
    ///
    enum PIPE_BUF       = 512;
}
else version (DragonFlyBSD)
{
    ///
    enum MAX_CANON      = 255;
    ///
    enum MAX_INPUT      = 255;
    ///
    enum NAME_MAX       = 255;
    ///
    enum PATH_MAX       = 1024;
    ///
    enum PIPE_BUF       = 512;
}
else version (FreeBSD)
{
    ///
    enum MAX_CANON      = 255;
    ///
    enum MAX_INPUT      = 255;
    ///
    enum NAME_MAX       = 255;
    ///
    enum PATH_MAX       = 1024;
    ///
    enum PIPE_BUF       = 512;
}
else version (linux)
{
    ///
    enum MAX_CANON      = 255;
    ///
    enum MAX_INPUT      = 255;
    ///
    enum NAME_MAX       = 255;
    ///
    enum PATH_MAX       = 4096;
    ///
    enum PIPE_BUF       = 4096;
}
else version (NetBSD)
{
    ///
    enum MAX_CANON      = 255;
    ///
    enum MAX_INPUT      = 255;
    ///
    enum NAME_MAX       = 511;
    ///
    enum PATH_MAX       = 1024;
    ///
    enum PIPE_BUF       = 512;
}
else version (OpenBSD)
{
    ///
    enum MAX_CANON      = 255;
    ///
    enum MAX_INPUT      = 255;
    ///
    enum NAME_MAX       = 255;
    ///
    enum PATH_MAX       = 1024;
    ///
    enum PIPE_BUF       = 512;
}
else version (Solaris)
{
    ///
    enum MAX_CANON      = 256;
    ///
    enum MAX_INPUT      = 512;
    ///
    enum NAME_MAX       = 255;
    ///
    enum PATH_MAX       = 1024;
    ///
    enum PIPE_BUF       = 5120;
}
else version (Windows)
{
    ///
    enum MAX_CANON      = 256;
    ///
    enum MAX_INPUT      = 256;
    ///
    enum NAME_MAX       = 256;
    ///
    enum PATH_MAX       = 260;
    ///
    enum PIPE_BUF       = 5120;
}
else
    static assert(0, "unsupported OS");
