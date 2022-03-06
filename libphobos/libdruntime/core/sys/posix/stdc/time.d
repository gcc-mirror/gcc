/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_time.h.html, _time.h)
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly,
 *            Alex Rønne Petersen
 * Source:    $(DRUNTIMESRC core/stdc/_time.d)
 * Standards: ISO/IEC 9899:1999 (E)
 */

module core.sys.posix.stdc.time;

version (Posix):

import core.stdc.config;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

extern (C):
@trusted: // There are only a few functions here that use unsafe C strings.
nothrow:
@nogc:

///
struct tm
{
    int     tm_sec;     /// seconds after the minute [0-60]
    int     tm_min;     /// minutes after the hour [0-59]
    int     tm_hour;    /// hours since midnight [0-23]
    int     tm_mday;    /// day of the month [1-31]
    int     tm_mon;     /// months since January [0-11]
    int     tm_year;    /// years since 1900
    int     tm_wday;    /// days since Sunday [0-6]
    int     tm_yday;    /// days since January 1 [0-365]
    int     tm_isdst;   /// Daylight Savings Time flag
    c_long  tm_gmtoff;  /// offset from CUT in seconds
    char*   tm_zone;    /// timezone abbreviation
}

public import core.sys.posix.sys.types : time_t, clock_t;

///
version (CRuntime_Glibc)
{
    enum clock_t CLOCKS_PER_SEC = 1_000_000;
    clock_t clock();
}
else version (CRuntime_Musl)
{
    enum clock_t CLOCKS_PER_SEC = 1_000_000;
    clock_t clock();
}
else version (CRuntime_Bionic)
{
    enum clock_t CLOCKS_PER_SEC = 1_000_000;
    clock_t clock();
}
else version (CRuntime_UClibc)
{
    enum clock_t CLOCKS_PER_SEC = 1_000_000;
    clock_t clock();
}
else version (OSX)
{
    enum clock_t CLOCKS_PER_SEC = 1_000_000; // was 100 until OSX 10.4/10.5
    version (X86)
        extern (C) pragma(mangle, "clock$UNIX2003") clock_t clock();
    else
        clock_t clock();
}
else version (Darwin) // other Darwins (iOS, TVOS, WatchOS)
{
    enum clock_t CLOCKS_PER_SEC = 1_000_000;
    clock_t clock();
}
else version (FreeBSD)
{
    enum clock_t CLOCKS_PER_SEC = 128;
    clock_t clock();
}
else version (NetBSD)
{
    enum clock_t CLOCKS_PER_SEC = 100;
    clock_t clock();
}
else version (OpenBSD)
{
    enum clock_t CLOCKS_PER_SEC = 100;
    clock_t clock();
}
else version (DragonFlyBSD)
{
    enum clock_t CLOCKS_PER_SEC = 128;
    clock_t clock();
}
else version (Solaris)
{
    enum clock_t CLOCKS_PER_SEC = 1_000_000;
    clock_t clock();
}
else
{
    static assert(0, "unsupported system");
}

version (Darwin)
{
    ///
    void tzset();                            // non-standard
    ///
    extern __gshared const(char)*[2] tzname; // non-standard
}
else version (CRuntime_Glibc)
{
    ///
    void tzset();                            // non-standard
    ///
    extern __gshared const(char)*[2] tzname; // non-standard
}
else version (FreeBSD)
{
    ///
    void tzset();                            // non-standard
    ///
    extern __gshared const(char)*[2] tzname; // non-standard
}
else version (NetBSD)
{
    ///
    void tzset();                            // non-standard
    ///
    extern __gshared const(char)*[2] tzname; // non-standard
}
else version (OpenBSD)
{
    ///
    void tzset();                            // non-standard
    ///
    extern __gshared const(char)*[2] tzname; // non-standard
}
else version (DragonFlyBSD)
{
    ///
    void tzset();                            // non-standard
    ///
    extern __gshared const(char)*[2] tzname; // non-standard
}
else version (Solaris)
{
    ///
    void tzset();
    ///
    extern __gshared const(char)*[2] tzname;
}
else version (CRuntime_Bionic)
{
    ///
    void tzset();
    ///
    extern __gshared const(char)*[2] tzname;
}
else version (CRuntime_Musl)
{
    ///
    void tzset();                            // non-standard
    ///
    extern __gshared const(char)*[2] tzname; // non-standard
}
else version (CRuntime_UClibc)
{
    ///
    void tzset();
    ///
    extern __gshared const(char)*[2] tzname;
}
else
{
    static assert(false, "Unsupported platform");
}
