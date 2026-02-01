/**
 * D header file for C99/C11.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_time.h.html, _time.h)
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly,
 *            Alex Rønne Petersen
 * Source:    $(DRUNTIMESRC core/sys/windows/stdc/_time.d)
 * Standards: ISO/IEC 9899:1999 (E)
 */

module core.sys.windows.stdc.time;

version (Windows):

import core.stdc.config;

extern (C):
@trusted: // There are only a few functions here that use unsafe C strings.
nothrow:
@nogc:

///
struct tm
{
    int     tm_sec;     /// seconds after the minute - [0, 60]
    int     tm_min;     /// minutes after the hour - [0, 59]
    int     tm_hour;    /// hours since midnight - [0, 23]
    int     tm_mday;    /// day of the month - [1, 31]
    int     tm_mon;     /// months since January - [0, 11]
    int     tm_year;    /// years since 1900
    int     tm_wday;    /// days since Sunday - [0, 6]
    int     tm_yday;    /// days since January 1 - [0, 365]
    int     tm_isdst;   /// Daylight Saving Time flag
}

///
alias c_long time_t;
///
alias c_long clock_t;

enum clock_t CLOCKS_PER_SEC = 1000;
clock_t clock();

///
void  tzset();                           // non-standard
///
void  _tzset();                          // non-standard
///
@system char* _strdate(return scope char* s);                 // non-standard
///
@system char* _strtime(return scope char* s);                 // non-standard

///
extern __gshared const(char)*[2] tzname; // non-standard

// timespec functions, introduced in C11
alias __time64_t = long;
alias __time32_t = int;

/// 32-bit timespec struct
struct _timespec32
{
    __time32_t tv_sec;
    c_long     tv_nsec;
}

/// 64-bit timespec struct
struct _timespec64
{
    __time64_t tv_sec;
    c_long     tv_nsec;
}

/// Timespec structure, introduced in C11
alias timespec = _timespec64;

/// Base Value used for timespec_get
enum TIME_UTC = 1;

/// 64-bit version of timespec_get for Windows
@system int _timespec64_get(scope _timespec64* ts, int base);

/// 32-bit version of timespec_get for Windows
@system int _timespec32_get(scope _timespec32* ts, int base);

/// timespec_get, introduced in C11
alias timespec_get = _timespec64_get;
