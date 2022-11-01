/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.inttypes;

import core.sys.posix.config;
public import core.stdc.inttypes;

version (Posix):
extern (C) nothrow @nogc:

//
// Required
//
/*
intmax_t  imaxabs(intmax_t);
imaxdiv_t imaxdiv(intmax_t, intmax_t);
intmax_t  strtoimax(const scope char*, char**, int);
uintmax_t strtoumax(const scope char*, char**, int);
intmax_t  wcstoimax(const scope wchar_t*, wchar_t**, int);
uintmax_t wcstoumax(const scope wchar_t*, wchar_t**, int);
*/

intmax_t  imaxabs(intmax_t);
imaxdiv_t imaxdiv(intmax_t, intmax_t);
intmax_t  strtoimax(const scope char*, char**, int);
uintmax_t strtoumax(const scope char*, char**, int);
intmax_t  wcstoimax(const scope wchar_t*, wchar_t**, int);
uintmax_t wcstoumax(const scope wchar_t*, wchar_t**, int);
