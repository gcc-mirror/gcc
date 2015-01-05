// Prototypes for GLIBC thread locale __-prefixed functions -*- C++ -*-

// Copyright (C) 2002-2015 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/c++locale_internal.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{locale}
 */

// Written by Jakub Jelinek <jakub@redhat.com>

#include <bits/c++config.h>
#include <clocale>
#include <cstdlib>
#include <cstring>
#include <cstddef>
#include <langinfo.h>

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
                                                  
extern "C" __typeof(nl_langinfo_l) __nl_langinfo_l;
extern "C" __typeof(strcoll_l) __strcoll_l;
extern "C" __typeof(strftime_l) __strftime_l;
extern "C" __typeof(strtod_l) __strtod_l;
extern "C" __typeof(strtof_l) __strtof_l;
extern "C" __typeof(strtold_l) __strtold_l;
extern "C" __typeof(strxfrm_l) __strxfrm_l;
extern "C" __typeof(newlocale) __newlocale;
extern "C" __typeof(freelocale) __freelocale;
extern "C" __typeof(duplocale) __duplocale;
extern "C" __typeof(uselocale) __uselocale;

#ifdef _GLIBCXX_USE_WCHAR_T
extern "C" __typeof(iswctype_l) __iswctype_l;
extern "C" __typeof(towlower_l) __towlower_l;
extern "C" __typeof(towupper_l) __towupper_l;
extern "C" __typeof(wcscoll_l) __wcscoll_l;
extern "C" __typeof(wcsftime_l) __wcsftime_l;
extern "C" __typeof(wcsxfrm_l) __wcsxfrm_l;
extern "C" __typeof(wctype_l) __wctype_l;
#endif 

#endif // GLIBC 2.3 and later
