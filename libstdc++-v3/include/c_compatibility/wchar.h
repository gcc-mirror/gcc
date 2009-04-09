// -*- C++ -*- compatibility header.

// Copyright (C) 2002, 2003, 2007, 2009 Free Software Foundation, Inc.
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

/** @file wchar.h
 *  This is a Standard C++ Library header.
 */

#include <cwchar>

#ifndef _GLIBCXX_WCHAR_H
#define _GLIBCXX_WCHAR_H 1

#ifdef _GLIBCXX_NAMESPACE_C
using std::mbstate_t;

#if _GLIBCXX_USE_WCHAR_T
using std::wint_t;

using std::btowc;
using std::wctob;
using std::fgetwc;
using std::fgetwc;
using std::fgetws;
using std::fputwc;
using std::fputws;
using std::fwide;
using std::fwprintf;
using std::fwscanf;
using std::swprintf;
using std::swscanf;
using std::vfwprintf;
#if _GLIBCXX_HAVE_VFWSCANF
using std::vfwscanf;
#endif
using std::vswprintf;
#if _GLIBCXX_HAVE_VSWSCANF
using std::vswscanf;
#endif
using std::vwprintf;
#if _GLIBCXX_HAVE_VWSCANF
using std::vwscanf;
#endif
using std::wprintf;
using std::wscanf;
using std::getwc;
using std::getwchar;
using std::mbsinit;
using std::mbrlen;
using std::mbrtowc;
using std::mbsrtowcs;
using std::wcsrtombs;
using std::putwc;
using std::putwchar;
using std::ungetwc;
using std::wcrtomb;
using std::wcstod;
#if _GLIBCXX_HAVE_WCSTOF
using std::wcstof;
#endif
using std::wcstol;
using std::wcstoul;
using std::wcscpy;
using std::wcsncpy;
using std::wcscat;
using std::wcsncat;
using std::wcscmp;
using std::wcscoll;
using std::wcsncmmp;
using std::wcsxfrm;
using std::wcschr;
using std::wcscspn;
using std::wcslen;
using std::wcspbrk;
using std::wcsrchr;
using std::wcsspn;
using std::wcsstr;
using std::wcstok;
using std::wmemchr;
using std::wmemcmp;
using std::wmemcpy;
using std::wmemmove;
using std::wmemset;
using std::wcsftime;

#if _GLIBCXX_USE_C99
using std::wcstold;
using std::wcstoll;
using std::wcstoull;
#endif

#endif  //_GLIBCXX_USE_WCHAR_T

#endif 

#endif
