// { dg-do compile }

// 2006-02-03  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2018 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 8.6 Additions to header <cwchar>

#include <tr1/cwchar>
#include <cstdio>
#include <cstdarg>

#if _GLIBCXX_USE_WCHAR_T

void test01(int dummy, ...)
{
  std::va_list arg;
  va_start(arg, dummy);

#if _GLIBCXX_HAVE_WCSTOF
  const wchar_t* nptr1 = 0;
  wchar_t** endptr1 = 0;
  float fret;
  fret = std::tr1::wcstof(nptr1, endptr1);

  fret = fret; // Suppress unused warning.
#endif

#if _GLIBCXX_HAVE_VFWSCANF
  FILE* stream = 0;
  const wchar_t* format1 = 0;
  int ret1;
  ret1 = std::tr1::vfwscanf(stream, format1, arg);

  ret1 = ret1; // Suppress unused warning.
#endif

#if _GLIBCXX_HAVE_VSWSCANF
  const wchar_t* s = 0;
  const wchar_t* format2 = 0;
  int ret2;
  ret2 = std::tr1::vswscanf(s, format2, arg);

  ret2 = ret2; // Suppress unused warning.
#endif

#if _GLIBCXX_HAVE_VWSCANF
  const wchar_t* format3 = 0;
  int ret3;
  ret3 = std::tr1::vwscanf(format3, arg);

  ret3 = ret3; // Suppress unused warning.
#endif

#if _GLIBCXX_USE_C99_WCHAR

  const wchar_t* nptr2 = 0;
  wchar_t** endptr2 = 0;
  long double ldret;
  ldret = std::tr1::wcstold(nptr2, endptr2);

  int base = 0;
  long long llret;
  unsigned long long ullret;
  llret = std::tr1::wcstoll(nptr2, endptr2, base);
  ullret = std::tr1::wcstoull(nptr2, endptr2, base);

  ldret = ldret; // Suppress unused warnings.
  llret = llret;
  ullret = ullret;

#endif
}

#endif
