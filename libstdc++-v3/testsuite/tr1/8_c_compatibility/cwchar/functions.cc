// { dg-do compile }

// 2006-02-03  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 8.6 Additions to header <cwchar>

#include <tr1/cwchar>
#include <cstdio>
#include <cstdarg>

#if _GLIBCXX_USE_WCHAR_T

void test01()
{
#if _GLIBCXX_USE_C99_WCHAR_TR1

  const wchar_t* nptr = 0;
  const wchar_t* format = 0;
  const wchar_t* s = 0;
  wchar_t** endptr = 0;
  FILE* stream = 0;
  std::va_list arg = 0;

  float fret;
  long double ldret;
  int ret;

  fret = std::tr1::wcstof(nptr, endptr);
  ldret = std::tr1::wcstold(nptr, endptr);
  ret = std::tr1::vfwscanf(stream, format, arg);
  ret = std::tr1::vswscanf(s, format, arg);
  ret = std::tr1::vwscanf(format, arg);

#ifdef _GLIBCXX_USE_LONG_LONG
  int base = 0;
  long long llret;
  unsigned long long ullret;
  llret = std::tr1::wcstoll(nptr, endptr, base);
  ullret = std::tr1::wcstoull(nptr, endptr, base);
#endif

#endif
}

#endif
