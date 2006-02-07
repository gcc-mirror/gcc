// { dg-do compile }

// 2006-01-30  Paolo Carlini  <pcarlini@suse.de>
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

// 8.11 Header <cinttypes>

#include <tr1/cinttypes>

void test01()
{
#if _GLIBCXX_USE_C99_INTTYPES_TR1

  std::tr1::intmax_t i = 0, numer = 0, denom = 0, base = 0;
  const char* s = 0;
  char** endptr = 0;
#ifdef _GLIBCXX_USE_WCHAR_T
  const wchar_t* ws = 0;
  wchar_t** wendptr = 0;
#endif  

  std::tr1::intmax_t  ret;
  std::tr1::uintmax_t uret;
  std::tr1::imaxdiv_t dret;

  ret = std::tr1::imaxabs(i);
  // ret = std::tr1::abs(i);

  dret = std::tr1::imaxdiv(numer, denom);
  // dret = std::tr1::div(numer, denom);

  ret = std::tr1::strtoimax(s, endptr, base);
  uret = std::tr1::strtoumax(s, endptr, base);

#ifdef _GLIBCXX_USE_WCHAR_T
  ret = std::tr1::wcstoimax(ws, wendptr, base);
  uret = std::tr1::wcstoumax(ws, wendptr, base);
#endif

#endif
}
