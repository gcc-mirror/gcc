// { dg-do compile }

// 2006-02-07  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2020 Free Software Foundation, Inc.
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

// 8.25 Additions to header <cstdlib>

#include <tr1/cstdlib>

#if _GLIBCXX_HOSTED

void test01()
{
#if _GLIBCXX_USE_C99_STDLIB

  long long i = 0;
  const char* s = "";
  char** endptr = 0;
  int base = 0;

  long long ret;
  unsigned long long uret;
  float fret;
  long double ldret;

#if !_GLIBCXX_USE_C99_LONG_LONG_DYNAMIC
  long long numer = 0, denom = 0;
  std::tr1::lldiv_t dret;

  ret = std::tr1::llabs(i);
  dret = std::tr1::lldiv(numer, denom);
#endif

  ret = std::tr1::atoll(s);
  ret = std::tr1::strtoll(s, endptr, base);
  uret = std::tr1::strtoull(s, endptr, base);

  fret = std::tr1::strtof(s, endptr);
  ldret = std::tr1::strtold(s, endptr);

  ret = std::tr1::abs(i);

  ret = ret; // Suppress unused warning.
  uret = uret;
  fret = fret;
  ldret = ldret;

#if !_GLIBCXX_USE_C99_LONG_LONG_DYNAMIC
  dret = std::tr1::div(numer, denom);

  dret = dret;
#endif

#endif
}

#endif
