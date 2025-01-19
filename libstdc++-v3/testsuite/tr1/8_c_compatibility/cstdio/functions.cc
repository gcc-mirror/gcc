// { dg-do compile }

// 2006-02-05  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2025 Free Software Foundation, Inc.
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

// 8.24 Additions to header <cstdio>

#include <tr1/cstdio>
#include <cstdarg>
#include <cstddef>

void test01(int dummy, ...)
{
  std::va_list ap;
  va_start(ap, dummy);

#if _GLIBCXX_USE_C99_STDIO

  char* s = 0;
  const char* cs = 0;
  const char* format = "%i";
  FILE* stream = va_arg(ap, FILE*);
  std::size_t n = 0;

  int ret;

  ret = std::tr1::snprintf(s, n, format, dummy);
  ret = std::tr1::vsnprintf(s, n, format, ap);

  ret = std::tr1::vfscanf(stream, format, ap);
  ret = std::tr1::vscanf(format, ap);
  ret = std::tr1::vsscanf(cs, format, ap);
  ret = ret; // Suppress unused warning.

#endif
}
