// { dg-do compile }

// 2006-02-05  Paolo Carlini  <pcarlini@suse.de>
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

// 8.24 Additions to header <cstdio>

#include <tr1/cstdio>
#include <cstdarg>
#include <cstddef>

void test01(int dummy, ...)
{
  std::va_list ap;
  va_start(ap, dummy);

#if _GLIBCXX_USE_C99

  char* s = 0;
  const char* cs = 0;
  const char* format = "%i";
  FILE* stream = 0;
  std::size_t n = 0;

  int ret;

  ret = std::tr1::snprintf(s, n, format, dummy);
  ret = std::tr1::vsnprintf(s, n, format, ap);

  ret = std::tr1::vfscanf(stream, format, ap); 
  ret = std::tr1::vscanf(format, ap);
  ret = std::tr1::vsscanf(cs, format, ap);
  
#endif
}
