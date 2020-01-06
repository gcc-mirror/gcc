// Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }
// { dg-require-normal-namespace "" }

#include <charconv>

namespace std
{
  struct from_chars_result;

  const char* from_chars_result::*pm2 = &from_chars_result::ptr;
  errc from_chars_result::*pm1 = &from_chars_result::ec;

  from_chars_result (*f1)(const char*, const char*, char&, int)
    = &from_chars;
  from_chars_result (*f2)(const char*, const char*, signed char&, int)
    = &from_chars;
  from_chars_result (*f3)(const char*, const char*, unsigned char&, int)
    = &from_chars;
  from_chars_result (*f4)(const char*, const char*, signed short&, int)
    = &from_chars;
  from_chars_result (*f5)(const char*, const char*, unsigned short&, int)
    = &from_chars;
  from_chars_result (*f6)(const char*, const char*, signed int&, int)
    = &from_chars;
  from_chars_result (*f7)(const char*, const char*, unsigned int&, int)
    = &from_chars;
  from_chars_result (*f8)(const char*, const char*, signed long&, int)
    = &from_chars;
  from_chars_result (*f9)(const char*, const char*, unsigned long&, int)
    = &from_chars;
  from_chars_result (*f10)(const char*, const char*, signed long long&, int)
    = &from_chars;
  from_chars_result (*f11)(const char*, const char*, unsigned long long&, int)
    = &from_chars;
}

void bind()
{
  const char buf[1] = "";
  int i;
  auto [p, e] = std::from_chars(buf, buf + 1, i, 10);
  const char** pa = &p;
  std::errc* ea = &e;
}
