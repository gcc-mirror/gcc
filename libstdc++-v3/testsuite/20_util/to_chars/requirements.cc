// Copyright (C) 2017 Free Software Foundation, Inc.
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
  struct to_chars_result;

  char* to_chars_result::*pm2 = &to_chars_result::ptr;
  errc to_chars_result::*pm1 = &to_chars_result::ec;

  to_chars_result (*f1)(char*, char*, char, int) = &to_chars;
  to_chars_result (*f2)(char*, char*, signed char, int) = &to_chars;
  to_chars_result (*f3)(char*, char*, unsigned char, int) = &to_chars;
  to_chars_result (*f4)(char*, char*, signed short, int) = &to_chars;
  to_chars_result (*f5)(char*, char*, unsigned short, int) = &to_chars;
  to_chars_result (*f6)(char*, char*, signed int, int) = &to_chars;
  to_chars_result (*f7)(char*, char*, unsigned int, int) = &to_chars;
  to_chars_result (*f8)(char*, char*, signed long, int) = &to_chars;
  to_chars_result (*f9)(char*, char*, unsigned long, int) = &to_chars;
  to_chars_result (*f10)(char*, char*, signed long long, int) = &to_chars;
  to_chars_result (*f11)(char*, char*, unsigned long long, int) = &to_chars;
}

void bind()
{
  char buf[1];
  auto [p, e] = std::to_chars(buf, buf + 1, 1, 10);
  char** pa = &p;
  std::errc* ea = &e;
}
