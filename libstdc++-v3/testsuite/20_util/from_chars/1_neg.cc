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

#include <charconv>

void
test01(const char* first, const char* last)
{
#if _GLIBCXX_USE_WCHAR_T
  wchar_t wc;
  std::from_chars(first, last, wc); // { dg-error "no matching" }
  std::from_chars(first, last, wc, 10); // { dg-error "no matching" }
#endif

  char16_t c16;
  std::from_chars(first, last, c16); // { dg-error "no matching" }
  std::from_chars(first, last, c16, 10); // { dg-error "no matching" }
  char32_t c32;
  std::from_chars(first, last, c32); // { dg-error "no matching" }
  std::from_chars(first, last, c32, 10); // { dg-error "no matching" }
}
