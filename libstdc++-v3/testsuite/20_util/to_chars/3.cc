// Copyright (C) 2019 Free Software Foundation, Inc.
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
// { dg-do run { target c++17 } }
// { dg-require-string-conversions "" }

#include <charconv>
#include <string_view>
#include <testsuite_hooks.h>

template<typename C>
bool
check_to_chars(C val)
{
  using std::string_view;

  char buf1[32], buf2[32], buf3[32];
  std::to_chars_result r1 = std::to_chars(buf1, buf1+sizeof(buf1), val);
  if (r1.ec != std::errc{})
    return false;
  std::to_chars_result r2 = std::to_chars(buf2, buf2+sizeof(buf2), val, 10);
  if (r2.ec != std::errc{})
    return false;
  if (string_view(buf1, r1.ptr - buf1) != string_view(buf2, r2.ptr - buf2))
    return false;
  std::to_chars_result r3 = std::to_chars(buf3, buf3+sizeof(buf3), (long)val);
  if (string_view(buf1, r1.ptr - buf1) != string_view(buf3, r3.ptr - buf3))
    return false;
  return true;
}

void
test01()
{
  VERIFY( check_to_chars(u'\x21') );
  VERIFY( check_to_chars(U'\x21') );
#if _GLIBCXX_USE_WCHAR_T
  VERIFY( check_to_chars(L'\x21') );
#endif
}

int main()
{
  test01();
}
