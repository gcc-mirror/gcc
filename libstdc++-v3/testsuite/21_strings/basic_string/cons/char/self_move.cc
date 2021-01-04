// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <string>
#include <debug/string>
#include <testsuite_hooks.h>

template<typename String>
void
test(const char* s)
{
  String s1 = s;
  std::string s2 __attribute__((unused)) = s1.c_str();
  s1 = std::move(s1);

  String s3 __attribute__((unused)) = s1;
  s1 = std::move(s1);

  s1.begin(); // causes COW string to "leak"
  s1 = std::move(s1);

  String s4 __attribute__((unused)) = s1;
  s1 = std::move(s1);

  s1.reserve(2 * s1.capacity()); // causes SSO string to be on the heap
  s1 = std::move(s1);
}

int
main()
{
  test<std::string>("short");
  test<std::string>("very, very, very, VERY long");
  test<__gnu_debug::string>("short");
  test<__gnu_debug::string>("very, very, very, VERY long");
}
