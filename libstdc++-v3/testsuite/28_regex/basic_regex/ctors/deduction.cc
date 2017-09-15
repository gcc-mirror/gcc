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

#include <regex>
#include <testsuite_iterators.h>

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test01()
{
  std::basic_regex x("");
  check_type<std::basic_regex<char>>(x);
  char s[1] = {};
  std::basic_regex x2(s);
  check_type<std::basic_regex<char>>(x2);
  std::basic_regex x3(U"");
  check_type<std::basic_regex<char32_t>>(x3);
  std::basic_regex x4(U"", std::regex_constants::grep);
  check_type<std::basic_regex<char32_t>>(x4);

  // Test explicit guide:
  std::basic_regex x5(s, s+1);
  check_type<std::basic_regex<char>>(x5);
  std::basic_regex x6((const char*)s, (const char*)s+1);
  check_type<std::basic_regex<char>>(x6);
  std::basic_regex x7(s, s+1, std::regex_constants::grep);
  check_type<std::basic_regex<char>>(x7);
  __gnu_test::test_container<char, __gnu_test::forward_iterator_wrapper> f(s);
  std::basic_regex x8(f.begin(), f.end());
  check_type<std::basic_regex<char>>(x8);
  std::basic_regex x9(f.begin(), f.end(), std::regex_constants::grep);
  check_type<std::basic_regex<char>>(x9);

  std::basic_regex copy = x;
  check_type<std::basic_regex<char>>(copy);
  std::basic_regex move = std::move(x);
  check_type<std::basic_regex<char>>(move);
}
