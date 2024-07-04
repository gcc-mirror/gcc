// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <string_view>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

#if __STDC_HOSTED__
# include <vector>
#endif // HOSTED

constexpr char str[] = "abcdefg";
constexpr std::basic_string_view<char> s(std::begin(str), std::cend(str) - 1);
static_assert( s == str );
static_assert( s.data() == str );
constexpr std::basic_string_view ctad(std::begin(str), std::cend(str) - 1);
static_assert( ctad == s );

// The standard does not require this constructor to have a noexcept-specifier.
static_assert( noexcept(std::basic_string_view<char>(str, str)) );
using I = __gnu_test::contiguous_iterator_wrapper<char>;
static_assert( ! noexcept(std::basic_string_view<char>(I{}, I{})) );

void
test01()
{
#if __STDC_HOSTED__
  std::vector<char> v{'a', 'b', 'c'};
  std::basic_string_view<char> s(v.begin(), v.end());
  VERIFY( s.data() == v.data() );
  std::basic_string_view ctad(v.begin(), v.end());
  VERIFY( ctad == s );
#endif // HOSTED
}

int
main()
{
  test01();
}
