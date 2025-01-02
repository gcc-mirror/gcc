// { dg-do run { target c++14 } }

// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

// C++ 2014 24.7, range access [iterator.range]

#include <iterator>
#include <vector>
#include <testsuite_hooks.h>

void
test01()
{
  int i[1];
  VERIFY(std::cbegin(i) == i);
  VERIFY(std::cend(i) == i+1);
  VERIFY(std::rbegin(i) == std::reverse_iterator<int*>(i+1));
  VERIFY(std::rend(i) == std::reverse_iterator<int*>(i));
  VERIFY(std::crbegin(i) == std::reverse_iterator<int*>(i+1));
  VERIFY(std::crend(i) == std::reverse_iterator<int*>(i));
}

void
test02()
{
  static int i[1];
  // LWG 2280
  constexpr auto b  __attribute__((unused)) = std::begin(i);
  constexpr auto e  __attribute__((unused)) = std::end(i);
  constexpr auto cb __attribute__((unused)) = std::cbegin(i);
  constexpr auto ce __attribute__((unused)) = std::cend(i);

  // LWG 2280
  static_assert( noexcept(std::begin(i)),  "LWG 2280" );
  static_assert( noexcept(std::end(i)),    "LWG 2280" );
  static_assert( noexcept(std::cbegin(i)), "LWG 2280" );
  static_assert( noexcept(std::cend(i)),   "LWG 2280" );

  // LWG 3537
  static_assert( noexcept(std::rbegin(i)),  "LWG 3537" );
  static_assert( noexcept(std::rend(i)),    "LWG 3537" );
}

void
test03()
{
  std::initializer_list<int> il{1};
  VERIFY(std::cbegin(il) == il.begin());
  VERIFY(std::cend(il) == il.end());
  VERIFY(std::rbegin(il) == std::reverse_iterator<const int*>(il.end()));
  VERIFY(std::rend(il) == std::reverse_iterator<const int*>(il.begin()));
  VERIFY(std::crbegin(il) == std::reverse_iterator<const int*>(il.end()));
  VERIFY(std::crend(il) == std::reverse_iterator<const int*>(il.begin()));

  // LWG 3537
  static_assert( noexcept(std::rbegin(il)),  "LWG 3537" );
  static_assert( noexcept(std::rend(il)),    "LWG 3537" );
}

void
test04()
{
  std::vector<int> v{1};
  VERIFY(std::cbegin(v) == v.cbegin());
  VERIFY(std::cend(v) == v.cend());
  VERIFY(std::rbegin(v) == v.rbegin());
  VERIFY(std::rend(v) == v.rend());
  VERIFY(std::crbegin(v) == v.crbegin());
  VERIFY(std::crend(v) == v.crend());
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
