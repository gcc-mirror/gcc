// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

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

// C++ 2017 27.7, range access [iterator.range]

#include <iterator>

void
test01()
{
  using std::reverse_iterator;
  static int i[1];
  static_assert(std::cbegin(i) == i);
  static_assert(std::cend(i) == i+1);
  static_assert(std::rbegin(i) == reverse_iterator<int*>(i+1));
  static_assert(std::rend(i) == reverse_iterator<int*>(i));
  static_assert(std::crbegin(i) == reverse_iterator<int*>(i+1));
  static_assert(std::crend(i) == reverse_iterator<int*>(i));
}

void
test02()
{
  static int i[] = { 1, 2 };
  static_assert(std::distance(std::begin(i), std::end(i)) == 2);
  static_assert(std::distance(std::cbegin(i), std::cend(i)) == 2);
}

void
test03()
{
  using std::reverse_iterator;
  static constexpr std::initializer_list<int> il{1};
  static_assert(std::cbegin(il) == il.begin());
  static_assert(std::cend(il) == il.end());
  static_assert(std::rbegin(il) == reverse_iterator<const int*>(il.end()));
  static_assert(std::rend(il) == reverse_iterator<const int*>(il.begin()));
  static_assert(std::crbegin(il) == reverse_iterator<const int*>(il.end()));
  static_assert(std::crend(il) == reverse_iterator<const int*>(il.begin()));
}
