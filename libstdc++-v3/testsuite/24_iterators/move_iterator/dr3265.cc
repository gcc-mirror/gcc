// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <iterator>

struct Iter
{
  using iterator_category = std::forward_iterator_tag;
  using value_type = int;
  using pointer = int*;
  using reference = int&;
  using difference_type = std::ptrdiff_t;

  Iter();

  // Construction from int* is not valid:
  Iter(int*) = delete;

  // Assignment from int* is valid:
  Iter& operator=(int*);

  Iter& operator++();
  Iter operator++(int);
  int& operator*() const;
  int* operator->() const;

  template<int N> friend bool operator==(Iter, Iter);
};

void test01()
{
  std::move_iterator<Iter> m;
  int i = 0;
  m = std::make_move_iterator(&i); // Should use assignment not construction
}
