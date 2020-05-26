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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <iterator>
#include <testsuite_hooks.h>

struct value { int n; };

struct sentinel { int limit; };

struct iterator
{
  using iterator_category = std::input_iterator_tag;
  using value_type = value;
  using difference_type = std::ptrdiff_t;
  using reference = value;

  value operator*() const { return value{counter}; }

  iterator& operator++() { ++counter; return *this; }

  iterator operator++(int) { auto i = *this; ++counter; return i; }

  bool operator==(sentinel s) const { return counter == s.limit; }

  int counter = 0;
};

void
test01()
{
  iterator i;
  sentinel s{2};
  std::common_iterator<iterator, sentinel> begin = i, end = s;
  VERIFY( begin->n == 0 );
  ++begin;
  VERIFY( begin->n == 1 );
  ++begin;
  VERIFY( begin == end );
}

int
main()
{
  test01();
}
