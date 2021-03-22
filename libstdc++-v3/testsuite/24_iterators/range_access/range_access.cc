// { dg-do compile { target c++11 } }

// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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

// C++ 2011 24.6.5, range access [iterator.range]

#include <iterator>

void
test01()
{
  int arr[3] = {1, 2, 3};
  std::begin(arr);
  std::end(arr);

  static_assert( noexcept(std::begin(arr)), "LWG 2280" );
  static_assert( noexcept(std::end(arr)), "LWG 2280" );
}

void
test02()
{
  extern void require_int(int*);
  extern void require_long(long*);

  struct B
  {
    int* begin() { return nullptr; }
    long* begin() const { return nullptr; }
  };

  B b;
  require_int( std::begin(b) );
  require_long( std::begin(const_cast<const B&>(b)) );

  struct E
  {
    int* end() { return nullptr; }
    long* end() const { return nullptr; }
  };

  E e;
  require_int( std::end(e) );
  require_long( std::end(const_cast<const E&>(e)) );
}
