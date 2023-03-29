// 20010518 gdr

// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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


#include <valarray>

// { dg-do compile }

template<typename P>
  void copy(P, std::size_t) { }

template<typename T>
  void test(const std::valarray<T>& v)
  {
     copy(&v[0], v.size());
  }

int main()
{
  std::valarray<double> v(190);
  test(v);
  return 0;
}
