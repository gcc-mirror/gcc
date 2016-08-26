// { dg-do run { target c++14 } }

// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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

// You should have received a moved_to of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <experimental/propagate_const>
#include <testsuite_hooks.h>

using std::experimental::propagate_const;
using std::hash;

int main()
{
  int x[2];
  propagate_const<int*> xx1{&x[0]};
  propagate_const<int*> xx2{&x[1]};
  VERIFY(xx1 == xx1);
  VERIFY(xx1 != xx2);
  VERIFY(xx1 < xx2);
  VERIFY(xx2 > xx1);
  VERIFY(xx1 <= xx2);
  VERIFY(xx2 >= xx1);
  VERIFY(xx1 <= xx1);
  VERIFY(xx2 >= xx2);
  VERIFY(std::equal_to<propagate_const<int*>>{}(xx1, xx1));
  VERIFY(std::not_equal_to<propagate_const<int*>>{}(xx1, xx2));
  VERIFY(std::less<propagate_const<int*>>{}(xx1, xx2));
  VERIFY(std::greater<propagate_const<int*>>{}(xx2, xx1));
  VERIFY(std::less_equal<propagate_const<int*>>{}(xx1, xx2));
  VERIFY(std::greater_equal<propagate_const<int*>>{}(xx2, xx1));
  VERIFY(std::less_equal<propagate_const<int*>>{}(xx1, xx1));
  VERIFY(std::greater_equal<propagate_const<int*>>{}(xx2, xx2));
  static constexpr int x2[2]{};
  constexpr propagate_const<const int*> xx3{&x2[0]};
  constexpr propagate_const<const int*> xx4{&x2[1]};
  static_assert(xx3 == xx3, "");
  static_assert(xx3 != xx4, "");
  static_assert(xx3 < xx4, "");
  static_assert(xx4 > xx3, "");
  static_assert(xx3 <= xx4, "");
  static_assert(xx4 >= xx3, "");
  static_assert(xx3 <= xx3, "");
  static_assert(xx4 >= xx4, "");
  static_assert(std::equal_to<propagate_const<const int*>>{}(xx3, xx3), "");
  static_assert(std::not_equal_to<propagate_const<const int*>>{}(xx3, xx4), "");
  static_assert(std::less<propagate_const<const int*>>{}(xx3, xx4), "");
  static_assert(std::greater<propagate_const<const int*>>{}(xx4, xx3), "");
  static_assert(std::less_equal<propagate_const<const int*>>{}(xx3, xx4), "");
  static_assert(std::greater_equal<propagate_const<const int*>>{}(xx4, xx3), "");
  static_assert(std::less_equal<propagate_const<const int*>>{}(xx3, xx3), "");
  static_assert(std::greater_equal<propagate_const<const int*>>{}(xx4, xx4), "");
}
