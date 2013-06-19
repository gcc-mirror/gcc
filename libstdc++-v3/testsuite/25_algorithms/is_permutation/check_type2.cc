// Copyright (C) 2013 Free Software Foundation, Inc.
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

// 25.2.12 [alg.is_permutation] Is permutation

// { dg-do compile }
// { dg-options " -std=gnu++1y " }

#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::forward_iterator_wrapper;

struct X { };
bool operator==(const X&, const X) { return true; }

struct Y { };
bool predicate(const Y&, const Y&) { return true; }

bool
test1(forward_iterator_wrapper<X>& x1, 
      forward_iterator_wrapper<X>& x2)
{
  return std::is_permutation(x1, x1, x2, x2);
}

bool
test2(forward_iterator_wrapper<Y>& y1,
      forward_iterator_wrapper<Y>& y2)
{
  return std::is_permutation(y1, y1, y2, y2, predicate);
}
