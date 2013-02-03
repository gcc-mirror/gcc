// Copyright (C) 2005-2013 Free Software Foundation, Inc.
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

// 25.1.3 [lib.alg.find.end]

// { dg-do compile }


#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::forward_iterator_wrapper;

struct Lhs1 { };

struct Rhs1 { };

bool operator==(const Lhs1&, const Rhs1&) {return true;}

struct X1 { };

struct X2 { };

bool predicate(const X1&, const X2&) {return true;}

forward_iterator_wrapper<Lhs1>
test1(forward_iterator_wrapper<Lhs1>& lhs1, 
      forward_iterator_wrapper<Rhs1>& rhs1)
{
  return std::find_end(lhs1, lhs1, rhs1, rhs1);
}

forward_iterator_wrapper<X1>
test2(forward_iterator_wrapper<X1>& x1,
      forward_iterator_wrapper<X2>& x2)
{
  return std::find_end(x1, x1, x2, x2, predicate);
}
