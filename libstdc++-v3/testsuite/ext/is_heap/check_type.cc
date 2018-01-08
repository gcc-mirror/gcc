// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::random_access_iterator_wrapper;

struct S { };

bool 
operator<(const S&, const S&) {return true;}

struct X { };

bool 
predicate(const X&, const X&) {return true;}

bool
test1(random_access_iterator_wrapper<S>& start,
      random_access_iterator_wrapper<S>& end)
{ return std::__is_heap(start, end) && std::__is_heap(start, 1); }

bool
test2(random_access_iterator_wrapper<X>& start,
      random_access_iterator_wrapper<X>& end)
{ 
  return std::__is_heap(start, end, predicate) &&
         std::__is_heap(start, predicate, 1);
}
