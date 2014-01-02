// Copyright (C) 2005-2014 Free Software Foundation, Inc.
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

// 25.3.3.2 [lib.upper.bound]

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::forward_iterator_wrapper;

struct S { };

bool 
operator<(const S&, const S&) {return true;}

struct X { };

bool 
predicate(const X&, const X&) {return true;}

forward_iterator_wrapper<S>
test1(forward_iterator_wrapper<S>& s)
{ return std::upper_bound(s, s, *s); }

forward_iterator_wrapper<X>
test2(forward_iterator_wrapper<X>& x)
{ return std::upper_bound(x, x, *x, predicate); }
