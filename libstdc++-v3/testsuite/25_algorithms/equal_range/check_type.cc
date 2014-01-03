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

// 25.3.3.3 [lib.equal.range]

// { dg-do compile }

#include <algorithm>
#include <utility>
#include <testsuite_iterators.h>

using __gnu_test::forward_iterator_wrapper;

struct S { };

bool operator<(const S&, const S&) {return true;}

struct X { };

bool predicate(const X&, const X&) {return true;}

std::pair<forward_iterator_wrapper<S>, forward_iterator_wrapper<S> > 
test1(forward_iterator_wrapper<S>& s)
{ return std::equal_range(s, s, *s); }

std::pair<forward_iterator_wrapper<X>, forward_iterator_wrapper<X> >
test2(forward_iterator_wrapper<X>& x)
{ return std::equal_range(x, x, *x, predicate); }
