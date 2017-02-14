// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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

// 25.3.5.1 [lib.includes]

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::input_iterator_wrapper;

struct S { };

bool 
operator<(const S&, const S&) 
{ return true; }

struct X { };

bool
predicate(const X&, const X&)
{ return true; }

bool
test1(input_iterator_wrapper<S>& s)
{ return std::includes(s, s, s, s); }

bool
test2(input_iterator_wrapper<X>& x)
{ return std::includes(x, x, x, x, predicate); }
