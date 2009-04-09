// Copyright (C) 2005, 2009 Free Software Foundation, Inc.
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

// 25.3.5.5 [lib.set.symmetric.difference]

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

struct S { };

bool 
operator<(const S&, const S&) {return true;}

struct X { };

bool 
predicate(const X&, const X&) {return true;}

output_iterator_wrapper<S>
test1(input_iterator_wrapper<S>& in, output_iterator_wrapper<S>& out)
{ return std::set_symmetric_difference(in, in, in, in, out); }

output_iterator_wrapper<X> 
test2(input_iterator_wrapper<X>& in, output_iterator_wrapper<X>& out)
{ return std::set_symmetric_difference(in, in, in, in, out, predicate); 
}
