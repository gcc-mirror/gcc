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

// 25.5.8 [lib.alg.unique_copy]

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

struct S1 { };

struct S2
{
  S2(const S1&) {}
};

bool 
operator==(const S1&, const S1&) {return true;}

struct X1 { };

struct X2
{
  X2(const X1&) {}
};

bool 
predicate(const X1&, const X1&) {return true;}

output_iterator_wrapper<S2> 
test1(input_iterator_wrapper<S1>& s1, output_iterator_wrapper<S2>& s2)
{ return std::unique_copy(s1, s1, s2); }

output_iterator_wrapper<X2>
test2(input_iterator_wrapper<X1>& x1, output_iterator_wrapper<X2>& x2)
{ return std::unique_copy(x1, x1, x2, predicate); }
