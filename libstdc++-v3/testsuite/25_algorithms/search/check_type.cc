// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// 25.1.9 [lib.alg.search]

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::forward_iterator_wrapper;

struct T1 { };
struct T2 { };

struct S1 
{
  S1(T1) { }
};

struct S2 {
  S2(T2) { }
};

bool 
operator==(const S1&, const S2&) {return true;}

struct V1 { };
struct V2 { };

struct X1 
{
  X1(V1) { };
};

struct X2
{
  X2(V2) { };
};

bool 
predicate(const X1&, const X2&) {return true;}

forward_iterator_wrapper<S1>
test1(forward_iterator_wrapper<S1>& s1, forward_iterator_wrapper<S2>& s2)
{ return std::search(s1, s1, s2, s2); }

forward_iterator_wrapper<T1>
test2(forward_iterator_wrapper<T1>& s1, forward_iterator_wrapper<T2>& s2)
{ return std::search(s1, s1, s2, s2); }

forward_iterator_wrapper<X1>
test3(forward_iterator_wrapper<X1>& x1, forward_iterator_wrapper<X2>& x2)
{ return std::search(x1, x1, x2, x2, predicate); }

forward_iterator_wrapper<V1>
test4(forward_iterator_wrapper<V1>& x1, forward_iterator_wrapper<V2>& x2)
{ return std::search(x1, x1, x2, x2, predicate); }
