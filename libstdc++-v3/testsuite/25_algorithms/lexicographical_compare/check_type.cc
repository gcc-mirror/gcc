// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

// 25.3.8 [lib.alg.lex.comparison]

// { dg-do compile }


#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::input_iterator_wrapper;

struct Lhs1 { };

struct Rhs1 { };

bool 
operator<(const Lhs1&, const Rhs1&) {return true;}

bool 
operator<(const Rhs1&, const Lhs1&) {return false;}

struct X { };

bool 
predicate(const X&, const X&) {return true;}

bool 
test1(input_iterator_wrapper<Lhs1>& lhs1,
      input_iterator_wrapper<Rhs1>& rhs1)
{ return std::lexicographical_compare(lhs1, lhs1, rhs1, rhs1); }

bool 
test2(input_iterator_wrapper<X>& x)
{ return std::lexicographical_compare(x, x, x, x, predicate); }
