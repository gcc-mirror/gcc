// Copyright (C) 2013-2019 Free Software Foundation, Inc.
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

// 25.2.11 [alg.equal]

// { dg-do compile { target c++14 } }

#include <algorithm>
#include <testsuite_iterators.h>
using __gnu_test::input_iterator_wrapper;

struct Lhs1 { };

struct Rhs1 { };

bool operator==(const Lhs1&, const Rhs1&) {return true;}

struct Lhs2 { };

struct Rhs2 { };

bool 
predicate(const Lhs2&, const Rhs2&) {return true;}

bool 
test1(input_iterator_wrapper<Lhs1>& lhs1,
      input_iterator_wrapper<Rhs1>& rhs1)
{ return std::equal(lhs1, lhs1, rhs1, rhs1); }

bool 
test2(input_iterator_wrapper<Lhs2>& lhs2,
      input_iterator_wrapper<Rhs2>& rhs2)
{ return std::equal(lhs2, lhs2, rhs2, rhs2, predicate); }
