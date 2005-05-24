// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 25.1.9 [lib.alg.search]

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::forward_iterator_wrapper;

struct S1 { };
struct S2 { };

bool 
operator==(const S1&, const S2&) {return true;}

struct X1 { };
struct X2 { };

bool 
predicate(const X1&, const X2&) {return true;}

forward_iterator_wrapper<S1>
test1(forward_iterator_wrapper<S1>& s1, forward_iterator_wrapper<S2>& s2)
{ return std::search(s1, s1, s2, s2); }

forward_iterator_wrapper<X1>
test2(forward_iterator_wrapper<X1>& x1, forward_iterator_wrapper<X2>& x2)
{ return std::search(x1, x1, x2, x2, predicate); }
