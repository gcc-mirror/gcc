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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 25.3.1.4 [lib.partial.sort.copy]

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::input_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

struct S1 { };
struct S2 
{ 
  S2(const S1&) {}
  S2() {}
};

bool 
operator<(const S1&, const S1&) 
{return true;}

bool 
operator<(const S2&, const S2&) 
{return true;}

struct X1 { };
struct X2 
{
  X2(const X1&) {}
  X2() {}
};

struct predicate
{
  bool 
  operator()(const X1&, const X1&) 
  {return true;}
  
  bool 
  operator()(const X2&, const X2&) 
  {return true;}
};

random_access_iterator_wrapper<S2>
test1(input_iterator_wrapper<S1>& s1, random_access_iterator_wrapper<S2>& s2)
{ return std::partial_sort_copy(s1, s1, s2, s2); }

random_access_iterator_wrapper<X2>
test2(input_iterator_wrapper<X1>& x1, random_access_iterator_wrapper<X2>& x2)
{ return std::partial_sort_copy(x1, x1, x2, x2, predicate()); }
