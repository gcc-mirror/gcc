// 1999-11-09 bkoz

// Copyright (C) 1999-2021 Free Software Foundation, Inc.
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

// 23.2.4.3 vector modifiers

#include <vector>
#include "testsuite_hooks.h"

template<typename T>
  struct A { };

struct B { };

// vector::insert(iterator, inputiterator first, inputiterator last)
void
test01()
{
  // POD types
  typedef std::vector<int> 	vec_POD;
  vec_POD	vec01;
  int 		i01 = 5;
  int*		pi01 = &i01;
  vec01.insert(vec01.begin(), pi01, pi01 + 1);

  // non POD types
  typedef std::vector< A<B> >	vec_nonPOD;
  vec_nonPOD	vec02;
  A<B>		np01;
  A<B>*		pnp01 = &np01;
  vec02.insert(vec02.begin(), pnp01, pnp01 + 1);
}

int main()
{
  test01();
  return 0;
}
