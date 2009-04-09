// { dg-do compile }
// { dg-require-parallel-mode "" }
// { dg-options "-fopenmp" { target *-*-* } }

// Copyright (C) 2007, 2009 Free Software Foundation, Inc.
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

// Make sure to test without _GLIBCXX_PARALLEL
#ifdef _GLIBCXX_PARALLEL 
# undef _GLIBCXX_PARALLEL 
#endif

#include <parallel/numeric>
#include <numeric>
#include <vector>
#include <numeric>

void test()
{
  typedef unsigned short 	value_type;
  typedef std::vector<value_type> vector_type;
  
  const value_type c(0);

  vector_type v(10);
  std::accumulate(v.begin(), v.end(), value_type(1));
  __gnu_parallel::accumulate(v.begin(), v.end(), value_type(1));
}
