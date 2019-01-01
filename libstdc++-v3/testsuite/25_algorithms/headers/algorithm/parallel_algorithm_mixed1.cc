// { dg-do compile }
// { dg-require-parallel-mode "" }
// { dg-options "-fopenmp" { target *-*-* } }

// Copyright (C) 2007-2019 Free Software Foundation, Inc.
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

#include <parallel/algorithm>
#include <vector>
#include <algorithm>

void test()
{
  typedef unsigned short 	value_type;
  typedef std::vector<value_type> vector_type;
  
  const value_type c(0);

  vector_type v(10), result(20);

  std::equal(v.begin(), v.end(), v.begin());
  std::equal(v.begin(), v.end(), v.begin(), std::equal_to<value_type>());
  __gnu_parallel::equal(v.begin(), v.end(), v.begin());
  __gnu_parallel::equal(v.begin(), v.end(), v.begin(),
                                            std::equal_to<value_type>());

  std::find(v.begin(), v.end(), c);
  __gnu_parallel::find(v.begin(), v.end(), c);

  std::find_first_of(v.begin(), v.end(), v.begin(), v.end());
  std::find_first_of(v.begin(), v.end(), v.begin(), v.end(), 
                                                std::equal_to<value_type>());
  __gnu_parallel::find_first_of(v.begin(), v.end(), v.begin(), v.end());
  __gnu_parallel::find_first_of(v.begin(), v.end(), v.begin(), v.end(),
                                                std::equal_to<value_type>());

  std::search_n(v.begin(), v.end(), 5, value_type(1));
  std::search_n(v.begin(), v.end(), 5, value_type(1),
                                                std::equal_to<value_type>());
  __gnu_parallel::search_n(v.begin(), v.end(), 5, value_type(1));
  __gnu_parallel::search_n(v.begin(), v.end(), 5, value_type(1),
                                                std::equal_to<value_type>());

  std::merge(v.begin(), v.end(), v.begin(), v.end(), result.begin());
  std::merge(v.begin(), v.end(), v.begin(), v.end(), result.begin(),
                                                std::less<value_type>());
  __gnu_parallel::merge(v.begin(), v.end(), v.begin(), v.end(),
                        result.begin());
  __gnu_parallel::merge(v.begin(), v.end(), v.begin(), v.end(),
                        result.begin(), std::less<value_type>());

  std::nth_element(v.begin(), v.begin() + 5, v.end());
  std::nth_element(v.begin(), v.begin() + 5, v.end(), std::less<value_type>());
  __gnu_parallel::nth_element(v.begin(), v.begin() + 5, v.end());
  __gnu_parallel::nth_element(v.begin(), v.begin() + 5, v.end(),
                                                      std::less<value_type>());

  std::partial_sort(v.begin(), v.begin() + 5, v.end());
  std::partial_sort(v.begin(), v.begin() + 5, v.end(),
                                                      std::less<value_type>());
  __gnu_parallel::partial_sort(v.begin(), v.begin() + 5, v.end());
  __gnu_parallel::partial_sort(v.begin(), v.begin() + 5, v.end(),
                                                      std::less<value_type>());

  std::min_element(v.begin(), v.end());
  std::min_element(v.begin(), v.end(), std::less<value_type>());
  __gnu_parallel::min_element(v.begin(), v.end());
  __gnu_parallel::min_element(v.begin(), v.end(), std::less<value_type>());

  std::max_element(v.begin(), v.end());
  std::max_element(v.begin(), v.end(), std::less<value_type>());
  __gnu_parallel::max_element(v.begin(), v.end());
  __gnu_parallel::max_element(v.begin(), v.end(), std::less<value_type>());
}
