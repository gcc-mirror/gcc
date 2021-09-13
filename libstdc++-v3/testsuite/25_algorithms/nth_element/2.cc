// Copyright (C) 2006-2021 Free Software Foundation, Inc.
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

// 25.3.2 [lib.alg.nth.element]

// { dg-options "-DMAX_SIZE=256" { target simulator } }

#ifndef MAX_SIZE
#define MAX_SIZE (1 << 10)
#endif

#include <vector>
#include <algorithm>
#include <testsuite_hooks.h>

void
test_set(std::vector<unsigned>& v, unsigned size)
{
  v.clear();

  for (unsigned i = 0; i < size; i += 4)
    {
      v.push_back(i / 2);
      v.push_back((size - 2) - (i / 2));
    }
  for (unsigned i = 1; i < size; i += 2)
    v.push_back(i);
}

void
do_test01(unsigned size)
{
  std::vector<unsigned> v, s;

  for (unsigned j = 0; j < size; ++j)
    {
      test_set(v, size);
      s = v;
      std::sort(s.begin(), s.end());
      
      std::nth_element(v.begin(), v.begin() + j, v.end());

      VERIFY( v[j] == s[j] );
      
      for (unsigned i = 0; i < j; ++i)
	VERIFY( !(v[j] < v[i]) );

      for (unsigned i = j; i < v.size(); ++i)
	VERIFY( !(v[i] < v[j]) );
    }
}

void
test01()
{
  for (unsigned size = 4; size <= MAX_SIZE; size <<= 1)
    do_test01(size);
}

int main()
{
  test01();
  return 0;
}
