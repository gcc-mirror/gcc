// { dg-do run { target c++11 } }

// 2010-03-25  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <unordered_map>
#include <algorithm>
#include <testsuite_hooks.h>

void test01()
{
  typedef std::pair<const int, int> Pair;
  std::unordered_multimap<int, int> umm1, umm2;
  VERIFY( umm1 == umm2 );
  VERIFY( !(umm1 != umm2) );

  int second1[] = { -1, -2, -3, -4, -5 };
  int second2[] = { -1, -2, -3, -4, -5 };
  const unsigned size = sizeof(second1) / sizeof(int);

  for (unsigned perm1 = 0; perm1 < 120; ++perm1)
    {
      umm1.clear();
      std::next_permutation(second1, second1 + size);
      for (unsigned i1 = 0; i1 < size; ++i1)
	umm1.insert(Pair(0, second1[i1]));

      for (unsigned perm2 = 0; perm2 < 120; ++perm2)
	{
	  umm2.clear();
	  std::next_permutation(second2, second2 + size);
	  for (unsigned i2 = 0; i2 < size; ++i2)
	    umm2.insert(Pair(0, second2[i2]));

	  VERIFY( umm1 == umm2 );
	  VERIFY( !(umm1 != umm2) );
	}
    }
}

int main()
{
  test01();
  return 0;
}
