// { dg-do run { target c++14 } }
// { dg-require-cstdint "" }
// { dg-require-effective-target random_device }
// { dg-require-effective-target tls_runtime }
// { dg-add-options tls }

// Derived from: 2010-03-19  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2023 Free Software Foundation, Inc.
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

#include <experimental/algorithm>
#include <vector>
#include <testsuite_hooks.h>

void test01()
{
  for (unsigned size = 0; size < 50; ++size)
    {
      std::vector<int> vref(size);
      std::iota(vref.begin(), vref.end(), 0);
      std::vector<int> v1(vref), v2(vref);

      std::experimental::shuffle(v1.begin(), v1.end());
      std::experimental::shuffle(v2.begin(), v2.end());

      if (size >= 10)
	{
	  VERIFY( !std::equal(v1.begin(), v1.end(), vref.begin()) );
	  VERIFY( !std::equal(v2.begin(), v2.end(), vref.begin()) );
	  VERIFY( !std::equal(v1.begin(), v1.end(), v2.begin()) );
	}

      for (unsigned ind = 0; ind < size; ++ind)
	{
	  auto it1 = std::find(v1.begin(), v1.end(), vref[ind]);
	  auto it2 = std::find(v2.begin(), v2.end(), vref[ind]);
	  VERIFY( it1 != v1.end() );
	  VERIFY( it2 != v2.end() );
	  v1.erase(it1);
	  v2.erase(it2);
	}
      VERIFY( v1.empty() );
      VERIFY( v2.empty() );
    }
}

int main()
{
  test01();
}
