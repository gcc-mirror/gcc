// { dg-do run { target c++11 } }

// Copyright (C) 2012-2023 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>

void test01()
{
  typedef std::unordered_map<int, int> Map;
  Map m;

  // Make sure max load factor is 1 so that reserved elements is directly
  // the bucket count.
  m.max_load_factor(1);

  int i = -1;
  for (;;)
    {
      m.reserve(m.bucket_count());

      std::size_t bkts = m.bucket_count();

      m.reserve(bkts);
      VERIFY( m.bucket_count() == bkts );

      for (++i; i < bkts; ++i)
	{
	  m.insert(std::make_pair(i, i));

	  // As long as we insert less than the reserved number of elements we
	  // shouldn't experiment any rehash.
	  VERIFY( m.bucket_count() == bkts );

	  VERIFY( m.load_factor() <= m.max_load_factor() );
	}

      // One more element should rehash.
      m.insert(std::make_pair(i, i));
      VERIFY( m.bucket_count() != bkts );
      VERIFY( m.load_factor() <= m.max_load_factor() );

      if (i > 1024)
	break;
    }
}

int main()
{
  test01();
  return 0;
}
