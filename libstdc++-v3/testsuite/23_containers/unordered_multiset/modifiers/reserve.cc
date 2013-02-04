// { dg-options "-std=gnu++0x" }

// Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

void test01()
{
  const int N = 1000;

  typedef std::unordered_multiset<int> MSet;
  MSet s;
  s.reserve(N * 2);

  std::size_t bkts = s.bucket_count();
  for (int i = 0; i != N; ++i)
    {
      s.insert(i);
      s.insert(i);
      // As long as we insert less than the reserved number of elements we
      // shouldn't experiment any rehash.
      VERIFY( s.bucket_count() == bkts );
    }
}

int main()
{
  test01();
  return 0;
}
