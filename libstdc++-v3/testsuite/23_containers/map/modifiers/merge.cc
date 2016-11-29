// Copyright (C) 2016 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }

#include <map>
#include <algorithm>
#include <testsuite_hooks.h>

using test_type = std::map<int, int>;

void
test01()
{
  const test_type c0{ {1, 10}, {2, 20}, {3, 30} };
  test_type c1 = c0, c2 = c0;

  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( c2 == c0 );

  c1.clear();
  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( c2.empty() );

  c2.merge(std::move(c1));
  VERIFY( c1.empty() );
  VERIFY( c2 == c0 );
}

void
test02()
{
  const test_type c0{ {1, 10}, {2, 20}, {3, 30} };
  test_type c1 = c0;
  std::map<int, int, std::less<>> c2( c0.begin(), c0.end() );

  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( std::equal(c2.begin(), c2.end(), c0.begin(), c0.end()) );

  c1.clear();
  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( c2.empty() );

  c2.merge(std::move(c1));
  VERIFY( c1.empty() );
  VERIFY( std::equal(c2.begin(), c2.end(), c0.begin(), c0.end()) );
}

void
test03()
{
  const test_type c0{ {1, 10}, {2, 20}, {3, 30} };
  test_type c1 = c0;
  std::map<int, int, std::greater<>> c2( c0.begin(), c0.end() );

  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( std::equal(c2.rbegin(), c2.rend(), c0.begin(), c0.end()) );

  c1.clear();
  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( c2.empty() );

  c2.merge(c1);
  VERIFY( c1.empty() );
  VERIFY( std::equal(c2.rbegin(), c2.rend(), c0.begin(), c0.end()) );

  c1.merge(std::move(c2));
  VERIFY( c1 == c0 );
  VERIFY( c2.empty() );
}

void
test04()
{
  const test_type c0{ {1, 10}, {2, 20}, {3, 30} };
  test_type c1 = c0;
  std::multimap<int, int, std::greater<>> c2( c0.begin(), c0.end() );

  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( std::equal(c2.rbegin(), c2.rend(), c0.begin(), c0.end()) );

  c1.clear();
  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( c2.empty() );

  c2.merge(c1);
  VERIFY( c1.empty() );
  VERIFY( std::equal(c2.rbegin(), c2.rend(), c0.begin(), c0.end()) );

  c1 = c0;
  c2.merge(c1);
  VERIFY( c1.empty() );
  VERIFY( c2.size() == (2 * c0.size()) );
  VERIFY( std::is_sorted(c2.begin(), c2.end(), c2.value_comp()) );

  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( std::equal(c2.rbegin(), c2.rend(), c0.begin(), c0.end()) );

  c1.merge(std::move(c2));
  VERIFY( c1 == c0 );
  VERIFY( std::equal(c2.rbegin(), c2.rend(), c0.begin(), c0.end()) );

  c1.clear();
  c1.merge(std::move(c2));
  VERIFY( c1 == c0 );
  VERIFY( c2.empty() );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
