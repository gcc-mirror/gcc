// Copyright (C) 2016-2020 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <algorithm>
#include <testsuite_hooks.h>

using test_type = std::unordered_set<int>;

struct hash {
  auto operator()(int i) const noexcept { return ~std::hash<int>()(i); }
};
struct equal : std::equal_to<> { };

template<typename C1, typename C2>
bool equal_elements(const C1& c1, const C2& c2)
{
  if (c2.size() != c1.size())
    return false;
  for (auto& i : c1)
    if (c2.count(i) != c1.count(i))
      return false;
  return true;
}

void
test01()
{
  const test_type c0{ 1, 2, 3, };
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
  const test_type c0{ 1, 2, 3, };
  test_type c1 = c0;
  std::unordered_set<int, hash, equal> c2( c0.begin(), c0.end() );

  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( equal_elements(c2, c0) );

  c1.clear();
  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( c2.empty() );

  c2.merge(c1);
  VERIFY( c1.empty() );
  VERIFY( equal_elements(c2, c0) );

  c1.merge(std::move(c2));
  VERIFY( c2.empty() );
  VERIFY( c1 == c0 );
}

void
test03()
{
  const test_type c0{ 1, 2, 3, };
  test_type c1 = c0;
  std::unordered_multiset<int, hash, equal> c2( c0.begin(), c0.end() );
  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( equal_elements(c2, c0) );

  c1.clear();
  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( c2.empty() );

  c2.merge(c1);
  VERIFY( c1.empty() );
  VERIFY( equal_elements(c2, c0) );

  c1 = c0;
  c2.merge(c1);
  VERIFY( c1.empty() );
  VERIFY( c2.size() == (2 * c0.size()) );
  VERIFY( c2.count(1) == 2 );
  VERIFY( c2.count(2) == 2 );
  VERIFY( c2.count(3) == 2 );

  c1.merge(c2);
  VERIFY( c1 == c0 );
  VERIFY( equal_elements(c2, c0) );

  c1.merge(std::move(c2));
  VERIFY( c1 == c0 );
  VERIFY( equal_elements(c2, c0) );

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
}
