// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <unordered_map>
#include <algorithm>
#include <testsuite_hooks.h>

using test_type = std::unordered_multimap<int, int>;
struct hash {
  auto operator()(int i) const noexcept { return ~std::hash<int>()(i); }
};
struct equal : std::equal_to<> { };

void
test01()
{
  const test_type c0{ {1, 10}, {1, 11}, {2, 20}, {2, 21}, {3, 30}, {3, 31} };
  test_type c1 = c0, c2 = c0;

  c1.merge(c2);
  for (auto& i : c1)
    VERIFY( c1.count(i.first) == (2 * c0.count(i.first)) );
  VERIFY( c2.empty() );

  c1.clear();
  c2 = c0;
  c1.merge(std::move(c2));
  VERIFY( c1 == c0 );
  VERIFY( c2.empty() );
}

void
test02()
{
  const test_type c0{ {1, 10}, {1, 11}, {2, 20}, {2, 21}, {3, 30}, {3, 31} };
  test_type c1 = c0;
  std::unordered_multimap<int, int, hash, equal> c2( c0.begin(), c0.end() );

  c1.merge(c2);
  VERIFY( c1.size() == (2 * c0.size()) );
  for (auto& i : c1)
    VERIFY( c1.count(i.first) == (2 * c0.count(i.first)) );
  VERIFY( c2.empty() );

  c1.clear();
  c2.insert( c0.begin(), c0.end() );
  c1.merge(std::move(c2));
  VERIFY( c1 == c0 );
  VERIFY( c2.empty() );
}

void
test03()
{
  const test_type c0{ {1, 10}, {1, 11}, {2, 20}, {2, 21}, {3, 30}, {3, 31} };
  test_type c1 = c0;
  std::unordered_multimap<int, int, hash, equal> c2( c0.begin(), c0.end() );

  c1.merge(c2);
  VERIFY( c1.size() == (2 * c0.size()) );
  for (auto& i : c1)
    VERIFY( c1.count(i.first) == (2 * c0.count(i.first)) );
  VERIFY( c2.empty() );

  c1.clear();
  c2.insert( c0.begin(), c0.end() );
  c1.merge(std::move(c2));
  VERIFY( c1 == c0 );
  VERIFY( c2.empty() );
}

void
test04()
{
  const test_type c0{ {1, 10}, {1, 11}, {2, 20}, {2, 21}, {3, 30}, {3, 31} };
  test_type c1 = c0;
  std::unordered_map<int, int, hash, equal> c2( c0.begin(), c0.end() );

  c1.merge(c2);
  VERIFY( c1.size() == (1.5 * c0.size()) );
  for (auto& i : c1)
    VERIFY( c1.count(i.first) == (1.5 * c0.count(i.first)) );
  VERIFY( c2.empty() );

  c1.clear();
  c2.insert( c0.begin(), c0.end() );
  c1.merge(std::move(c2));
  VERIFY( c1.size() == (0.5 * c0.size()) );
  VERIFY( c2.empty() );
}

void
test07()
{
  test_type c1{ {1, 1}, {3, 3}, {5, 5} };
  test_type c2{ {2, 2}, {4, 4}, {6, 6} };
  const test_type c3 = c2;

  c1.merge(c2);
  VERIFY( c1.size() == 6 );
  VERIFY( c2.empty() );
  const test_type c4 = c1;

  c2 = c3;
  c1.clear();
  c1.merge(std::move(c2));
  VERIFY( c1 == c3 );
  VERIFY( c2.empty() );

  c2.merge(std::move(c1));
  VERIFY( c1.empty() );
  VERIFY( c2 == c3 );

  c2.merge(c1);
  VERIFY( c1.empty() );
  VERIFY( c2 == c3 );

  c2.merge(c2);
  VERIFY( c2 == c3 );

  auto c5 = c4;
  c2.merge(c5);
  VERIFY( c2.size() == 9 );
  VERIFY( c5.empty() );

  c5 = c4;
  c5.emplace(9, 9);
  c2.merge(c5);
  VERIFY( c2.size() == 16 );
  VERIFY( c5.empty() );
}

void
test08()
{
  test_type c1{ {1, 1}, {3, 3}, {5, 5} };
  std::unordered_map<int, int, hash, equal> c2{ {2, 2}, {4, 4}, {6, 6} };
  const auto c3 = c2;

  c1.merge(c2);
  VERIFY( c1.size() == 6 );
  VERIFY( c2.empty() );
  const test_type c4 = c1;

  c2 = c3;
  c1.clear();
  c1.merge(std::move(c2));
  VERIFY( c2.empty() );

  c2.merge(std::move(c1));
  VERIFY( c1.empty() );
  VERIFY( c2 == c3 );

  c2.merge(c1);
  VERIFY( c1.empty() );
  VERIFY( c2 == c3 );

  c2.merge(c2);
  VERIFY( c2 == c3 );
}

void
test09()
{
  struct stateful_hash
  {
    size_t seed = 0;

    auto operator()(const int& i) const noexcept
    { return std::hash<int>()(i) + seed; }
  };

  using map_type = std::unordered_multimap<int, int, stateful_hash>;
  map_type c1({ {1, 1}, {3, 3}, {5, 5} }, 0, stateful_hash{1});
  map_type c2({ {2, 2}, {4, 4}, {6, 6} }, 0, stateful_hash{2});
  const auto c3 = c2;

  c1.merge(c2);
  VERIFY( c1.size() == 6 );
  VERIFY( c2.empty() );

  c2 = c3;
  c1.clear();
  c1.merge(std::move(c2));
  VERIFY( c1 == c3 );
  VERIFY( c2.empty() );

  c2.merge(std::move(c1));
  VERIFY( c1.empty() );
  VERIFY( c2 == c3 );

  c2.merge(c1);
  VERIFY( c1.empty() );
  VERIFY( c2 == c3 );

  c2.merge(c2);
  VERIFY( c2 == c3 );

  test_type c4{ {-1, 0}, {-3, 0}, {-5, 0} };
  c2.merge(c4);
  VERIFY( c2.size() == 6 );
  VERIFY( c4.empty() );
  auto c6 = c3;
  c6.merge(c2);
  VERIFY( c6.size() == 9 );
  VERIFY( c2.size() == 0 );
}
int
main()
{
  test01();
  test02();
  test03();
  test04();
  test07();
  test08();
  test09();
}
