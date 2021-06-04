// Copyright (C) 2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <unordered_map>

#ifndef __cpp_lib_generic_unordered_lookup
# error "Feature-test macro for generic lookup missing in <unordered_map>"
#elif __cpp_lib_generic_unordered_lookup < 201811L
# error "Feature-test macro for generic lookup has wrong value in <unordered_map>"
#endif

#include <testsuite_hooks.h>

struct Equal
{
  typedef void is_transparent;

  bool operator()(int i, long l) const { return i == l; }
  bool operator()(long l, int i) const { return l == i; }
  bool operator()(int i, int j) const { ++count; return i == j; }

  static int count;
};

int Equal::count = 0;

struct Hash
{
  typedef void is_transparent;

  std::size_t operator()(int i) const { ++count; return i; }
  std::size_t operator()(long l) const { return l; }

  static int count;
};

int Hash::count = 0;

using test_type = std::unordered_map<int, char, Hash, Equal>;

test_type x{ { 1, '2' }, { 3, '4' } };
const test_type& cx = x;

void
test01()
{
  Hash::count = 0;
  Equal::count = 0;

  VERIFY( x.contains(1L) );

  auto it = x.find(1L);
  VERIFY( it != x.end() && it->second == '2' );
  it = x.find(2L);
  VERIFY( it == x.end() );

  auto cit = cx.find(3L);
  VERIFY( cit != cx.end() && cit->second == '4' );
  cit = cx.find(2L);
  VERIFY( cit == cx.end() );

  VERIFY( Hash::count == 0 );
  VERIFY( Equal::count == 0 );

  static_assert(std::is_same<decltype(it), test_type::iterator>::value,
      "find returns iterator");
  static_assert(std::is_same<decltype(cit), test_type::const_iterator>::value,
      "const find returns const_iterator");
}

void
test02()
{
  Hash::count = 0;
  Equal::count = 0;

  auto n = x.count(1L);
  VERIFY( n == 1 );
  n = x.count(2L);
  VERIFY( n == 0 );

  auto cn = cx.count(3L);
  VERIFY( cn == 1 );
  cn = cx.count(2L);
  VERIFY( cn == 0 );

  VERIFY( Hash::count == 0 );
  VERIFY( Equal::count == 0 );
}

void
test03()
{
  Hash::count = 0;
  Equal::count = 0;

  auto it = x.equal_range(1L);
  VERIFY( it.first != it.second && it.first->second == '2' );
  it = x.equal_range(2L);
  VERIFY( it.first == it.second && it.first == x.end() );

  auto cit = cx.equal_range(1L);
  VERIFY( cit.first != cit.second && cit.first->second == '2' );
  cit = cx.equal_range(2L);
  VERIFY( cit.first == cit.second && cit.first == cx.end() );

  VERIFY( Hash::count == 0 );
  VERIFY( Equal::count == 0 );

  using pair = std::pair<test_type::iterator, test_type::iterator>;
  static_assert(std::is_same<decltype(it), pair>::value,
      "equal_range returns pair<iterator, iterator>");
  using cpair = std::pair<test_type::const_iterator, test_type::const_iterator>;
  static_assert(std::is_same<decltype(cit), cpair>::value,
      "const equal_range returns pair<const_iterator, const_iterator>");
}

void
test04()
{
  struct E
  {
    bool operator()(int l, int r) const { return l == r; }

    struct Partition { };

    bool operator()(int l, Partition) const { return l < 6; }
    bool operator()(Partition, int r) const { return 3 < r; }

    using is_transparent = void;
  };

  struct H
  {
    size_t
    operator()(int x) const
    { return 0; }

    size_t
    operator()(E::Partition) const
    { return 0; }

    using is_transparent = void;
  };

  std::unordered_map<int, int, H, E> m{ {1,0}, {2,0}, {3,0}, {4, 0}, {5, 0} };

  auto n = m.count(E::Partition{});
  VERIFY( n == 2 );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
