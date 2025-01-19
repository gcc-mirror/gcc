// { dg-do run { target c++11 } }

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <utility>
#include <unordered_map>
#include <vector>

#include <testsuite_hooks.h>
#include <testsuite_counter_type.h>
#include <testsuite_allocator.h>

void test01()
{
  using namespace std;
  using __gnu_test::counter_type;

  std::vector<pair<int, counter_type>> insts { { 0, 0 }, { 1, 1 }, { 2, 2 } };
  typedef unordered_map<int, counter_type> Map;
  Map m;

  counter_type::reset();

  m.insert(make_move_iterator(insts.begin()), make_move_iterator(insts.end()));

  VERIFY( m.size() == 3 );
  VERIFY( counter_type::default_count == 0 );
  VERIFY( counter_type::copy_count == 0 );
  VERIFY( counter_type::move_count == 3 );
}

void test02()
{
  using namespace std;
  using __gnu_test::counter_type;
  using __gnu_test::propagating_allocator;

  typedef propagating_allocator<pair<const int, counter_type>, false> Alloc;
  typedef unordered_map<int, counter_type,
			hash<int>, equal_to<int>,
			Alloc> Map;

  Alloc a1(1);
  Map m1(3, a1);
  m1 = { { 0, 0 }, { 1, 1 }, { 2, 2 } };
  Alloc a2(2);
  Map m2(3, a2);
  m2 = { { 3, 0 }, { 4, 1 }, { 5, 2 } };

  counter_type::reset();

  m2 = move(m1);

  VERIFY( m1.empty() );
  VERIFY( m2.size() == 3 );
  VERIFY( counter_type::default_count == 0 );
  VERIFY( counter_type::copy_count == 0 );
  VERIFY( counter_type::move_count == 3 );
}

int main()
{
  test01();
  test02();
  return 0;
}
