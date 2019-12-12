// { dg-do run { target c++11 } }

// Copyright (C) 2013-2019 Free Software Foundation, Inc.
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

// range insert using move iterator

#include <iterator>
#include <vector>
#include <unordered_set>
#include <testsuite_hooks.h>
#include <testsuite_counter_type.h>

void test01()
{
  using namespace __gnu_test;

  std::vector<counter_type> ref = { 0, 1, 2, 3, 4, 5 };
  typedef std::unordered_set<counter_type, counter_type_hasher> Set;
  Set s;

  counter_type::reset();

  s.insert(std::make_move_iterator(ref.begin()),
	   std::make_move_iterator(ref.end()));

  VERIFY( s.size() == ref.size() );
  VERIFY( counter_type::move_count == ref.size() );
}

int main()
{
  test01();
  return 0;
}
