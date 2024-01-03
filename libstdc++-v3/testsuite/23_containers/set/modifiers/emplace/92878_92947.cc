// { dg-do run { target c++20 } }

// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include <set>
#include <testsuite_hooks.h>

struct aggressive_aggregate
{
    int a;
    int b;
};

bool operator<(const aggressive_aggregate& a,
	       const aggressive_aggregate& b)
{
  return a.a < b.a;
};

void test_emplace()
{
  std::set<aggressive_aggregate> x;
  auto emplaced = x.emplace(1, 2);
  VERIFY(emplaced.first->a == 1);
  VERIFY(emplaced.first->b == 2);
  emplaced = x.emplace(2);
  VERIFY(emplaced.first->a == 2);
  VERIFY(emplaced.first->b == 0);
  emplaced = x.emplace();
  VERIFY(emplaced.first->a == 0);
  VERIFY(emplaced.first->b == 0);
}

void test_emplace_hint()
{
  std::set<aggressive_aggregate> x;
  auto it = x.emplace_hint(x.begin(),
		      3, 2);
  VERIFY(it->a == 3);
  VERIFY(it->b == 2);
  it = x.emplace_hint(x.begin(),
		      4);
  VERIFY(it->a == 4);
  VERIFY(it->b == 0);
  it = x.emplace_hint(x.begin());
  VERIFY(it->a == 0);
  VERIFY(it->b == 0);
}

int main()
{
  test_emplace();
  test_emplace_hint();
}
