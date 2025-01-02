// { dg-do run { target c++20 } }

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <map>
#include <testsuite_hooks.h>

struct aggressive_aggregate
{
    int a;
    int b;
};

void test_emplace()
{
  std::multimap<int, aggressive_aggregate> x;
  auto it = x.emplace(std::piecewise_construct,
			    std::tuple(0), std::tuple(1, 2));
  VERIFY(it->second.a == 1);
  VERIFY(it->second.b == 2);
  it = x.emplace(std::piecewise_construct,
		       std::tuple(1), std::tuple(1));
  VERIFY(it->second.a == 1);
  VERIFY(it->second.b == 0);
  it = x.emplace(std::piecewise_construct,
		       std::tuple(2), std::tuple());
  VERIFY(it->second.a == 0);
  VERIFY(it->second.b == 0);
}

void test_emplace_hint()
{
  std::multimap<int, aggressive_aggregate> x;
  auto it = x.emplace_hint(x.begin(),
			   std::piecewise_construct,
			   std::tuple(3), std::tuple(1, 2));
  VERIFY(it->second.a == 1);
  VERIFY(it->second.b == 2);
  it = x.emplace_hint(x.begin(),
		      std::piecewise_construct,
		      std::tuple(4), std::tuple(1));
  VERIFY(it->second.a == 1);
  VERIFY(it->second.b == 0);
  it = x.emplace_hint(x.begin(),
		      std::piecewise_construct,
		      std::tuple(5), std::tuple());
  VERIFY(it->second.a == 0);
  VERIFY(it->second.b == 0);
}

int main()
{
  test_emplace();
  test_emplace_hint();
}
