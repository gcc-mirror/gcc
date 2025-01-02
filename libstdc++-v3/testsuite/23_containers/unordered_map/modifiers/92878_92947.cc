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

#include <unordered_map>
#include <testsuite_hooks.h>

struct aggressive_aggregate
{
    int a;
    int b;
};

void test_emplace()
{
  std::unordered_map<int, aggressive_aggregate> x;
  auto emplaced = x.emplace(std::piecewise_construct,
			    std::tuple(0), std::tuple(1, 2));
  VERIFY(emplaced.first->second.a == 1);
  VERIFY(emplaced.first->second.b == 2);
  emplaced = x.emplace(std::piecewise_construct,
		       std::tuple(1), std::tuple(1));
  VERIFY(emplaced.first->second.a == 1);
  VERIFY(emplaced.first->second.b == 0);
  emplaced = x.emplace(std::piecewise_construct,
		       std::tuple(2), std::tuple());
  VERIFY(emplaced.first->second.a == 0);
  VERIFY(emplaced.first->second.b == 0);
}

void test_emplace_hint()
{
  std::unordered_map<int, aggressive_aggregate> x;
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

void test_try_emplace_rvalue()
{
  std::unordered_map<int, aggressive_aggregate> x;
  auto emplaced = x.try_emplace(6, 1, 2);
  VERIFY(emplaced.first->second.a == 1);
  VERIFY(emplaced.first->second.b == 2);
  emplaced = x.try_emplace(7, 1);
  VERIFY(emplaced.first->second.a == 1);
  VERIFY(emplaced.first->second.b == 0);
  emplaced = x.try_emplace(8);
  VERIFY(emplaced.first->second.a == 0);
  VERIFY(emplaced.first->second.b == 0);
}

void test_try_emplace_lvalue()
{
  std::unordered_map<int, aggressive_aggregate> x;
  int key = 9;
  auto emplaced = x.try_emplace(key, 1, 2);
  VERIFY(emplaced.first->second.a == 1);
  VERIFY(emplaced.first->second.b == 2);
  key = 10;
  emplaced = x.try_emplace(key, 1);
  VERIFY(emplaced.first->second.a == 1);
  VERIFY(emplaced.first->second.b == 0);
  key = 11;
  emplaced = x.try_emplace(key);
  VERIFY(emplaced.first->second.a == 0);
  VERIFY(emplaced.first->second.b == 0);
}

void test_try_emplace_hint_rvalue()
{
  std::unordered_map<int, aggressive_aggregate> x;
  auto it = x.try_emplace(x.begin(), 12, 1, 2);
  VERIFY(it->second.a == 1);
  VERIFY(it->second.b == 2);
  it = x.try_emplace(x.begin(), 13, 1);
  VERIFY(it->second.a == 1);
  VERIFY(it->second.b == 0);
  it = x.try_emplace(x.begin(), 14);
  VERIFY(it->second.a == 0);
  VERIFY(it->second.b == 0);
}

void test_try_emplace_hint_lvalue()
{
  std::unordered_map<int, aggressive_aggregate> x;
  int key = 15;
  auto it = x.try_emplace(x.begin(), key, 1, 2);
  VERIFY(it->second.a == 1);
  VERIFY(it->second.b == 2);
  key = 16;
  it = x.try_emplace(x.begin(), key, 1);
  VERIFY(it->second.a == 1);
  VERIFY(it->second.b == 0);
  key = 17;
  it = x.try_emplace(x.begin(), key);
  VERIFY(it->second.a == 0);
  VERIFY(it->second.b == 0);
}

int main()
{
  test_emplace();
  test_emplace_hint();
  test_try_emplace_rvalue();
  test_try_emplace_lvalue();
  test_try_emplace_hint_rvalue();
  test_try_emplace_hint_lvalue();
}
