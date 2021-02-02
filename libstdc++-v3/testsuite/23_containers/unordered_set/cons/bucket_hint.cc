// { dg-do run { target c++11 } }

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <vector>
#include <forward_list>
#include <unordered_set>

#include <testsuite_hooks.h>

void test01()
{
  std::unordered_set<int> a;
  a.reserve(2);

  std::unordered_set<int> b({ 0, 1, 0, 1, 0, 1, 0, 1 }, a.bucket_count());
  VERIFY( b.bucket_count() == a.bucket_count() );
}

void test02()
{
  std::unordered_set<int> a;
  a.reserve(2);

  std::vector<int> v { 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 };

  std::unordered_set<int> b(v.begin(), v.end(), a.bucket_count());
  VERIFY( b.bucket_count() == a.bucket_count() );
}

void test03()
{
  std::unordered_set<int> a;
  a.reserve(2);

  std::forward_list<int> fl { 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 };

  std::unordered_set<int> b(fl.begin(), fl.end(), a.bucket_count());
  VERIFY( b.bucket_count() == a.bucket_count() );
}

int main()
{
  test01();
  test02();
  test03();
  return 0;
}
