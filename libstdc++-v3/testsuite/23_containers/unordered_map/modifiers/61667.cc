// { dg-options "-std=gnu++11" }

// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

bool test __attribute__((unused)) = true;

void test01()
{
  std::unordered_map<int, int> um(20);

  std::size_t bkt_count = um.bucket_count();

  um.max_load_factor(um.max_load_factor());

  VERIFY( um.bucket_count() >= bkt_count );

  um.max_load_factor(um.max_load_factor() * 2.f);

  VERIFY( um.bucket_count() >= bkt_count );
}

int main()
{
  test01();
  return 0;
}
