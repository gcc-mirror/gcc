// { dg-options "-std=gnu++0x" }

// 2010-01-08  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010 Free Software Foundation, Inc.
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

#include <unordered_map>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::unordered_map<int, int>  umap_type;
  typedef umap_type::value_type        value_type;

  umap_type um0{ value_type(1, 1), value_type(2, 2), value_type(3, 3) };

  const umap_type um1(um0);
  um0 = std::move(um0);
  VERIFY( um0.size() == 3 );
  // VERIFY( um0 == um1 );
}

int main()
{
  test01();
  return 0;
}
