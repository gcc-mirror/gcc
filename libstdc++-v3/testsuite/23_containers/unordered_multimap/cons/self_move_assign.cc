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

  typedef std::unordered_multimap<int, int>  ummap_type;
  typedef ummap_type::value_type             value_type;

  ummap_type umm0{ value_type(1, 1), value_type(2, 2), value_type(3, 3) };

  const ummap_type umm1(umm0);
  umm0 = std::move(umm0);
  VERIFY( umm0.size() == 3 );
  // VERIFY( umm0 == umm1 );
}

int main()
{
  test01();
  return 0;
}
