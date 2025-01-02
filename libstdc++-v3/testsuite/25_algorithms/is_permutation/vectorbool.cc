// { dg-do run { target c++14 } }

// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

// 25.2.12 [alg.is_permutation] Is permutation

#include <vector>
#include <algorithm>
#include <testsuite_hooks.h>

void test01()
{
  std::vector<bool> v1 = { true, false, true, false, true };
  std::vector<bool> v2 = { false, true, false, true, true };
  VERIFY( std::is_permutation(v1.begin(), v1.end(), v2.begin()) );
  VERIFY( !std::is_permutation(v1.begin() + 1, v1.end(), v2.begin() + 1) );
}

void test02()
{
  std::vector<bool> v1 = { true, false, true, false, true };
  std::vector<bool> v2 = { false, true, false, true, true };
  VERIFY( std::is_permutation(v1.begin(), v1.end(), v2.begin(), v2.end()) );
  VERIFY( !std::is_permutation(v1.begin(), v1.end() - 1, v2.begin(), v2.end()) );
}

int main()
{
  test01();
  test02();
}
