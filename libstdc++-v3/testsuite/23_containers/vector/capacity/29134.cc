// Copyright (C) 2006-2025 Free Software Foundation, Inc.
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

// 23.2.4.2 vector capacity [lib.vector.capacity]

#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// libstdc++/29134
void test01()
{
  std::vector<int> v;

  std::allocator<int> a = v.get_allocator();
#if __cplusplus > 201703L
  // std::allocator_traits::max_size() is unrealistically large,
  // so std::vector::max_size() returns a smaller value.
  VERIFY( v.max_size() <= __gnu_test::max_size(a) );
#else
  VERIFY( v.max_size() == __gnu_test::max_size(a) );
#endif

}

int main()
{
  test01();
  return 0;
}
