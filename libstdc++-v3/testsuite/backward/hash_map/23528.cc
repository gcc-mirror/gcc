// { dg-options "-Wno-deprecated" }
// { dg-do compile }

// Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

#include <hash_map>

// libstdc++/23528
void test01()
{
  __gnu_cxx::hash_map<int, int> m;
  m[888] = 999;

  __gnu_cxx::hash_map<int, int>::allocator_type a = m.get_allocator();

  __gnu_cxx::hash_map<int, int>::value_type *y = a.allocate(1);

#if __cplusplus >= 201103L
  std::allocator_traits<decltype(a)>::construct(a, y, *m.begin());
  std::allocator_traits<decltype(a)>::destroy(a, y);
#else
  a.construct(y, *m.begin());
  a.destroy(y);
#endif
  a.deallocate(y, 1);
}

int main()
{
  test01();
  return 0;
}
