// { dg-options "-Wno-deprecated" }
// { dg-do compile }

// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <hash_map>

// libstdc++/23528
void test01()
{
  __gnu_cxx::hash_map<int, int> m;
  m[888] = 999;

  __gnu_cxx::hash_map<int, int>::allocator_type a = m.get_allocator();

  __gnu_cxx::hash_map<int, int>::value_type *y = a.allocate(1);

  a.construct(y, *m.begin());

  a.destroy(y);
  a.deallocate(y, 1);
}

int main()
{
  test01();
  return 0;
}
