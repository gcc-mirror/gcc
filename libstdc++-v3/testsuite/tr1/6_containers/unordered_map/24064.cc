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

// 6.3 Unordered associative containers

#include <tr1/unordered_map>
#include <testsuite_hooks.h>

// libstdc++/24064
void test01()
{
  bool test __attribute__((unused)) = true;

  using namespace std;
  using namespace tr1;

  unordered_map<int, char, hash<int>, equal_to<int>,
    allocator<pair<const int, char> >, true> m;
 
  for (int i = 0; i < 1000; ++i)
    m[i] = '0' + i % 9;
		
  for (int i = 0; i < 1000; ++i)
    VERIFY( ++m.find(i)->second == '1' + i % 9 );
}
  
int main()
{
  test01();
  return 0;
}
