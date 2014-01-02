// { dg-options "-std=gnu++0x" }

// 2010-08-11  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

#include <forward_list>
#include <testsuite_hooks.h>

// 23.3.3.5 forward_list operations [forwardlist.ops]

// Used to cause many Valgrind errors: LWG 526-type situation.
void test01()
{
  bool test __attribute__((unused)) = true;

  std::forward_list<int> fl1;
  
  fl1.push_front(1);
  fl1.push_front(2);
  fl1.push_front(3);
  fl1.push_front(4);
  fl1.push_front(1);

  fl1.remove(*fl1.begin());

  VERIFY( std::distance(fl1.begin(), fl1.end()) == 3 );

  auto it1 = fl1.begin();

  VERIFY( *it1 == 4 );
  ++it1;
  VERIFY( *it1 == 3 );
  ++it1;
  VERIFY( *it1 == 2 );

  std::forward_list<int> fl2;
  
  fl2.push_front(3);
  fl2.push_front(3);
  fl2.push_front(3);
  fl2.push_front(3);
  fl2.push_front(3);

  auto it2 = fl2.begin();
  ++it2;
  ++it2;

  fl2.remove(*it2);

  VERIFY( std::distance(fl2.begin(), fl2.end()) == 0 );

  std::forward_list<int> fl3;
  
  fl3.push_front(1);
  fl3.push_front(2);
  fl3.push_front(3);
  fl3.push_front(3);
  fl3.push_front(3);

  auto it3 = fl3.begin();
  ++it3;
  ++it3;

  fl3.remove(*it3);

  VERIFY( std::distance(fl3.begin(), fl3.end()) == 2 );

  it3 = fl3.begin();
  VERIFY( *it3 == 2 );
  ++it3;
  VERIFY( *it3 == 1 );
}

int main()
{
  test01();
  return 0;
}
