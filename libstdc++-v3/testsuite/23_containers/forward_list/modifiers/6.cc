// { dg-options "-std=gnu++11" }

// Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

#include <forward_list>

#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  std::forward_list<int> fl1(1, 5), fl2(1, 4), fl3(1, 3),
                         fl4(1, 2), fl5(1, 1), fl6(1, 0);

  fl1.splice_after(fl1.before_begin(), fl2);

  auto it = fl1.begin();

  VERIFY( *it == 4 );

  ++it;
  
  VERIFY( *it == 5 );

  fl3.splice_after(fl3.before_begin(), fl4, fl4.before_begin());

  it = fl3.begin();

  VERIFY( *it == 2 );

  ++it;
  
  VERIFY( *it == 3 );

  fl5.splice_after(fl5.before_begin(), fl6, fl6.before_begin(), fl6.end());

  it = fl5.begin();

  VERIFY( *it == 0 );

  ++it;
  
  VERIFY( *it == 1 );

  fl1.merge(fl2);

  it = fl1.begin();

  VERIFY( *it == 4 );

  ++it;

  VERIFY( *it == 5 );

  fl1.merge(fl3, std::less<int>());

  it = fl1.begin();

  VERIFY( *it == 2 );

  ++it;
  
  VERIFY( *it == 3 );

  ++it;
  
  VERIFY( *it == 4 );

  ++it;
  
  VERIFY( *it == 5 );
}

int main()
{
  test01();
  return 0;
}
