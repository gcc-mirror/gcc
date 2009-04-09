// { dg-options "-std=gnu++0x" }

// 2008-06-11  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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

// DR 691.
void test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::unordered_map<int, int> um_type;
  um_type um;
  um[1] = 1;
  VERIFY( um.cbegin(0) == um.begin(0) );
  VERIFY( um.cend(0) == um.end(0) );
  const um_type::size_type bn = um.bucket(1);
  VERIFY( um.cbegin(bn) != um.cend(bn) );
}

int main()
{
  test01();
  return 0;
}
