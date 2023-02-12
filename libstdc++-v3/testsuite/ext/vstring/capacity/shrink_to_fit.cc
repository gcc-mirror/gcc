// { dg-do run { target c++11 } }

// 2010-01-08  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2023 Free Software Foundation, Inc.
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

#include <ext/vstring.h>
#include <testsuite_hooks.h>

// libstdc++/42573
void test01()
{
  __gnu_cxx::__vstring vs(100, 'a');
  vs.push_back('b');
  vs.push_back('b');
  VERIFY( vs.size() < vs.capacity() );
  vs.shrink_to_fit();
  VERIFY( vs.size() == vs.capacity() );
}

int main()
{
  test01();
  return 0;
}
