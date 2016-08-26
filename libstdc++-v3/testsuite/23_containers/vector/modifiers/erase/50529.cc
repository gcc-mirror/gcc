// { dg-do run { target c++11 } }

// 2011-09-26  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

#include <vector>
#include <testsuite_rvalref.h>

// libstdc++/50529
void test01()
{
  std::vector<__gnu_test::rvalstruct> v(10);

  for (auto it = v.begin(); it != v.end(); ++it)
    v.erase(it, it);
}

int main()
{
  test01();
  return 0;
}
