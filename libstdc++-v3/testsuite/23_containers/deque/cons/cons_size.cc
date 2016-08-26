// { dg-do run { target c++11 } }

// 2010-06-18  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2016 Free Software Foundation, Inc.
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

#include <deque>
#include <testsuite_hooks.h>
#include <testsuite_api.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  std::deque<__gnu_test::NonCopyConstructible> d(1000);
  VERIFY( std::distance(d.begin(), d.end()) == 1000 );
  for(auto it = d.begin(); it != d.end(); ++it)
    VERIFY( *it == -1 );
}

int main()
{
  test01();
  return 0;
}
