// 2002-03-05 Stephen M. Webb  <stephen.webb@bregmasoft.com>

// Copyright (C) 2002-2014 Free Software Foundation, Inc.
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

// 23.2.5  class vector<bool>

#include <vector>
#include <testsuite_hooks.h>

// libstdc++/6886
void test02()
{
  bool test __attribute__((unused)) = true;
  typedef std::vector<bool>  bvec;
  int  i, num = 0;
  bvec v;

  v.resize(66);

  for (i = 0 ; i < 66 ; ++i)
    v[i] = 0;

  v[1]    = 1;
  v[33]   = 1;
  v[49]   = 1;
  v[65]   = 1;

  for (bvec::iterator j = v.begin() ; j != v.end() ; j++)
    if (bool(*j)) ++num;

  VERIFY( num == 4 );
}

int main()
{
  test02();
  return 0;
}
