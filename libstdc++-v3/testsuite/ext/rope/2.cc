// Copyright (C) 2004-2021 Free Software Foundation, Inc.
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

// rope (SGI extension)

#include <ext/rope>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;
  using namespace __gnu_cxx;

  crope r(10000, 'x');
  crope r2 = r + "abc" + r;
  crope r3 = r2.substr(10000, 3); 
  crope r4 = r2.substr(10000, 10000); 

  reverse(r2.mutable_begin(), r2.mutable_end());
  VERIFY( r2[10000] == 'c' );

  crope r5('a');
  r5.push_front('b');
  VERIFY( r5[0] == 'b' );
  VERIFY( r5[1] == 'a' );
}

int main()
{
  test01();
  return 0;
}

