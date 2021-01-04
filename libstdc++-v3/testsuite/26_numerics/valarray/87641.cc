// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

#include <valarray>
#include <testsuite_hooks.h>

void
test01()
{
  // PR libstdc++/87641
  std::valarray<int> v1(3);
  v1[0] = 1;
  v1[1] = 2;
  v1[2] = 3;
  std::valarray< std::valarray<int> > v2(v1, 3);
  std::valarray<int> v3 = v2.sum();
  VERIFY( v3.size() == v1.size() );
  VERIFY( v3[0] == 3 );
  VERIFY( v3[1] == 6 );
  VERIFY( v3[2] == 9 );
}

struct X
{
  X() : val(1) { }

  X& operator+=(const X& x) { val += x.val; return *this; }
  bool operator==(const X& x) { return val == x.val; }

  int val;
};

void
test02()
{
  std::valarray<X> v1(1);
  VERIFY( v1.sum() == v1[0] );

  std::valarray<X> v2(2);
  VERIFY( v2.sum().val == 2 );
}

struct Y
{
  X& operator+=(const Y&) { throw 1; }
};

void
test03()
{
  std::valarray<Y> v1(1);
  (void) v1.sum(); // no addition performed for a single element
}

int
main()
{
  test01();
  test02();
  test03();
}
