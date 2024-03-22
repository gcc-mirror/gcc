// { dg-do run { target c++14 } }

// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

#include <experimental/any>
#include <testsuite_hooks.h>

using std::experimental::any;

void test01()
{
  any x;
  any y;
  y = x;
  VERIFY( x.empty() );
  VERIFY( y.empty() );

  y = std::move(x);
  VERIFY( x.empty() );
  VERIFY( y.empty() );
}

void test02()
{
  any x(1);
  any y;
  y = x;
  VERIFY( !x.empty() );
  VERIFY( !y.empty() );

  x = std::move(y);
  VERIFY( !x.empty() );
  VERIFY( y.empty() );

  x = y;
  VERIFY( x.empty() );
  VERIFY( y.empty() );
}

int main()
{
  test01();
  test02();
}
