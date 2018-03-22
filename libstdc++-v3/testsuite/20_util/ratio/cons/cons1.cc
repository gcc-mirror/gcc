// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

// 2008-07-03 Chris Fairles <chris.fairles@gmail.com>

// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <ratio>
#include <testsuite_hooks.h>

void
test01()
{
  std::ratio<1,3> r0;
  std::ratio<2,6> r1;  
  std::ratio<2,-6> r2;
  std::ratio<-2,6> r3;

  VERIFY( r0.num == 1 );
  VERIFY( r0.den == 3 );

  VERIFY( r1.num == r0.num );
  VERIFY( r1.den == r0.den );  
  VERIFY( r2.num == -r0.num );
  VERIFY( r2.den == r0.den ); 
  VERIFY( r3.num == -r0.num );
  VERIFY( r3.den == r0.den );  
}

int main()
{
  test01();
  return 0;
}
