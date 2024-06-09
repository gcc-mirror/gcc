// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++98 -ffast-math" }
// { dg-do run { target i?86-*-* x86_64-*-* } }

#include <cmath>
#include <testsuite_hooks.h>

void
test01()
{
  long double ld = -5.3165867831218916301793863361917824e-2467L;
  VERIFY( std::signbit(ld) == 1 );
}

int
main()
{
  test01();
}
