// 2006-06-04  Stephen M. Webb <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2006-2018 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 5.1.5 Engines with predefined parameters
// 5.1.5 [3]

#include <tr1/random>
#include <testsuite_hooks.h>

void
test01()
{
  std::tr1::ranlux4 a;
  for (int i = 0; i < 9999; ++i)
    a();

  VERIFY( a() == 8587295 );
}

int main()
{
  test01();
  return 0;
}
