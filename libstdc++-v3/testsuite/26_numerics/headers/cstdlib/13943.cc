// Copyright (C) 2005-2023 Free Software Foundation, Inc.
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

// Math-related cstdlib bits are not freestanding.
// { dg-require-effective-target hosted }

#include <cstdlib>
#include <testsuite_hooks.h>

#if _GLIBCXX_USE_C99_STDLIB
// libstdc++/13943
void test01()
{
  using namespace std;

  VERIFY( llabs(-3) == 3 );
  
  lldiv_t q = lldiv(6, 4);
  VERIFY( q.quot == 1 );
  VERIFY( q.rem == 2 );
}
#endif

int main()
{
#if _GLIBCXX_USE_C99_STDLIB
  test01();
#endif
  return 0;
}
