// 2000-12-19 bkoz

// Copyright (C) 2000-2019 Free Software Foundation, Inc.
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

// 27.4.2.5 ios_base storage functions

// This fails on some versions of Darwin 8 because malloc doesn't return
// NULL even if an allocation fails (filed as Radar 3884894).
// { dg-do run { xfail *-*-darwin8.[0-4].* } }

// Skip test at -m64 on Darwin because RLIMITS are not being honored.
// Radar 6467883: 10.4/10.5 setrlimits are not honored by memory allocators
// Radar 6467884: 10.X systems are not robust when paging space is exceeded
// { dg-skip-if "" { *-*-darwin* && lp64 } }

#include <sstream>
#include <iostream>
#include <limits>
#include <testsuite_hooks.h>

// libstdc++/3129
void test02()
{
  bool test = true;
  int max = std::numeric_limits<int>::max() - 1;
  std::stringbuf        strbuf;
  std::ios              ios(&strbuf);

  ios.exceptions(std::ios::badbit);

  long l = 0;
  void* v = 0;

  // pword
  ios.pword(1) = v;
  VERIFY( ios.pword(1) == v );

  try
    {
      v = ios.pword(max);
    }
  catch(std::ios_base::failure&)
    {
      // Ok.
      VERIFY( ios.bad() );
    }
  catch(...)
    {
      VERIFY( false );
    }
  VERIFY( v == 0 );

  VERIFY( ios.pword(1) == v );

  // max is different code path from max-1
  v = &test;
  try
    {
      v = ios.pword(std::numeric_limits<int>::max());
    }
  catch(std::ios_base::failure&)
    {
      // Ok.
      VERIFY( ios.bad() );
    }
  catch(...)
    {
      VERIFY( false );
    }
  VERIFY( v == &test );

  // iword
  ios.iword(1) = 1;
  VERIFY( ios.iword(1) == 1 );

  try
    {
      l = ios.iword(max);
    }
  catch(std::ios_base::failure&)
    {
      // Ok.
      VERIFY( ios.bad() );
    }
  catch(...)
    {
      VERIFY( false );
    }
  VERIFY( l == 0 );

  VERIFY( ios.iword(1) == 1 );

  // max is different code path from max-1
  l = 1;
  try
    {
      l = ios.iword(std::numeric_limits<int>::max());
    }
  catch(std::ios_base::failure&)
    {
      // Ok.
      VERIFY( ios.bad() );
    }
  catch(...)
    {
      VERIFY( false );
    }
  VERIFY( l == 1 );
}

int main(void)
{
  __gnu_test::set_memory_limits();
  test02();
  return 0;
}
