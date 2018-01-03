// { dg-do run { target c++11 } }
// 2005-01-15 Douglas Gregor <dgregor@cs.indiana.edu>
//
// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// 20.7.15 polymorphic function object wrapper
#include <functional>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

using namespace __gnu_test;

// Put function pointers into function<> wrappers
void test02()
{
  using std::function;

  function<int(float)> f1(truncate_float);
  VERIFY( f1 );
  VERIFY( !!f1 );
  VERIFY( !(f1 == 0) );
  VERIFY( !(0 == f1) );
  VERIFY( f1 != 0 );
  VERIFY( 0 != f1 );

  // Copy-construction
  function<int(float)> f2(f1);
  VERIFY( f2 );

  // Invocation
  VERIFY( f1(3.1f) == 3 );
  VERIFY( f2(3.1f) == 3 );

  // Assignment to zero
  f1 = 0;
  VERIFY( !f1 );

  // Swap
  f1.swap(f2);
  VERIFY( f1 );
  VERIFY( !f2 );
  VERIFY( f1(3.1f) == 3 );

  // Assignment from a function pointer
  f2 = truncate_float;
  VERIFY( f2(3.1f) == 3 );

  // target_type and target() functions
  const function<int(float)>& f1c = f1;
  VERIFY( typeid(int(*)(float)) == f1.target_type() );
  VERIFY( f2.target<int(*)(float)>() != 0 );
  VERIFY( *f2.target<int(*)(float)>() == &truncate_float );
  VERIFY( f1c.target<int(*)(float)>() != 0 );
  VERIFY( *f1c.target<int(*)(float)>() == &truncate_float );
}

int main()
{
  test02();
  return 0;
}
